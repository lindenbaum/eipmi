%%%=============================================================================
%%% Copyright (c) 2012 Lindenbaum GmbH
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% A server providing session management for IPMI over lan channels. The
%%% session will be established as soon as the server gets started. An
%%% established session will be closed when the server terminates. The user
%%% can close the session using {@link stop/1}.
%%%
%%% Synchronous requests can be issued over this session at any given time using
%%% {@link rpc/3} or {@link rpc/4}. When the session is not yet established
%%% requests will be queued and issued as soon as the far end (BMC) is ready.
%%% Request timeouts can be configured on state machine startup using the
%%% `timeout' property of the `Options' field.
%%%
%%% A session may be shared between mutliple processes. While the requests of
%%% one process will be synchronous and thus ordered, requests from different
%%% processes will not block each other. However, it should be mentioned that
%%% a BMC is allowed to discard packets with a sequence number difference of 8.
%%% Since the session does not (yet) provide any kind of flow control it is the
%%% responsibility of the rpc-ing processes to care for the total number of
%%% concurrent RPCs or to live with error/timeout returns.
%%%
%%% The server will handle the low level RMCP and IPMI protocol regarding
%%% encoding and decoding of messages, as well as correct packet
%%% acknowledgement, sequence number handling and session opening/closing.
%%% Packet loss is not handled during session setup.
%%%
%%% A session will use the modules {@link eipmi_request} and
%%% {@link eipmi_response} to encode and decode requests/responses. Therefore,
%%% there's no need to edit the session but extending these modules when
%%% support for new requests/responses is added.
%%%
%%% TODO:
%%% * Use Activate Session requests for session keep-alive
%%% @end
%%%=============================================================================
-module(eipmi_session).

-behaviour(gen_server).

%% API
-export([start_link/3,
         rpc/3,
         rpc/4,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("eipmi.hrl").

-type property() ::
        eipmi:option() |
        {auth_type, none | pwd | md5 | md2} |
        {auth_types, [none | pwd | md5 | md2]} |
        {challenge, binary()} |
        {completion, atom()} |
        {inbound_seq_nr, non_neg_integer()} |
        {login_status, [anonymous | null | non_null]} |
        {outbound_seq_nr, non_neg_integer()} |
        {rq_seq_nr, 0..16#40} |
        {session_id, non_neg_integer()}.

-type property_name() ::
        eipmi:option_name() |
        auth_type |
        auth_types |
        challenge |
        completion |
        inbound_seq_nr |
        login_status |
        outbound_seq_nr |
        rq_seq_nr |
        session_id.

-export_type([property/0, property_name/0]).

-define(RETRANSMITS, 2).
-define(KEEP_ALIVE_TIMER, 45000).

%%------------------------------------------------------------------------------
%% Session defaults, partially modifyable by the user.
%%------------------------------------------------------------------------------
-define(DEFAULTS,
        [
         %% default values modifyable through eipmi:open/2
         {initial_outbound_seq_nr, 16#1337},
         {keep_alive_retransmits, ?RETRANSMITS},
         {password, ""},
         {port, ?RMCP_PORT_NUMBER},
         {privilege, administrator},
         {rq_addr, 16#81},
         {timeout, 1000},
         {user, ""},
         %% unmodifyable session defaults
         {auth_type, none},    %% initial packets are not authenticated
         {inbound_seq_nr, 0},  %% initial packets have the null seqnr
         {outbound_seq_nr, 0}, %% initial packets have the null seqnr
         {rq_lun, ?IPMI_REQUESTOR_LUN},
         {rq_seq_nr, 0},       %% initial requests have the null seqnr
         {rs_addr, ?IPMI_RESPONDER_ADDR},
         {rs_lun, ?IPMI_RESPONDER_LUN},
         {session_id, 0}       %% initial have the null session id
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts a session server which in turn will occupy a free UDP socket. The
%% provided IP address and network port will be used to send requests. This
%% means that the target for this session server is static and configured on
%% startup. This will also setup a session by sending the necessary IPMI
%% protocol messages. All requests received before the session is activated will
%% be queued and sent as soon as the session is established.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(eipmi:session(),
                 inet:ip_address() | inet:hostname(),
                 [property()]) ->
                        {ok, pid()} | {error, term()}.
start_link(Session, IPAddress, Options) ->
    gen_server:start_link(?MODULE, [Session, IPAddress, Options], []).

%%------------------------------------------------------------------------------
%% @doc
%% Basically the same as {@link rpc/4}, but the number of allowed retransmits
%% is taken from the `eipmi' application configuration. If not configured the
%% default number of allowed retransmits is `2'.
%% @see rpc/4
%% @end
%%------------------------------------------------------------------------------
-spec rpc(pid(), eipmi:request(), proplists:proplist()) ->
                 {ok, proplists:proplist()} | {error, term()}.
rpc(Pid, Request, Properties) ->
    Retransmits = eipmi_util:get_env(retransmits, ?RETRANSMITS),
    rpc(Pid, Request, Properties, Retransmits).

%%------------------------------------------------------------------------------
%% @doc
%% Send a synchronous IPMI RPC over this session. If the session is not yet
%% established the request will be queued. The last arguments specifies the
%% number of allowed retransmits. Since requests get sent over UDP packet
%% delivery may be unreliable and requests could get lost.
%% @end
%%------------------------------------------------------------------------------
-spec rpc(pid(), eipmi:request(), proplists:proplist(), non_neg_integer()) ->
                 {ok, proplists:proplist()} | {error, term()}.
rpc(Pid, Request, Properties, Retransmits) ->
    F = fun() -> gen_server:call(Pid, {rpc, Request, Properties}, infinity) end,
    rpc_(F(), F, Retransmits).
rpc_({error, timeout}, Fun, Retransmits) when Retransmits > 0 ->
    rpc_(Fun(), Fun, Retransmits - 1);
rpc_(Result, _Fun, _Retransmits) ->
    Result.

%%------------------------------------------------------------------------------
%% @doc
%% Stop the server, close the session.
%% @end
%%------------------------------------------------------------------------------
-spec stop(pid()) ->
                  ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

-record(state, {
          active = false :: boolean(),
          keep_alive     :: non_neg_integer(),
          last_send      :: non_neg_integer(),
          queue = []     :: [{eipmi:request(), proplists:proplist(), term()}],
          requests = []  :: [{rpc | internal, 0..63, reference(), term()}],
          session        :: eipmi:session(),
          address        :: inet:ip_address() | inet:hostname(),
          socket         :: gen_udp:socket(),
          properties     :: [property()]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([S, Addr, Options]) ->
    process_flag(trap_exit, true),
    {ok, Sock} = gen_udp:open(0, [binary]),
    Opts = eipmi_util:merge_vals(Options, ?DEFAULTS),
    {ok,
     process_request(
       {{?IPMI_NETFN_APPLICATION_REQUEST,
         ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES}, [],
        fun handle_get_channel_authentication_capabilites_response/2},
       #state{session = S, address = Addr, socket = Sock, properties = Opts})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({rpc, Request, Arguments}, From, State = #state{active = true}) ->
    {noreply, process_request({Request, Arguments, From}, State)};
handle_call({rpc, Request, Arguments}, From, State = #state{queue = Q}) ->
    {noreply, State#state{queue = Q ++ [{Request, Arguments, From}]}};
handle_call(Request, _From, State) ->
    {reply, undef, fire({unhandled, {call, Request}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {noreply, fire({unhandled, {cast, Request}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp_closed, Socket}, S) when S#state.socket =:= Socket ->
    {stop, socket_closed, S};
handle_info({udp, Socket, _, _, Bin}, S) when S#state.socket =:= Socket ->
    {noreply, handle_rmcp(eipmi_decoder:packet(Bin), S)};
handle_info({timeout, RqSeqNr}, State) ->
    NewState1 = fire({timeout, RqSeqNr}, State),
    {Requests, NewState2} = unregister_request(RqSeqNr, NewState1),
    {noreply, lists:foldl(reply({error, timeout}), NewState2, Requests)};
handle_info(keep_alive, State) ->
    {noreply, keep_alive(to_millis(os:timestamp()), State)};
handle_info(Info, State) ->
    {noreply, fire({unhandled, {info, Info}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(Reason, State = #state{requests = Requests}) ->
    NewState = lists:foldl(reply({error, {closed, Reason}}), State, Requests),
    fire({closed, Reason}, maybe_close_session(NewState)),
    gen_udp:close(NewState#state.socket),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_rmcp({ok, Packet = #rmcp_ipmi{header = Header}}, State) ->
    handle_ipmi(Packet, maybe_send_ack(Header, State));
handle_rmcp({ok, #rmcp_ack{}}, State) ->
    State;
handle_rmcp({ok, #rmcp_asf{header = Header}}, State) ->
    maybe_send_ack(Header, State);
handle_rmcp({error, Reason}, State) ->
    fire({decode_error, Reason}, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ipmi(Packet = #rmcp_ipmi{properties = Properties}, State) ->
    RqSeqNr = eipmi_util:get_val(rq_seq_nr, Properties),
    handle_ipmi_(get_response(Packet), unregister_request(RqSeqNr, State)).
handle_ipmi_(Message, {[], State}) ->
    fire({unhandled, {ipmi, Message}}, State);
handle_ipmi_(Message, {Requests, State}) ->
    lists:foldl(reply(Message), State, Requests).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_response(Packet = #rmcp_ipmi{properties = Ps}) ->
    get_response(eipmi_util:get_val(completion, Ps), Packet).
get_response(normal, #rmcp_ipmi{cmd = Cmd, data = Data}) ->
    %% Currently all incoming packets are considered to be IPMI responses
    %% and outbound sequence numbers are not tracked/checked.
    {ok, eipmi_response:decode(Cmd, Data)};
get_response(Completion, _Packet) ->
    {error, {bmc_error, Completion}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
register_request(RqSeqNr, Receiver, State = #state{requests = Rs}) ->
    Timeout = get_state_val(timeout, State),
    Ref = erlang:send_after(Timeout, self(), {timeout, RqSeqNr}),
    Tag = case is_function(Receiver) of true -> internal; _ -> external end,
    State#state{requests = [{Tag, RqSeqNr, Ref, Receiver} | Rs]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unregister_request(RqSeqNr, State = #state{requests = Rs}) ->
    Pred = fun({_, S, _, _}) -> RqSeqNr =:= S end,
    {Request, NewRs} = lists:partition(Pred, Rs),
    {Request, State#state{requests = NewRs}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_request({Request, Arguments, Receiver}, State) ->
    {RqSeqNr, NewState} = send_request(Request, Arguments, State),
    register_request(RqSeqNr, Receiver, NewState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_request(Request, Arguments, State = #state{properties = Ps}) ->
    Data = eipmi_request:encode(Request, eipmi_util:merge_vals(Arguments, Ps)),
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    Bin = eipmi_encoder:ipmi(Header, Ps, Request, Data),
    incr_rq_seq_nr(incr_inbound_seq_nr(udp_send(Bin, State))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
incr_rq_seq_nr(State) ->
    RqSeqNr = get_state_val(rq_seq_nr, State),
    {RqSeqNr, update_state_val(rq_seq_nr, (RqSeqNr + 1) rem 16#40, State)}.

%%------------------------------------------------------------------------------
%% @private
%% if prior to a session the inbound seq nr will not be updated
%%------------------------------------------------------------------------------
incr_inbound_seq_nr(State = #state{active = false}) ->
    State;
incr_inbound_seq_nr(State) ->
    SeqNr = get_state_val(inbound_seq_nr, State),
    update_state_val(inbound_seq_nr, (SeqNr + 1) rem (16#ffffffff + 1), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_close_session(State = #state{active = false}) ->
    State;
maybe_close_session(State) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?CLOSE_SESSION},
    {_, NewState} = send_request(Request, [], State),
    NewState.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_send_ack(#rmcp_header{seq_nr = ?RMCP_NOREPLY}, State) ->
    State;
maybe_send_ack(Header, State) ->
    udp_send(eipmi_encoder:ack(Header#rmcp_header{class = ?RMCP_ASF}), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
udp_send(Bin, S = #state{socket = Socket, address = IPAddress}) ->
    Port = eipmi_util:get_val(port, S#state.properties),
    ok = gen_udp:send(Socket, IPAddress, Port, Bin),
    S#state{last_send = to_millis(os:timestamp())}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_get_channel_authentication_capabilites_response({ok, Fields}, State) ->
    process_request(
      {{?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SESSION_CHALLENGE}, [],
       fun handle_get_session_challenge_response/2},
      select_auth(Fields, select_login(Fields, State))).

%%------------------------------------------------------------------------------
%% @private
%% Selects an authentication method from the available methods. Preferred
%% selection order is none, pwd, md5, md2.
%%------------------------------------------------------------------------------
select_auth(Fields, State) ->
    AuthTypes = eipmi_util:get_val(auth_types, Fields),
    None = lists:member(none, AuthTypes),
    Pwd = lists:member(pwd, AuthTypes),
    Md5 = lists:member(md5, AuthTypes),
    Md2 = lists:member(md2, AuthTypes),
    case {None, Pwd, Md5, Md2} of
        {true, _, _, _} ->
            update_state_val(auth_type, none, State);
        {false, true, _, _} ->
            update_state_val(auth_type, pwd, State);
        {false, false, true, _} ->
            update_state_val(auth_type, md5, State);
        {false, false, false, true} ->
            update_state_val(auth_type, md2, State)
    end.

%%------------------------------------------------------------------------------
%% @private
%% Selects a login method from the available methods. Preferred selection order
%% is anonymous, null, non_null. In case anonymous is selected the user and
%% password contained in the state will be reset, in case of null user only the
%% state user will be reset, otherwise user and password must exist and must
%% have sensible values in the state.
%%------------------------------------------------------------------------------
select_login(Fields, State) ->
    Logins = eipmi_util:get_val(login_status, Fields),
    case {lists:member(anonymous, Logins), lists:member(null, Logins)} of
        {true, _} ->
            update_state_val(password, "", update_state_val(user, "", State));
        {false, true} ->
            update_state_val(user, "", State);
        {false, false} ->
            State
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_get_session_challenge_response({ok, Fields}, State) ->
    process_request(
      {{?IPMI_NETFN_APPLICATION_REQUEST, ?ACTIVATE_SESSION}, [],
       fun handle_activate_session_response/2},
      copy_state_vals([challenge, session_id], Fields, State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_activate_session_response({ok, Fields}, State) ->
    process_request(
      {{?IPMI_NETFN_APPLICATION_REQUEST, ?SET_SESSION_PRIVILEGE_LEVEL}, [],
       fun handle_set_session_privilege_response/2},
      copy_state_vals([auth_type, session_id, inbound_seq_nr], Fields, State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_set_session_privilege_response({ok, Fields}, State) ->
    handle_set_session_privilege_response(
      eipmi_util:get_val(privilege, Fields) =:= get_state_val(privilege, State),
      Fields, State).
handle_set_session_privilege_response(true, _, State = #state{queue = Q}) ->
    NewState = State#state{active = true, queue = []},
    keep_alive(
      to_millis(os:timestamp()),
      fire(established, lists:foldl(fun process_request/2, NewState, Q))).

%%------------------------------------------------------------------------------
%% @private
%% According to spec we need to send a request at least every 60000ms. What we
%% do is to keep track of the timestamp of the last packet. A periodical timer
%% will then lookup this timestamp. If the last packet timestamp plus the next
%% timer duration is still under 60000ms we do nothing (other packets will)
%% probably be sent in this period anyways. In the other case we need to send a
%% keep-alive request.
%%------------------------------------------------------------------------------
keep_alive(Now, State = #state{last_send = Last})
  when Now - Last + ?KEEP_ALIVE_TIMER < 55000 ->
    start_keep_alive_timer(State);
keep_alive(_, State) ->
    send_keep_alive(State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_keep_alive({error, timeout}, S = #state{keep_alive = C}) when C > 0 ->
    send_keep_alive(S#state{keep_alive = C - 1});
handle_keep_alive({ok, _Fields}, State) ->
    start_keep_alive_timer(State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_keep_alive_timer(State) ->
    erlang:send_after(?KEEP_ALIVE_TIMER, self(), keep_alive),
    State#state{keep_alive = get_state_val(keep_alive_retransmits, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_keep_alive(State) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?SET_SESSION_PRIVILEGE_LEVEL},
    process_request({Request, [], fun handle_keep_alive/2}, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply(Message) ->
    fun(Request, State) -> reply(Message, Request, State) end.
reply(Message, {Type, _RqSeqNr, TimerRef, Receiver}, State) ->
    erlang:cancel_timer(TimerRef),
    reply(Message, Type, Receiver, State).
reply(Message, external, From, State) ->
    catch gen_server:reply(From, Message),
    State;
reply(Message, internal, Fun, State) ->
    Fun(Message, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
fire(Event, State = #state{session = Session, address = Address}) ->
    eipmi_events:fire(Session, Address, Event),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_millis({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_state_val(Property, #state{properties = Ps}) ->
    eipmi_util:get_val(Property, Ps).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_state_val(Property, Value, State = #state{properties = Ps}) ->
    State#state{properties = eipmi_util:update_val(Property, Value, Ps)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_val(Property, Fields, S = #state{properties = Ps}) ->
    S#state{properties = eipmi_util:copy_val(Property, Ps, Fields)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_vals(PropertyList, Fields, State) ->
    Action = fun(P, Acc) -> copy_state_val(P, Fields, Acc) end,
    lists:foldl(Action, State, PropertyList).
