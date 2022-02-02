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
%%% Currently all incoming packets are considered to be IPMI responses
%%% and outbound sequence numbers are not tracked/checked.
%%%
%%% TODO:
%%% * Use Activate Session requests for session keep-alive
%%% @end
%%%=============================================================================
-module(eipmi_session).

-behaviour(gen_server).

%% API
-export([start_link/2,
         rpc/3,
         rpc/4,
         stop/2]).

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
        {rq_auth_type, none | pwd | md5 | md2} |
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
        rq_auth_type |
        rq_seq_nr |
        session_id.

-export_type([property/0, property_name/0]).

-define(KEEP_ALIVE_TIMER, 45000).

%%------------------------------------------------------------------------------
%% Session defaults, partially modifyable by the user.
%%------------------------------------------------------------------------------
-define(DEFAULTS,
        [
         %% default values modifyable through eipmi:open/2
         {initial_outbound_seq_nr, 16#1337},
         {keep_alive_retransmits, ?IPMI_RETRANSMITS},
         {password, ""},
         {port, ?RMCP_PORT_NUMBER},
         {privilege, administrator},
         {rq_addr, ?IPMI_REQUESTOR_ADDR},
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
-spec start_link(eipmi:session(), [property()]) -> {ok, pid()} | {error, term()}.
start_link(Session, Options) ->
    gen_server:start_link(?MODULE, [Session, Options], []).

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
    Retransmits = eipmi_util:get_env(retransmits, ?IPMI_RETRANSMITS),
    rpc(Pid, Request, Properties, Retransmits).

%%------------------------------------------------------------------------------
%% @doc
%% Send a synchronous IPMI RPC over this session. The last arguments specifies
%% the number of allowed retransmits. Since requests get sent over UDP packet
%% delivery may be unreliable and requests could get lost.
%% @end
%%------------------------------------------------------------------------------
-spec rpc(pid(), eipmi:request(), proplists:proplist(), non_neg_integer()) ->
                 {ok, proplists:proplist()} | {error, term()}.
rpc(Pid, Request, Properties, Retransmits) ->
    Data = eipmi_request:encode(Request, Properties),
    F = fun() -> gen_server:call(Pid, {rpc, Request, Data}, infinity) end,
    rpc_(F(), F, Retransmits).
rpc_({error, timeout}, Fun, Retransmits) when Retransmits > 0 ->
    rpc_(Fun(), Fun, Retransmits - 1);
rpc_(Result, _Fun, _Retransmits) ->
    Result.

%%------------------------------------------------------------------------------
%% @doc
%% Stop the session process with the specified reason. This is a handy function
%% to be used by e.g. {@link eipmi_poll} to be able to notify errors to the
%% session server.
%% @end
%%------------------------------------------------------------------------------
-spec stop(pid(), term()) -> ok.
stop(Pid, Reason) -> gen_server:cast(Pid, {stop, Reason}).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

-type rq() :: {eipmi:response(), 0..63}.

-record(state, {
          owner          :: {pid(), reference()},
          keep_alive     :: non_neg_integer() | undefined,
          last_send      :: non_neg_integer() | undefined,
          requests = []  :: [{rq(), reference(), term()}],
          session        :: eipmi:session(),
          address        :: inet:ip_address() | inet:hostname(),
          socket         :: inet:socket(),
          properties     :: [property()]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Session = {session, {Addr, _}, {Owner, _}}, Options]) ->
    process_flag(trap_exit, true),
    Ref = erlang:monitor(process, Owner),
    Opts = eipmi_util:merge_vals(Options, ?DEFAULTS),
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
    State = #state{owner = {Owner, Ref},
                   session = Session,
                   address = Addr,
                   socket = Sock,
                   properties = Opts},
    try
        {ok, State1} = get_authentication_capabilities(State),
        {ok, State2} = get_session_challenge(State1),
        {ok, State3} = activate_session(State2),
        {ok, State4} = set_session_privilege_level(State3),
        ok = inet:setopts(Sock, [{active, true}]),
        {ok, State4}
    catch
        C:E -> {stop, {shutdown, {C, E}}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({rpc, Request, Data}, From, State) ->
    {noreply, process_request({Request, Data, From}, State)};
handle_call(Request, _From, State) ->
    {reply, undef, fire({unhandled, {call, Request}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, {shutdown, Reason}, State};
handle_cast(Request, State) ->
    {noreply, fire({unhandled, {cast, Request}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp_closed, Socket}, State = #state{socket = Socket}) ->
    {stop, socket_closed, State};
handle_info({udp, Socket, _, _, Bin}, State = #state{socket = Socket}) ->
    try {noreply, handle_rmcp(eipmi_decoder:packet(Bin, State#state.properties), State)}
    catch
        C:E -> {stop, {C, E}, State}
    end;
handle_info({timeout, Rq = {{NetFn, Cmd}, RqSeqNr}}, State) ->
    NewState1 = fire({timeout, {{NetFn - 1, Cmd}, RqSeqNr}}, State),
    {Requests, NewState2} = unregister_request(Rq, NewState1),
    try {noreply, lists:foldl(reply({error, timeout}), NewState2, Requests)}
    catch
        C:E -> {stop, {C, E}, NewState2}
    end;
handle_info(keep_alive, State) ->
    {noreply, keep_alive(to_millis(os:timestamp()), State)};
handle_info({'DOWN', R, process, P, Reason}, State = #state{owner = {P, R}}) ->
    {stop, {shutdown, {owner_exited, Reason}}, State};
handle_info(Info, State) ->
    {noreply, fire({unhandled, {info, Info}}, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(Reason, State = #state{socket = Socket, requests = Requests}) ->
    NewState = lists:foldl(reply({error, {closed, Reason}}), State, Requests),
    fire({closed, Reason}, NewState),
    close_session(NewState),
    gen_udp:close(Socket),
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
get_authentication_capabilities(State = #state{socket = Socket}) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST,
               ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES},
    Data = eipmi_request:encode(Request, State#state.properties),
    State1 = element(2, send_request(Request, Data, State)),
    Timeout = get_state_val(timeout, State1),
    {ok, {_, _, Bin}} = gen_udp:recv(Socket, 2000, Timeout),
    {ok, Packet = #rmcp_ipmi{header = Header}} = eipmi_decoder:packet(Bin, State#state.properties),
    State2 = maybe_send_ack(Header, State1),
    {ok, Fields} = get_response(Packet),
    {ok, select_auth(Fields, check_login(Fields, State2))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_challenge(State = #state{socket = Socket}) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SESSION_CHALLENGE},
    Data = eipmi_request:encode(Request, State#state.properties),
    State1 = element(2, send_request(Request, Data, State)),
    Timeout = get_state_val(timeout, State1),
    {ok, {_, _, Bin}} = gen_udp:recv(Socket, 2000, Timeout),
    {ok, Packet = #rmcp_ipmi{header = Header}} = eipmi_decoder:packet(Bin, State1#state.properties),
    State2 = maybe_send_ack(Header, State1),
    AuthType = get_state_val(rq_auth_type, State2),
    State3 = update_state_val(auth_type, AuthType, State2),
    {ok, Fields} = get_response(Packet),
    {ok, copy_state_vals([challenge, session_id], Fields, State3)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
activate_session(State = #state{socket = Socket}) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?ACTIVATE_SESSION},
    Data = eipmi_request:encode(Request, State#state.properties),
    State1 = element(2, send_request(Request, Data, State)),
    Timeout = get_state_val(timeout, State1),
    {ok, {_, _, Bin}} = gen_udp:recv(Socket, 2000, Timeout),
    {ok, Packet = #rmcp_ipmi{header = Header}} = eipmi_decoder:packet(Bin, State1#state.properties),
    State2 = maybe_send_ack(Header, State1),
    {ok, Fields} = get_response(Packet),
    {ok, copy_state_vals([auth_type, session_id, inbound_seq_nr], Fields, State2)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_session_privilege_level(State = #state{socket = Socket}) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?SET_SESSION_PRIVILEGE_LEVEL},
    Data = eipmi_request:encode(Request, State#state.properties),
    State1 = element(2, send_request(Request, Data, State)),
    Timeout = get_state_val(timeout, State1),
    {ok, {_, _, Bin}} = gen_udp:recv(Socket, 2000, Timeout),
    {ok, Packet = #rmcp_ipmi{header = Header}} = eipmi_decoder:packet(Bin, State1#state.properties),
    State2 = maybe_send_ack(Header, State1),
    {ok, Fields} = get_response(Packet),
    RequestedPrivilege = get_state_val(privilege, State),
    assert(RequestedPrivilege == proplists:get_value(privilege, Fields),
           failed_to_set_requested_privilege),
    {ok, keep_alive(to_millis(os:timestamp()), fire(established, State2))}.

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
handle_ipmi(Packet = #rmcp_ipmi{cmd = Response, properties = Ps}, State) ->
    Rq = {Response, proplists:get_value(rq_seq_nr, Ps)},
    handle_ipmi_(get_response(Packet), unregister_request(Rq, State)).
handle_ipmi_(Message, {Requests, State}) ->
    lists:foldl(reply(Message), State, Requests).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_response(Packet = #rmcp_ipmi{properties = Ps}) ->
    get_response(proplists:get_value(completion, Ps), Packet).
get_response(normal, #rmcp_ipmi{cmd = Cmd, data = Data}) ->
    eipmi_response:decode(Cmd, Data);
get_response(Completion, _Packet) ->
    {error, {bmc_error, Completion}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
register_request(Rq, Receiver, State = #state{requests = Rs}) ->
    Timeout = get_state_val(timeout, State),
    Ref = erlang:send_after(Timeout, self(), {timeout, Rq}),
    State#state{requests = [{Rq, Ref, Receiver} | Rs]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unregister_request(Rq, State = #state{requests = Rs}) ->
    {Requests, NewRs} = lists:partition(fun({R, _, _}) -> Rq =:= R end, Rs),
    {Requests, State#state{requests = NewRs}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_request({{NetFn, Cmd}, Data, Receiver}, State) ->
    {RqSeqNr, NewState} = send_request({NetFn, Cmd}, Data, State),
    NextState = incr_inbound_seq_nr(NewState),
    %% Response NetFn is request NetFn + 1
    register_request({{NetFn + 1, Cmd}, RqSeqNr}, Receiver, NextState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_request(Request, Data, State = #state{properties = Ps}) ->
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    Bin = eipmi_encoder:ipmi(Header, Ps, Request, Data),
    incr_rq_seq_nr(udp_send(Bin, State)).

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
incr_inbound_seq_nr(State) ->
    SeqNr = get_state_val(inbound_seq_nr, State),
    update_state_val(inbound_seq_nr, (SeqNr + 1) rem (16#ffffffff + 1), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
close_session(State) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?CLOSE_SESSION},
    Data = eipmi_request:encode(Request, State#state.properties),
    catch send_request(Request, Data, State),
    ok.

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
    Port = proplists:get_value(port, S#state.properties),
    ok = gen_udp:send(Socket, IPAddress, Port, Bin),
    S#state{last_send = to_millis(os:timestamp())}.

%%------------------------------------------------------------------------------
%% @private
%% Selects an authentication method from the available methods. Preferred
%% selection order is md5, md2, pwd, none.
%%------------------------------------------------------------------------------
select_auth(Fields, State) ->
    AuthTypes = proplists:get_value(auth_types, Fields),
    RqAuthType = get_state_val(rq_auth_type, State),
    case lists:member(RqAuthType, AuthTypes) of
        true ->
            State;
        _ ->
            None = lists:member(none, AuthTypes),
            Pwd = lists:member(pwd, AuthTypes),
            Md5 = lists:member(md5, AuthTypes),
            Md2 = lists:member(md2, AuthTypes),
            case {Md5, Md2, Pwd, None} of
                {true, _, _, _} ->
                    update_state_val(rq_auth_type, md5, State);
                {false, true, _, _} ->
                    update_state_val(rq_auth_type, md2, State);
                {false, false, true, _} ->
                    update_state_val(rq_auth_type, pwd, State);
                {false, false, false, true} ->
                    update_state_val(rq_auth_type, none, State)
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% Checks the configured user and password according to the available login
%% methods. In case of anonymous login no field is required. While the null user
%% login requires at least a password, the normal login will need username and
%% password set.
%%------------------------------------------------------------------------------
check_login(Fields, State) ->
    Logins = proplists:get_value(login_status, Fields),
    case {lists:member(anonymous, Logins), lists:member(null, Logins)} of
        {true, _} ->
            ok;
        {false, true} ->
            assert(get_state_val(password, State) /= "", password_required);
        {false, false} ->
            assert(get_state_val(user, State) /= "", username_required),
            assert(get_state_val(password, State) /= "", password_required)
    end,
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
assert(true, _) -> ok;
assert(false, MsgTerm) -> throw(MsgTerm).

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
start_keep_alive_timer(State) ->
    erlang:send_after(?KEEP_ALIVE_TIMER, self(), keep_alive),
    State#state{keep_alive = get_state_val(keep_alive_retransmits, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_keep_alive(State) ->
    Request = {?IPMI_NETFN_APPLICATION_REQUEST, ?SET_SESSION_PRIVILEGE_LEVEL},
    Data = eipmi_request:encode(Request, State#state.properties),
    process_request({Request, Data, keep_alive}, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply(Message) ->
    fun(Request, State) -> reply(Message, Request, State) end.
reply(Message, {_RqSeqNr, TimerRef, Receiver}, State) ->
    erlang:cancel_timer(TimerRef),
    reply_(Message, Receiver, State).
reply_({error, timeout}, keep_alive, S = #state{keep_alive = C}) when C > 0 ->
    send_keep_alive(S#state{keep_alive = C - 1});
reply_({error, _}, keep_alive, _State) ->
    throw(lost_connection);
reply_({ok, _}, keep_alive, State) ->
    start_keep_alive_timer(State);
reply_(Message, From, State) ->
    catch gen_server:reply(From, Message),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
fire(Event, State = #state{session = Session, address = Address}) ->
    element(1, State#state.owner) ! {ipmi, Session, Address, Event},
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
    proplists:get_value(Property, Ps).

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
