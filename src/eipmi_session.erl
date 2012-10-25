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
%%% A state machine providing session management for IPMI over lan channels.
%%% The session will be established as soon as the state machine gets started.
%%% An established session will be closed when the state machine terminates.
%%% The user can close the session using {@link stop/1}.
%%%
%%% Synchronous requests can be issued over this session at any given time using
%%% {@link request/3}. When the session is not yet established requests will be
%%% queued and issued as soon as the far end (BMC) is ready. Request timeouts
%%% can be configured on state machine startup using the `timeout' property
%%% of the `Options' field.
%%%
%%% A session may be shared between mutliple processes. While the requests of
%%% one process will be synchronous and thus ordered, requests from different
%%% processes will not block each other.
%%%
%%% A session will use the modules {@link eipmi_request} and
%%% {@link eipmi_response} to encode and decode requests/responses. Therefore,
%%% there's no need to edit the session but extending these modules when
%%% support for new requests/responses is added.
%%%
%%% @TODO Implement session keep alive.
%%% @end
%%%=============================================================================
-module(eipmi_session).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
         request/3,
         stop/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4,
         activating/2,
         active/2]).

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

-type handler(State) :: fun(({error, term()} | {ok, #rmcp_ipmi{}},
                             activating | active,
                             State) ->
                                   {activating | active, State}).

-export_type([property/0, property_name/0]).

%%------------------------------------------------------------------------------
%% Session defaults, partially modifyable by the user.
%%------------------------------------------------------------------------------
-define(DEFAULTS,
        [
         %% default values modifyable through eipmi:open/2
         {initial_outbound_seq_nr, 16#1337},
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
%% Start a session state machine. This will also setup a session by sending the
%% necessary IPMI protocol messages.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(eipmi:session(),
                 inet:ip_address() | inet:hostname(),
                 [property()]) ->
                        {ok, pid()} | {error, term()}.
start_link(Session, IPAddress, Options) ->
    gen_fsm:start_link(?MODULE, [Session, IPAddress, Options], []).

%%------------------------------------------------------------------------------
%% @doc
%% Send a synchronous IPMI request over this session. If the session is not yet
%% established the request will be queued.
%% @end
%%------------------------------------------------------------------------------
-spec request(pid(), eipmi:request(), proplists:proplist()) ->
                     {ok, proplists:proplist()} | {error, term()}.
request(Pid, Request, Properties) ->
    gen_fsm:sync_send_all_state_event(Pid, {request, Request, Properties}).

%%------------------------------------------------------------------------------
%% @doc
%% Stop the server, close the session.
%% @end
%%------------------------------------------------------------------------------
-spec stop(pid()) ->
                  ok.
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%%=============================================================================
%%% gen_fsm Callbacks
%%%=============================================================================

-record(state, {
          session    :: eipmi:session(),
          address    :: string(),
          socket     :: gen_udp:socket(),
          queued     :: [{eipmi:request(), proplists:proplist(), handler(#state{})}],
          pending    :: [{0..63, reference(), handler(#state{})}],
          properties :: [property()]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Session, IPAddress, Opts]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0, [binary]),
    Options = eipmi_util:merge_vals(Opts, ?DEFAULTS),
    Queued = [{{?IPMI_NETFN_APPLICATION_REQUEST,
                ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES}, [],
               get_channel_capabilities_handler()},
              {{?IPMI_NETFN_APPLICATION_REQUEST,
                ?GET_SESSION_CHALLENGE}, [],
               get_session_challenge_handler()},
              {{?IPMI_NETFN_APPLICATION_REQUEST,
                ?ACTIVATE_SESSION}, [],
               get_activate_session_handler()},
              {{?IPMI_NETFN_APPLICATION_REQUEST,
                ?SET_SESSION_PRIVILEGE_LEVEL}, [],
               get_session_privilege_handler()}],
    {ok, activating,
     process_next_request(
       false,
       #state{session = Session,
              address = IPAddress,
              socket = Socket,
              queued = Queued,
              pending = [],
              properties = Options})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event({request, Req, Properties}, From, active, State) ->
    Request = {Req, Properties, get_reply_handler(From)},
    NewState = process_next_request(true, enqueue_request(Request, State)),
    {next_state, active, NewState};
handle_sync_event({request, Req, Properties}, From, StateName, State) ->
    Request = {Req, Properties, get_reply_handler(From)},
    {next_state, StateName, enqueue_request(Request, State)};
handle_sync_event(Event, _From, StateName, State) ->
    {reply, {undef, Event}, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    error_logger:info_msg("unhandled event ~p in state ~p", [Event, StateName]),
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp, So, _, _, Bin}, StateName, S) when S#state.socket =:= So ->
    handle_packet(eipmi_decoder:packet(Bin), StateName, S);
handle_info(Info, StateName, State) ->
    error_logger:info_msg("unhandled info ~p in state ~p", [Info, StateName]),
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, active, State = #state{socket = Socket}) ->
    clear_requests(active, State),
    send_request({?IPMI_NETFN_APPLICATION_REQUEST, ?CLOSE_SESSION}, [], State),
    gen_udp:close(Socket),
    ok;
terminate(_Reason, StateName, State = #state{socket = Socket}) ->
    clear_requests(StateName, State),
    gen_udp:close(Socket),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
activating({timeout, RqSeqNr}, State) ->
    {NewStateName, NewState} = handle_request_timeout(RqSeqNr, activating, State),
    {next_state, NewStateName, NewState}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
active({timeout, RqSeqNr}, State) ->
    {NewStateName, NewState} = handle_request_timeout(RqSeqNr, active, State),
    {next_state, NewStateName, NewState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_packet({ok, I = #rmcp_ipmi{header = Header}}, StateName, State) ->
    maybe_send_ack(Header, State),
    {NewStateName, NewState} = handle_request_response(I, StateName, State),
    {next_state, NewStateName,
     process_next_request(NewStateName =:= active, NewState)};
handle_packet({ok, Ack = #rmcp_ack{}}, StateName, State) ->
    error_logger:info_msg("unhandled ACK message ~p", [Ack]),
    {next_state, StateName, State};
handle_packet({ok, Asf = #rmcp_asf{header = Header}}, StateName, State) ->
    maybe_send_ack(Header, State),
    error_logger:info_msg("unhandled ASF message ~p", [Asf]),
    {next_state, StateName, State};
handle_packet({error, Reason}, StateName, State) ->
    error_logger:info_msg("failed to decode packet ~p", [Reason]),
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_send_ack(#rmcp_header{seq_nr = ?RMCP_NOREPLY}, _State) ->
    ok;
maybe_send_ack(Header, State) ->
    udp_send(eipmi_encoder:ack(Header#rmcp_header{class = ?RMCP_ASF}), State),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% if prior to a session the inbound seq nr will not be updated
%%------------------------------------------------------------------------------
process_next_request(_SessionActive, State = #state{queued = []}) ->
    State;
process_next_request(false, State = #state{queued = [Request | Rest]}) ->
    process_request(Request, State#state{queued = Rest});
process_next_request(true, State = #state{queued = [Request | Rest]}) ->
    incr_inbound_seq_nr(process_request(Request, State#state{queued = Rest})).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_request({Req, Properties, Handler}, State) ->
    {RqSeqNr, NewState} = send_request(Req, Properties, State),
    register_request(RqSeqNr, Handler, NewState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_request(Req, Properties, State = #state{properties = Ps}) ->
    Data = eipmi_request:encode(Req, eipmi_util:merge_vals(Properties, Ps)),
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    udp_send(eipmi_encoder:ipmi(Header, Ps, Req, Data), State),
    incr_rq_seq_nr(State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
udp_send(Binary, S = #state{socket = Socket, address = IPAddress}) ->
    ok = gen_udp:send(Socket, IPAddress, get_state_val(port, S), Binary).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_request_response(I = #rmcp_ipmi{properties = Ps}, StateName, State) ->
    RqSeqNr = eipmi_util:get_val(rq_seq_nr, Ps),
    {Response, NewState} = check_seq_nr(check_completion({{ok, I}, State})),
    unregister_request(RqSeqNr, Response, StateName, NewState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_request_timeout(RqSeqNr, StateName, State) ->
    unregister_request(RqSeqNr, {error, timeout}, StateName, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unregister_request(RqSeqNr, Response, StateName, State = #state{pending = P}) ->
    case lists:keyfind(RqSeqNr, 1, P) of
        false ->
            {StateName, State};
        {RqSeqNr, TimerRef, Handler} ->
            gen_fsm:cancel_timer(TimerRef),
            Handler(
              Response,
              StateName,
              State#state{pending = lists:keydelete(RqSeqNr, 1, P)})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
register_request(RqSeqNr, Handler, State = #state{pending = P}) ->
    Timeout = get_state_val(timeout, State),
    TimerRef = gen_fsm:send_event_after(Timeout, {timeout, RqSeqNr}),
    State#state{pending = [{RqSeqNr, TimerRef, Handler} | P]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
enqueue_request(Request, State = #state{queued = Q}) ->
    State#state{queued = Q ++ [Request]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
clear_requests(StateName, State = #state{queued = Q, pending = P}) ->
    [catch Handler({error, closed}, StateName, State) || {_, _, Handler} <- P],
    [catch Handler({error, closed}, StateName, State) || {_, _, Handler} <- Q].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
incr_inbound_seq_nr(State) ->
    SeqNr = get_state_val(inbound_seq_nr, State),
    update_state_val(inbound_seq_nr, (SeqNr + 1) rem (16#ffffffff + 1), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
incr_rq_seq_nr(State) ->
    SeqNr = get_state_val(rq_seq_nr, State),
    {SeqNr, update_state_val(rq_seq_nr, (SeqNr + 1) rem 16#40, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_reply_handler(From) ->
    fun({error, Error}, StateName, State) ->
            catch gen_fsm:reply(From, {error, Error}),
            {StateName, State};
       ({ok, #rmcp_ipmi{cmd = Resp, data = D}}, StateName, State) ->
            Fields = eipmi_response:decode(Resp, D),
            catch gen_fsm:reply(From, {ok, Fields}),
            {StateName, State}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_channel_capabilities_handler() ->
    fun({ok, #rmcp_ipmi{cmd = Resp = {_, C}, data = D}}, activating, State)
          when C =:= ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES ->
            Fields = eipmi_response:decode(Resp, D),
            {activating, select_auth(Fields, select_login(Fields, State))}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_challenge_handler() ->
    fun({ok, #rmcp_ipmi{cmd = Resp = {_, C}, data = D}}, activating, State)
          when C =:= ?GET_SESSION_CHALLENGE ->
            {activating,
             copy_state_vals(
               [challenge, session_id],
               eipmi_response:decode(Resp, D),
               State)}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_activate_session_handler() ->
    fun({ok, #rmcp_ipmi{cmd = Resp = {_, C}, data = D}}, activating, State)
          when C =:= ?ACTIVATE_SESSION ->
            {active,
             copy_state_vals(
               [auth_type, session_id, inbound_seq_nr],
               eipmi_response:decode(Resp, D),
               State)}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_privilege_handler() ->
    fun({ok, #rmcp_ipmi{cmd = Resp = {_, C}, data = D}}, active, State)
          when C =:= ?SET_SESSION_PRIVILEGE_LEVEL ->
            Fields = eipmi_response:decode(Resp, D),
            Requested = get_state_val(privilege, State),
            Actual = eipmi_util:get_val(privilege, Fields),
            Requested = Actual,
            {active, State}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_completion(In = {{ok, #rmcp_ipmi{properties = Ps}}, State}) ->
    case eipmi_util:get_val(completion, Ps) of
        normal ->
            In;
        Completion ->
            {{error, {bmc_error, Completion}}, State}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_seq_nr(In = {{error, _}, _}) ->
    In;
check_seq_nr(In = {Ok = {ok, #rmcp_ipmi{properties = Ps}}, State}) ->
    SeqNr = eipmi_util:get_val(outbound_seq_nr, Ps),
    case get_state_val(outbound_seq_nr, State) of
        StateSeqNr when SeqNr >= StateSeqNr ->
            {Ok, update_state_val(outbound_seq_nr, SeqNr, State)};
        StateSeqNr when SeqNr + 8 < StateSeqNr ->
            {{error, {seq_nr_too_old, SeqNr}}, State};
        _ ->
            In
    end.

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
    lists:foldl(
      fun(P, Acc) -> copy_state_val(P, Fields, Acc) end,
      State, PropertyList).
