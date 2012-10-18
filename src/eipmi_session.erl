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
%%% TODO
%%% Commands that can be sent prior to a session being established:
%%% * Get System GUID
%%% * Get Channel Authentication Capabilities
%%% * Get Session Challenge
%%% * Activate Session
%%% * Get Channel Cipher Suites
%%% * PET Acknowledge
%%% @end
%%%=============================================================================
-module(eipmi_session).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
         stop/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% gen_fsm states
-export([discovery/2,
         activation0/2,
         activation1/2,
         closing/2]).

-include("eipmi.hrl").

%%------------------------------------------------------------------------------
%% Session defaults in case the user does not provide these.
%%------------------------------------------------------------------------------
-define(SESSION_DEFAULTS,
        [?AUTH_TYPE(none),          %% initial packets are not authenticated
         ?INBOUND_SEQ_NR(0),        %% initial packets have the null seqnr
         ?OUTBOUND_SEQ_NR(8),
         ?PASSWORD(""),
         ?PRIVILEGE(administrator),
         ?RQ_ADDR(16#81),
         ?RQ_SEQ_NR(0),             %% initial requests have the null seqnr
         ?SESSION_ID(0),            %% initial have the null session id
         ?USER("")]).

%%------------------------------------------------------------------------------
%% Message defaults used to fill not provided session and request fields in
%% IPMI messages.
%%------------------------------------------------------------------------------
-define(MSG_DEFAULTS,
        [{net_fn, ?IPMI_NETFN_APPLICATION_REQUEST},
         {rq_lun, ?IPMI_REQUESTOR_LUN},
         {rs_addr, ?IPMI_RESPONDER_ADDR},
         {rs_lun, ?IPMI_RESPONDER_LUN}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start the server.
%% @end
%%------------------------------------------------------------------------------
start_link(Session, IPAddress, Options) ->
    gen_fsm:start_link(?MODULE, [Session, IPAddress, Options], []).

%%------------------------------------------------------------------------------
%% @doc
%% Stop the server, close the session.
%% @end
%%------------------------------------------------------------------------------
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%%=============================================================================
%%% gen_fsm Callbacks
%%%=============================================================================

-record(state, {
          session    :: eipmi:session(),
          address    :: string(),
          socket     :: gen_udp:socket(),
          properties :: proplists:proplist()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Session, IPAddress, Opts]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0, [binary]),
    Options = eipmi_util:merge_vals(Opts, ?SESSION_DEFAULTS),
    {ok, discovery,
     send_prior_session_request(
       ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
       #state{session = Session,
              address = IPAddress,
              socket = Socket,
              properties = Options})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    {reply, {undef, Event}, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(stop, active, State) ->
    {next_state, closing, send_prior_session_request(?CLOSE_SESSION, State)};
handle_event(stop, StateName, State) when StateName =/= closing ->
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
terminate(_Reason, _StateName, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%=============================================================================
%%% gen_fsm States
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% The only expected message while in discovery is the answer to the channel
%% capabilites request.
%%------------------------------------------------------------------------------
discovery({?GET_CHANNEL_AUTHENTICATION_CAPABILITIES, Fields}, State) ->
    {ok, Fields, NewState} =
        copy_state_val(
          ?PER_MSG_ENABLED,
          select_auth(
            select_login(
              assert_completion(
                assert_seq_nr({ok, Fields, State}))))),
    {next_state,
     activation0,
     send_prior_session_request(?GET_SESSION_CHALLENGE, NewState)}.

%%------------------------------------------------------------------------------
%% @private
%% The only expected message while in activation0 is the answer to the session
%% challenge request.
%%------------------------------------------------------------------------------
activation0({?GET_SESSION_CHALLENGE, Fields}, State) ->
    {ok, Fields, NewState} =
        copy_state_vals(
          [?CHALLENGE, ?SESSION_ID],
          assert_completion(
            assert_seq_nr({ok, Fields, State}))),
    {next_state,
     activation1,
     send_prior_session_request(?ACTIVATE_SESSION, NewState)}.

%%------------------------------------------------------------------------------
%% @private
%% The only expected message while in activation1 is the answer to the activate
%% session request.
%% TODO change user privilege, assign proper rq_seq_nr
%%------------------------------------------------------------------------------
activation1({?ACTIVATE_SESSION, Fields}, State) ->
    {ok, Fields, NewState} =
        copy_state_vals(
          [?AUTH_TYPE, ?SESSION_ID, ?PRIVILEGE, ?INBOUND_SEQ_NR],
          assert_completion(
            check_seq_nr({ok, Fields, State}))),
    {next_state, active, NewState}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
closing({?CLOSE_SESSION, _Fields}, State) ->
    {stop, normal, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_packet({ok, I = #rmcp_ipmi{cmd = C, type = response}}, StateName, State) ->
    maybe_send_ack(I#rmcp_ipmi.header, State),
    Fields = eipmi_response:decode(C, I#rmcp_ipmi.data),
    ?MODULE:StateName({C, I#rmcp_ipmi.properties ++ Fields}, State);
handle_packet({ok, Ack = #rmcp_ack{}}, StateName, State) ->
    error_logger:info_msg("unhandled ACK message ~p", [Ack]),
    {next_state, StateName, State};
handle_packet({ok, Asf = #rmcp_asf{}}, StateName, State) ->
    maybe_send_ack(Asf#rmcp_asf.header, State),
    error_logger:info_msg("unhandled ASF message ~p", [Asf]),
    {next_state, StateName, State};
handle_packet({error, Reason}, _StateName, State) ->
    error_logger:info_msg("failed to decode packet ~p", [Reason]),
    {stop, {decoder_error, Reason}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_completion(Error = {error, _}) ->
    Error;
check_completion({ok, Fields, State = #state{properties = Ps}}) ->
    case eipmi_util:get_val(?COMPLETION, Ps) of
        normal ->
            {ok, Fields, State};
        Completion ->
            {error, {bmc_error, Completion}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_seq_nr(Error = {error, _}) ->
    Error;
check_seq_nr({ok, Fields, State = #state{properties = Ps}}) ->
    SeqNr = eipmi_util:get_val(?OUTBOUND_SEQ_NR, Fields),
    case eipmi_util:get_val(?OUTBOUND_SEQ_NR, Ps) of
        StateSeqNr when SeqNr >= StateSeqNr ->
            {ok, Fields, update_state_val(?OUTBOUND_SEQ_NR, SeqNr, State)};
        StateSeqNr when SeqNr + 8 < StateSeqNr ->
            {error, {seq_nr_too_old, SeqNr}};
        _ ->
            {ok, Fields, State};
    end.

%%------------------------------------------------------------------------------
%% @private
%% assert that request processing completed normally
%%------------------------------------------------------------------------------
assert_completion(Error = {error, _}) ->
    Error;
assert_completion({ok, Fields, State}) ->
    normal = eipmi_util:get_val(?COMPLETION, Fields),
    {ok, Fields, State}.

%%------------------------------------------------------------------------------
%% @private
%% ssert that received sequence number is zero.
%%------------------------------------------------------------------------------
assert_seq_nr(Error = {error, _}) ->
    Error;
assert_seq_nr({ok, Fields, State}) ->
    0 = eipmi_util:get_val(?OUTBOUND_SEQ_NR, Fields),
    {ok, Fields, State}.

%%------------------------------------------------------------------------------
%% @private
%% Selects an authentication method from the available methods. Preferred
%% selection order is none, pwd, md5, md2.
%%------------------------------------------------------------------------------
select_auth({ok, Fields, State}) ->
    AuthTypes = eipmi_util:get_val(?AUTH_TYPES, Fields),
    None = lists:member(none, AuthTypes),
    Pwd = lists:member(pwd, AuthTypes),
    Md5 = lists:member(md5, AuthTypes),
    Md2 = lists:member(md2, AuthTypes),
    case {None, Pwd, Md5, Md2} of
        {true, _, _, _} ->
            {ok, Fields, update_state_val(?AUTH_TYPE, none, State)};
        {false, true, _, _} ->
            {ok, Fields, update_state_val(?AUTH_TYPE, pwd, State)};
        {false, false, true, _} ->
            {ok, Fields, update_state_val(?AUTH_TYPE, md5, State)};
        {false, false, false, true} ->
            {ok, Fields, update_state_val(?AUTH_TYPE, md2, State)}
    end.

%%------------------------------------------------------------------------------
%% @private
%% Selects a login method from the available methods. Preferred selection order
%% is anonymous, null, non_null. In case anonymous is selected the user and
%% password contained in the state will be reset, in case of null user only the
%% state user will be reset, otherwise user and password must exist and must
%% have sensible values in the state.
%%------------------------------------------------------------------------------
select_login({ok, Fields, State}) ->
    Logins = eipmi_util:get_val(?LOGIN_STATUS, Fields),
    case {lists:member(anonymous, Logins), lists:member(null, Logins)} of
        {true, _} ->
            {ok,
             Fields,
             update_state_val(
               ?PASSWORD, "",
               update_state_val(?USER, "", State))};
        {false, true} ->
            {ok, Fields, update_state_val(?USER, "", State)};
        {false, false} ->
            {ok, Fields, State}
    end.

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
%% This function will not update any sequence numbers.
%%------------------------------------------------------------------------------
send_prior_session_request(Cmd, State = #state{properties = Ps}) ->
    Data = eipmi_request:encode(Cmd, Ps),
    send_ipmi(Cmd, Data, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_request(Cmd, RqProperties, State = #state{properties = Ps}) ->
    Data = eipmi_request:encode(Cmd, RqProperties ++ Ps),
    incr_rq_seq_nr(incr_inbound_seq_nr(send_ipmi(Cmd, Data, State))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
incr_inbound_seq_nr(State = #state{properties = Ps}) ->
    Current = eipmi_util:get_val(?INBOUND_SEQ_NR, Ps),
    update_state_val(?INBOUND_SEQ_NR, Current + 1, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
incr_rq_seq_nr(State = #state{properties = Ps}) ->
    Current = eipmi_util:get_val(?RQ_SEQ_NR, Ps),
    update_state_val(?RQ_SEQ_NR, (Current + 1) rem 64, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_ipmi(Cmd, Data, State = #state{properties = Ps}) ->
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    udp_send(eipmi_encoder:ipmi(Header, Ps ++ ?MSG_DEFAULTS, Cmd, Data), State),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
udp_send(Binary, #state{socket = Socket, address = IPAddress}) ->
    ok = gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Binary).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
%% get_state_val(Property, #state{properties = Ps}) ->
%%     eipmi_util:get_val(Property, Ps).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_state_val(Property, Value, State = #state{properties = Ps}) ->
    State#state{properties = eipmi_util:update_val(Property, Value, Ps)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_val(_Property, Error = {error, _}) ->
    Error;
copy_state_val(Property, {ok, Fields, S = #state{properties = Ps}}) ->
    {ok,
     Fields,
     S#state{properties = eipmi_util:copy_val(Property, Ps, Fields)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_vals(_PropertyList, Error = {error, _}) ->
    Error;
copy_state_vals(PropertyList, {ok, Fields, State}) ->
    lists:foldl(
      fun(P, {ok, _, Acc}) -> copy_state_val(P, {ok, Fields, Acc}) end,
      {ok, Fields, State}, PropertyList).
