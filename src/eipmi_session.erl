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
%%% @end
%%%=============================================================================
-module(eipmi_session).

-behaviour(gen_fsm).

%% API
-export([start_link/2,
         stop/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("eipmi_internal.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start the server.
%% @end
%%------------------------------------------------------------------------------
start_link(IPAddress, Options) ->
    gen_fsm:start_link(?MODULE, [IPAddress, Options], []).

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
          address          :: string(),
          socket           :: gen_udp:socket(),
          properties       :: proplists:proplist()}).

          %% user = ""        :: string(),
          %% password = ""    :: string(),
          %% session_id = 0   :: non_neg_integer(),
          %% challenge = <<>> :: binary(),
          %% outbound_seq_nr  :: non_neg_integer(),
          %% inbound_seq_nr   :: non_neg_integer(),
          %% privilege        :: callback | user | operator | administrator,
          %% requestor        :: non_neg_integer(),
          %% per_msg = true   :: boolean(),
          %% auth_type = none :: none | pwd | md5 | md2

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([IPAddress, Opts]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0, [binary]),
    State = #state{address = IPAddress, socket = Socket, properties = Opts},
    {ok, discovery, send_channel_auth_capabilities(State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
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
handle_info({udp, S, Addr, _, Binary}, StateName, State)
  when State#state.socket =:= S andalso State#state.address =:= Addr ->
    case eipmi_decoder:packet(Binary) of
        {ok, Ack} = {ok, #rmcp_ack{}} ->
            {NewStateName, NewState} = handle_ack(Ack, StateName, State),
            {next_state, NewStateName, NewState};

        {ok, Ipmi} = {ok, #rmcp_ipmi{type = #ipmi_response{code = normal}}} ->
            maybe_send_ack(Ipmi#rmcp_ipmi.header, State),
            {NewStateName, NewState} = handle_ipmi(Ipmi, StateName, State),
            {next_state, NewStateName, NewState};

        {ok, Ipmi} = {ok, #rmcp_ipmi{type = #ipmi_response{code = Code}}} ->
            maybe_send_ack(Ipmi#rmcp_ipmi.header, State),
            {stop, {bmc_error, Code}, State};

        {ok, Asf} = {ok, #rmcp_asf{}} ->
            maybe_send_ack(Asf#rmcp_asf.header, State),
            {NewStateName, NewState} = handle_asf(Asf, StateName, State),
            {next_state, NewStateName, NewState};

        {error, Reason} ->
            {stop, {decoder_error, Reason}, State}
    end;
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
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ack(Ack = #rmcp_ack{}, StateName, State) ->
    error_logger:info_msg("unhandled ACK message ~p", [Ack]),
    {StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ipmi(Ipmi = #rmcp_ipmi{cmd = Cmd, data = Data}, StateName, State) ->
    try eipmi_response:decode(Cmd, Data) of
        Fields ->
            handle_ipmi(Cmd, Fields, StateName, State)
    catch
        _:_ ->
            error_logger:info_msg("unimplemented IPMI resonse ~p", [Ipmi]),
            {StateName, State}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_ipmi(?GET_CHANNEL_AUTHENTICATION_CAPABILITIES, Fields, discovery, State) ->
    {activation0,
     send_session_challenge(
       copy_state_val(
         ?PER_MSG_ENABLED,
         select_auth(Fields, select_login(Fields, State)),
         Fields))};

handle_ipmi(?GET_SESSION_CHALLENGE, Fields, activation0, State) ->
    {activation1,
     send_activate_session(
       copy_state_vals([?CHALLENGE, ?SESSION_ID], State, Fields))};

handle_ipmi(?ACTIVATE_SESSION, Fields, _StateName, State) ->
    {established,
     copy_state_vals(
       [?AUTH_TYPE, ?SESSION_ID, ?PRIVILEGE, ?INBOUND_SEQ_NR],
       State,
       Fields)};

handle_ipmi(Cmd, Fields, StateName, State) ->
    error_logger:info_msg("unhandled IPMI message ~p: ~p", [Cmd, Fields]),
    {StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_asf(Asf = #rmcp_asf{}, StateName, State) ->
    error_logger:info_msg("unhandled ASF message ~p", [Asf]),
    {StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%% Selects an authentication method from the available methods. Preferred
%% selection order is none, pwd, md5, md2.
%%------------------------------------------------------------------------------
select_auth(Fields, State) ->
    AuthTypes = get_val(?AUTH_TYPES, Fields),
    None = lists:member(none, AuthTypes),
    Pwd = lists:member(pwd, AuthTypes),
    Md5 = lists:member(md5, AuthTypes),
    Md2 = lists:member(md2, AuthTypes),
    case {None, Pwd, Md5, Md2} of
        {true, _, _, _} ->
            update_state_val(?AUTH_TYPE, none, State);
        {false, true, _, _} ->
            update_state_val(?AUTH_TYPE, pwd, State);
        {false, false, true, _} ->
            update_state_val(?AUTH_TYPE, md5, State);
        {false, false, false, true} ->
            update_state_val(?AUTH_TYPE, md2, State)
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
    Logins = get_val(?LOGIN_STATUS, Fields),
    case {lists:member(anonymous, Logins), lists:member(null, Logins)} of
        {true, _} ->
            update_state_val(?PASSWORD, "", update_state_val(?USER, "", State));
        {false, true} ->
            update_state_val(?USER, "", State);
        {false, false} ->
            State
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ipmi_request(SeqNr, #state{requestor = R}) ->
    #ipmi_request{rq_addr = R, rq_seq_nr = SeqNr}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ipmi_session(SeqNr, #state{auth_type = T, session_id = I, password = P}) ->
    #ipmi_session{type = T, seq_nr = SeqNr, id = I, key = P}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
udp_send(Binary, #state{socket = Socket, address = IPAddress}) ->
    ok = gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Binary).

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
%%------------------------------------------------------------------------------
send_channel_auth_capabilities(State = #state{properties = Ps}) ->
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    Session = ipmi_session(0, State),
    Request = ipmi_request(0, State),
    Cmd = ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
    Data = eipmi_request:get_channel_authentication_capabilities(P),
    udp_send(eipmi_encoder:ipmi(Header, Session, Request, Cmd, Data), State),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_session_challenge(State = #state{auth_type = T, user = U}) ->
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    Session = ipmi_session(0, State),
    Request = ipmi_request(0, State),
    Cmd = ?GET_SESSION_CHALLENGE,
    Data = eipmi_request:get_session_challenge(T, U),
    udp_send(eipmi_encoder:ipmi(Header, Session, Request, Cmd, Data), State),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_activate_session(State) ->
    Header = #rmcp_header{seq_nr = ?RMCP_NOREPLY, class = ?RMCP_IPMI},
    Session = ipmi_session(0, State),
    Request = ipmi_request(0, State),
    Cmd = ?ACTIVATE_SESSION,
    Data = eipmi_request:activate_session(
             State#state.auth_type,
             State#state.privilege,
             State#state.challenge,
             State#state.outbound_seq_nr),
    udp_send(eipmi_encoder:ipmi(Header, Session, Request, Cmd, Data), State),
    State.

%%------------------------------------------------------------------------------
%% @private
%% Return the value of a property.
%%------------------------------------------------------------------------------
get_val(Property, Properties) ->
    proplists:get_value(Property, Properties).

%%------------------------------------------------------------------------------
%% @private
%% Update the value of a property in a proplist.
%%------------------------------------------------------------------------------
update_val(Property, Value, Properties) ->
    [{Properties, Value} | proplists:delete(Property, Properties)].

%%------------------------------------------------------------------------------
%% @private
%% Copies the value of a property from one proplist to another.
%%------------------------------------------------------------------------------
copy_val(Property, DestProperties, SrcProperties) ->
    case proplists:get_value(Property, SrcProperties) of
        undefined ->
            DestProperties;

        Value ->
            update_val(Property, Value, DestProperties)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_state_val(Property, #state{properties = Ps}) ->
    get_val(Property, Ps).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_state_val(Property, Value, State = #state{properties = Ps}) ->
    State#state{properties = update_val(Property, Value, Ps)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_val(Property, State = #state{properties = Ps}, SrcProperties) ->
    State#state{properties = copy_val(Property, Ps, SrcProperties)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
copy_state_vals(PropertyList, State, SrcProperties) ->
    lists:foldl(
      fun(P, Acc) -> copy_state_val(P, State, SrcProperties) end,
      State, PropertyList).
