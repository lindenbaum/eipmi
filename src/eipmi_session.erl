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
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4,
         expect_pong/2]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start the server.
%% @end
%%------------------------------------------------------------------------------
start_link(IPAddress) ->
    gen_fsm:start_link(?MODULE, [IPAddress], []).

%%%=============================================================================
%%% gen_fsm Callbacks
%%%=============================================================================

-record(state, {
          address :: string(),
          socket  :: gen_udp:socket()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([IPAddress]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0, [binary]),
    State = #state{address = IPAddress, socket = Socket},
    {ok, Ping} = eipmi_messages:encode(#rmcp_ping{seq_nr = 0, asf_tag = 0}),
    udp_send(Ping, State),
    {ok, expect_pong, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    {reply, {undef, Event}, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp, _, _, _, Bin}, StateName, State) ->
    {ok, Message} = eipmi_messages:decode(Bin),
    ?MODULE:StateName(Message, State);

handle_info(_Event, StateName, State) ->
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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expect_pong(#rmcp_ack{}, State) ->
    {next_state, expect_pong, State};

expect_pong(#rmcp_pong{seq_nr = SeqNr}, State) ->
    {ok, Ack} = eipmi_messages:encode(#rmcp_ack{seq_nr = SeqNr}),
    udp_send(Ack, State),
    {stop, normal, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
udp_send(Binary, #state{socket = Socket, address = IPAddress}) ->
    ok = gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Binary).
