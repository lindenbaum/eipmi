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
%%%=============================================================================

-module(eipmi_events_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

event_test() ->
    ?assertMatch({ok, _}, eipmi_events:start_link()),
    ?assertEqual(ok, eipmi_events:subscribe(?MODULE, self())),
    ?assertMatch([?MODULE], eipmi_events:list_handlers()),
    eipmi_events:fire(session, address, event),
    receive {ipmi, session, address, event} -> ok end,
    ?assertEqual(ok, eipmi_events:unsubscribe(?MODULE, self())),
    ?assertMatch([], eipmi_events:list_handlers()).

%%%=============================================================================
%%% Test gen_event handler
%%%=============================================================================

init(Pid) -> {ok, Pid}.

handle_event(Event, Pid) -> Pid ! Event, {ok, Pid}.

handle_call(Request, Pid) -> Pid ! Request, {ok, undef, Pid}.

handle_info(Info, Pid) -> Pid ! Info, {ok, Pid}.

terminate(_Pid, _State) -> ok.

code_change(_OldVsn, Pid, _Extra) -> {ok, Pid}.
