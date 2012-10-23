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

-module(eipmi_session_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

ping_test() ->
    ping(inet:gethostname()).

ping({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    ?assertEqual(pong, eipmi:ping("10.1.31.10"));
ping(_) ->
    ok.

open_close_test() ->
    open_close(inet:gethostname()).

open_close({ok, "tirana"}) ->
    process_flag(trap_exit, true),
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi_session:start_link(session, "10.1.31.10", []),
    receive after 1000 -> ok end,
    ?assertEqual(ok, eipmi_session:stop(Session)),
    ?assertEqual(normal, receive {'EXIT', Session, Reason} -> Reason end);
open_close(_) ->
    ok.

parallel_request_test() ->
    parallel_request(inet).

parallel_request({ok, "tirana"}) ->
    process_flag(trap_exit, true),
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi_session:start_link(session, "10.1.31.10", []),
    Action = fun() -> {ok, _} = eipmi_session:request(Session, 16#3b, []) end,
    lists:map(fun(_) -> spawn_link(Action) end, lists:seq(1, 10)),
    Receive = fun(_) -> receive {'EXIT', _, Reason} -> Reason end end,
    Results = lists:map(Receive, lists:seq(1, 10)),
    ?assert(lists:all(fun(E) -> E =:= normal end, Results)),
    ?assertEqual(ok, eipmi_session:stop(Session)),
    ?assertEqual(normal, receive {'EXIT', Session, Reason} -> Reason end);
parallel_request(_) ->
    ok.
