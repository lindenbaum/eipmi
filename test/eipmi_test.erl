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

-module(eipmi_test).

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
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open("10.1.31.10"),
    Mon = monitor_session(Session),
    receive after 1000 -> ok end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(normal, receive {'DOWN', Mon, _, _, Reason} -> Reason end);
open_close(_) ->
    ok.

parallel_request_test() ->
    parallel_request(inet:gethostname()).

parallel_request({ok, "tirana"}) ->
    process_flag(trap_exit, true),
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open("10.1.31.10"),
    Mon = monitor_session(Session),
    Action = fun() -> {ok, _} = eipmi:raw(Session, 16#06, 16#3b, []) end,
    Pids = lists:map(fun(_) -> spawn_link(Action) end, lists:seq(1, 10)),
    Receive = fun(P) -> receive {'EXIT', P, Reason} -> Reason end end,
    Results = lists:map(Receive, Pids),
    ?assert(lists:all(fun(E) -> E =:= normal end, Results)),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(normal, receive {'DOWN', Mon, _, _, Reason} -> Reason end);
parallel_request(_) ->
    ok.

read_fru_test() ->
    read_fru(inet:gethostname()).

read_fru({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open("10.1.31.10"),
    Mon = monitor_session(Session),
    {ok, Data} = eipmi:read_fru(Session, 5),
    error_logger:info_msg("fru data:~n~p~n", [Data]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(normal, receive {'DOWN', Mon, _, _, Reason} -> Reason end);
read_fru(_) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% don't look
monitor_session(Session) ->
    Cs = supervisor:which_children(eipmi),
    [Pid] = [P || {I, P, worker, _} <- Cs, I =:= Session andalso is_pid(P)],
    monitor(process, Pid).
