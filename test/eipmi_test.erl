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

-define(IP, "10.1.31.11").
-define(GET_HOSTNAME, inet:gethostname()).
%%-define(GET_HOSTNAME, skip).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

ping_test() ->
    ping(?GET_HOSTNAME).

ping({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    ?assertEqual(pong, eipmi:ping(?IP)),
    application:stop(eipmi);
ping(_) ->
    ok.

open_close_test() ->
    open_close(?GET_HOSTNAME).

open_close({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    receive after 2000 -> ok end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
open_close(_) ->
    ok.

parallel_request_test() ->
    parallel_request(?GET_HOSTNAME).

parallel_request({ok, "tirana"}) ->
    process_flag(trap_exit, true),
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    Action = fun() -> {ok, _} = eipmi:get_sel_info(Session) end,
    Pids = lists:map(fun(_) -> spawn_link(Action) end, lists:seq(1, 10)),
    Receive = fun(P) -> receive {'EXIT', P, Reason} -> Reason end end,
    Results = lists:map(Receive, Pids),
    ?assert(lists:all(fun(E) -> E =:= normal end, Results)),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
parallel_request(_) ->
    ok.

read_fru_test() ->
    read_fru(?GET_HOSTNAME).

read_fru({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, Fru} = eipmi:read_fru(Session, 253),
    error_logger:info_msg("~n~p~n", [Fru]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
read_fru(_) ->
    ok.

get_sdr_repository_test() ->
    get_sdr_repository(?GET_HOSTNAME).

get_sdr_repository({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    error_logger:info_msg("~n~p~n", [SDRRepository]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
get_sdr_repository(_) ->
    ok.

read_fru_inventory_test_() ->
    {timeout, 30000, [fun() -> read_fru_inventory(?GET_HOSTNAME) end]}.

read_fru_inventory({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    {ok, FruInventory} = eipmi:read_fru_inventory(Session, SDRRepository),
    error_logger:info_msg("~n~p~n", [FruInventory]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
read_fru_inventory(_) ->
    ok.

read_sel_test() ->
    read_sel(?GET_HOSTNAME).

read_sel({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, Sel} = eipmi:get_sel(Session, false),
    error_logger:info_msg("~n~p~n", [Sel]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
read_sel(_) ->
    ok.

auto_read_sel_test() ->
    auto_read_sel(?GET_HOSTNAME).

auto_read_sel({ok, "tirana"}) ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi),
    {ok, Session} = eipmi:open(?IP, [{poll_sel, 500}, {clear_sel, false}]),
    Mon = monitor_session(Session),
    receive after 2000 -> ok end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    application:stop(eipmi);
auto_read_sel(_) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% don't look
monitor_session(Session) ->
    Cs = supervisor:which_children(eipmi),
    [Pid] = [P || {I, P, worker, _} <- Cs, I =:= Session andalso is_pid(P)],
    monitor(process, Pid).
