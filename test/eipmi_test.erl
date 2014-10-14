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

-include("eipmi.hrl").

-define(IP, "192.168.101.1").
%%-define(GET_HOSTNAME, inet:gethostname()).
-define(GET_HOSTNAME, skip).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

ping_test() -> ping(?GET_HOSTNAME).
ping({ok, "tirana"}) ->
    start(),
    ?assertEqual(pong, eipmi:ping(?IP)),
    stop();
ping(_) ->
    ok.

open_close_test() -> open_close(?GET_HOSTNAME).
open_close({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    receive {ipmi, Session, ?IP, established} -> ok end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
open_close(_) ->
    ok.

parallel_request_test() -> parallel_request(?GET_HOSTNAME).
parallel_request({ok, "tirana"}) ->
    process_flag(trap_exit, true),
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    Action = fun() -> {ok, _} = eipmi:get_sel_info(Session) end,
    Pids = lists:map(fun(_) -> spawn_link(Action) end, lists:seq(1, 10)),
    Receive = fun(P) -> receive {'EXIT', P, Reason} -> Reason end end,
    Results = lists:map(Receive, Pids),
    ?assert(lists:all(fun(E) -> E =:= normal end, Results)),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
parallel_request(_) ->
    ok.

read_fru_test() -> read_fru(?GET_HOSTNAME).
read_fru({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, Fru} = eipmi:read_fru(Session, 253),
    error_logger:info_msg("~s~n", [eipmi_fru:to_list(Fru)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
read_fru(_) ->
    ok.

get_sdr_repository_test() -> get_sdr_repository(?GET_HOSTNAME).
get_sdr_repository({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    error_logger:info_msg("~s~n", [eipmi_sdr:to_list(SDRRepository)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
get_sdr_repository(_) ->
    ok.

read_fru_inventory_test_() ->
    {timeout, 30000, [fun() -> read_fru_inventory(?GET_HOSTNAME) end]}.
read_fru_inventory({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    {ok, FruInventory} = eipmi:read_fru_inventory(Session, SDRRepository),
    error_logger:info_msg("~s~n", [eipmi_fru:to_list(FruInventory)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
read_fru_inventory(_) ->
    ok.

read_sel_test() -> read_sel(?GET_HOSTNAME).
read_sel({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    Mon = monitor_session(Session),
    {ok, Sel} = eipmi:get_sel(Session, false),
    error_logger:info_msg("~n~p~n", [Sel]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
read_sel(_) ->
    ok.

auto_read_sel_test() -> auto_read_sel(?GET_HOSTNAME).
auto_read_sel({ok, "tirana"}) ->
    start(),
    {ok, Session} = eipmi:open(?IP),
    {ok, _} = eipmi:poll_sel(Session, 500, false),
    Mon = monitor_session(Session),
    receive after 2000 -> ok end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(shutdown, receive {'DOWN', Mon, _, _, Reason} -> Reason end),
    stop();
auto_read_sel(_) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(md2),
    application:start(eipmi).

stop() -> application:stop(eipmi).

%% don't look
monitor_session(Session) ->
    Cs = supervisor:which_children(eipmi),
    [Pid] = [P || {I, P, worker, _} <- Cs, I =:= Session andalso is_pid(P)],
    monitor(process, Pid).
