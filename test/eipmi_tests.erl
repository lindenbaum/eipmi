%%%=============================================================================
%%% Copyright (c) 2012-2019 Lindenbaum GmbH
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

-module(eipmi_tests).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%% Unfortunately, for these tests you actually need hardware to test against.
%% Thus, these can only selectively by executed.
%%-define(MCH_IP, "192.168.1.41").
-define(MCH_IP, skip).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

ping_test() -> ping(?MCH_IP).
ping(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    ?assertEqual(pong, eipmi:ping(IP)),
    application:stop(eipmi);
ping(_) ->
    ok.

open_close_test() -> open_close(?MCH_IP).
open_close(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    ?assertEqual([Session], eipmi:sessions()),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
open_close(_) ->
    ok.

parallel_request_test() -> parallel_request(?MCH_IP).
parallel_request(IP) when is_list(IP); is_tuple(IP) ->
    process_flag(trap_exit, true),
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    Action = fun() -> {ok, _} = eipmi:get_sel_info(Session) end,
    Pids = lists:map(fun(_) -> spawn_link(Action) end, lists:seq(1, 10)),
    Receive = fun(P) ->
        receive
            {'EXIT', P, Reason} -> Reason
        end
    end,
    Results = lists:map(Receive, Pids),
    ?assert(lists:all(fun(E) -> E =:= normal end, Results)),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
parallel_request(_) ->
    ok.

read_fru_test() -> read_fru(?MCH_IP).
read_fru(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    {ok, Fru} = eipmi:read_fru(Session, 253),
    io:format(standard_error, "~s~n", [eipmi_fru:to_list(Fru)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
read_fru(_) ->
    ok.

get_sdr_repository_test() -> get_sdr_repository(?MCH_IP).
get_sdr_repository(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    io:format(standard_error, "~s~n", [eipmi_sdr:to_list(SDRRepository)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
get_sdr_repository(_) ->
    ok.

read_fru_inventory_test_() ->
    {timeout, 30000, [fun() -> read_fru_inventory(?MCH_IP) end]}.
read_fru_inventory(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    {ok, SDRRepository} = eipmi:get_sdr_repository(Session),
    {ok, FruInventory} = eipmi:read_fru_inventory(Session, SDRRepository),
    io:format(standard_error, "~s~n", [eipmi_fru:to_list(FruInventory)]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
read_fru_inventory(_) ->
    ok.

read_sel_test() -> read_sel(?MCH_IP).
read_sel(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    Mon = monitor_session(Session),
    {ok, Sel} = eipmi:get_sel(Session, false),
    io:format(standard_error, "~n~p~n", [Sel]),
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
read_sel(_) ->
    ok.

auto_read_sel_test() -> auto_read_sel(?MCH_IP).
auto_read_sel(IP) when is_list(IP); is_tuple(IP) ->
    application:ensure_all_started(eipmi),
    {ok, Session} = eipmi:open(IP),
    {ok, _} = eipmi:poll_sel(Session, 500, false),
    Mon = monitor_session(Session),
    receive
        {ipmi, Session, IP, {system_event, _}} -> ok
    after 500 -> ok
    end,
    ?assertEqual(ok, eipmi:close(Session)),
    ?assertEqual(
        shutdown,
        receive
            {'DOWN', Mon, _, _, Reason} -> Reason
        end
    ),
    application:stop(eipmi);
auto_read_sel(_) ->
    ok.

trap_receiver_test_() ->
    {timeout, 2000, fun() ->
        application:ensure_all_started(eipmi),
        Previous = process_flag(trap_exit, true),
        LinkDown =
            <<16#30, 16#38, 16#02, 16#01, 16#00, 16#04, 16#06, 16#70, 16#75,
                16#62, 16#6c, 16#69, 16#63, 16#a4, 16#2b, 16#06, 16#06, 16#2b,
                16#06, 16#01, 16#02, 16#01, 16#0b, 16#40, 16#04, 16#0a, 16#01,
                16#28, 16#8c, 16#02, 16#01, 16#02, 16#02, 16#01, 16#00, 16#43,
                16#02, 16#42, 16#18, 16#30, 16#11, 16#30, 16#0f, 16#06, 16#0a,
                16#2b, 16#06, 16#01, 16#02, 16#01, 16#02, 16#02, 16#01, 16#08,
                16#02, 16#02, 16#01, 16#07>>,
        TestPort = 51539,
        {ok, Socket} = gen_udp:open(0, [{active, false}]),
        {ok, Pid} = eipmi_trap:start_link(TestPort),
        ok = gen_udp:send(Socket, {127, 0, 0, 1}, TestPort, LinkDown),
        ok = timer:sleep(1000),
        exit(Pid, shutdown),
        ok =
            receive
                {'EXIT', Pid, shutdown} -> ok
            after 500 -> timeout
            end,
        process_flag(trap_exit, Previous),
        application:stop(eipmi)
    end}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% don't look
monitor_session(Session) ->
    Cs = supervisor:which_children(eipmi),
    [Pid] = [P || {I, P, worker, _} <- Cs, I =:= Session andalso is_pid(P)],
    monitor(process, Pid).
