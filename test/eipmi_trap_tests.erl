%%%=============================================================================
%%% Copyright (c) 2019-2020 Lindenbaum GmbH
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

-module(eipmi_trap_tests).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

link_down_event_test() ->
    {ok, {AgentAddr, {trap, Ps}}} =
        eipmi_trap:decode(
            <<16#30, 16#38, 16#02, 16#01, 16#00, 16#04, 16#06, 16#70, 16#75,
                16#62, 16#6c, 16#69, 16#63, 16#a4, 16#2b, 16#06, 16#06, 16#2b,
                16#06, 16#01, 16#02, 16#01, 16#0b, 16#40, 16#04, 16#0a, 16#01,
                16#28, 16#8c, 16#02, 16#01, 16#02, 16#02, 16#01, 16#00, 16#43,
                16#02, 16#42, 16#18, 16#30, 16#11, 16#30, 16#0f, 16#06, 16#0a,
                16#2b, 16#06, 16#01, 16#02, 16#01, 16#02, 16#02, 16#01, 16#08,
                16#02, 16#02, 16#01, 16#07>>
        ),
    ?assertEqual({10, 1, 40, 140}, AgentAddr),
    ?assertEqual(link_down, proplists:get_value(type, Ps)),
    ?assertEqual(7, proplists:get_value(if_index, Ps)),
    ?assert(proplists:is_defined(time, Ps)),
    ?assert(proplists:is_defined(enterprise, Ps)).

power_channel_event_test() ->
    {ok, {AgentAddr, {trap, Ps}}} =
        eipmi_trap:decode(
            <<16#30, 16#81, 16#ac, 16#02, 16#01, 16#00, 16#04, 16#06, 16#70,
                16#75, 16#62, 16#6c, 16#69, 16#63, 16#a4, 16#81, 16#9e, 16#06,
                16#09, 16#2b, 16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01,
                16#01, 16#40, 16#04, 16#0a, 16#01, 16#28, 16#8c, 16#02, 16#01,
                16#06, 16#02, 16#04, 16#00, 16#f3, 16#6f, 16#01, 16#43, 16#02,
                16#41, 16#e4, 16#30, 16#7e, 16#30, 16#7c, 16#06, 16#0a, 16#2b,
                16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01, 16#01, 16#01,
                16#04, 16#6e, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#ff, 16#24,
                16#ff, 16#ff, 16#c2, 16#1c, 16#0a, 16#61, 16#a1, 16#0b, 16#05,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00>>
        ),
    ?assertEqual({10, 1, 40, 140}, AgentAddr),
    ?assertEqual(enterprise_specific, proplists:get_value(type, Ps)),
    ?assertEqual(ipmi, proplists:get_value(trap_source, Ps)),
    ?assertEqual(power_channel, proplists:get_value(sensor_type, Ps)),
    ?assertEqual(local_state_change, proplists:get_value(sensor_value, Ps)),
    ?assertEqual(5, proplists:get_value(channel, Ps)),
    ?assert(not proplists:get_value(payload_power_enabled, Ps)),
    ?assert(proplists:get_value(management_power_enabled, Ps)).

fru_hot_swap_event1_test() ->
    {ok, {AgentAddr, {trap, Ps}}} =
        eipmi_trap:decode(
            <<16#30, 16#81, 16#ac, 16#02, 16#01, 16#00, 16#04, 16#06, 16#70,
                16#75, 16#62, 16#6c, 16#69, 16#63, 16#a4, 16#81, 16#9e, 16#06,
                16#09, 16#2b, 16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01,
                16#01, 16#40, 16#04, 16#0a, 16#01, 16#28, 16#8c, 16#02, 16#01,
                16#06, 16#02, 16#04, 16#00, 16#f0, 16#6f, 16#05, 16#43, 16#02,
                16#41, 16#d5, 16#30, 16#7e, 16#30, 16#7c, 16#06, 16#0a, 16#2b,
                16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01, 16#01, 16#01,
                16#04, 16#6e, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#ff, 16#24,
                16#ff, 16#ff, 16#72, 16#09, 16#c1, 16#61, 16#a5, 16#04, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00>>
        ),
    ?assertEqual({10, 1, 40, 140}, AgentAddr),
    ?assertEqual(enterprise_specific, proplists:get_value(type, Ps)),
    ?assertEqual(ipmi, proplists:get_value(trap_source, Ps)),
    ?assertEqual(fru_hot_swap, proplists:get_value(sensor_type, Ps)),
    ?assertEqual(deactivation_requested, proplists:get_value(sensor_value, Ps)),
    ?assertEqual(active, proplists:get_value(previous_value, Ps)).

fru_hot_swap_event2_test() ->
    {ok, {AgentAddr, {trap, Ps}}} =
        eipmi_trap:decode(
            <<16#30, 16#81, 16#ac, 16#02, 16#01, 16#00, 16#04, 16#06, 16#70,
                16#75, 16#62, 16#6c, 16#69, 16#63, 16#a4, 16#81, 16#9e, 16#06,
                16#09, 16#2b, 16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01,
                16#01, 16#40, 16#04, 16#0a, 16#01, 16#28, 16#8c, 16#02, 16#01,
                16#06, 16#02, 16#04, 16#00, 16#f0, 16#6f, 16#01, 16#43, 16#02,
                16#41, 16#eb, 16#30, 16#7e, 16#30, 16#7c, 16#06, 16#0a, 16#2b,
                16#06, 16#01, 16#04, 16#01, 16#98, 16#6f, 16#01, 16#01, 16#01,
                16#04, 16#6e, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#ff, 16#24,
                16#ff, 16#ff, 16#72, 16#09, 16#c1, 16#61, 16#a1, 16#06, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00>>
        ),
    ?assertEqual({10, 1, 40, 140}, AgentAddr),
    ?assertEqual(enterprise_specific, proplists:get_value(type, Ps)),
    ?assertEqual(ipmi, proplists:get_value(trap_source, Ps)),
    ?assertEqual(fru_hot_swap, proplists:get_value(sensor_type, Ps)),
    ?assertEqual(inactive, proplists:get_value(sensor_value, Ps)),
    ?assertEqual(
        deactivation_in_progress,
        proplists:get_value(previous_value, Ps)
    ).
