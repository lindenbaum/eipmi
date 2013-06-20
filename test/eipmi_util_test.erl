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

-module(eipmi_util_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

normalize_test() ->
    ?assertEqual(<<"1234">>, eipmi_util:normalize(4, "12345")),
    ?assertEqual(<<"1234">>, eipmi_util:normalize(4, "1234")),
    ?assertEqual(<<"123", 0>>, eipmi_util:normalize(4, "123")),
    ?assertEqual(<<"12", 0, 0>>, eipmi_util:normalize(4, "12")),
    ?assertEqual(<<"1", 0, 0, 0>>, eipmi_util:normalize(4, "1")),
    ?assertEqual(<<0, 0, 0, 0>>, eipmi_util:normalize(4, "")),
    ?assertEqual(<<"1234">>, eipmi_util:normalize(4, <<"12345">>)),
    ?assertEqual(<<"1234">>, eipmi_util:normalize(4, <<"1234">>)),
    ?assertEqual(<<"123", 0>>, eipmi_util:normalize(4, <<"123">>)),
    ?assertEqual(<<"12", 0, 0>>, eipmi_util:normalize(4, <<"12">>)),
    ?assertEqual(<<"1", 0, 0, 0>>, eipmi_util:normalize(4, <<"1">>)),
    ?assertEqual(<<0, 0, 0, 0>>, eipmi_util:normalize(4, <<>>)).

format_test() ->
    ?assertEqual("1.2", eipmi_util:format("~B.~B", [1, 2])).

update_val_test() ->
    ?assertEqual([{k, v1}], eipmi_util:update_val(k, v1, [])),
    ?assertEqual([{k, v1}], eipmi_util:update_val(k, v1, [{k, v0}])).

copy_val_test() ->
    ?assertEqual([], eipmi_util:copy_val(k, [], [])),
    ?assertEqual([{k, v}], eipmi_util:copy_val(k, [{k, v}], [])),
    ?assertEqual([{k, v}], eipmi_util:copy_val(k, [], [{k, v}])),
    ?assertEqual([{k, v1}], eipmi_util:copy_val(k, [{k, v0}], [{k, v1}])).

merge_vals_test() ->
    Merged1 = eipmi_util:merge_vals([], [{a, 1}, {b, 2}]),
    ?assertEqual(1, proplists:get_value(a, Merged1)),
    ?assertEqual(2, proplists:get_value(b, Merged1)),

    Merged2 = eipmi_util:merge_vals([{a, 1}, {b, 2}], []),
    ?assertEqual(1, proplists:get_value(a, Merged2)),
    ?assertEqual(2, proplists:get_value(b, Merged2)),

    Merged3 = eipmi_util:merge_vals([{a, 1}, {b, 2}], [{a, 3}, {c, 4}]),
    ?assertEqual(1, proplists:get_value(a, Merged3)),
    ?assertEqual(2, proplists:get_value(b, Merged3)),
    ?assertEqual(4, proplists:get_value(c, Merged3)).

eipmi_catch_test() ->
    ?assertEqual({error, b}, ?EIPMI_CATCH(a = b)),
    ?assertEqual({error, b}, ?EIPMI_CATCH(a = {error, b})).

from_bcd_plus_test() ->
    Bin = <<16#0:4, 16#1:4, 16#2:4, 16#3:4, 16#4:4, 16#5:4, 16#6:4, 16#7:4,
            16#8:4, 16#9:4, 16#a:4, 16#b:4, 16#c:4, 16#d:4, 16#e:4, 16#f:4>>,
    ?assertEqual("0123456789 -.:,_", eipmi_util:from_bcd_plus(Bin)).

from_packed_ascii_test() ->
    Bin3 = <<2#00101001, 2#11011100, 2#10100110>>,
    ?assertEqual("IPMI", eipmi_util:from_packed_ascii(Bin3)),
    Bin2 = <<2#00101001, 2#11011100>>,
    ?assertEqual("IP", eipmi_util:from_packed_ascii(Bin2)),
    Bin1 = <<2#00101001>>,
    ?assertEqual("I", eipmi_util:from_packed_ascii(Bin1)).

from_base26_test() ->
    ?assertEqual("A", eipmi_util:from_base26(0)),
    ?assertEqual("Z", eipmi_util:from_base26(25)),
    ?assertEqual("AA", eipmi_util:from_base26(26)),
    ?assertEqual("AZ", eipmi_util:from_base26(51)),
    ?assertEqual("BA", eipmi_util:from_base26(52)),
    ?assertEqual("BZ", eipmi_util:from_base26(77)),
    ?assertEqual("CA", eipmi_util:from_base26(78)),
    ?assertEqual("ZZ", eipmi_util:from_base26(701)),
    ?assertEqual("AAA", eipmi_util:from_base26(702)),
    ?assertEqual("AAZ", eipmi_util:from_base26(727)),
    ?assertEqual("ZZZ", eipmi_util:from_base26(18277)).

get_bool_test() ->
    ?assert(not eipmi_util:get_bool(0)),
    ?assert(eipmi_util:get_bool(other)).

binary_to_string_test() ->
    ?assertEqual("abc", eipmi_util:binary_to_string(<<$a, $b, $c>>)),
    ?assertEqual("abc", eipmi_util:binary_to_string(<<$a, $b, $c, 0>>)),
    ?assertEqual("abc", eipmi_util:binary_to_string(<<$a, $b, $c, 0, 0>>)).

join_nl_test() ->
    ?assertEqual("a", eipmi_util:join_nl(["a"])),
    ?assertEqual("a" ++ io_lib:nl() ++ "b", eipmi_util:join_nl(["a", "b"])).
