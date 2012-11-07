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

get_val_test() ->
    ?assertEqual(v, eipmi_util:get_val(k, [{k, v}])),
    ?assertEqual(undefined, eipmi_util:get_val(k, [])),
    ?assertEqual(v, eipmi_util:get_val(k, [{k, v}], v0)),
    ?assertEqual(v, eipmi_util:get_val(k, [], v)).

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
    ?assertEqual(1, eipmi_util:get_val(a, Merged1)),
    ?assertEqual(2, eipmi_util:get_val(b, Merged1)),

    Merged2 = eipmi_util:merge_vals([{a, 1}, {b, 2}], []),
    ?assertEqual(1, eipmi_util:get_val(a, Merged2)),
    ?assertEqual(2, eipmi_util:get_val(b, Merged2)),

    Merged3 = eipmi_util:merge_vals([{a, 1}, {b, 2}], [{a, 3}, {c, 4}]),
    ?assertEqual(1, eipmi_util:get_val(a, Merged3)),
    ?assertEqual(2, eipmi_util:get_val(b, Merged3)),
    ?assertEqual(4, eipmi_util:get_val(c, Merged3)).

eipmi_catch_test() ->
    ?assertEqual({error, b}, ?EIPMI_CATCH(a = b)),
    ?assertEqual({error, b}, ?EIPMI_CATCH(a = {error, b})).

read_test() ->
    Reader = fun(0, _Count) -> {3, <<$a, $b, $c>>};
                (3, _Count) -> {3, <<$d, $e, $f>>};
                (6, _Count) -> {2, <<$g, $h>>}
             end,
    ?assertEqual(
       <<$a, $b, $c, $d, $e, $f, $g, $h>>,
       eipmi_util:read(Reader, 8, 7)).

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
