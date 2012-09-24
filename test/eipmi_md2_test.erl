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

-module(eipmi_md2_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

hash_empty_test() ->
    ?assertEqual(
       "8350e5a3e24c153df2275c9f80692773",
       to_string(eipmi_md2:hash(""))).

hash_a_test() ->
    ?assertEqual(
       "32ec01ec4a6dac72c0ab96fb34c0b5d1",
       to_string(eipmi_md2:hash("a"))).

hash_abc_test() ->
    ?assertEqual(
       "da853b0d3f88d99b30283a69e6ded6bb",
       to_string(eipmi_md2:hash("abc"))).

hash_message_digest_test() ->
    ?assertEqual(
       "ab4f496bfb2a530b219ff33031fe06b0",
       to_string(eipmi_md2:hash("message digest"))).

hash_15chars_test() ->
    ?assertEqual(
       "de55ecdd23edf68b25b0b14d3d95fe06",
       to_string(eipmi_md2:hash("012345678901234"))).

hash_16chars_test() ->
    ?assertEqual(
       "4c13aeb6bc05a52e1c8b658c93088a39",
       to_string(eipmi_md2:hash("0123456789012345"))).

hash_17chars_test() ->
    ?assertEqual(
       "b1eca9ec65e8ff8b2a079ba458c8f165",
       to_string(eipmi_md2:hash("01234567890123456"))).

hash_alphabet_test() ->
    ?assertEqual(
       "4e8ddff3650292ab5a4108c3aa47940b",
       to_string(eipmi_md2:hash("abcdefghijklmnopqrstuvwxyz"))).

hash_alphabet_and_digits_test() ->
    Str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    ?assertEqual(
       "da33def2a42df13975352846c30338cd",
       to_string(eipmi_md2:hash(Str))).

hash_random_test() ->
    Str = "12345678901234567890123456789012345678901234567890123456789012345678901234567890",
    ?assertEqual(
       "d5976f79d83d3a0dc9806c3c66f3efd8",
       to_string(eipmi_md2:hash(Str))).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

to_string(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b", [XI])
                   || XI <- binary_to_list(Binary)]).
