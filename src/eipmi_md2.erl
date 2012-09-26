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
%%%
%%% @doc
%%% A module providing an MD2 hash implementation according to RFC 1319.
%%% @end
%%%=============================================================================

-module(eipmi_md2).

-export([hash/1]).

-define(BLOCK, 16).

-define(INITIAL_T, 0).
-define(INITIAL_L, 0).
-define(INITIAL_I, 1).
-define(INITIAL_J, 0).
-define(INITIAL_K, 1).
-define(INITIAL_C, erlang:make_tuple(?BLOCK, 0)).
-define(INITIAL_X, erlang:make_tuple(?BLOCK * 3, 0)).

-define(PAD01(Bin), <<Bin/binary, 1:8>>).
-define(PAD02(Bin), <<Bin/binary, 2:8, 2:8>>).
-define(PAD03(Bin), <<Bin/binary, 3:8, 3:8, 3:8>>).
-define(PAD04(Bin), <<Bin/binary, 4:8, 4:8, 4:8, 4:8>>).
-define(PAD05(Bin), <<Bin/binary, 5:8, 5:8, 5:8, 5:8, 5:8>>).
-define(PAD06(Bin), <<Bin/binary, 6:8, 6:8, 6:8, 6:8, 6:8, 6:8>>).
-define(PAD07(Bin), <<Bin/binary, 7:8, 7:8, 7:8, 7:8, 7:8, 7:8, 7:8>>).
-define(PAD08(Bin), <<Bin/binary, 8:8, 8:8, 8:8, 8:8, 8:8, 8:8, 8:8, 8:8>>).
-define(PAD09(Bin), <<Bin/binary, 9:8, 9:8, 9:8, 9:8, 9:8, 9:8, 9:8, 9:8,
                      9:8>>).
-define(PAD10(Bin), <<Bin/binary, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8,
                      10:8, 10:8, 10:8>>).
-define(PAD11(Bin), <<Bin/binary, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8,
                      11:8, 11:8, 11:8, 11:8>>).
-define(PAD12(Bin), <<Bin/binary, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8,
                      12:8, 12:8, 12:8, 12:8, 12:8>>).
-define(PAD13(Bin), <<Bin/binary, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8,
                      13:8, 13:8, 13:8, 13:8, 13:8, 13:8>>).
-define(PAD14(Bin), <<Bin/binary, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8,
                      14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8>>).
-define(PAD15(Bin), <<Bin/binary, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8,
                      15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8>>).
-define(PAD16, <<16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8,
                 16:8, 16:8, 16:8, 16:8, 16:8, 16:8>>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
-spec hash(string() | binary()) ->
                  binary().
hash(In) when is_list(In) ->
    hash(erlang:list_to_binary(In));
hash(In) when is_binary(In) ->
    loop(In).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Process the whole binary in blocks of 16bytes.
%%------------------------------------------------------------------------------
loop(In) ->
    loop(In, {?INITIAL_L, ?INITIAL_C}, ?INITIAL_X).

loop(<<>>, {L, C}, X) ->
    loop_final(?PAD16, {L, C}, X);
loop(<<Current:1/binary>>, {L, C}, X) ->
    loop_final(?PAD15(Current), {L, C}, X);
loop(<<Current:2/binary>>, {L, C}, X) ->
    loop_final(?PAD14(Current), {L, C}, X);
loop(<<Current:3/binary>>, {L, C}, X) ->
    loop_final(?PAD13(Current), {L, C}, X);
loop(<<Current:4/binary>>, {L, C}, X) ->
    loop_final(?PAD12(Current), {L, C}, X);
loop(<<Current:5/binary>>, {L, C}, X) ->
    loop_final(?PAD11(Current), {L, C}, X);
loop(<<Current:6/binary>>, {L, C}, X) ->
    loop_final(?PAD10(Current), {L, C}, X);
loop(<<Current:7/binary>>, {L, C}, X) ->
    loop_final(?PAD09(Current), {L, C}, X);
loop(<<Current:8/binary>>, {L, C}, X) ->
    loop_final(?PAD08(Current), {L, C}, X);
loop(<<Current:9/binary>>, {L, C}, X) ->
    loop_final(?PAD07(Current), {L, C}, X);
loop(<<Current:10/binary>>, {L, C}, X) ->
    loop_final(?PAD06(Current), {L, C}, X);
loop(<<Current:11/binary>>, {L, C}, X) ->
    loop_final(?PAD05(Current), {L, C}, X);
loop(<<Current:12/binary>>, {L, C}, X) ->
    loop_final(?PAD04(Current), {L, C}, X);
loop(<<Current:13/binary>>, {L, C}, X) ->
    loop_final(?PAD03(Current), {L, C}, X);
loop(<<Current:14/binary>>, {L, C}, X) ->
    loop_final(?PAD02(Current), {L, C}, X);
loop(<<Current:15/binary>>, {L, C}, X) ->
    loop_final(?PAD01(Current), {L, C}, X);
loop(<<Current:?BLOCK/binary, Rest/binary>>, {L, C}, X) ->
    {NewL, NewC, NewX} = loop_block(?INITIAL_I, Current, {L, C}, X),
    loop(Rest, {NewL, NewC}, digest(?INITIAL_T, NewX)).

%%------------------------------------------------------------------------------
%% @private
%% Process the last 32bytes which is the last 16bytes (padded) of the binary and
%% the 16bytes checksum.
%%------------------------------------------------------------------------------
loop_final(<<Current:?BLOCK/binary>>, {L, C}, X) ->
    %% last (padded) block
    {NewL, NewC, NewX} = loop_block(?INITIAL_I, Current, {L, C}, X),
    NextX = digest(?INITIAL_T, NewX),
    %% checksum block
    CBlock = tuple_to_binary(NewC),
    {_, _, PreFinalX} = loop_block(?INITIAL_I, CBlock, {NewL, NewC}, NextX),
    tuple_to_binary(digest(?INITIAL_T, PreFinalX), ?BLOCK).

%%------------------------------------------------------------------------------
%% @private
%% Process each byte of a 16byte block.
%%------------------------------------------------------------------------------
loop_block(_, <<>>, {L, C}, X) ->
    {L, C, X};
loop_block(I, <<B:8, Rest/binary>>, {L, C}, X) ->
    loop_block(I + 1, Rest, checksum_step(I, B, L, C), digest_step(I, B, X)).

%%------------------------------------------------------------------------------
%% @private
%% The checksum step of the block iteration.
%%------------------------------------------------------------------------------
checksum_step(I, B, L, C) ->
    NewCI = erlang:element(I, C) bxor pi(B bxor L),
    {NewCI, erlang:setelement(I, C, NewCI)}.

%%------------------------------------------------------------------------------
%% @private
%% The digest step of the block iteration.
%%------------------------------------------------------------------------------
digest_step(I, B, X) ->
    erlang:setelement(
      ?BLOCK + ?BLOCK + I,
      erlang:setelement(?BLOCK + I, X, B),
      B bxor erlang:element(I, X)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
digest(T, X) ->
    loop18(?INITIAL_J, {T, X}).

%%------------------------------------------------------------------------------
%% @private
%% Make the 18-loop digest magic
%%------------------------------------------------------------------------------
loop18(18, {_, X}) ->
    X;
loop18(J, {T, X}) ->
    loop18(J + 1, loop_x(?BLOCK * 3 + 1, J, ?INITIAL_K, T, X)).

%%------------------------------------------------------------------------------
%% @private
%% Loop over X (the complete digest buffer).
%%------------------------------------------------------------------------------
loop_x(K, J, K, T, X) ->
    {(T + J) rem 256, X};
loop_x(Exit, J, K, T, X) ->
    NewT = erlang:element(K, X) bxor pi(T),
    loop_x(Exit, J, K + 1, NewT, erlang:setelement(K, X, NewT)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
tuple_to_binary(Tuple) ->
    erlang:list_to_binary(erlang:tuple_to_list(Tuple)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
tuple_to_binary(Tuple, MaxLen) ->
    erlang:list_to_binary(lists:sublist(erlang:tuple_to_list(Tuple), MaxLen)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_byte(Integer, I) ->
    (Integer band (16#ff bsl I * 8)) bsr I * 8.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_byte(Integer, I, Value) ->
    (Integer band bnot (16#ff bsl I * 8)) bor (Value bsl I * 8).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
pi(0) -> 41;
pi(1) -> 46;
pi(2) -> 67;
pi(3) -> 201;
pi(4) -> 162;
pi(5) -> 216;
pi(6) -> 124;
pi(7) -> 1;
pi(8) -> 61;
pi(9) -> 54;
pi(10) -> 84;
pi(11) -> 161;
pi(12) -> 236;
pi(13) -> 240;
pi(14) -> 6;
pi(15) -> 19;
pi(16) -> 98;
pi(17) -> 167;
pi(18) -> 5;
pi(19) -> 243;
pi(20) -> 192;
pi(21) -> 199;
pi(22) -> 115;
pi(23) -> 140;
pi(24) -> 152;
pi(25) -> 147;
pi(26) -> 43;
pi(27) -> 217;
pi(28) -> 188;
pi(29) -> 76;
pi(30) -> 130;
pi(31) -> 202;
pi(32) -> 30;
pi(33) -> 155;
pi(34) -> 87;
pi(35) -> 60;
pi(36) -> 253;
pi(37) -> 212;
pi(38) -> 224;
pi(39) -> 22;
pi(40) -> 103;
pi(41) -> 66;
pi(42) -> 111;
pi(43) -> 24;
pi(44) -> 138;
pi(45) -> 23;
pi(46) -> 229;
pi(47) -> 18;
pi(48) -> 190;
pi(49) -> 78;
pi(50) -> 196;
pi(51) -> 214;
pi(52) -> 218;
pi(53) -> 158;
pi(54) -> 222;
pi(55) -> 73;
pi(56) -> 160;
pi(57) -> 251;
pi(58) -> 245;
pi(59) -> 142;
pi(60) -> 187;
pi(61) -> 47;
pi(62) -> 238;
pi(63) -> 122;
pi(64) -> 169;
pi(65) -> 104;
pi(66) -> 121;
pi(67) -> 145;
pi(68) -> 21;
pi(69) -> 178;
pi(70) -> 7;
pi(71) -> 63;
pi(72) -> 148;
pi(73) -> 194;
pi(74) -> 16;
pi(75) -> 137;
pi(76) -> 11;
pi(77) -> 34;
pi(78) -> 95;
pi(79) -> 33;
pi(80) -> 128;
pi(81) -> 127;
pi(82) -> 93;
pi(83) -> 154;
pi(84) -> 90;
pi(85) -> 144;
pi(86) -> 50;
pi(87) -> 39;
pi(88) -> 53;
pi(89) -> 62;
pi(90) -> 204;
pi(91) -> 231;
pi(92) -> 191;
pi(93) -> 247;
pi(94) -> 151;
pi(95) -> 3;
pi(96) -> 255;
pi(97) -> 25;
pi(98) -> 48;
pi(99) -> 179;
pi(100) -> 72;
pi(101) -> 165;
pi(102) -> 181;
pi(103) -> 209;
pi(104) -> 215;
pi(105) -> 94;
pi(106) -> 146;
pi(107) -> 42;
pi(108) -> 172;
pi(109) -> 86;
pi(110) -> 170;
pi(111) -> 198;
pi(112) -> 79;
pi(113) -> 184;
pi(114) -> 56;
pi(115) -> 210;
pi(116) -> 150;
pi(117) -> 164;
pi(118) -> 125;
pi(119) -> 182;
pi(120) -> 118;
pi(121) -> 252;
pi(122) -> 107;
pi(123) -> 226;
pi(124) -> 156;
pi(125) -> 116;
pi(126) -> 4;
pi(127) -> 241;
pi(128) -> 69;
pi(129) -> 157;
pi(130) -> 112;
pi(131) -> 89;
pi(132) -> 100;
pi(133) -> 113;
pi(134) -> 135;
pi(135) -> 32;
pi(136) -> 134;
pi(137) -> 91;
pi(138) -> 207;
pi(139) -> 101;
pi(140) -> 230;
pi(141) -> 45;
pi(142) -> 168;
pi(143) -> 2;
pi(144) -> 27;
pi(145) -> 96;
pi(146) -> 37;
pi(147) -> 173;
pi(148) -> 174;
pi(149) -> 176;
pi(150) -> 185;
pi(151) -> 246;
pi(152) -> 28;
pi(153) -> 70;
pi(154) -> 97;
pi(155) -> 105;
pi(156) -> 52;
pi(157) -> 64;
pi(158) -> 126;
pi(159) -> 15;
pi(160) -> 85;
pi(161) -> 71;
pi(162) -> 163;
pi(163) -> 35;
pi(164) -> 221;
pi(165) -> 81;
pi(166) -> 175;
pi(167) -> 58;
pi(168) -> 195;
pi(169) -> 92;
pi(170) -> 249;
pi(171) -> 206;
pi(172) -> 186;
pi(173) -> 197;
pi(174) -> 234;
pi(175) -> 38;
pi(176) -> 44;
pi(177) -> 83;
pi(178) -> 13;
pi(179) -> 110;
pi(180) -> 133;
pi(181) -> 40;
pi(182) -> 132;
pi(183) -> 9;
pi(184) -> 211;
pi(185) -> 223;
pi(186) -> 205;
pi(187) -> 244;
pi(188) -> 65;
pi(189) -> 129;
pi(190) -> 77;
pi(191) -> 82;
pi(192) -> 106;
pi(193) -> 220;
pi(194) -> 55;
pi(195) -> 200;
pi(196) -> 108;
pi(197) -> 193;
pi(198) -> 171;
pi(199) -> 250;
pi(200) -> 36;
pi(201) -> 225;
pi(202) -> 123;
pi(203) -> 8;
pi(204) -> 12;
pi(205) -> 189;
pi(206) -> 177;
pi(207) -> 74;
pi(208) -> 120;
pi(209) -> 136;
pi(210) -> 149;
pi(211) -> 139;
pi(212) -> 227;
pi(213) -> 99;
pi(214) -> 232;
pi(215) -> 109;
pi(216) -> 233;
pi(217) -> 203;
pi(218) -> 213;
pi(219) -> 254;
pi(220) -> 59;
pi(221) -> 0;
pi(222) -> 29;
pi(223) -> 57;
pi(224) -> 242;
pi(225) -> 239;
pi(226) -> 183;
pi(227) -> 14;
pi(228) -> 102;
pi(229) -> 88;
pi(230) -> 208;
pi(231) -> 228;
pi(232) -> 166;
pi(233) -> 119;
pi(234) -> 114;
pi(235) -> 248;
pi(236) -> 235;
pi(237) -> 117;
pi(238) -> 75;
pi(239) -> 10;
pi(240) -> 49;
pi(241) -> 68;
pi(242) -> 80;
pi(243) -> 180;
pi(244) -> 143;
pi(245) -> 237;
pi(246) -> 31;
pi(247) -> 26;
pi(248) -> 219;
pi(249) -> 153;
pi(250) -> 141;
pi(251) -> 51;
pi(252) -> 159;
pi(253) -> 17;
pi(254) -> 131;
pi(255) -> 20.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_byte_test() ->
    ?assertEqual(16#33, get_byte(16#00112233, 0)),
    ?assertEqual(16#22, get_byte(16#00112233, 1)),
    ?assertEqual(16#11, get_byte(16#00112233, 2)),
    ?assertEqual(16#00, get_byte(16#00112233, 3)).

set_byte_test() ->
    ?assertEqual(16#00112244, set_byte(16#00112233, 0, 16#44)),
    ?assertEqual(16#4400, set_byte(16#00, 1, 16#44)),
    ?assertEqual(16#00114433, set_byte(16#00112233, 1, 16#44)),
    ?assertEqual(16#00442233, set_byte(16#00112233, 2, 16#44)),
    ?assertEqual(16#44112233, set_byte(16#00112233, 3, 16#44)).

-endif.
