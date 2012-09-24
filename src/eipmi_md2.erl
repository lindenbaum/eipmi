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

-define(PI_SUBST, {
          41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
          19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
          76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
          138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
          245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
          148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
          39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
          181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
          150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
          112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
          96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
          85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
          234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
          129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
          8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
          203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
          166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
          31, 26, 219, 153, 141, 51, 159, 17, 131, 20 }).

-define(PADDING, {
          << 1:8>>,
          << 2:8,  2:8>>,
          << 3:8,  3:8,  3:8>>,
          << 4:8,  4:8,  4:8,  4:8>>,
          << 5:8,  5:8,  5:8,  5:8,  5:8>>,
          << 6:8,  6:8,  6:8,  6:8,  6:8,  6:8>>,
          << 7:8,  7:8,  7:8,  7:8,  7:8,  7:8,  7:8>>,
          << 8:8,  8:8,  8:8,  8:8,  8:8,  8:8,  8:8,  8:8>>,
          << 9:8,  9:8,  9:8,  9:8,  9:8,  9:8,  9:8,  9:8,  9:8>>,
          <<10:8, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8, 10:8>>,
          <<11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8, 11:8>>,
          <<12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8, 12:8,
            12:8>>,
          <<13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8, 13:8,
            13:8, 13:8>>,
          <<14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8, 14:8,
            14:8, 14:8, 14:8>>,
          <<15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8, 15:8,
            15:8, 15:8, 15:8, 15:8>>,
          <<16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8, 16:8,
            16:8, 16:8, 16:8, 16:8, 16:8>>
         }).

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
    Padding = padding(size(In)),
    Checksum = checksum(<<In/binary, Padding/binary>>),
    digest(<<In/binary, Padding/binary, Checksum/binary>>).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
padding(InSize) ->
    Padding = ?BLOCK - (InSize rem ?BLOCK),
    erlang:element(case Padding of 0 -> 16; I -> I end, ?PADDING).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
checksum(M) ->
    checksum(0, M, erlang:make_tuple(?BLOCK, 0)).

checksum(_L, <<>>, Checksum) ->
    erlang:list_to_binary(erlang:tuple_to_list(Checksum));
checksum(L, <<M:?BLOCK/binary, MRest/binary>>, Checksum) ->
    {NewL, NewChecksum} = checksum_block(1, L, M, Checksum),
    checksum(NewL, MRest, NewChecksum).

checksum_block(_J, L, <<>>, Checksum) ->
    {L, Checksum};
checksum_block(J, L, <<C:8, CRest/binary>>, Checksum) ->
    NewCJ = erlang:element(J, Checksum) bxor pi_subst((C bxor L) + 1),
    checksum_block(J + 1, NewCJ, CRest, erlang:setelement(J, Checksum, NewCJ)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
digest(M) ->
    digest(M, erlang:make_tuple(?BLOCK * 3, 0)).

digest(<<>>, X) ->
    erlang:list_to_binary(lists:sublist(erlang:tuple_to_list(X), ?BLOCK));
digest(<<M:?BLOCK/binary, MRest/binary>>, X) ->
    digest(MRest, digest_outer_rounds(0, 0, digest_copy_block(1, M, X))).

digest_copy_block(_J, <<>>, X) ->
    X;
digest_copy_block(J, <<MJ:8, MJRest/binary>>, X) ->
    digest_copy_block(
      J + 1,
      MJRest,
      erlang:setelement(
        ?BLOCK + ?BLOCK + J,
        erlang:setelement(?BLOCK + J, X, MJ),
        MJ bxor erlang:element(J, X))).

digest_outer_rounds(18, _T, X) ->
    X;
digest_outer_rounds(J, T, X) ->
    {NewT, NewX} = digest_inner_rounds(J, 1, T, X),
    digest_outer_rounds(J + 1, NewT, NewX).

digest_inner_rounds(J, K, T, X) ->
    case K =:= (3 * ?BLOCK + 1) of
        true ->
            {(T + J) rem 256, X};
        false ->
            NewT = erlang:element(K, X) bxor pi_subst(T + 1),
            digest_inner_rounds(J, K + 1, NewT, erlang:setelement(K, X, NewT))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
pi_subst(Index) ->
    element(Index, ?PI_SUBST).
