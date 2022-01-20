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
%%% A module providing the supported authentication algorithms for IPMI packets.
%%% Currently supported algorithms are MD2, MD5 and straight password.
%%% @end
%%%=============================================================================

-module(eipmi_auth).

-export([encrypt/3,
         decrypt/3,
         encode_type/1,
         decode_type/1,
         hash/2
        ]).

-type encrypt_type() :: none | aes_cbc.
-type type() :: none | md2 | md5 | pwd | rmcp_plus.

-export_type([type/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Hashes a given binary according to the requested authentication
%% algorithm. The returned cipher is always a binary of 16bytes or an empty
%% binary for authentication code `none'. Straight passwords are either padded
%% or cut to a length of 16bytes.
%% @end
%%------------------------------------------------------------------------------
-spec hash(type(), binary() | string()) -> binary().
hash(none, _Ignored) ->
    <<>>;
hash(md2, Binary) ->
    md2:hash(Binary);
hash(md5, Binary) ->
    crypto:hash(md5, Binary);
hash(pwd, Password) ->
    eipmi_util:normalize(16, Password).

%%-------------------------------------------------------------------------------
%% @doc
%% Encrypts a given binary according to the requested confidentiality
%% algorithm. Returns a binary containing the Confidentiality Header and the
%% encrypted Payload + Confidentiality Trailer.
%%-------------------------------------------------------------------------------
-spec encrypt(encrypt_type(), binary(), binary()) -> binary().
encrypt(none, _Key, Binary) ->
    Binary;
encrypt(aes_cbc, Key, Binary) ->
    Iv = crypto:strong_rand_bytes(16),
    % IPMI uses a different padding scheme than the
    % crypto library: pad to a multiple of 16 bytes,
    % less 1 to encode the padding length; the padding
    % bytes are sequential numbers starting from 1.
    PadLength = 15 - size(Binary) rem 16,
    Padding = list_to_binary(lists:seq(1, PadLength)),
    ToEncrypt = <<Binary/binary, Padding/binary, PadLength:8>>,
    Encrypted = crypto:crypto_one_time(aes_128_cbc, Key, Iv, ToEncrypt,
                                       [{encrypt, true}, {padding, none}]),
    <<Iv/binary, Encrypted/binary>>.

decrypt(none, _Key, Binary) ->
    Binary;
decrypt(aes_cbc, Key, <<Iv:16/binary, Encrypted/binary>>) ->
    Result = crypto:crypto_one_time(aes_128_cbc, Key, Iv, Encrypted,
                                    [{encrypt, false}, {padding, none}]),
    PadLength = binary:last(Result),
    % PadLength does not count its own byte.
    binary_part(Result, {0, size(Result) - 1 - PadLength}).

%%------------------------------------------------------------------------------
%% @doc
%% Encodes an authentication type into its integer representation.
%% @end
%%------------------------------------------------------------------------------
-spec encode_type(type()) -> 0..6.
encode_type(none) -> 0;
encode_type(md2) -> 1;
encode_type(md5) -> 2;
encode_type(pwd) -> 4;
encode_type(rmcp_plus) -> 6.

%%------------------------------------------------------------------------------
%% @doc
%% Decodes an authentication type integer into human readable format.
%% @end
%%------------------------------------------------------------------------------
-spec decode_type(0..6) -> type().
decode_type(0) -> none;
decode_type(1) -> md2;
decode_type(2) -> md5;
decode_type(4) -> pwd;
decode_type(6) -> rmcp_plus.
