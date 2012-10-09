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

-export([encrypt/2,
         encode_type/1]).

-type type() :: none | md2 | md5 | pwd.

-export_type([type/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encrypts a given binary according to the requested authentication
%% algorithm. The returned cipher is always a binary of 16bytes or an empty
%% binary for authentication code `none'. Straight passwords are either padded
%% or cut to a length of 16bytes.
%% @end
%%------------------------------------------------------------------------------
encrypt(none, _Ignored) ->
    <<>>;
encrypt(md2, Binary) ->
    md2:hash(Binary);
encrypt(md5, Binary) ->
    crypto:md5(Binary);
encrypt(pwd, Password) ->
    eipmi_util:normalize(16, Password).

%%------------------------------------------------------------------------------
%% @doc
%% Encodes an authentication type into its integer representation.
%% @end
%%------------------------------------------------------------------------------
encode_type(none) -> 0;
encode_type(md2) -> 1;
encode_type(md5) -> 2;
encode_type(pwd) -> 4.
