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

-module(eipmi_auth_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

none_test() ->
    AuthType = none,
    Unencrypted = <<"abcd">>,
    Encrypted = <<>>,
    ?assertEqual(Encrypted, eipmi_auth:encrypt(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

md2_test() ->
    AuthType = md2,
    Unencrypted = <<"abcd">>,
    Encrypted = eipmi_auth:encrypt(AuthType, Unencrypted),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

md5_test() ->
    AuthType = md5,
    Unencrypted = <<"abcd">>,
    Encrypted = eipmi_auth:encrypt(AuthType, Unencrypted),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

string_pwd_test() ->
    AuthType = pwd,
    Unencrypted = "abcd",
    Encrypted = <<"abcd", 0:(12*8)>>,
    ?assertEqual(Encrypted, eipmi_auth:encrypt(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

short_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcd">>,
    Encrypted = <<"abcd", 0:(12*8)>>,
    ?assertEqual(Encrypted, eipmi_auth:encrypt(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

exact_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcdefghijklmnop">>,
    Encrypted = <<"abcdefghijklmnop">>,
    ?assertEqual(Encrypted, eipmi_auth:encrypt(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

long_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcdefghijklmnopqrstuvwxyz">>,
    Encrypted = <<"abcdefghijklmnop">>,
    ?assertEqual(Encrypted, eipmi_auth:encrypt(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

validate(none, _, _) ->
    true;
validate(AuthType, Unencrypted, Encrypted) ->
    eipmi_auth:encrypt(AuthType, Unencrypted) =:= Encrypted.
