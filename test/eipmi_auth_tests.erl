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
    ?assertEqual(Encrypted, eipmi_auth:hash(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

md2_test() ->
    AuthType = md2,
    Unencrypted = <<"abcd">>,
    Encrypted = eipmi_auth:hash(AuthType, Unencrypted),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

md5_test() ->
    AuthType = md5,
    Unencrypted = <<"abcd">>,
    Encrypted = eipmi_auth:hash(AuthType, Unencrypted),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

string_pwd_test() ->
    AuthType = pwd,
    Unencrypted = "abcd",
    Encrypted = <<"abcd", 0:(12*8)>>,
    ?assertEqual(Encrypted, eipmi_auth:hash(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

short_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcd">>,
    Encrypted = <<"abcd", 0:(12*8)>>,
    ?assertEqual(Encrypted, eipmi_auth:hash(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

exact_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcdefghijklmnop">>,
    Encrypted = <<"abcdefghijklmnop">>,
    ?assertEqual(Encrypted, eipmi_auth:hash(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

long_pwd_test() ->
    AuthType = pwd,
    Unencrypted = <<"abcdefghijklmnopqrstuvwxyz">>,
    Encrypted = <<"abcdefghijklmnop">>,
    ?assertEqual(Encrypted, eipmi_auth:hash(AuthType, Unencrypted)),
    ?assert(validate(AuthType, Unencrypted, Encrypted)).

hmac_md5_test() ->
    HashType = hmac_md5_128,
    Key =
        <<16#37, 16#8d, 16#60, 16#63, 16#fe, 16#16,
          16#05, 16#2c, 16#cc, 16#40, 16#bf, 16#88,
          16#92, 16#af, 16#28, 16#4a>>,
    Clear = <<"abcdefghijklmnopqrstuvwxyz">>,
    Hashed =
        <<16#04, 16#c0, 16#46, 16#ef, 16#75, 16#a1,
          16#ff, 16#11, 16#db, 16#4d, 16#f3, 16#b2,
          16#39, 16#21, 16#20, 16#59>>,
    ?assertEqual(Hashed, eipmi_auth:hash(HashType, Key, Clear)).

hmac_sha1_test() ->
    HashType = hmac_sha1_96,
    Key =
        <<16#37, 16#8d, 16#60, 16#63, 16#fe, 16#16,
          16#05, 16#2c, 16#cc, 16#40, 16#bf, 16#88,
          16#92, 16#af, 16#28, 16#4a>>,
    Clear = <<"abcdefghijklmnopqrstuvwxyz">>,
    Hashed =
        <<16#82, 16#5b, 16#5b, 16#c1, 16#7b, 16#56,
          16#f9, 16#d9, 16#27, 16#72, 16#76, 16#07>>,
    ?assertEqual(Hashed, eipmi_auth:hash(HashType, Key, Clear)).

hmac_sha256_test() ->
    HashType = hmac_sha256_128,
    Key =
        <<16#37, 16#8d, 16#60, 16#63, 16#fe, 16#16,
          16#05, 16#2c, 16#cc, 16#40, 16#bf, 16#88,
          16#92, 16#af, 16#28, 16#4a>>,
    Clear = <<"abcdefghijklmnopqrstuvwxyz">>,
    Hashed =
        <<16#33, 16#ca, 16#58, 16#57, 16#8a, 16#fc,
          16#86, 16#e6, 16#6d, 16#24, 16#8d, 16#98,
          16#0f, 16#6d, 16#8d, 16#06>>,
    ?assertEqual(Hashed, eipmi_auth:hash(HashType, Key, Clear)).

none_encryption_test() ->
    EncryptType = none,
    Key = <<>>,
    Unencrypted = <<"abc">>,
    ?assertEqual(Unencrypted, eipmi_auth:encrypt(EncryptType, Key, Unencrypted)),
    ?assertEqual(Unencrypted, eipmi_auth:decrypt(EncryptType, Key, Unencrypted)).

aes_encryption_test() ->
    EncryptType = aes_cbc,
    Key = crypto:strong_rand_bytes(16),
    Unencrypted = <<"abcdefghijklmnopqrstuvwxyz">>,
    Encrypted = eipmi_auth:encrypt(EncryptType, Key, Unencrypted),
    ?assertEqual(Unencrypted, eipmi_auth:decrypt(EncryptType, Key, Encrypted)).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

validate(none, _, _) ->
    true;
validate(AuthType, Unencrypted, Encrypted) ->
    eipmi_auth:hash(AuthType, Unencrypted) =:= Encrypted.
