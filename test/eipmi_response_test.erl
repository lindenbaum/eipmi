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

-module(eipmi_response_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

decode_get_channel_authentication_capabilities_test() ->
    Cmd = ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
    Bin = <<16#00, 16#17, 16#1f, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    Properties = eipmi_response:decode(Cmd, Bin),
    ?assertEqual(false, eipmi_util:get_val(?PER_MSG_ENABLED, Properties)),
    ?assertEqual([pwd, md5, md2, none],
                 eipmi_util:get_val(?AUTH_TYPES, Properties)),
    ?assertEqual([non_null, null, anonymous],
                 eipmi_util:get_val(?LOGIN_STATUS, Properties)).

decode_get_session_challenge_test() ->
    Cmd = ?GET_SESSION_CHALLENGE,
    Bin = <<16#44, 16#33, 16#22, 16#11, $h, $e, $l, $l, $o, $_, $w, $o, $r,
            $l, $d, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    Properties = eipmi_response:decode(Cmd, Bin),
    ?assertEqual(16#11223344, eipmi_util:get_val(?SESSION_ID, Properties)),
    ?assertEqual(<<$h, $e, $l, $l, $o, $_, $w, $o, $r, $l, $d,
                   16#00, 16#00, 16#00, 16#00, 16#00>>,
                 eipmi_util:get_val(?CHALLENGE, Properties)).

decode_activate_session_test() ->
    Cmd = ?ACTIVATE_SESSION,
    Bin = <<16#00, 16#44, 16#33, 16#22, 16#11, 16#88, 16#77, 16#66, 16#55, 16#04>>,
    Properties = eipmi_response:decode(Cmd, Bin),
    ?assertEqual(none, eipmi_util:get_val(?AUTH_TYPE, Properties)),
    ?assertEqual(16#11223344, eipmi_util:get_val(?SESSION_ID, Properties)),
    ?assertEqual(16#55667788, eipmi_util:get_val(?INBOUND_SEQ_NR, Properties)),
    ?assertEqual(administrator, eipmi_util:get_val(?PRIVILEGE, Properties)).

decode_close_session_test() ->
    Cmd = ?CLOSE_SESSION,
    Bin = <<>>,
    ?assertEqual([], eipmi_response:decode(Cmd, Bin)).
