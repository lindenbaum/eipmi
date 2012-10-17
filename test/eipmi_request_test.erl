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

-module(eipmi_request_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

encode_get_channel_authentication_capabilities_test() ->
    Cmd = ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
    Properties = [?PRIVILEGE(administrator)],
    ?assertEqual(
       <<16#0e, 16#04>>,
       eipmi_request:encode(Cmd, Properties)).

encode_get_session_challenge_test() ->
    Cmd = ?GET_SESSION_CHALLENGE,
    Properties = [?AUTH_TYPE(none), ?USER("hello_world")],
    ?assertEqual(
       <<16#00, $h, $e, $l, $l, $o, $_, $w, $o, $r, $l, $d,
         16#00, 16#00, 16#00, 16#00, 16#00>>,
       eipmi_request:encode(Cmd, Properties)).

encode_activate_session_test() ->
    Cmd = ?ACTIVATE_SESSION,
    Properties = [?AUTH_TYPE(none), ?PRIVILEGE(administrator),
                  ?CHALLENGE(<<$h, $e, $l, $l, $o, 16#00, 16#00, 16#00, 16#00,
                               16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>),
                  ?OUTBOUND_SEQ_NR(16#11223344)],
    ?assertEqual(
       <<16#00, 16#04, $h, $e, $l, $l, $o, 16#00, 16#00, 16#00, 16#00, 16#00,
         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#44, 16#33, 16#22, 16#11>>,
       eipmi_request:encode(Cmd, Properties)).

encode_close_session_test() ->
    Cmd = ?CLOSE_SESSION,
    Properties = [?SESSION_ID(16#11223344)],
    ?assertEqual(
       <<16#44, 16#33, 16#22, 16#11>>,
       eipmi_request:encode(Cmd, Properties)).
