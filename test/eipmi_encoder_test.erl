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

-module(eipmi_encoder_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

ack_test() ->
    ?assertEqual(
       <<16#06, 16#00, 16#01, 16#86>>,
       eipmi_encoder:ack(#rmcp_header{seq_nr = 1})).

ping_test() ->
    ?assertEqual(
       <<16#06, 16#00, 16#01, 16#06, 16#00, 16#00, 16#11, 16#be,
         16#80, 16#00, 16#00, 16#00>>,
       eipmi_encoder:ping(#rmcp_header{seq_nr = 1}, #asf_ping{})).

ipmi_test() ->
    ?assertEqual(
       <<16#06, 16#00, 16#ff, 16#07, 16#00, 16#00, 16#00, 16#00,
         16#00, 16#00, 16#00, 16#00, 16#00, 16#09, 16#20, 16#18,
         16#c8, 16#81, 16#00, 16#38, 16#0e, 16#04, 16#35>>,
       eipmi_encoder:ipmi(
         #rmcp_header{class = ?RMCP_IPMI},
         [{auth_type, none},
          {inbound_seq_nr, 0},
          {rq_addr, 16#81},
          {rq_seq_nr, 0},
          {session_id, 0},
          {rq_lun, ?IPMI_REQUESTOR_LUN},
          {rs_addr, ?IPMI_RESPONDER_ADDR},
          {rs_lun, ?IPMI_RESPONDER_LUN}],
         {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES},
         <<16#0e, 16#04>>)).
