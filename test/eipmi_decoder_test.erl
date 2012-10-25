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

-module(eipmi_decoder_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

not_rmcp_packet_test() ->
    ?assertEqual(
       {error, not_rmcp_packet},
       eipmi_decoder:packet(<<>>)).

unsupported_rmcp_packet_test() ->
    ?assertEqual(
       {error, unsupported_rmcp_packet},
       eipmi_decoder:packet(<<16#06, 16#00, 16#01, 16#00>>)).

unsupported_asf_packet_test() ->
    ?assertEqual(
       {error, unsupported_asf_packet},
       eipmi_decoder:packet(<<16#06, 16#00, 16#01, 16#06, 16#00, 16#00>>)).

ack_test() ->
    ?assertEqual(
       {ok, #rmcp_ack{header = #rmcp_header{seq_nr = 1}}},
       eipmi_decoder:packet(<<16#06, 16#00, 16#01, 16#86>>)).

pong_test() ->
    ?assertEqual(
       {ok, #rmcp_asf{
               header = #rmcp_header{seq_nr = 1},
               payload = #asf_pong{entities = [ipmi]}}},
       eipmi_decoder:packet(
         <<16#06, 16#00, 16#01, 16#06, 16#00, 16#00, 16#11,
           16#be, 16#40, 16#00, 16#00, 16#10, 16#00, 16#00,
           16#11, 16#be, 16#00, 16#00, 16#00, 16#00, 16#81,
           16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>)).

ipmi_response_test() ->
    {ok, Ipmi} =
        eipmi_decoder:packet(
          <<16#06, 16#00, 16#ff, 16#07, 16#00, 16#00, 16#00,
            16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#10,
            16#81, 16#1c, 16#63, 16#20, 16#00, 16#38, 16#00,
            16#00, 16#01, 16#19, 16#00, 16#00, 16#00, 16#00,
            16#00, 16#8e>>),
    ?assertMatch(
       #rmcp_ipmi{
          header = #rmcp_header{class = ?RMCP_IPMI},
          cmd = {?IPMI_NETFN_APPLICATION_RESPONSE,
                 ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES},
          data = <<16#00, 16#01, 16#19, 16#00, 16#00, 16#00, 16#00, 16#00>>},
       Ipmi),
    Ps = Ipmi#rmcp_ipmi.properties,
    ?assertEqual(none, eipmi_util:get_val(auth_type, Ps)),
    ?assertEqual(0, eipmi_util:get_val(outbound_seq_nr, Ps)),
    ?assertEqual(0, eipmi_util:get_val(session_id, Ps)),
    ?assertEqual(16#81, eipmi_util:get_val(rq_addr, Ps)),
    ?assertEqual(0, eipmi_util:get_val(rq_seq_nr, Ps)),
    ?assertEqual(normal, eipmi_util:get_val(completion, Ps)).
