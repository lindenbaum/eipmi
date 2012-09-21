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
%%% A module providing encoding functionality for RMCP packets.
%%% @end
%%%=============================================================================

-module(eipmi_encoder).

-export([ack/1,
         ping/2,
         ipmi/5]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a RMCP ACK packet. An ACK is a requested response to a formerly
%% received message. To be able to associate the ACK with the message it is
%% generated for specific values must/will be copied from the header of the
%% received message.
%% @end
%%------------------------------------------------------------------------------
-spec ack(#rmcp_header{}) ->
                 binary().
ack(Header = #rmcp_header{class = ?RMCP_ASF}) ->
    header(Header, ?RMCP_ACK).

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a RMCP ASF Ping packet. A Ping is usually sent to retrieve the IPMI
%% capabilities of the far end.
%% @end
%%------------------------------------------------------------------------------
-spec ping(#rmcp_header{}, #asf_ping{}) ->
                  binary().
ping(Header = #rmcp_header{class = ?RMCP_ASF}, #asf_ping{iana = I, tag = T}) ->
    HeaderBin = header(Header, ?RMCP_NORMAL),
    PingBin = <<I:32, ?ASF_PING:8, T:8, ?EIPMI_RESERVED:8, 0:8>>,
    <<HeaderBin/binary, PingBin/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a RMCP IPMI packet. This encapsulates the logic binary packet
%% construction and checksum calculation.
%% @end
%%------------------------------------------------------------------------------
-spec ipmi(#rmcp_header{}, #ipmi_session{}, #ipmi_request{}, 0..255, binary()) ->
                  binary().
ipmi(Header = #rmcp_header{class = ?RMCP_IPMI}, Session, Request, Cmd, Data) ->
    HeaderBin = header(Header, ?RMCP_NORMAL),
    SessionBin = session(Session),
    RequestBin = request(Request, Cmd, Data),
    Length = size(RequestBin),
    <<HeaderBin/binary, SessionBin/binary, Length:8, RequestBin/binary>>.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
header(#rmcp_header{version = V, seq_nr = S, class = C}, Ack) ->
    <<V:8, ?EIPMI_RESERVED:8, S:8, Ack:1, ?EIPMI_RESERVED:2, C:5>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
session(#ipmi_session{type = none, seq_nr = S, id = I}) ->
    <<?EIPMI_RESERVED:4, (encode_auth_type(none)):4, S:32, I:32>>;
session(#ipmi_session{type = T, seq_nr = S, id = I, code = C}) ->
    <<?EIPMI_RESERVED:4, (encode_auth_type(T)):4, S:32, I:32, C:128>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request(Request, Cmd, Data) ->
    Head = request_head(Request),
    HeadSum = calc_checksum(Head),
    Tail = request_tail(Request, Cmd, Data),
    TailSum = calc_checksum(Tail),
    <<Head/binary, HeadSum:8/signed, Tail/binary, TailSum:8/signed>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_head(#ipmi_request{rs_addr = A, net_fn = N, rs_lun = L}) ->
    <<A:8, N:6, L:2>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_tail(#ipmi_request{rq_addr = A, rq_seq_nr = S, rq_lun = L}, C, D) ->
    <<A:8, S:6, L:2, C:8, D/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Calculates the two's complement of the 8-bit checksum of the input binary.
%% Use <code>(calc_checksum(Binary)):7/signed</code> to insert into a binary.
%% @end
%%------------------------------------------------------------------------------
calc_checksum(Binary) ->
    List = binary_to_list(Binary),
    bnot lists:foldl(fun(Byte, Acc) -> (Acc + Byte) rem 256 end, 0, List) + 1.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_auth_type(none) -> 0;
encode_auth_type(md2) -> 1;
encode_auth_type(md5) -> 2;
encode_auth_type(pwd) -> 4.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_privilege_level(callback) -> 1;
encode_privilege_level(user) -> 2;
encode_privilege_level(operator) -> 3;
encode_privilege_level(administrator) -> 4.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_completion_code(command_completed_normally) -> 16#00;
encode_completion_code(node_busy) -> 16#c0;
encode_completion_code(invalid_command) -> 16#c1;
encode_completion_code(invalid_command_for_lun) -> 16#c2;
encode_completion_code(timeout) -> 16#c3;
encode_completion_code(out_of_space) -> 16#c4;
encode_completion_code(reservation_canceled) -> 16#c5;
encode_completion_code(data_truncated) -> 16#c6;
encode_completion_code(data_length_invalid) -> 16#c7;
encode_completion_code(data_length_limit_exceeded) -> 16#c8;
encode_completion_code(parameter_out_of_range) -> 16#c9;
encode_completion_code(cannot_return_number_of_requested_data_bytes) -> 16#ca;
encode_completion_code(requested_sensor_not_present) -> 16#cb;
encode_completion_code(invalid_data_field) -> 16#cc;
encode_completion_code(command_illegal_for_sensor) -> 16#cd;
encode_completion_code(response_not_provided) -> 16#ce;
encode_completion_code(duplicated_request) -> 16#cf;
encode_completion_code(sdr_repository_in_update_mode) -> 16#d0;
encode_completion_code(device_in_firmware_update_mode) -> 16#d1;
encode_completion_code(bmc_initialization_in_progress) -> 16#d2;
encode_completion_code(destination_unavailable) -> 16#d3;
encode_completion_code(insufficient_privilege_level) -> 16#d4;
encode_completion_code(command_not_supported) -> 16#d5;
encode_completion_code(command_disabled) -> 16#d6;
encode_completion_code(_) -> 16#ff.
