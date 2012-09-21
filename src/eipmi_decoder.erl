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
%%% A module providing decoding functionality for RMCP packets.
%%% @end
%%%=============================================================================

-module(eipmi_decoder).

-export([packet/1]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a binary representing a RMCP message erlang record representation.
%% @end
%%------------------------------------------------------------------------------
-spec packet(binary()) ->
                    {ok, #rmcp_ack{} | #rmcp_asf{} | #rmcp_ipmi{}} |
                    {error, term()}.
packet(<<?RMCP_VERSION:8, ?EIPMI_RESERVED:8, SeqNr:8, Rest/binary>>) ->
    class(SeqNr, Rest);
packet(_Binary) ->
    {error, not_rmcp_packet}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
class(SeqNr, <<?RMCP_ACK:1, ?EIPMI_RESERVED:2, Class:5>>) ->
    Header = #rmcp_header{seq_nr = SeqNr, class = Class},
    {ok, #rmcp_ack{header = Header}};
class(SeqNr, <<?RMCP_NORMAL:1, ?EIPMI_RESERVED:2, ?RMCP_ASF:5, Rest/binary>>) ->
    Header = #rmcp_header{seq_nr = SeqNr, class = ?RMCP_ASF},
    asf(#rmcp_asf{header = Header}, Rest);
class(SeqNr, <<?RMCP_NORMAL:1, ?EIPMI_RESERVED:2, ?RMCP_IPMI:5, Rest/binary>>) ->
    Header = #rmcp_header{seq_nr = SeqNr, class = ?RMCP_IPMI},
    ipmi(#rmcp_ipmi{header = Header}, Rest);
class(_SeqNr, _Binary) ->
    {error, unsupported_rmcp_packet}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
asf(Asf, <<?ASF_IANA:32, ?ASF_PONG:8, Tag:8, ?EIPMI_RESERVED:8,
           16:8, Iana:32, Oem:32, Entities:8, _:56>>) ->
    Es = case Entities of 2#10000001 -> [ipmi]; _ -> [] end,
    Pong = #asf_pong{iana = Iana, tag = Tag, oem = Oem, entities = Es},
    {ok, Asf#rmcp_asf{payload = Pong}};
asf(_Asf, _Binary) ->
    {error, unsupported_asf_packet}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ipmi(Ipmi, Binary) ->
    {Session, <<Size:8, Rest/binary>>} = session(Binary),
    Len = Size - (1 + 2 + 1 + 1) * 8,
    <<Head:16/binary, Sum1:8/signed, Tail:Len/binary, Sum2:8/signed>> = Rest,
    case has_integrity(Head, Sum1) andalso has_integrity(Tail, Sum2) of
        true ->
            lan(Ipmi#rmcp_ipmi{session = Session}, Head, Tail);

        false ->
            {error, corrupted_ipmi_message}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
session(<<?EIPMI_RESERVED:4, 0:4, SeqNr:32, Id:32, Rest/binary>>) ->
    {#ipmi_session{type = none, seq_nr = SeqNr, id = Id}, Rest};
session(<<?EIPMI_RESERVED:4, 1:4, SeqNr:32, Id:32, Code:128, Rest/binary>>) ->
    {#ipmi_session{type = md2, seq_nr = SeqNr, id = Id, code = Code}, Rest};
session(<<?EIPMI_RESERVED:4, 2:4, SeqNr:32, Id:32, Code:128, Rest/binary>>) ->
    {#ipmi_session{type = md5, seq_nr = SeqNr, id = Id, code = Code}, Rest};
session(<<?EIPMI_RESERVED:4, 3:4, SeqNr:32, Id:32, Code:128, Rest/binary>>) ->
    {#ipmi_session{type = pwd, seq_nr = SeqNr, id = Id, code = Code}, Rest}.

%%------------------------------------------------------------------------------
%% @private
%% Note: Currently only application responses are supported.
%%------------------------------------------------------------------------------
lan(Ipmi,
    <<RqAddr:8, ?IPMI_NETFN_APPLICATION_RESPONSE:6, RqLun:2>>,
    <<RsAddr:8, RqSeqNr:6, RsLun:2, Cmd:8, Code:8, Data/binary>>) ->
    Response = #ipmi_response{
                  rq_addr = RqAddr,
                  rq_lun = RqLun,
                  rq_seq_nr = RqSeqNr,
                  rs_addr = RsAddr,
                  rs_lun = RsLun,
                  completion_code = from_completion_code(Code)},
    {ok, Ipmi#rmcp_ipmi{type = Response, cmd = Cmd, data = Data}};
lan(_Ipmi, _Head, _Tail) ->
    {error, unsupported_ipmi_message}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
has_integrity(Binary, Checksum) ->
    Checksum == calc_checksum(Binary).

%%------------------------------------------------------------------------------
%% @doc
%% Calculates the two's complement of the 8-bit checksum of the input binary.
%% @end
%%------------------------------------------------------------------------------
calc_checksum(Binary) ->
    List = binary_to_list(Binary),
    bnot lists:foldl(fun(Byte, Acc) -> (Acc + Byte) rem 256 end, 0, List) + 1.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
from_completion_code(16#00) -> command_completed_normally;
from_completion_code(16#c0) -> node_busy;
from_completion_code(16#c1) -> invalid_command;
from_completion_code(16#c2) -> invalid_command_for_lun;
from_completion_code(16#c3) -> timeout;
from_completion_code(16#c4) -> out_of_space;
from_completion_code(16#c5) -> reservation_canceled;
from_completion_code(16#c6) -> data_truncated;
from_completion_code(16#c7) -> data_length_invalid;
from_completion_code(16#c8) -> data_length_limit_exceeded;
from_completion_code(16#c9) -> parameter_out_of_range;
from_completion_code(16#ca) -> cannot_return_number_of_requested_data_bytes;
from_completion_code(16#cb) -> requested_sensor_not_present;
from_completion_code(16#cc) -> invalid_data_field;
from_completion_code(16#cd) -> command_illegal_for_sensor;
from_completion_code(16#ce) -> response_not_provided;
from_completion_code(16#cf) -> duplicated_request;
from_completion_code(16#d0) -> sdr_repository_in_update_mode;
from_completion_code(16#d1) -> device_in_firmware_update_mode;
from_completion_code(16#d2) -> bmc_initialization_in_progress;
from_completion_code(16#d3) -> destination_unavailable;
from_completion_code(16#d4) -> insufficient_privilege_level;
from_completion_code(16#d5) -> command_not_supported;
from_completion_code(16#d6) -> command_disabled;
from_completion_code(_) -> unspecified_error.
