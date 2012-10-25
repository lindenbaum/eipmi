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
%% Decodes a binary representing a RMCP message in its erlang record
%% representation.
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
    {SessionProps, <<Size:8, Rest:Size/binary>>} = session(Binary),
    Len = (Size - 4),
    <<Head:2/binary, Sum1:8/signed, Tail:Len/binary, Sum2:8/signed>> = Rest,
    case has_integrity(Head, Sum1) andalso has_integrity(Tail, Sum2) of
        true ->
            lan(Ipmi#rmcp_ipmi{properties = SessionProps}, Head, Tail);
        false ->
            {error, corrupted_ipmi_message}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
session(<<?EIPMI_RESERVED:4, 0:4, S:32/little, I:32/little, Rest/binary>>) ->
    {[{auth_type, none}, {outbound_seq_nr, S}, {session_id, I}], Rest};
session(<<?EIPMI_RESERVED:4, 1:4, S:32/little, I:32/little, _:128, Rest/binary>>) ->
    {[{auth_type, md2}, {outbound_seq_nr, S}, {session_id, I}], Rest};
session(<<?EIPMI_RESERVED:4, 2:4, S:32/little, I:32/little, _:128, Rest/binary>>) ->
    {[{auth_type, md5}, {outbound_seq_nr, S}, {session_id, I}], Rest};
session(<<?EIPMI_RESERVED:4, 3:4, S:32/little, I:32/little, _:128, Rest/binary>>) ->
    {[{auth_type, pwd}, {outbound_seq_nr, S}, {session_id, I}], Rest}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
lan(Ipmi = #rmcp_ipmi{properties = Ps},
    <<RqAddr:8, NetFn:6, RqLun:2>>,
    <<RsAddr:8, RqSeqNr:6, RsLun:2, Cmd:8, Code:8, Data/binary>>) ->
    {ok, Ipmi#rmcp_ipmi{
           properties =
               Ps ++ [{rq_addr, RqAddr},
                      {rq_lun, RqLun},
                      {rq_seq_nr, RqSeqNr},
                      {rs_addr, RsAddr},
                      {rs_lun, RsLun},
                      {completion, completion_code(Code)}],
           cmd = {NetFn, Cmd},
           data = Data}};
lan(_Ipmi, _Head, _Tail) ->
    {error, unsupported_ipmi_message}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
has_integrity(Binary, Checksum) ->
    (Checksum + sum(Binary, 0)) rem 256 =:= 0.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sum(<<>>, Sum) ->
    Sum;
sum(<<Byte:8, Rest/binary>>, Sum) ->
    sum(Rest, Sum + Byte).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
completion_code(16#00) -> normal;
completion_code(16#c0) -> node_busy;
completion_code(16#c1) -> invalid_command;
completion_code(16#c2) -> invalid_command_for_lun;
completion_code(16#c3) -> timeout;
completion_code(16#c4) -> out_of_space;
completion_code(16#c5) -> reservation_canceled;
completion_code(16#c6) -> data_truncated;
completion_code(16#c7) -> data_length_invalid;
completion_code(16#c8) -> data_length_limit_exceeded;
completion_code(16#c9) -> parameter_out_of_range;
completion_code(16#ca) -> cannot_return_number_of_requested_data_bytes;
completion_code(16#cb) -> requested_sensor_not_present;
completion_code(16#cc) -> invalid_data_field;
completion_code(16#cd) -> command_illegal_for_sensor;
completion_code(16#ce) -> response_not_provided;
completion_code(16#cf) -> duplicated_request;
completion_code(16#d0) -> sdr_repository_in_update_mode;
completion_code(16#d1) -> device_in_firmware_update_mode;
completion_code(16#d2) -> bmc_initialization_in_progress;
completion_code(16#d3) -> destination_unavailable;
completion_code(16#d4) -> insufficient_privilege_level;
completion_code(16#d5) -> command_not_supported;
completion_code(16#d6) -> command_disabled;
completion_code(_) -> unspecified_error.
