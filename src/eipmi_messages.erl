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
%%% A module providing encoding/decoding functionality for RMCP/IPMI packets.
%%% @end
%%%=============================================================================

-module(eipmi_messages).

-export([encode/1,
         decode/1]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encodes an erlang IPMI/RMCP structure into a binary packet.
%% @end
%%------------------------------------------------------------------------------
-spec encode(#rmcp_ack{} | #rmcp_ping{} | #rmcp_pong{} | #rmcp_ipmi{}) ->
                    {ok, binary()} | {error, term()}.
encode(Ack = #rmcp_ack{}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ack#rmcp_ack.seq_nr):8,
           ?RMCP_ACK:1,?EIPMI_RESERVED:2,(Ack#rmcp_ack.class):5>>};

encode(Ping = #rmcp_ping{}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ping#rmcp_ping.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_ASF:5,
           ?ASF_IANA:32,
           ?ASF_PING:8,
           (Ping#rmcp_ping.asf_tag):8,
           ?EIPMI_RESERVED:8,
           0:8>>};

encode(Pong = #rmcp_pong{}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Pong#rmcp_pong.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_ASF:5,
           ?ASF_IANA:32,
           ?ASF_PONG:8,
           (Pong#rmcp_pong.asf_tag):8,
           ?EIPMI_RESERVED:8,
           16:8,
           (Pong#rmcp_pong.iana):32,
           (Pong#rmcp_pong.oem):32,
           (encode_supported_entities(Pong#rmcp_pong.entities)):8,
           0:8, %% no interaction supported
           0:48>>};

encode(Ipmi = #rmcp_ipmi{auth_type = none, payload = Payload}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ipmi#rmcp_ipmi.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_IPMI:5,
           ?EIPMI_RESERVED:4,(encode_auth_type(none)):4,
           (Ipmi#rmcp_ipmi.session_seq_nr):32,
           (Ipmi#rmcp_ipmi.session_id):32,
           (size(Payload)):8,
           Payload/binary>>};

encode(Ipmi = #rmcp_ipmi{auth_type = Type, payload = Payload}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ipmi#rmcp_ipmi.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_IPMI:5,
           ?EIPMI_RESERVED:4,(encode_auth_type(Type)):4,
           (Ipmi#rmcp_ipmi.session_seq_nr):32,
           (Ipmi#rmcp_ipmi.session_id):32,
           (Ipmi#rmcp_ipmi.auth_code):128,
           (size(Payload)):8,
           Payload/binary>>};

encode(Message) ->
    {error, {unsupported_message, Message}}.

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
encode_ipmi_lan(NetFn, Lun, RqAddr, RqSeqNr, Cmd, Data) ->
    Header = <<16#20:8,NetFn:6,Lun:2>>,
    Checksum1 = ipmi_checksum(Header),
    Body = <<RqAddr:8,RqSeqNr:6,0:2,Cmd:8,Data/binary>>,
    Checksum2 = ipmi_checksum(Body),
    <<Header/binary, Checksum1:8/signed,Body/binary,Checksum2:8/signed>>.

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a binary representing an IPMI/RMCP packet into the corresponding
%% erlang structure.
%% @end
%%------------------------------------------------------------------------------
-spec decode(binary()) ->
                    {ok, #rmcp_pong{} | #rmcp_ping{} | #rmcp_ack{} | #rmcp_ipmi{}} |
                    {error, term()}.
decode(<<?RMCP_VERSION:8,?EIPMI_RESERVED:8,SeqNr:8,Rest/binary>>) ->
    decode_class(SeqNr, Rest);

decode(_) ->
    {error, unsupported_rmcp_message}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_class(SeqNr, <<?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_ASF:5,Rest/binary>>) ->
    decode_asf(SeqNr, Rest);

decode_class(SeqNr, <<?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_IPMI:5,Rest/binary>>) ->
    decode_ipmi(SeqNr, Rest);

decode_class(SeqNr, <<?RMCP_ACK:1,?EIPMI_RESERVED:2,Class:5>>) ->
    {ok, #rmcp_ack{seq_nr = SeqNr, class = Class}};

decode_class(_SeqNr, _Binary) ->
    {error, unsupported_rmcp_class}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_asf(SeqNr, <<?ASF_IANA:32,Type:8,Tag:8,?EIPMI_RESERVED:8,Rest/binary>>) ->
    case Type of
        ?ASF_PONG ->
            decode_asf_packet(#rmcp_pong{seq_nr = SeqNr, asf_tag = Tag}, Rest);

        ?ASF_PING ->
            decode_asf_packet(#rmcp_ping{seq_nr = SeqNr, asf_tag = Tag}, Rest);

        _ ->
            {error, unsupported_asf_message}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_asf_packet(Ping = #rmcp_ping{}, _Binary) ->
    {ok, Ping};

decode_asf_packet(Pong = #rmcp_pong{}, <<16:8,IANA:32,OEM:32,1:1,1:7,_:8,_:48>>) ->
    {ok, Pong#rmcp_pong{iana = IANA, oem = OEM, entities = [ipmi]}};

decode_asf_packet(Pong = #rmcp_pong{}, <<16:8,IANA:32,OEM:32,_:8,_:8,_:48>>) ->
    {ok, Pong#rmcp_pong{iana = IANA, oem = OEM, entities = []}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,0:4,SSeqNr:32,SId:32,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = none,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_payload(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,1:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = md2,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_payload(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,2:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = md5,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_payload(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,4:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = pwd,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_payload(Ipmi, Rest);

decode_ipmi(_SeqNr, _Binary) ->
    {error, unsupported_ipmi_message}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_ipmi_payload(Ipmi, <<Len:8,Payload:Len/binary>>) ->
    {ok, Ipmi#rmcp_ipmi{payload = Payload}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_supported_entities(Entities) ->
    lists:foldl(
      fun(ipmi, Acc) ->
              Acc bor 2#10000000
      end,
      2#00000001,
      Entities).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_auth_type(none) ->
    0;

encode_auth_type(md2) ->
    1;

encode_auth_type(md5) ->
    2;

encode_auth_type(pwd) ->
    4.

%%------------------------------------------------------------------------------
%% @private
%% two's complement of the 8-bit checksum of the input binary
%%------------------------------------------------------------------------------
ipmi_checksum(Binary) ->
    List = binary_to_list(Binary),
    bnot lists:foldl(fun(Byte, Acc) -> (Acc + Byte) rem 256 end, 0, List) + 1.
