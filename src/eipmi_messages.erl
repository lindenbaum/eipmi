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

-export([encode_packet/1,
         decode_packet/1]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encodes an erlang IPMI/RMCP structure into a binary packet.
%% @end
%%------------------------------------------------------------------------------
-spec encode_packet(#rmcp_ack{} | #rmcp_ping{} | #rmcp_pong{} | #rmcp_ipmi{}) ->
                           {ok, binary()} | {error, term()}.
encode_packet(Ack = #rmcp_ack{}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ack#rmcp_ack.seq_nr):8,
           ?RMCP_ACK:1,?EIPMI_RESERVED:2,(Ack#rmcp_ack.class):5>>};

encode_packet(Ping = #rmcp_ping{}) ->
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ping#rmcp_ping.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_ASF:5,
           ?ASF_IANA:32,
           ?ASF_PING:8,
           (Ping#rmcp_ping.asf_tag):8,
           ?EIPMI_RESERVED:8,
           0:8>>};

encode_packet(Ipmi = #rmcp_ipmi{auth_type = none}) ->
    Payload = encode_ipmi_lan(Ipmi#rmcp_ipmi.requestor_addr,
                              Ipmi#rmcp_ipmi.requestor_seq_nr,
                              Ipmi#rmcp_ipmi.data),
    {ok, <<?RMCP_VERSION:8,
           ?EIPMI_RESERVED:8,
           (Ipmi#rmcp_ipmi.seq_nr):8,
           ?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_IPMI:5,
           ?EIPMI_RESERVED:4,(encode_auth_type(none)):4,
           (Ipmi#rmcp_ipmi.session_seq_nr):32,
           (Ipmi#rmcp_ipmi.session_id):32,
           (size(Payload)):8,
           Payload/binary>>};

encode_packet(Ipmi = #rmcp_ipmi{auth_type = Type}) ->
    Payload = encode_ipmi_lan(Ipmi#rmcp_ipmi.requestor_addr,
                              Ipmi#rmcp_ipmi.requestor_seq_nr,
                              Ipmi#rmcp_ipmi.data),
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

encode_packet(Message) ->
    {error, {unsupported_message, Message}}.

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a binary representing an IPMI/RMCP packet into the corresponding
%% erlang structure.
%% @end
%%------------------------------------------------------------------------------
-spec decode_packet(binary()) ->
                           {ok, #rmcp_pong{} | #rmcp_ping{} | #rmcp_ack{} | #rmcp_ipmi{}} |
                           {error, term()}.
decode_packet(<<?RMCP_VERSION:8,?EIPMI_RESERVED:8,SeqNr:8,Rest/binary>>) ->
    decode_rmcp(SeqNr, Rest);

decode_packet(_) ->
    {error, unsupported_message}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_rmcp(SeqNr, <<?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_ASF:5,Rest/binary>>) ->
    decode_asf(SeqNr, Rest);

decode_rmcp(SeqNr, <<?RMCP_NORMAL:1,?EIPMI_RESERVED:2,?RMCP_IPMI:5,Rest/binary>>) ->
    decode_ipmi(SeqNr, Rest);

decode_rmcp(SeqNr, <<?RMCP_ACK:1,?EIPMI_RESERVED:2,Class:5>>) ->
    {ok, #rmcp_ack{seq_nr = SeqNr, class = Class}};

decode_rmcp(_SeqNr, _Binary) ->
    {error, unsupported_rmcp_class}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_asf(SeqNr, <<?ASF_IANA:32,Type:8,Tag:8,?EIPMI_RESERVED:8,Rest/binary>>) ->
    case Type of
        ?ASF_PONG ->
            Pong = #rmcp_pong{seq_nr = SeqNr, asf_tag = Tag},
            case Rest of
                <<16:8,IANA:32,OEM:32,1:1,1:7,_:8,_:48>> ->
                    {ok, Pong#rmcp_pong{iana = IANA, oem = OEM, entities = [ipmi]}};

                <<16:8,IANA:32,OEM:32,_:8,_:8,_:48>> ->
                    {ok, Pong#rmcp_pong{iana = IANA, oem = OEM, entities = []}};

                _ ->
                    {error, unsupported_asf_message}
            end;

        ?ASF_PING ->
            {ok, #rmcp_ping{seq_nr = SeqNr, asf_tag = Tag}};

        _ ->
            {error, unsupported_asf_message}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,0:4,SSeqNr:32,SId:32,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = none,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_lan(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,1:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = md2,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_lan(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,2:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = md5,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_lan(Ipmi, Rest);

decode_ipmi(SeqNr, <<?EIPMI_RESERVED:4,4:4,SSeqNr:32,SId:32,Code:128,Rest/binary>>) ->
    Ipmi = #rmcp_ipmi{
      seq_nr = SeqNr,
      auth_type = pwd,
      auth_code = Code,
      session_id = SId,
      session_seq_nr = SSeqNr},
    decode_ipmi_lan(Ipmi, Rest);

decode_ipmi(_SeqNr, _Binary) ->
    {error, unsupported_ipmi_message}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_ipmi_lan(RqAddr, RqSeqNr, Data) ->
    NetFn = ?IPMI_NETFN_APPLICATION_REQUEST,
    Header = <<?IPMI_RESPONDER_ADDR:8,NetFn:6,?IPMI_RESPONDER_LUN:2>>,
    Checksum1 = calc_checksum(Header),
    {Cmd, DataBin} = encode_ipmi_data(Data),
    Body = <<RqAddr:8,RqSeqNr:6,?IPMI_REQUESTOR_LUN:2,Cmd:8,DataBin/binary>>,
    Checksum2 = calc_checksum(Body),
    <<Header/binary, Checksum1:8/signed,Body/binary,Checksum2:8/signed>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_ipmi_data(Req = #ipmi_get_channel_authentication_capabilities{}) ->
    P = Req#ipmi_get_channel_authentication_capabilities.privilege,
    L = encode_privilege_level(P),
    {?IPMI_GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
     <<0:1,?EIPMI_RESERVED:3,?IPMI_REQUESTED_CHANNEL:4,?EIPMI_RESERVED:4,L:4>>}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_ipmi_lan(Ipmi, <<_Len:8,Header:24/binary,Body/binary>>) ->
    case eval_checksum(Header) andalso eval_checksum(Body) of
        true ->
            {ok, Ipmi#rmcp_ipmi{}};

        false ->
            {error, checksum_error}
    end.

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
%%------------------------------------------------------------------------------
encode_privilege_level(callback) ->
    1;
encode_privilege_level(user) ->
    2;
encode_privilege_level(operator) ->
    3;
encode_privilege_level(administrator) ->
    4.

%%------------------------------------------------------------------------------
%% @private
%% two's complement of the 8-bit checksum of the input binary
%%------------------------------------------------------------------------------
calc_checksum(Binary) ->
    List = binary_to_list(Binary),
    bnot lists:foldl(fun(Byte, Acc) -> (Acc + Byte) rem 256 end, 0, List) + 1.

%%------------------------------------------------------------------------------
%% @private
%% checks the integrity of a checksummed binary, first byte of the binary
%% contains the checksum (same algorithm than calc_checksum/1).
%%------------------------------------------------------------------------------
eval_checksum(Binary) ->
    MsgSize = size(Binary) - 1,
    <<Message:MsgSize/binary,Checksum:8/signed>> = Binary,
    Checksum == calc_checksum(Message).
