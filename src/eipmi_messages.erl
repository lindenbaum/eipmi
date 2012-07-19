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

-module(eipmi_messages).

-export([encode/1,
         decode/1]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec encode(#rmcp_ack{} | #rmcp_ping{} | #rmcp_pong{}) ->
                    {ok, binary()} | {error, term()}.
encode(#rmcp_ack{seq_nr = SeqNr, class = Class}) ->
    {ok, <<?RMCP_VERSION:8,
           ?RMCP_RESERVED:8,
           SeqNr:8,
           ?RMCP_CLASS_ACK:1,?RMCP_CLASS_RESERVED:2,Class:5>>};

encode(#rmcp_ping{seq_nr = SeqNr, asf_tag = ASFTag}) ->
    {ok, <<?RMCP_VERSION:8,
           ?RMCP_RESERVED:8,
           SeqNr:8,
           ?RMCP_CLASS_NORMAL:1,?RMCP_CLASS_RESERVED:2,?RMCP_CLASS_ASF:5,
           ?ASF_IANA:32,
           ?ASF_PING:8,
           ASFTag:8,
           ?ASF_RESERVED:8,
           0:8>>};

encode(Pong = #rmcp_pong{seq_nr = SeqNr, asf_tag = ASFTag}) ->
    {ok, <<?RMCP_VERSION:8,
           ?RMCP_RESERVED:8,
           SeqNr:8,
           ?RMCP_CLASS_NORMAL:1,?RMCP_CLASS_RESERVED:2,?RMCP_CLASS_ASF:5,
           ?ASF_IANA:32,
           ?ASF_PONG:8,
           ASFTag:8,
           ?ASF_RESERVED:8,
           16:8,
           (Pong#rmcp_pong.iana):32,
           (Pong#rmcp_pong.oem):32,
           (encode_supported_entities(Pong#rmcp_pong.entities)):8,
           0:8, %% no interaction supported
           0:48>>};

encode(Ipmi = #rmcp_ipmi{seq_nr = SeqNr, auth_type = none, payload = Payload}) ->
    {ok, <<?RMCP_VERSION:8,
           ?RMCP_RESERVED:8,
           SeqNr:8,
           ?RMCP_CLASS_NORMAL:1,?RMCP_CLASS_RESERVED:2,?RMCP_CLASS_IPMI:5,
           ?IPMI_AUTH_RESERVED:4,(encode_auth_type(none)):4,
           (Ipmi#rmcp_ipmi.session_seq_nr):32,
           (Ipmi#rmcp_ipmi.session_id):32,
           (size(Payload)):8,
           Payload/binary>>};

encode(Ipmi = #rmcp_ipmi{seq_nr = SeqNr, auth_type = Type, payload = Payload}) ->
    {ok, <<?RMCP_VERSION:8,
           ?RMCP_RESERVED:8,
           SeqNr:8,
           ?RMCP_CLASS_NORMAL:1,?RMCP_CLASS_RESERVED:2,?RMCP_CLASS_IPMI:5,
           ?IPMI_AUTH_RESERVED:4,(encode_auth_type(Type)):4,
           (Ipmi#rmcp_ipmi.session_seq_nr):32,
           (Ipmi#rmcp_ipmi.session_id):32,
           (Ipmi#rmcp_ipmi.auth_code):128,
           (size(Payload)):8,
           Payload/binary>>};

encode(Message) ->
    {error, {unsupported_message, Message}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec decode(binary()) ->
                    {ok, #rmcp_pong{} | #rmcp_pong{} | #rmcp_ack{}} |
                    {error, term()}.
decode(<<?RMCP_VERSION:8,?RMCP_RESERVED:8,SeqNr:8,Rest/binary>>) ->
    decode(SeqNr, Rest);

decode(_) ->
    {error, no_rmcp_message}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode(SeqNr, <<?RMCP_CLASS_NORMAL:1,
                ?RMCP_CLASS_RESERVED:2,
                ?RMCP_CLASS_ASF:5,
                ?ASF_IANA:32,
                MessageType:8,
                Rest/binary>>) ->
    rmcp_decode(SeqNr, MessageType, Rest);

decode(SeqNr, <<?RMCP_CLASS_NORMAL:1,
                ?RMCP_CLASS_RESERVED:2,
                ?RMCP_CLASS_IPMI:5,
                Rest/binary>>) ->
    ipmi_decode(SeqNr, Rest);

decode(SeqNr, <<?RMCP_CLASS_ACK:1,?RMCP_CLASS_RESERVED:2,Class:5>>) ->
    {ok, #rmcp_ack{seq_nr = SeqNr, class = Class}};

decode(_SeqNr, _Binary) ->
    {error, no_asf_message}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rmcp_decode(SeqNr, ?ASF_PING, <<ASFTag:8,?ASF_RESERVED:8,0:8>>) ->
    {ok, #rmcp_ping{seq_nr = SeqNr, asf_tag = ASFTag}};

rmcp_decode(SeqNr, ?ASF_PONG, <<ASFTag:8,
                                ?ASF_RESERVED:8,16:8,
                                IANAEnterprise:32,
                                OEMDefined:32,
                                Entities:8,
                                _:8,
                                _:48>>) ->
    {ok, #rmcp_pong{
       seq_nr = SeqNr,
       asf_tag = ASFTag,
       iana = IANAEnterprise,
       oem = OEMDefined,
       entities = decode_supported_entities(<<Entities:8>>)}};

rmcp_decode(_SeqNr, Type, _Binary) ->
    {error, {unsupported_asf_message, Type}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ipmi_decode(SeqNr, <<?IPMI_AUTH_RESERVED:4,0:4,
                     SessionSeqNr:32,
                     SessionId:32,
                     Length:8,
                     Payload:Length/binary>>) ->
    {ok, #rmcp_ipmi{
       seq_nr = SeqNr,
       session_id = SessionId,
       session_seq_nr = SessionSeqNr,
       payload = Payload}};

ipmi_decode(SeqNr, <<?IPMI_AUTH_RESERVED:4,AuthType:4,
                     SessionSeqNr:32,
                     SessionId:32,
                     AuthCode:128,
                     Length:8,
                     Payload:Length/binary>>) ->
    {ok, #rmcp_ipmi{
       seq_nr = SeqNr,
       auth_type = decode_auth_type(AuthType),
       auth_code = AuthCode,
       session_id = SessionId,
       session_seq_nr = SessionSeqNr,
       payload = Payload}};

ipmi_decode(_SeqNr, _Binary) ->
    {error, unsupported_ipmi_message}.

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
decode_supported_entities(<<1:1,1:7>>) ->
    [ipmi];

decode_supported_entities(_) ->
    [].

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
decode_auth_type(0) ->
    none;

decode_auth_type(1) ->
    md2;

decode_auth_type(2) ->
    md5;

decode_auth_type(4) ->
    pwd.
