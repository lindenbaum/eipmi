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
           (encode_supported_interactions(Pong#rmcp_pong.interactions)):8,
           0:48>>};

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
                                Interactions:8,
                                _:48>>) ->
    {ok, #rmcp_pong{
       seq_nr = SeqNr,
       asf_tag = ASFTag,
       iana = IANAEnterprise,
       oem = OEMDefined,
       entities = decode_supported_entities(<<Entities:8>>),
       interactions = decode_supported_interactions(<<Interactions:8>>)}};

rmcp_decode(_SeqNr, Type, _Binary) ->
    {error, {unsupported_asf_message, Type}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ipmi_decode(_SeqNr, _Binary) ->
    {error, not_yet_implemented}.


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
encode_supported_interactions(Interactions) ->
    lists:foldl(
      fun(rmcp_security_extensions, Acc) ->
              Acc bor 2#10000000;

         (dtmf_dash, Acc) ->
              Acc bor 2#00100000
      end,
      2#00000000,
      Interactions).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_supported_interactions(<<Sec:1,0:1,Dtmf:1,_:5>>) ->
    case {Sec, Dtmf} of
        {1, 1} ->
            [rmcp_security_extensions, dtmf_dash];

        {1, 0} ->
            [rmcp_security_extensions];

        {0, 1} ->
            [dtmf_dash];

        {0, 0} ->
            []
    end.
