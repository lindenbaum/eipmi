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
         ipmi/4,
         request/3]).

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
-spec ack(#rmcp_header{}) -> binary().
ack(Header = #rmcp_header{class = ?RMCP_ASF}) ->
    header(Header, ?RMCP_ACK).

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a RMCP ASF Ping packet. A Ping is usually sent to retrieve the IPMI
%% capabilities of the far end.
%% @end
%%------------------------------------------------------------------------------
-spec ping(#rmcp_header{}, #asf_ping{}) -> binary().
ping(Header = #rmcp_header{class = ?RMCP_ASF}, #asf_ping{iana = I, tag = T}) ->
    HeaderBin = header(Header, ?RMCP_NORMAL),
    PingBin = <<I:32, ?ASF_PING:8, T:8, 0:8, 0:8>>,
    <<HeaderBin/binary, PingBin/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a RMCP IPMI packet. This encapsulates the logic of binary packet
%% construction with authentication and checksum calculation. Please note that
%% currently only IPMI requests are supported.
%% @end
%%------------------------------------------------------------------------------
-spec ipmi(#rmcp_header{}, proplists:proplist(), eipmi:request(), binary()) ->
                  binary().
ipmi(Header = #rmcp_header{class = ?RMCP_IPMI}, Properties, Req, Data) ->
    HeaderBin = header(Header, ?RMCP_NORMAL),
    case proplists:get_value(auth_type, Properties) of
        rmcp_plus ->
            E = proplists:get_value(encrypt_type, Properties),
            H = proplists:get_value(integrity_type, Properties),
            SIK = proplists:get_value(session_key, Properties),
            K2 = eipmi_auth:extra_key(E, H, SIK),

            SessionBin = session2(Properties),
            RequestBin = request(Properties, Req, Data),
            Encrypted = eipmi_auth:encrypt(E, K2, RequestBin),
            Length = size(Encrypted),
            Unpadded = <<SessionBin/binary, Length:16/little, Encrypted/binary>>,
            % AuthCode needs payload to be a multiple of 4 bytes, but we add 2
            % bytes for the padding length itself and the NextHeader field.
            % PadLength = (6 - size(ToHash) rem 4) rem 4,
            PadLength = case size(Unpadded) rem 4 of
                            0 -> 2;
                            1 -> 1;
                            2 -> 0;
                            3 -> 3
                        end,
            Padding = binary:copy(<<255>>, PadLength),
            NextHeader = 7,
            K1 = eipmi_auth:extra_key(1, H, SIK),
            ToHash = <<Unpadded/binary, Padding/binary, PadLength:8, NextHeader:8>>,
            AuthCode = eipmi_auth:hash(H, K1, ToHash),
            <<HeaderBin/binary, ToHash/binary, AuthCode/binary>>;
        AuthType ->
            RequestBin = request(Properties, Req, Data),
            S = proplists:get_value(inbound_seq_nr, Properties),
            I = proplists:get_value(session_id, Properties),
            P = proplists:get_value(password, Properties),
            SessionBin = session1(AuthType, S, I, P, RequestBin),
            Length = size(RequestBin),
            <<HeaderBin/binary, SessionBin/binary, Length:8, RequestBin/binary>>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a raw IPMB request according the standard format:
%%   `rsSA, netFn/rsLUN, chk1, rqSA, rqSeq/rqLUN, cmd, <data>, chk2'
%% Calling this directly is useful for e.g. bridged requests. Refer to chapter
%% 6.13, BMC Message Bridging in the IPMI specification.
%% @end
%%------------------------------------------------------------------------------
-spec request(proplists:proplist(), {eipmi:req_net_fn(), 0..255}, binary()) ->
                     binary().
request(Properties, {NetFn, Cmd}, Data) ->
    Head = request_head(NetFn, Properties),
    HeadSum = checksum(Head),
    Tail = request_tail(Properties, Cmd, Data),
    TailSum = checksum(Tail),
    <<Head/binary, HeadSum:8/signed, Tail/binary, TailSum:8/signed>>.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
header(#rmcp_header{version = V, seq_nr = S, class = C}, Ack) ->
    <<V:8, 0:8, S:8, Ack:1, 0:2, C:5>>.

%%------------------------------------------------------------------------------
%% @private
%% This will also do the authentication according to the multi session
%% authentication.
%%------------------------------------------------------------------------------
% v1.5
session1(none, S, I, _P, _Data) ->
    Type = eipmi_auth:encode_type(none),
    <<0:4, Type:4, S:32/little, I:32/little>>;
session1(pwd, S, I, P, _Data) ->
    Type = eipmi_auth:encode_type(pwd),
    C = eipmi_auth:hash(pwd, P),
    <<0:4, Type:4, S:32/little, I:32/little, C/binary>>;
session1(T, S, I, P, Data) ->
    Type = eipmi_auth:encode_type(T),
    C = eipmi_util:normalize(16, P),
    ToHash = <<C/binary, I:32/little, Data/binary, S:32/little, C/binary>>,
    Ci = eipmi_auth:hash(T, ToHash),
    <<0:4, Type:4, S:32/little, I:32/little, Ci/binary>>.

% v2.0
session2(Properties) ->
    I = proplists:get_value(session_id, Properties, <<0:32>>),
    E = proplists:get_value(encrypt_type, Properties, none),
    P = proplists:get_value(payload_type, Properties),
    AuthType = eipmi_auth:encode_type(rmcp_plus),
    Pt = eipmi_auth:encode_payload_type(P),
    {Authenticated, Seq} = case I of
                        <<0:32>> -> {0, inbound_unauth_seq_nr};
                        _ -> {1, inbound_auth_seq_nr}
                    end,
    S = proplists:get_value(Seq, Properties),
    % Even if we have determined what encryption algorithm to use in the middle
    % of the RAKP process, we still don't encrypt the messages until the
    % session is fully activated.
    Encrypted = case {E, P} of
                    {none, ipmi} -> 0;
                    {_, ipmi} -> 1;
                    _ -> 0
                end,
    <<AuthType:8, Encrypted:1, Authenticated:1, Pt:6, I:32/little, S:32/little>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_head(NetFn, Properties) ->
    A = proplists:get_value(rs_addr, Properties),
    L = proplists:get_value(rs_lun, Properties),
    <<A:8, NetFn:6, L:2>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_tail(Properties, Cmd, Data) ->
    A = proplists:get_value(rq_addr, Properties),
    S = proplists:get_value(rq_seq_nr, Properties),
    L = proplists:get_value(rq_lun, Properties),
    <<A:8, S:6, L:2, Cmd:8, Data/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Calculates the two's complement of the 8-bit checksum of the input binary.
%% Use <code>(checksum(Binary)):8/signed</code> to insert into a binary.
%% @end
%%------------------------------------------------------------------------------
checksum(Binary) ->
    bnot sum(Binary, 0) + 1.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sum(<<>>, Sum) ->
    Sum;
sum(<<Byte:8, Rest/binary>>, Sum) ->
    sum(Rest, (Sum + Byte) rem 256).
