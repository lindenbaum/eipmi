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

-include("eipmi_internal.hrl").

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
%% Encodes a RMCP IPMI packet. This encapsulates the logic of binary packet
%% construction with authentication and checksum calculation.
%% @end
%%------------------------------------------------------------------------------
-spec ipmi(#rmcp_header{}, #ipmi_session{}, #ipmi_request{}, 0..255, binary()) ->
                  binary().
ipmi(Header = #rmcp_header{class = ?RMCP_IPMI}, Session, Request, Cmd, Data) ->
    HeaderBin = header(Header, ?RMCP_NORMAL),
    SessionBin = session(Session, Data),
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
%% This will also do the authentication according to the multi session
%% authentication.
%%------------------------------------------------------------------------------
session(#ipmi_session{type = none, seq_nr = S, id = I}, _Data) ->
    Type = eipmi_auth:encode_type(none),
    <<?EIPMI_RESERVED:4, Type:4, S:32, I:32>>;
session(#ipmi_session{type = pwd, key = K, seq_nr = S, id = I}, _Data) ->
    Type = eipmi_auth:encode_type(pwd),
    C = eipmi_auth:encrypt(pwd, K),
    <<?EIPMI_RESERVED:4, Type:4, S:32, I:32, C/binary>>;
session(#ipmi_session{type = T, key = K, seq_nr = S, id = I}, Data) ->
    Type = eipmi_auth:encode_type(T),
    C = eipmi_util:normalize(16, K),
    Ci = eipmi_auth:encrypt(T, <<C/binary, I:32, Data/binary, S:32, C/binary>>),
    <<?EIPMI_RESERVED:4, Type:4, S:32, I:32, Ci/binary>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request(Request, Cmd, Data) ->
    Head = request_head(Request),
    HeadSum = checksum(Head),
    Tail = request_tail(Request, Cmd, Data),
    TailSum = checksum(Tail),
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
