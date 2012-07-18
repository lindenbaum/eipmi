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

-ifndef(eipmi_hrl_).
-define(eipmi_hrl_, 1).

%%%=============================================================================
%%% Defines
%%%=============================================================================

%%------------------------------------------------------------------------------
%% The currently supported RMCP version (length is 8bits).
%%------------------------------------------------------------------------------
-define(RMCP_VERSION, 16#06).

%%------------------------------------------------------------------------------
%% The RMCP reserved value (second byte of the RMCP message format).
%%------------------------------------------------------------------------------
-define(RMCP_RESERVED, 0).

%%------------------------------------------------------------------------------
%% The RMCP sequence number indicating that this message does not request a
%% reply/ack.
%%------------------------------------------------------------------------------
-define(RMCP_NOREPLY, 255).

%%------------------------------------------------------------------------------
%% The RMCP normal message type (length is 1bit).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_NORMAL, 0).

%%------------------------------------------------------------------------------
%% The RMCP ACK message type (length is 1bit).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_ACK, 1).

%%------------------------------------------------------------------------------
%% The RMCP class reserved value (length is 2bits).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_RESERVED, 0).

%%------------------------------------------------------------------------------
%% The RMCP ASF class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_ASF, 16#06).

%%------------------------------------------------------------------------------
%% The RMCP IPMI class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_IPMI, 16#07).

%%------------------------------------------------------------------------------
%% The RMCP OEM class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_CLASS_OEM, 16#08).

%%------------------------------------------------------------------------------
%% The ASF IANA prefix (length is 16bits, the enterprise numer is allowed to
%% take another 16bits).
%%------------------------------------------------------------------------------
-define(ASF_IANA, 4542).

%%------------------------------------------------------------------------------
%% The ASF message type for PONG messages (length is 8bits).
%%------------------------------------------------------------------------------
-define(ASF_PONG, 16#40).

%%------------------------------------------------------------------------------
%% The ASF message type for PING messages (length is 8bits).
%%------------------------------------------------------------------------------
-define(ASF_PING, 16#80).

%%------------------------------------------------------------------------------
%% The ASF message tag indicating that this message is not part of a
%% request/response pair.
%%------------------------------------------------------------------------------
-define(ASF_NOREPLY, 255).

%%------------------------------------------------------------------------------
%% The ASF reserved byte (length is obviously 8bits).
%%------------------------------------------------------------------------------
-define(ASF_RESERVED, 0).

%%------------------------------------------------------------------------------
%% The IPMI capability of the ASF supported entities field (length is 1bit).
%%------------------------------------------------------------------------------
-define(ASF_IPMI_SUPPORTED, 1).

%%------------------------------------------------------------------------------
%% The currently supported ASF version (1.0, length is 1bit).
%%------------------------------------------------------------------------------
-define(ASF_VERSION_1_0, 1).

%%%=============================================================================
%%% Messages
%%%=============================================================================

%%------------------------------------------------------------------------------
%% The RMCP ASF ACK Message.
%%------------------------------------------------------------------------------
-record(rmcp_ack, {
          seq_nr = 255             :: 0..255,
          class  = ?RMCP_CLASS_ASF :: non_neg_integer()}).

%%------------------------------------------------------------------------------
%% The RMCP ASF PING Message.
%%------------------------------------------------------------------------------
-record(rmcp_ping, {
         seq_nr = 255  :: 0..255,
         asf_tag = 255 :: 0..255}).

%%------------------------------------------------------------------------------
%% The RMCP ASF PONG Message.
%%------------------------------------------------------------------------------
-record(rmcp_pong, {
          seq_nr = 255     :: 0..255,
          asf_tag = 255    :: 0..255,
          iana = 4542      :: non_neg_integer(), %% the reported IANA enterprise number
          oem = 0          :: non_neg_integer(), %% OEM defined values
          entities = 16#81 :: 0..255,            %% supported entities
          interactions = 0 :: 0..255}).          %% supported interactions

-endif. %% eipmi_hrl_
