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
%% A zero value indicating reserved fields in protocol messages.
%%------------------------------------------------------------------------------
-define(EIPMI_RESERVED, 0).

%%------------------------------------------------------------------------------
%% The default RMCP port.
%%------------------------------------------------------------------------------
-define(RMCP_PORT_NUMBER, 623).

%%------------------------------------------------------------------------------
%% The currently supported RMCP version (length is 8bits).
%%------------------------------------------------------------------------------
-define(RMCP_VERSION, 16#06).

%%------------------------------------------------------------------------------
%% The RMCP sequence number indicating that this message does not request a
%% reply/ack.
%%------------------------------------------------------------------------------
-define(RMCP_NOREPLY, 255).

%%------------------------------------------------------------------------------
%% The RMCP normal message type (length is 1bit).
%%------------------------------------------------------------------------------
-define(RMCP_NORMAL, 0).

%%------------------------------------------------------------------------------
%% The RMCP ACK message type (length is 1bit).
%%------------------------------------------------------------------------------
-define(RMCP_ACK, 1).

%%------------------------------------------------------------------------------
%% The RMCP ASF class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_ASF, 16#06).

%%------------------------------------------------------------------------------
%% The RMCP IPMI class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_IPMI, 16#07).

%%------------------------------------------------------------------------------
%% The RMCP OEM class identifier (length is 5bits).
%%------------------------------------------------------------------------------
-define(RMCP_OEM, 16#08).

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
          %% RMCP part
          seq_nr = 255       :: 0..255,
          %% ASF part
          class  = ?RMCP_ASF :: non_neg_integer()}).

%%------------------------------------------------------------------------------
%% The RMCP ASF PING Message.
%%------------------------------------------------------------------------------
-record(rmcp_ping, {
          %% RMCP part
          seq_nr = 255  :: 0..255,
          %% ASF part
          asf_tag = 255 :: 0..255}).

%%------------------------------------------------------------------------------
%% The RMCP ASF PONG Message.
%%------------------------------------------------------------------------------
-record(rmcp_pong, {
          %% RMCP part
          seq_nr = 255  :: 0..255,
          %% ASF part
          asf_tag = 255 :: 0..255,
          iana = 4542   :: non_neg_integer(), %% the IANA enterprise number
          oem = 0       :: non_neg_integer(), %% OEM defined values
          entities = [] :: [ipmi]}).          %% supported entities

%%------------------------------------------------------------------------------
%% The RMCP IPMI Message.
%%------------------------------------------------------------------------------
-record(rmcp_ipmi, {
          %% RMCP part
          seq_nr = 255       :: 0..255,
          %% IPMI 1.5 Session Header part
          auth_type = none   :: none | md2 | md5 | pwd,
          auth_code          :: undefined | integer(),    %% omitted when undefined
          session_id = 0     :: non_neg_integer(),
          session_seq_nr = 0 :: non_neg_integer(),
          %% IPMI 1.5 Payload part
          payload            :: binary()}).               %% must not be empty

-endif. %% eipmi_hrl_
