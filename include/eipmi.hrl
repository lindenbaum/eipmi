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
%%% Protocol Defines
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
%% The channel number a session is requested for (default is the current channel).
%%------------------------------------------------------------------------------
-define(IPMI_REQUESTED_CHANNEL, 16#e).

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
%% The IPMI net function for application requests (length is 6bits).
%%------------------------------------------------------------------------------
-define(IPMI_NETFN_APPLICATION_REQUEST, 16#06).

%%------------------------------------------------------------------------------
%% The IPMI net function for application responses (length is 6bits).
%%------------------------------------------------------------------------------
-define(IPMI_NETFN_APPLICATION_RESPONSE, 16#07).

%%------------------------------------------------------------------------------
%% The default reponder address sent in all IPMI requests, currently this is
%% directly the BMC.
%%------------------------------------------------------------------------------
-define(IPMI_RESPONDER_ADDR, 16#20).

%%------------------------------------------------------------------------------
%% The default reponder logical unit (the BMC's event receiver function).
%%------------------------------------------------------------------------------
-define(IPMI_RESPONDER_LUN, 2#00).

%%------------------------------------------------------------------------------
%% The default requestor logical unit.
%%------------------------------------------------------------------------------
-define(IPMI_REQUESTOR_LUN, 2#00).

%%%=============================================================================
%%% IPMI Commands
%%%=============================================================================

-define(GET_CHANNEL_AUTHENTICATION_CAPABILITIES, 16#38).
-define(GET_SESSION_CHALLENGE, 16#39).
-define(ACTIVATE_SESSION, 16#3a).
-define(CLOSE_SESSION, 16#3c).

%%%=============================================================================
%%% Properties
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Valid property tags.
%%------------------------------------------------------------------------------
-define(AUTH_TYPE, auth_type).
-define(AUTH_TYPES, auth_types).
-define(CHALLENGE, challenge).
-define(COMPLETION, completion).
-define(INBOUND_SEQ_NR, inbound_seq_nr).
-define(LOGIN_STATUS, login_status).
-define(OUTBOUND_SEQ_NR, outbound_seq_nr).
-define(PER_MSG_ENABLED, per_msg_enabled).
-define(PASSWORD, password).
-define(PRIVILEGE, privilege).
-define(RQ_ADDR, rq_addr).
-define(RQ_SEQ_NR, rq_seq_nr).
-define(SESSION_ID, session_id).
-define(USER, user).

%%------------------------------------------------------------------------------
%% Valid property tuple constructors.
%%------------------------------------------------------------------------------
-define(AUTH_TYPE(Value), {?AUTH_TYPE, Value}).
-define(AUTH_TYPES(Value), {?AUTH_TYPES, Value}).
-define(CHALLENGE(Value), {?CHALLENGE, Value}).
-define(COMPLETION(Value), {?COMPLETION, Value}).
-define(INBOUND_SEQ_NR(Value), {?INBOUND_SEQ_NR, Value}).
-define(LOGIN_STATUS(Value), {?LOGIN_STATUS, Value}).
-define(OUTBOUND_SEQ_NR(Value), {?OUTBOUND_SEQ_NR, Value}).
-define(PER_MSG_ENABLED(Value), {?PER_MSG_ENABLED, Value}).
-define(PASSWORD(Value), {?PASSWORD, Value}).
-define(PRIVILEGE(Value), {?PRIVILEGE, Value}).
-define(RQ_ADDR(Value), {?RQ_ADDR, Value}).
-define(RQ_SEQ_NR(Value), {?RQ_SEQ_NR, Value}).
-define(SESSION_ID(Value), {?SESSION_ID, Value}).
-define(USER(Value), {?USER, Value}).

%%%=============================================================================
%%% Messages
%%%=============================================================================

%%------------------------------------------------------------------------------
%% The RMCP message header.
%%------------------------------------------------------------------------------
-record(rmcp_header, {
          version = ?RMCP_VERSION :: 0..255,
          seq_nr  = ?RMCP_NOREPLY :: 0..255,
          class   = ?RMCP_ASF     :: 0..255}).

%%------------------------------------------------------------------------------
%% The RMCP ACK Message.
%%------------------------------------------------------------------------------
-record(rmcp_ack, {
          header :: #rmcp_header{}}).

%%------------------------------------------------------------------------------
%% The ASF Ping payload.
%%------------------------------------------------------------------------------
-record(asf_ping, {
          iana = ?ASF_IANA :: non_neg_integer(), %% the IANA enterprise number
          tag = 0          :: 0..255}).

%%------------------------------------------------------------------------------
%% The ASF Pong payload.
%%------------------------------------------------------------------------------
-record(asf_pong, {
          iana = ?ASF_IANA :: non_neg_integer(), %% the IANA enterprise number
          tag = 0          :: 0..255,
          oem = 0          :: non_neg_integer(), %% OEM defined values
          entities = []    :: [ipmi]}).          %% supported entities

%%------------------------------------------------------------------------------
%% An RMCP ASF Message.
%%------------------------------------------------------------------------------
-record(rmcp_asf, {
          header  :: #rmcp_header{},
          payload :: #asf_ping{} | #asf_pong{}}).

%%------------------------------------------------------------------------------
%% An RMCP IPMI Message.
%%------------------------------------------------------------------------------
-record(rmcp_ipmi, {
          header          :: #rmcp_header{},
          properties = [] :: proplists:proplist(),
          cmd             :: 0..255,
          type = request  :: request | response,
          data = <<>>     :: binary()}).

-endif. %% eipmi_hrl_
