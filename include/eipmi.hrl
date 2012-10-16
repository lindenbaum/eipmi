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
%% The channel number a session is requested for (default is the current channel).
%%------------------------------------------------------------------------------
-define(IPMI_REQUESTED_CHANNEL, 16#e).

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

-define(AUTH_TYPE, auth_type).
-define(AUTH_TYPES, auth_types).
-define(CHALLENGE, challenge).
-define(INBOUND_SEQ_NR, inbound_seq_nr).
-define(LOGIN_STATUS, login_status).
-define(OUTBOUND_SEQ_NR, outbound_seq_nr).
-define(PER_MSG_ENABLED, per_msg_enabled).
-define(PASSWORD, password).
-define(PRIVILEGE, privilege).
-define(SESSION_ID, session_id).
-define(USER, user).

-define(AUTH_TYPE(Value), {?AUTH_TYPE, Value}).
-define(AUTH_TYPES(Value), {?AUTH_TYPES, Value}).
-define(CHALLENGE(Value), {?CHALLENGE, Value}).
-define(INBOUND_SEQ_NR(Value), {?INBOUND_SEQ_NR, Value}).
-define(LOGIN_STATUS(Value), {?LOGIN_STATUS, Value}).
-define(OUTBOUND_SEQ_NR(Value), {?OUTBOUND_SEQ_NR, Value}).
-define(PER_MSG_ENABLED(Value), {?PER_MSG_ENABLED, Value}).
-define(PASSWORD(Value), {?PASSWORD, Value}).
-define(PRIVILEGE(Value), {?PRIVILEGE, Value}).
-define(SESSION_ID(Value), {?SESSION_ID, Value}).
-define(USER(Value), {?USER, Value}).

-endif. %% eipmi_hrl_
