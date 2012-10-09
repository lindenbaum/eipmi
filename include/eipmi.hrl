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
%%% Requests
%%%=============================================================================

%%%=============================================================================
%%% Responses
%%%=============================================================================

%%------------------------------------------------------------------------------
%% The GetChannelAuthenticationCapabilities response.
%%------------------------------------------------------------------------------
-record(channel_authentication_capabilities, {
          auth_types = []                   :: [none | md2 | md5 | pwd],
          per_msg_auth_enabled = false      :: boolean(),
          user_level_auth_enabled = false   :: boolean(),
          anonymous_login_status = []       :: [null | non_null | anonymous]}).

%%------------------------------------------------------------------------------
%% The GetSessionChallenge response.
%%------------------------------------------------------------------------------
-record(get_session_challenge, {
          temp_id   :: non_neg_integer(),
          challenge :: binary()}).

%%------------------------------------------------------------------------------
%% The ActivateSession response.
%%------------------------------------------------------------------------------
-record(activate_session, {
          auth_type = none       :: eipmi_auth:type(),
          session_id             :: non_neg_integer(),
          initial_inbound_seq_nr :: non_neg_integer(),
          privilege_level        :: callback | user | operator | administrator}).

-endif. %% eipmi_hrl_
