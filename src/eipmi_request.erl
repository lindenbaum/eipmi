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
%%% A module providing encoding functionality for the data parts of IPMI
%%% requests.
%%% @end
%%%=============================================================================

-module(eipmi_request).

-export([get_channel_authentication_capabilities/1,
         get_session_challenge/2,
         activate_session/4]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a "Get Channel Authentication Capabilites" request. The privilege
%% level is the maximum level requested during the session.
%% @end
%%------------------------------------------------------------------------------
get_channel_authentication_capabilities(Privilege) ->
    <<0:1, ?EIPMI_RESERVED:3, ?IPMI_REQUESTED_CHANNEL:4,
      ?EIPMI_RESERVED:4,(encode_privilege(Privilege)):4>>.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes a "Get Session Challenge" request. The authentication type must be
%% one of the types provided in the "Get Channel Authentication Capabilites"
%% response. User name may be specified as string or as undefined when the
%% null user (User 1) is requested. The user name will be trimmed/padded to
%% 16bytes.
%% @end
%%------------------------------------------------------------------------------
get_session_challenge(AuthType, undefined) ->
    <<?EIPMI_RESERVED:4, (eipmi_auth:encode_type(AuthType)):4, 0:128>>;
get_session_challenge(AuthType, UserName) ->
    User = eipmi_util:normalize(16, UserName),
    <<?EIPMI_RESERVED:4, (eipmi_auth:encode_type(AuthType)):4, User/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes an "Activate Session" request. The authentication type must match the
%% type requested in the corresponding "Get Session Challenge" request. The
%% privilege level must be less or equal to the privilege level requested in the
%% "Get Session Challenge" request. Authentication code has to be a binary
%% containing the encrypted secret (16bytes). The initial outbound sequence
%% number must be greater than zero.
%% @end
%%------------------------------------------------------------------------------
activate_session(AuthType, Privilege, AuthCode, InitialOutboundSeqNr) ->
    <<?EIPMI_RESERVED:4, (eipmi_auth:encode_type(AuthType)):4,
      ?EIPMI_RESERVED:4, (encode_privilege(Privilege)):4,
      AuthCode/binary, InitialOutboundSeqNr:32/little>>.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_privilege(callback) -> 1;
encode_privilege(user) -> 2;
encode_privilege(operator) -> 3;
encode_privilege(administrator) -> 4.
