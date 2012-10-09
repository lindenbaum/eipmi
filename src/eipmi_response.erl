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
%%% A module providing decoding functionality for the data parts of IPMI
%%% responses.
%%% @end
%%%=============================================================================

-module(eipmi_response).

-export([get_channel_authentication_capabilities/1]).

-include("eipmi.hrl").

-record(channel_authentication_capabilities, {
          auth_types = []                   :: [none | md2 | md5 | pwd],
          per_msg_auth_enabled = false      :: boolean(),
          user_level_auth_enabled = false   :: boolean(),
          anonymous_login_status = []       :: [null | non_null | anonymous]}).

-record(get_session_challenge, {
         temp_id   :: non_neg_integer(),
         challenge :: binary()}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
get_channel_authentication_capabilities(
  <<_:8, _:2, AuthTypes:6,
    ?EIPMI_RESERVED:3, PerMsg:1, UserLevel:1, LoginStatus:3,
    ?EIPMI_RESERVED:40>>) ->
    #channel_authentication_capabilities{
       auth_types = get_auth_types(AuthTypes),
       per_msg_auth_enabled = to_bool(PerMsg),
       user_level_auth_enabled = to_bool(UserLevel),
       anonymous_login_status = get_login_status(LoginStatus)}.

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
get_session_challenge(<<TempId:32/little, Challenge/binary>>) ->
    #get_session_challenge{temp_id = TempId, challenge = Challenge}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_bool(0) -> true;
to_bool(1) -> false.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_auth_types(AuthTypes) ->
    A = case AuthTypes band 2#10000 of 1 -> [pwd]; _ -> [] end,
    B = case AuthTypes band 2#100 of 1 -> [md5]; _ -> [] end,
    C = case AuthTypes band 2#10 of 1 -> [md2]; _ -> [] end,
    D = case AuthTypes band 2#1 of 1 -> [none]; _ -> [] end,
    A ++ B ++ C ++ D.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_login_status(LoginStatus) ->
    A = case LoginStatus band 2#100 of 1 -> [non_null]; _ -> [] end,
    B = case LoginStatus band 2#10 of 1 -> [null]; _ -> [] end,
    C = case LoginStatus band 2#1 of 1 -> [anonymous]; _ -> [] end,
    A ++ B ++ C.
