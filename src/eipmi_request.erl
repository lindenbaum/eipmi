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

-export([encode/2]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Encodes IPMI requests according to the concrete request type. All needed
%% values will be retrieved from the provided property list.
%% @end
%%------------------------------------------------------------------------------
encode(?GET_CHANNEL_AUTHENTICATION_CAPABILITIES, Properties) ->
    P = encode_privilege(get_val(?PRIVILEGE, Properties)),
    <<0:1, ?EIPMI_RESERVED:3, ?IPMI_REQUESTED_CHANNEL:4, ?EIPMI_RESERVED:4,P:4>>;

encode(?GET_SESSION_CHALLENGE, Properties) ->
    A = eipmi_auth:encode_type(get_val(?AUTH_TYPE, Properties)),
    U = eipmi_util:normalize(16, get_val(?USER, Properties)),
    <<?EIPMI_RESERVED:4, A:4, U/binary>>;

encode(?ACTIVATE_SESSION, Properties) ->
    A = eipmi_auth:encode_type(get_val(?AUTH_TYPE, Properties)),
    P = encode_privilege(get_val(?PRIVILEGE, Properties)),
    C = eipmi_util:normalize(16, get_val(?CHALLENGE, Properties)),
    S = get_val(?OUTBOUND_SEQ_NR, Properties),
    <<?EIPMI_RESERVED:4, A:4, ?EIPMI_RESERVED:4, P:4, C/binary, S:32/little>>;

encode(?CLOSE_SESSION, Properties) ->
    <<(get_val(?SESSION_ID, Properties)):32>>.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_val(Property, Properties) ->
    proplists:get_value(Property, Properties).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_privilege(callback) -> 1;
encode_privilege(user) -> 2;
encode_privilege(operator) -> 3;
encode_privilege(administrator) -> 4.
