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

-export([decode/2]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Decodes IPMI responses according to the concrete command code, returning a
%% property list with the decoded values.
%% @end
%%------------------------------------------------------------------------------
-spec decode(eipmi:response(), binary()) ->
                    proplists:proplist().
decode({?IPMI_NETFN_SENSOR_EVENT_RESPONSE, Cmd}, Data) ->
    decode_sensor_event(Cmd, Data);
decode({?IPMI_NETFN_APPLICATION_RESPONSE, Cmd}, Data) ->
    decode_application(Cmd, Data);
decode({?IPMI_NETFN_STORAGE_RESPONSE, Cmd}, Data) ->
    decode_storage(Cmd, Data);
decode({?IPMI_NETFN_TRANSPORT_RESPONSE, Cmd}, Data) ->
    decode_transport(Cmd, Data).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_sensor_event(_Cmd, _Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_application(?GET_DEVICE_ID,
                   <<Id:8, _:1, ?EIPMI_RESERVED:3, Revision:4,
                     Operation:1, Major:7, Minor:8,
                     IPMILeast:4, IPMIMost:4,
                     Support:8, Manufacterer:24/little,
                     Product:16/little, _/binary>>) ->
    [{device_id, Id},
     {device_revision, Revision},
     {operation, case Operation of 0 -> normal; 1 -> progress end},
     {firmware_version, eipmi_util:format("~B.~B", [Major, Minor])},
     {ipmi_version, eipmi_util:format("~B.~B", [IPMIMost, IPMILeast])},
     {device_support, get_device_support(Support)},
     {manufacturer_id, Manufacterer},
     {product_id, Product}];
decode_application(?COLD_RESET, <<>>) ->
    [];
decode_application(?WARM_RESET, <<>>) ->
    [];
decode_application(?GET_DEVICE_GUID, <<GUID/binary>>) ->
    [{guid, binary_to_list(GUID)}];
decode_application(?GET_SYSTEM_GUID, <<GUID/binary>>) ->
    [{guid, binary_to_list(GUID)}];
decode_application(?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
                   <<_:8, 0:1, _:1, A:6, ?EIPMI_RESERVED:3, _:1, _:1, L:3,
                     ?EIPMI_RESERVED:40>>) ->
    [{auth_types, get_auth_types(A)}, {login_status, get_login_status(L)}];
decode_application(?GET_SESSION_CHALLENGE, <<I:32/little, C/binary>>) ->
    [{session_id, I}, {challenge, C}];
decode_application(?ACTIVATE_SESSION,
                   <<?EIPMI_RESERVED:4, A:4, I:32/little, S:32/little,
                     ?EIPMI_RESERVED:4, P:4>>) ->
    [{session_id, I},
     {inbound_seq_nr, S},
     {auth_type, eipmi_auth:decode_type(A)},
     {privilege, decode_privilege(P)}];
decode_application(?SET_SESSION_PRIVILEGE_LEVEL, <<?EIPMI_RESERVED:4, P:4>>) ->
    [{privilege, decode_privilege(P)}];
decode_application(?CLOSE_SESSION, <<>>) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_storage(_Cmd, _Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_transport(_Cmd, _Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_device_support(Support) ->
    A = case Support band 2#10000000 of 2#10000000 -> [chassis]; _ -> [] end,
    B = case Support band 2#1000000 of 2#1000000 -> [bridge]; _ -> [] end,
    C = case Support band 2#100000 of 2#100000 -> [event_generator]; _ -> [] end,
    D = case Support band 2#10000 of 2#10000 -> [event_receiver]; _ -> [] end,
    E = case Support band 2#1000 of 2#1000 -> [fru_inventory]; _ -> [] end,
    F = case Support band 2#100 of 2#100 -> [sel]; _ -> [] end,
    G = case Support band 2#10 of 2#10 -> [sdr]; _ -> [] end,
    H = case Support band 2#1 of 2#1 -> [sensor]; _ -> [] end,
    A ++ B ++ C ++ D ++ E ++ F ++ G ++ H.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_auth_types(AuthTypes) ->
    A = case AuthTypes band 2#10000 of 2#10000 -> [pwd]; _ -> [] end,
    B = case AuthTypes band 2#100 of 2#100 -> [md5]; _ -> [] end,
    C = case AuthTypes band 2#10 of 2#10 -> [md2]; _ -> [] end,
    D = case AuthTypes band 2#1 of 2#1 -> [none]; _ -> [] end,
    A ++ B ++ C ++ D.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_login_status(LoginStatus) ->
    A = case LoginStatus band 2#100 of 2#100 -> [non_null]; _ -> [] end,
    B = case LoginStatus band 2#10 of 2#10 -> [null]; _ -> [] end,
    C = case LoginStatus band 2#1 of 2#1 -> [anonymous]; _ -> [] end,
    A ++ B ++ C.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_privilege(1) -> callback;
decode_privilege(2) -> user;
decode_privilege(3) -> operator;
decode_privilege(4) -> administrator.
