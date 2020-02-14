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
%%% requests. This module will need care if support for new requests is
%%% demanded.
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
-spec encode(eipmi:request(), proplists:proplist()) -> binary().
encode({?IPMI_NETFN_SENSOR_EVENT_REQUEST, Cmd}, Properties) ->
    encode_sensor_event(Cmd, Properties);
encode({?IPMI_NETFN_APPLICATION_REQUEST, Cmd}, Properties) ->
    encode_application(Cmd, Properties);
encode({?IPMI_NETFN_STORAGE_REQUEST, Cmd}, Properties) ->
    encode_storage(Cmd, Properties);
encode({?IPMI_NETFN_TRANSPORT_REQUEST, Cmd}, Properties) ->
    encode_transport(Cmd, Properties);
encode({?IPMI_NETFN_PICMG_REQUEST, Cmd}, Properties) ->
    encode_picmg(Cmd, Properties);
encode({NetFn, Cmd}, Properties) when NetFn >= 16#2e ->
    encode_oem(NetFn, Cmd, Properties).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_sensor_event(?GET_DEVICE_SDR, Properties) ->
    Reservation = proplists:get_value(reservation_id, Properties, 16#0000),
    Record = proplists:get_value(record_id, Properties),
    Offset = proplists:get_value(offset, Properties, 16#00),
    Count = proplists:get_value(count, Properties, 16#ff),
    true = Record =< 16#ffff,
    <<Reservation:16/little, Record:16/little, Offset:8, Count:8>>;
encode_sensor_event(?RESERVE_DEVICE_SDR_REPOSITORY, _Properties) ->
    <<>>;
encode_sensor_event(?GET_SENSOR_READING, Properties) ->
    N = proplists:get_value(sensor_number, Properties),
    <<N:8>>;
encode_sensor_event(?PET_ACKNOWLEDGE, Properties) ->
    SeqNr = proplists:get_value(seq_nr, Properties, 0),
    LocalTime = proplists:get_value(local_time, Properties),
    EventSource = proplists:get_value(event_source_raw, Properties),
    SensorDevice = proplists:get_value(sensor_device, Properties, 16#ff),
    SensorNumber = proplists:get_value(sensor_number, Properties, 16#00),
    <<Data:3/binary, _/binary>> = proplists:get_value(data, Properties),
    <<SeqNr:16/little, LocalTime:32/little, EventSource, SensorDevice,
      SensorNumber, Data/binary>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_application(?SEND_MESSAGE, Properties) ->
    Channel = proplists:get_value(channel, Properties),
    Embedded = proplists:get_value(request, Properties),
    RsAddr = proplists:get_value(rec_rs_addr, Properties, ?IPMI_RESPONDER_ADDR),
    RsLun = proplists:get_value(rec_rs_lun, Properties, ?IPMI_RESPONDER_LUN),
    NetFn = proplists:get_value(net_fn, Embedded),
    Cmd = proplists:get_value(cmd, Embedded),
    RecursiveRs = [{rec_rs_addr, RsAddr}, {rec_rs_lun, RsLun}],
    Data = encode({NetFn, Cmd}, Embedded ++ RecursiveRs),
    Rq = [{rq_addr, RsAddr}, {rq_lun, RsLun}, {rq_seq_nr, 0}],
    Request = eipmi_encoder:request(Embedded ++ Rq, {NetFn, Cmd}, Data),
    %% currently, only tracked requests are supported
    <<1:2, 0:2, Channel:4, Request/binary>>;
encode_application(?GET_CHANNEL_AUTHENTICATION_CAPABILITIES, Properties) ->
    P = encode_privilege(proplists:get_value(privilege, Properties)),
    <<0:1, 0:3, ?IPMI_REQUESTED_CHANNEL:4, 0:4,P:4>>;
encode_application(?GET_SESSION_CHALLENGE, Properties) ->
    A = eipmi_auth:encode_type(proplists:get_value(auth_type, Properties)),
    U = eipmi_util:normalize(16, proplists:get_value(user, Properties)),
    <<0:4, A:4, U/binary>>;
encode_application(?ACTIVATE_SESSION, Properties) ->
    A = eipmi_auth:encode_type(proplists:get_value(auth_type, Properties)),
    P = encode_privilege(proplists:get_value(privilege, Properties)),
    C = eipmi_util:normalize(16, proplists:get_value(challenge, Properties)),
    S = proplists:get_value(initial_outbound_seq_nr, Properties),
    <<0:4, A:4, 0:4, P:4, C/binary, S:32/little>>;
encode_application(?SET_SESSION_PRIVILEGE_LEVEL, Properties) ->
    <<0:4, (encode_privilege(proplists:get_value(privilege, Properties))):4>>;
encode_application(?CLOSE_SESSION, Properties) ->
    <<(proplists:get_value(session_id, Properties)):32/little>>;
encode_application(Req, _Properties)
  when Req =:= ?GET_DEVICE_ID orelse
       Req =:= ?COLD_RESET orelse
       Req =:= ?WARM_RESET orelse
       Req =:= ?GET_SELF_TEST_RESULTS orelse
       Req =:= ?GET_ACPI_POWER_STATE orelse
       Req =:= ?GET_DEVICE_GUID orelse
       Req =:= ?GET_SYSTEM_GUID ->
    <<>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_storage(?GET_FRU_INVENTORY_AREA_INFO, Properties) ->
    <<(proplists:get_value(fru_id, Properties)):8>>;
encode_storage(?READ_FRU_DATA, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    Offset = proplists:get_value(offset, Properties),
    Count = proplists:get_value(count, Properties),
    true = Offset =< 16#ffff,
    <<FruId:8, Offset:16/little, Count:8>>;
encode_storage(?GET_SEL_ENTRY, Properties) ->
    Record = proplists:get_value(record_id, Properties),
    true = Record =< 16#ffff,
    <<0:16, Record:16/little, 0:8, 16#ff:8>>;
encode_storage(?CLEAR_SEL, Properties) ->
    Reservation = proplists:get_value(reservation_id, Properties),
    Init = proplists:get_value(initiate, Properties, true),
    InitOrGet = case Init of true -> 16#aa; false -> 0 end,
    <<Reservation:16/little, $C:8, $L:8, $R:8, InitOrGet:8>>;
encode_storage(?GET_SDR, Properties) ->
    Reservation = proplists:get_value(reservation_id, Properties, 16#0000),
    Record = proplists:get_value(record_id, Properties),
    Offset = proplists:get_value(offset, Properties, 16#00),
    Count = proplists:get_value(count, Properties, 16#ff),
    true = Record =< 16#ffff,
    <<Reservation:16/little, Record:16/little, Offset:8, Count:8>>;
encode_storage(Req, _Properties)
  when Req =:= ?GET_SEL_INFO orelse
       Req =:= ?RESERVE_SEL orelse
       Req =:= ?GET_SDR_REPOSITORY_INFO orelse
       Req =:= ?RESERVE_SDR_REPOSITORY ->
    <<>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_transport(?GET_IP_UDP_RMCP_STATISTICS, Properties) ->
    Clear = proplists:get_value(clear_statistics, Properties, false),
    C = case Clear of true -> 1; false -> 0 end,
    <<0:4, ?IPMI_REQUESTED_CHANNEL:4, 0:7, C:1>>;
encode_transport(?SET_LAN_CONFIGURATION_PARAMETERS, Properties) ->
    P = proplists:get_value(parameter, Properties),
    D = case P of
            3 ->
                V = proplists:get_value(ip_address, Properties),
                list_to_binary(tuple_to_list(V));
            4 ->
                case proplists:get_value(ip_assignment, Properties) of
                    static         -> <<0:4, 1:4>>;
                    dhcp           -> <<0:4, 2:4>>;
                    bios_or_system -> <<0:4, 3:4>>;
                    other          -> <<0:4, 4:4>>
                end;
            5 ->
                V = proplists:get_value(mac_address, Properties),
                list_to_binary(tuple_to_list(V));
            6 ->
                V = proplists:get_value(subnet_mask, Properties),
                list_to_binary(tuple_to_list(V));
            12 ->
                V = proplists:get_value(default_gateway, Properties),
                list_to_binary(tuple_to_list(V));
            13 ->
                V = proplists:get_value(default_gateway_mac_address, Properties),
                list_to_binary(tuple_to_list(V));
            14 ->
                V = proplists:get_value(backup_gateway, Properties),
                list_to_binary(tuple_to_list(V));
            15 ->
                V = proplists:get_value(backup_gateway_gateway_mac_address, Properties),
                list_to_binary(tuple_to_list(V));
            16 ->
                C = proplists:get_value(community, Properties),
                Len = length(C),
                case Len >= 18 of
                    true  -> list_to_binary(string:substr(C, 1, 18));
                    false -> list_to_binary(C ++ lists:duplicate(18 - Len, 0))
                end;
            18 ->
                S = proplists:get_value(set, Properties, 0),
                T = case proplists:get_value(destination_type, Properties, trap) of
                        trap -> 0;
                        oem1 -> 6;
                        oem2 -> 7
                    end,
                A = case proplists:get_value(acknowledge, Properties, false) of
                        true  -> 1;
                        false -> 0
                    end,
                To = proplists:get_value(timeout, Properties, 0),
                R = proplists:get_value(retries, Properties, 0),
                <<0:4, S:4, A:1, 0:4, T:3, To:8, 0:5, R:3>>;
            19 ->
                S = proplists:get_value(set, Properties, 0),
                G = case proplists:get_value(gateway, Properties, default) of
                        default -> 0;
                        backup  -> 1
                    end,
                DefIA = {0, 0, 0, 0},
                IA = proplists:get_value(ip_address, Properties, DefIA),
                I = list_to_binary(tuple_to_list(IA)),
                DefMA = {0, 0, 0, 0, 0, 0},
                MA = proplists:get_value(mac_address, Properties, DefMA),
                M = list_to_binary(tuple_to_list(MA)),
                <<0:4, S:4, 0:4, 0:4, 0:7, G:1, I/binary, M/binary>>
            end,
    <<0:4, ?IPMI_REQUESTED_CHANNEL:4, P:8, D/binary>>;
encode_transport(?GET_LAN_CONFIGURATION_PARAMETERS, Properties) ->
    P = proplists:get_value(parameter, Properties),
    S = proplists:get_value(set, Properties, 0),
    B = proplists:get_value(block, Properties, 0),
    <<0:1, 0:3, ?IPMI_REQUESTED_CHANNEL:4, P:8, S:8 , B:8>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_picmg(?GET_PICMG_PROPERTIES, _Properties) ->
    <<?PICMG_ID:8>>;
encode_picmg(?GET_ADDRESS_INFO, Properties) ->
    N = proplists:get_value(site_number, Properties),
    case proplists:get_value(fru_id, Properties) of
        undefined ->
            case proplists:get_value(site_type, Properties) of
                undefined ->
                    <<?PICMG_ID:8>>;
                T ->
                    SiteT = encode_picmg_site_type(T),
                    <<?PICMG_ID:8, 0:8, 3:8, N:8, SiteT:8>>
            end;
        FruId ->
            <<?PICMG_ID:8, FruId:8>>
    end;
encode_picmg(?SET_FRU_ACTIVATION_POLICY, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    D = proplists:get_value(deactivation_locked, Properties),
    L = proplists:get_value(locked, Properties),
    Mask = set_activation_mask([L, D]),
    Set = set_activation_policy([L, D]),
    <<?PICMG_ID:8, FruId:8, Mask:8, Set:8>>;
encode_picmg(?GET_FRU_ACTIVATION_POLICY, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    <<?PICMG_ID:8, FruId:8>>;
encode_picmg(?SET_FRU_ACTIVATION, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    Activate = proplists:get_value(activate, Properties),
    <<?PICMG_ID:8, FruId:8, (case Activate of true -> 1; false -> 0 end):8>>;
encode_picmg(?FRU_CONTROL, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    Control = proplists:get_value(control, Properties),
    <<?PICMG_ID:8, FruId:8, (encode_fru_control(Control)):8>>;
encode_picmg(?GET_DEVICE_LOCATOR_RECORD_ID, Properties) ->
    FruId = proplists:get_value(fru_id, Properties),
    <<?PICMG_ID:8, FruId:8>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_oem(_, _, Properties) -> proplists:get_value(data, Properties).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_activation_mask(List) ->
    {Mask, _} = lists:foldl(fun set_activation_mask/2, {0, 0}, List),
    Mask.
set_activation_mask(undefined, {Mask, Bit}) -> {Mask, Bit + 1};
set_activation_mask(_, {Mask, Bit}) -> {Mask + (1 bsl Bit), Bit + 1}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_activation_policy(List) ->
    {Set, _} = lists:foldl(fun set_activation_policy/2, {0, 0}, List),
    Set.
set_activation_policy(true, {Set, Bit}) -> {Set + (1 bsl Bit), Bit + 1};
set_activation_policy(_, {Set, Bit}) -> {Set, Bit + 1}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_privilege(present) -> 0;
encode_privilege(callback) -> 1;
encode_privilege(user) -> 2;
encode_privilege(operator) -> 3;
encode_privilege(administrator) -> 4.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_fru_control(cold_reset) -> 0;
encode_fru_control(warm_reset) -> 1;
encode_fru_control(graceful_reboot) -> 2;
encode_fru_control(diagnostic_interrupt) -> 3;
encode_fru_control(quiesce) -> 4.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_picmg_site_type(picmg_board) -> 16#00;
encode_picmg_site_type(power_entry) -> 16#01;
encode_picmg_site_type(shelf_fru_information) -> 16#02;
encode_picmg_site_type(dedicated_shelf_management_controller) -> 16#03;
encode_picmg_site_type(fan_tray) -> 16#04;
encode_picmg_site_type(fan_filter_tray) -> 16#05;
encode_picmg_site_type(alarm) -> 16#06;
encode_picmg_site_type(amc) -> 16#07;
encode_picmg_site_type(pmc) -> 16#08;
encode_picmg_site_type(rear_transition_module) -> 16#09;
encode_picmg_site_type(mch) -> 16#0a;
encode_picmg_site_type(power_module) -> 16#0b.
