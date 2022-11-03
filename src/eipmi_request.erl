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

-export([encode/2, encode_privilege/1]).

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
-spec encode(
    eipmi:request() | open_session_rq | rakp1 | rakp3,
    proplists:proplist()
) -> binary().
encode({?IPMI_NETFN_SENSOR_EVENT_REQUEST, Cmd}, Properties) ->
    encode_sensor_event(Cmd, Properties);
encode({?IPMI_NETFN_APPLICATION_REQUEST, Cmd}, Properties) ->
    encode_application(Cmd, Properties);
encode({?IPMI_NETFN_STORAGE_REQUEST, Cmd}, Properties) ->
    encode_storage(Cmd, Properties);
encode({?IPMI_NETFN_TRANSPORT_REQUEST, Cmd}, Properties) ->
    encode_transport(Cmd, Properties);
encode({?IPMI_NETFN_CHASSIS_REQUEST, Cmd}, Properties) ->
    encode_chassis(Cmd, Properties);
encode({?IPMI_NETFN_PICMG_REQUEST, Cmd}, Properties) ->
    encode_picmg(Cmd, Properties);
encode({NetFn, Cmd}, Properties) when NetFn >= 16#2e ->
    encode_oem(NetFn, Cmd, Properties);
encode(open_session_rq, Properties) ->
    encode_open_session_rq(Properties);
encode(rakp1, Properties) ->
    encode_rakp1(Properties);
encode(rakp3, Properties) ->
    encode_rakp3(Properties).

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
    LocalTime = proplists:get_value(local_time, Properties, 0),
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
    <<1:1, 0:3, ?IPMI_REQUESTED_CHANNEL:4, 0:4, P:4>>;
encode_application(?GET_SESSION_CHALLENGE, Properties) ->
    A = eipmi_auth:encode_type(proplists:get_value(rq_auth_type, Properties)),
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
encode_application(Req, _Properties) when
    Req =:= ?GET_DEVICE_ID orelse
        Req =:= ?COLD_RESET orelse
        Req =:= ?WARM_RESET orelse
        Req =:= ?GET_SELF_TEST_RESULTS orelse
        Req =:= ?GET_ACPI_POWER_STATE orelse
        Req =:= ?GET_DEVICE_GUID orelse
        Req =:= ?GET_SYSTEM_GUID
->
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
    InitOrGet =
        case Init of
            true -> 16#aa;
            false -> 0
        end,
    <<Reservation:16/little, $C:8, $L:8, $R:8, InitOrGet:8>>;
encode_storage(?GET_SDR, Properties) ->
    Reservation = proplists:get_value(reservation_id, Properties, 16#0000),
    Record = proplists:get_value(record_id, Properties),
    Offset = proplists:get_value(offset, Properties, 16#00),
    Count = proplists:get_value(count, Properties, 16#ff),
    true = Record =< 16#ffff,
    <<Reservation:16/little, Record:16/little, Offset:8, Count:8>>;
encode_storage(Req, _Properties) when
    Req =:= ?GET_SEL_INFO orelse
        Req =:= ?RESERVE_SEL orelse
        Req =:= ?GET_SDR_REPOSITORY_INFO orelse
        Req =:= ?RESERVE_SDR_REPOSITORY
->
    <<>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_transport(?GET_IP_UDP_RMCP_STATISTICS, Properties) ->
    C = get_encoded_bool(clear_statistics, Properties),
    <<0:4, ?IPMI_REQUESTED_CHANNEL:4, 0:7, C:1>>;
encode_transport(?SET_LAN_CONFIGURATION_PARAMETERS, Properties) ->
    P = proplists:get_value(parameter, Properties),
    D =
        case P of
            ?IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS ->
                V = proplists:get_value(ip_address, Properties),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS_SOURCE ->
                case proplists:get_value(ip_assignment, Properties) of
                    static -> <<0:4, 1:4>>;
                    dhcp -> <<0:4, 2:4>>;
                    bios_or_system -> <<0:4, 3:4>>;
                    other -> <<0:4, 4:4>>
                end;
            ?IPMI_LAN_CONFIGURATION_PARAMETER_MAC_ADDRESS ->
                V = proplists:get_value(mac_address, Properties),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_SUBNET_MASK ->
                V = proplists:get_value(subnet_mask, Properties),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY ->
                V = proplists:get_value(default_gateway, Properties),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS ->
                V = proplists:get_value(
                    default_gateway_mac_address,
                    Properties
                ),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY ->
                V = proplists:get_value(backup_gateway, Properties),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS ->
                V = proplists:get_value(
                    backup_gateway_gateway_mac_address,
                    Properties
                ),
                list_to_binary(tuple_to_list(V));
            ?IPMI_LAN_CONFIGURATION_PARAMETER_COMMUNITY_STRING ->
                C = proplists:get_value(community, Properties),
                Len = length(C),
                case Len >= 18 of
                    true -> list_to_binary(string:substr(C, 1, 18));
                    false -> list_to_binary(C ++ lists:duplicate(18 - Len, 0))
                end;
            ?IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_TYPE ->
                S = proplists:get_value(set, Properties, 0),
                T =
                    case
                        proplists:get_value(destination_type, Properties, trap)
                    of
                        trap -> 0;
                        oem1 -> 6;
                        oem2 -> 7
                    end,
                A = get_encoded_bool(acknowledge, Properties),
                To = proplists:get_value(timeout, Properties, 0),
                R = proplists:get_value(retries, Properties, 0),
                <<0:4, S:4, A:1, 0:4, T:3, To:8, 0:5, R:3>>;
            ?IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_ADDRESSES ->
                S = proplists:get_value(set, Properties, 0),
                G =
                    case proplists:get_value(gateway, Properties, default) of
                        default -> 0;
                        backup -> 1
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
    <<0:1, 0:3, ?IPMI_REQUESTED_CHANNEL:4, P:8, S:8, B:8>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_chassis(?CHASSIS_CONTROL, Properties) ->
    C = proplists:get_value(command, Properties),
    <<0:4, (chassis_command(C)):4>>;
encode_chassis(?CHASSIS_IDENTIFY, Properties) ->
    F = proplists:get_value(force, Properties),
    T = proplists:get_value(interval, Properties),
    case {T, F} of
        {undefined, undefined} ->
            <<>>;
        {T, undefined} ->
            <<T:8>>;
        {undefined, false} ->
            <<0:8, 0:7, 0:1>>;
        {T, false} ->
            <<T:8, 0:7, 0:1>>;
        {_, true} ->
            <<0:8, 0:7, 1:1>>
    end;
encode_chassis(?SET_CHASSIS_CAPABILITIES, Properties) ->
    L = get_encoded_bool(lockout, Properties),
    I = get_encoded_bool(intrusion, Properties),
    F = proplists:get_value(fru_address, Properties),
    Sdr = proplists:get_value(sdr_address, Properties),
    Sel = proplists:get_value(sel_address, Properties),
    Sm = proplists:get_value(sm_address, Properties),
    Acc = <<0:6, L:1, I:1, F:8, Sdr:8, Sel:8, Sm:8>>,
    case proplists:get_value(bridge_address, Properties) of
        undefined ->
            Acc;
        B ->
            <<Acc/binary, B:8>>
    end;
encode_chassis(?SET_POWER_RESTORE_POLICY, Properties) ->
    P =
        case proplists:get_value(policy, Properties) of
            no_change -> 3;
            always_on -> 2;
            last_state -> 1;
            always_off -> 0
        end,
    <<0:5, P:3>>;
encode_chassis(?SET_SYSTEM_BOOT_OPTIONS, Properties) ->
    Param = proplists:get_value(parameter, Properties),
    Data =
        case Param of
            5 -> encode_boot_flags(Properties)
        end,
    <<0:1, Param:7, Data/binary>>;
encode_chassis(?GET_SYSTEM_BOOT_OPTIONS, Properties) ->
    Param = proplists:get_value(parameter, Properties),
    <<0:1, Param:7, 0:8, 0:8>>;
encode_chassis(?SET_FRONT_PANEL_ENABLES, Properties) ->
    S = get_encoded_bool(disable_standby, Properties),
    D = get_encoded_bool(disable_diagnostic_interrupt, Properties),
    R = get_encoded_bool(disable_reset, Properties),
    P = get_encoded_bool(disable_power, Properties),
    <<0:4, S:1, D:1, R:1, P:1>>;
encode_chassis(?SET_POWER_CYCLE_INTERVAL, Properties) ->
    I = proplists:get_value(interval, Properties, 0),
    <<I:8>>;
encode_chassis(Req, _Properties) when
    Req =:= ?GET_CHASSIS_CAPABILITIES orelse
        Req =:= ?GET_CHASSIS_STATUS orelse
        Req =:= ?CHASSIS_RESET orelse
        Req =:= ?GET_SYSTEM_RESTART_CAUSE orelse
        Req =:= ?GET_POH_COUNTER
->
    <<>>.

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
    Activate = get_encoded_bool(activate, Properties),
    <<?PICMG_ID:8, FruId:8, Activate:8>>;
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
encode_boot_flags(Properties) ->
    Persist = get_encoded_bool(persist, Properties),
    Type =
        case proplists:get_value(boot_type, Properties, legacy) of
            legacy -> 0;
            efi -> 1
        end,
    ClrCmos = get_encoded_bool(clear_cmos, Properties),
    LockKbd = get_encoded_bool(lock_keyboard, Properties),
    Device =
        case proplists:get_value(boot_device, Properties, no_override) of
            no_override -> 0;
            pxe -> 1;
            hdd -> 2;
            safe_mode -> 3;
            diag_part -> 4;
            cd_dvd -> 5;
            bios_setup -> 6;
            remote_floppy -> 7;
            remote_cd -> 8;
            remote_primary -> 9;
            remote_hdd -> 11;
            floppy -> 15
        end,
    ScrBlnk = get_encoded_bool(screen_blank, Properties),
    LockReset = get_encoded_bool(lock_reset, Properties),
    LockPower = get_encoded_bool(lock_power, Properties),
    V =
        case proplists:get_value(bios_verbosity, Properties, default) of
            default -> 0;
            quiet -> 1;
            verbose -> 2
        end,
    Traps = get_encoded_bool(progress_traps, Properties),
    PwdBypass = get_encoded_bool(password_bypass, Properties),
    LockSleep = get_encoded_bool(lock_sleeep, Properties),
    Redirection =
        case proplists:get_value(console_redirection, Properties, default) of
            default -> 0;
            disable -> 1;
            enable -> 2
        end,
    Inst = proplists:get_value(device_instance, Properties, 0),
    % Data1
    <<1:1, Persist:1, Type:1, 0:5,
        % Data2
        ClrCmos:1, LockKbd:1, Device:4, ScrBlnk:1, LockReset:1,
        % Data3
        LockPower:1, V:2, Traps:1, PwdBypass:1, LockSleep:1, Redirection:2,
        % Data4
        0:4, 0:1, 0:3,
        % Data5
        0:3, Inst:5>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_open_session_rq(Properties) ->
    Tag = proplists:get_value(message_tag, Properties),
    P = encode_privilege(proplists:get_value(privilege, Properties)),
    S = proplists:get_value(rq_session_id, Properties),
    A = eipmi_auth:encode_rakp_type(
        proplists:get_value(rakp_auth_type, Properties)
    ),
    I = eipmi_auth:encode_integrity_type(
        proplists:get_value(integrity_type, Properties)
    ),
    E = eipmi_auth:encode_encrypt_type(
        proplists:get_value(encrypt_type, Properties)
    ),
    <<Tag:8, 0:4, P:4, 0:16, S:32/little, 0:8, 0:16, 8:8, 0:2, A:6, 0:24, 1:8,
        0:16, 8:8, 0:2, I:6, 0:24, 2:8, 0:16, 8:8, 0:2, E:6, 0:24>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_rakp1(Properties) ->
    Tag = proplists:get_value(message_tag, Properties),
    S = proplists:get_value(rs_session_id, Properties),
    R = proplists:get_value(rq_nonce, Properties),
    L = proplists:get_value(lookup_type, Properties),
    P = encode_privilege(proplists:get_value(privilege, Properties)),
    U = iolist_to_binary(proplists:get_value(user, Properties, <<>>)),
    <<Tag:8, 0:24, S:32/little, R:16/binary, 0:3, L:1, P:4, 0:16,
        (byte_size(U)):8, U/binary>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_rakp3(Properties) ->
    Tag = proplists:get_value(message_tag, Properties),
    Rs = proplists:get_value(rs_nonce, Properties),
    L = proplists:get_value(lookup_type, Properties),
    P = encode_privilege(proplists:get_value(privilege, Properties)),
    Sq = proplists:get_value(rq_session_id, Properties),
    Ss = proplists:get_value(rs_session_id, Properties),
    A = proplists:get_value(rakp_auth_type, Properties),
    U = iolist_to_binary(proplists:get_value(user, Properties, "")),
    K = eipmi_util:normalize(20, proplists:get_value(password, Properties)),
    ToHash = <<Rs/binary, Sq:32/little, L:4, P:4, (byte_size(U)):8, U/binary>>,
    Hmac = eipmi_auth:rakp_hash(A, rakp3, K, ToHash),
    <<Tag:8, 0:8, 0:16, Ss:32/little, Hmac/binary>>.

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
chassis_command(power_down) -> 0;
chassis_command(power_up) -> 1;
chassis_command(power_cycle) -> 2;
chassis_command(hard_reset) -> 3;
chassis_command(diagnostic_interrupt) -> 4;
chassis_command(acpi_shutdown) -> 5.

%%------------------------------------------------------------------------------
%% @doc
%% Encodes privilege level as an integer.
%% @end
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

get_encoded_bool(Prop, List) ->
    case proplists:get_bool(Prop, List) of
        true -> 1;
        false -> 0
    end.
