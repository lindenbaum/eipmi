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
%%% responses. This module will need care if support for new responses is
%%% demanded.
%%% @end
%%%=============================================================================

-module(eipmi_response).

-export([decode/2,
         get_device_support/1,
         get_picmg_site_type/1,
         decode_lan_configuration_parameters/3]).

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
                    {ok, proplists:proplist()} | {error, term()}.
decode({NetFn, Cmd}, Data) ->
    try decode(NetFn, Cmd, Data)
    catch
        C:E -> {error, {C, E}}
    end.
decode(?IPMI_NETFN_SENSOR_EVENT_RESPONSE, Cmd, Data) ->
    {ok, decode_sensor_event(Cmd, Data)};
decode(?IPMI_NETFN_APPLICATION_RESPONSE, Cmd, Data) ->
    decode_application(Cmd, Data);
decode(?IPMI_NETFN_STORAGE_RESPONSE, Cmd, Data) ->
    {ok, decode_storage(Cmd, Data)};
decode(?IPMI_NETFN_TRANSPORT_RESPONSE, Cmd, Data) ->
    {ok, decode_transport(Cmd, Data)};
decode(?IPMI_NETFN_CHASSIS_RESPONSE, Cmd, Data) ->
    {ok, decode_chassis(Cmd, Data)};
decode(?IPMI_NETFN_PICMG_RESPONSE, Cmd, Data) ->
    {ok, decode_picmg(Cmd, Data)};
decode(NetFn, Cmd, Data) when NetFn >= 16#2f ->
    {ok, decode_oem(NetFn, Cmd, Data)}.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of supported capabilities of a device (decoded from integer).
%% @end
%%------------------------------------------------------------------------------
-spec get_device_support(non_neg_integer()) -> [atom()].
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
%% @doc
%% Returns the site PICMG specific site type for an integer code.
%% @end
%%------------------------------------------------------------------------------
get_picmg_site_type(16#00) -> [{site_type, picmg_board}];
get_picmg_site_type(16#01) -> [{site_type, power_entry}];
get_picmg_site_type(16#02) -> [{site_type, shelf_fru_information}];
get_picmg_site_type(16#03) -> [{site_type, dedicated_shelf_management_controller}];
get_picmg_site_type(16#04) -> [{site_type, fan_tray}];
get_picmg_site_type(16#05) -> [{site_type, fan_filter_tray}];
get_picmg_site_type(16#06) -> [{site_type, alarm}];
get_picmg_site_type(16#07) -> [{site_type, amc}];
get_picmg_site_type(16#08) -> [{site_type, pmc}];
get_picmg_site_type(16#09) -> [{site_type, rear_transition_module}];
get_picmg_site_type(16#0a) -> [{site_type, mch}];
get_picmg_site_type(16#0b) -> [{site_type, power_module}];
get_picmg_site_type(_) -> [].

%%------------------------------------------------------------------------------
%% @doc
%% Decode the given binary according to IPMI 2.0 Table 23.
%% @end
%%------------------------------------------------------------------------------
-spec decode_lan_configuration_parameters(pos_integer(),
                                          non_neg_integer(),
                                          binary()) ->
          eipmi:lan_configurations().
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_SET_IN_PROGRESS,
  _,
  <<?EIPMI_RESERVED:6, S:2, _/binary>>) ->
    case S of
        0 -> [{set_in_progress, set_complete}];
        1 -> [{set_in_progress, set_in_progress}];
        2 -> [{set_in_progress, commit_write}];
        _ -> []
    end;
decode_lan_configuration_parameters(
 ?IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE,
  _,
  <<?EIPMI_RESERVED:2, A:6, _/binary>>) ->
    [{auth_types, get_auth_types(A)}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE_ENABLES,
  _,
  <<?EIPMI_RESERVED:2, A:6,
    ?EIPMI_RESERVED:2, B:6,
    ?EIPMI_RESERVED:2, C:6,
    ?EIPMI_RESERVED:2, D:6,
    ?EIPMI_RESERVED:2, E:6,
    _/binary>>) ->
    [{callback_level_auth_types, get_auth_types(A)},
     {user_level_auth_types, get_auth_types(B)},
     {operator_level_auth_types, get_auth_types(C)},
     {administrator_level_auth_types, get_auth_types(D)},
     {oem_level_auth_types, get_auth_types(E)}];
decode_lan_configuration_parameters(
 ?IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS,
  _,
  <<I1, I2, I3, I4, _/binary>>) ->
    [{ip_address, {I1, I2, I3, I4}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS_SOURCE,
  _,
  <<?EIPMI_RESERVED:4, S:4, _/binary>>) ->
    case S of
        1 -> [{ip_assignment, static}];
        2 -> [{ip_assignment, dhcp}];
        3 -> [{ip_assignment, bios_or_system}];
        4 -> [{ip_assignment, other}];
        _ -> []
    end;
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_MAC_ADDRESS,
  _,
  <<M1, M2, M3, M4, M5, M6, _/binary>>) ->
    [{mac_address, {M1, M2, M3, M4, M5, M6}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_SUBNET_MASK,
  _,
  <<I1, I2, I3, I4, _/binary>>) ->
    [{subnet_mask, {I1, I2, I3, I4}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_IPV4_HEADER_PARAMETERS,
  _,
  <<Ttl, _, Prec:4, Tos:3, ?EIPMI_RESERVED:1, _/binary>>) ->
    [{ttl, Ttl}, {precendence, Prec}, {type_of_service, Tos}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_PRIMARY_RMCP_PORT,
  _,
  <<P:16/little, _/binary>>) ->
    [{primary_port, P}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_SECONDARY_RMCP_PORT,
  _,
  <<P:16/little, _/binary>>) ->
    [{secondary_port, P}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY,
  _,
  <<I1, I2, I3, I4, _/binary>>) ->
    [{default_gateway, {I1, I2, I3, I4}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS,
  _,
  <<M1, M2, M3, M4, M5, M6, _/binary>>) ->
    [{default_gateway_mac_address, {M1, M2, M3, M4, M5, M6}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY,
  _,
  <<I1, I2, I3, I4, _/binary>>) ->
    [{backup_gateway, {I1, I2, I3, I4}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS,
  _,
  <<M1, M2, M3, M4, M5, M6, _/binary>>) ->
    [{backup_gateway_mac_address, {M1, M2, M3, M4, M5, M6}}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_COMMUNITY_STRING,
  _,
  Binary) ->
    [{community, string:strip(binary_to_list(Binary), right, $\0)}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS,
  _,
  <<?EIPMI_RESERVED:4, Num:4, _/binary>>) ->
    [{num_destinations, Num}];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_TYPE,
  _,
  <<?EIPMI_RESERVED:4, Sel:4, Ack:1, ?EIPMI_RESERVED:4, Type:3,
    Timeout:8, ?EIPMI_RESERVED:5, Retries:3, _/binary>>) ->
    [{set, Sel},
     {acknowledge, eipmi_util:get_bool(Ack)},
     {timeout, Timeout},
     {retries, Retries}
     | case Type of
           0 -> [{destination_type, trap}];
           6 -> [{destination_type, oem1}];
           7 -> [{destination_type, oem2}];
           _ -> []
       end];
decode_lan_configuration_parameters(
  ?IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_ADDRESSES,
  _,
  <<?EIPMI_RESERVED:4, Sel:4, 0:4, ?EIPMI_RESERVED:4,
    ?EIPMI_RESERVED:7, Gw:1, I1, I2, I3, I4,
    M1, M2, M3, M4, M5, M6, _/binary>>) ->
    [{set, Sel},
     {gateway, case Gw of 0 -> default; 1 -> backup end},
     {ip_address, {I1, I2, I3, I4}},
     {mac_address, {M1, M2, M3, M4, M5, M6}}];
decode_lan_configuration_parameters(_, _, _) ->
    [].

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_sensor_event(?GET_DEVICE_SDR, <<Next:16/little, Data/binary>>) ->
    [{next_record_id, Next}, {data, Data}];
decode_sensor_event(?RESERVE_DEVICE_SDR_REPOSITORY, <<Reservation:16/little>>) ->
    [{reservation_id, Reservation}];
decode_sensor_event(?GET_SENSOR_READING,
                    <<Reading:1/binary, Events:1, Scanning:1, Available:1,
                      ?EIPMI_RESERVED:5, States/binary>>) ->
    [{events_enabled, eipmi_util:get_bool(Events)},
     {scanning_enabled, eipmi_util:get_bool(Scanning)}]
        ++ case Available of 0 -> [{raw_reading, Reading}]; _ -> [] end
        ++ case Available of 0 -> [{raw_states, States}]; _ -> [] end;
decode_sensor_event(?PET_ACKNOWLEDGE, _) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_application(?GET_DEVICE_ID,
                   <<Id:8, _:1, ?EIPMI_RESERVED:3, Revision:4,
                     Operation:1, Major:7, Minor:8, IPMIVersion:1/binary,
                     Support:8, Manufacterer:24/little,
                     Product:16/little, _/binary>>) ->
    [Iv1 | IvRest] = lists:reverse(eipmi_util:from_bcd_plus(IPMIVersion)),
    {ok, [{device_id, Id},
          {device_revision, Revision},
          {operation, case Operation of 0 -> normal; 1 -> progress end},
          {firmware_version, eipmi_util:format("~B.~B", [Major, Minor])},
          {ipmi_version, [Iv1 | [$. | IvRest]]},
          {device_support, get_device_support(Support)},
          {manufacturer_id, Manufacterer},
          {product_id, Product}]};
decode_application(?COLD_RESET, _) ->
    {ok, []};
decode_application(?WARM_RESET, _) ->
    {ok, []};
decode_application(?GET_SELF_TEST_RESULTS, <<Result:8, Detail:8>>) ->
    {ok, [{result, get_self_test_result(Result, Detail)}]};
decode_application(?GET_ACPI_POWER_STATE, <<_:1, System:7, _:1, Device:7>>) ->
    {ok, [{system, get_system_power_state(System)},
          {device, get_device_power_state(Device)}]};
decode_application(?GET_DEVICE_GUID, <<GUID/binary>>) ->
    {ok, [{guid, eipmi_util:binary_to_string(GUID)}]};
decode_application(?SEND_MESSAGE, <<Binary/binary>>) ->
    case eipmi_decoder:response(Binary) of
        {ok, #rmcp_ipmi{properties = Ps, cmd = Cmd, data = Data}} ->
            case proplists:get_value(completion, Ps) of
                normal ->
                    decode(Cmd, Data);
                Error ->
                    {error, {slave_error, Error}}
            end;
        Error ->
            Error
    end;
decode_application(?GET_SYSTEM_GUID, <<GUID/binary>>) ->
    {ok, [{guid, eipmi_util:binary_to_string(GUID)}]};
decode_application(?GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
                   <<Channel:8, 0:1, ?EIPMI_RESERVED:1, A:6,
                     ?EIPMI_RESERVED:3, P:1, U:1, L:3,
                     ?EIPMI_RESERVED:40>>) ->
    {ok, [{channel, Channel},
          {auth_types, get_auth_types(A)},
          {per_message_authentication_enabled, not eipmi_util:get_bool(P)},
          {user_level_authentication_enabled, not eipmi_util:get_bool(U)},
          {login_status, get_login_status(L)}]};
decode_application(?GET_SESSION_CHALLENGE, <<I:32/little, C/binary>>) ->
    {ok, [{session_id, I}, {challenge, C}]};
decode_application(?ACTIVATE_SESSION,
                   <<?EIPMI_RESERVED:4, A:4, I:32/little, S:32/little,
                     ?EIPMI_RESERVED:4, P:4>>) ->
    {ok, [{session_id, I},
          {inbound_seq_nr, S},
          {auth_type, eipmi_auth:decode_type(A)},
          {privilege, decode_privilege(P)}]};
decode_application(?SET_SESSION_PRIVILEGE_LEVEL, <<?EIPMI_RESERVED:4, P:4>>) ->
    {ok, [{privilege, decode_privilege(P)}]};
decode_application(?CLOSE_SESSION, _) ->
    {ok, []}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_storage(?GET_FRU_INVENTORY_AREA_INFO,
               <<AreaSize:16/little, ?EIPMI_RESERVED:7, Access:1>>) ->
    [{area_size, AreaSize},
     {access, case Access of 1 -> by_words; 0 -> by_bytes end}];
decode_storage(?READ_FRU_DATA, <<Count:8, Data/binary>>) ->
    [{count, Count}, {data, Data}];
decode_storage(Cmd,
               <<Version:1/binary, Entries:16/little, Free:16/little,
                 Addition:32/little, Erase:32/little,
                 Overflow:1, _:3, Operations:4>>)
  when Cmd =:= ?GET_SEL_INFO orelse Cmd =:= ?GET_SDR_REPOSITORY_INFO ->
    [{version, lists:reverse(eipmi_util:from_bcd_plus(Version))},
     {entries, Entries},
     {free_space, get_free_space(Cmd, Free)},
     {most_recent_addition, Addition},
     {most_recent_erase, Erase},
     {overflow, case Overflow of 1 -> true; 0 -> false end},
     {operations, get_operations(Operations)}];
decode_storage(Cmd, <<Reservation:16/little>>)
  when Cmd =:= ?RESERVE_SEL orelse Cmd =:= ?RESERVE_SDR_REPOSITORY ->
    [{reservation_id, Reservation}];
decode_storage(Cmd, <<Next:16/little, Data/binary>>)
  when Cmd =:= ?GET_SEL_ENTRY orelse Cmd =:= ?GET_SDR ->
    [{next_record_id, Next}, {data, Data}];
decode_storage(?CLEAR_SEL, <<?EIPMI_RESERVED:4, Progress:4>>) ->
    [{progress, case Progress of 1 -> completed; 0 -> in_progress end}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_transport(?GET_IP_UDP_RMCP_STATISTICS,
                 <<IPRx:16/little, IPRxHdErr:16/little, IPRxAddrErr:16/little,
                   IPRxFrag:16/little,IPTx:16/little, UDPRx:16/little,
                   RMCPRx:16/little, UDPRxProxy:16/little, UDPDr:16/little>>) ->
    [{ip_packets_received, IPRx},
     {ip_header_errors, IPRxHdErr},
     {ip_address_errors, IPRxAddrErr},
     {ip_fragmented_packets_received, IPRxFrag},
     {ip_packets_transmitted, IPTx},
     {udp_packets_received, UDPRx},
     {udp_proxy_packets_received, UDPRxProxy},
     {udp_proxy_packets_dropped, UDPDr},
     {rmcp_packets_received, RMCPRx}];
decode_transport(?SET_LAN_CONFIGURATION_PARAMETERS, _) ->
    [];
decode_transport(?GET_LAN_CONFIGURATION_PARAMETERS, <<Rev:8, Data/binary>>) ->
    [{revision, Rev}, {data, Data}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_chassis(?GET_CHASSIS_CAPABILITIES, <<?EIPMI_RESERVED:4, L:1, D:1, F:1,
                                            I:1, Fru:8, Sdr: 8, Sel:8, Sm:8,
                                            Rest/binary>>) ->
    Caps = case Rest of
            <<>> ->
                [];
            <<Bridge:8>> ->
               [{bridge_address, Bridge}]
           end,
    [{interlock, case L of 1 -> true; 0 -> false end},
     {lockout, case F of 1 -> true; 0 -> false end},
     {diagnostic, case D of 1 -> true; 0 -> false end},
     {intrusion, case I of 1 -> true; 0 -> false end},
     {fru_address, Fru},
     {sdr_address, Sdr},
     {sel_address, Sel},
     {sm_address, Sm}
     | Caps];
decode_chassis(?GET_CHASSIS_STATUS, <<?EIPMI_RESERVED:1, Policy:2,
                                      ControlFault:1, Fault:1, Interlock:1,
                                      Overload:1, OnOff:1,
                                      LastPowerRsn:8,
                                      ?EIPMI_RESERVED:1, Identifies:1,
                                      Identifying:2, FanFault:1, DriveFault:1,
                                      PanelLockout:1, Intrusion:1, Rest/binary>>)
->
    Front = case Rest of
                <<>> ->
                    [];
                <<DisableStandby:1, DisableDiagInterrupt:1, DisableReset:1,
                  DisablePowerOff:1, StandbyDisabled:1,
                  DiagInterruptDisabled:1, ResetDisabled:1,
                  PowerOffDisabled:1>> ->
                    [{disable_standby_allowed, DisableStandby},
                     {disable_diagnostic_allowed, DisableDiagInterrupt},
                     {disable_reset_allowed, DisableReset},
                     {disable_power_off_allowed, DisablePowerOff},
                     {standby_disabled, StandbyDisabled},
                     {diagnostic_disabled, DiagInterruptDisabled},
                     {reset_disabled, ResetDisabled},
                     {power_off_disabled, PowerOffDisabled}]
            end,
    [{power_restore_policy, Policy},
     {power_control_fault, ControlFault},
     {power_fault, Fault},
     {interlock_status, Interlock},
     {overload, Overload},
     {power_status, OnOff},
     {last_power_reason, LastPowerRsn},
     {identify_supported, Identifies},
     {identify_status, Identifying},
     {fan_fault, FanFault},
     {drive_fault, DriveFault},
     {lockout_active, PanelLockout},
     {intrusion_detection, Intrusion}
     | Front];
decode_chassis(?GET_POH_COUNTER, <<MinPerCount:8, Count:32/little>>) ->
    [{counter, Count},
     {minutes_per_count, MinPerCount}];
decode_chassis(?GET_SYSTEM_RESTART_CAUSE, <<?EIPMI_RESERVED:4, Cause:4,
                                            Channel:8>>) ->
    [{restart_cause, decode_restart_cause(Cause)},
     {channel, Channel}];
decode_chassis(?SET_POWER_RESTORE_POLICY, <<?EIPMI_RESERVED:5, PowerUp:1,
                                            LastState:1, PowerOff:1>>) ->
    [{supports_always_on, PowerUp},
     {supports_last_state, LastState},
     {supports_always_off, PowerOff}];
decode_chassis(Cmd, _)
  when Cmd =:= ?CHASSIS_CONTROL orelse
       Cmd =:= ?CHASSIS_IDENTIFY orelse
       Cmd =:= ?CHASSIS_RESET orelse
       Cmd =:= ?SET_CHASSIS_CAPABILITIES orelse
       Cmd =:= ?SET_FRONT_PANEL_ENABLES orelse
       Cmd =:= ?SET_POWER_CYCLE_INTERVAL ->
    [].

%%------------------------------------------------------------------------------
%% @private
%% we've seen error prone implementation that do not include the PICMG
%% identifier into the reponse.
%%------------------------------------------------------------------------------
decode_picmg(?GET_PICMG_PROPERTIES, <<?PICMG_ID:8, V:1/binary, M:8, I:8>>) ->
    decode_picmg_(?GET_PICMG_PROPERTIES, [V, M, I]);
decode_picmg(?GET_PICMG_PROPERTIES, <<V:1/binary, M:8, I:8>>) ->
    decode_picmg_(?GET_PICMG_PROPERTIES, [V, M, I]);
decode_picmg(?GET_ADDRESS_INFO,
             <<?PICMG_ID:8, M:8, I:8, ?EIPMI_RESERVED:8, F:8, N:8, T:8, C:8>>) ->
    decode_picmg_(?GET_ADDRESS_INFO, [M, I, F, N, T, C]);
decode_picmg(?GET_ADDRESS_INFO,
             <<M:8, I:8, ?EIPMI_RESERVED:8, F:8, N:8, T:8, C:8>>) ->
    decode_picmg_(?GET_ADDRESS_INFO, [M, I, F, N, T, C]);
decode_picmg(?SET_FRU_ACTIVATION_POLICY, _) ->
    [];
decode_picmg(?GET_FRU_ACTIVATION_POLICY,
             <<?PICMG_ID:8, ?EIPMI_RESERVED:6, D:1, L:1>>) ->
    decode_picmg_(?GET_FRU_ACTIVATION_POLICY, [D, L]);
decode_picmg(?GET_FRU_ACTIVATION_POLICY, <<?EIPMI_RESERVED:6, D:1, L:1>>) ->
    decode_picmg_(?GET_FRU_ACTIVATION_POLICY, [D, L]);
decode_picmg(?SET_FRU_ACTIVATION, _) ->
    [];
decode_picmg(?FRU_CONTROL, _) ->
    [];
decode_picmg(?GET_DEVICE_LOCATOR_RECORD_ID, <<?PICMG_ID:8, I:16/little>>) ->
    decode_picmg_(?GET_DEVICE_LOCATOR_RECORD_ID, [I]);
decode_picmg(?GET_DEVICE_LOCATOR_RECORD_ID, <<I:16/little>>) ->
    decode_picmg_(?GET_DEVICE_LOCATOR_RECORD_ID, [I]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_picmg_(?GET_PICMG_PROPERTIES, [Version, MaxFruId, IPMCFruId]) ->
    [Major | Minor] = lists:reverse(eipmi_util:from_bcd_plus(Version)),
    [{picmg_extension, [Major | [$. | Minor]]},
     {max_fru_id, MaxFruId},
     {ipmc_fru_id, IPMCFruId}];
decode_picmg_(?GET_ADDRESS_INFO, [MCH, Addr, FruId, SiteN, SiteT, Carrier]) ->
    [{mch_site_number, MCH}, {ipmb_address, Addr}]
        ++ case FruId of 16#ff -> []; _ -> [{fru_id, FruId}] end
        ++ case SiteN of 0 -> []; _ -> [{site_number, SiteN}] end
        ++ case SiteT of 16#ff -> []; _ -> get_picmg_site_type(SiteT) end
        ++ case Carrier of 0 -> []; _ -> [{carrier_number, Carrier}] end;
decode_picmg_(?GET_FRU_ACTIVATION_POLICY, [Deactivation, Locked]) ->
    [{deactivation_locked, eipmi_util:get_bool(Deactivation)},
     {locked, eipmi_util:get_bool(Locked)}];
decode_picmg_(?GET_DEVICE_LOCATOR_RECORD_ID, [Id]) ->
    [{record_id, Id}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_oem(_, _, Data) -> [{data, Data}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_self_test_result(16#55, 16#00) ->
    self_tests_passed;
get_self_test_result(16#56, 16#00) ->
    self_tests_not_implemented;
get_self_test_result(16#57, Bitfield) ->
    A = case Bitfield band 2#10000000 of 2#10000000 -> [sel]; _ -> [] end,
    B = case Bitfield band 2#1000000 of 2#1000000 -> [sdr]; _ -> [] end,
    C = case Bitfield band 2#100000 of 2#100000 -> [fru]; _ -> [] end,
    D = case Bitfield band 2#10000 of 2#10000 -> [ipmb_signal_lines]; _ -> [] end,
    E = case Bitfield band 2#1000 of 2#1000 -> [sdr]; _ -> [] end,
    F = case Bitfield band 2#100 of 2#100 -> [fru]; _ -> [] end,
    G = case Bitfield band 2#10 of 2#10 -> [boot_firmware]; _ -> [] end,
    H = case Bitfield band 2#1 of 2#1 -> [optional_firmware]; _ -> [] end,
    {{corrupted_devices, A ++ B ++ C ++ D},
     {inaccessible_devices, E ++ F ++ G ++ H}};
get_self_test_result(16#58, Detail) ->
    {fatal_hardware_error, Detail};
get_self_test_result(Result, Detail) ->
    {device_specific_error, Result, Detail}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_system_power_state(16#00) -> {s0_g0, working};
get_system_power_state(16#01) -> {s1, clocks_stopped};
get_system_power_state(16#02) -> {s2, clocks_stopped};
get_system_power_state(16#03) -> {s3, suspend_to_ram};
get_system_power_state(16#04) -> {s4, suspend_to_disk};
get_system_power_state(16#05) -> {s5_g2, soft_off};
get_system_power_state(16#06) -> {s4_s5, soft_off};
get_system_power_state(16#07) -> {g3, mechanical_off};
get_system_power_state(16#08) -> {s1_s2_s3, sleeping};
get_system_power_state(16#09) -> {g1, sleeping};
get_system_power_state(16#0a) -> {s5, override};
get_system_power_state(16#20) -> legacy_on;
get_system_power_state(16#21) -> legacy_off;
get_system_power_state(_) -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_device_power_state(16#00) -> d0;
get_device_power_state(16#01) -> d1;
get_device_power_state(16#02) -> d2;
get_device_power_state(16#03) -> d3;
get_device_power_state(_) -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_auth_types(AuthTypes) ->
    A = case AuthTypes band 2#100000 of 2#100000 -> [oem]; _ -> [] end,
    B = case AuthTypes band 2#10000 of 2#10000 -> [pwd]; _ -> [] end,
    C = case AuthTypes band 2#100 of 2#100 -> [md5]; _ -> [] end,
    D = case AuthTypes band 2#10 of 2#10 -> [md2]; _ -> [] end,
    E = case AuthTypes band 2#1 of 2#1 -> [none]; _ -> [] end,
    A ++ B ++ C ++ D ++ E.

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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_restart_cause(0) -> unknown;
decode_restart_cause(1) -> control_command;
decode_restart_cause(2) -> reset_button;
decode_restart_cause(3) -> power_button;
decode_restart_cause(4) -> watchdog_expired;
decode_restart_cause(5) -> oem;
decode_restart_cause(6) -> always_on_restore_policy;
decode_restart_cause(7) -> last_state_restore_policy;
decode_restart_cause(8) -> pef_reset;
decode_restart_cause(9) -> pef_power_cycle;
decode_restart_cause(10) -> soft_reset;
decode_restart_cause(11) -> clock_wakeup;
decode_restart_cause(_) -> reserved.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_free_space(?GET_SEL_INFO, 16#ffff) ->
    '65535b_or_more';
get_free_space(?GET_SEL_INFO, Free) ->
    {Free, b};
get_free_space(?GET_SDR_REPOSITORY_INFO, 16#ffff) ->
    unspecified;
get_free_space(?GET_SDR_REPOSITORY_INFO, 16#fffe) ->
    '64kb_or_more';
get_free_space(?GET_SDR_REPOSITORY_INFO, Free) ->
    {Free, b}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_operations(Operations) ->
    A = case Operations band 2#1000 of 2#1000 -> [delete]; _ -> [] end,
    B = case Operations band 2#100 of 2#100 -> [partial_add]; _ -> [] end,
    C = case Operations band 2#10 of 2#10 -> [reserve]; _ -> [] end,
    D = case Operations band 2#1 of 2#1 -> [get_allocation_info]; _ -> [] end,
    A ++ B ++ C ++ D.
