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

-module(eipmi_response_tests).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

decode_get_device_sdr_test() ->
    Resp = {?IPMI_NETFN_SENSOR_EVENT_RESPONSE, ?GET_DEVICE_SDR},
    Bin = <<16#22, 16#11, $d, $a, $t, $a>>,
    ?assertEqual(
        {ok, [
            {next_record_id, 16#1122},
            {data, <<$d, $a, $t, $a>>}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_reserve_device_sdr_repository_test() ->
    Resp = {?IPMI_NETFN_SENSOR_EVENT_RESPONSE, ?RESERVE_DEVICE_SDR_REPOSITORY},
    Bin = <<16#22, 16#11>>,
    ?assertEqual(
        {ok, [{reservation_id, 16#1122}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_sensor_reading_test() ->
    Resp = {?IPMI_NETFN_SENSOR_EVENT_RESPONSE, ?GET_SENSOR_READING},
    Bin = <<16#01, 2#11000000, 2#11110000, 2#10000001>>,
    ?assertEqual(
        {ok, [
            {events_enabled, true},
            {scanning_enabled, true},
            {raw_reading, <<16#01>>},
            {raw_states, <<2#1111000010000001:16>>}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_device_id_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_DEVICE_ID},
    Bin =
        <<16#00, 16#82, 16#02, 16#0d, 16#51, 16#3e, 16#78, 16#6c, 16#00, 16#03,
            16#0b, 16#00, 16#00, 16#00, 16#00>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual(0, proplists:get_value(device_id, Properties)),
    ?assertEqual(2, proplists:get_value(device_revision, Properties)),
    ?assertEqual(normal, proplists:get_value(operation, Properties)),
    ?assertEqual("2.13", proplists:get_value(firmware_version, Properties)),
    ?assertEqual("1.5", proplists:get_value(ipmi_version, Properties)),
    ?assertEqual(
        [event_generator, event_receiver, fru_inventory, sel, sdr],
        proplists:get_value(device_support, Properties)
    ),
    ?assertEqual(27768, proplists:get_value(manufacturer_id, Properties)),
    ?assertEqual(2819, proplists:get_value(product_id, Properties)).

decode_cold_reset_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?COLD_RESET},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_warm_reset_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?WARM_RESET},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_get_self_test_results_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_SELF_TEST_RESULTS},
    Bin = <<16#57, 16#ff>>,
    ?assertEqual(
        {ok, [
            {result, {
                {corrupted_devices, [sel, sdr, fru, ipmb_signal_lines]},
                {inaccessible_devices, [
                    sdr,
                    fru,
                    boot_firmware,
                    optional_firmware
                ]}
            }}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_acpi_power_state_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_ACPI_POWER_STATE},
    Bin = <<16#00, 16#01>>,
    ?assertEqual(
        {ok, [{system, {s0_g0, working}}, {device, d1}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_device_guid_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_DEVICE_GUID},
    Bin = <<$h, $e, $l, $l, $o>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual("hello", proplists:get_value(guid, Properties)).

decode_send_message_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?SEND_MESSAGE},
    Bin =
        <<16#20, 16#2c, 16#b4, 16#82, 16#60, 16#23, 16#00, 16#01, 16#00, 16#34,
            16#41, 16#fa, 16#14, 16#f0, 16#41, 16#46>>,
    ?assertEqual(
        {ok, [
            {next_record_id, 16#0001},
            {data, <<16#34, 16#41, 16#fa, 16#14, 16#f0, 16#41>>}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_system_guid_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_SYSTEM_GUID},
    Bin = <<$h, $e, $l, $l, $o>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual("hello", proplists:get_value(guid, Properties)).

decode_get_channel_authentication_capabilities_test() ->
    Resp =
        {?IPMI_NETFN_APPLICATION_RESPONSE,
            ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES},
    Bin = <<16#00, 16#17, 16#1f, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual(
        [pwd, md5, md2, none],
        proplists:get_value(auth_types, Properties)
    ),
    ?assertEqual(
        [non_null, null, anonymous],
        proplists:get_value(login_status, Properties)
    ).

decode_get_session_challenge_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_SESSION_CHALLENGE},
    Bin =
        <<16#44, 16#33, 16#22, 16#11, $h, $e, $l, $l, $o, $_, $w, $o, $r, $l,
            $d, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual(16#11223344, proplists:get_value(session_id, Properties)),
    ?assertEqual(
        <<
            $h,
            $e,
            $l,
            $l,
            $o,
            $_,
            $w,
            $o,
            $r,
            $l,
            $d,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00
        >>,
        proplists:get_value(challenge, Properties)
    ).

decode_activate_session_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?ACTIVATE_SESSION},
    Bin =
        <<16#00, 16#44, 16#33, 16#22, 16#11, 16#88, 16#77, 16#66, 16#55,
            16#04>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual(none, proplists:get_value(auth_type, Properties)),
    ?assertEqual(16#11223344, proplists:get_value(session_id, Properties)),
    ?assertEqual(16#55667788, proplists:get_value(inbound_seq_nr, Properties)),
    ?assertEqual(administrator, proplists:get_value(privilege, Properties)).

decode_set_session_privilege_level_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?SET_SESSION_PRIVILEGE_LEVEL},
    Bin = <<16#04>>,
    {ok, Properties} = eipmi_response:decode(Resp, Bin),
    ?assertEqual(administrator, proplists:get_value(privilege, Properties)).

decode_close_session_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?CLOSE_SESSION},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_get_fru_inventory_area_info_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?GET_FRU_INVENTORY_AREA_INFO},
    Bin = <<16#22, 16#11, 16#00>>,
    ?assertEqual(
        {ok, [{area_size, 16#1122}, {access, by_bytes}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_read_fru_data_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?READ_FRU_DATA},
    Bin = <<5, $h, $e, $l, $l, $o>>,
    ?assertEqual(
        {ok, [{count, 5}, {data, <<$h, $e, $l, $l, $o>>}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_sel_info_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?GET_SEL_INFO},
    Bin =
        <<16#51, 16#01, 16#00, 16#ff, 16#ff, 16#44, 16#33, 16#22, 16#11, 16#44,
            16#33, 16#22, 16#11, 16#8f>>,
    ?assertEqual(
        {ok, [
            {version, "15"},
            {entries, 1},
            {free_space, '65535b_or_more'},
            {most_recent_addition, 16#11223344},
            {most_recent_erase, 16#11223344},
            {overflow, true},
            {operations, [delete, partial_add, reserve, get_allocation_info]}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_reserve_sel_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?RESERVE_SEL},
    Bin = <<16#22, 16#11>>,
    ?assertEqual(
        {ok, [{reservation_id, 16#1122}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_sel_entry_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?GET_SEL_ENTRY},
    Bin = <<16#22, 16#11, $d, $a, $t, $a>>,
    ?assertEqual(
        {ok, [
            {next_record_id, 16#1122},
            {data, <<$d, $a, $t, $a>>}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_clear_sel_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?CLEAR_SEL},
    Bin = <<16#01>>,
    ?assertEqual(
        {ok, [{progress, completed}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_ip_udp_rmcp_statistics_test() ->
    Resp = {?IPMI_NETFN_TRANSPORT_RESPONSE, ?GET_IP_UDP_RMCP_STATISTICS},
    Bin =
        <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08, 16#09, 16#a,
            16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08>>,
    ?assertEqual(
        {ok, [
            {ip_packets_received, 16#0201},
            {ip_header_errors, 16#0403},
            {ip_address_errors, 16#0605},
            {ip_fragmented_packets_received, 16#0807},
            {ip_packets_transmitted, 16#0a09},
            {udp_packets_received, 16#0201},
            {udp_proxy_packets_received, 16#0605},
            {udp_proxy_packets_dropped, 16#0807},
            {rmcp_packets_received, 16#0403}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_lan_configuration_parameters_test() ->
    Resp = {?IPMI_NETFN_TRANSPORT_RESPONSE, ?GET_LAN_CONFIGURATION_PARAMETERS},
    Bin = <<16#11, $d, $a, $t, $a>>,
    ?assertEqual(
        {ok, [{revision, 17}, {data, <<$d, $a, $t, $a>>}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_sdr_repository_info_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?GET_SDR_REPOSITORY_INFO},
    Bin =
        <<16#51, 16#01, 16#00, 16#ff, 16#ff, 16#44, 16#33, 16#22, 16#11, 16#44,
            16#33, 16#22, 16#11, 16#8f>>,
    ?assertEqual(
        {ok, [
            {version, "15"},
            {entries, 1},
            {free_space, unspecified},
            {most_recent_addition, 16#11223344},
            {most_recent_erase, 16#11223344},
            {overflow, true},
            {operations, [delete, partial_add, reserve, get_allocation_info]}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_reserve_sdr_repository_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?RESERVE_SDR_REPOSITORY},
    Bin = <<16#22, 16#11>>,
    ?assertEqual(
        {ok, [{reservation_id, 16#1122}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_sdr_test() ->
    Resp = {?IPMI_NETFN_STORAGE_RESPONSE, ?GET_SDR},
    Bin = <<16#22, 16#11, $d, $a, $t, $a>>,
    ?assertEqual(
        {ok, [
            {next_record_id, 16#1122},
            {data, <<$d, $a, $t, $a>>}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_chassis_caps_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?GET_CHASSIS_CAPABILITIES},
    Bin = <<15:8, 27:8, 132:8, 65:8, 213:8, 40:8>>,
    ?assertEqual(
        {ok, [
            {interlock, true},
            {lockout, true},
            {diagnostic, true},
            {intrusion, true},
            {fru_address, 27},
            {sdr_address, 132},
            {sel_address, 65},
            {sm_address, 213},
            {bridge_address, 40}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_chassis_status_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?GET_CHASSIS_STATUS},
    Bin = <<127:8, 8:8, 0:8, 2#11110000>>,
    ?assertEqual(
        {ok, [
            {power_restore_policy, 3},
            {power_control_fault, 1},
            {power_fault, 1},
            {interlock_status, 1},
            {overload, 1},
            {power_status, 1},
            {last_power_reason, 8},
            {identify_supported, 0},
            {identify_status, 0},
            {fan_fault, 0},
            {drive_fault, 0},
            {lockout_active, 0},
            {intrusion_detection, 0},
            {disable_standby_allowed, 1},
            {disable_diagnostic_allowed, 1},
            {disable_reset_allowed, 1},
            {disable_power_off_allowed, 1},
            {standby_disabled, 0},
            {diagnostic_disabled, 0},
            {reset_disabled, 0},
            {power_off_disabled, 0}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_chassis_poh_counter_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?GET_POH_COUNTER},
    Bin = <<5:8, 200:8, 0:8, 0:8, 0:8>>,
    ?assertEqual(
        {ok, [
            {counter, 200},
            {minutes_per_count, 5}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_chassis_restart_cause_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?GET_SYSTEM_RESTART_CAUSE},
    Bin = <<6:8, 13:8>>,
    ?assertEqual(
        {ok, [
            {restart_cause, always_on_restore_policy},
            {channel, 13}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_chassis_set_boot_options_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?SET_SYSTEM_BOOT_OPTIONS},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_chassis_get_boot_options_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?GET_SYSTEM_BOOT_OPTIONS},
    Bin = <<1:8, 1:1, 5:7, 7:3, 0:7, 1:4, 0:6, 1:1, 0:19>>,
    ?assertEqual(
        {ok, [
            {valid, 1},
            {boot_flags_valid, 1},
            {persist, 1},
            {boot_type, 1},
            {clear_cmos, 0},
            {lock_keyboard, 0},
            {boot_device, pxe},
            {screen_blank, 0},
            {lock_reset, 0},
            {lock_power, 0},
            {bios_verbosity, 0},
            {progress_traps, 0},
            {password_bypass, 1},
            {lock_sleep, 0},
            {console_redirection, 0},
            {device_instance, 0}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_chassis_set_power_restore_policy_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?SET_POWER_RESTORE_POLICY},
    Bin = <<4:8>>,
    ?assertEqual(
        {ok, [
            {supports_always_on, 1},
            {supports_last_state, 0},
            {supports_always_off, 0}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_chassis_control_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?CHASSIS_CONTROL},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_chassis_identify_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?CHASSIS_IDENTIFY},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_chassis_reset_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?CHASSIS_RESET},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_set_chassis_caps_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?SET_CHASSIS_CAPABILITIES},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_chassis_set_front_panel_enables_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?SET_FRONT_PANEL_ENABLES},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_chassis_set_power_cycle_interval_test() ->
    Resp = {?IPMI_NETFN_CHASSIS_RESPONSE, ?SET_POWER_CYCLE_INTERVAL},
    Bin = <<>>,
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, Bin)).

decode_get_picmg_properties_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?GET_PICMG_PROPERTIES},
    Bin = <<16#00, 16#12, 16#05, 16#6>>,
    ?assertEqual(
        {ok, [
            {picmg_extension, "2.1"},
            {max_fru_id, 5},
            {ipmc_fru_id, 6}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_get_address_info_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?GET_ADDRESS_INFO},
    Bin = <<16#00, 16#01, 16#02, 16#00, 16#05, 16#05, 16#07, 16#00>>,
    ?assertEqual(
        {ok, [
            {mch_site_number, 1},
            {ipmb_address, 2},
            {fru_id, 5},
            {site_number, 5},
            {site_type, amc}
        ]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_set_fru_activation_policy_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?SET_FRU_ACTIVATION_POLICY},
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, <<16#00>>)).

decode_get_fru_activation_policy_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?GET_FRU_ACTIVATION_POLICY},
    ?assertEqual(
        {ok, [{deactivation_locked, true}, {locked, true}]},
        eipmi_response:decode(Resp, <<16#00, 16#03>>)
    ).

decode_set_fru_activation_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?SET_FRU_ACTIVATION},
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, <<16#00>>)).

decode_fru_control_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?FRU_CONTROL},
    ?assertEqual({ok, []}, eipmi_response:decode(Resp, <<16#00>>)).

decode_get_device_locator_record_id_test() ->
    Resp = {?IPMI_NETFN_PICMG_RESPONSE, ?GET_DEVICE_LOCATOR_RECORD_ID},
    Bin = <<16#00, 16#34, 16#12>>,
    ?assertEqual(
        {ok, [{record_id, 16#1234}]},
        eipmi_response:decode(Resp, Bin)
    ).

decode_oem_test() ->
    Resp = {16#2f, 16#01},
    Bin = <<$d, $a, $t, $a>>,
    ?assertEqual({ok, [{data, Bin}]}, eipmi_response:decode(Resp, Bin)).

decode_error_test() ->
    Resp = {?IPMI_NETFN_APPLICATION_RESPONSE, ?GET_DEVICE_ID},
    Bin = <<$f, $a, $i, $l>>,
    ?assertMatch({error, _}, eipmi_response:decode(Resp, Bin)).
