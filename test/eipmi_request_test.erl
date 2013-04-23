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

-module(eipmi_request_test).

-include_lib("eunit/include/eunit.hrl").

-include("eipmi.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

encode_get_device_sdr_test() ->
    Req = {?IPMI_NETFN_SENSOR_EVENT_REQUEST, ?GET_DEVICE_SDR},
    Properties = [{reservation_id, 16#1122},
                  {record_id, 16#3344},
                  {offset, 0},
                  {count, 5}],
    ?assertEqual(
       <<16#22, 16#11, 16#44, 16#33, 16#00, 16#05>>,
       eipmi_request:encode(Req, Properties)).

encode_reserve_device_sdr_repository_test() ->
    Req = {?IPMI_NETFN_SENSOR_EVENT_REQUEST, ?RESERVE_DEVICE_SDR_REPOSITORY},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_sensor_reading_test() ->
    Req = {?IPMI_NETFN_SENSOR_EVENT_REQUEST, ?GET_SENSOR_READING},
    ?assertEqual(<<16#03>>, eipmi_request:encode(Req, [{sensor_number, 3}])).

encode_get_device_id_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_DEVICE_ID},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_cold_reset_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?COLD_RESET},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_warm_reset_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?WARM_RESET},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_self_test_results_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SELF_TEST_RESULTS},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_acpi_power_state_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_ACPI_POWER_STATE},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_device_guid_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_DEVICE_GUID},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_system_guid_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SYSTEM_GUID},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_send_message_raw_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?SEND_MESSAGE},
    ?assertEqual(
       <<16#80, $h, $e, $l, $l, $o>>,
       eipmi_request:encode(Req, [{data, <<$h, $e, $l, $l, $o>>}])).

encode_send_message_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?SEND_MESSAGE},
    Embedded = [{rs_addr, 16#82},
                {rs_lun, 0},
                {rq_addr, 16#20},
                {rq_lun, 0},
                {rq_seq_nr, 16#22},
                {reservation_id, 16#099b},
                {record_id, 16#0001},
                {offset, 25},
                {count, 6}],
    Properties = [{net_fn, ?IPMI_NETFN_STORAGE_REQUEST},
                  {cmd, ?GET_SDR},
                  {data, Embedded}],
    ?assertEqual(
       <<16#00, 16#82, 16#28, 16#56, 16#20, 16#88, 16#23, 16#9b,
         16#09, 16#01, 16#00, 16#19, 16#06, 16#71>>,
       eipmi_request:encode(Req, Properties)).

encode_get_channel_authentication_capabilities_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_CHANNEL_AUTHENTICATION_CAPABILITIES},
    Properties = [{privilege, administrator}],
    ?assertEqual(<<16#0e, 16#04>>, eipmi_request:encode(Req, Properties)).

encode_get_session_challenge_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SESSION_CHALLENGE},
    Properties = [{auth_type, none}, {user, "hello_world"}],
    ?assertEqual(
       <<16#00, $h, $e, $l, $l, $o, $_, $w, $o, $r, $l, $d,
         16#00, 16#00, 16#00, 16#00, 16#00>>,
       eipmi_request:encode(Req, Properties)).

encode_activate_session_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?ACTIVATE_SESSION},
    Properties = [{auth_type, none},
                  {privilege, administrator},
                  {initial_outbound_seq_nr, 16#11223344},
                  {challenge, <<$h, $e, $l, $l, $o, 16#00, 16#00, 16#00, 16#00,
                                16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>}],
    ?assertEqual(
       <<16#00, 16#04, $h, $e, $l, $l, $o, 16#00, 16#00, 16#00, 16#00, 16#00,
         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#44, 16#33, 16#22, 16#11>>,
       eipmi_request:encode(Req, Properties)).

encode_set_session_privilege_level_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?SET_SESSION_PRIVILEGE_LEVEL},
    Properties = [{privilege, administrator}],
    ?assertEqual(<<16#04>>, eipmi_request:encode(Req, Properties)).

encode_close_session_test() ->
    Req = {?IPMI_NETFN_APPLICATION_REQUEST, ?CLOSE_SESSION},
    Properties = [{session_id, 16#11223344}],
    ?assertEqual(
       <<16#44, 16#33, 16#22, 16#11>>,
       eipmi_request:encode(Req, Properties)).

encode_get_fru_inventory_area_info_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_FRU_INVENTORY_AREA_INFO},
    Properties = [{fru_id, 4}],
    ?assertEqual(<<16#04>>, eipmi_request:encode(Req, Properties)).

encode_read_fru_data_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?READ_FRU_DATA},
    Properties = [{fru_id, 4}, {offset, 16#1122}, {count, 5}],
    ?assertEqual(
       <<16#04, 16#22, 16#11, 16#05>>,
       eipmi_request:encode(Req, Properties)).

encode_get_sel_info_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SEL_INFO},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_reserve_sel_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?RESERVE_SEL},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_sel_entry_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SEL_ENTRY},
    ?assertEqual(
       <<16#00, 16#00, 16#22, 16#11, 16#00, 16#ff>>,
       eipmi_request:encode(Req, [{record_id, 16#1122}])).

encode_clear_sel_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?CLEAR_SEL},
    ?assertEqual(
       <<16#22, 16#11, $C, $L, $R, 16#aa>>,
       eipmi_request:encode(Req, [{reservation_id, 16#1122}, initiate])).

encode_get_ip_udp_rmcp_statistics_test() ->
    Req = {?IPMI_NETFN_TRANSPORT_REQUEST, ?GET_IP_UDP_RMCP_STATISTICS},
    Properties = [{clear_statistics, true}],
    ?assertEqual(<<16#0e, 16#01>>, eipmi_request:encode(Req, Properties)).

encode_get_lan_configuration_parameters_test() ->
    Req = {?IPMI_NETFN_TRANSPORT_REQUEST, ?GET_LAN_CONFIGURATION_PARAMETERS},
    Properties = [{parameter, 3}],
    ?assertEqual(
       <<16#8e, 16#03, 16#00, 16#00>>,
       eipmi_request:encode(Req, Properties)).

encode_get_sdr_repository_info_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR_REPOSITORY_INFO},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_reserve_sdr_repository_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?RESERVE_SDR_REPOSITORY},
    ?assertEqual(<<>>, eipmi_request:encode(Req, [])).

encode_get_sdr_test() ->
    Req = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR},
    Properties = [{reservation_id, 16#1122},
                  {record_id, 16#3344},
                  {offset, 0},
                  {count, 5}],
    ?assertEqual(
       <<16#22, 16#11, 16#44, 16#33, 16#00, 16#05>>,
       eipmi_request:encode(Req, Properties)).

encode_get_picmg_properties_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_PICMG_PROPERTIES},
    ?assertEqual(<<16#00>>, eipmi_request:encode(Req, [])).

encode_get_address_info_0_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO},
    ?assertEqual(<<16#00>>, eipmi_request:encode(Req, [])).

encode_get_address_info_1_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO},
    ?assertEqual(<<16#00, 16#05>>, eipmi_request:encode(Req, [{fru_id, 5}])).

encode_get_address_info_2_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO},
    ?assertEqual(
       <<16#00, 16#00, 16#03, 16#05, 16#07>>,
       eipmi_request:encode(Req, [{site_type, amc}, {site_number, 5}])).

encode_set_fru_activation_policy_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?SET_FRU_ACTIVATION_POLICY},
    Properties = [{fru_id, 5}, {deactivation_locked, true}, {locked, false}],
    ?assertEqual(
       <<16#00, 16#05, 16#3, 16#2>>,
       eipmi_request:encode(Req, Properties)).

encode_get_fru_activation_policy_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_FRU_ACTIVATION_POLICY},
    Properties = [{fru_id, 5}],
    ?assertEqual(<<16#00, 16#05>>, eipmi_request:encode(Req, Properties)).

encode_set_fru_activation_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?SET_FRU_ACTIVATION},
    Properties = [{fru_id, 5}, {activate, true}],
    ?assertEqual(
       <<16#00, 16#05, 16#01>>,
       eipmi_request:encode(Req, Properties)).

encode_fru_control_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?FRU_CONTROL},
    Properties = [{fru_id, 5}, {control, graceful_reboot}],
    ?assertEqual(
       <<16#00, 16#05, 16#02>>,
       eipmi_request:encode(Req, Properties)).

encode_get_device_locator_record_id_test() ->
    Req = {?IPMI_NETFN_PICMG_REQUEST, ?GET_DEVICE_LOCATOR_RECORD_ID},
    Properties = [{fru_id, 5}],
    ?assertEqual(<<16#00, 16#05>>, eipmi_request:encode(Req, Properties)).
