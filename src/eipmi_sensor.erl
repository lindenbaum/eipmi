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
%%% Sensor-related functions, e.g. mapping of reading/event type codes/offsets,
%%% sensor name retrieval, etc.
%%% TODOs:
%%% * decode threshold based sensor readings
%%% * decode oem based sensor readings
%%% * populate property type
%%% @end
%%%=============================================================================

-module(eipmi_sensor).

-export([get_reading/2,
         get_entity/1,
         decode_event_data/3,
         decode_addr/1]).

-include("eipmi.hrl").

-type property() :: term(). %% TODO

-export_type([property/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve the sensor category and reading type from the given sensor and
%% reading type. See sections 41 and 42 of the IPMI specification for more
%% information.
%% @end
%%------------------------------------------------------------------------------
get_reading(16#01, _SensorType) ->
    {threshold, {threshold, 16#01}};
get_reading(ReadingType, _SensorType)
  when ReadingType >= 16#02 andalso ReadingType =< 16#0c ->
    {discrete, {generic, ReadingType}};
get_reading(16#6f, SensorType) ->
    {discrete, {specific, SensorType}};
get_reading(ReadingType, SensorType)
  when ReadingType >= 16#70 andalso ReadingType =< 16#7f ->
    {oem, {ReadingType, SensorType}}.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the human readable entity for a given entity id according to
%% section 43.14 from the IPMI specification.
%% @end
%%------------------------------------------------------------------------------
get_entity(16#00) -> unspecified;
get_entity(16#01) -> other;
get_entity(16#02) -> unknown;
get_entity(16#03) -> processor;
get_entity(16#04) -> disk_or_disk_bay;
get_entity(16#05) -> peripheral_bay;
get_entity(16#06) -> system_management_module;
get_entity(16#07) -> system_board;
get_entity(16#08) -> memory_module;
get_entity(16#09) -> processor_module;
get_entity(16#0a) -> power_supply;
get_entity(16#0b) -> add_in_card;
get_entity(16#0c) -> front_panel_board;
get_entity(16#0d) -> back_panel_board;
get_entity(16#0e) -> power_system_board;
get_entity(16#0f) -> drive_backplane;
get_entity(16#10) -> system_internal_expansion_board;
get_entity(16#11) -> other_system_board;
get_entity(16#12) -> processor_board;
get_entity(16#13) -> power_unit;
get_entity(16#14) -> power_module;
get_entity(16#15) -> power_management;
get_entity(16#16) -> chassis_back_panel_board;
get_entity(16#17) -> system_chassis;
get_entity(16#18) -> sub_chassis;
get_entity(16#19) -> other_chassis_board;
get_entity(16#1a) -> disk_drive_bay;
get_entity(16#1b) -> peripheral_bay;
get_entity(16#1c) -> device_bay;
get_entity(16#1d) -> cooling_device;
get_entity(16#1e) -> cooling_unit;
get_entity(16#1f) -> interconnect;
get_entity(16#20) -> memory_device;
get_entity(16#21) -> system_management_software;
get_entity(16#22) -> system_firmware;
get_entity(16#23) -> operating_system;
get_entity(16#24) -> system_bus;
get_entity(16#25) -> group;
get_entity(16#26) -> remote_management_communication_device;
get_entity(16#27) -> external_environment;
get_entity(16#28) -> battery;
get_entity(16#29) -> processing_blade;
get_entity(16#2a) -> connectivity_switch;
get_entity(16#2b) -> processor_memory_module;
get_entity(16#2c) -> io_module;
get_entity(16#2d) -> processor_io_module;
get_entity(16#2e) -> management_controller_firmware;
get_entity(16#2f) -> ipmi_channel;
get_entity(16#30) -> pci_bus;
get_entity(16#31) -> pci_expresstm_bus;
get_entity(16#32) -> scsi_bus;
get_entity(16#33) -> sata_bus;
get_entity(16#34) -> front_side_bus;
get_entity(16#35) -> real_time_clock;
get_entity(16#37) -> air_inlet;
get_entity(16#40) -> air_inlet;
get_entity(16#41) -> processor;
get_entity(16#42) -> main_system_board;
get_entity(Id)    -> Id.

%%------------------------------------------------------------------------------
%% @doc
%% Decode the system event log 'event data' fields according to the given
%% sensor reading type (as returned from {@link get_reading/2}).
%% @end
%%------------------------------------------------------------------------------
decode_event_data({threshold, Type}, Assertion, Data) ->
    [{sensor, get_sensor(Type)}] ++ decode_threshold(Type, Assertion, Data);
decode_event_data({discrete, Type}, Assertion, Data) ->
    [{sensor, get_sensor(Type)}] ++ decode_generic(Type, Assertion, Data);
decode_event_data({oem, Type}, Assertion, Data) ->
    [{sensor, get_sensor(Type)}] ++ decode_oem(Type, Assertion, Data).

%%------------------------------------------------------------------------------
%% @doc
%% Decodes the id, lun and channel from a standard encoded binary.
%% @end
%%------------------------------------------------------------------------------
decode_addr(<<Addr:7, 0:1, 0:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}];
decode_addr(<<Addr:7, 0:1, Channel:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}, {channel, Channel}];
decode_addr(<<Id:7, 1:1, 0:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}];
decode_addr(<<Id:7, 1:1, Channel:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}, {channel, Channel}].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_threshold(Type, Assertion, <<1:2, 0:2, Offset:4, _B2:8, _:8>>) ->
    [map(Type, Offset, Assertion, 16#ff, 16#ff)];
decode_threshold(Type, Assertion, <<1:2, 1:2, Offset:4, _B2:8, _B3:8>>) ->
    [map(Type, Offset, Assertion, 16#ff, 16#ff)];
decode_threshold(Type, Assertion, <<_:2, _:2, Offset:4, _:8, _:8>>) ->
    [map(Type, Offset, Assertion, 16#ff, 16#ff)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_generic(Type, Assertion, <<0:2, 3:2, Offset:4, _:8, B3:8>>) ->
    [map(Type, Offset, Assertion, 16#ff, B3)];
decode_generic(Type, Assertion, <<1:2, 0:2, Offset:4, SOff:4, POff:4, _:8>>) ->
    Severity = maybe_severity(SOff),
    Previous = maybe_previous(Type, POff, Assertion),
    [map(Type, Offset, Assertion, 16#ff, 16#ff)] ++ Severity ++ Previous;
decode_generic(Type, Assertion, <<1:2, 3:2, Offset:4, SOff:4, POff:4, B3:8>>) ->
    Severity = maybe_severity(SOff),
    Previous = maybe_previous(Type, POff, Assertion),
    [map(Type, Offset, Assertion, 16#ff, B3)] ++ Severity ++ Previous;
decode_generic(Type, Assertion, <<3:2, 0:2, Offset:4, B2:8, _:8>>) ->
    [map(Type, Offset, Assertion, B2, 16#ff)];
decode_generic(Type, Assertion, <<3:2, 3:2, Offset:4, B2:8, B3:8>>) ->
    [map(Type, Offset, Assertion, B2, B3)];
decode_generic(Type, Assertion, <<_:2, _:2, Offset:4, _:8, _:8>>) ->
    [map(Type, Offset, Assertion, 16#ff, 16#ff)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_oem(_Type, _Asserted, _Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
%% threshold
map({threshold, 16#01}, 16#00, _,     _,     _) ->
    {lower_non_critical, going_low};
map({threshold, 16#01}, 16#01, _,     _,     _) ->
    {lower_non_critical, going_high};
map({threshold, 16#01}, 16#02, _,     _,     _) ->
    {lower_critical, going_low};
map({threshold, 16#01}, 16#03, _,     _,     _) ->
    {lower_critical, going_high};
map({threshold, 16#01}, 16#04, _,     _,     _) ->
    {lower_non_recoverable, going_low};
map({threshold, 16#01}, 16#05, _,     _,     _) ->
    {lower_non_recoverable, going_high};
map({threshold, 16#01}, 16#06, _,     _,     _) ->
    {upper_non_critical, going_low};
map({threshold, 16#01}, 16#07, _,     _,     _) ->
    {upper_non_critical, going_high};
map({threshold, 16#01}, 16#08, _,     _,     _) ->
    {upper_critical, going_low};
map({threshold, 16#01}, 16#09, _,     _,     _) ->
    {upper_critical, going_high};
map({threshold, 16#01}, 16#0a, _,     _,     _) ->
    {upper_non_recoverable, going_low};
map({threshold, 16#01}, 16#0b, _,     _,     _) ->
    {upper_non_recoverable, going_high};
%% dmi_usage
map({generic,   16#02}, 16#00, _,     _,     _) ->
    transition_to_idle;
map({generic,   16#02}, 16#01, _,     _,     _) ->
    transition_to_active;
map({generic,   16#02}, 16#02, _,     _,     _) ->
    transition_to_busy;
%% state
map({generic,   16#03}, 16#00, _,     _,     _) ->
    deasserted;
map({generic,   16#03}, 16#01, _,     _,     _) ->
    asserted;
%% predictive_failure
map({generic,   16#04}, 16#00, _,     _,     _) ->
    deasserted;
map({generic,   16#04}, 16#01, _,     _,     _) ->
    asserted;
%% limit
map({generic,   16#05}, 16#00, _,     _,     _) ->
    not_exceeded;
map({generic,   16#05}, 16#01, _,     _,     _) ->
    exceeded;
%% performance
map({generic,   16#06}, 16#00, _,     _,     _) ->
    met;
map({generic,   16#06}, 16#01, _,     _,     _) ->
    lags;
%% severity
map({generic,   16#07}, 16#00, _,     _,     _) ->
    transition_to_ok;
map({generic,   16#07}, 16#01, _,     _,     _) ->
    transition_to_non_critical_from_ok;
map({generic,   16#07}, 16#02, _,     _,     _) ->
    transition_to_critical_from_less_severe;
map({generic,   16#07}, 16#03, _,     _,     _) ->
    transition_to_non_recoverable_from_less_severe;
map({generic,   16#07}, 16#04, _,     _,     _) ->
    transition_to_non_critical_from_more_severe;
map({generic,   16#07}, 16#05, _,     _,     _) ->
    transition_to_critical_from_non_recoverable;
map({generic,   16#07}, 16#06, _,     _,     _) ->
    transition_to_non_recoverable;
map({generic,   16#07}, 16#07, _,     _,     _) ->
    monitor;
map({generic,   16#07}, 16#08, _,     _,     _) ->
    informational;
%% availability
map({generic,   16#08}, 16#00, _,     _,     _) ->
    device_removed_or_absent;
map({generic,   16#08}, 16#01, _,     _,     _) ->
    device_inserted_or_present;
map({generic,   16#09}, 16#00, _,     _,     _) ->
    device_disabled;
map({generic,   16#09}, 16#01, _,     _,     _) ->
    device_enabled;
map({generic,   16#0a}, 16#00, _,     _,     _) ->
    transition_to_running;
map({generic,   16#0a}, 16#01, _,     _,     _) ->
    transition_to_in_test;
map({generic,   16#0a}, 16#02, _,     _,     _) ->
    transition_to_power_off;
map({generic,   16#0a}, 16#03, _,     _,     _) ->
    transition_to_on_line;
map({generic,   16#0a}, 16#04, _,     _,     _) ->
    transition_to_off_line;
map({generic,   16#0a}, 16#05, _,     _,     _) ->
    transition_to_off_duty;
map({generic,   16#0a}, 16#06, _,     _,     _) ->
    transition_to_degraded;
map({generic,   16#0a}, 16#07, _,     _,     _) ->
    transition_to_power_save;
map({generic,   16#0a}, 16#08, _,     _,     _) ->
    install_error;
%% redundancy
map({generic,   16#0b}, 16#00, _,     _,     _) ->
    full;
map({generic,   16#0b}, 16#01, _,     _,     _) ->
    lost;
map({generic,   16#0b}, 16#02, _,     _,     _) ->
    degraded;
map({generic,   16#0b}, 16#03, _,     _,     _) ->
    none_from_degraded_or_full;
map({generic,   16#0b}, 16#04, _,     _,     _) ->
    none_sufficient_resources;
map({generic,   16#0b}, 16#05, _,     _,     _) ->
    none_insufficient_resources;
map({generic,   16#0b}, 16#06, _,     _,     _) ->
    degraded_from_full;
map({generic,   16#0b}, 16#07, _,     _,     _) ->
    degraded_from_none;
%% acpi_power_state
map({generic,   16#0c}, 16#00, _,     _,     _) ->
    d0;
map({generic,   16#0c}, 16#01, _,     _,     _) ->
    d1;
map({generic,   16#0c}, 16#02, _,     _,     _) ->
    d2;
map({generic,   16#0c}, 16#03, _,     _,     _) ->
    d3;
%% intrusion
map({specific,  16#05}, 16#00, _,     _,     _) ->
    general_chassis;
map({specific,  16#05}, 16#01, _,     _,     _) ->
    drive_bay;
map({specific,  16#05}, 16#02, _,     _,     _) ->
    io_card_area;
map({specific,  16#05}, 16#03, _,     _,     _) ->
    processor_area;
map({specific,  16#05}, 16#04, _,     _,     _) ->
    lan_leash_lost;
map({specific,  16#05}, 16#05, _,     _,     _) ->
    unauthorized_dock;
map({specific,  16#05}, 16#06, _,     _,     _) ->
    fan_area;
%% violation_attempt
map({specific,  16#06}, 16#00, _,     _,     _) ->
    secure_mode;
map({specific,  16#06}, 16#01, _,     _,     _) ->
    pre_boot_user_password;
map({specific,  16#06}, 16#02, _,     _,     _) ->
    pre_boot_setup_password;
map({specific,  16#06}, 16#03, _,     _,     _) ->
    pre_boot_network_password;
map({specific,  16#06}, 16#04, _,     _,     _) ->
    pre_boot_other_password;
map({specific,  16#06}, 16#05, _,     _,     _) ->
    pre_boot_out_of_band_access;
%% processor
map({specific,  16#07}, 16#00, _,     _,     _) ->
    internal_error;
map({specific,  16#07}, 16#01, _,     _,     _) ->
    thermal_trip;
map({specific,  16#07}, 16#02, _,     _,     _) ->
    frb1_bist_failure;
map({specific,  16#07}, 16#03, _,     _,     _) ->
    frb2_hang_in_post_failure;
map({specific,  16#07}, 16#04, _,     _,     _) ->
    frb3_startup_initialization_failure;
map({specific,  16#07}, 16#05, _,     _,     _) ->
    configuration_error;
map({specific,  16#07}, 16#06, _,     _,     _) ->
    uncorrectable_cpu_complex_error;
map({specific,  16#07}, 16#07, _,     _,     _) ->
    presence_detected;
map({specific,  16#07}, 16#08, _,     _,     _) ->
    disabled;
map({specific,  16#07}, 16#09, _,     _,     _) ->
    terminator_presence_detected;
map({specific,  16#07}, 16#0a, _,     _,     _) ->
    throttled_automatically;
map({specific,  16#07}, 16#0b, _,     _,     _) ->
    uncorrectable_machine_check_error;
map({specific,  16#07}, 16#0c, _,     _,     _) ->
    correctable_machine_check_error;
%% power_supply
map({specific,  16#08}, 16#00, _,     _,     _) ->
    presence_detected;
map({specific,  16#08}, 16#01, _,     _,     _) ->
    failure_detected;
map({specific,  16#08}, 16#02, _,     _,     _) ->
    predictive_failure;
map({specific,  16#08}, 16#03, _,     _,     _) ->
    input_lost;
map({specific,  16#08}, 16#04, _,     _,     _) ->
    input_lost_or_out_of_range;
map({specific,  16#08}, 16#05, _,     _,     _) ->
    input_out_of_range_but_present;
map({specific,  16#08}, 16#06, _,     _, 16#00) ->
    {configuration_error, vendor_mismatch};
map({specific,  16#08}, 16#06, _,     _, 16#01) ->
    {configuration_error, revision_mismatch};
map({specific,  16#08}, 16#06, _,     _, 16#02) ->
    {configuration_error, processor_missing};
map({specific,  16#08}, 16#06, _,     _, 16#03) ->
    {configuration_error, power_supply_rating_mismatch};
map({specific,  16#08}, 16#06, _,     _, 16#04) ->
    {configuration_error, voltage_rating_mismatch};
map({specific,  16#08}, 16#06, _,     _,     _) ->
    {configuration_error, unknown};
%% power_unit
map({specific,  16#09}, 16#00, _,     _,     _) ->
    power_off;
map({specific,  16#09}, 16#01, _,     _,     _) ->
    power_cycle;
map({specific,  16#09}, 16#02, _,     _,     _) ->
    power_down_240va;
map({specific,  16#09}, 16#03, _,     _,     _) ->
    power_down_interlock;
map({specific,  16#09}, 16#04, _,     _,     _) ->
    power_input_lost;
map({specific,  16#09}, 16#05, _,     _,     _) ->
    soft_power_control_failure;
map({specific,  16#09}, 16#06, _,     _,     _) ->
    failure_detected;
map({specific,  16#09}, 16#07, _,     _,     _) ->
    predictive_failure;
%% memory
map({specific,  16#0c}, 16#00, _,     _,     _) ->
    correctable_ecc;
map({specific,  16#0c}, 16#01, _,     _,     _) ->
    uncorrectable_ecc;
map({specific,  16#0c}, 16#02, _,     _,     _) ->
    parity;
map({specific,  16#0c}, 16#03, _,     _,     _) ->
    memory_scrub_failed;
map({specific,  16#0c}, 16#04, _,     _,     _) ->
    device_disabled;
map({specific,  16#0c}, 16#05, _,     _,     _) ->
    error_logging_limit_reached;
map({specific,  16#0c}, 16#06, _,     _,     _) ->
    presence_detected;
map({specific,  16#0c}, 16#07, _,     _,     _) ->
    configuration_error;
map({specific,  16#0c}, 16#08, _,     _, 16#ff) ->
    {spare, unspecified};
map({specific,  16#0c}, 16#08, _,     _,    Id) ->
    {spare, Id};
map({specific,  16#0c}, 16#09, _,     _,     _) ->
    throttled_automatically;
map({specific,  16#0c}, 16#0a, _,     _,     _) ->
    critical_overtemperature;
%% drive_slot
map({specific,  16#0d}, 16#00, _,     _,     _) ->
    drive_presence;
map({specific,  16#0d}, 16#01, _,     _,     _) ->
    drive_fault;
map({specific,  16#0d}, 16#02, _,     _,     _) ->
    predictive_failure;
map({specific,  16#0d}, 16#03, _,     _,     _) ->
    hot_spare;
map({specific,  16#0d}, 16#04, _,     _,     _) ->
    consistency_check_in_progress;
map({specific,  16#0d}, 16#05, _,     _,     _) ->
    in_critical_array;
map({specific,  16#0d}, 16#06, _,     _,     _) ->
    in_failed_array;
map({specific,  16#0d}, 16#07, _,     _,     _) ->
    rebuild_in_progress;
map({specific,  16#0d}, 16#08, _,     _,     _) ->
    rebuild_aborted;
%% system_firmware
map({specific,  16#0f}, 16#00, _, 16#01,     _) ->
    {error, no_memory_installed};
map({specific,  16#0f}, 16#00, _, 16#02,     _) ->
    {error, no_memory_usable};
map({specific,  16#0f}, 16#00, _, 16#03,     _) ->
    {error, hard_disk_failure};
map({specific,  16#0f}, 16#00, _, 16#04,     _) ->
    {error, system_board_failure};
map({specific,  16#0f}, 16#00, _, 16#05,     _) ->
    {error, diskette_subsystem_failure};
map({specific,  16#0f}, 16#00, _, 16#06,     _) ->
    {error, hard_disk_controller_failure};
map({specific,  16#0f}, 16#00, _, 16#07,     _) ->
    {error, keyboard_failure};
map({specific,  16#0f}, 16#00, _, 16#08,     _) ->
    {error, boot_media_not_found};
map({specific,  16#0f}, 16#00, _, 16#09,     _) ->
    {error, video_controller_failure};
map({specific,  16#0f}, 16#00, _, 16#0a,     _) ->
    {error, no_video_device};
map({specific,  16#0f}, 16#00, _, 16#0b,     _) ->
    {error, firmware_corrupted};
map({specific,  16#0f}, 16#00, _, 16#0c,     _) ->
    {error, cpu_voltage_missing};
map({specific,  16#0f}, 16#00, _, 16#0d,     _) ->
    {error, cpu_speed_matching_failure};
map({specific,  16#0f}, 16#00, _,     _,     _) ->
    {error, unspecified};
map({specific,  16#0f}, 16#01, _,     _,     _) ->
    hang;
map({specific,  16#0f}, 16#02, _, 16#01,     _) ->
    {progress, memory_initialization};
map({specific,  16#0f}, 16#02, _, 16#02,     _) ->
    {progress, hard_disk_initialization};
map({specific,  16#0f}, 16#02, _, 16#03,     _) ->
    {progress, sec_processor_initialization};
map({specific,  16#0f}, 16#02, _, 16#04,     _) ->
    {progress, user_authentication};
map({specific,  16#0f}, 16#02, _, 16#05,     _) ->
    {progress, user_initiated_system_setup};
map({specific,  16#0f}, 16#02, _, 16#06,     _) ->
    {progress, usb_resource_configuration};
map({specific,  16#0f}, 16#02, _, 16#07,     _) ->
    {progress, pci_resource_configuration};
map({specific,  16#0f}, 16#02, _, 16#08,     _) ->
    {progress, option_rom_initialization};
map({specific,  16#0f}, 16#02, _, 16#09,     _) ->
    {progress, video_initialization};
map({specific,  16#0f}, 16#02, _, 16#0a,     _) ->
    {progress, cache_initialization};
map({specific,  16#0f}, 16#02, _, 16#0b,     _) ->
    {progress, sm_bus_initialization};
map({specific,  16#0f}, 16#02, _, 16#0c,     _) ->
    {progress, keyboard_controller_initialization};
map({specific,  16#0f}, 16#02, _, 16#0d,     _) ->
    {progress, mgmt_controller_initialization};
map({specific,  16#0f}, 16#02, _, 16#0e,     _) ->
    {progress, docking_station_attachment};
map({specific,  16#0f}, 16#02, _, 16#0f,     _) ->
    {progress, enabling_docking_station};
map({specific,  16#0f}, 16#02, _, 16#10,     _) ->
    {progress, docking_station_ejection};
map({specific,  16#0f}, 16#02, _, 16#11,     _) ->
    {progress, disabling_docking_station};
map({specific,  16#0f}, 16#02, _, 16#12,     _) ->
    {progress, calling_operating_system_wake_up_vector};
map({specific,  16#0f}, 16#02, _, 16#13,     _) ->
    {progress, starting_operating_system_boot_process};
map({specific,  16#0f}, 16#02, _, 16#14,     _) ->
    {progress, baseboard_initialization};
map({specific,  16#0f}, 16#02, _, 16#16,     _) ->
    {progress, floppy_initialization};
map({specific,  16#0f}, 16#02, _, 16#17,     _) ->
    {progress, keyboard_test};
map({specific,  16#0f}, 16#02, _, 16#18,     _) ->
    {progress, pointing_device_test};
map({specific,  16#0f}, 16#02, _, 16#19,     _) ->
    {progress, prim_processor_initialization};
map({specific,  16#0f}, 16#02, _,     _,     _) ->
    {progress, unspecified};
%% event_logging
map({specific,  16#10}, 16#00, _, 16#ff,     _) ->
    {disabled, {memory, unspecified}};
map({specific,  16#10}, 16#00, _,    Id,     _) ->
    {disabled, {memory, Id}};
map({specific,  16#10}, 16#01, _, 16#ff, 16#ff) ->
    {disabled, {event, unspecified}};
map({specific,  16#10}, 16#01, _, RType,  Byte) ->
    {_, Type} = get_reading(RType, 16#00),
    Offset = Byte band 16#0f,
    Assertion = (Byte band 16#10) bsr 4,
    Tag = case (Byte band 16#20) bsr 5 of 0 -> disabled; 1 -> disabled_all end,
    {Tag, {event, map(Type, Offset, Assertion, 16#ff, 16#ff)}};
map({specific,  16#10}, 16#02, _,     _,     _) ->
    log_area_cleared;
map({specific,  16#10}, 16#03, _,     _,     _) ->
    {disabled, all};
map({specific,  16#10}, 16#04, _,     _,     _) ->
    sel_full;
map({specific,  16#10}, 16#05, _,     _, 16#ff) ->
    {sel_almost_full, unknown};
map({specific,  16#10}, 16#05, _,     _,  Fill) ->
    {sel_almost_full, Fill};
map({specific,  16#10}, 16#06, _, 16#ff, 16#ff) ->
    {disabled_all, correctable_machine_check};
map({specific,  16#10}, 16#06, _,    Id,     0) ->
    {disabled, {correctable_machine_check, {instance, Id}}};
map({specific,  16#10}, 16#06, _,    Id, 16#80) ->
    {disabled, {correctable_machine_check, {processor, Id}}};
map({specific,  16#10}, 16#06, _,    Id,     _) ->
    {disabled, {correctable_machine_check, {instance, Id}}};
%% watchdog
map({specific,  16#11}, 16#00, _,     _,     _) ->
    bios_reset;
map({specific,  16#11}, 16#01, _,     _,     _) ->
    os_reset;
map({specific,  16#11}, 16#02, _,     _,     _) ->
    shut_down;
map({specific,  16#11}, 16#03, _,     _,     _) ->
    power_down;
map({specific,  16#11}, 16#04, _,     _,     _) ->
    power_cycle;
map({specific,  16#11}, 16#05, _,     _,     _) ->
    os_nmi;
map({specific,  16#11}, 16#06, _,     _,     _) ->
    expired;
map({specific,  16#11}, 16#07, _,     _,     _) ->
    os_non_nmi;
%% system_event
map({specific,  16#12}, 16#00, _,     _,     _) ->
    system_reconfigured;
map({specific,  16#12}, 16#01, _,     _,     _) ->
    oem_boot_event;
map({specific,  16#12}, 16#02, _,     _,     _) ->
    hardware_failure;
map({specific,  16#12}, 16#03, _, 16#ff,     _) ->
    {auxiliary_log, unknown};
map({specific,  16#12}, 16#03, _,  Data,     _) ->
    Type = get_auxiliary_log_type(Data band 16#0f),
    Action = get_auxiliary_log_action(Data band 16#f0 bsr 4),
    {auxiliary_log, [{action, Action}, {type, Type}]};
map({specific,  16#12}, 16#04, _, 16#ff,     _) ->
    {pef_actions, []};
map({specific,  16#12}, 16#04, _,  Data,     _) ->
    {pef_actions, get_pef_actions(Data)};
%% critical_interrupt
map({specific,  16#13}, 16#00, _,     _,     _) ->
    front_panel_nmi;
map({specific,  16#13}, 16#01, _,     _,     _) ->
    bus_timeout;
map({specific,  16#13}, 16#02, _,     _,     _) ->
    io_channel_check_nmi;
map({specific,  16#13}, 16#03, _,     _,     _) ->
    software_nmi;
map({specific,  16#13}, 16#04, _,     _,     _) ->
    pci_perr;
map({specific,  16#13}, 16#05, _,     _,     _) ->
    pci_serr;
map({specific,  16#13}, 16#06, _,     _,     _) ->
    eisa_fail_safe_timeout;
map({specific,  16#13}, 16#07, _,     _,     _) ->
    bus_correctable_error;
map({specific,  16#13}, 16#08, _,     _,     _) ->
    bus_uncorrectable_error;
map({specific,  16#13}, 16#09, _,     _,     _) ->
    fatal_nmi;
map({specific,  16#13}, 16#0a, _,     _,     _) ->
    bus_fatal_error;
map({specific,  16#13}, 16#0b, _,     _,     _) ->
    bus_degraded;
%% switch
map({specific,  16#14}, 16#00, _,     _,     _) ->
    power_button_pressed;
map({specific,  16#14}, 16#01, _,     _,     _) ->
    sleep_button_pressed;
map({specific,  16#14}, 16#02, _,     _,     _) ->
    reset_button_pressed;
map({specific,  16#14}, 16#03, _,     _,     _) ->
    fru_latch_open;
map({specific,  16#14}, 16#04, _,     _,     _) ->
    fru_service_request_button;
%% chip_set
map({specific,  16#19}, 16#00, _,   Req,   Cur) ->
    Requested = get_power_state(Req),
    Current = get_power_state(Cur),
    {soft_power_control_failure, [{requested, Requested}, {current, Current}]};
map({specific,  16#19}, 16#01, _,     _,     _) ->
    thermal_trip;
%% interconnect
map({specific,  16#1b}, 16#00, _,     _,     _) ->
    connected;
map({specific,  16#1b}, 16#01, _,     _,     _) ->
    configuration_error;
%% system_boot
map({specific,  16#1d}, 16#00, _,     _,     _) ->
    initiated_by_power_up;
map({specific,  16#1d}, 16#01, _,     _,     _) ->
    initiated_by_hard_reset;
map({specific,  16#1d}, 16#02, _,     _,     _) ->
    initiated_by_warm_reset;
map({specific,  16#1d}, 16#03, _,     _,     _) ->
    user_requested_pxe_boot;
map({specific,  16#1d}, 16#04, _,     _,     _) ->
    automatic_boot_to_diagnostic;
map({specific,  16#1d}, 16#05, _,     _,     _) ->
    software_initiated_hard_reset;
map({specific,  16#1d}, 16#06, _,     _,     _) ->
    software_initiated_warm_reset;
map({specific,  16#1d}, 16#07, _,   Cau,  Chan) ->
    Channel = case Chan of 16#ff -> unspecified; _ -> Chan end,
    Cause = get_system_restart_cause(Cau),
    {system_restart, [{channel, Channel}, {cause, Cause}]};
%% boot_error
map({specific,  16#1e}, 16#00, _,     _,     _) ->
    no_bootable_media;
map({specific,  16#1e}, 16#01, _,     _,     _) ->
    non_bootable_media_left_in_drive;
map({specific,  16#1e}, 16#02, _,     _,     _) ->
    pxe_server_not_found;
map({specific,  16#1e}, 16#03, _,     _,     _) ->
    invalid_boot_sector;
map({specific,  16#1e}, 16#04, _,     _,     _) ->
    timeout_waiting_for_user;
%% os_boot
map({specific,  16#1f}, 16#00, _,     _,     _) ->
    completed_a;
map({specific,  16#1f}, 16#01, _,     _,     _) ->
    completed_c;
map({specific,  16#1f}, 16#02, _,     _,     _) ->
    completed_pxe;
map({specific,  16#1f}, 16#03, _,     _,     _) ->
    completed_diagnostic;
map({specific,  16#1f}, 16#04, _,     _,     _) ->
    completed_cdrom;
map({specific,  16#1f}, 16#05, _,     _,     _) ->
    completed_rom;
map({specific,  16#1f}, 16#06, _,     _,     _) ->
    completed;
%% os_shutdown
map({specific,  16#20}, 16#00, _,     _,     _) ->
    critical_stop_during_startup;
map({specific,  16#20}, 16#01, _,     _,     _) ->
    critical_stop_during_runtime;
map({specific,  16#20}, 16#02, _,     _,     _) ->
    graceful_stop;
map({specific,  16#20}, 16#03, _,     _,     _) ->
    graceful_shutdown;
map({specific,  16#20}, 16#04, _,     _,     _) ->
    soft_shutdown;
map({specific,  16#20}, 16#05, _,     _,     _) ->
    agent_not_responding;
%% slot
map({specific,  16#21}, 16#00, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {fault_status_asserted, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#01, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {identify_status_asserted, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#02, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {installed, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#03, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {ready_for_installation, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#04, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {ready_for_removal, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#05, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {off, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#06, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {removal_request, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#07, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {interlock_asserted, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#08, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {disabled, [{type, T}, {number, Num}]};
map({specific,  16#21}, 16#09, _,  Type,   Num) ->
    T = get_slot_type(Type band 2#01111111),
    {spare, [{type, T}, {number, Num}]};
%% system_acpi_power_state
map({specific,  16#22},   Off, _,     _,     _) ->
    get_power_state(Off);
%% watchdog
map({specific,  16#23}, 16#00, _,  Data,     _) ->
    Interrupt = get_interrupt_type(Data band 16#f0 bsr 4),
    Timer = get_timer_use(Data band 16#0f),
    {timer_expired, [{interrup, Interrupt}, {timer_use, Timer}]};
map({specific,  16#23}, 16#01, _,  Data,     _) ->
    Interrupt = get_interrupt_type(Data band 16#f0 bsr 4),
    Timer = get_timer_use(Data band 16#0f),
    {hard_reset, [{interrup, Interrupt}, {timer_use, Timer}]};
map({specific,  16#23}, 16#02, _,  Data,     _) ->
    Interrupt = get_interrupt_type(Data band 16#f0 bsr 4),
    Timer = get_timer_use(Data band 16#0f),
    {power_down, [{interrup, Interrupt}, {timer_use, Timer}]};
map({specific,  16#23}, 16#03, _,  Data,     _) ->
    Interrupt = get_interrupt_type(Data band 16#f0 bsr 4),
    Timer = get_timer_use(Data band 16#0f),
    {power_cycle, [{interrup, Interrupt}, {timer_use, Timer}]};
map({specific,  16#23}, 16#08, _,  Data,     _) ->
    Interrupt = get_interrupt_type(Data band 16#f0 bsr 4),
    Timer = get_timer_use(Data band 16#0f),
    {timer_interrupt, [{interrup, Interrupt}, {timer_use, Timer}]};
%% platform_alert
map({specific,  16#24}, 16#00, _,     _,     _) ->
    generated_page;
map({specific,  16#24}, 16#01, _,     _,     _) ->
    generated_lan_alert;
map({specific,  16#24}, 16#02, _,     _,     _) ->
    generated_event_trap;
map({specific,  16#24}, 16#03, _,     _,     _) ->
    generated_snmp_trap;
%% entity_presence
map({specific,  16#25}, 16#00, _,     _,     _) ->
    present;
map({specific,  16#25}, 16#01, _,     _,     _) ->
    absent;
map({specific,  16#25}, 16#02, 0,     _,     _) ->
    disabled;
map({specific,  16#25}, 16#02, 1,     _,     _) ->
    enabled;
%% lan
map({specific,  16#27}, 16#00, _,     _,     _) ->
    heartbeat_lost;
map({specific,  16#27}, 16#01, _,     _,     _) ->
    heartbeat;
%% management_subsystem_health
map({specific,  16#28}, 16#00, _,     _,     _) ->
    sensor_access_degraded;
map({specific,  16#28}, 16#01, _,     _,     _) ->
    controller_access_degraded;
map({specific,  16#28}, 16#02, _,     _,     _) ->
    controller_offline;
map({specific,  16#28}, 16#03, _,     _,     _) ->
    controller_unavailable;
map({specific,  16#28}, 16#04, _, 16#ff,     _) ->
    sensor_failure;
map({specific,  16#28}, 16#04, _,   Num,     _) ->
    {sensor_failure, Num};
map({specific,  16#28}, 16#05, _, 16#ff, 16#ff) ->
    fru_failure;
map({specific,  16#28}, 16#05, _, Data1, Data2) ->
    {fru_failure, get_fru_device(Data1, Data2)};
%% battery
map({specific,  16#29}, 16#00, _,     _,     _) ->
    low;
map({specific,  16#29}, 16#01, _,     _,     _) ->
    failed;
map({specific,  16#29}, 16#02, _,     _,     _) ->
    presence_detected;
%% session
map({specific,  16#2a}, 16#00, _,  User,  Data) ->
    {activated, get_session_data(User, Data)};
map({specific,  16#2a}, 16#01, _,  User,  Data) ->
    {deactivated, get_session_data(User, Data)};
map({specific,  16#2a}, 16#02, _,  User,  Data) ->
    {invalid_user_or_password, get_session_data(User, Data)};
map({specific,  16#2a}, 16#03, _,  User,  Data) ->
    {invalid_password_disable, get_session_data(User, Data)};
%% version
map({specific,  16#2b}, 16#00, _,  Type,     _) ->
    {hardware_change_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#01, _,  Type,     _) ->
    {firmware_change_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#02, _,  Type,     _) ->
    {hardware_incompatibility_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#03, _,  Type,     _) ->
    {firmware_incompatibility_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#04, _,  Type,     _) ->
    {unsupported_hardware_version, get_version_change_type(Type)};
map({specific,  16#2b}, 16#05, _,  Type,     _) ->
    {unsupported_firmware_version, get_version_change_type(Type)};
map({specific,  16#2b}, 16#06, 0,  Type,     _) ->
    {successful_hardware_change_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#06, 1,  Type,     _) ->
    {unsuccessful_hardware_change_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#07, 0,  Type,     _) ->
    {successful_firmware_change_detected, get_version_change_type(Type)};
map({specific,  16#2b}, 16#07, 1,  Type,     _) ->
    {unsuccessful_firmware_change_detected, get_version_change_type(Type)};
%% fru state
map({specific,  16#2c}, 16#00, _,  Data,     _) ->
    {not_installed, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#01, _,  Data,     _) ->
    {inactive, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#02, _,  Data,     _) ->
    {activation_requested, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#03, _,  Data,     _) ->
    {activation_in_progress, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#04, _,  Data,     _) ->
    {active, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#05, _,  Data,     _) ->
    {deactivation_requested, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#06, _,  Data,     _) ->
    {deactivation_in_progress, get_fru_state_data(Data)};
map({specific,  16#2c}, 16#07, _,  Data,     _) ->
    {communication_lost, get_fru_state_data(Data)};
map(                 _,     _, _,     _,     _) ->
    unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_sensor({threshold, 16#01}) -> threshold;
get_sensor({generic,   16#02}) -> dmi_usage;
get_sensor({generic,   16#03}) -> state;
get_sensor({generic,   16#04}) -> predictive_failure;
get_sensor({generic,   16#05}) -> limit;
get_sensor({generic,   16#06}) -> performance;
get_sensor({generic,   16#07}) -> severity;
get_sensor({generic,   16#08}) -> availability;
get_sensor({generic,   16#0b}) -> redundancy;
get_sensor({generic,   16#0c}) -> acpi_power_state;
get_sensor({specific,  16#01}) -> temperature;
get_sensor({specific,  16#02}) -> voltage;
get_sensor({specific,  16#03}) -> current;
get_sensor({specific,  16#04}) -> fan;
get_sensor({specific,  16#05}) -> intrusion;
get_sensor({specific,  16#06}) -> violation_attempt;
get_sensor({specific,  16#07}) -> processor;
get_sensor({specific,  16#08}) -> power_supply;
get_sensor({specific,  16#09}) -> power_unit;
get_sensor({specific,  16#0a}) -> cooling_unit;
get_sensor({specific,  16#0b}) -> other_unit;
get_sensor({specific,  16#0c}) -> memory;
get_sensor({specific,  16#0d}) -> drive_slot;
get_sensor({specific,  16#0e}) -> post_memory_resize;
get_sensor({specific,  16#0f}) -> system_firmware;
get_sensor({specific,  16#10}) -> event_logging;
get_sensor({specific,  16#11}) -> watchdog_old;
get_sensor({specific,  16#12}) -> system_event;
get_sensor({specific,  16#13}) -> critical_interrupt;
get_sensor({specific,  16#14}) -> switch;
get_sensor({specific,  16#15}) -> board;
get_sensor({specific,  16#16}) -> microcontroller;
get_sensor({specific,  16#17}) -> add_in_card;
get_sensor({specific,  16#18}) -> chassis;
get_sensor({specific,  16#19}) -> chip_set;
get_sensor({specific,  16#1a}) -> other_fru;
get_sensor({specific,  16#1b}) -> interconnect;
get_sensor({specific,  16#1c}) -> terminator;
get_sensor({specific,  16#1d}) -> system_boot;
get_sensor({specific,  16#1e}) -> boot_error;
get_sensor({specific,  16#1f}) -> os_boot;
get_sensor({specific,  16#20}) -> os_shutdown;
get_sensor({specific,  16#21}) -> slot;
get_sensor({specific,  16#22}) -> system_acpi_power_state;
get_sensor({specific,  16#23}) -> watchdog;
get_sensor({specific,  16#24}) -> platform_alert;
get_sensor({specific,  16#25}) -> entity_presence;
get_sensor({specific,  16#26}) -> monitor_asic_ic;
get_sensor({specific,  16#27}) -> lan;
get_sensor({specific,  16#28}) -> management_subsystem_health;
get_sensor({specific,  16#29}) -> battery;
get_sensor({specific,  16#2a}) -> session;
get_sensor({specific,  16#2b}) -> version;
get_sensor({specific,  16#2c}) -> fru_state;
get_sensor(_)                  -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_power_state(16#00) -> {s0_g0, working};
get_power_state(16#01) -> {s1, clocks_stopped};
get_power_state(16#02) -> {s2, clocks_stopped};
get_power_state(16#03) -> {s3, suspend_to_ram};
get_power_state(16#04) -> {s4, suspend_to_disk};
get_power_state(16#05) -> {s5_g2, soft_off};
get_power_state(16#06) -> {s4_s5, soft_off};
get_power_state(16#07) -> {g3, mechanical_off};
get_power_state(16#08) -> {s1_s2_s3, sleeping};
get_power_state(16#09) -> {g1, sleeping};
get_power_state(16#0a) -> {s5, override};
get_power_state(16#0b) -> legacy_on;
get_power_state(16#0c) -> legacy_off;
get_power_state(_)     -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_auxiliary_log_type(0) -> mca_log;
get_auxiliary_log_type(1) -> oem1;
get_auxiliary_log_type(2) -> oem2;
get_auxiliary_log_type(_) -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_auxiliary_log_action(0) -> entry_added;
get_auxiliary_log_action(1) -> entry_added;
get_auxiliary_log_action(2) -> entry_added;
get_auxiliary_log_action(3) -> cleared;
get_auxiliary_log_action(4) -> disabled;
get_auxiliary_log_action(5) -> enabled;
get_auxiliary_log_action(_) -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_system_restart_cause(16#01) -> chassis_control_command;
get_system_restart_cause(16#02) -> reset_via_pushbutton;
get_system_restart_cause(16#03) -> power_up_via_power_pushbutton;
get_system_restart_cause(16#04) -> watchdog_expiration;
get_system_restart_cause(16#05) -> oem;
get_system_restart_cause(16#06) -> automatic_always_restore;
get_system_restart_cause(16#07) -> automatic_restore_previous;
get_system_restart_cause(16#08) -> reset_via_pef;
get_system_restart_cause(16#09) -> power_cycle_via_pef;
get_system_restart_cause(16#0a) -> soft_reset;
get_system_restart_cause(16#0b) -> power_up_via_rtc;
get_system_restart_cause(_)     -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_slot_type(0)  -> pci;
get_slot_type(1)  -> drive_array;
get_slot_type(2)  -> external_connector;
get_slot_type(3)  -> docking;
get_slot_type(4)  -> internal_expansion;
get_slot_type(5)  -> associated_with_entity_of_sensor;
get_slot_type(6)  -> atca;
get_slot_type(7)  -> memory;
get_slot_type(8)  -> fan;
get_slot_type(9)  -> pci_express;
get_slot_type(10) -> scsi;
get_slot_type(11) -> sata;
get_slot_type(_)  -> unknown.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_interrupt_type(0) -> none;
get_interrupt_type(1) -> smi;
get_interrupt_type(2) -> nmi;
get_interrupt_type(3) -> messaging;
get_interrupt_type(_) -> unspecified.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_timer_use(1) -> bios_frb2;
get_timer_use(2) -> bios;
get_timer_use(3) -> os_load;
get_timer_use(4) -> sms;
get_timer_use(5) -> oem;
get_timer_use(_) -> unspecified.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_pef_actions(Actions) ->
    A = case Actions band 16#20 bsr 5 of 0 -> []; 1 -> diagnostic_interrupt end,
    B = case Actions band 16#10 bsr 4 of 0 -> []; 1 -> oem end,
    C = case Actions band 16#08 bsr 3 of 0 -> []; 1 -> power_cycle end,
    D = case Actions band 16#04 bsr 2 of 0 -> []; 1 -> reset end,
    E = case Actions band 16#02 bsr 1 of 0 -> []; 1 -> power_off end,
    F = case Actions band 16#01 of 0 -> []; 1 -> alert end,
    A ++ B ++ C ++ D ++ E ++ F.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_device(A, B) when (A band 16#80 bsr 7) == 0 ->
    {non_logical, B band 2#11111110 bsr 1};
get_fru_device(_, B) ->
    {logical, B}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_state_data(16#ff) ->
    [];
get_fru_state_data(A) ->
    get_fru_state_data_(<<A:8>>).
get_fru_state_data_(<<0:4, P:4>>) ->
    [{cause, normal_state_change}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<1:4, P:4>>) ->
    [{cause, external_software}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<2:4, P:4>>) ->
    [{cause, latch}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<3:4, P:4>>) ->
    [{cause, hot_swap}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<4:4, P:4>>) ->
    [{cause, internal_software}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<5:4, P:4>>) ->
    [{cause, communication_lost}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<6:4, P:4>>) ->
    [{cause, communication_lost}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<7:4, P:4>>) ->
    [{cause, unexpected_extraction}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<8:4, P:4>>) ->
    [{cause, update}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<9:4, P:4>>) ->
    [{cause, no_ipmb_address}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<10:4, P:4>>) ->
    [{cause, unexpedcted_deactivation}] ++ maybe_previous({specific,  16#2c}, P, 0);
get_fru_state_data_(<<_:4, P:4>>) ->
    maybe_previous({specific,  16#2c}, P, 0).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_data(16#ff, 16#ff) ->
    [];
get_session_data(A, B) ->
    get_session_data_(<<A:8>>, <<B:8>>).
get_session_data_(<<_:2, UserId:6>>, <<_:2, 0:2, Channel:4>>) ->
    [{user_id, UserId}, {channel, Channel}];
get_session_data_(<<_:2, UserId:6>>, <<_:2, 1:2, Channel:4>>) ->
    [{user_id, UserId}, {channel, Channel}, {cause, close_session_command}];
get_session_data_(<<_:2, UserId:6>>, <<_:2, 2:2, Channel:4>>) ->
    [{user_id, UserId}, {channel, Channel}, {cause, timeout}];
get_session_data_(<<_:2, UserId:6>>, <<_:2, 3:2, Channel:4>>) ->
    [{user_id, UserId}, {channel, Channel}, {cause, configuration_change}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_version_change_type(16#01) -> management_controller_device_id;
get_version_change_type(16#02) -> management_controller_firmware_revision;
get_version_change_type(16#03) -> management_controller_device_revision;
get_version_change_type(16#04) -> management_controller_manufacturer_id;
get_version_change_type(16#05) -> management_controller_ipmi_version;
get_version_change_type(16#06) -> management_controller_auxiliary_firmware_id;
get_version_change_type(16#07) -> management_controller_firmware_boot_block;
get_version_change_type(16#08) -> management_controller_firmware;
get_version_change_type(16#09) -> system_firmware_change;
get_version_change_type(16#0a) -> sm_bios_change;
get_version_change_type(16#0b) -> operating_system_change;
get_version_change_type(16#0c) -> operating_system_loader_change;
get_version_change_type(16#0d) -> partition_change;
get_version_change_type(16#0e) -> management_software_agent_change;
get_version_change_type(16#0f) -> management_software_application_change;
get_version_change_type(16#10) -> management_software_middleware_change;
get_version_change_type(16#11) -> programmable_hardware_change;
get_version_change_type(16#12) -> fru_module_change;
get_version_change_type(16#13) -> fru_component_change;
get_version_change_type(16#14) -> fru_replaced_with_equivalent_version;
get_version_change_type(16#15) -> fru_replaced_with_newer_version;
get_version_change_type(16#16) -> fru_replaced_with_older_version;
get_version_change_type(16#17) -> fru_hardware_configuration_change;
get_version_change_type(_)     -> unspecified.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_severity(16#f) ->
    [];
maybe_severity(Offset) ->
    [map({generic, 16#07}, Offset, 0, 16#ff, 16#ff)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_previous(_Type, 16#f, _Assert) ->
    [];
maybe_previous(Type, Offset, Assert) ->
    [{previous, map(Type, Offset, Assert, 16#ff, 16#ff)}].
