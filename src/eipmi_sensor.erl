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
%%% @end
%%%=============================================================================

-module(eipmi_sensor).

-export([get_addr/1,
         get_reading/2,
         get_type/1,
         get_value/5,
         get_entity/2,
         get_entity_id/1,
         get_unit/1]).

-include("eipmi.hrl").

-type addr() ::
        {slave_addr, non_neg_integer()} |
        {slave_lun, non_neg_integer()} |
        {channel, non_neg_integer()} |
        {software_id, non_neg_integer()}.

-type entity_id() ::
        unspecified | other | unknown | processor | disk_or_disk_bay |
        peripheral_bay | system_management_module | system_board |
        memory_module | processor_module | power_supply | add_in_card |
        front_panel_board | back_panel_board | power_system_board |
        drive_backplane | system_internal_expansion_board | other_system_board |
        processor_board | power_unit | power_module | power_management |
        chassis_back_panel_board | system_chassis | sub_chassis |
        other_chassis_board | disk_drive_bay | peripheral_bay | device_bay |
        cooling_device | cooling_unit | interconnect | memory_device |
        system_management_software | system_firmware | operating_system |
        system_bus | group | remote_management_communication_device |
        external_environment | battery | processing_blade |
        connectivity_switch | processor_memory_module | io_module |
        processor_io_module | management_controller_firmware | ipmi_channel |
        pci_bus | pci_expresstm_bus | scsi_bus | sata_bus | front_side_bus |
        real_time_clock | air_inlet | air_inlet | processor |
        main_system_board | non_neg_integer().

-type entity() ::
        {entity_id, entity_id()} |
        {entity_type, logical | physical} |
        {entity_instance, non_neg_integer()}.

-type type() ::
        threshold | dmi_usage | state | predictive_failure | limit |
        performance | severity | availability | redundancy | acpi_power_state |
        temperature | voltage | current | fan | intrusion | violation_attempt |
        processor | power_supply | power_unit | cooling_unit | other_unit |
        memory | drive_slot | post_memory_resize | system_firmware |
        event_logging | watchdog_old | system_event | critical_interrupt |
        switch | board | microcontroller | add_in_card | chassis | chip_set |
        other_fru | interconnect | terminator | system_boot | boot_error |
        os_boot | os_shutdown | slot | system_acpi_power_state | watchdog |
        platform_alert | entity_presence | monitor_asic_ic | lan |
        management_subsystem_health | battery | session | version | fru_state |
        non_neg_integer().

-type value() :: term().

-export_type([addr/0, entity_id/0, entity/0, type/0, value/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Decodes the sensor from a 2byte binary according to the encoding provided by
%% SEL and SDR records.
%% @end
%%------------------------------------------------------------------------------
-spec get_addr(binary()) ->
                      [addr()].
get_addr(<<Addr:7, 0:1, 0:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}];
get_addr(<<Addr:7, 0:1, Channel:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}, {channel, Channel}];
get_addr(<<Id:7, 1:1, 0:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}];
get_addr(<<Id:7, 1:1, Channel:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}, {channel, Channel}].

%%------------------------------------------------------------------------------
%% @doc
%% Returns the sensor category and reading type from the given sensor and
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
%% Returns the type of sensor according to the given sensor reading type which
%% is actually the second parameter of the result tuple returned by
%% {@link get_reading/2}.
%% @end
%%------------------------------------------------------------------------------
-spec get_type({threshold | generic | specific | term(), non_neg_integer()}) ->
                      [{sensor_type, type()}].
get_type({threshold, 16#01}) -> [{sensor_type, threshold}];
get_type({generic,   16#02}) -> [{sensor_type, dmi_usage}];
get_type({generic,   16#03}) -> [{sensor_type, state}];
get_type({generic,   16#04}) -> [{sensor_type, predictive_failure}];
get_type({generic,   16#05}) -> [{sensor_type, limit}];
get_type({generic,   16#06}) -> [{sensor_type, performance}];
get_type({generic,   16#07}) -> [{sensor_type, severity}];
get_type({generic,   16#08}) -> [{sensor_type, availability}];
get_type({generic,   16#0b}) -> [{sensor_type, redundancy}];
get_type({generic,   16#0c}) -> [{sensor_type, acpi_power_state}];
get_type({specific,  16#01}) -> [{sensor_type, temperature}];
get_type({specific,  16#02}) -> [{sensor_type, voltage}];
get_type({specific,  16#03}) -> [{sensor_type, current}];
get_type({specific,  16#04}) -> [{sensor_type, fan}];
get_type({specific,  16#05}) -> [{sensor_type, intrusion}];
get_type({specific,  16#06}) -> [{sensor_type, violation_attempt}];
get_type({specific,  16#07}) -> [{sensor_type, processor}];
get_type({specific,  16#08}) -> [{sensor_type, power_supply}];
get_type({specific,  16#09}) -> [{sensor_type, power_unit}];
get_type({specific,  16#0a}) -> [{sensor_type, cooling_unit}];
get_type({specific,  16#0b}) -> [{sensor_type, other_unit}];
get_type({specific,  16#0c}) -> [{sensor_type, memory}];
get_type({specific,  16#0d}) -> [{sensor_type, drive_slot}];
get_type({specific,  16#0e}) -> [{sensor_type, post_memory_resize}];
get_type({specific,  16#0f}) -> [{sensor_type, system_firmware}];
get_type({specific,  16#10}) -> [{sensor_type, event_logging}];
get_type({specific,  16#11}) -> [{sensor_type, watchdog_old}];
get_type({specific,  16#12}) -> [{sensor_type, system_event}];
get_type({specific,  16#13}) -> [{sensor_type, critical_interrupt}];
get_type({specific,  16#14}) -> [{sensor_type, switch}];
get_type({specific,  16#15}) -> [{sensor_type, board}];
get_type({specific,  16#16}) -> [{sensor_type, microcontroller}];
get_type({specific,  16#17}) -> [{sensor_type, add_in_card}];
get_type({specific,  16#18}) -> [{sensor_type, chassis}];
get_type({specific,  16#19}) -> [{sensor_type, chip_set}];
get_type({specific,  16#1a}) -> [{sensor_type, other_fru}];
get_type({specific,  16#1b}) -> [{sensor_type, interconnect}];
get_type({specific,  16#1c}) -> [{sensor_type, terminator}];
get_type({specific,  16#1d}) -> [{sensor_type, system_boot}];
get_type({specific,  16#1e}) -> [{sensor_type, boot_error}];
get_type({specific,  16#1f}) -> [{sensor_type, os_boot}];
get_type({specific,  16#20}) -> [{sensor_type, os_shutdown}];
get_type({specific,  16#21}) -> [{sensor_type, slot}];
get_type({specific,  16#22}) -> [{sensor_type, system_acpi_power_state}];
get_type({specific,  16#23}) -> [{sensor_type, watchdog}];
get_type({specific,  16#24}) -> [{sensor_type, platform_alert}];
get_type({specific,  16#25}) -> [{sensor_type, entity_presence}];
get_type({specific,  16#26}) -> [{sensor_type, monitor_asic_ic}];
get_type({specific,  16#27}) -> [{sensor_type, lan}];
get_type({specific,  16#28}) -> [{sensor_type, management_subsystem_health}];
get_type({specific,  16#29}) -> [{sensor_type, battery}];
get_type({specific,  16#2a}) -> [{sensor_type, session}];
get_type({specific,  16#2b}) -> [{sensor_type, version}];
get_type({specific,  16#2c}) -> [{sensor_type, fru_state}];
get_type({_,          Type}) -> [{sensor_type, Type}].

%%------------------------------------------------------------------------------
%% @doc
%% Returns a (raw) sensor reading. The returned reading may have to be converted
%% into human readable values (at least for threshold based sensors).
%% @end
%%------------------------------------------------------------------------------
get_value(Type, Offset, Assertion, Value1, Value2) ->
    map(Type, Offset, Assertion, Value1, Value2).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list containing the decoded (human readable) entity properties. To
%% retrieve only the entity id use {@link get_entity_id/1}.
%% @end
%%------------------------------------------------------------------------------
-spec get_entity(non_neg_integer(), binary() | non_neg_integer()) ->
                        [entity()].
get_entity(Id, Instance) when is_integer(Instance) ->
    get_entity(Id, <<Instance:8>>);
get_entity(Id, <<Type:1, Instance:7>>) ->
    get_entity(get_entity_id(Id), Type, Instance).
get_entity(Acc, 0, Instance) when Instance < 16#60 ->
    Acc ++ [{entity_type, physical}, {entity_instance, Instance}];
get_entity(Acc, 1, Instance) when Instance < 16#60 ->
    Acc ++ [{entity_type, logical}, {entity_instance, Instance}];
get_entity(Acc, 0, Instance) ->
    Acc ++ [{entity_type, physical}, {entity_instance, Instance - 16#60}];
get_entity(Acc, 1, Instance) ->
    Acc ++ [{entity_type, logical}, {entity_instance, Instance - 16#60}].

%%------------------------------------------------------------------------------
%% @doc
%% Returns the human readable entity for a given entity id according to
%% section 43.14 from the IPMI specification. For full entity decoding refer to
%% {@link get_entity/2}.
%% @end
%%------------------------------------------------------------------------------
-spec get_entity_id(non_neg_integer()) ->
                           [{entity_id, entity_id()}].
get_entity_id(16#00) -> [{entity_id, unspecified}];
get_entity_id(16#01) -> [{entity_id, other}];
get_entity_id(16#02) -> [{entity_id, unknown}];
get_entity_id(16#03) -> [{entity_id, processor}];
get_entity_id(16#04) -> [{entity_id, disk_or_disk_bay}];
get_entity_id(16#05) -> [{entity_id, peripheral_bay}];
get_entity_id(16#06) -> [{entity_id, system_management_module}];
get_entity_id(16#07) -> [{entity_id, system_board}];
get_entity_id(16#08) -> [{entity_id, memory_module}];
get_entity_id(16#09) -> [{entity_id, processor_module}];
get_entity_id(16#0a) -> [{entity_id, power_supply}];
get_entity_id(16#0b) -> [{entity_id, add_in_card}];
get_entity_id(16#0c) -> [{entity_id, front_panel_board}];
get_entity_id(16#0d) -> [{entity_id, back_panel_board}];
get_entity_id(16#0e) -> [{entity_id, power_system_board}];
get_entity_id(16#0f) -> [{entity_id, drive_backplane}];
get_entity_id(16#10) -> [{entity_id, system_internal_expansion_board}];
get_entity_id(16#11) -> [{entity_id, other_system_board}];
get_entity_id(16#12) -> [{entity_id, processor_board}];
get_entity_id(16#13) -> [{entity_id, power_unit}];
get_entity_id(16#14) -> [{entity_id, power_module}];
get_entity_id(16#15) -> [{entity_id, power_management}];
get_entity_id(16#16) -> [{entity_id, chassis_back_panel_board}];
get_entity_id(16#17) -> [{entity_id, system_chassis}];
get_entity_id(16#18) -> [{entity_id, sub_chassis}];
get_entity_id(16#19) -> [{entity_id, other_chassis_board}];
get_entity_id(16#1a) -> [{entity_id, disk_drive_bay}];
get_entity_id(16#1b) -> [{entity_id, peripheral_bay}];
get_entity_id(16#1c) -> [{entity_id, device_bay}];
get_entity_id(16#1d) -> [{entity_id, cooling_device}];
get_entity_id(16#1e) -> [{entity_id, cooling_unit}];
get_entity_id(16#1f) -> [{entity_id, interconnect}];
get_entity_id(16#20) -> [{entity_id, memory_device}];
get_entity_id(16#21) -> [{entity_id, system_management_software}];
get_entity_id(16#22) -> [{entity_id, system_firmware}];
get_entity_id(16#23) -> [{entity_id, operating_system}];
get_entity_id(16#24) -> [{entity_id, system_bus}];
get_entity_id(16#25) -> [{entity_id, group}];
get_entity_id(16#26) -> [{entity_id, remote_management_communication_device}];
get_entity_id(16#27) -> [{entity_id, external_environment}];
get_entity_id(16#28) -> [{entity_id, battery}];
get_entity_id(16#29) -> [{entity_id, processing_blade}];
get_entity_id(16#2a) -> [{entity_id, connectivity_switch}];
get_entity_id(16#2b) -> [{entity_id, processor_memory_module}];
get_entity_id(16#2c) -> [{entity_id, io_module}];
get_entity_id(16#2d) -> [{entity_id, processor_io_module}];
get_entity_id(16#2e) -> [{entity_id, management_controller_firmware}];
get_entity_id(16#2f) -> [{entity_id, ipmi_channel}];
get_entity_id(16#30) -> [{entity_id, pci_bus}];
get_entity_id(16#31) -> [{entity_id, pci_expresstm_bus}];
get_entity_id(16#32) -> [{entity_id, scsi_bus}];
get_entity_id(16#33) -> [{entity_id, sata_bus}];
get_entity_id(16#34) -> [{entity_id, front_side_bus}];
get_entity_id(16#35) -> [{entity_id, real_time_clock}];
get_entity_id(16#37) -> [{entity_id, air_inlet}];
get_entity_id(16#40) -> [{entity_id, air_inlet}];
get_entity_id(16#41) -> [{entity_id, processor}];
get_entity_id(16#42) -> [{entity_id, main_system_board}];
get_entity_id(Id)    -> [{entity_id, Id}].

%%------------------------------------------------------------------------------
%% @doc
%% Returns the sensor unit from the given type code according to section 43.17
%% from the IPMI specification.
%% @end
%%------------------------------------------------------------------------------
get_unit(0) ->  unspecified;
get_unit(1) ->  clesius;
get_unit(2) ->  fahrenheit;
get_unit(3) ->  kelvin;
get_unit(4) ->  volts;
get_unit(5) ->  amps;
get_unit(6) ->  watts;
get_unit(7) ->  joules;
get_unit(8) ->  coulombs;
get_unit(9) ->  va;
get_unit(10) -> nits;
get_unit(11) -> lumen;
get_unit(12) -> lux;
get_unit(13) -> candela;
get_unit(14) -> kPa;
get_unit(15) -> psi;
get_unit(16) -> newton;
get_unit(17) -> cfm;
get_unit(18) -> rpm;
get_unit(19) -> hz;
get_unit(20) -> microsecond;
get_unit(21) -> millisecond;
get_unit(22) -> second;
get_unit(23) -> minute;
get_unit(24) -> hour;
get_unit(25) -> day;
get_unit(26) -> week;
get_unit(27) -> mil;
get_unit(28) -> inches;
get_unit(29) -> feet;
get_unit(30) -> cubic_inches;
get_unit(31) -> cubic_feet;
get_unit(32) -> mm;
get_unit(33) -> cm;
get_unit(34) -> m;
get_unit(35) -> cubic_cm;
get_unit(36) -> cubic_m;
get_unit(37) -> liters;
get_unit(38) -> fluid_ounce;
get_unit(39) -> radians;
get_unit(40) -> steradians;
get_unit(41) -> revolutions;
get_unit(42) -> cycles;
get_unit(43) -> gravities;
get_unit(44) -> ounce;
get_unit(45) -> pound;
get_unit(46) -> ft_lb;
get_unit(47) -> oz_inch;
get_unit(48) -> gauss;
get_unit(49) -> gilberts;
get_unit(50) -> henry;
get_unit(51) -> millihenry;
get_unit(52) -> farad;
get_unit(53) -> microfarad;
get_unit(54) -> ohms;
get_unit(55) -> siemens;
get_unit(56) -> mole;
get_unit(57) -> becquerel;
get_unit(58) -> ppm;
get_unit(59) -> reserved;
get_unit(60) -> decibels;
get_unit(61) -> dBA;
get_unit(62) -> dBC;
get_unit(63) -> gray;
get_unit(64) -> sievert;
get_unit(65) -> color_temperature_kelvin;
get_unit(66) -> bit;
get_unit(67) -> kilobit;
get_unit(68) -> megabit;
get_unit(69) -> gigabit;
get_unit(70) -> byte;
get_unit(71) -> kilobyte;
get_unit(72) -> megabyte;
get_unit(73) -> gigabyte;
get_unit(74) -> word;
get_unit(75) -> dword;
get_unit(76) -> qword;
get_unit(77) -> memory_line;
get_unit(78) -> hit;
get_unit(79) -> miss;
get_unit(80) -> retry;
get_unit(81) -> reset;
get_unit(82) -> overrun;
get_unit(83) -> underrun;
get_unit(84) -> collision;
get_unit(85) -> packets;
get_unit(86) -> messages;
get_unit(87) -> characters;
get_unit(88) -> error;
get_unit(89) -> correctable_error;
get_unit(90) -> uncorrectable_error;
get_unit(91) -> fatal_error;
get_unit(92) -> grams.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
%% threshold
map({threshold, 16#01}, 16#00, _,    B2,    B3) ->
    {lower_non_critical, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#01, _,    B2,    B3) ->
    {lower_non_critical, going_high, B2 ++ B3};
map({threshold, 16#01}, 16#02, _,    B2,    B3) ->
    {lower_critical, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#03, _,    B2,    B3) ->
    {lower_critical, going_high, B2 ++ B3};
map({threshold, 16#01}, 16#04, _,    B2,    B3) ->
    {lower_non_recoverable, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#05, _,    B2,    B3) ->
    {lower_non_recoverable, going_high, B2 ++ B3};
map({threshold, 16#01}, 16#06, _,    B2,    B3) ->
    {upper_non_critical, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#07, _,    B2,    B3) ->
    {upper_non_critical, going_high, B2 ++ B3};
map({threshold, 16#01}, 16#08, _,    B2,    B3) ->
    {upper_critical, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#09, _,    B2,    B3) ->
    {upper_critical, going_high, B2 ++ B3};
map({threshold, 16#01}, 16#0a, _,    B2,    B3) ->
    {upper_non_recoverable, going_low, B2 ++ B3};
map({threshold, 16#01}, 16#0b, _,    B2,    B3) ->
    {upper_non_recoverable, going_high, B2 ++ B3};
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
map(              Type,     O, A,    B2,    B3) ->
    {unknown, [{type, Type}, {offset, O},
               {asserted, eipmi_util:get_bool_inv(A)},
               {data2, B2}, {data3, B3}]}.

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
maybe_previous(_Type, 16#f, _Assert) ->
    [];
maybe_previous(Type, Offset, Assert) ->
    [{previous_value, map(Type, Offset, Assert, 16#ff, 16#ff)}].
