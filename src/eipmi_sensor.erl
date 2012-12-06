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
         get_type/2,
         get_value/5,
         get_entity/2,
         get_entity_id/1,
         get_unit/1]).

-include("eipmi.hrl").

-type addr() ::
        {fru_id, non_neg_integer()} |
        {fru_lun, non_neg_integer()} |
        {sensor_addr, non_neg_integer()} |
        {sensor_lun, non_neg_integer()} |
        {bus_id, non_neg_integer()} |
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
        main_system_board | front_board | amc | mch |
        shelf_management_controller | filtration_unit | shelf_fru_information |
        alarm_panel | non_neg_integer().

-type entity() ::
        {entity_id, entity_id()} |
        {entity_type, logical | physical} |
        {entity_instance, non_neg_integer()}.

-type type() ::
        threshold | dmi_usage | state | predictive_failure | limit |
        performance | severity | availability0 | availability1 | availability2 |
        redundancy | acpi_power_state | temperature | voltage | current | fan |
        intrusion | violation_attempt | processor | power_supply | power_unit |
        cooling_unit | other_unit | memory | drive_slot | post_memory_resize |
        system_firmware | event_logging | watchdog_old | system_event |
        critical_interrupt | switch | board | microcontroller | add_in_card |
        chassis | chip_set | other_fru | interconnect | terminator |
        system_boot | boot_error | os_boot | os_shutdown | slot |
        system_acpi_power_state | watchdog | platform_alert | entity_presence |
        monitor_asic_ic | lan | management_subsystem_health | battery | session |
        version | fru_state | fru_hot_swap | ipmb_physical | module_hot_swap |
        power_channel | telco_alarm | non_neg_integer().

-type value() ::
        {sensor_value, term()} |
        {previous_value, term()} |
        {severity_value, term()} |
        term().

-type unit() ::
        celsius | fahrenheit | kelvin | volts | amps | watts | joules |
        coulombs | va | nits | lumen | lux | candela | kPa | psi | newton |
        cfm | rpm | hz | microsecond | millisecond | second | minute | hour |
        day | week | mil | inches | feet | cubic_inches | cubic_feet | mm | cm |
        m | cubic_cm | cubic_m | liters | fluid_ounce | radians | steradians |
        revolutions | cycles | gravities | ounce | pound | ft_lb | oz_inch |
        gauss | gilberts | henry | millihenry | farad | microfarad | ohms |
        siemens | mole | becquerel | ppm | reserved | decibels | dBA | dBC |
        gray | sievert | color_temperature_kelvin | bit | kilobit | megabit |
        gigabit | byte | kilobyte | megabyte | gigabyte | word | dword | qword |
        memory_line | hit | miss | retry | reset | overrun | underrun |
        collision | packets | messages | characters | error |
        correctable_error | uncorrectable_error | fatal_error | grams |
        unspecified.

-export_type([addr/0,
              entity_id/0,
              entity/0,
              type/0,
              value/0,
              unit/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Decodes the sensor from a 2 or 3 byte binary according to the encoding
%% provided by SEL and SDR records.
%% @end
%%------------------------------------------------------------------------------
-spec get_addr(binary()) -> [addr()].
get_addr(<<FruId:8, 1:1, _:2, Lun:2, _:3, C:4, _:4>>) ->
    [{fru_id, FruId}, {sensor_lun, Lun}] ++ get_channel(C);
get_addr(<<Addr:7, _:1, 0:1, _:2, Lun:2, 0:3, C:4, _:4>>) ->
    [{sensor_addr, Addr}, {sensor_lun, Lun}] ++ get_channel(C);
get_addr(<<Addr:7, _:1, 0:1, _:2, Lun:2, Bus:3, C:4, _:4>>) ->
    [{sensor_addr, Addr}, {sensor_lun, Lun}, {bus_id, Bus}] ++ get_channel(C);
get_addr(<<Addr:7, 0:1, C:4, 0:2, Lun:2>>) ->
    [{sensor_addr, Addr}, {sensor_lun, Lun}] ++ get_channel(C);
get_addr(<<Addr:7, 0:1, C:4, FLun:2, Lun:2>>) ->
    [{sensor_addr, Addr}, {sensor_lun, Lun}, {fru_lun, FLun}] ++ get_channel(C);
get_addr(<<Id:7, 1:1, C:4, 0:2, 0:2>>) ->
    [{software_id, Id}] ++ get_channel(C);
get_addr(<<Id:7, 1:1, C:4, FLun:2, 0:2>>) ->
    [{software_id, Id}, {fru_lun, FLun}] ++ get_channel(C).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the sensor category and reading type from the given sensor and
%% reading type. See sections 41 and 42 of the IPMI specification for more
%% information.
%% @end
%%------------------------------------------------------------------------------
-spec get_type(non_neg_integer(), non_neg_integer()) ->
                      {threshold | discrete | {oem, non_neg_integer()}, type()}.
get_type(16#01, _SensorType) ->
    {threshold, get_sensor_type({threshold, 16#01})};
get_type(ReadingType, _SensorType)
  when ReadingType >= 16#02 andalso ReadingType =< 16#0c ->
    {discrete, get_sensor_type({generic, ReadingType})};
get_type(16#6f, SensorType) ->
    {discrete, get_sensor_type({specific, SensorType})};
get_type(ReadingType, SensorType)
  when ReadingType >= 16#70 andalso ReadingType =< 16#7f ->
    {{oem, ReadingType}, get_sensor_type({oem, SensorType})}.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a (raw) sensor reading. The returned reading may have to be converted
%% into human readable values (at least for threshold based sensors).
%% @end
%%------------------------------------------------------------------------------
-spec get_value(type(), non_neg_integer(), 0 | 1, 0..255, 0..255) -> [value()].
get_value(Type, Offset, Assertion, Value1, Value2) ->
    map(Type, Offset, Assertion, Value1, Value2).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list containing the decoded (human readable) entity properties. To
%% retrieve only the entity id use {@link get_entity_id/1}.
%% @end
%%------------------------------------------------------------------------------
-spec get_entity(non_neg_integer(), binary() | non_neg_integer()) -> [entity()].
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
-spec get_entity_id(non_neg_integer()) -> [{entity_id, entity_id()}].
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
get_entity_id(16#a0) -> [{entity_id, front_board}];
get_entity_id(16#c1) -> [{entity_id, amc}];
get_entity_id(16#c2) -> [{entity_id, mch}];
get_entity_id(16#f0) -> [{entity_id, shelf_management_controller}];
get_entity_id(16#f1) -> [{entity_id, filtration_unit}];
get_entity_id(16#f2) -> [{entity_id, shelf_fru_information}];
get_entity_id(16#f3) -> [{entity_id, alarm_panel}];
get_entity_id(Id)    -> [{entity_id, Id}].

%%------------------------------------------------------------------------------
%% @doc
%% Returns the sensor unit from the given type code according to section 43.17
%% from the IPMI specification.
%% @end
%%------------------------------------------------------------------------------
-spec get_unit(non_neg_integer()) -> unit().
get_unit(1) ->  celsius;
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
get_unit(92) -> grams;
get_unit(_)  ->  unspecified.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_sensor_type({threshold, 16#01}) -> threshold;
get_sensor_type({generic,   16#02}) -> dmi_usage;
get_sensor_type({generic,   16#03}) -> state;
get_sensor_type({generic,   16#04}) -> predictive_failure;
get_sensor_type({generic,   16#05}) -> limit;
get_sensor_type({generic,   16#06}) -> performance;
get_sensor_type({generic,   16#07}) -> severity;
get_sensor_type({generic,   16#08}) -> availability0;
get_sensor_type({generic,   16#09}) -> availability1;
get_sensor_type({generic,   16#0a}) -> availability2;
get_sensor_type({generic,   16#0b}) -> redundancy;
get_sensor_type({generic,   16#0c}) -> acpi_power_state;
get_sensor_type({specific,  16#01}) -> temperature;
get_sensor_type({specific,  16#02}) -> voltage;
get_sensor_type({specific,  16#03}) -> current;
get_sensor_type({specific,  16#04}) -> fan;
get_sensor_type({specific,  16#05}) -> intrusion;
get_sensor_type({specific,  16#06}) -> violation_attempt;
get_sensor_type({specific,  16#07}) -> processor;
get_sensor_type({specific,  16#08}) -> power_supply;
get_sensor_type({specific,  16#09}) -> power_unit;
get_sensor_type({specific,  16#0a}) -> cooling_unit;
get_sensor_type({specific,  16#0b}) -> other_unit;
get_sensor_type({specific,  16#0c}) -> memory;
get_sensor_type({specific,  16#0d}) -> drive_slot;
get_sensor_type({specific,  16#0e}) -> post_memory_resize;
get_sensor_type({specific,  16#0f}) -> system_firmware;
get_sensor_type({specific,  16#10}) -> event_logging;
get_sensor_type({specific,  16#11}) -> watchdog_old;
get_sensor_type({specific,  16#12}) -> system_event;
get_sensor_type({specific,  16#13}) -> critical_interrupt;
get_sensor_type({specific,  16#14}) -> switch;
get_sensor_type({specific,  16#15}) -> board;
get_sensor_type({specific,  16#16}) -> microcontroller;
get_sensor_type({specific,  16#17}) -> add_in_card;
get_sensor_type({specific,  16#18}) -> chassis;
get_sensor_type({specific,  16#19}) -> chip_set;
get_sensor_type({specific,  16#1a}) -> other_fru;
get_sensor_type({specific,  16#1b}) -> interconnect;
get_sensor_type({specific,  16#1c}) -> terminator;
get_sensor_type({specific,  16#1d}) -> system_boot;
get_sensor_type({specific,  16#1e}) -> boot_error;
get_sensor_type({specific,  16#1f}) -> os_boot;
get_sensor_type({specific,  16#20}) -> os_shutdown;
get_sensor_type({specific,  16#21}) -> slot;
get_sensor_type({specific,  16#22}) -> system_acpi_power_state;
get_sensor_type({specific,  16#23}) -> watchdog;
get_sensor_type({specific,  16#24}) -> platform_alert;
get_sensor_type({specific,  16#25}) -> entity_presence;
get_sensor_type({specific,  16#26}) -> monitor_asic_ic;
get_sensor_type({specific,  16#27}) -> lan;
get_sensor_type({specific,  16#28}) -> management_subsystem_health;
get_sensor_type({specific,  16#29}) -> battery;
get_sensor_type({specific,  16#2a}) -> session;
get_sensor_type({specific,  16#2b}) -> version;
get_sensor_type({specific,  16#2c}) -> fru_state;
get_sensor_type({specific,  16#f0}) -> fru_hot_swap;
get_sensor_type({specific,  16#f1}) -> ipmb_physical;
get_sensor_type({specific,  16#f2}) -> module_hot_swap;
get_sensor_type({specific,  16#f3}) -> power_channel;
get_sensor_type({specific,  16#f4}) -> telco_alarm;
get_sensor_type(Type)               -> Type.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
map(threshold,                   A, _, _, _) ->
    get_threshold(A);
map(dmi_usage,                   A, _, _, _) ->
    get_dmi_usage(A);
map(state,                       A, _, _, _) ->
    get_assertion(A);
map(predictive_failure,          A, _, _, _) ->
    get_assertion(A);
map(limit,                       A, _, _, _) ->
    get_limit(A);
map(performance,                 A, _, _, _) ->
    get_performance(A);
map(severity,                    A, _, _, _) ->
    get_severity(A);
map(availability0,               A, _, _, _) ->
    get_availability08(A);
map(availability1,               A, _, _, _) ->
    get_availability09(A);
map(availability2,               A, _, _, _) ->
    get_availability0a(A);
map(redundancy,                  A, _, _, _) ->
    get_redundancy(A);
map(acpi_power_state,            A, _, _, _) ->
    get_acpi_power_state(A);
map(intrusion,                   A, _, _, _) ->
    get_intrusion(A);
map(violation_attempt,           A, _, _, _) ->
    get_violation_attempt(A);
map(processor,                   A, _, _, _) ->
    get_processor_failure(A);
map(power_supply,                A, _, _, D) ->
    get_power_supply_event(A, D);
map(power_unit,                  A, _, _, _) ->
    get_power_unit_event(A);
map(memory,                      A, _, _, D) ->
    get_memory_event(A, D);
map(drive_slot,                  A, _, _, _) ->
    get_drive_slot_state(A);
map(system_firmware,             A, _, C, _) ->
    get_system_firmware_event(A, C);
map(event_logging,               A, _, C, D) ->
    get_event_logging_event(A, C, D);
map(watchdog_old,                A, _, _, _) ->
    get_watchdog_old(A);
map(system_event,                A, _, C, _) ->
    get_system_event(A, C);
map(critical_interrupt,          A, _, _, _) ->
    get_critical_interrupt(A);
map(switch,                      A, _, _, _) ->
    get_switch_event(A);
map(chip_set,                    A, _, C, D) ->
    get_chip_set_event(A, C, D);
map(interconnect,                A, _, _, _) ->
    get_interconnect(A);
map(system_boot,                 A, _, C, D) ->
    get_system_boot_event(A, C, D);
map(boot_error,                  A, _, _, _) ->
    get_boot_error(A);
map(os_boot,                     A, _, _, _) ->
    get_os_boot_event(A);
map(os_shutdown,                 A, _, _, _) ->
    get_os_shutdown_event(A);
map(slot,                        A, _, C, D) ->
    get_slot_event(A, C, D);
map(system_acpi_power_state,     A, _, _, _) ->
    get_power_state(A);
map(watchdog,                    A, _, C, _) ->
    get_watchdog_new(A, C);
map(platform_alert,              A, _, _, _) ->
    get_platform_alert(A);
map(entity_presence,             A, B, _, _) ->
    get_entity_presence(A, B);
map(lan,                         A, _, _, _) ->
    get_lan_event(A);
map(management_subsystem_health, A, _, C, D) ->
    get_management_subsystem_health(A, C, D);
map(battery,                     A, _, _, _) ->
    get_battery_state(A);
map(session,                     A, _, C, D) ->
    get_session_event(A, C, D);
map(version,                     A, B, C, _) ->
    get_version_change(A, B, C);
map(fru_state,                   A, _, C, _) ->
    get_fru_event(A, C);
map(fru_hot_swap,                A, _, C, D) ->
    get_fru_hot_swap_event(A, C, D);
map(module_hot_swap,             A, _, _, _) ->
    get_module_hot_swap_event(A);
map(power_channel,               A, _, C, D) ->
    get_power_channel_notification(A, C, D);
map(_,                           A, B, C, D) ->
    [{offset, A}, {data2, C}, {data3, D}, {asserted, not eipmi_util:get_bool(B)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_threshold(16#00) ->
    [{sensor_value, lower_non_critical}, {tendency, going_low}];
get_threshold(16#01) ->
    [{sensor_value, lower_non_critical}, {tendency, going_high}];
get_threshold(16#02) ->
    [{sensor_value, lower_critical}, {tendency, going_low}];
get_threshold(16#03) ->
    [{sensor_value, lower_critical}, {tendency, going_high}];
get_threshold(16#04) ->
    [{sensor_value, lower_non_recoverable}, {tendency, going_low}];
get_threshold(16#05) ->
    [{sensor_value, lower_non_recoverable}, {tendency, going_high}];
get_threshold(16#06) ->
    [{sensor_value, upper_non_critical}, {tendency, going_low}];
get_threshold(16#07) ->
    [{sensor_value, upper_non_critical}, {tendency, going_high}];
get_threshold(16#08) ->
    [{sensor_value, upper_critical}, {tendency, going_low}];
get_threshold(16#09) ->
    [{sensor_value, upper_critical}, {tendency, going_high}];
get_threshold(16#0a) ->
    [{sensor_value, upper_non_recoverable}, {tendency, going_low}];
get_threshold(16#0b) ->
    [{sensor_value, upper_non_recoverable}, {tendency, going_high}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_dmi_usage(16#00) -> [{sensor_value, transition_to_idle}];
get_dmi_usage(16#01) -> [{sensor_value, transition_to_active}];
get_dmi_usage(16#02) -> [{sensor_value, transition_to_busy}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_assertion(16#00) -> [{sensor_value, deasserted}];
get_assertion(16#01) -> [{sensor_value, asserted}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_limit(16#00) -> [{sensor_value, not_exceeded}];
get_limit(16#01) -> [{sensor_value, exceeded}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_performance(16#00) -> [{sensor_value, met}];
get_performance(16#01) -> [{sensor_value, lags}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_severity(16#00) -> [{sensor_value, ok}];
get_severity(16#01) -> [{sensor_value, non_critical}];
get_severity(16#02) -> [{sensor_value, critical}];
get_severity(16#03) -> [{sensor_value, non_recoverable}];
get_severity(16#04) -> [{sensor_value, non_critical}];
get_severity(16#05) -> [{sensor_value, critical}];
get_severity(16#06) -> [{sensor_value, non_recoverable}];
get_severity(16#07) -> [{sensor_value, monitor}];
get_severity(16#08) -> [{sensor_value, informational}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_availability08(16#00) -> [{sensor_value, removed_or_absent}];
get_availability08(16#01) -> [{sensor_value, inserted_or_present}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_availability09(16#00) -> [{sensor_value, device_disabled}];
get_availability09(16#01) -> [{sensor_value, device_enabled}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_availability0a(16#00) -> [{sensor_value, to_running}];
get_availability0a(16#01) -> [{sensor_value, to_in_test}];
get_availability0a(16#02) -> [{sensor_value, to_power_off}];
get_availability0a(16#03) -> [{sensor_value, to_on_line}];
get_availability0a(16#04) -> [{sensor_value, to_off_line}];
get_availability0a(16#05) -> [{sensor_value, to_off_duty}];
get_availability0a(16#06) -> [{sensor_value, to_degraded}];
get_availability0a(16#07) -> [{sensor_value, to_power_save}];
get_availability0a(16#08) -> [{sensor_value, install_error}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_redundancy(16#00) -> [{sensor_value, full}];
get_redundancy(16#01) -> [{sensor_value, lost}];
get_redundancy(16#02) -> [{sensor_value, degraded}];
get_redundancy(16#03) -> [{sensor_value, none_from_degraded_or_full}];
get_redundancy(16#04) -> [{sensor_value, none_sufficient_resources}];
get_redundancy(16#05) -> [{sensor_value, none_insufficient_resources}];
get_redundancy(16#06) -> [{sensor_value, degraded_from_full}];
get_redundancy(16#07) -> [{sensor_value, degraded_from_none}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_acpi_power_state(16#00) -> [{sensor_value, d0}];
get_acpi_power_state(16#01) -> [{sensor_value, d1}];
get_acpi_power_state(16#02) -> [{sensor_value, d2}];
get_acpi_power_state(16#03) -> [{sensor_value, d3}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_intrusion(16#00) -> [{sensor_value, general_chassis}];
get_intrusion(16#01) -> [{sensor_value, drive_bay}];
get_intrusion(16#02) -> [{sensor_value, io_card_area}];
get_intrusion(16#03) -> [{sensor_value, processor_area}];
get_intrusion(16#04) -> [{sensor_value, lan_leash_lost}];
get_intrusion(16#05) -> [{sensor_value, unauthorized_dock}];
get_intrusion(16#06) -> [{sensor_value, fan_area}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_violation_attempt(16#00) -> [{sensor_value, secure_mode}];
get_violation_attempt(16#01) -> [{sensor_value, pre_boot_user_password}];
get_violation_attempt(16#02) -> [{sensor_value, pre_boot_setup_password}];
get_violation_attempt(16#03) -> [{sensor_value, pre_boot_network_password}];
get_violation_attempt(16#04) -> [{sensor_value, pre_boot_other_password}];
get_violation_attempt(16#05) -> [{sensor_value, pre_boot_out_of_band_access}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_processor_failure(16#00) ->
    [{sensor_value, internal_error}];
get_processor_failure(16#01) ->
    [{sensor_value, thermal_trip}];
get_processor_failure(16#02) ->
    [{sensor_value, frb1_bist_failure}];
get_processor_failure(16#03) ->
    [{sensor_value, frb2_hang_in_post_failure}];
get_processor_failure(16#04) ->
    [{sensor_value, frb3_startup_initialization_failure}];
get_processor_failure(16#05) ->
    [{sensor_value, configuration_error}];
get_processor_failure(16#06) ->
    [{sensor_value, uncorrectable_cpu_complex_error}];
get_processor_failure(16#07) ->
    [{sensor_value, presence_detected}];
get_processor_failure(16#08) ->
    [{sensor_value, disabled}];
get_processor_failure(16#09) ->
    [{sensor_value, terminator_presence_detected}];
get_processor_failure(16#0a) ->
    [{sensor_value, throttled_automatically}];
get_processor_failure(16#0b) ->
    [{sensor_value, uncorrectable_machine_check_error}];
get_processor_failure(16#0c) ->
    [{sensor_value, correctable_machine_check_error}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_power_supply_event(16#00,     _) ->
    [{sensor_value, presence_detected}];
get_power_supply_event(16#01,     _) ->
    [{sensor_value, failure_detected}];
get_power_supply_event(16#02,     _) ->
    [{sensor_value, predictive_failure}];
get_power_supply_event(16#03,     _) ->
    [{sensor_value, input_lost}];
get_power_supply_event(16#04,     _) ->
    [{sensor_value, input_lost_or_out_of_range}];
get_power_supply_event(16#05,     _) ->
    [{sensor_value, input_out_of_range_but_present}];
get_power_supply_event(16#06, 16#00) ->
    [{sensor_value, configuration_error}, {reason, vendor_mismatch}];
get_power_supply_event(16#06, 16#01) ->
    [{sensor_value, configuration_error}, {reason, revision_mismatch}];
get_power_supply_event(16#06, 16#02) ->
    [{sensor_value, configuration_error}, {reason, processor_missing}];
get_power_supply_event(16#06, 16#03) ->
    [{sensor_value, configuration_error}, {reason, power_supply_rating_mismatch}];
get_power_supply_event(16#06, 16#04) ->
    [{sensor_value, configuration_error}, {reason, voltage_rating_mismatch}];
get_power_supply_event(16#06,     _) ->
    [{sensor_value, configuration_error}, {reason, unknown}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_power_unit_event(16#00) -> [{sensor_value, power_off}];
get_power_unit_event(16#01) -> [{sensor_value, power_cycle}];
get_power_unit_event(16#02) -> [{sensor_value, power_down_240va}];
get_power_unit_event(16#03) -> [{sensor_value, power_down_interlock}];
get_power_unit_event(16#04) -> [{sensor_value, power_input_lost}];
get_power_unit_event(16#05) -> [{sensor_value, soft_power_control_failure}];
get_power_unit_event(16#06) -> [{sensor_value, failure_detected}];
get_power_unit_event(16#07) -> [{sensor_value, predictive_failure}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_memory_event(16#00,     _) -> [{sensor_value, correctable_ecc}];
get_memory_event(16#01,     _) -> [{sensor_value, uncorrectable_ecc}];
get_memory_event(16#02,     _) -> [{sensor_value, parity}];
get_memory_event(16#03,     _) -> [{sensor_value, memory_scrub_failed}];
get_memory_event(16#04,     _) -> [{sensor_value, device_disabled}];
get_memory_event(16#05,     _) -> [{sensor_value, error_logging_limit_reached}];
get_memory_event(16#06,     _) -> [{sensor_value, presence_detected}];
get_memory_event(16#07,     _) -> [{sensor_value, configuration_error}];
get_memory_event(16#08, 16#ff) -> [{sensor_value, spare}];
get_memory_event(16#08,    Id) -> [{sensor_value, spare}, {spare_id, Id}];
get_memory_event(16#09,     _) -> [{sensor_value, throttled_automatically}];
get_memory_event(16#0a,     _) -> [{sensor_value, critical_overtemperature}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_drive_slot_state(16#00) -> [{sensor_value, drive_presence}];
get_drive_slot_state(16#01) -> [{sensor_value, drive_fault}];
get_drive_slot_state(16#02) -> [{sensor_value, predictive_failure}];
get_drive_slot_state(16#03) -> [{sensor_value, hot_spare}];
get_drive_slot_state(16#04) -> [{sensor_value, consistency_check_in_progress}];
get_drive_slot_state(16#05) -> [{sensor_value, in_critical_array}];
get_drive_slot_state(16#06) -> [{sensor_value, in_failed_array}];
get_drive_slot_state(16#07) -> [{sensor_value, rebuild_in_progress}];
get_drive_slot_state(16#08) -> [{sensor_value, rebuild_aborted}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_system_firmware_event(16#00, 16#01) ->
    [{sensor_value, error}, {reason, no_memory_installed}];
get_system_firmware_event(16#00, 16#02) ->
    [{sensor_value, error}, {reason, no_memory_usable}];
get_system_firmware_event(16#00, 16#03) ->
    [{sensor_value, error}, {reason, hard_disk_failure}];
get_system_firmware_event(16#00, 16#04) ->
    [{sensor_value, error}, {reason, system_board_failure}];
get_system_firmware_event(16#00, 16#05) ->
    [{sensor_value, error}, {reason, diskette_subsystem_failure}];
get_system_firmware_event(16#00, 16#06) ->
    [{sensor_value, error}, {reason, hard_disk_controller_failure}];
get_system_firmware_event(16#00, 16#07) ->
    [{sensor_value, error}, {reason, keyboard_failure}];
get_system_firmware_event(16#00, 16#08) ->
    [{sensor_value, error}, {reason, boot_media_not_found}];
get_system_firmware_event(16#00, 16#09) ->
    [{sensor_value, error}, {reason, video_controller_failure}];
get_system_firmware_event(16#00, 16#0a) ->
    [{sensor_value, error}, {reason, no_video_device}];
get_system_firmware_event(16#00, 16#0b) ->
    [{sensor_value, error}, {reason, firmware_corrupted}];
get_system_firmware_event(16#00, 16#0c) ->
    [{sensor_value, error}, {reason, cpu_voltage_missing}];
get_system_firmware_event(16#00, 16#0d) ->
    [{sensor_value, error}, {reason, cpu_speed_matching_failure}];
get_system_firmware_event(16#00,     _) ->
    [{sensor_value, error}, {reason, unspecified}];
get_system_firmware_event(16#01,     _) ->
    [{sensor_value, hang}];
get_system_firmware_event(16#02, 16#01) ->
    [{sensor_value, progress}, {process, memory_initialization}];
get_system_firmware_event(16#02, 16#02) ->
    [{sensor_value, progress}, {process, hard_disk_initialization}];
get_system_firmware_event(16#02, 16#03) ->
    [{sensor_value, progress}, {process, sec_processor_initialization}];
get_system_firmware_event(16#02, 16#04) ->
    [{sensor_value, progress}, {process, user_authentication}];
get_system_firmware_event(16#02, 16#05) ->
    [{sensor_value, progress}, {process, user_initiated_system_setup}];
get_system_firmware_event(16#02, 16#06) ->
    [{sensor_value, progress}, {process, usb_resource_configuration}];
get_system_firmware_event(16#02, 16#07) ->
    [{sensor_value, progress}, {process, pci_resource_configuration}];
get_system_firmware_event(16#02, 16#08) ->
    [{sensor_value, progress}, {process, option_rom_initialization}];
get_system_firmware_event(16#02, 16#09) ->
    [{sensor_value, progress}, {process, video_initialization}];
get_system_firmware_event(16#02, 16#0a) ->
    [{sensor_value, progress}, {process, cache_initialization}];
get_system_firmware_event(16#02, 16#0b) ->
    [{sensor_value, progress}, {process, sm_bus_initialization}];
get_system_firmware_event(16#02, 16#0c) ->
    [{sensor_value, progress}, {process, keyboard_controller_initialization}];
get_system_firmware_event(16#02, 16#0d) ->
    [{sensor_value, progress}, {process, mgmt_controller_initialization}];
get_system_firmware_event(16#02, 16#0e) ->
    [{sensor_value, progress}, {process, docking_station_attachment}];
get_system_firmware_event(16#02, 16#0f) ->
    [{sensor_value, progress}, {process, enabling_docking_station}];
get_system_firmware_event(16#02, 16#10) ->
    [{sensor_value, progress}, {process, docking_station_ejection}];
get_system_firmware_event(16#02, 16#11) ->
    [{sensor_value, progress}, {process, disabling_docking_station}];
get_system_firmware_event(16#02, 16#12) ->
    [{sensor_value, progress}, {process, calling_operating_system_wake_up_vector}];
get_system_firmware_event(16#02, 16#13) ->
    [{sensor_value, progress}, {process, starting_operating_system_boot_process}];
get_system_firmware_event(16#02, 16#14) ->
    [{sensor_value, progress}, {process, baseboard_initialization}];
get_system_firmware_event(16#02, 16#16) ->
    [{sensor_value, progress}, {process, floppy_initialization}];
get_system_firmware_event(16#02, 16#17) ->
    [{sensor_value, progress}, {process, keyboard_test}];
get_system_firmware_event(16#02, 16#18) ->
    [{sensor_value, progress}, {process, pointing_device_test}];
get_system_firmware_event(16#02, 16#19) ->
    [{sensor_value, progress}, {process, prim_processor_initialization}];
get_system_firmware_event(16#02,     _) ->
    [{sensor_value, progress}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_event_logging_event(16#00, 16#ff,     _) ->
    [{sensor_value, disabled}, {component, memory}];
get_event_logging_event(16#00,    Id,     _) ->
    [{sensor_value, disabled}, {component, memory}, {component_id, Id}];
get_event_logging_event(16#02,     _,     _) ->
    [{sensor_value, log_area_cleared}];
get_event_logging_event(16#03,     _,     _) ->
    [{sensor_value, disabled_all}];
get_event_logging_event(16#04,     _,     _) ->
    [{sensor_value, sel_full}];
get_event_logging_event(16#05,     _, 16#ff) ->
    [{sensor_value, sel_almost_full}, {level, unknown}];
get_event_logging_event(16#05,     _,  Fill) ->
    [{sensor_value, sel_almost_full}, {level, Fill}];
get_event_logging_event(16#06, 16#ff, 16#ff) ->
    [{sensor_value, disabled}, {component, correctable_machine_check}];
get_event_logging_event(16#06,    Id, 16#80) ->
    [{sensor_value, disabled}, {component, correctable_machine_check}, {processor, Id}];
get_event_logging_event(16#06,    Id,     _) ->
    [{sensor_value, disabled}, {component, correctable_machine_check}, {instance, Id}];
get_event_logging_event(16#01, 16#ff, 16#ff) ->
    [{sensor_value, disabled}];
get_event_logging_event(16#01, RType,  Byte) ->
    Sensor = get_event(RType, Byte band 16#0f, (Byte band 16#10) bsr 4),
    [{sensor_value, disabled}, {component, Sensor}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_event(ReadingType, Offset, Assertion) ->
    {_, Type} = get_type(ReadingType, 16#00),
    [{sensor_type, Type}] ++ map(Type, Offset, Assertion, 16#ff, 16#ff).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_watchdog_old(16#00) -> [{sensor_value, bios_reset}];
get_watchdog_old(16#01) -> [{sensor_value, os_reset}];
get_watchdog_old(16#02) -> [{sensor_value, shut_down}];
get_watchdog_old(16#03) -> [{sensor_value, power_down}];
get_watchdog_old(16#04) -> [{sensor_value, power_cycle}];
get_watchdog_old(16#05) -> [{sensor_value, os_nmi}];
get_watchdog_old(16#06) -> [{sensor_value, expired}];
get_watchdog_old(16#07) -> [{sensor_value, os_non_nmi}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_system_event(16#00,     _) ->
    [{sensor_value, system_reconfigured}];
get_system_event(16#01,     _) ->
    [{sensor_value, oem_boot_event}];
get_system_event(16#02,     _) ->
    [{sensor_value, hardware_failure}];
get_system_event(16#04, 16#ff) ->
    [{sensor_value, pef_actions}, {pef_actions, []}];
get_system_event(16#04,  Data) ->
    [{sensor_value, pef_actions}, {pef_actions, get_pef_actions(Data)}];
get_system_event(16#03, 16#ff) ->
    [{sensor_value, auxiliary_log}];
get_system_event(16#03,  Data) ->
    Type = get_auxiliary_log_type(Data band 16#0f),
    Action = get_auxiliary_log_action(Data band 16#f0 bsr 4),
    [{sensor_value, auxiliary_log}, {log_action, Action}, {log_type, Type}].

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
get_critical_interrupt(16#00) -> [{sensor_value, front_panel_nmi}];
get_critical_interrupt(16#01) -> [{sensor_value, bus_timeout}];
get_critical_interrupt(16#02) -> [{sensor_value, io_channel_check_nmi}];
get_critical_interrupt(16#03) -> [{sensor_value, software_nmi}];
get_critical_interrupt(16#04) -> [{sensor_value, pci_perr}];
get_critical_interrupt(16#05) -> [{sensor_value, pci_serr}];
get_critical_interrupt(16#06) -> [{sensor_value, eisa_fail_safe_timeout}];
get_critical_interrupt(16#07) -> [{sensor_value, bus_correctable_error}];
get_critical_interrupt(16#08) -> [{sensor_value, bus_uncorrectable_error}];
get_critical_interrupt(16#09) -> [{sensor_value, fatal_nmi}];
get_critical_interrupt(16#0a) -> [{sensor_value, bus_fatal_error}];
get_critical_interrupt(16#0b) -> [{sensor_value, bus_degraded}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_switch_event(16#00) -> [{sensor_value, power_button_pressed}];
get_switch_event(16#01) -> [{sensor_value, sleep_button_pressed}];
get_switch_event(16#02) -> [{sensor_value, reset_button_pressed}];
get_switch_event(16#03) -> [{sensor_value, fru_latch_open}];
get_switch_event(16#04) -> [{sensor_value, fru_service_request_button}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_chip_set_event(16#01,   _,   _) ->
    [{sensor_value, thermal_trip}];
get_chip_set_event(16#00, Requested, Actual) ->
    [{sensor_value, R}] = get_power_state(Requested),
    [{sensor_value, A}] = get_power_state(Actual),
    [{sensor_value, soft_power_control_failure}, {requested, R}, {actual, A}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_interconnect(16#00) -> [{sensor_value, connected}];
get_interconnect(16#01) -> [{sensor_value, configuration_error}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_system_boot_event(16#00,   _,    _) ->
    [{sensor_value, initiated_by_power_up}];
get_system_boot_event(16#01,   _,    _) ->
    [{sensor_value, initiated_by_hard_reset}];
get_system_boot_event(16#02,   _,    _) ->
    [{sensor_value, initiated_by_warm_reset}];
get_system_boot_event(16#03,   _,    _) ->
    [{sensor_value, user_requested_pxe_boot}];
get_system_boot_event(16#04,   _,    _) ->
    [{sensor_value, automatic_boot_to_diagnostic}];
get_system_boot_event(16#05,   _,    _) ->
    [{sensor_value, software_initiated_hard_reset}];
get_system_boot_event(16#06,   _,    _) ->
    [{sensor_value, software_initiated_warm_reset}];
get_system_boot_event(16#07, Cau, Chan) ->
    [{sensor_value, system_restart},
     {cause, get_system_restart_cause(Cau)}]
        ++ case Chan of 16#ff -> []; _ -> [{channel, Chan}] end.

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
get_boot_error(16#00) -> [{sensor_value, no_bootable_media}];
get_boot_error(16#01) -> [{sensor_value, non_bootable_media_left_in_drive}];
get_boot_error(16#02) -> [{sensor_value, pxe_server_not_found}];
get_boot_error(16#03) -> [{sensor_value, invalid_boot_sector}];
get_boot_error(16#04) -> [{sensor_value, timeout_waiting_for_user}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_os_boot_event(16#00) -> [{sensor_value, completed_a}];
get_os_boot_event(16#01) -> [{sensor_value, completed_c}];
get_os_boot_event(16#02) -> [{sensor_value, completed_pxe}];
get_os_boot_event(16#03) -> [{sensor_value, completed_diagnostic}];
get_os_boot_event(16#04) -> [{sensor_value, completed_cdrom}];
get_os_boot_event(16#05) -> [{sensor_value, completed_rom}];
get_os_boot_event(16#06) -> [{sensor_value, completed}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_os_shutdown_event(16#00) -> [{sensor_value, critical_stop_during_startup}];
get_os_shutdown_event(16#01) -> [{sensor_value, critical_stop_during_runtime}];
get_os_shutdown_event(16#02) -> [{sensor_value, graceful_stop}];
get_os_shutdown_event(16#03) -> [{sensor_value, graceful_shutdown}];
get_os_shutdown_event(16#04) -> [{sensor_value, soft_shutdown}];
get_os_shutdown_event(16#05) -> [{sensor_value, agent_not_responding}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_slot_event(Value, Type, Num) ->
    T = get_slot_type(Type band 2#01111111),
    get_slot_event(Value) ++ [{slot_type, T}, {slot_number, Num}].
get_slot_event(16#00) -> [{sensor_value, fault_status_asserted}];
get_slot_event(16#01) -> [{sensor_value, identify_status_asserted}];
get_slot_event(16#02) -> [{sensor_value, installed}];
get_slot_event(16#03) -> [{sensor_value, ready_for_installation}];
get_slot_event(16#04) -> [{sensor_value, ready_for_removal}];
get_slot_event(16#05) -> [{sensor_value, off}];
get_slot_event(16#06) -> [{sensor_value, removal_request}];
get_slot_event(16#07) -> [{sensor_value, interlock_asserted}];
get_slot_event(16#08) -> [{sensor_value, disabled}];
get_slot_event(16#09) -> [{sensor_value, spare}].

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
get_power_state(16#00) -> [{sensor_value, {s0_g0, working}}];
get_power_state(16#01) -> [{sensor_value, {s1, clocks_stopped}}];
get_power_state(16#02) -> [{sensor_value, {s2, clocks_stopped}}];
get_power_state(16#03) -> [{sensor_value, {s3, suspend_to_ram}}];
get_power_state(16#04) -> [{sensor_value, {s4, suspend_to_disk}}];
get_power_state(16#05) -> [{sensor_value, {s5_g2, soft_off}}];
get_power_state(16#06) -> [{sensor_value, {s4_s5, soft_off}}];
get_power_state(16#07) -> [{sensor_value, {g3, mechanical_off}}];
get_power_state(16#08) -> [{sensor_value, {s1_s2_s3, sleeping}}];
get_power_state(16#09) -> [{sensor_value, {g1, sleeping}}];
get_power_state(16#0a) -> [{sensor_value, {s5, override}}];
get_power_state(16#0b) -> [{sensor_value, legacy_on}];
get_power_state(16#0c) -> [{sensor_value, legacy_off}];
get_power_state(_)     -> [{sensor_value, unknown}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_watchdog_new(Value, Data) ->
    get_watchdog_new(Value)
        ++ get_interrupt_type(Data band 16#f0 bsr 4)
        ++ get_timer_use(Data band 16#0f).
get_watchdog_new(16#00) -> [{sensor_value, timer_expired}];
get_watchdog_new(16#01) -> [{sensor_value, hard_reset}];
get_watchdog_new(16#02) -> [{sensor_value, power_down}];
get_watchdog_new(16#03) -> [{sensor_value, power_cycle}];
get_watchdog_new(16#08) -> [{sensor_value, timer_interrupt}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_interrupt_type(0) -> [{interrupt, none}];
get_interrupt_type(1) -> [{interrupt, smi}];
get_interrupt_type(2) -> [{interrupt, nmi}];
get_interrupt_type(3) -> [{interrupt, messaging}];
get_interrupt_type(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_timer_use(1) -> [{timer_use, bios_frb2}];
get_timer_use(2) -> [{timer_use, bios}];
get_timer_use(3) -> [{timer_use, os_load}];
get_timer_use(4) -> [{timer_use, sms}];
get_timer_use(5) -> [{timer_use, oem}];
get_timer_use(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_platform_alert(16#00) -> [{sensor_value, generated_page}];
get_platform_alert(16#01) -> [{sensor_value, generated_lan_alert}];
get_platform_alert(16#02) -> [{sensor_value, generated_event_trap}];
get_platform_alert(16#03) -> [{sensor_value, generated_snmp_trap}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_entity_presence(16#00, _) -> [{sensor_value, present}];
get_entity_presence(16#01, _) -> [{sensor_value, absent}];
get_entity_presence(16#02, 0) -> [{sensor_value, disabled}];
get_entity_presence(16#02, 1) -> [{sensor_value, enabled}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_lan_event(16#00) -> [{sensor_value, heartbeat_lost}];
get_lan_event(16#01) -> [{sensor_value, heartbeat}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_management_subsystem_health(16#00,     _,     _) ->
    [{sensor_value, sensor_access_degraded}];
get_management_subsystem_health(16#01,     _,     _) ->
    [{sensor_value, controller_access_degraded}];
get_management_subsystem_health(16#02,     _,     _) ->
    [{sensor_value, controller_offline}];
get_management_subsystem_health(16#03,     _,     _) ->
    [{sensor_value, controller_unavailable}];
get_management_subsystem_health(16#04, 16#ff,     _) ->
    [{sensor_value, sensor_failure}];
get_management_subsystem_health(16#04,   Num,     _) ->
    [{sensor_value, sensor_failure}, {failed_sensor_number, Num}];
get_management_subsystem_health(16#05, 16#ff, 16#ff) ->
    [{sensor_value, fru_failure}];
get_management_subsystem_health(16#05, Data1, Data2) ->
    [{sensor_value, fru_failure}] ++ get_addr(<<Data2:8, Data1:8, 0:8>>).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_battery_state(16#00) -> [{sensor_value, low}];
get_battery_state(16#01) -> [{sensor_value, failed}];
get_battery_state(16#02) -> [{sensor_value, presence_detected}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_event(Value, User, Data) ->
    get_session_event(Value) ++ get_session_data(User, Data).
get_session_event(16#00) -> [{sensor_value, activated}];
get_session_event(16#01) -> [{sensor_value, deactivated}];
get_session_event(16#02) -> [{sensor_value, invalid_user_or_password}];
get_session_event(16#03) -> [{sensor_value, invalid_password_disable}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session_data(16#ff, 16#ff) ->
    [];
get_session_data(A, B) ->
    get_session_data_(<<A:8>>, <<B:8>>).
get_session_data_(<<_:2, UserId:6>>, <<_:2, 0:2, Channel:4>>) ->
    [{user_id, UserId}] ++ get_channel(Channel);
get_session_data_(<<_:2, UserId:6>>, <<_:2, 1:2, Channel:4>>) ->
    [{user_id, UserId}, {cause, close_session_command}] ++ get_channel(Channel);
get_session_data_(<<_:2, UserId:6>>, <<_:2, 2:2, Channel:4>>) ->
    [{user_id, UserId}, {cause, timeout}] ++ get_channel(Channel);
get_session_data_(<<_:2, UserId:6>>, <<_:2, 3:2, Channel:4>>) ->
    [{user_id, UserId}, {cause, configuration_change}] ++ get_channel(Channel).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_version_change(Val1, Val2, Type) ->
    get_version_change(Val1, Val2) ++ get_version_change_type(Type).
get_version_change(16#00, _) ->
    [{sensor_value, hardware_change_detected}];
get_version_change(16#01, _) ->
    [{sensor_value, firmware_change_detected}];
get_version_change(16#02, _) ->
    [{sensor_value, hardware_incompatibility_detected}];
get_version_change(16#03, _) ->
    [{sensor_value, firmware_incompatibility_detected}];
get_version_change(16#04, _) ->
    [{sensor_value, unsupported_hardware_version}];
get_version_change(16#05, _) ->
    [{sensor_value, unsupported_firmware_version}];
get_version_change(16#06, 0) ->
    [{sensor_value, successful_hardware_change_detected}];
get_version_change(16#06, 1) ->
    [{sensor_value, unsuccessful_hardware_change_detected}];
get_version_change(16#07, 0) ->
    [{sensor_value, successful_firmware_change_detected}];
get_version_change(16#07, 1) ->
    [{sensor_value, unsuccessful_firmware_change_detected}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_version_change_type(16#01) ->
    [{change_type, management_controller_device_id}];
get_version_change_type(16#02) ->
    [{change_type, management_controller_firmware_revision}];
get_version_change_type(16#03) ->
    [{change_type, management_controller_device_revision}];
get_version_change_type(16#04) ->
    [{change_type, management_controller_manufacturer_id}];
get_version_change_type(16#05) ->
    [{change_type, management_controller_ipmi_version}];
get_version_change_type(16#06) ->
    [{change_type, management_controller_auxiliary_firmware_id}];
get_version_change_type(16#07) ->
    [{change_type, management_controller_firmware_boot_block}];
get_version_change_type(16#08) ->
    [{change_type, management_controller_firmware}];
get_version_change_type(16#09) ->
    [{change_type, system_firmware_change}];
get_version_change_type(16#0a) ->
    [{change_type, sm_bios_change}];
get_version_change_type(16#0b) ->
    [{change_type, operating_system_change}];
get_version_change_type(16#0c) ->
    [{change_type, operating_system_loader_change}];
get_version_change_type(16#0d) ->
    [{change_type, partition_change}];
get_version_change_type(16#0e) ->
    [{change_type, management_software_agent_change}];
get_version_change_type(16#0f) ->
    [{change_type, management_software_application_change}];
get_version_change_type(16#10) ->
    [{change_type, management_software_middleware_change}];
get_version_change_type(16#11) ->
    [{change_type, programmable_hardware_change}];
get_version_change_type(16#12) ->
    [{change_type, fru_module_change}];
get_version_change_type(16#13) ->
    [{change_type, fru_component_change}];
get_version_change_type(16#14) ->
    [{change_type, fru_replaced_with_equivalent_version}];
get_version_change_type(16#15) ->
    [{change_type, fru_replaced_with_newer_version}];
get_version_change_type(16#16) ->
    [{change_type, fru_replaced_with_older_version}];
get_version_change_type(16#17) ->
    [{change_type, fru_hardware_configuration_change}];
get_version_change_type(_)     ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_event(Value, 16#ff) ->
    [{sensor_value, get_fru_state(Value)}];
get_fru_event(Value, Data) ->
    [{sensor_value, get_fru_state(Value)}] ++ get_fru_event(<<Data:8>>).
get_fru_event(<<Cause:4, Prev:4>>) ->
    get_fru_state_cause(Cause) ++ [{previous_value, get_fru_state(Prev)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_state(16#00) -> not_installed;
get_fru_state(16#01) -> inactive;
get_fru_state(16#02) -> activation_requested;
get_fru_state(16#03) -> activation_in_progress;
get_fru_state(16#04) -> active;
get_fru_state(16#05) -> deactivation_requested;
get_fru_state(16#06) -> deactivation_in_progress;
get_fru_state(16#07) -> communication_lost.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_state_cause(16#00) -> [{cause, normal_state_change}];
get_fru_state_cause(16#01) -> [{cause, external_software}];
get_fru_state_cause(16#02) -> [{cause, latch}];
get_fru_state_cause(16#03) -> [{cause, hot_swap}];
get_fru_state_cause(16#04) -> [{cause, internal_software}];
get_fru_state_cause(16#05) -> [{cause, communication_lost}];
get_fru_state_cause(16#06) -> [{cause, communication_lost}];
get_fru_state_cause(16#07) -> [{cause, unexpected_extraction}];
get_fru_state_cause(16#08) -> [{cause, update}];
get_fru_state_cause(16#09) -> [{cause, no_ipmb_address}];
get_fru_state_cause(16#0a) -> [{cause, unexpedcted_deactivation}];
get_fru_state_cause(    _) -> [{cause, unknown}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_hot_swap_event(Value, 16#ff, 16#ff) ->
    [{sensor_value, get_fru_state(Value)}];
get_fru_hot_swap_event(Value, Data, FruId) ->
    [{sensor_value, get_fru_state(Value)}, {fru_id, FruId}]
        ++ get_fru_hot_swap_event(<<Data:8>>).
get_fru_hot_swap_event(<<C:4, P:4>>) ->
    get_fru_hot_swap_state_cause(C) ++ [{previous_value, get_fru_state(P)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_hot_swap_state_cause(16#00) ->
    [{cause, normal}];
get_fru_hot_swap_state_cause(16#01) ->
    [{cause, shelf_manager}];
get_fru_hot_swap_state_cause(16#02) ->
    [{cause, operator}];
get_fru_hot_swap_state_cause(16#03) ->
    [{cause, fru_action}];
get_fru_hot_swap_state_cause(16#04) ->
    [{cause, communication_lost_or_regained}];
get_fru_hot_swap_state_cause(16#05) ->
    [{cause, communication_lost_or_regained_locally}];
get_fru_hot_swap_state_cause(16#06) ->
    [{cause, surprise}];
get_fru_hot_swap_state_cause(16#07) ->
    [{cause, provided_information}];
get_fru_hot_swap_state_cause(16#08) ->
    [{cause, invalid_hardware_address}];
get_fru_hot_swap_state_cause(    _) ->
    [{cause, unknown}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_module_hot_swap_event(0) -> [{sensor_value, handle_closed}];
get_module_hot_swap_event(1) -> [{sensor_value, handle_opened}];
get_module_hot_swap_event(2) -> [{sensor_value, quiesced}];
get_module_hot_swap_event(3) -> [{sensor_value, backend_power_failure}];
get_module_hot_swap_event(4) -> [{sensor_value, backend_power_shutdown}];
get_module_hot_swap_event(5) -> [{sensor_value, rtm_present}];
get_module_hot_swap_event(6) -> [{sensor_value, rtm_absent}];
get_module_hot_swap_event(7) -> [{sensor_value, rtm_compatible}];
get_module_hot_swap_event(8) -> [{sensor_value, rtm_incompatible}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_power_channel_notification(0, Bitfield, _) ->
    get_global_power_channel_notification(<<Bitfield:8>>);
get_power_channel_notification(1, Bitfield, Channel) ->
    get_local_power_channel_notification(<<Bitfield:8>>, Channel).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_global_power_channel_notification(
  <<?EIPMI_RESERVED:4, A:1, B:1, C:1, D:1>>) ->
    [{sensor_value, global_state_change},
     {redundant_pm_provides_payload_power, eipmi_util:get_bool(A)},
     {payload_power, case B of 0 -> not_good; _ -> good end},
     {management_power, case C of 0 -> not_good; _ -> good end},
     {role, case D of 0 -> redundant; _ -> primary end}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_local_power_channel_notification(
  <<?EIPMI_RESERVED:1, A:1, B:1, C:1, D:1, E:1, F:1, G:1>>, Channel) ->
    [{sensor_value, local_state_change},
     {channel, Channel},
     {power_on_asserted, eipmi_util:get_bool(A)},
     {payload_power_overcurrent_detected, eipmi_util:get_bool(B)},
     {payload_power_enabled, eipmi_util:get_bool(C)},
     {enable_asserted, eipmi_util:get_bool(D)},
     {management_power_overcurrent_detected, eipmi_util:get_bool(E)},
     {management_power_enabled, eipmi_util:get_bool(F)},
     {ps1_asserted, eipmi_util:get_bool(G)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_channel(0) -> [];
get_channel(Channel) -> [{channel, Channel}].
