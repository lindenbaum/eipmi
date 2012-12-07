
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
%%% A module providing reading and decoding functionality for Sensor Data
%%% Records (SDRs).
%%% @end
%%%=============================================================================

-module(eipmi_sdr).

-export([get_repository/1,
         maybe_get_repository/2,
         get/2,
         get_sensor_reading/3,
         get_sensor_reading/2,
         convert/2]).

-include("eipmi.hrl").

-define(MAX_READ_COUNT, 16).

-define(READ, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR}).
-define(GET_INFO, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR_REPOSITORY_INFO}).
-define(RESERVE, {?IPMI_NETFN_STORAGE_REQUEST, ?RESERVE_SDR_REPOSITORY}).
-define(GET_READING, {?IPMI_NETFN_SENSOR_EVENT_REQUEST, ?GET_SENSOR_READING}).

-type record_type() ::
        full |
        compact |
        event_only |
        entity_association |
        device_relative_entity_association |
        generic_device_locator |
        fru_device_locator |
        management_controller_device_locator |
        management_controller_confirmation |
        bmc_message_channel_info |
        oem |
        reserved.

-type property() ::
        eipmi_sensor:addr() |
        {record_id, non_neg_integer()} |
        {sdr_version, string()} |
        {record_type, record_type()} |
        {record_length, non_neg_integer()} |
        {nominal_reading, non_neg_integer()} |
        {nominal_maximum, non_neg_integer()} |
        {nominal_minimum, non_neg_integer()} |
        {maximum_reading, non_neg_integer()} |
        {minimum_reading, non_neg_integer()} |
        {container, [eipmi_sensor:entity()]} |
        {containee, [eipmi_sensor:entity() | eipmi_sensor:addr()]} |
        {linked_records_exist, boolean()} |
        {presence_sensor_always_accessible, boolean()} |
        {access_addr, non_neg_integer()} |
        {device_type, non_neg_integer()} |
        {device_type_modifier, non_neg_integer()} |
        {id, string()} |
        {device_support, [atom()]} |
        {firmware_version, string()} |
        {ipmi_version, string()} |
        {manufacturer_id, non_neg_integer()} |
        {product_id, non_neg_integer()} |
        {guid, string()} |
        {msg_channel_info,
         [{transmit_support, boolean()} |
          {sensor_lun, non_neg_integer()} |
          {protocol, ipmb | {icmb, number()} | {sm_bus, number()} |
           system_format | {oem, number()}}]} |
        {messaging_interrupt,
         {irq, non_neg_integer()} |
         {pci, string()} |
         smi |
         sci |
         {interrupt, non_neg_integer()} |
         assigned} |
        {event_message_buffer_interrupt,
         {irq, non_neg_integer()} |
         {pci, string()} |
         smi |
         sci |
         {interrupt, non_neg_integer()} |
         assigned} |
        {data, binary()} |
        {percentage, boolean()} |
        {sensor_format, unsigned | ones_complement | twos_complement} |
        {sensor_rate,
         eipmi_sensor:unit() |
         {eipmi_sensor:unit(), eipmi_sensor:unit()} |
         {eipmi_sensor:unit(), per, eipmi_sensor:unit()}} |
        {sensor_unit, eipmi_sensor:unit()} |
        {sensor_tolerance, {number(), eipmi_sensor:unit()}} |
        {sensor_resolution, {number(), eipmi_sensor:unit()}} |
        {sensor_accuracy, {number(), percent}} |
        {sensor_coefficients,
         {linear | ln | log10 | log2 | e | exp10 | exp2 | '1/x' | sqr | cube |
          sqrt | 'cube-1' | non_linear | oem_non_linear,
          integer(), integer(), integer(), integer()}} |
        {sensor_number, non_neg_integer()} |
        {sensor_numbers, [non_neg_integer()]} |
        {sensor_direction, input | output}.

-type entry() ::
        {record_type(), [property()]} |
        {sdr_info,
         [{version, string()} |
          {entries, non_neg_integer()} |
          {free_space, non_neg_integer()} |
          {most_recent_addition, non_neg_integer()} |
          {most_recent_erase, non_neg_integer()} |
          {overflow, boolean()} |
          {operations,
           [delete | partial_add | reserve | get_allocation_info]}]}.

-type reading() ::
        eipmi_sensor:value() |
        {sensor_reading, {number(), eipmi_sensor:unit()}}.

-export_type([record_type/0, property/0, entry/0, reading/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Read all entries from the sensor data record repository (SDR).
%% @end
%%------------------------------------------------------------------------------
-spec get_repository(pid()) -> [entry()].
get_repository(SessionPid) ->
    maybe_get_repository(SessionPid, []).

%%------------------------------------------------------------------------------
%% @doc
%% Read all entries from the sensor data record repository (SDR) *only* when
%% the SDR repository changed since the last read.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_get_repository(pid(), [entry()]) -> [entry()].
maybe_get_repository(SessionPid, OldSdrRepository) ->
    {ok, SdrInfo} = eipmi_session:rpc(SessionPid, ?GET_INFO, []),
    OldSdrInfo = proplists:get_value(sdr_info, OldSdrRepository, []),
    LastRecentActionTimestamp = get_recent_action_timestamp(OldSdrInfo),
    CurrentRecentActionTimestamp = get_recent_action_timestamp(SdrInfo),
    case CurrentRecentActionTimestamp > LastRecentActionTimestamp of
        true ->
            Reserve = needs_reservation(SdrInfo),
            Entries = proplists:get_value(entries, SdrInfo),
            maybe_get_repository(Entries, SessionPid, Reserve, SdrInfo);
        false ->
            OldSdrRepository
    end.
maybe_get_repository(0, _SessionPid, _Reserve, SdrInfo) ->
    [{sdr_info, SdrInfo}];
maybe_get_repository(_, SessionPid, Reserve, SdrInfo) ->
    [{sdr_info, SdrInfo}]
        ++ maybe_reserve(SessionPid, fun read_all/2, [SessionPid], Reserve).

%%------------------------------------------------------------------------------
%% @doc
%% Read one specific entry from the sensor data record repository (SDR).
%% @end
%%------------------------------------------------------------------------------
-spec get(pid(), non_neg_integer()) -> entry().
get(SessionPid, RecordId) ->
    {ok, SdrInfo} = eipmi_session:rpc(SessionPid, ?GET_INFO, []),
    Args = [SessionPid, RecordId],
    Reserve = lists:member(reserve, proplists:get_value(operations, SdrInfo)),
    {_, Entry} = maybe_reserve(SessionPid, fun read_one/3, Args, Reserve),
    Entry.

%%------------------------------------------------------------------------------
%% @doc
%% Return the current reading of a specific sensor referred to by its number.
%% @end
%%------------------------------------------------------------------------------
-spec get_sensor_reading(pid(), non_neg_integer(), [entry()]) -> [reading()].
get_sensor_reading(SessionPid, SensorNumber, SdrRepository) ->
    Pred = fun(Ps) -> lists:member({sensor_number, SensorNumber}, Ps) end,
    [Sdr] = [Record || Record = {_, Ps} <- SdrRepository, Pred(Ps)],
    get_sensor_reading(SessionPid, Sdr).

%%------------------------------------------------------------------------------
%% @doc
%% Return the current reading of a specific sensor referred to by its number.
%% @end
%%------------------------------------------------------------------------------
-spec get_sensor_reading(pid(), {full | compact, [property()]}) -> [reading()].
get_sensor_reading(SessionPid, {Type, Properties})
  when Type =:= full orelse Type =:= compact ->
    SensorType = proplists:get_value(sensor_type, Properties),
    Args = [lists:keyfind(sensor_number, 1, Properties)],
    {ok, Result} = eipmi_session:rpc(SessionPid, ?GET_READING, Args),
    State = proplists:get_value(state, Result),
    Raw = proplists:get_value(raw_reading, Result),
    Reading = get_reading(sensor_reading, Raw, Properties),
    get_sensor_reading_(SensorType, State) ++ Reading.
get_sensor_reading_(threshold, <<S:6/bitstring, _:2>>) ->
    [eipmi_sensor:get_value(threshold, O, 1, 16#ff, 16#ff) || O <- get_offsets(S)];
get_sensor_reading_(Type, S) ->
    [eipmi_sensor:get_value(Type, O, 1, 16#ff, 16#ff) || O <- get_offsets(S)].

%%------------------------------------------------------------------------------
%% @doc
%% Convert a raw sensor reading into a meaningful value, using the sensors
%% full SDR record.
%% @end
%%------------------------------------------------------------------------------
-spec convert(binary(), {full, [property()]}) -> [reading()].
convert(Raw, {full, Properties}) ->
    get_reading(sensor_reading, Raw, Properties).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec maybe_reserve(pid(), function(), [term()], boolean()) ->
                           [entry()] | {non_neg_integer(), entry()}.
maybe_reserve(SessionPid, Fun, Args, true) ->
    {ok, Reserve} = eipmi_session:rpc(SessionPid, ?RESERVE, []),
    ReservationId = proplists:get_value(reservation_id, Reserve),
    erlang:apply(Fun, Args ++ [ReservationId]);
maybe_reserve(_SessionPid, Fun, Args, false) ->
    erlang:apply(Fun, Args ++ [16#0000]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_all(SessionPid, ReservationId) ->
    read_all(SessionPid, ReservationId, 16#0000, []).
read_all(_SessionPid, _ReservationId, 16#ffff, Acc) ->
    lists:reverse(Acc);
read_all(SessionPid, ReservationId, RecordId, Acc) ->
    {NextRecordId, Entry} = read_one(SessionPid, RecordId, ReservationId),
    read_all(SessionPid, ReservationId, NextRecordId, [Entry | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_one(SessionPid, RecordId, ReservationId) ->
    {NextRecordId, Header} = read_header(SessionPid, RecordId, ReservationId),
    Length = proplists:get_value(record_length, Header),
    Type = proplists:get_value(record_type, Header),
    Body = read_body(SessionPid, RecordId, ReservationId, Type, Length),
    {NextRecordId, {Type, proplists:delete(record_type, Header) ++ Body}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_header(Pid, Record, Reservation) ->
    Ps = [{reservation_id, Reservation}, {record_id, Record},
          {offset, 0}, {count, 5}],
    {ok, Read} = eipmi_session:rpc(Pid, ?READ, Ps),
    NextRecordId = proplists:get_value(next_record_id, Read),
    {NextRecordId, decode_header(proplists:get_value(data, Read))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_body(SessionPid, RecordId, ReservationId, Type, Length) ->
    decode_body(Type, read_body(SessionPid,
                                RecordId,
                                ReservationId,
                                Length,
                                ?MAX_READ_COUNT,
                                {0, <<>>})).
read_body(_Pid, _Record, _Reservation, Len, _Count, {Len, Data}) ->
    Data;
read_body(Pid, Record, Reservation, Len, Count, Acc = {Offset, _})
  when Offset + Count =< Len ->
    NewAcc = do_read(Pid, Record, Reservation, Count, Acc),
    read_body(Pid, Record, Reservation, Len, Count, NewAcc);
read_body(Pid, Record, Reservation, Len, Count, Acc = {Offset, _}) ->
    NewAcc = do_read(Pid, Record, Reservation, Len - Offset, Acc),
    read_body(Pid, Record, Reservation, Len, Count, NewAcc).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_read(Pid, Record, Reservation, Count, {Offset, Acc}) ->
    Ps = [{reservation_id, Reservation}, {record_id, Record},
          {offset, 5 + Offset}, {count, Count}],
    {ok, Read} = eipmi_session:rpc(Pid, ?READ, Ps),
    {Offset + Count, <<Acc/binary, (proplists:get_value(data, Read))/binary>>}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_header(<<Id:16/little, Version:1/binary, Type:8, L:8>>) ->
    T = get_record_type(Type),
    [V1 | VRest] = lists:reverse(eipmi_util:from_bcd_plus(Version)),
    V = [V1 | [$. | VRest]],
    [{record_id, Id}, {sdr_version, V}, {record_type, T}, {record_length, L}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_body(full, Data) ->
    decode_full_sensor_record(Data);
decode_body(compact, Data) ->
    decode_compact_sensor_record(Data);
decode_body(event_only, Data) ->
    decode_event_only_record(Data);
decode_body(entity_association, Data) ->
    decode_entity_association_record(Data);
decode_body(device_relative_entity_association, Data) ->
    decode_device_relative_entity_association_record(Data);
decode_body(generic_device_locator, Data) ->
    decode_generic_device_locator_record(Data);
decode_body(fru_device_locator, Data) ->
    decode_fru_device_locator_record(Data);
decode_body(management_controller_device_locator, Data) ->
    decode_management_controller_device_locator_record(Data);
decode_body(management_controller_confirmation, Data) ->
    decode_management_controller_confirmation_record(Data);
decode_body(bmc_message_channel_info, Data) ->
    decode_bmc_message_channel_info_record(Data);
decode_body(oem, Data) ->
    decode_oem_record(Data).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_record_type(16#01) -> full;
get_record_type(16#02) -> compact;
get_record_type(16#03) -> event_only;
get_record_type(16#08) -> entity_association;
get_record_type(16#09) -> device_relative_entity_association;
get_record_type(16#10) -> generic_device_locator;
get_record_type(16#11) -> fru_device_locator;
get_record_type(16#12) -> management_controller_device_locator;
get_record_type(16#13) -> management_controller_confirmation;
get_record_type(16#14) -> bmc_message_channel_info;
get_record_type(16#c0) -> oem;
get_record_type(_)     -> reserved.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_full_sensor_record(
  <<SensorAddr:2/binary, SensorNumber:8, EntityId:8, EntityInstance:1/binary,
    _:16, SensorType:8, ReadingType:8, _:48, SensorUnit:3/binary,
    SensorProperties:7/binary, _:8, NominalReading:1/binary,
    NominalMaximum:1/binary, NominalMinimum:1/binary, MaximumReading:1/binary,
    MinimumReading:1/binary, _:64, ?EIPMI_RESERVED:16, _OEM:8, _IdLen:8,
    Id/binary>>) ->
    {_, Type} = eipmi_sensor:get_type(ReadingType, SensorType),
    Units = get_units(SensorUnit),
    Properties = get_sensor_properties(SensorProperties, Units),
    eipmi_sensor:get_addr(SensorAddr)
        ++ get_sensor_numbers(SensorNumber, 0)
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{sensor_type, Type}] ++ Units ++ Properties
        ++ get_reading(nominal_reading, NominalReading, Units ++ Properties)
        ++ get_reading(nominal_maximum, NominalMaximum, Units ++ Properties)
        ++ get_reading(nominal_minimum, NominalMinimum, Units ++ Properties)
        ++ get_reading(maximum_reading, MaximumReading, Units ++ Properties)
        ++ get_reading(minimum_reading, MinimumReading, Units ++ Properties)
        ++ [{id, eipmi_util:binary_to_string(Id)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_compact_sensor_record(
  <<SensorAddr:2/binary, SensorNumber:8, EntityId:8, EntityInstance:1/binary,
    _:16, SensorType:8, ReadingType:8, _:48, SensorUnit:3/binary, Direction:2,
    Modifier:2, Count:4, Sharing:1/binary, _:16, ?EIPMI_RESERVED:24, _OEM:8,
    _IdLen:8, Id/binary>>) ->
    {_, Type} = eipmi_sensor:get_type(ReadingType, SensorType),
    eipmi_sensor:get_addr(SensorAddr)
        ++ get_sensor_numbers(SensorNumber, Count)
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{sensor_type, Type}]
        ++ get_units(SensorUnit)
        ++ get_direction(Direction)
        ++ get_id(Count, Sharing, Modifier, eipmi_util:binary_to_string(Id)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_event_only_record(
  <<SensorAddr:2/binary, SensorNumber:8, EntityId:8, EntityInstance:1/binary,
    SensorType:8, ReadingType:8, Direction:2, Modifier:2, Count:4,
    Sharing:1/binary, ?EIPMI_RESERVED:8, _OEM:8, _IdLen:8, Id/binary>>) ->
    {_, Type} = eipmi_sensor:get_type(ReadingType, SensorType),
    eipmi_sensor:get_addr(SensorAddr)
        ++ get_sensor_numbers(SensorNumber, Count)
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{sensor_type, Type}]
        ++ get_direction(Direction)
        ++ get_id(Count, Sharing, Modifier, eipmi_util:binary_to_string(Id)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_entity_association_record(
  <<Id:8, Instance:1/binary, Range:1, RecordLink:1, Presence:1,
    ?EIPMI_RESERVED:5, Data/binary>>) ->
    [{container, eipmi_sensor:get_entity(Id, Instance)},
     {linked_records_exist, eipmi_util:get_bool(RecordLink)},
     {presence_sensor_always_accessible, eipmi_util:get_bool(Presence)}]
        ++ get_contained(Range, direct, Data).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_device_relative_entity_association_record(
  <<Id:8, Inst:1/binary, Addr:2/binary, Range:1, RecordLink:1, Presence:1,
    ?EIPMI_RESERVED:5, Data:16/binary, _/binary>>) ->
    [{container, eipmi_sensor:get_entity(Id, Inst) ++ get_relative_addr(Addr)},
     {linked_records_exist, eipmi_util:get_bool(RecordLink)},
     {presence_sensor_always_accessible, eipmi_util:get_bool(Presence)}]
        ++ get_contained(Range, relative, Data).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_generic_device_locator_record(
  <<AccessAddr:7, ?EIPMI_RESERVED:1, Addr:3/binary, ?EIPMI_RESERVED:8,
    Type:8, TypeModifier:8, EntityId:8, EntityInstance:1/binary, _OEM:8,
    _IdLen:8, Id/binary>>) ->
    [{access_addr, AccessAddr}]
        ++ get_generic_addr(Addr)
        ++ [{device_type, Type}, {device_type_modifier, TypeModifier}]
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{id, eipmi_util:binary_to_string(Id)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_fru_device_locator_record(
  <<AccessAddr:7, ?EIPMI_RESERVED:1, DeviceAddr:3/binary, ?EIPMI_RESERVED:8,
    Type:8, TypeModifier:8, EntityId:8, EntityInstance:1/binary, _OEM:8,
    _IdLen:8, Id/binary>>) ->
    [{access_addr, AccessAddr}]
        ++ eipmi_sensor:get_addr(DeviceAddr)
        ++ [{device_type, Type}, {device_type_modifier, TypeModifier}]
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{id, eipmi_util:binary_to_string(Id)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_device_locator_record(
  <<Addr:7, ?EIPMI_RESERVED:5, Channel:4, _PowerStateNotification:4,
    _GlobalInitialization:4, Capabilities:8, ?EIPMI_RESERVED:24,
    EntityId:8, EntityInstance:1/binary, _OEM:8, _IdLen:8, Id/binary>>) ->
    eipmi_sensor:get_addr(<<Addr:7, 0:1, 0:4, Channel:4>>)
        ++ [{device_support, eipmi_response:get_device_support(Capabilities)}]
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ [{id, eipmi_util:binary_to_string(Id)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_confirmation_record(
  <<?EIPMI_RESERVED:1, Major:7, Minor:8, IPMIVersion:1/binary,
    Manufacturer:24/little, Product:16/little, GUID/binary>>) ->
    [Iv1 | IvRest] = lists:reverse(eipmi_util:from_bcd_plus(IPMIVersion)),
    [{firmware_version, eipmi_util:format("~B.~B", [Major, Minor])},
     {ipmi_version, [Iv1 | [$. | IvRest]]},
     {manufacturer_id, Manufacturer},
     {product_id, Product},
     {guid, eipmi_util:binary_to_string(GUID)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_bmc_message_channel_info_record(
  <<C0:1/binary, C1:1/binary, C2:1/binary, C3:1/binary, C4:1/binary,
    C5:1/binary, C6:1/binary, C7:1/binary, MsgInterrupt:8, EventInterrupt:8,
    ?EIPMI_RESERVED:8>>) ->
    [{msg_channel_info, get_message_channel_info(C0)},
     {msg_channel_info, get_message_channel_info(C1)},
     {msg_channel_info, get_message_channel_info(C2)},
     {msg_channel_info, get_message_channel_info(C3)},
     {msg_channel_info, get_message_channel_info(C4)},
     {msg_channel_info, get_message_channel_info(C5)},
     {msg_channel_info, get_message_channel_info(C6)},
     {msg_channel_info, get_message_channel_info(C7)}]
        ++ get_interrupt_type(messaging_interrupt, MsgInterrupt)
        ++ get_interrupt_type(event_message_buffer_interrupt, EventInterrupt).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_oem_record(<<Manufacturer:24/little, Data/binary>>) ->
    [{manufacturer_id, Manufacturer}, {data, Data}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_units(<<Format:2, Rate:3, Modifier:2, Percentage:1, Base:8, Mod:8>>) ->
    get_format(Format)
        ++ get_rate(Rate)
        ++ get_unit(Modifier, Base, Mod)
        ++ [{percentage, eipmi_util:get_bool(Percentage)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_format(0) -> [{sensor_format, unsigned}];
get_format(1) -> [{sensor_format, ones_complement}];
get_format(2) -> [{sensor_format, twos_complement}];
get_format(3) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_rate(1) -> [{sensor_rate, eipmi_sensor:get_unit(20)}];
get_rate(2) -> [{sensor_rate, eipmi_sensor:get_unit(21)}];
get_rate(3) -> [{sensor_rate, eipmi_sensor:get_unit(22)}];
get_rate(4) -> [{sensor_rate, eipmi_sensor:get_unit(23)}];
get_rate(5) -> [{sensor_rate, eipmi_sensor:get_unit(24)}];
get_rate(6) -> [{sensor_rate, eipmi_sensor:get_unit(25)}];
get_rate(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_unit(0, Base, _) ->
    [{sensor_unit, eipmi_sensor:get_unit(Base)}];
get_unit(_, Base, 16#00) ->
    [{sensor_unit, eipmi_sensor:get_unit(Base)}];
get_unit(1, Base, Mod) ->
    M = eipmi_sensor:get_unit(Mod),
    [{sensor_unit, {eipmi_sensor:get_unit(Base), per, M}}];
get_unit(2, Base, Mod) ->
    M = eipmi_sensor:get_unit(Mod),
    [{sensor_unit, {eipmi_sensor:get_unit(Base), M}}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_linearization(16#00) -> linear;
get_linearization(16#01) -> ln;
get_linearization(16#02) -> log10;
get_linearization(16#03) -> log2;
get_linearization(16#04) -> e;
get_linearization(16#05) -> exp10;
get_linearization(16#06) -> exp2;
get_linearization(16#07) -> '1/x';
get_linearization(16#08) -> sqr;
get_linearization(16#09) -> cube;
get_linearization(16#0a) -> sqrt;
get_linearization(16#0b) -> 'cube-1';
get_linearization(16#70) -> non_linear;
get_linearization(L) when L =< 16#7f andalso L >= 16#71 -> oem_non_linear.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_linearization_fun(ln) -> fun(X) -> math:log(X) end;
get_linearization_fun(log10) -> fun(X) -> math:log10(X) end;
get_linearization_fun(log2) -> fun(X) -> math:log(X) / math:log(2) end;
get_linearization_fun(e) -> fun(X) -> math:exp(X) end;
get_linearization_fun(exp10) -> fun(X) -> math:pow(10, X) end;
get_linearization_fun(exp2) -> fun(X) -> math:pow(2, X) end;
get_linearization_fun('1/x') -> fun(X) when X /= 0 -> 1 / X; (X) -> X end;
get_linearization_fun(sqr) -> fun(X) -> X * X end;
get_linearization_fun(cube) -> fun(X) -> X * X * X end;
get_linearization_fun(sqrt) -> fun(X) -> math:sqrt(X) end;
get_linearization_fun('cube-1') -> fun(X) -> math:pow(X, 1 / 3) end;
get_linearization_fun(_) -> fun(X) -> X end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_sensor_properties(Binary, Properties) ->
    Unit = proplists:get_value(sensor_unit, Properties),
    get_sensor_properties(Unit =/= unspecified, Binary, Unit).
get_sensor_properties(
  true,
  <<?EIPMI_RESERVED:1, Linearization:7, MLS:8/bitstring, MMS:2/bitstring,
    Tolerance:6, BLS:8/bitstring, BMS:2/bitstring, ALS:6/bitstring,
    AMS:4/bitstring, AccuracyExp:2, Direction:2, ResultExp:4/signed,
    BExp:4/signed>>, Unit) ->
    get_sensor_properties(
      <<MMS:2/bitstring, MLS:8/bitstring>>,
      <<BMS:2/bitstring, BLS:8/bitstring>>,
      <<AMS:4/bitstring, ALS:6/bitstring>>,
      Linearization, Tolerance, AccuracyExp,
      Direction, ResultExp, BExp, Unit);
get_sensor_properties(false, _, _) ->
    [].
get_sensor_properties(
  <<M:10/signed>>, <<B:10/signed>>, <<Accuracy:10>>, Linearization,
  Tolerance, AccuracyExp, Direction, ResultExp, BExp, Unit) ->
    L = get_linearization(Linearization),
    LFun = get_linearization_fun(L),
    [{sensor_tolerance, {LFun(M * Tolerance / 2 * math:pow(10, ResultExp)), Unit}},
     {sensor_resolution, {abs(M * math:pow(10, ResultExp)), Unit}},
     {sensor_accuracy, {math:pow(Accuracy, AccuracyExp) / 100, percent}},
     {sensor_coefficients, {L, M, B, BExp, ResultExp}}]
        ++ get_direction(Direction).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_sensor_numbers(SensorNumber, 0) ->
    [{sensor_number, SensorNumber}];
get_sensor_numbers(SensorNumber, 1) ->
    [{sensor_number, SensorNumber}];
get_sensor_numbers(SensorNumber, ShareCount) ->
    [{sensor_numbers, [SensorNumber + I || I <- lists:seq(0, ShareCount - 1)]}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_direction(0) -> [];
get_direction(1) -> [{sensor_direction, input}];
get_direction(2) -> [{sensor_direction, output}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_id(0, Sharing, Type, Id) ->
    get_id(1, Sharing, Type, Id);
get_id(_, <<0:1, _:7>>, _, Id) ->
    [{id, Id}];
get_id(Count, <<1:1, Offset:7>>, 0, Id) ->
    Is = lists:seq(Offset, Offset + Count - 1),
    [{id, string:join([Id ++ integer_to_list(I) || I <- Is], ", ")}];
get_id(Count, <<1:1, Offset:7>>, 1, Id) ->
    Is = lists:seq(Offset, Offset + Count - 1),
    [{id, string:join([Id ++ eipmi_util:from_base26(I) || I <- Is], ", ")}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_lun(<<0:1, Lun:2>>) -> [{sensor_lun, Lun}];
get_lun(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_generic_addr(<<Addr:7, 0:4, Lun:2, 0:3, ?EIPMI_RESERVED:5, Span:3>>) ->
    get_generic_addr(Addr, Span) ++ [{sensor_lun, Lun}];
get_generic_addr(<<Addr:7, 0:4, Lun:2, Bus:3, ?EIPMI_RESERVED:5, Span:3>>) ->
    get_generic_addr(Addr, Span) ++ [{sensor_lun, Lun}, {bus_id, Bus}];
get_generic_addr(<<Addr:7, Chan:4, Lun:2, 0:3, ?EIPMI_RESERVED:5, Span:3>>) ->
    get_generic_addr(Addr, Span) ++ [{channel, Chan}, {sensor_lun, Lun}];
get_generic_addr(<<Addr:7, Chan:4, Lun:2, Bus:3, ?EIPMI_RESERVED:5, Span:3>>) ->
    get_generic_addr(Addr, Span)
        ++ [{channel, Chan}, {sensor_lun, Lun}, {bus_id, Bus}].
get_generic_addr(Addr, Span) ->
    [{sensor_addr, Addr + I} || I <- lists:seq(0, Span)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_relative_addr(<<0:7, ?EIPMI_RESERVED:1, 0:4, ?EIPMI_RESERVED:4>>) ->
    [];
get_relative_addr(<<Addr:7, ?EIPMI_RESERVED:1, 0:4, ?EIPMI_RESERVED:4>>) ->
    [{sensor_addr, Addr}];
get_relative_addr(<<Addr:7, ?EIPMI_RESERVED:1, Chan:4, ?EIPMI_RESERVED:4>>) ->
    [{sensor_addr, Addr}, {channel, Chan}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_protocol(16#1) -> [{protocol, ipmb}];
get_protocol(16#2) -> [{protocol, {icmb, 1.0}}];
get_protocol(16#3) -> [{protocol, {icmb, 0.9}}];
get_protocol(16#4) -> [{protocol, {sm_bus, 1.0}}];
get_protocol(16#5) -> [{protocol, system_format}];
get_protocol(16#c) -> [{protocol, {oem, 1}}];
get_protocol(16#d) -> [{protocol, {oem, 2}}];
get_protocol(16#e) -> [{protocol, {oem, 3}}];
get_protocol(16#f) -> [{protocol, {oem, 4}}];
get_protocol(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_message_channel_info(<<T:1, Lun:3/bitstring, Protocol:4>>) ->
    [{transmit_support, eipmi_util:get_bool(T)}]
        ++ get_lun(Lun) ++ get_protocol(Protocol).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_interrupt_type(Tag, IRQ) when IRQ < 16#10 -> [{Tag, {irq, IRQ}}];
get_interrupt_type(Tag, PCI) when PCI < 16#14 -> [{Tag, {pci, [49 + PCI]}}];
get_interrupt_type(Tag, 16#14) -> [{Tag, smi}];
get_interrupt_type(Tag, 16#15) -> [{Tag, sci}];
get_interrupt_type(Tag, I) when I < 16#5f-> [{Tag, {interrupt, I - 16#20}}];
get_interrupt_type(Tag, 16#60) -> [{Tag, assigned}];
get_interrupt_type(_Tag, _) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_contained(0, direct, Binary) ->
    get_contained_from_list(Binary, []);
get_contained(0, relative, Binary) ->
    get_rel_contained_from_list(Binary, []);
get_contained(1, direct, Binary) ->
    get_contained_from_range(Binary, []);
get_contained(1, relative, Binary) ->
    get_rel_contained_from_range(Binary, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_contained_from_list(<<>>, Acc) ->
    lists:reverse(Acc);
get_contained_from_list(<<0:16, Rest/binary>>, Acc) ->
    get_contained_from_list(Rest, Acc);
get_contained_from_list(<<Id:8, I:1/binary, R/binary>>, Acc) ->
    Entity = {containee, eipmi_sensor:get_entity(Id, I)},
    get_contained_from_list(R, [Entity | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_contained_from_range(<<>>, Acc) ->
    Acc;
get_contained_from_range(<<0:32, Rest/binary>>, Acc) ->
    get_contained_from_range(Rest, Acc);
get_contained_from_range(<<Id:8, I0:8, Id:8, I1:8, Rest/binary>>, Acc) ->
    Es = [{containee, eipmi_sensor:get_entity(Id, I)} || I <- lists:seq(I0, I1)],
    get_contained_from_range(Rest, Acc ++ Es).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_rel_contained_from_list(<<>>, Acc) ->
    lists:reverse(Acc);
get_rel_contained_from_list(<<0:32, Rest/binary>>, Acc) ->
    get_rel_contained_from_list(Rest, Acc);
get_rel_contained_from_list(<<A:2/binary, Id:8, I:1/binary, R/binary>>, Acc) ->
    Entity = {containee, eipmi_sensor:get_entity(Id, I) ++ get_relative_addr(A)},
    get_rel_contained_from_list(R, [Entity | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_rel_contained_from_range(<<>>, Acc) ->
    Acc;
get_rel_contained_from_range(<<0:64, Rest/binary>>, Acc) ->
    get_rel_contained_from_range(Rest, Acc);
get_rel_contained_from_range(
  <<A:2/binary, Id:8, I0:8, _:16, Id:8, I1:8, R/binary>>, Acc) ->
    Es = [{containee, eipmi_sensor:get_entity(Id, I) ++ get_relative_addr(A)}
          || I <- lists:seq(I0, I1)],
    get_rel_contained_from_range(R, Acc ++ Es).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_reading(Tag, Raw, Properties) ->
    Unit = proplists:get_value(sensor_unit, Properties),
    Format = proplists:get_value(sensor_format, Properties),
    Coefficients = proplists:get_value(sensor_coefficients, Properties),
    get_reading(Tag, Unit, Format, Raw, Coefficients).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_reading(_, U, _, _, _) when U =:= unspecified orelse U =:= undefined ->
    [];
get_reading(Tag, Unit, unsigned, <<Value:8/unsigned>>, Coefficients) ->
    [{Tag, calc_reading(Unit, Value, Coefficients)}];
get_reading(Tag, Unit, ones_complement, <<1:1, Raw:7>>, Coefficients) ->
    [{Tag, calc_reading(Unit, (bnot Raw) * -1, Coefficients)}];
get_reading(Tag, Unit, ones_complement, <<0:1, Value:7>>, Coefficients) ->
    [{Tag, calc_reading(Unit, Value, Coefficients)}];
get_reading(Tag, Unit, twos_complement, <<Value:8/signed>>, Coefficients) ->
    [{Tag, calc_reading(Unit, Value, Coefficients)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
calc_reading(Unit, X, {L, M, B, BExp, ResultExp}) ->
    Fun = get_linearization_fun(L),
    {Fun((M * X + B * math:pow(10, BExp)) * math:pow(10, ResultExp)), Unit}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
needs_reservation(SdrInfo) ->
    lists:member(reserve, proplists:get_value(operations, SdrInfo)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_offsets(StateBitstring) -> get_offsets(StateBitstring, 0, []).
get_offsets(<<>>, _, Acc) -> lists:reverse(Acc);
get_offsets(<<0:1, R/bitstring>>, I, Acc) -> get_offsets(R, I + 1, Acc);
get_offsets(<<1:1, R/bitstring>>, I, Acc) -> get_offsets(R, I + 1, [I | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_recent_action_timestamp(SdrInfo) ->
    erlang:max(
      proplists:get_value(most_recent_addition, SdrInfo, 0),
      proplists:get_value(most_recent_erase, SdrInfo, 0)).
