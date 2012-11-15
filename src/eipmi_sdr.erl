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

-export([get_info/1,
         read/1,
         read/2]).

-include("eipmi.hrl").

-define(MAX_READ_COUNT, 16).

-define(READ, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR}).
-define(GET_INFO, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR_REPOSITORY_INFO}).
-define(RESERVE, {?IPMI_NETFN_STORAGE_REQUEST, ?RESERVE_SDR_REPOSITORY}).

-type info() ::
        {version, string()} |
        {entries, non_neg_integer()} |
        {free_space, non_neg_integer()} |
        {most_recent_addition, non_neg_integer()} |
        {most_recent_erase, non_neg_integer()} |
        {overflow, boolean()} |
        {operations, [delete | partial_add | reserve | get_allocation_info]}.

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

-type entry() ::
        {record_type(), [term()]}.

-export_type([info/0, record_type/0, entry/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
-spec get_info(pid()) ->
                      {ok, [info()]} | {error, term()}.
get_info(SessionPid) ->
    eipmi_session:request(SessionPid, ?GET_INFO, []).

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
-spec read(pid()) ->
                  [entry()].
read(SessionPid) ->
    {ok, SdrInfo} = eipmi_session:request(SessionPid, ?GET_INFO, []),
    case eipmi_util:get_val(entries, SdrInfo) of
        0 ->
            [];
        _ ->
            Operations = eipmi_util:get_val(operations, SdrInfo),
            Reserve = lists:member(reserve, Operations),
            maybe_reserve(SessionPid, fun read_all/2,  [SessionPid], Reserve)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
-spec read(pid(), non_neg_integer()) ->
                  entry().
read(SessionPid, RecordId) ->
    {ok, SdrInfo} = eipmi_session:request(SessionPid, ?GET_INFO, []),
    Args = [SessionPid, RecordId],
    Reserve = lists:member(reserve, eipmi_util:get_val(operations, SdrInfo)),
    {_, Entry} = maybe_reserve(SessionPid, fun read_one/3, Args, Reserve),
    Entry.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec maybe_reserve(pid(), function(), [term()], boolean()) ->
                           [entry()] | {non_neg_integer(), entry()}.
maybe_reserve(SessionPid, Fun, Args, true) ->
    {ok, Reserve} = eipmi_session:request(SessionPid, ?RESERVE, []),
    ReservationId = eipmi_util:get_val(reservation_id, Reserve),
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
    Length = eipmi_util:get_val(record_length, Header),
    Type = eipmi_util:get_val(record_type, Header),
    Body = read_body(SessionPid, RecordId, ReservationId, Type, Length),
    {NextRecordId, {Type, proplists:delete(record_type, Header) ++ Body}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_header(Pid, Record, Reservation) ->
    Ps = [{reservation_id, Reservation}, {record_id, Record},
          {offset, 0}, {count, 5}],
    {ok, Read} = eipmi_session:request(Pid, ?READ, Ps),
    NextRecordId = eipmi_util:get_val(next_record_id, Read),
    {NextRecordId, decode_header(eipmi_util:get_val(data, Read))}.

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
    {ok, Read} = eipmi_session:request(Pid, ?READ, Ps),
    {Offset + Count, <<Acc/binary, (eipmi_util:get_val(data, Read))/binary>>}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_header(<<Id:16/little, Version:1/binary, Type:8, L:8>>) ->
    T = get_record_type(Type),
    V = lists:reverse(eipmi_util:from_bcd_plus(Version)),
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
decode_body(_Type, _Data) ->
    [].

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
    _Initialization:8, _Capabilities:8, SensorType:8, ReadingType:8,
    _AssertionMask:16, _DeassertionMask:16, _ReadingMask:16, Units1:1/binary,
    Units2:8, Units3:8, ?EIPMI_RESERVED:1, Linearization:7,
    MAndTolerance:16/little, BAndAccuracy:24/little, R:4, B:4,
    _AnalogCharacteristics:8, NominalReading:8, NominalMaximum:8,
    NominalMinimum:8, MaximumReading:8, MinimumReading:8, _Thresholds:64,
    ?EIPMI_RESERVED:16, _OEM:8, _IdLen:8, Id/binary>>) ->
    %% TODO
    {_, Type} = eipmi_sensor:get_reading(ReadingType, SensorType),
    eipmi_sensor:get_addr(SensorAddr)
        ++ [{sensor_number, SensorNumber}]
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ eipmi_sensor:get_type(Type)
        ++ get_units(Units1, Units2, Units3)
        ++ get_linearization(Linearization).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_compact_sensor_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_event_only_record(
  <<SensorAddr:2/binary, SensorNumber:8, EntityId:8, EntityInstance:1/binary,
    SensorType:8, ReadingType:8, Direction:2, Modifier:2, Count:4,
    Sharing:1/binary, ?EIPMI_RESERVED:8, _OEM:8, _IdLen:8, Id/binary>>) ->
    {_, Type} = eipmi_sensor:get_reading(ReadingType, SensorType),
    eipmi_sensor:get_addr(SensorAddr)
        ++ get_sensor_numbers(SensorNumber, Count)
        ++ eipmi_sensor:get_entity(EntityId, EntityInstance)
        ++ eipmi_sensor:get_type(Type)
        ++ get_direction(Direction)
        ++ get_id(Count, Sharing, Modifier, eipmi_util:binary_to_string(Id)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_entity_association_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_device_relative_entity_association_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_generic_device_locator_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_fru_device_locator_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_device_locator_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_confirmation_record(
  <<?EIPMI_RESERVED:1, Major:7, Minor:8, IPMILeast:4, IPMIMost:4,
    Manufacturer:24/little, Product:16/little, GUID/binary>>) ->
    [{firmware_version, eipmi_util:format("~B.~B", [Major, Minor])},
     {ipmi_version, eipmi_util:format("~B.~B", [IPMIMost, IPMILeast])},
     {manufacturer_id, Manufacturer},
     {product_id, Product},
     {guid, eipmi_util:binary_to_string(GUID)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_bmc_message_channel_info_record(_Data) ->
    %% TODO
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_units(<<Format:2, Rate:3, Modifier:2, Percentage:1>>, Unit2, Unit3) ->
    get_format(Format)
        ++ get_rate(Rate)
        ++ get_unit(Modifier, Unit2, Unit3)
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
get_rate(0) -> [];
get_rate(1) -> [{sensor_rate, eipmi_sensor:get_unit(20)}];
get_rate(2) -> [{sensor_rate, eipmi_sensor:get_unit(21)}];
get_rate(3) -> [{sensor_rate, eipmi_sensor:get_unit(22)}];
get_rate(4) -> [{sensor_rate, eipmi_sensor:get_unit(23)}];
get_rate(5) -> [{sensor_rate, eipmi_sensor:get_unit(24)}];
get_rate(6) -> [{sensor_rate, eipmi_sensor:get_unit(25)}].

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
get_linearization(16#00) -> [{sensor_linearization, linear}];
get_linearization(16#01) -> [{sensor_linearization, ln}];
get_linearization(16#02) -> [{sensor_linearization, log10}];
get_linearization(16#03) -> [{sensor_linearization, log2}];
get_linearization(16#04) -> [{sensor_linearization, e}];
get_linearization(16#05) -> [{sensor_linearization, exp10}];
get_linearization(16#06) -> [{sensor_linearization, exp2}];
get_linearization(16#07) -> [{sensor_linearization, '1/x'}];
get_linearization(16#08) -> [{sensor_linearization, sqr}];
get_linearization(16#09) -> [{sensor_linearization, cube}];
get_linearization(16#0a) -> [{sensor_linearization, sqrt}];
get_linearization(16#0b) -> [{sensor_linearization, 'cube-1'}];
get_linearization(16#70) -> [{sensor_linearization, non_linear}];
get_linearization(L) when L =< 16#7f andalso L >= 16#71 ->
    [{sensor_linearization, oem_non_linear}].

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
