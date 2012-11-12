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
%%% A module providing FRU reading and decoding functionality according to the
%%% official IPMI Platform Management FRU Information Storage Definition.
%%% Currently missing features:
%%% * support for mutli record fields (according to PICMG spec)
%%% @end
%%%=============================================================================

-module(eipmi_fru).

-export([read/2]).

-include("eipmi.hrl").

-define(OLD_ENGLISH, 0).
-define(ENGLISH, 25).
-define(EOFIELDS, 16#c1).
-define(MAX_READ_COUNT, 23).

-define(READ, {?IPMI_NETFN_STORAGE_REQUEST, ?READ_FRU_DATA}).
-define(GET_INFO, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_FRU_INVENTORY_AREA_INFO}).

-type chassis_info() ::
        {type, string()} |
        {part_number, string()} |
        {serial_number, string()} |
        {custom, term()}.

-type board_info() ::
        {manufacturer, string()} |
        {name, string()} |
        {serial_number, string()} |
        {part_number, string()} |
        {fru_file_id, string()} |
        {custom, term()}.

-type product_info() ::
        board_info() |
        {version, string()} |
        {asset_tag, string()} |
        {custom, term()}.

-type info() ::
        [{chassis_area, [chassis_info()]} |
         {board_area, [board_info()]} |
         {product_area, [product_info()]} |
         {record_area, [term()]}].

-export_type([chassis_info/0, board_info/0, product_info/0, info/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Read and decode FRU inventory data for a specific FRU id using the provided
%% IPMI session. The returned FRU information is a property list that does only
%% contain the available and checksum error free fields of the inventory.
%% If no FRU data is available for a specific id the returned inventory data is
%% the empty list.
%% @end
%%------------------------------------------------------------------------------
-spec read(pid(), 0..254) ->
                  {ok, info()} | {error, term()}.
read(SessionPid, FruId) ->
    FruInfo = eipmi_session:request(SessionPid, ?GET_INFO, [{fru_id, FruId}]),
    read_fru(FruInfo, SessionPid, FruId).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_fru({error, {bmc_error, parameter_out_of_range}}, _SessionPid, _FruId) ->
    {ok, []};
read_fru(Error = {error, _}, _SessionPid, _FruId) ->
    Error;
read_fru({ok, FruInfo}, SessionPid, FruId) ->
    Access = eipmi_util:get_val(access, FruInfo),
    Divider = case Access of by_words -> 2; by_bytes -> 1 end,
    AreaSize = eipmi_util:get_val(area_size, FruInfo) div Divider,
    decode(read_fru(SessionPid, FruId, AreaSize, ?MAX_READ_COUNT div Divider)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_fru(SessionPid, FruId, Size, BlockSize) ->
    read_fru(SessionPid, FruId, Size, BlockSize, {0, <<>>}).
read_fru(_SessionPid, _FruId, Size, _BlockSize, {Size, Acc}) ->
    Acc;
read_fru(SessionPid, FruId, Size, BlockSize, {Offset, Acc})
  when Offset + BlockSize =< Size ->
    NewAcc = do_read_fru(SessionPid, FruId, Offset, BlockSize, Acc),
    read_fru(SessionPid, FruId, Size, BlockSize, NewAcc);
read_fru(SessionPid, FruId, Size, BlockSize, {Offset, Acc}) ->
    NewAcc = do_read_fru(SessionPid, FruId, Offset, Size - Offset, Acc),
    read_fru(SessionPid, FruId, Size, BlockSize, NewAcc).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_read_fru(SessionPid, FruId, Offset, Count, Acc) ->
    Ps = [{fru_id, FruId}, {offset, Offset}, {count, Count}],
    {ok, R} = eipmi_session:request(SessionPid, ?READ, Ps),
    Data = eipmi_util:get_val(data, R),
    {Offset + eipmi_util:get_val(count, R), <<Acc/binary, Data/binary>>}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode(<<>>) ->
    {ok, []};
decode(FruData = <<?EIPMI_RESERVED:4,?FRU_VERSION:4, Offsets:5/binary, _/binary>>) ->
    OffsetTuple = list_to_tuple(binary_to_list(Offsets)),
    decode(is_header_sane(FruData), OffsetTuple, FruData);
decode(_FruData) ->
    {error, unsupported_fru_data}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode(false, _Offsets, _FruData) ->
    {error, incorrect_header_checksum};
decode(true, {_, C, B, P, M}, FruData) ->
    {ok,
     decode_chassis(get_area(C, non_zero([B, P, M]), FruData))
     ++ decode_board(get_area(B, non_zero([P, M]), FruData))
     ++ decode_product(get_area(P, non_zero([M]), FruData))
     ++ decode_multi_record(get_area(M, [], FruData))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_chassis(<<>>) ->
    [];
decode_chassis(Area = <<?EIPMI_RESERVED:4, ?FRU_VERSION:4, _:8, Data/binary>>) ->
    decode_chassis(is_binary_sane(Area), Data).
decode_chassis(false, _Data) ->
    [];
decode_chassis(true, <<Type:8, Rest/binary>>) ->
    {PartNumber, Rest1} = decode_field(part_number, ?ENGLISH, Rest),
    {SerialNumber, Rest2} = decode_field(serial_number, ?ENGLISH, Rest1),
    CustomFields =  decode_fields(custom, ?ENGLISH, Rest2),
    [{chassis_area,
      [{type, Type}]
      ++ PartNumber
      ++ SerialNumber
      ++ CustomFields}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_board(<<>>) ->
    [];
decode_board(Area = <<?EIPMI_RESERVED:4, ?FRU_VERSION:4, _:8, Data/binary>>) ->
    decode_board(is_binary_sane(Area), Data).
decode_board(false, _Data) ->
    [];
decode_board(true, <<Lang:8, MfgDate:24/little, Rest/binary>>) ->
    {Manufacturer, Rest1} = decode_field(manufacturer, Lang, Rest),
    {Name, Rest2} = decode_field(name, Lang, Rest1),
    {SerialNumber, Rest3} = decode_field(serial_number, ?ENGLISH, Rest2),
    {PartNumber, Rest4} = decode_field(part_number, Lang, Rest3),
    {FruFileId, Rest5} = decode_field(fru_file_id, ?ENGLISH, Rest4),
    CustomFields = decode_fields(custom, Lang, Rest5),
    [{board_area,
      [{manufacturing_date, MfgDate}]
      ++ Manufacturer
      ++ Name
      ++ SerialNumber
      ++ PartNumber
      ++ FruFileId
      ++ CustomFields}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_product(<<>>) ->
    [];
decode_product(Area = <<?EIPMI_RESERVED:4, ?FRU_VERSION:4, _:8, Data/binary>>) ->
    decode_product(is_binary_sane(Area), Data).
decode_product(false, _Data) ->
    [];
decode_product(true, <<Lang:8, Rest/binary>>) ->
    {Manufacturer, Rest1} = decode_field(manufacturer, Lang, Rest),
    {Name, Rest2} = decode_field(name, Lang, Rest1),
    {PartNumber, Rest3} = decode_field(part_number, Lang, Rest2),
    {Version, Rest4} = decode_field(version, Lang, Rest3),
    {SerialNumber, Rest5} = decode_field(serial_number, ?ENGLISH, Rest4),
    {AssetTag, Rest6} = decode_field(asset_tag, Lang, Rest5),
    {FruFileId, Rest7} = decode_field(fru_file_id, Lang, Rest6),
    CustomFields = decode_fields(custom, Lang, Rest7),
    [{product_area,
      Manufacturer
      ++ Name
      ++ PartNumber
      ++ Version
      ++ SerialNumber
      ++ AssetTag
      ++ FruFileId
      ++ CustomFields}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_multi_record(<<>>) ->
    [];
decode_multi_record(RecordData) ->
    decode_multi_record([], RecordData).
decode_multi_record(Records, <<>>) ->
    [{record_area, lists:reverse(Records)}];
decode_multi_record(Records, RestData) ->
    {Record, NewRestData} = decode_record(RestData),
    decode_multi_record(Record ++ Records, NewRestData).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_record(<<>>) ->
    {[], <<>>};
decode_record(Binary = <<Header:5/binary, _/binary>>) ->
    decode_record(is_binary_sane(Header), Binary).
decode_record(false, _Binary) ->
    {[], <<>>};
decode_record(true, <<T:8, 1:1, _:7, L:8, C:8, _:8, Data:L/binary, _/binary>>) ->
    {decode_record(sum(Data, C) =:= 0, T, Data), <<>>};
decode_record(true, <<T:8, 0:1, _:7, L:8, C:8, _:8, Data:L/binary, R/binary>>) ->
    {decode_record(sum(Data, C) =:= 0, T, Data), R}.
decode_record(false, _Type, _Data) ->
    [];
decode_record(true, 16#00, Data) ->
    [{power_supply, decode_power_supply(Data)}];
decode_record(true, 16#01, Data) ->
    [{dc_output, decode_dc_output(Data)}];
decode_record(true, 16#02, Data) ->
    [{dc_load, decode_dc_load(Data)}];
decode_record(true, 16#03, Data) ->
    [{management_access, decode_management_access(Data)}];
decode_record(true, 16#04, Data) ->
    [{base_compatibility, decode_compatibility(Data)}];
decode_record(true, 16#05, Data) ->
    [{extended_compatibility, decode_compatibility(Data)}];
decode_record(true, _Type, _Data) ->
    %% unsupported
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_power_supply(<<OverallCapacity:16/little, PeakVA:16/little,
                      InrushCurrent:8, InrushInterval:8,
                      LowEndInputV1:16/little, HighEndInputV1:16/little,
                      LowEndInputV2:16/little, HighEndInputV2:16/little,
                      LowEndInputFrequency:8, HighEndInputFrequency:8,
                      ACDropOutTolerance:8, 0:4, FailSignal:1,
                      HotSwap:1, Autoswitch:1, PowerFactor:1,
                      PredictiveFailSupport:1, PeakW:16/little,
                      CombinedWattageV1:4, CombinedWattageV2:4,
                      TotalWattage:16/little,
                      PredictiveFailTachometerLowerThreshold:8>>) ->
    <<PeakWattageSecs:4, PeakWattage:12>>  = <<PeakW:16>>,
    [{overall_capacitly, {OverallCapacity, 'W'}},
     {peak_voltage, maybe_value({PeakVA, 'VA'}, unspecified)},
     {inrush_current, maybe_value({InrushCurrent, 'A'}, unspecified)},
     {inrush_interval, maybe_value({InrushInterval, ms}, unspecified)},
     {low_end_input_voltage_range_110v, {LowEndInputV1 * 10, mV}},
     {high_end_input_voltage_range_110v, {HighEndInputV1 * 10, mV}},
     {low_end_input_voltage_range_220v, {LowEndInputV2 * 10, mV}},
     {high_end_input_voltage_range_220v, {HighEndInputV2 * 10, mV}},
     {low_end_input_frequency_range, {LowEndInputFrequency, hz}},
     {high_end_input_frequency_range, {HighEndInputFrequency, hz}},
     {ac_dropout_tolerance, {ACDropOutTolerance, ms}},
     {predictive_fail_signal,
      get_power_supply_predictive_fail_signal(FailSignal)},
     {hot_swap_supported, get_bool(HotSwap)},
     {autoswitch_supported, get_bool(Autoswitch)},
     {power_factor_correction_supported, get_bool(PowerFactor)},
     {predictive_fail_pin_supported, get_bool(PredictiveFailSupport)},
     get_predictive_fail_tachometer_lower_threshold(
       PredictiveFailTachometerLowerThreshold),
     {peak_wattage_tolerance, {PeakWattageSecs, s}},
     {peak_wattage, {PeakWattage, 'W'}},
     get_combined_wattage(CombinedWattageV1, CombinedWattageV2, TotalWattage)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_dc_output(<<Standby:1, 0:3, OutputNumber:4,
                   NominalVoltage:16/signed-little,
                   MaximumNegativeVoltageDeviation:16/signed-little,
                   MaximumPositiveVoltageDeviation:16/signed-little,
                   RippleAndNoise:16/little,
                   MinimumCurrentDraw:16/little,
                   MaximumCurrentDraw:16/little>>) ->
    [{provides_standby_output, get_bool(Standby)},
     {output, maybe_value({OutputNumber, number}, unknown)},
     {nominal_voltage, {NominalVoltage / 100, 'V'}},
     {maximum_negative_voltage_deviation,
      {MaximumNegativeVoltageDeviation / 100, 'V'}},
     {maximum_positive_voltage_deviation,
      {MaximumPositiveVoltageDeviation / 100, 'V'}},
     {ripple_and_noise, {RippleAndNoise, mV}},
     {minimum_current_draw, {MinimumCurrentDraw, mA}},
     {maximum_current_draw, {MaximumCurrentDraw, mA}}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_dc_load(<<0:4, OutputNumber:4,
                 NominalVoltage:16/signed-little,
                 MinimumVoltage:16/signed-little,
                 MaximumVoltage:16/signed-little,
                 RippleAndNoise:16/little,
                 MinimumCurrentLoad:16/little,
                 MaximumCurrentLoad:16/little>>) ->
    [{output, maybe_value({OutputNumber, number}, unknown)},
     {nominal_voltage, {NominalVoltage / 100, 'V'}},
     {minimum_voltage, {MinimumVoltage / 100, 'V'}},
     {maximum_voltage, {MaximumVoltage / 100, 'V'}},
     {ripple_and_noise, {RippleAndNoise, mV}},
     {minimum_current_load, {MinimumCurrentLoad, mA}},
     {maximum_current_load, {MaximumCurrentLoad, mA}}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_access(<<16#01:8, URL/binary>>) ->
    [{system_management_url, binary_to_list(URL)}];
decode_management_access(<<16#02:8, Name/binary>>) ->
    [{system_name, binary_to_list(Name)}];
decode_management_access(<<16#03:8, Addr/binary>>) ->
    [{system_ping_address, binary_to_list(Addr)}];
decode_management_access(<<16#04:8, URL/binary>>) ->
    [{component_management_url, binary_to_list(URL)}];
decode_management_access(<<16#05:8, Name/binary>>) ->
    [{component_name, binary_to_list(Name)}];
decode_management_access(<<16#06:8, Addr/binary>>) ->
    [{component_ping_address, binary_to_list(Addr)}];
decode_management_access(<<16#07:8, Id/binary>>) ->
    [{system_unique_id, binary_to_list(Id)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_compatibility(<<ManufacturerID:24/little, EntityID:8,
                       CompatibilityBase:8, 0:1, CodeStart:7,
                       CodeRanges/binary>>) ->
    [{manufacturer_id, ManufacturerID},
     {entity_id, EntityID},
     {compatibility_base, CompatibilityBase},
     {compatible_codes, get_code_ranges(CodeRanges, CodeStart)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_code_ranges(Bytes, CodeStart) ->
    lists:sort(get_code_ranges(Bytes, CodeStart, 0, [CodeStart])).
get_code_ranges(<<>>, _CodeStart, _BitIndex, Acc) ->
    lists:reverse(Acc);
get_code_ranges(<<1:1, Rest/bitstring>>, CodeStart, BitIndex, Acc) ->
    NewCode = CodeStart + (((BitIndex div 8) * 8) + ((8 - (BitIndex rem 8)))),
    get_code_ranges(Rest, CodeStart, BitIndex + 1, [NewCode|Acc]);
get_code_ranges(<<0:1, Rest/bitstring>>, CodeStart, BitIndex, Acc) ->
    get_code_ranges(Rest, CodeStart, BitIndex + 1, Acc).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_predictive_fail_tachometer_lower_threshold(0) ->
    {predictive_fail_lower_treshold, none};
get_predictive_fail_tachometer_lower_threshold(Value) ->
    {predictive_fail_lower_treshold, {Value, rps}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_combined_wattage(0, 0, 0) ->
    {combined_wattage, none};
get_combined_wattage(A, B, Wattage) ->
    {combined_wattage, {get_voltage(A), get_voltage(B), {Wattage, 'W'}}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_voltage(0) -> {12, 'V'};
get_voltage(1) -> {-12, 'V'};
get_voltage(2) -> {5, 'V'};
get_voltage(3) -> {3.3, 'V'}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_power_supply_predictive_fail_signal(0) ->
    one_pulse_per_rotation_or_signal_asserted;
get_power_supply_predictive_fail_signal(1) ->
    two_pulses_per_rotation_or_signal_deasserted.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_bool(0) -> false;
get_bool(1) -> true.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_value({0, _}, Alternative) -> Alternative;
maybe_value({16#ffff, _}, Alternative) -> Alternative;
maybe_value({16#ff, _}, Alternative) -> Alternative;
maybe_value(Value, _) -> Value.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_area(0, _NextOffsets, _FruData) ->
    <<>>;
get_area(Offset, [], FruData) ->
    SkipSize = Offset * 8,
    <<_:SkipSize/binary, Area/binary>> = FruData,
    Area;
get_area(Offset, [NextOffset | _], FruData) ->
    SkipSize = Offset * 8,
    Size = NextOffset * 8 - SkipSize,
    <<_:SkipSize/binary, Area:Size/binary, _/binary>> = FruData,
    Area.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_fields(Name, Lang, Data) ->
    decode_fields(Name, Lang, Data, []).
decode_fields(_Name, _Lang, <<?EOFIELDS:8, _/binary>>, Acc) ->
    lists:reverse(Acc);
decode_fields(Name, Lang, Data, Acc) ->
    {Field, Rest} = decode_field(Name, Lang, Data),
    decode_fields(Name, Lang, Rest, [Field | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_field(_Name, _Lang, <<_:2, 0:6, Rest/binary>>) ->
    {[], Rest};
decode_field(Name, _Lang, <<0:2, Len:6, Data:Len/binary, Rest/binary>>) ->
    {[{Name, Data}], Rest};
decode_field(Name, _Lang, <<1:2, Len:6, Data:Len/binary, Rest/binary>>) ->
    {[{Name, eipmi_util:from_bcd_plus(Data)}], Rest};
decode_field(Name, _Lang, <<2:2, Len:6, Data:Len/binary, Rest/binary>>) ->
    {[{Name, eipmi_util:from_packed_ascii(Data)}], Rest};
decode_field(Name, Lang, <<3:2, Len:6, Data:Len/binary, Rest/binary>>)
  when Lang =:= ?ENGLISH orelse Lang =:= ?OLD_ENGLISH ->
    {[{Name, binary_to_list(Data)}], Rest};
decode_field(Name, _Lang, <<3:2, _Len:6, Data/utf16-little, Rest/binary>>) ->
    {[{Name, Data}], Rest}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
non_zero(List) ->
    [Elem || Elem <- List, Elem > 0].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
is_header_sane(<<Header:8/binary, _/binary>>) ->
    is_binary_sane(Header).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
is_binary_sane(Binary) ->
    sum(Binary, 0) =:= 0.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sum(<<>>, Sum) ->
    Sum rem 256;
sum(<<Byte:8, Rest/binary>>, Sum) ->
    sum(Rest, Sum + Byte).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_compatibility_test() ->
    ?assertEqual([{manufacturer_id,123},
                  {entity_id,1},
                  {compatibility_base,42},
                  {compatible_codes,[10,11,12,13,14,15,16,22,23]}],
                 decode_compatibility(<<123:24/little,
                                        1:8, 42:8, 10:8,
                                        2#00111111:8,
                                        2#00011000:8>>)).

-endif.
