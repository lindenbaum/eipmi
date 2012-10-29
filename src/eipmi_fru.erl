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
%%% A module providing FRU decoding functionality according to the official IPMI
%%% Platform Management FRU Information Storage Definition.
%%% Currently missing features:
%%% * support for multi record fields (according to IPMI spec)
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
    GetFruInfo = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_FRU_INVENTORY_AREA_INFO},
    FruInfo = eipmi_session:request(SessionPid, GetFruInfo, [{fru_id, FruId}]),
    eipmi_util:no_badmatch(fun() -> read_fru(FruInfo, SessionPid, FruId) end).

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
    ReadFru = {?IPMI_NETFN_STORAGE_REQUEST, ?READ_FRU_DATA},
    AreaSize = eipmi_util:get_val(area_size, FruInfo),
    F = fun(Offset, Count) ->
                Ps = [{fru_id, FruId}, {offset, Offset}, {count, Count}],
                {ok, R} = eipmi_session:request(SessionPid, ReadFru, Ps),
                {eipmi_util:get_val(count, R), eipmi_util:get_val(data, R)}
        end,
    decode(eipmi_util:read(F, AreaSize, ?MAX_READ_COUNT)).

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
decode_record(true, 16#00, _Data) ->
    [{power_supply, [not_yet_implemented]}];
decode_record(true, 16#01, _Data) ->
    [{dc_output, [not_yet_implemented]}];
decode_record(true, 16#02, _Data) ->
    [{dc_load, [not_yet_implemented]}];
decode_record(true, 16#03, _Data) ->
    [{management_access, [not_yet_implemented]}];
decode_record(true, 16#04, _Data) ->
    [{base_compatibility, [not_yet_implemented]}];
decode_record(true, 16#05, _Data) ->
    [{extended_compatibility, [not_yet_implemented]}];
decode_record(true, _Type, _Data) ->
    %% unsupported
    [].

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
    {[{Name, from_bcd_plus(Data)}], Rest};
decode_field(Name, _Lang, <<2:2, Len:6, Data:Len/binary, Rest/binary>>) ->
    {[{Name, from_packed_ascii(Data)}], Rest};
decode_field(Name, Lang, <<3:2, Len:6, Data:Len/binary, Rest/binary>>)
  when Lang =:= ?ENGLISH orelse Lang =:= ?OLD_ENGLISH ->
    {[{Name, binary_to_list(Data)}], Rest};
decode_field(Name, _Lang, <<3:2, _Len:6, Data/utf16-little, Rest/binary>>) ->
    {[{Name, Data}], Rest}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
from_bcd_plus(Binary) ->
    from_bcd_plus(Binary, "").
from_bcd_plus(<<>>, Acc) ->
    lists:reverse(Acc);
from_bcd_plus(<<16#0:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$0 | Acc]);
from_bcd_plus(<<16#1:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$1 | Acc]);
from_bcd_plus(<<16#2:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$2 | Acc]);
from_bcd_plus(<<16#3:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$3 | Acc]);
from_bcd_plus(<<16#4:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$4 | Acc]);
from_bcd_plus(<<16#5:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$5 | Acc]);
from_bcd_plus(<<16#6:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$6 | Acc]);
from_bcd_plus(<<16#7:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$7 | Acc]);
from_bcd_plus(<<16#8:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$8 | Acc]);
from_bcd_plus(<<16#9:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$9 | Acc]);
from_bcd_plus(<<16#a:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [32 | Acc]);
from_bcd_plus(<<16#b:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$- | Acc]);
from_bcd_plus(<<16#c:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$. | Acc]);
from_bcd_plus(<<_:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, Acc).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
from_packed_ascii(Binary) ->
    from_packed_ascii1(Binary, "").
from_packed_ascii1(<<>>, Acc) ->
    lists:reverse(Acc);
from_packed_ascii1(<<Carry2:2, Char1:6, Rest/bitstring>>, Acc) ->
    from_packed_ascii2(Carry2, Rest, [Char1 + 32 | Acc]).
from_packed_ascii2(Carry2, <<Carry3:4, Char2:4, Rest/bitstring>>, Acc) ->
    from_packed_ascii3(Carry3, Rest, [Char2 bsl 2 + Carry2 + 32 | Acc]);
from_packed_ascii2(_Carry2, <<>>, Acc) ->
    lists:reverse(Acc).
from_packed_ascii3(Carry3, <<Char4:6, Char3:2, Rest/bitstring>>, Acc) ->
    from_packed_ascii1(Rest, [Char4 + 32 | [Char3 bsl 4 + Carry3 + 32 | Acc]]);
from_packed_ascii3(_Carry3, <<>>, Acc) ->
    lists:reverse(Acc).

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

from_bcd_plus_test() ->
    Bin = <<16#0:4, 16#1:4, 16#2:4, 16#3:4, 16#4:4, 16#5:4, 16#6:4, 16#7:4,
            16#8:4, 16#9:4, 16#a:4, 16#b:4, 16#c:4, 16#d:4, 16#e:4, 16#f:4>>,
    ?assertEqual("0123456789 -.", from_bcd_plus(Bin)).

from_packed_ascii_test() ->
    Bin = <<2#00101001, 2#11011100, 2#10100110>>,
    ?assertEqual("IPMI", from_packed_ascii(Bin)).

-endif.
