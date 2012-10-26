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
%%% * support for packetd 6bit ASCII
%%% * support for multi record fields (according to spec)
%%% @end
%%%=============================================================================

-module(eipmi_fru).

-export([decode/1]).

-include("eipmi.hrl").

-define(OLD_ENGLISH, 0).
-define(ENGLISH, 25).
-define(EOFIELDS, 16#c1).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
-spec decode(binary()) ->
                    proplists:proplist() | binary().
decode(FruData = <<?EIPMI_RESERVED:4,?FRU_VERSION:4, Offsets:5/binary, _/binary>>) ->
    OffsetTuple = list_to_tuple(binary_to_list(Offsets)),
    decode(is_header_sane(FruData), OffsetTuple, FruData);
decode(_FruData) ->
    {error, unsupported_fru_data}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode(false, _Offsets, _FruData) ->
    {error, incorrect_header_checksum};
decode(true, {_, C, B, P, M}, FruData) ->
    _MultiRecord = get_area(M, [], FruData),
    {ok,
     decode_chassis(get_area(C, non_zero([B, P, M]), FruData))
     ++ decode_board(get_area(B, non_zero([P, M]), FruData))
     ++ decode_product(get_area(P, non_zero([M]), FruData))}.

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
decode_field(Name, _Lang, <<2:2, Len:6, _Data:Len/binary, Rest/binary>>) ->
    {[{Name, '6bit_packed_ascii_unsupported'}], Rest};
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
