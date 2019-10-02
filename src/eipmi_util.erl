%%%=============================================================================
%%% Copyright (c) 2012-2019 Lindenbaum GmbH
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
%%% A module providing common utility functions.
%%% @end
%%%=============================================================================

-module(eipmi_util).

-export([normalize/2,
         format/2,
         get_env/2,
         update_val/3,
         copy_val/3,
         merge_vals/2,
         from_bcd_plus/1,
         from_packed_ascii/1,
         from_base26/1,
         get_bool/1,
         binary_to_string/1,
         join_nl/1,
         decode_event_data/4]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Normalizes a string or a binary to a binary with the specified length
%% (in bytes).
%% @end
%%------------------------------------------------------------------------------
-spec normalize(non_neg_integer(), string() | binary()) -> binary().
normalize(Length, String) when is_list(String) ->
    normalize(Length, list_to_binary(String));
normalize(Length, Binary) when is_binary(Binary) ->
    case Length - byte_size(Binary) of
        0 ->
            Binary;
        PadSize when PadSize < 0 ->
            erlang:binary_part(Binary, 0, Length);
        PadSize ->
            <<Binary/binary, 0:(PadSize * 8)>>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% A flattening wrapper for {@link io_lib:format/2}.
%% @end
%%------------------------------------------------------------------------------
-spec format(string(), [term()]) -> string().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%------------------------------------------------------------------------------
%% @doc
%% Return the value of a configuration property from the eipmi application
%% environment.
%% @end
%%------------------------------------------------------------------------------
-spec get_env(atom(), term()) -> term().
get_env(Property, Default) ->
    proplists:get_value(Property, application:get_all_env(eipmi), Default).

%%------------------------------------------------------------------------------
%% @doc
%% Update the value of a property in a proplist. This will remove all previous
%% values associated with the property.
%% @end
%%------------------------------------------------------------------------------
-spec update_val(atom(), term(), proplists:proplist()) -> proplists:proplist().
update_val(Property, Value, PropList) ->
    [{Property, Value} | proplists:delete(Property, PropList)].

%%------------------------------------------------------------------------------
%% @doc
%% Copies the value of a property from one proplist to another. If the property
%% is not found in the original proplist the original destination proplist is
%% returned. This will remove all previous values associated with the property.
%% @end
%%------------------------------------------------------------------------------
-spec copy_val(atom(), proplists:proplist(), proplists:proplist()) ->
                      proplists:proplist().
copy_val(Property, DestPropList, SrcPropList) ->
    case proplists:get_value(Property, SrcPropList) of
        undefined ->
            DestPropList;

        Value ->
            update_val(Property, Value, DestPropList)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Merges two proplists. The resulting proplist will preserve all key/values from
%% proplist one. Additionally values for missing keys will be added from the
%% second proplist.
%% @end
%%------------------------------------------------------------------------------
-spec merge_vals(proplists:proplist(), proplists:proplist()) ->
                        proplists:proplist().
merge_vals(PropList1, PropList2) ->
    lists:usort(
      lists:foldl(
        fun({K, V}, Acc) ->
                update_val(K, V, Acc);
           (K, Acc) ->
                update_val(K, true, Acc)
        end,
        PropList2,
        PropList1)).

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a BCD plus encoded binary into a string.
%% @end
%%------------------------------------------------------------------------------
-spec from_bcd_plus(bitstring()) -> string().
from_bcd_plus(Bitstring) ->
    from_bcd_plus(Bitstring, "").
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
from_bcd_plus(<<16#d:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$: | Acc]);
from_bcd_plus(<<16#e:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$, | Acc]);
from_bcd_plus(<<16#f:4, Rest/bitstring>>, Acc) ->
    from_bcd_plus(Rest, [$_ | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a packed 6bit ASCII encoded binary into a string.
%% @end
%%------------------------------------------------------------------------------
-spec from_packed_ascii(binary()) -> string().
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
%% @doc
%% Decodes a string from a base 26 encoded integer where `0' corresponds to
%% `A' and `25' corresponds to `Z'.
%% @end
%%------------------------------------------------------------------------------
-spec from_base26(non_neg_integer()) -> string().
from_base26(Number) ->
    from_base26(Number, []).
from_base26(Number, Acc) when Number < 26 ->
    [65 + Number | Acc];
from_base26(Number, Acc) ->
    from_base26((Number div 26) - 1, [65 + Number rem 26 | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% Return `false' if given value is `0', `true' otherwise.
%% @end
%%------------------------------------------------------------------------------
get_bool(0) -> false;
get_bool(_) -> true.

%%------------------------------------------------------------------------------
%% @doc
%% Convert a binary into a string removing all contained zero bytes.
%% @end
%%------------------------------------------------------------------------------
-spec binary_to_string(binary()) -> string().
binary_to_string(Binary) ->
    [C || C <- binary_to_list(Binary), C =/= 0].

%%------------------------------------------------------------------------------
%% @doc
%% Convert a binary into a string removing all contained zero bytes.
%% @end
%%------------------------------------------------------------------------------
-spec join_nl([string()]) -> string().
join_nl(StringList) -> string:join(StringList, io_lib:nl()).

%%------------------------------------------------------------------------------
%% @doc
%% A function to decode event data bytes from SEL or PET events.
%% @end
%%------------------------------------------------------------------------------
-spec decode_event_data(eipmi_sensor:reading(),
                        eipmi_sensor:type(),
                        0 | 1,
                        binary()) -> [eipmi_sensor:value()].
decode_event_data(threshold, Type, Assertion, Data) ->
    decode_threshold(Type, Assertion, pad_event_data(Data));
decode_event_data(Reading, Type, Assertion, Data) when is_atom(Reading) ->
    decode_generic(Reading, Type, Assertion, pad_event_data(Data));
decode_event_data(Reading, Type, Assertion, Data) when is_integer(Reading) ->
    decode_oem(Reading, Type, Assertion, pad_event_data(Data)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_threshold(Type, Assertion, <<1:2, 0:2, Offset:4, B2:8, _:8>>) ->
    eipmi_sensor:get_value(threshold, Type, Offset, Assertion, 16#ff, 16#ff)
        ++ [{raw_reading, <<B2:8>>}];
decode_threshold(Type, Assertion, <<1:2, 1:2, Offset:4, B2:8, B3:8>>) ->
    eipmi_sensor:get_value(threshold, Type, Offset, Assertion, 16#ff, 16#ff)
        ++ [{raw_reading, <<B2:8>>}, {raw_threshold, <<B3:8>>}];
decode_threshold(Type, Assertion, <<E2:2, E3:2, Offset:4, B2:8, B3:8>>) ->
    Byte2 = case E2 of 0 -> 16#ff; _ -> B2 end,
    Byte3 = case E3 of 0 -> 16#ff; _ -> B3 end,
    eipmi_sensor:get_value(threshold, Type, Offset, Assertion, Byte2, Byte3).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_generic(Reading, Type, Assertion, <<1:2, E3:2, Off:4, SOff:4, POff:4, B3:8>>) ->
    Severity = maybe_value(severity_value, severity, Type, SOff, 0),
    Previous = maybe_value(previous_value, Reading, Type, POff, Assertion),
    decode_generic(Reading, Type, Assertion, <<0:2, E3:2, Off:4, 16#ff:8, B3:8>>)
        ++ Severity ++ Previous;
decode_generic(Reading, Type, Assertion, <<E2:2, E3:2, Offset:4, B2:8, B3:8>>) ->
    Byte2 = case E2 of 0 -> 16#ff; _ -> B2 end,
    Byte3 = case E3 of 0 -> 16#ff; _ -> B3 end,
    eipmi_sensor:get_value(Reading, Type, Offset, Assertion, Byte2, Byte3).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_oem(Reading, Type, Assertion, <<1:2, E3:2, Off:4, SOff:4, POff:4, B3:8>>) ->
    Severity = maybe_value(severity_value, severity, Type, SOff, 0),
    Previous = maybe_value(previous_value, Reading, Type, POff, Assertion),
    decode_oem(Reading, Type, Assertion, <<0:2, E3:2, Off:4, 16#ff:8, B3:8>>)
        ++ Severity ++ Previous;
decode_oem(Reading, Type, Assertion, <<E2:2, E3:2, Offset:4, B2:8, B3:8>>) ->
    Byte2 = case E2 of 2 -> B2; _ -> 16#ff end,
    Byte3 = case E3 of 2 -> B3; _ -> 16#ff end,
    eipmi_sensor:get_value(Reading, Type, Offset, Assertion, Byte2, Byte3).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
pad_event_data(Data = <<_:1/binary>>) -> <<Data/binary, 16#ff:8, 16#ff:8>>;
pad_event_data(Data = <<_:2/binary>>) -> <<Data/binary, 16#ff:8>>;
pad_event_data(Data = <<_:3/binary>>) -> Data.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_value(_Tag, _Reading, _Type, 16#f, _Assert) ->
    [];
maybe_value(Tag, Reading, Type, Offset, Assert) ->
    Vs = eipmi_sensor:get_value(Reading, Type, Offset, Assert, 16#ff, 16#ff),
    [{Tag, proplists:get_value(sensor_value, Vs)}].
