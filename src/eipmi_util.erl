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
%%% A module providing common utility functions.
%%% @end
%%%=============================================================================

-module(eipmi_util).

-export([normalize/2,
         format/2,
         get_val/2,
         get_val/3,
         update_val/3,
         copy_val/3,
         merge_vals/2,
         from_bcd_plus/1,
         from_packed_ascii/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Normalizes a string or a binary to a binary with the specified length
%% (in bytes).
%% @end
%%------------------------------------------------------------------------------
-spec normalize(non_neg_integer(), string() | binary()) ->
                       binary().
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
-spec format(string(), [term()]) ->
                    string().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%------------------------------------------------------------------------------
%% @doc
%% Return the value of a property from a proplist.
%% @see proplists:get_value/2
%% @end
%%------------------------------------------------------------------------------
-spec get_val(atom(), proplists:proplist()) ->
                     term().
get_val(Property, PropList) ->
    proplists:get_value(Property, PropList).

%%------------------------------------------------------------------------------
%% @doc
%% Return the value of a property from a proplist.
%% @see proplists:get_value/3
%% @end
%%------------------------------------------------------------------------------
-spec get_val(atom(), proplists:proplist(), term()) ->
                     term().
get_val(Property, PropList, Default) ->
    proplists:get_value(Property, PropList, Default).

%%------------------------------------------------------------------------------
%% @doc
%% Update the value of a property in a proplist. This will remove all previous
%% values associated with the property.
%% @end
%%------------------------------------------------------------------------------
-spec update_val(atom(), term(), proplists:proplist()) ->
                        proplists:proplist().
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
-spec from_bcd_plus(binary()) ->
                           string().
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
-spec from_packed_ascii(binary()) ->
                               string().
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
