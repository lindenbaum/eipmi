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
         update_val/3,
         copy_val/3,
         merge_vals/2,
         no_badmatch/1]).

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
%% Evaluates the given function catching an occurence of badmatch errors. In
%% case of badmatch the offending term is returned, in case of normal function
%% completion its result is returned.
%% @end
%%------------------------------------------------------------------------------
-spec no_badmatch(fun(() -> term())) ->
                         term().
no_badmatch(Function) ->
    try Function() of
        Result -> Result
    catch
        error:{badmatch, Error} -> Error
    end.
