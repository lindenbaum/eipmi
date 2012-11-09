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
%%% A module providing reading and decoding functionality for the System
%%% Event Log (SEL).
%%% @end
%%%=============================================================================

-module(eipmi_sel).

-export([read/2,
         clear/1]).

-include("eipmi.hrl").

-define(READ, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SEL_ENTRY}).
-define(GET_INFO, {?IPMI_NETFN_STORAGE_REQUEST, ?GET_SEL_INFO}).
-define(RESERVE, {?IPMI_NETFN_STORAGE_REQUEST, ?RESERVE_SEL}).
-define(CLEAR, {?IPMI_NETFN_STORAGE_REQUEST, ?CLEAR_SEL}).

-type property() ::
        eipmi_sensor:property() |
        {id, non_neg_integer()} |
        {type, non_neg_integer()} |
        {time, non_neg_integer()} |
        {maunfacturer_id, non_neg_integer()} |
        {data, binary()} |
        {revision, non_neg_integer()} |
        {sensor_number, non_neg_integer()} |
        {slave_addr, non_neg_integer()} |
        {slave_lun, non_neg_integer()} |
        {channel, non_neg_integer()} |
        {software_id, non_neg_integer()}.

-type entry() ::
        {system_event |
         oem_timestamped |
         oem_non_timestamped,
         [property()]}.

-export_type([entry/0, property/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Read all entries from the system event log and optionally clear the log.
%% Reading the SEL without clearing it is implemented in a way that will only
%% send the least number of packets (no SEL reservation or info requests). Thus,
%% if a user knows that read SEL entries will be cleared automatically by the
%% BMC the user should not clear the SEL explicitly.
%% @end
%%------------------------------------------------------------------------------
-spec read(pid(), boolean()) ->
                  [entry()].
read(SessionPid, true) ->
    {ok, SelInfo} = eipmi_session:request(SessionPid, ?GET_INFO, []),
    Entries = get_sel(SessionPid, eipmi_util:get_val(entries, SelInfo)),
    Operations = eipmi_util:get_val(operations, SelInfo),
    clear_sel(SessionPid, lists:member(reserve, Operations)),
    Entries;
read(SessionPid, false) ->
    get_sel(SessionPid, all).

%%------------------------------------------------------------------------------
%% @doc
%% Clear the system event log regardless whether events have been read/processed
%% by any user application.
%% @end
%%------------------------------------------------------------------------------
-spec clear(pid()) ->
                   ok | {error, term()}.
clear(SessionPid) ->
    clear_sel(SessionPid).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_sel(_SessionPid, 0) ->
    [];
get_sel(SessionPid, _NumEntries) ->
    get_sel(SessionPid, 16#0000, []).
get_sel(_SessionPid, 16#ffff, Acc) ->
    lists:reverse(Acc);
get_sel(SessionPid, Id, Acc) ->
    case eipmi_session:request(SessionPid, ?READ, [{record_id, Id}]) of
        {ok, Entry} ->
            NextId = eipmi_util:get_val(next_record_id, Entry),
            Sel = decode_event(eipmi_util:get_val(data, Entry)),
            get_sel(SessionPid, NextId, [Sel | Acc]);
        {error, {bmc_error, _}} ->
            Acc
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
clear_sel(SessionPid) ->
    {ok, SelInfo} = eipmi_session:request(SessionPid, ?GET_INFO, []),
    Operations = eipmi_util:get_val(operations, SelInfo),
    clear_sel(SessionPid, lists:member(reserve, Operations)).
clear_sel(SessionPid, true) ->
    {ok, Reserve} = eipmi_session:request(SessionPid, ?RESERVE, []),
    ReservationId = eipmi_util:get_val(reservation_id, Reserve),
    clear_sel(SessionPid, ReservationId, true, false);
clear_sel(SessionPid, false) ->
    clear_sel(SessionPid, 16#0000, true, false).
clear_sel(_SessionPid, _ReservationId, _Initiate, true) ->
    ok;
clear_sel(SessionPid, ReservationId, Initiate, false) ->
    Args = [{reservation_id, ReservationId}, {initiate, Initiate}],
    {ok, Clr} = eipmi_session:request(SessionPid, ?CLEAR, Args),
    Completed = eipmi_util:get_val(progress, Clr) =:= completed,
    clear_sel(SessionPid, ReservationId, false, Completed).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_event(<<Id:16/little, 16#02:8, Time:32/little, Rest/binary>>) ->
    {system_event, [{id, Id}, {time, Time}] ++ decode_system_event(Rest)};
decode_event(<<Id:16/little, Type:8, Time:32/little, M:24/little, Data/binary>>)
  when Type >= 16#c0 andalso Type =< 16#df ->
    {oem_timestamped,
     [{id, Id}, {type, Type}, {time, Time}, {maunfacturer_id, M}, {data, Data}]};
decode_event(<<Id:16/little, Type:8, Data/binary>>)
  when Type >= 16#e0 andalso Type =< 16#ff ->
    {oem_non_timestamped, [{id, Id}, {type, Type}, {data, Data}]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_system_event(<<Generator:2/binary, 16#04:8, SensorType:8, SensorNum:8,
                      Assertion:1, EventType:7, EventData/binary>>) ->
    Reading = eipmi_sensor:get_reading(EventType, SensorType),
    get_generator(Generator)
        ++ [{revision, 16#04}, {sensor_number, SensorNum}]
        ++ eipmi_sensor:decode_data(Reading, Assertion, EventData);
decode_system_event(<<Generator:2/binary, Revision:8, Data/binary>>) ->
    get_generator(Generator) ++ [{revision, Revision}, {data, Data}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_generator(<<Addr:7, 0:1, 0:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}];
get_generator(<<Addr:7, 0:1, Channel:4, ?EIPMI_RESERVED:2, Lun:2>>) ->
    [{slave_addr, Addr}, {slave_lun, Lun}, {channel, Channel}];
get_generator(<<Id:7, 1:1, 0:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}];
get_generator(<<Id:7, 1:1, Channel:4, ?EIPMI_RESERVED:2, 0:2>>) ->
    [{software_id, Id}, {channel, Channel}].

%%%=============================================================================
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_event_1_test() ->
    Decoded =
        decode_event(
          <<16#01, 16#00, 16#02, 16#0a, 16#53, 16#1b, 16#00, 16#20,
            16#00, 16#04, 16#f3, 16#03, 16#6f, 16#a1, 16#01, 16#0d>>),
    error_logger:info_msg("~n~p~n", [Decoded]).

-endif.
