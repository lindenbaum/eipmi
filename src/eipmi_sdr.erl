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
%% @end
%%------------------------------------------------------------------------------
-spec get_info(pid()) ->
                      {ok, [info()]} | {error, term()}.
get_info(SessionPid) ->
    eipmi_session:request(SessionPid, ?GET_INFO, []).

%%------------------------------------------------------------------------------
%% @doc
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
decode_header(<<Id:16/little, Version:8, Type:8, L:8>>) ->
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
decode_full_sensor_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_compact_sensor_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_event_only_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_entity_association_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_device_relative_entity_association_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_generic_device_locator_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_fru_device_locator_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_device_locator_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_management_controller_confirmation_record(_Data) ->
    [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode_bmc_message_channel_info_record(_Data) ->
    [].
