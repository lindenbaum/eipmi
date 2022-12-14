%%%=============================================================================
%%% Copyright (c) 2019-2020 Lindenbaum GmbH
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
%%% A server that listens for IPMI Platform Event Trap (PET) messages forwarding
%%% these to the owner of all currently active and matching IPMI sessions.
%%% Matching is performed on the provided agent IP address. The first matching
%%% session is used to acknowledge the trap if it's sequence number is
%%% specified (not null).
%%% @end
%%%=============================================================================

-module(eipmi_trap).

-behaviour(gen_server).

%% API
-export([
    decode/1,
    start_link/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("snmp/include/snmp_types.hrl").

-type trap() :: {trap, [property()]}.
-type trap_severity() ::
    monitor
    | information
    | ok
    | non_critical
    | critical
    | non_recoverable.
-type trap_source() ::
    firmware
    | smi_handler
    | isv_system
    | alert_asic
    | ipmi
    | bios_vendor
    | system_board
    | system_integrator
    | third_party_add_in
    | osv
    | nic
    | system_management_card.
-type property() ::
    {type,
        cold_start
        | warm_start
        | authentication_failure
        | egp_neighbor_loss
        | enterprise_specific
        | link_down
        | link_up}
    | {enterprise, [non_neg_integer()]}
    | {seq_nr, non_neg_integer()}
    | {data, binary()}
    | {event_severity, trap_severity()}
    | {event_source, trap_source()}
    | {event_source_raw, non_neg_integer()}
    | {guid, binary()}
    | {if_index, non_neg_integer()}
    | {local_time, non_neg_integer()}
    | {manufacturer_id, non_neg_integer()}
    | {oid, [non_neg_integer()]}
    | {sensor_device, non_neg_integer()}
    | {sensor_number, non_neg_integer()}
    | {sensor_type, eipmi_sensor:type()}
    | {system_id, non_neg_integer()}
    | {reading_type, eipmi_sensor:reading()}
    | {time, non_neg_integer()}
    | {trap_source, trap_source()}
    | {utc_offset, non_neg_integer()}
    | eipmi_sensor:entity()
    | eipmi_sensor:value()
    | #varbind{}.

-export_type([
    trap/0,
    trap_severity/0,
    trap_source/0,
    property/0
]).

-include("eipmi.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Decodes a binary representing an SNMP (PET) trap message in its erlang
%% record representation.
%% @end
%%------------------------------------------------------------------------------
-spec decode(binary() | list()) ->
    {ok, {inet:ip4_address(), trap()}} | {error, term()}.
decode(Binary) when is_binary(Binary) ->
    decode(binary_to_list(Binary));
decode(List) when is_list(List) ->
    C = application:get_env(eipmi, community, undefined),
    try snmp_pdus:dec_message(List) of
        #message{
            version = 'version-1',
            community = Community,
            data = Trap = #trappdu{}
        } when C =:= undefined; C =:= Community ->
            case trap_pdu(Trap) of
                {ok, Properties} -> {ok, {agent_addr(Trap), Properties}};
                Error = {error, _} -> Error
            end;
        #message{
            version = 'version-1',
            community = Community,
            data = #trappdu{}
        } ->
            {error, {invalid_community, Community}};
        #message{version = 'version-1'} ->
            {error, not_snmp_trap};
        #message{version = Version} ->
            {error, {unsupported, Version}}
    catch
        exit:{error, Reason} ->
            {error, Reason};
        error:function_clause ->
            {error, {not_snmp_pdu, List}};
        C:E ->
            {error, {decode_error, {C, E}, List}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Start a trap listener process on the given port number.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) -> gen_server:start_link(?MODULE, [Port], []).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

-record(state, {socket :: inet:socket()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Port]) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    {ok, #state{socket = Socket}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) -> {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(_Request, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp_closed, Socket}, State = #state{socket = Socket}) ->
    {stop, socket_closed, State};
handle_info({udp, Socket, SrcIP, _, Bin}, State = #state{socket = Socket}) ->
    try decode(Bin) of
        {ok, {AgentIP, Trap}} ->
            case lists:filter(forward(AgentIP, Trap), eipmi:sessions()) of
                [Session | _] -> acknowledge(Session, Trap);
                _ -> ok
            end;
        {error, Reason} ->
            eipmi_util:warn("Unsupported trap from ~w: ~w", [SrcIP, Reason])
    catch
        C:E ->
            Reason = {decode_error, {C, E}},
            eipmi_util:warn("Bad trap from ~w: ~w", [SrcIP, Reason])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Forward the trap to a matching sessions' owner.
%%------------------------------------------------------------------------------
forward(AgentIP, Trap) ->
    fun
        (Session = {session, {IP, _}, {Owner, _}}) when AgentIP =:= IP ->
            Owner ! {ipmi, Session, AgentIP, Trap},
            true;
        (_Session) ->
            false
    end.

%%------------------------------------------------------------------------------
%% @private
%% Acknowledge enterprise specific traps with a specified sequence number
%% (cookie).
%%------------------------------------------------------------------------------
acknowledge(Session, {trap, TrapProperties}) ->
    case proplists:get_value(type, TrapProperties) of
        enterprise_specific ->
            case proplists:get_value(seq_nr, TrapProperties, 0) of
                X when X > 0 ->
                    _ = eipmi:raw(
                        Session,
                        ?IPMI_NETFN_SENSOR_EVENT_REQUEST,
                        ?PET_ACKNOWLEDGE,
                        [{retransmits, 0} | TrapProperties]
                    );
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%% For some reason the `snmp_pdus' module may return IPv4 addresses as list of
%% integers.
%%------------------------------------------------------------------------------
agent_addr(#trappdu{agent_addr = Addr}) -> agent_addr(Addr);
agent_addr(List) when is_list(List) -> list_to_tuple(List).

%%------------------------------------------------------------------------------
%% @private
%% RFC 1157
%%------------------------------------------------------------------------------
trap_pdu(Trap = #trappdu{generic_trap = 0}) ->
    {ok, {trap, [{type, cold_start} | trap_misc(Trap)]}};
trap_pdu(Trap = #trappdu{generic_trap = 1}) ->
    {ok, {trap, [{type, warm_start} | trap_misc(Trap)]}};
trap_pdu(Trap = #trappdu{generic_trap = 2, varbinds = Vs}) ->
    Ps = [
        {if_index, If}
     || #varbind{
            variabletype = 'INTEGER',
            oid = [1, 3, 6, 1, 2, 1, 2, 2, 1, 8 | _],
            value = If
        } <- Vs
    ],
    {ok, {trap, [{type, link_down} | trap_misc(Trap)] ++ Ps}};
trap_pdu(Trap = #trappdu{generic_trap = 3, varbinds = Vs}) ->
    Ps = [
        {if_index, If}
     || #varbind{
            variabletype = 'INTEGER',
            oid = [1, 3, 6, 1, 2, 1, 2, 2, 1, 8 | _],
            value = If
        } <- Vs
    ],
    {ok, {trap, [{type, link_up} | trap_misc(Trap)] ++ Ps}};
trap_pdu(Trap = #trappdu{generic_trap = 4}) ->
    {ok, {trap, [{type, authentication_failure} | trap_misc(Trap)]}};
trap_pdu(Trap = #trappdu{generic_trap = 5, varbinds = Vs}) ->
    Ps = [
        {neighbor_addr, list_to_tuple(IP)}
     || #varbind{
            variabletype = 'IpAddress',
            oid = [1, 3, 6, 1, 2, 1, 8, 5, 1, 2 | _],
            value = IP
        } <- Vs
    ],
    {ok, {trap, [{type, egp_neighbor_loss} | trap_misc(Trap)] ++ Ps}};
trap_pdu(
    Trap = #trappdu{
        generic_trap = 6,
        enterprise = Enterprise,
        specific_trap = SpecificTrap,
        varbinds = Vs
    }
) ->
    Ps = [trap_enterprise_specific(Enterprise, SpecificTrap, V) || V <- Vs],
    {ok,
        {trap,
            [{type, enterprise_specific} | trap_misc(Trap)] ++ lists:append(Ps)}};
trap_pdu(Trap = #trappdu{}) ->
    {error, {unsupported, Trap}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_misc(#trappdu{enterprise = E, time_stamp = T}) ->
    [{enterprise, E}, {time, T}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_enterprise_specific(
    [1, 3, 6, 1, 4, 1, 3183, 1, 1],
    SpecificTrap,
    #varbind{
        oid = [1, 3, 6, 1, 4, 1, 3183, 1, 1, 1],
        variabletype = 'OCTET STRING',
        value = String
    }
) ->
    trap_ipmi_pet(<<SpecificTrap:32>>, list_to_binary(String));
trap_enterprise_specific(
    [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2],
    _SpecificTrap,
    #varbind{
        oid = [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2, 1, 1],
        variabletype = 'IpAddress',
        value = IP
    }
) ->
    [{main_ip, list_to_tuple(IP)}];
trap_enterprise_specific(
    [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2],
    _SpecificTrap,
    #varbind{
        oid = [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2, 1, 2],
        variabletype = 'IpAddress',
        value = IP
    }
) ->
    [{primary_ip, list_to_tuple(IP)}];
trap_enterprise_specific(
    [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2],
    _SpecificTrap,
    #varbind{
        oid = [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2, 1, 3],
        variabletype = 'OCTET STRING',
        value = Mac
    }
) ->
    [
        {mac_address,
            lists:foldr(
                fun
                    (0, Acc) -> Acc;
                    ($-, Acc) -> [$: | Acc];
                    (C, Acc) -> [C | Acc]
                end,
                "",
                Mac
            )}
    ];
trap_enterprise_specific(_, _, Varbind) ->
    [Varbind].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_ipmi_pet(
    <<?EIPMI_RESERVED:8, SensorType:8, EventType:8, Assertion:1,
        ?EIPMI_RESERVED:3, _Offset:4>>,
    <<GUID:16/binary, SeqNr:16, Time:32, UtcOffset:16, TrapSource:8,
        EventSource:8, Severity:8, Device:8, Number:8, Entity:8, Instance:8,
        Data:3/binary, DataRest:5/binary, _LangCode:8, Manufacturer:32,
        SystemId:16, _OEMCustomFields/binary>>
) ->
    {Reading, Type} = eipmi_sensor:get_type(EventType, SensorType),
    lists:append(
        [
            [{event_source_raw, EventSource}],
            trap_smbios_guid(GUID),
            unspecified_if_0(seq_nr, SeqNr),
            unspecified_if_0(local_time, Time),
            unspecified_if_0(utc_offset, UtcOffset),
            trap_source(trap_source, TrapSource),
            trap_source(event_source, EventSource),
            trap_severity(Severity),
            unspecified_if_0(manufacturer_id, Manufacturer),
            unspecified_if_0(system_id, SystemId),
            unspecified_if_ff(sensor_device, Device),
            unspecified_if_ff(unspecified_if_0(sensor_number, Number)),
            unspecified_if_0(eipmi_sensor:get_entity(Entity, Instance)),
            [{sensor_type, Type}, {reading_type, Reading}],
            eipmi_util:decode_event_data(Reading, Type, Assertion, Data),
            [{data, <<Data/binary, DataRest/binary>>}]
        ]
    ).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_smbios_guid(<<0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16>>) ->
    [];
trap_smbios_guid(<<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P>>) ->
    GUID = io_lib:format(
        "~2.16.0B~2.16.0B~2.16.0B~2.16.0B-"
        "~2.16.0B~2.16.0B-"
        "~2.16.0B~2.16.0B-"
        "~2.16.0B~2.16.0B-"
        "~2.16.0B~2.16.0B~2.16.0B~2.16.0B~2.16B~2.16.0B",
        [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    ),
    [{guid, iolist_to_binary(GUID)}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_source(T, X) when X < 16#7 -> [{T, firmware}];
trap_source(T, X) when X < 16#f -> [{T, smi_handler}];
trap_source(T, X) when X < 16#17 -> [{T, isv_system}];
trap_source(T, X) when X < 16#1f -> [{T, alert_asic}];
trap_source(T, X) when X < 16#27 -> [{T, ipmi}];
trap_source(T, X) when X < 16#2f -> [{T, bios_vendor}];
trap_source(T, X) when X < 16#37 -> [{T, system_board}];
trap_source(T, X) when X < 16#3f -> [{T, system_integrator}];
trap_source(T, X) when X < 16#47 -> [{T, third_party_add_in}];
trap_source(T, X) when X < 16#4f -> [{T, osv}];
trap_source(T, X) when X < 16#57 -> [{T, nic}];
trap_source(T, X) when X < 16#5f -> [{T, system_management_card}];
trap_source(_, _) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_severity(16#1) -> [{event_severity, monitor}];
trap_severity(16#2) -> [{event_severity, information}];
trap_severity(16#4) -> [{event_severity, ok}];
trap_severity(16#8) -> [{event_severity, non_critical}];
trap_severity(16#10) -> [{event_severity, critical}];
trap_severity(16#20) -> [{event_severity, non_recoverable}];
trap_severity(_) -> [].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unspecified_if_0(L) when is_list(L) ->
    lists:flatmap(fun({T, Val}) -> unspecified_if_0(T, Val) end, L).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unspecified_if_0(_, 0) -> [];
unspecified_if_0(T, Val) -> [{T, Val}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unspecified_if_ff(L) when is_list(L) ->
    lists:flatmap(fun({T, Val}) -> unspecified_if_ff(T, Val) end, L).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unspecified_if_ff(_, 16#ff) -> [];
unspecified_if_ff(T, Val) -> [{T, Val}].

%%%=============================================================================
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

trap_enterprise_specific_test() ->
    MacAddress =
        {varbind, [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2, 1, 3], 'OCTET STRING',
            [
                48,
                48,
                45,
                52,
                48,
                45,
                52,
                50,
                45,
                48,
                98,
                45,
                50,
                99,
                45,
                51,
                48,
                0
            ],
            3},
    ?assertEqual(
        [{mac_address, "00:40:42:0b:2c:30"}],
        trap_enterprise_specific(
            [1, 3, 6, 1, 4, 1, 27768, 1, 1, 2],
            1337,
            MacAddress
        )
    ).

%% TEST
-endif.
