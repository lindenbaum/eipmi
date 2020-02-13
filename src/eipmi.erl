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
%%% The main entry module of the `eipmi' application containing the application
%%% callback as well as the top level supervisor.
%%%
%%% This module provides capabilities to discover and use IPMI-capable devices.
%%% The {@link eipmi_session} module provides a IPMI session implementation that
%%% is able to send requests and receive responses implemented in the
%%% {@link eipmi_request} and {@link eipmi_response} modules. Frontend API
%%% functions using a combination of several requests to provide a certain
%%% feature should be put here.
%%% @end
%%%=============================================================================
-module(eipmi).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([ping/1,
         ping/2,
         open/1,
         open/2,
         close/1,
         get_device_id/1,
         cold_reset/1,
         warm_reset/1,
         get_self_test_results/1,
         get_acpi_power_state/1,
         get_device_guid/1,
         send_message/7,
         get_system_guid/1,
         set_session_privilege_level/2,
         read_fru/2,
         read_frus/2,
         read_fru_inventory/2,
         get_sdr/2,
         get_sdr/3,
         read_sdr/2,
         read_sdr/3,
         get_sdr_repository/1,
         get_sdr_repository/2,
         get_sdr_repository_info/1,
         get_sel/2,
         poll_sel/1,
         poll_sel/3,
         clear_sel/1,
         get_sel_info/1,
         set_lan_configuration_parameters/3,
         get_lan_configuration_parameters/3,
         get_ip_udp_rmcp_statistics/2,
         get_picmg_properties/1,
         get_address_info/1,
         get_address_info/2,
         get_address_info/3,
         set_fru_activation_policy/3,
         get_fru_activation_policy/2,
         set_fru_activation/3,
         fru_control/3,
         get_device_locator_record_id/2,
         oem/4,
         raw/4,
         sel_to_sdr/2,
         sel_to_fru/2,
         sel_to_fru/3,
         sdr_to_fru/2,
         sdr_to_fru/3,
         id_to_fru/2,
         sessions/0,
         info/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-include("eipmi.hrl").

-type target() :: {inet:ip4_address(), inet:port_number()}.

-type session() :: {session, target(), {pid(), reference()}}.

-type req_net_fn()  :: 0..62. %% 0x00-0x3e
-type resp_net_fn() :: 1..63. %% 0x01-0xef

-type request() :: {req_net_fn(), Command :: 0..255}.
-type response() :: {resp_net_fn(), Command :: 0..255}.

-type privilege() :: callback | user | operator | administrator.

-type option_name() ::
        initial_outbound_seq_nr | keep_alive_retransmits | password | port |
        privilege | rq_addr | timeout | user.
-type option() ::
        {initial_outbound_seq_nr, non_neg_integer()} |
        {keep_alive_retransmits, non_neg_integer()} |
        {password, string()} |
        {port, inet:port_number()} |
        {privilege, privilege()} |
        {rq_addr, 16#81..16#8d} |
        {timeout, non_neg_integer()} |
        {user, string()}.

-type repository_info() ::
        [{version, string()} |
         {entries, non_neg_integer()} |
         {free_space, non_neg_integer()} |
         {most_recent_addition, non_neg_integer()} |
         {most_recent_erase, non_neg_integer()} |
         {overflow, boolean()} |
         {operations, [delete | partial_add | reserve | get_allocation_info]}].
-type fru_info() :: eipmi_fru:info().
-type fru_inventory() :: [fru_info()].
-type sdr_info() :: repository_info().
-type sdr() :: eipmi_sdr:entry().
-type sdr_reading() :: [eipmi_sdr:reading()].
-type sdr_repository() :: [sdr()].
-type sel_info() :: repository_info().
-type sel_entry() :: eipmi_sel:entry().
-type trap() :: eipmi_trap:trap().

-type device_support() ::
        chassis | bridge | event_generator | event_receiver | fru_inventory |
        sel | sdr | sensor.
-type device_info() ::
        [{device_id, non_neg_integer()} |
         {device_revision, non_neg_integer()} |
         {operation, normal | progress} |
         {firmware_version, string()} |
         {ipmi_version, string()} |
         {device_support, [device_support()]} |
         {manufacturer_id, non_neg_integer()} |
         {product_id, non_neg_integer()}].

-type self_test() ::
        [{result,
          self_tests_passed |
          self_tests_not_implemented |
          {{corrupted_devices,
            [sel | sdr | fru | ipmb_signal_lines]},
           {inaccessible_devices,
            [sdr | fru | boot_firmware | optional_firmware]}} |
          {fatal_hardware_error, non_neg_integer()},
          {device_specific_error, non_neg_integer(), non_neg_integer()}}].

-type power_state() ::
        [{system,
          {s0_g0, working} |
          {s1, clocks_stopped} |
          {s2, clocks_stopped} |
          {s3, suspend_to_ram} |
          {s4, suspend_to_disk} |
          {s5_g2, soft_off} |
          {s4_s5, soft_off} |
          {g3, mechanical_off} |
          {s1_s2_s3, sleeping} |
          {g1, sleeping} |
          {s5, override} |
          legacy_on |
          legacy_off |
          unknown}|
         {device, d0 | d1 | d2 | d3 | unknown}].

-type network_statistics() ::
        [{ip_packets_received, non_neg_integer()} |
         {ip_header_errors, non_neg_integer()} |
         {ip_address_errors, non_neg_integer()} |
         {ip_fragmented_packets_received, non_neg_integer()} |
         {ip_packets_transmitted, non_neg_integer()} |
         {udp_packets_received, non_neg_integer()} |
         {udp_proxy_packets_received, non_neg_integer()} |
         {udp_proxy_packets_dropped, non_neg_integer()} |
         {rmcp_packets_received, non_neg_integer()}].

-type lan_configurations() ::
        [{set, 0..15} |
         {parameter, 0..19} |
         {set_in_progress, set_complete | set_in_progress | commit_write} |
         {auth_types, [eipmi_auth:type() | oem]} |
         {callback_level_auth_types, [eipmi_auth:type() | oem]} |
         {user_level_auth_types, [eipmi_auth:type() | oem]} |
         {operator_level_auth_types, [eipmi_auth:type() | oem]} |
         {administrator_level_auth_types, [eipmi_auth:type() | oem]} |
         {oem_level_auth_types, [eipmi_auth:type() | oem]} |
         {ip_assignment, bios_or_system | dhcp | static | other} |
         {subnet_mask, inet:ip4_address()} |
         {ttl, byte()} |
         {precendence, 0..15} |
         {type_of_service, 0..7} |
         {primary_port, inet:port_number()} |
         {secondary_port, inet:port_number()} |
         {ip_address, inet:ip4_address()} |
         {mac_address, {byte(), byte(), byte(), byte(), byte(), byte()}} |
         {default_gateway, inet:ip4_address()} |
         {default_gateway_mac_address, {byte(), byte(), byte(), byte(), byte(), byte()}} |
         {backup_gateway, inet:ip4_address()} |
         {backup_gateway_mac_address, {byte(), byte(), byte(), byte(), byte(), byte()}} |
         {community, string()} |
         {num_destinations, 0..15} |
         {acknowledge, boolean()} |
         {timeout, byte()} |
         {retries, 0..3} |
         {gateway, default|backup}].

-type picmg_properties() ::
        [{picmg_extension, string()} |
         {max_fru_id, 0..254} |
         {ipmc_fru_id, 0..254}].
-type picmg_site_type() ::
        picmg_board | power_entry | shelf_fru_information |
        dedicated_shelf_management_controller | fan_tray | fan_filter_tray |
        alarm | amc | pmc | rear_transition_module | mch | power_module.
-type picmg_address_info() ::
        [{mch_site_number, non_neg_integer()} |
         {ipmb_address, non_neg_integer()} |
         {fru_id, 0..254} |
         {site_number, non_neg_integer()} |
         {site_type, picmg_site_type()} |
         {carrier_number, non_neg_integer()}].
-type picmg_fru_control() ::
        cold_reset | warm_reset | graceful_reboot | diagnostic_interrupt |
        quiesce.

-export_type([target/0,
              session/0,
              req_net_fn/0,
              request/0,
              response/0,
              privilege/0,
              option/0,
              option_name/0,
              fru_info/0,
              fru_inventory/0,
              sdr_info/0,
              sdr/0,
              sdr_reading/0,
              sdr_repository/0,
              sel_info/0,
              sel_entry/0,
              trap/0,
              device_info/0,
              self_test/0,
              power_state/0,
              network_statistics/0,
              lan_configurations/0,
              picmg_properties/0,
              picmg_site_type/0,
              picmg_address_info/0,
              picmg_fru_control/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Pings a given host using RMCP ping. This can be done to check a device for
%% the IPMI capability before opening a session to it. Default receive timeout
%% is 5000 milliseconds. Returns `pong' if the pinged host supports IPMI,
%% `pang' otherwise.
%% @see ping/2
%% @end
%%------------------------------------------------------------------------------
-spec ping(inet:ip4_address() | inet:hostname()) -> pang | pong.
ping(IPAddress) ->
    ping(IPAddress, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Same as {@link ping/1} but allows the specification of a custom receive
%% timeout value in milliseconds.
%% @end
%%------------------------------------------------------------------------------
-spec ping(inet:ip4_address() | inet:hostname(), timeout()) -> pang | pong.
ping(IPAddress, Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    try do_ping(IPAddress, Timeout, Socket) of
        true -> pong;
        false -> pang
    catch
        _:_ -> pang
    after
        gen_udp:close(Socket)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Opens an IPMI session to a given host. With default parameters. Please note
%% that this will only work for IPMI targets supporting anonymous login. For all
%% other login types at least the `password' and maybe the `user' option will be
%% required.
%%
%% The returned handle can be used to send requests to the target BMC using one
%% of the functions provided by this module (e.g. {@link raw/4}) or close the
%% session using {@link close/1}.
%%
%% While the session may be used by arbitrary processes, the opening process is
%% simultaneously the session owner that will receive asynchronous messages
%% regarding the session. The session is terminated automatically, when the
%% owning process exits. The messages will be sent using ordinary Erlang
%% messaging and can be one of the following:
%% <dl>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       established}'
%%   </dt>
%%   <dd>
%%     <p>the session was successfully established and activated</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {closed, Reason :: term()}}'
%%   </dt>
%%   <dd>
%%     <p>the session was closed with the provided reason</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {decode_error, Reason :: term()}}'
%%   </dt>
%%   <dd>
%%     <p>a received packet could not be decoded</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {timeout, RqSeqNr :: 0..63}}'
%%   </dt>
%%   <dd>
%%     <p>the corresponding request timed out</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {unhandled, {call, term()}}}'
%%   </dt>
%%   <dd>
%%     <p>the session received an invalid `gen_server' call</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {unhandled, {cast, term()}}}'
%%   </dt>
%%   <dd>
%%     <p>the session received an invalid `gen_server' cast</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {unhandled, {info, term()}}}'
%%   </dt>
%%   <dd>
%%     <p>the session received an invalid message</p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       SELEntry :: sel_entry()}'
%%   </dt>
%%   <dd>
%%     <p>
%%       a SEL event forwarded through the automatic SEL polling mechanism
%%     </p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       {sel_read_error, Reason :: term()}}'
%%   </dt>
%%   <dd>
%%     <p>
%%       an error occured when polling the System Event Log
%%     </p>
%%   </dd>
%%   <dt>
%%     `{ipmi,
%%       Session :: session(),
%%       Address :: inet:ip4_address(),
%%       Trap :: trap()}'
%%   </dt>
%%   <dd>
%%     <p>
%%       an IPMI PET event forwarded by the trap handling mechanism
%%     </p>
%%   </dd>
%% </dl>
%% @see open/2
%% @see close/1
%% @end
%%------------------------------------------------------------------------------
-spec open(inet:ip4_address() | inet:hostname()) ->
                  {ok, eipmi:session()} | {error, term()}.
open(Host) ->
    open(Host, []).

%%------------------------------------------------------------------------------
%% @doc
%% Same as {@link open/1} but allows the specification of the following custom
%% options:
%% <dl>
%%   <dt>`{initial_outbound_seq_nr, non_neg_integer()}'</dt>
%%   <dd>
%%     <p>
%%     The initial outbound sequence number that will be requested on the BMC,
%%     default is `16#1337'.
%%     </p>
%%  </dd>
%%   <dt>`{keep_alive_retransmits, non_neg_integer()}'</dt>
%%   <dd>
%%     <p>
%%     The number of retransmits allowed for session keep_alive requests,
%%     default is `2'.
%%     </p>
%%  </dd>
%%   <dt>`{password, string() with length <= 16bytes}'</dt>
%%   <dd>
%%     <p>
%%     A password string used for authentication when anonymous login is not
%%     available, default is `""'.
%%     </p>
%%   </dd>
%%   <dt>`{port, inet:port_number()}'</dt>
%%   <dd>
%%     <p>
%%     The RMCP port the far end is expecting incoming RMCP and IPMI packets,
%%     default is `623'.
%%     </p>
%%   </dd>
%%   <dt>`{privilege, callback | user | operator | administrator}'</dt>
%%   <dd>
%%     <p>
%%     The requested privilege level for this session, default is
%%     `administrator'.
%%     </p>
%%   </dd>
%%   <dt>`{rq_addr, 16#81..16#8d}'</dt>
%%   <dd>
%%     <p>
%%     The requestor address used in IPMI lan packages, the default value of
%%     `16#81' should be suitable for all common cases.
%%     </p>
%%   </dd>
%%   <dt>`{timeout, non_neg_integer()}'</dt>
%%   <dd>
%%     <p>
%%     The timeout for IPMI requests, default is `1000ms'.
%%     </p>
%%   </dd>
%%   <dt>`{user, string() with length <= 16bytes}'</dt>
%%   <dd>
%%     <p>
%%     A user name string used for authentication when neither anonymous nor
%%     null user login are available, default is `""'.
%%     </p>
%%   </dd>
%% </dl>
%% @end
%%------------------------------------------------------------------------------
-spec open(inet:ip4_address() | inet:hostname(), [option()]) ->
                  {ok, eipmi:session()} | {error, term()}.
open(Host, Options) ->
    {ok, IPAddress} = inet:getaddr(Host, inet),
    Port = proplists:get_value(port, Options, ?RMCP_PORT_NUMBER),
    start_session({IPAddress, Port}, Options).

%%------------------------------------------------------------------------------
%% @doc
%% Closes an IPMI session previously opened with {@link open/1} or
%% {@link open/2}. This will return `{error, no_session}' when the given
%% session is not active any more.
%% @end
%%------------------------------------------------------------------------------
-spec close(eipmi:session()) -> ok | {error, term()}.
close(Session) ->
    Result = supervisor:terminate_child(?MODULE, Session),
    supervisor:delete_child(?MODULE, Session),
    Result.

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get Device Id' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_device_id(session()) -> {ok, device_info()} | {error, term()}.
get_device_id(Session) ->
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?GET_DEVICE_ID, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Cold Reset' request using the provided session. This will
%% perform a power cycle of the connected device!
%% @end
%%------------------------------------------------------------------------------
-spec cold_reset(session()) -> ok | {error, term()}.
cold_reset(Session) ->
    cold_reset_(raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?COLD_RESET, [])).
cold_reset_({error, timeout}) -> ok;
cold_reset_(Result) -> Result.

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Warm Reset' request using the provided session. This will
%% perform a reset of the connected device!
%% @end
%%------------------------------------------------------------------------------
-spec warm_reset(session()) -> ok | {error, term()}.
warm_reset(Session) ->
    warm_reset_(raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?WARM_RESET, [])).
warm_reset_({error, timeout}) -> ok;
warm_reset_(Result) -> Result.

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get Self Test Results' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_self_test_results(session()) -> {ok, self_test()} | {error, term()}.
get_self_test_results(Session) ->
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SELF_TEST_RESULTS, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get ACPI Power State' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_acpi_power_state(session()) -> {ok, power_state()} | {error, term()}.
get_acpi_power_state(Session) ->
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?GET_ACPI_POWER_STATE, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get Device GUID' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_device_guid(session()) -> {ok, [{guid, string()}]} | {error, term()}.
get_device_guid(Session) ->
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?GET_DEVICE_GUID, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Send Message' request to the specified device (addr/lun)
%% located on a bus behind the BMC. Please note that currently only 'tracked
%% requests' are supported. For more information refer to section 6.13.3 in the
%% IPMI reference. This is an example for a PICMG 'Double Bridged' request
%% sending an OEM specific command to the controller with the address `16#14'
%% located on an IPMB-L bus behind the BMC:
%% ```
%% eipmi:send_message(
%%   Session, 16#82, 0, 0, ?IPMI_NETFN_APPLICATION_REQUEST, ?SEND_MESSAGE,
%%   [{channel, 7},
%%    {request,
%%    [{net_fn,  16#30},
%%     {cmd,     16#02},
%%     {rs_addr, 16#14},
%%     {rs_lun,  16#00},
%%     {data,    <<0, 0, 16#80, 0>>}]}])
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec send_message(session(),
                   0..255,
                   0..4,
                   0..15,
                   req_net_fn(),
                   0..255,
                   proplists:proplist()) ->
                          ok | {ok, proplists:proplist()} | {error, term()}.
send_message(Session, TargetAddr, TargetLun, Channel, NetFn, Cmd, Properties) ->
    Data = [{net_fn, NetFn}, {cmd, Cmd}, {rs_addr, TargetAddr}, {rs_lun, TargetLun}],
    Args = [{channel, Channel}, {request, Data ++ Properties}],
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?SEND_MESSAGE, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get System GUID' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_system_guid(session()) -> {ok, [{guid, string()}]} | {error, term()}.
get_system_guid(Session) ->
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, ?GET_SYSTEM_GUID, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Set Session Privilege Level' request using the provided
%% session. This can be used to change the increase/decrease the privilege level
%% of the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec set_session_privilege_level(session(), privilege()) ->
                                         {ok, [{privilege, privilege()}]} |
                                         {error, term()}.
set_session_privilege_level(Session, Privilege) ->
    Args = [{privilege, Privilege}],
    Command = ?SET_SESSION_PRIVILEGE_LEVEL,
    raw(Session, ?IPMI_NETFN_APPLICATION_REQUEST, Command, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Return the FRU inventory data for a specific FRU id. The returned FRU
%% information is a property list that does only contain the available and
%% checksum error free fields of the inventory. If no FRU data is available for
%% a specific id the returned inventory data is the empty list.
%% @end
%%------------------------------------------------------------------------------
-spec read_fru(session(), 0..254) -> {ok, fru_info()} | {error, term()}.
read_fru(Session, FruId) ->
    with_session(Session, fun(Pid) -> eipmi_fru:read(Pid, FruId) end).

%%------------------------------------------------------------------------------
%% @doc
%% Basically the same than {@link read_fru/2} but allows to read a list of FRU
%% ids.
%% @see read_fru/2
%% @end
%%------------------------------------------------------------------------------
-spec read_frus(session(), [0..254]) -> {ok, [fru_info()]} | {error, term()}.
read_frus(Session, FruIds) ->
    lists:foldr(
      fun(FruId, {ok, FruInfos}) ->
              case read_fru(Session, FruId) of
                  {ok, FruInfo} ->
                      {ok, [FruInfo | FruInfos]};
                  Error ->
                      Error
              end;
         (_, Error) ->
              Error
      end, {ok, []}, FruIds).

%%------------------------------------------------------------------------------
%% @doc
%% Return the FRU inventory data for all FRU represented by a FRU Device Locator
%% Record in the Sensor Data Record (SDR) Repository.
%% @see read_fru/2
%% @end
%%------------------------------------------------------------------------------
-spec read_fru_inventory(session(), [eipmi_sdr:entry()]) ->
                                {ok, fru_inventory()} | {error, [term()]}.
read_fru_inventory(Session, SdrRepository) ->
    read_frus(Session, get_fru_ids(SdrRepository)).

%%------------------------------------------------------------------------------
%% @doc
%% Return a specific record from the Sensor Data Record (SDR) Repository.
%%
%% Reading of the SDR may fail (e.g. return `{error, term()}') when the
%% reservation for SDR reading with non-zero offsets gets cancelled. This is not
%% a severe error. It is most likely that the SDR can be read successfully when
%% retried.
%%
%% If the SDR repository has already been read, consider using get_sdr/3 which
%% does not involve session-related calls.
%% @end
%%------------------------------------------------------------------------------
-spec get_sdr(session(), non_neg_integer()) -> {ok, sdr()} | {error, term()}.
get_sdr(Session, RecordId) ->
    F = fun(Pid) -> eipmi_sdr:get(Pid, RecordId) end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Return the sensor record with the given type corresponding to a specific FRU
%% id from the Sensor Data Record (SDR) Repository, if any.
%% @end
%%------------------------------------------------------------------------------
-spec get_sdr(0..254, eipmi_sensor:type(), sdr_repository()) ->
                     {ok, [sdr()]} | {error, term()}.
get_sdr(FruId, SensorType, SdrRepository) ->
    case id_to_fru(FruId, SdrRepository) of
        {ok, {fru_device_locator, LocatorProps}} ->
            {ok, get_elements_by_properties(
                   maybe_keyfind(entity_id, 1, LocatorProps)
                   ++ maybe_keyfind(entity_instance, 1, LocatorProps)
                   ++ [{sensor_type, SensorType}],
                   SdrRepository)};
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Get the current sensor reading for a specific Sensor Data Record (SDR). Note
%% that only sensors of type `full' and `compact' can be read.
%% @end
%%------------------------------------------------------------------------------
-spec read_sdr(session(), sdr()) -> {ok, sdr_reading()} | {error, term()}.
read_sdr(Session, Sdr) ->
    F = fun(Pid) -> eipmi_sdr:get_sensor_reading(Pid, Sdr) end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Get the current sensor reading for a Sensor Data Record (SDR) referred to by
%% its number. The restrictions mentioned in {@link read_sdr/2} do also apply to
%% this function.
%% @end
%%------------------------------------------------------------------------------
-spec read_sdr(session(), non_neg_integer(), sdr_repository()) ->
                      {ok, sdr_reading()} | {error, term()}.
read_sdr(Session, SensorNumber, SdrRepository) ->
    F = fun(Pid) ->
                eipmi_sdr:get_sensor_reading(Pid, SensorNumber, SdrRepository)
        end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Return all records contained in the Sensor Data Record (SDR) Repository.
%% Please note that reading the complete SDR repository may take a significant
%% amount of time since there may be lots of sensor available.
%%
%% Reading of the SDR may fail (e.g. return `{error, term()}') when the
%% reservation for SDR reading with non-zero offsets gets cancelled. This is not
%% a severe error. It is most likely that the SDR can be read successfully when
%% retried.
%% @end
%%------------------------------------------------------------------------------
-spec get_sdr_repository(session()) -> {ok, sdr_repository()} | {error, term()}.
get_sdr_repository(Session) ->
    F = fun(Pid) -> eipmi_sdr:get_repository(Pid) end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Basically the same as {@link read_sdr_repository/1} but the SDR will only be
%% read when there were changes compared to the given SDR repository (regarding)
%% modification timestamps. In case the SDR repository did not receive any
%% updates the given old SDR repository is returned.
%%
%% Reading of the SDR may fail (e.g. return `{error, term()}') when the
%% reservation for SDR reading with non-zero offsets gets cancelled. This is not
%% a severe error. It is most likely that the SDR can be read successfully when
%% retried.
%% @end
%%------------------------------------------------------------------------------
-spec get_sdr_repository(session(), sdr_repository()) ->
                                {ok, sdr_repository()} | {error, term()}.
get_sdr_repository(Session, Previous) ->
    F = fun(Pid) -> eipmi_sdr:maybe_get_repository(Pid, Previous) end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get SDR Repository Info' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_sdr_repository_info(session()) -> {ok, sdr_info()} | {error, term()}.
get_sdr_repository_info(Session) ->
    raw(Session, ?IPMI_NETFN_STORAGE_REQUEST, ?GET_SDR_REPOSITORY_INFO, []).

%%------------------------------------------------------------------------------
%% @doc
%% Return all currently available entries from the System Event Log (SEL). The
%% returned SEL entries are property lists that do only contain the available
%% and checksum error free fields of the respective entry. Using the second
%% argument the SEL can optionally be cleared after reading.
%%
%% Clearing the SEL may fail in rare cases when the reservation for SEL
%% clearance gets cancelled by the BMC. This will be ignored. However in this
%% case the following read may return duplicates of already read events.
%% @end
%%------------------------------------------------------------------------------
-spec get_sel(session(), boolean()) -> {ok, [sel_entry()]} | {error, term()}.
get_sel(Session, Clear) ->
    F = fun(Pid) -> eipmi_sel:read(Pid, Clear) end,
    to_ok_tuple(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Will start a dedicated server that polls the system event log using a
%% specific session every 500ms. When polling is enabled the SEL will
%% periodically be read and all events will be forwarded to the session owner.
%%
%% The automatic polling can be stopped by shutting down or exiting the returned
%% process. The process will exit automatically when its corresponding session
%% is closed or has errors.
%% @end
%%------------------------------------------------------------------------------
-spec poll_sel(session()) -> {ok, pid()} | {error, term()}.
poll_sel(Session) ->
    poll_sel(Session, 500, true).

%%------------------------------------------------------------------------------
%% @doc
%% Basically the same as {@link poll_sel/1} but allows the specification of the
%% `Interval' and `Clear' options. While `Interval' specifies the minimum
%% interval between two consecutive SEL reads the `Clear' flag indicates whether
%% the SEL should be cleared after a read.
%% @end
%%------------------------------------------------------------------------------
-spec poll_sel(session(), non_neg_integer(), boolean()) ->
                      {ok, pid()} | {error, term()}.
poll_sel(Session = {session, _, _}, Interval, Clear) when Interval > 0 ->
    Children = supervisor:which_children(?MODULE),
    poll_sel(get_session(Session, Children), Session, Interval, Clear).
poll_sel({ok, Pid}, Session, Interval, Clear) ->
    start_poll(Pid, Session, [{read_sel, Interval}, {clear_sel, Clear}]);
poll_sel(Error, _Session, _Interval, _Clear) ->
    Error.

%%------------------------------------------------------------------------------
%% @doc
%% Clears all current records from the System Event Log (SEL).
%% @end
%%------------------------------------------------------------------------------
-spec clear_sel(session()) -> ok | {error, term()}.
clear_sel(Session) ->
    with_session(Session, fun(Pid) -> eipmi_sel:clear(Pid) end).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get SEL Info' request using the provided session.
%% @end
%%------------------------------------------------------------------------------
-spec get_sel_info(session()) -> {ok, sel_info()} | {error, term()}.
get_sel_info(Session) ->
    raw(Session, ?IPMI_NETFN_STORAGE_REQUEST, ?GET_SEL_INFO, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Set LAN Configuration Parameters' request using the provided
%% session. The needed `Properties' depend on the actual Parameter that should
%% get set. Refer to the {@link eipmi_request} module to see which properties
%% can be used.
%% @end
%%------------------------------------------------------------------------------
-spec set_lan_configuration_parameters(session(),
                                       0..19,
                                       lan_configurations()) ->
          ok | {error, term()}.
set_lan_configuration_parameters(Session, Parameter, Properties) ->
    raw(Session,
        ?IPMI_NETFN_TRANSPORT_REQUEST,
        ?SET_LAN_CONFIGURATION_PARAMETERS,
        [{parameter, Parameter} | Properties]).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get LAN Configuration Parameters' request using the provided
%% session. The `SetSelector' should be set to `0' if the queried parameter does
%% not support selection.
%% @end
%%------------------------------------------------------------------------------
-spec get_lan_configuration_parameters(session(), 0..19, 0..15) ->
          {ok, lan_configurations()} | {error, term()}.
get_lan_configuration_parameters(Session, Parameter, SetSelector) ->
    case
        raw(Session,
            ?IPMI_NETFN_TRANSPORT_REQUEST,
            ?GET_LAN_CONFIGURATION_PARAMETERS,
            [{parameter, Parameter}, {set, SetSelector}])
    of
        {ok, RawResult} ->
            RawData = proplists:get_value(data, RawResult, <<>>),
            case
                eipmi_response:decode_lan_configuration_parameters(Parameter,
                                                                   SetSelector,
                                                                   RawData)
            of
                []     -> {error, unsupported};
                Result -> {ok, Result}
            end;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get IP/UDP/RMCP Statistics' request using the provided session.
%% The statistics can optionally be cleared.
%% @end
%%------------------------------------------------------------------------------
-spec get_ip_udp_rmcp_statistics(session(), boolean()) ->
                                        {ok, network_statistics()} |
                                        {error, term()}.
get_ip_udp_rmcp_statistics(Session, Clear) ->
    A = [{clear_statistics, Clear}],
    raw(Session, ?IPMI_NETFN_TRANSPORT_REQUEST, ?GET_IP_UDP_RMCP_STATISTICS, A).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a 'Get PICMG Properties' request using the provided session. This
%% will return the (current) highest used FRU Id and the supported PICMG
%% extension (amongst others).
%% @end
%%------------------------------------------------------------------------------
-spec get_picmg_properties(session()) ->
                                  {ok, picmg_properties()} | {error, term()}.
get_picmg_properties(Session) ->
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_PICMG_PROPERTIES, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Get Address Info' request using the provided session.
%% This version will return addressing info of the implementing MCMC.
%% @end
%%------------------------------------------------------------------------------
-spec get_address_info(session()) ->
                              {ok, picmg_address_info()} | {error, term()}.
get_address_info(Session) ->
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO, []).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Get Address Info' request using the provided session.
%% This version will return addressing info associated with the requested FRU.
%% @end
%%------------------------------------------------------------------------------
-spec get_address_info(session(), 0..254) ->
                              {ok, picmg_address_info()} | {error, term()}.
get_address_info(Session, FruId) ->
    Args = [{fru_id, FruId}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Get Address Info' request using the provided session.
%% This version will return addressing info associated with the requested site.
%% @end
%%------------------------------------------------------------------------------
-spec get_address_info(session(), 0..254, picmg_site_type()) ->
                              {ok, picmg_address_info()} | {error, term()}.
get_address_info(Session, SiteType, SiteNumber) ->
    Args = [{site_type, SiteType}, {site_number, SiteNumber}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_ADDRESS_INFO, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Set FRU Activation Policy' request using the provided
%% session. This can be used to lock FRUs from beeing deactivated or from
%% getting reactivated automatically.
%% @end
%%------------------------------------------------------------------------------
-spec set_fru_activation_policy(
        session(),
        0..254,
        [{locked, boolean()} | {deactivation_locked, boolean()}]) ->
                                       ok | {error, term()}.
set_fru_activation_policy(Session, FruId, Flags) ->
    Args = [{fru_id, FruId}] ++ Flags,
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?SET_FRU_ACTIVATION_POLICY, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Get FRU Activation Policy' request using the provided
%% session.
%% @end
%%------------------------------------------------------------------------------
-spec get_fru_activation_policy(session(), 0..254) ->
                                       {ok, [{locked, boolean()} |
                                             {deactivation_locked, boolean()}]} |
                                       {error, term()}.
get_fru_activation_policy(Session, FruId) ->
    Args = [{fru_id, FruId}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_FRU_ACTIVATION_POLICY, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Set FRU Activation' request using the provided session.
%% This will power up/down the requested FRU.
%% @end
%%------------------------------------------------------------------------------
-spec set_fru_activation(session(), 0..254, boolean()) -> ok | {error, term()}.
set_fru_activation(Session, FruId, Activate) ->
    Args = [{fru_id, FruId}, {activate, Activate}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?SET_FRU_ACTIVATION, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'FRU Control' request using the provided session. This can
%% be used to power cycle, reset, etc. the requested FRU.
%% @end
%%------------------------------------------------------------------------------
-spec fru_control(session(), 0..254, picmg_fru_control()) -> ok | {error, term()}.
fru_control(Session, FruId, Action) ->
    Args = [{fru_id, FruId}, {control, Action}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?FRU_CONTROL, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Will issue a PICMG 'Get Device Locator Record Id' request using the provided
%% session. This can be used to retrieve the record id of a 'FRU Device Locator
%% Record' for a specific FRU. Having this idea makes it possible to read this
%% record directly using {@link get_sdr/2}.
%%
%% If the SDR repository is already available, the device locator record can
%% easier be retrieved using {@link id_to_fru/2}.
%% @end
%%------------------------------------------------------------------------------
-spec get_device_locator_record_id(session(), 0..254) ->
                                          {ok, [{record_id, non_neg_integer()}]} |
                                          {error, term()}.
get_device_locator_record_id(Session, FruId) ->
    Args = [{fru_id, FruId}],
    raw(Session, ?IPMI_NETFN_PICMG_REQUEST, ?GET_DEVICE_LOCATOR_RECORD_ID, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends an OEM specific IPMI command over a given session. DO NOT USE THIS
%% unless you really know what you're doing! The request data has to be given as
%% binary, the response will also be provided in binary format.
%% @end
%%------------------------------------------------------------------------------
-spec oem(session(), req_net_fn(), 0..255, binary()) ->
                 ok | {ok, binary()} | {error, term()}.
oem(Session, NetFn, Command, Binary) ->
    case raw(Session, NetFn, Command, [{data, Binary}]) of
        {ok, [{data, <<>>} | _]} ->
            ok;
        {ok, [{data, Data} | _]} ->
            {ok, Data};
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Sends a raw IPMI command over a given session. DO NOT USE THIS unless you
%% really know what you're doing! The main purpose of this function is to test
%% implementations of new request/repsonse pairs.
%% @end
%%------------------------------------------------------------------------------
-spec raw(session(), req_net_fn(), 0..255, proplists:proplist()) ->
                 ok | {ok, proplists:proplist()} | {error, term()}.
raw(Session, NetFn, Command, Properties) ->
    F = fun(Pid) -> eipmi_session:rpc(Pid, {NetFn, Command}, Properties) end,
    maybe_ok_return(with_session(Session, F)).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the Sensor Data Record (SDR) from the SDR repository associated with
%% an entry in the System Event Log (SEL), if any.
%% @end
%%------------------------------------------------------------------------------
-spec sel_to_sdr(sel_entry(), sdr_repository()) -> {ok, sdr()} | {error, term()}.
sel_to_sdr({_, SelEntryProps}, SdrRepository) ->
    get_element_by_properties(
      maybe_keyfind(sensor_number, 1, SelEntryProps)
      ++ maybe_keyfind(sensor_addr, 1, SelEntryProps)
      ++ maybe_keyfind(software_id, 1, SelEntryProps),
      SdrRepository).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU Device Locator Record associated with an entry in the System
%% Event Log (SEL), if any.
%% @end
%%------------------------------------------------------------------------------
-spec sel_to_fru(sel_entry(), sdr_repository()) -> {ok, sdr()} | {error, term()}.
sel_to_fru(SelEntry, SdrRepository) ->
    case sel_to_sdr(SelEntry, SdrRepository) of
        {ok, Sdr} ->
            sdr_to_fru(Sdr, SdrRepository);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU inventory data associated with an entry in the System
%% Event Log (SEL), if any.
%% @end
%%------------------------------------------------------------------------------
-spec sel_to_fru(sel_entry(), sdr_repository(), fru_inventory()) ->
                        {ok, fru_info()} | {error, term()}.
sel_to_fru(SelEntry, SdrRepository, FruInventory) ->
    case sel_to_sdr(SelEntry, SdrRepository) of
        {ok, Sdr} ->
            sdr_to_fru(Sdr, SdrRepository, FruInventory);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU Device Locator Record associated with a specific sensor from
%% the Sensor Data Record (SDR) repository, if any.
%% @end
%%------------------------------------------------------------------------------
-spec sdr_to_fru(sdr(), sdr_repository()) -> {ok, sdr()} | {error, term()}.
sdr_to_fru({_, SensorProps}, SdrRepository) ->
    get_element_by_properties(
      maybe_keyfind(entity_id, 1, SensorProps)
      ++ maybe_keyfind(entity_instance, 1, SensorProps),
      filter_by_key(fru_device_locator, SdrRepository)).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU inventory data associated with a specific sensor from the
%% Sensor Data Record (SDR) repository, if any.
%% @end
%%------------------------------------------------------------------------------
-spec sdr_to_fru(sdr(), sdr_repository(), fru_inventory()) ->
                        {ok, fru_info()} | {error, term()}.
sdr_to_fru(Sdr, SdrRepository, FruInventory) ->
    case sdr_to_fru(Sdr, SdrRepository) of
        {ok, {fru_device_locator, Props}} ->
            get_element_by_properties(
              maybe_keyfind(fru_id, 1, Props),
              FruInventory);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU Device Locator Record associated with a specific FRU id from
%% the Sensor Data Record (SDR) repository, if any.
%% @end
%%------------------------------------------------------------------------------
-spec id_to_fru(0..254, sdr_repository()) -> {ok, sdr()} | {error, term()}.
id_to_fru(FruId, SdrRepository) ->
    get_element_by_properties(
      [{fru_id, FruId}],
      filter_by_key(fru_device_locator, SdrRepository)).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the handles of all currently opened sessions.
%% @end
%%------------------------------------------------------------------------------
-spec sessions() -> [session()].
sessions() ->
    Cs = supervisor:which_children(?MODULE),
    [S || {S = {session, _, _}, P, _, _} <- Cs, is_pid(P)].

%%------------------------------------------------------------------------------
%% @doc
%% Prints information about the currently opened sessions.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> ok.
info() ->
    Sessions = sessions(),
    io:format("~w IPMI sessions:~n", [length(Sessions)]),
    [io:format(" * ~w~n", [Session]) || Session <- Sessions],
    ok.

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> ok.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    TrapPorts = application:get_env(?MODULE, trap_ports, []),
    {ok, {{one_for_one, 0, 1}, [trap_spec(Port) || Port <- TrapPorts]}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
trap_spec(Port) ->
    Start = {eipmi_trap, start_link, [Port]},
    {{trap, Port}, Start, permanent, brutal_kill, work, [eipmi_trap]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_session(Target, Options) ->
    Session = {session, Target, {self(), make_ref()}},
    Start = {eipmi_session, start_link, [Session, Options]},
    Spec = {Session, Start, temporary, 2000, worker, [eipmi_session]},
    case supervisor:start_child(?MODULE, Spec) of
        Error = {error, _} ->
            Error;
        Ok when element(1, Ok) =:= ok ->
            {ok, Session}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_poll(SessionPid, Session = {session, _, _}, Options) ->
    Id = {poll, erlang:make_ref()},
    Start = {eipmi_poll, start_link, [SessionPid, Session, Options]},
    Spec = {Id, Start, temporary, brutal_kill, worker, [eipmi_poll]},
    supervisor:start_child(?MODULE, Spec).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
with_session(Session, Fun) ->
    Children = supervisor:which_children(?MODULE),
    with_session_(get_session(Session, Children), Fun).
with_session_({ok, Pid}, Fun) -> ?EIPMI_CATCH(Fun(Pid));
with_session_(Error, _Fun) -> Error.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session(S, Cs) ->
    get_session([P || {Id, P, _, _} <- Cs, Id =:= S andalso is_pid(P)]).
get_session([])  -> {error, no_session};
get_session([P]) -> {ok, P}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping(IPAddress, Timeout, Socket) ->
    Ping = eipmi_encoder:ping(#rmcp_header{seq_nr = 0}, #asf_ping{}),
    ok = gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Ping),
    do_ping_receive(IPAddress, Timeout, Socket).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping_receive(IPAddress, Timeout, Socket) ->
    {ok, {_, _, Packet}} = gen_udp:recv(Socket, 8192, Timeout),
    case eipmi_decoder:packet(Packet) of
        {ok, #rmcp_ack{}} ->
            do_ping_receive(IPAddress, Timeout, Socket);
        {ok, #rmcp_asf{header = H, payload = #asf_pong{entities = Es}}} ->
            Ack = eipmi_encoder:ack(H),
            gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Ack),
            Es =:= [ipmi]
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_ok_tuple(Error = {error, _})      -> Error;
to_ok_tuple(Result = {ok, _})        -> Result;
to_ok_tuple(Result)                  -> {ok, Result}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_ok_return({ok, []}) -> ok;
maybe_ok_return(Result) -> Result.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fru_ids(SdrRepository) ->
    FruRecords = filter_by_key(fru_device_locator, SdrRepository),
    FruIds = [proplists:get_value(fru_id, Ps) || {_, Ps} <- FruRecords],
    [FruId || FruId <- FruIds, FruId =/= undefined].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_element_by_properties([], _List) ->
    {error, unresolvable};
get_element_by_properties(Props, List) ->
    case get_elements_by_properties(Props, List) of
        []            -> {error, {not_found, Props}};
        [Element | _] -> {ok, Element}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_elements_by_properties(Props, List) ->
    Pred = fun(Ps) -> lists:all(fun(P) -> lists:member(P, Ps) end, Props) end,
    [Element || Element = {_, Ps} <- List, Pred(Ps)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
filter_by_key(Key, List) -> [Element || Element = {K, _} <- List, K =:= Key].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_keyfind(Key, N, List) ->
    maybe_keyfind(lists:keyfind(Key, N, List)).
maybe_keyfind(false) ->
    [];
maybe_keyfind(Otherwise) ->
    [Otherwise].
