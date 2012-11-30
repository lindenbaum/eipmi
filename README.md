EIPMI
=====

EIPMI is a native Erlang/OTP library application for RMCP/IPMI 1.5. The goal is
to implement the remote console part of the IPMI LAN interface, as specified
[here](http://www.intel.com/design/servers/ipmi/spec.htm). We would like to
provide a low threshold for learning and using EIPMI, aiming to make it fit well
with other Erlang/OTP concepts and solutions.

Initial planned features
------------------------

* Simple API and user work-flow
* Extensible design, open for contributions
* RMCP presence ping/pong
* Session initiation and activation
* Some basic IPMI queries

We really want to provide a simple enough basic implementation, that is open
for extension, and not going for a complete coverage of the specification right
away - that should be added by the community as the needs appear.

Contributing
------------

If you whish to contribute fixes or enhancements, please make your code look
nice, for example using the Emacs Erlang-mode, formatting your code before
committing. Also, you should always write proper `edoc` module documentation.
When writing documentation, please try to keep the tone simple and on a higher
abstraction level, so that users may understand the concepts without having to
know too much of the standard.

The modules `eipmi_request` and `eipmi_response` are a starting point for
developers that want add support for not yet implemented request/response pairs.
To add support for a new request the encoding of the corresponding IPMI data
part needs to be added to the `eipmi_request` module. To add support for a new
response the decoding of the corresponding IPMI data part needs to be added to
the `eipmi_response` module. As soon as encoding and decoding is implemented the
command is ready to be issued using `eipmi:raw/4`. For features that need to
combine multiple requests (e.g. reading the FRU) it would be even more nice to
add a corresponding frontend API function to the `eipmi` module.

Documentation
-------------

The following sections will give a brief description of the EIPMI features as
well as some usage examples for developers integrating the application into
their project. Additional information can be found in the projects EDoc
documentation located [here](http://lindenbaum.github.com/eipmi/). The EIPMI
API functions are exported by the `eipmi` module.

### Sessions &amp; Authentication

EIPMI does all the necessary session handling for the user as soon as a session
is requested using `eipmi:open/1` or `eipmi:open/2`. However, according to the
capabilities of the target BMC the user eventually has to pass in user and
password credentials using the `Options` argument of `eipmi:open/2`.

All authentication mechanism mentioned in the specification are supported,
including *anonymous*, *null user* and *non-null user*. Additionally, all
digester algorithms proposed by the specification are supported.

In case the target BMC only supports *non-null* users the options `user` and
`password` need to be passed to a call to `eipmi:open/2`. In case *null users*
are configured on the BMC only the `password` option will be required. If the
BMC supports *anonymous* logins no options need to be set.

A session may be shared between mutliple processes. While the requests of one
process will be synchronous and thus ordered, requests from different processes
will not block each other. However, flow control is performed over all requests
of a session. If a maximum of 8 pending requests is reached new requests will
be queued and sent as pending requests get completed.

### Asynchronous Events

The API of EIPMI has been designed to be as similar as possible to existing
erlang protocol implementations (e.g. `gen_udp`). However, since sessions may be
share between multiple processes it is not possible to send asynchronous events
to a specific process.

Therefore, EIPMI provides the possibility to distribute asynchronous events for
all currently existing sessions through a `gen_event`. User can subscribe to
EIPMI events using `eipmi:subscribe/2`. Subscription can be cancelled using
`eipmi:unsubscribe/2`. The subscriber must implement the `gen_event` behaviour
and be prepared to receive the following events on the `handle_event/2`
callback:

```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 established}
```
The session was successfully established and activated.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {closed, Reason :: term()}}
```
The session was closed with the provided reason.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {decode_error, Reason :: term()}}
```
A received packet could not be decoded.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {timeout, RqSeqNr :: 0..63}}
```
The corresponding request timed out.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {unhandled, {call, Request :: term()}}}
```
The session received an invalid `gen_server` call.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {unhandled, {cast, Request :: term()}}}
```
The session received an invalid `gen_server` cast.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {unhandled, {info, Info :: term()}}}
```
The session received an invalid message.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip_address() | inet:hostname(),
 {unhandled, {ipmi, {ok | error, term()}}}}
```
The session received an IPMI response but no handler was found for it.

### Building

If you are using [rebar](http://github.com/basho/rebar) to build your project
and want to use EIPMI just specify it as a dependency in your `rebar.config`
(please note that the below link will change in the future as soon as EIPMI gets
released for the first time).

```erlang
{deps,
 [{eipmi, "1.0.0", {git, "https://github.com/lindenbaum/eipmi.git"}}]}.
```

### Usage

IPMI device discovery usually starts with a PING message to the target host:

```erlang
case eipmi:ping("10.1.31.11") of
     pong ->
          ipmi_supported;
     pang ->
          ipmi_not_supported
end,
```

After successful device discovery an actual session can be opened. This example
will open a session, read the FRU with id 253 and close the session again.

```erlang
{ok, Session} = eipmi:open("10.1.31.11"),
{ok, {fru_data, FruInfo}} = eipmi:read_fru(Session, 253),
BoardArea = proplists:get_value(board_area, FruInfo),
Name = proplists:get_value(name, BoardArea),
Serial = proplists:get_value(serial_number, BoardArea),
error_logger:info_msg("Board ~s has serial number ~s.~n", [Name, Serial]),
eipmi:close(Session),
```

The following snippet first reads the BMC's Sensor Data Record Repository and
then returns the complete FRU inventory based on the found FRU Device Locator
Records.

```erlang
{ok, Session} = eipmi:open("10.1.31.11"),
{ok, SDRRepository} = eipmi:read_sdr_repository(Session),
{ok, FruInventory} = eipmi:read_fru_inventory(Session, SDRRepository),
error_logger:info_msg("FRU inventory:~n~p~n", [FruInventory]),
eipmi:close(Session),
```

EIPMI also allows to send *raw* requests over a session. However, raw does not
mean that binary data can be sent directly. The corresponding request/response
encode/decode functionality must be present. The following snippet will issue
the *Set Session Privilege Level* command, trying to set the current session
privilege to `administrator`.

```erlang
{ok, Response} = eipmi:raw(Session, 16#06, 16#3b, [{privilege, administrator}]),
NewPrivilege = proplists:get_value(privilege, Response),
error_logger:info_msg("New privilege level is ~p.~n", [NewPrivilege]),
```

### Command Support

The following commands are currently supported, e.g. can be sent as `raw`
request/response:

#### Get Device Id

Argument:
```erlang
[]
```

Return:
```erlang
[{device_id, non_neg_integer()},
 {device_revision, non_neg_integer()},
 {operation, normal | progress},
 {firmware_version, string()},
 {ipmi_version, string()},
 {device_support, [chassis | bridge | event_generator | event_receiver | fru_inventory | sel | sdr | sensor]},
 {manufacturer_id, non_neg_integer()},
 {product_id, non_neg_integer()}]
```

#### Cold Reset
Argument:
```erlang
[]
```

Return:
```erlang
[]
```

#### Warm Reset
Argument:
```erlang
[]
```

Return:
```erlang
[]
```

#### Get Self Test Results
Argument:
```erlang
[]
```

Return:
```erlang
[{result,
  self_tests_passed |
  self_tests_not_implemented |
  {{corrupted_devices, [sel | sdr | fru | ipmb_signal_lines]},
   {inaccessible_devices, [sdr | fru | boot_firmware | optional_firmware]}} |
  {fatal_hardware_error, non_neg_integer()},
  {device_specific_error, non_neg_integer(), non_neg_integer()}}]
```

#### Get Acpi Power State
Argument:
```erlang
[]
```

Return:
```erlang
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
  unknown},
 {device, d0 | d1 | d2 | d3 | unknown}]
```

#### Get Device GUID
Argument:
```erlang
[]
```

Return:
```erlang
[{guid, string()}]
```

#### Get System GUID
Argument:
```erlang
[]
```

Return:
```erlang
[{guid, string()}]
```

#### Get Channel Authentication Capabilities
Argument:
```erlang
[{privilege, callback | user | operator | administrator}]
```

Return:
```erlang
[{channel, non_neg_integer()},
 {auth_types, none | pwd | md2 | md5},
 {per_message_authentication_enabled, boolean()},
 {user_level_authentication_enabled, boolean()},
 {login_status, [null | non_null | anonymous]}]
```

#### Get Session Challenge
Argument:
```erlang
[{auth_type, none | pwd | md2 | md5},
 {user, string()}]
```

Return:
```erlang
[{session_id, non_neg_integer()},
 {challenge, binary()}]
```

#### Activate Session
Argument:
```erlang
[{auth_type, none | pwd | md2 | md5},
 {privilege, callback | user | operator | administrator},
 {challenge, binary()},
 {initial_outbound_seq_nr, non_neg_integer()}]
```

Return:
```erlang
[{session_id, non_neg_integer()},
 {inbound_seq_nr, non_neg_integer()},
 {auth_type, none | pwd | md2 | md5},
 {privilege, callback | user | operator | administrator}]
```

#### Set Session Privilege Level
Argument:
```erlang
[{privilege, callback | user | operator | administrator}]
```

Return:
```erlang
[{privilege, callback | user | operator | administrator}]
```

#### Close Session
Argument:
```erlang
[{session_id, non_neg_integer()}]
```

Return:
```erlang
[]
```

#### Get FRU Inventory Area Info
Argument:
```erlang
[{fru_id, 0..254}]
```

Return:
```erlang
[{area_size, non_neg_integer()},
 {access, by_words | by_bytes}]
```

#### Read FRU Data
Argument:
```erlang
[{fru_id, 0..254},
 {offset, non_neg_integer()},
 {count, 1..255}]
```

Return:
```erlang
[{count, 1..255},
 {data, binary()}]
```

#### Get SEL Info
Argument:
```erlang
[]
```

Return:
```erlang
[{version, string()},
 {entries, non_neg_integer()},
 {free_space, non_neg_integer()},
 {most_recent_addition, non_neg_integer()},
 {most_recent_erase, non_neg_integer()},
 {overflow, boolean()},
 {operations, [delete | partial_add | reserve | get_allocation_info]}]
```

#### Reserve SEL
Argument:
```erlang
[]
```

Return:
```erlang
[{reservation_id, non_neg_integer()}]
```

#### Get SEL Entry
Argument:
```erlang
[{record_id, non_neg_integer()}]
```

Return:
```erlang
[{next_record_id, non_neg_integer()},
 {data, binary()}]
```

#### Clear SEL
Argument:
```erlang
[{reservation_id, non_neg_integer()},
 {initiate, boolean()} (optional)]
```

Return:
```erlang
[{progress, completed | in_progress}]
```

#### Get SDR Repository Info
Argument:
```erlang
[]
```

Return:
```erlang
[{version, string()},
 {entries, non_neg_integer()},
 {free_space, non_neg_integer()},
 {most_recent_addition, non_neg_integer()},
 {most_recent_erase, non_neg_integer()},
 {overflow, boolean()},
 {operations, [delete | partial_add | reserve | get_allocation_info]}]
```

#### Reserve SDR Repository
Argument:
```erlang
[]
```

Return:
```erlang
[{reservation_id, non_neg_integer()}]
```

#### Get SDR
Argument:
```erlang
[{reservation_id, non_neg_integer()}, (optional)
 {record_id, non_neg_integer()},
 {offset, non_neg_integer()}, (optional)
 {count, 1..255} (optional)]
```

Return:
```erlang
[{next_record_id, non_neg_integer()},
 {data, binary()}]
```

#### Get IP/UDP/RMCP Statistics
Argument:
```erlang
[{clear_statistics, boolean()} (optional)]
```

Return:
```erlang
[{ip_packets_received, non_neg_integer()},
 {ip_header_errors, non_neg_integer()},
 {ip_address_errors, non_neg_integer()},
 {ip_fragmented_packets_received, non_neg_integer()},
 {ip_packets_transmitted, non_neg_integer()},
 {udp_packets_received, non_neg_integer()},
 {udp_proxy_packets_received, non_neg_integer()},
 {udp_proxy_packets_dropped, non_neg_integer()},
 {rmcp_packets_received, non_neg_integer()}]
```

#### Get LAN Configuration Parameters
Argument:
```erlang
[{parameter, 0..255},
 {set, 0..255}, (optional)
 {block, 0..255} (optional)]
```

Returns:
```erlang
[{data, binary()}]
```

#### Get PICMG Properties
Argument:
```erlang
[]
```

Returns:
```erlang
[{picmg_extension, string()},
 {max_fru_id, 0..254},
 {ipmc_fru_id, 0..254}]
```

#### Set FRU Activation Policy
Argument:
```erlang
[{fru_id, 0..254},
 {deactivation_locked, boolean()}, (optional)
 {locked, boolean()} (optional)]
```

Returns:
```erlang
[]
```

#### Get FRU Activation Policy
Argument:
```erlang
[{fru_id, 0..254}]
```

Returns:
```erlang
[{deactivation_locked, boolean()},
 {locked, boolean()}]
```

#### Set FRU Activation
Argument:
```erlang
[{fru_id, 0..254},
 {activate, boolean()}]
```

Returns:
```erlang
[]
```

#### FRU Control
Argument:
```erlang
[{fru_id, 0..254},
 {control, cold_reset | warm_reset | graceful_reboot | diagnostic_interrupt | quiesce}]
```

Returns:
```erlang
[]
```

#### Get Device Locator Record ID
Argument:
```erlang
[{fru_id, 0..254}]
```

Returns:
```erlang
[{record_id, non_neg_integer()}]
```
