EIPMI
=====

EIPMI is a native Erlang/OTP application for RMCP/IPMI. It implements the
remote console part of the IPMI LAN interface, as specified
[here](http://www.intel.com/design/servers/ipmi/spec.htm). To provide a low
threshold for learning and using EIPMI it is designed close to the well known
`inet` API (think of `gen_udp`). It supports IPMI v1.5 as well as v2.0 (RMCP+).

Contributing
------------

If you whish to contribute fixes or enhancements, use `rebar3 fmt -w` to format
your code before committing. Also, you should always write proper `edoc` module
documentation. When writing documentation, please try to keep the tone simple
and on a higher abstraction level, so that users may understand the concepts
without having to know too much of the standard.

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
their project. Additional information can be found in the projects EDoc/source
documentation. The EIPMI API functions are exported by the `eipmi` module.

### Sessions &amp; Authentication

EIPMI does all the necessary session handling for the user as soon as a session
is requested using `eipmi:open/1` or `eipmi:open/2`. However, according to the
capabilities of the target BMC the user eventually has to pass in user and
password credentials using the `Options` argument of `eipmi:open/2`. A user
process can immediatelly start using a session. All requests received before the
session is established will be queued and issued after the session is
established.

All authentication mechanisms mentioned in the specification are supported,
including *anonymous*, *null user* and *non-null user*. Additionally, all
digester algorithms proposed by the specification are supported.

In case the target BMC only supports *non-null* users the options `user` and
`password` need to be passed in a call to `eipmi:open/2`. In case *null users*
are configured on the BMC only the `password` option will be required. If the
BMC supports *anonymous* logins no options need to be set.

A session may be shared between mutliple processes. While the requests of one
process will be synchronous and thus ordered, requests from different processes
will not block each other. However, flow control is not performed by the session
and a user has to ensure that only a limited number of processes issue
concurrent requests over the same session.

The API of EIPMI has been designed to be as similar as possible to existing
erlang protocol implementations (e.g. `gen_udp`). Therefore, the session owner
(the process calling `open/1,2`) will get asynchronous messages for its
sessions. The messages are sent using ordinary Erlang messaging and should be
handled accordingly. Be prepared to receive messages of the following form:

```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address() | inet:hostname(),
 established}
```
The session was successfully established and activated.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {closed, Reason :: term()}}
```
The session was closed with the provided reason.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {decode_error, Reason :: term()}}
```
A received packet could not be decoded.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {timeout, {eipmi:request(), RqSeqNr :: 0..63}}}
```
The corresponding request timed out.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {unhandled, {call, Request :: term()}}}
```
The session received an invalid `gen_server` call.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {unhandled, {cast, Request :: term()}}}
```
The session received an invalid `gen_server` cast.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {unhandled, {info, Info :: term()}}}
```
The session received an invalid message.
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 SELEntry :: eipmi:sel_entry()}
```
A forwarded entry from the System Event Log (only when automatic SEL polling is
enabled).
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 {sel_read_error, Reason :: term()}}
```
An error occured when polling the System Event Log (only when automatic SEL
polling is enabled).
```erlang
{ipmi,
 Session :: eipmi:session(),
 Address :: inet:ip4_address(),
 Trap :: eipmi:trap()}
```
An IPMI platform event trap forwarded by the trap handling mechanism (only if
enabled).

An established session will be kept alive by the session state machine until
either `eipmi:close/1` gets called or the owner process exits.

### Platform Event Traps

EIPMI is capable of receiving and forwarding platform event traps. In order to
receive trap events you will need to have trap handling enabled __and__ have at
least one IPMI session open to the respective target device. The events will
then be delivered to all owners of currently open sessions to this device.

Traps will automatically be acknowledged using the *PET Acknowledge* command if
their sequence number (cookie) is set/specified. To enable trap handling you
will need to set at least one port number for the `trap_ports` property in the
EIPMI application environment, e.g. in your `sys.config` add the following

```erlang
...
{eipmi, [{trap_ports, [162]}]},
...
```

Please be aware that depending on the chosen port (to be standard compliant you
will need to use port 162) your VM will need the permission to open internet
privileged ports.

### Usage

The `eipmi` module contains the whole functionality this project currently has
to offer. It defines functions to manage sessions as well as functions to issue
implemented requests. Additionally it contains functions that abstract
functionality that usually need in-detail knowledge of the IPMI standard, e.g.
reading SDR or SEL entries.

The general process of session establishment is briefly outlined in the next
paragraphs. IPMI device discovery usually starts with a PING message to the
target host:

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
{ok, SDRRepository} = eipmi:get_sdr_repository(Session),
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

History
-------

### Master (4.0.0)

* Add support for the *Get/Set System Boot Options* command (thanks to @IslandUsurper)
* Add support for RMCP+ (IPMI v2.0, big thanks to @IslandUsurper)
* Add support for the *Get/Set Chassis Capabilities* command (thanks to @IslandUsurper)
* Add support for the *Get Chassis Status* command (thanks to @IslandUsurper)
* Add support for the *Chassis Control* command (thanks to @IslandUsurper)
* Add support for the *Chassis Reset* command (thanks to @IslandUsurper)
* Add support for the *Chassis Identify* command (thanks to @IslandUsurper)
* Add support for the *Set Power Restore Policy* command (thanks to @IslandUsurper)
* Add support for the *Get System Restart Cause* command (thanks to @IslandUsurper)
* Add support for the *Set Front Panel Enables* command (thanks to @IslandUsurper)
* Add support for the *Set Power Cycle Interval* command (thanks to @IslandUsurper)
* Add support for the *Get POH Counter* command (thanks to @IslandUsurper)
* Drop support for OTP releases older than 22.2

### Version 3.0.0

* Drop support for OTP releases older than 18.3
* Add support for receiving/decoding/dispatching *IPMI Platform Event* and
  *SNMP* (RFC 1157) *Trap* messages
* Support for the *Get LAN Configuration Parameters* command
* Support for the *Set LAN Configuration Parameters* command
* Add `eipmi:sessions/0` and remove return value from `eipmi:info/0`

### Version 2.0.1

* Fix SDR timestamp handling
* Enhance/harden response dispatching
* Enhance robustness of SEL polling
* Minor bug fixes

### Version 2.0.0

* `eipmi` is now available on [hex.pm](https://hex.pm/packages/eipmi)
* Make project compatible to rebar3/hex
* Rename `eipmi:stats/0` to `eipmi:info/0`
* Switch asynchronous notifications from `gen_event` to Erlang messages
* Introduce the concept of session owners
* Extended support for reading sensors

### Version 1.2.7

* Fix SDR timestamp handling

### Version 1.2.6

* Enhance/harden response dispatching
* Enhance robustness of SEL polling

### Version 1.2.5

* Allow FRU fields with broken type/length field

### Version 1.2.4

* Allow periodic SEL polling to fail for `30` seconds

### Version 1.2.3

* Unify error returns of `eipmi:read_fru/2` and `eipmi:read_frus/2`

### Version 1.2.2

* Fix decoding of `AMC P2P Connectivity Record`s
* Make session more robust against decode errors

### Version 1.2.1

* Allow non-standard sensor ids (plain string without type)
* Fix calls to deprecated crypto API

### Version 1.2.0

* Improved session startup (now synchronous)
* Improved connection loss detection

### Version 1.1.0

* Support the *Send Message* command (needed for OEM requests) as well as
  *double bridged* requests
* New `to_list` functions for FRU and SDR entries
* Fix sensor readings for threshold and generic sensors

### Version 1.0.0

* Compliant to IPMI v1.5 (LAN interface only)
* Presence Ping
* Session management including session keep alive
* Support for lots of requests/responses/sensors from the IPMI v1.5 standard
* Support for lots of requests/responses/sensors defined by the PICMG (ATCA/µTCA)
* Automatic polling of the System Event Log (SEL)
