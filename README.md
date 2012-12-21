EIPMI
=====

EIPMI is a native Erlang/OTP library application for RMCP/IPMI 1.5. The goal is
to implement the remote console part of the IPMI LAN interface, as specified
[here](http://www.intel.com/design/servers/ipmi/spec.htm). We would like to
provide a low threshold for learning and using EIPMI, aiming to make it fit well
with other Erlang/OTP concepts and solutions.

* [Code](http://github.com/lindenbaum/eipmi)
* [EDoc](http://lindenbaum.github.com/eipmi)

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
password credentials using the `Options` argument of `eipmi:open/2`. A user
process can immediatelly start using a session. All requests received before the
session is established will be queued and issued after the session is
established.

All authentication mechanism mentioned in the specification are supported,
including *anonymous*, *null user* and *non-null user*. Additionally, all
digester algorithms proposed by the specification are supported.

In case the target BMC only supports *non-null* users the options `user` and
`password` need to be passed to a call to `eipmi:open/2`. In case *null users*
are configured on the BMC only the `password` option will be required. If the
BMC supports *anonymous* logins no options need to be set.

A session may be shared between mutliple processes. While the requests of one
process will be synchronous and thus ordered, requests from different processes
will not block each other. However, flow control is not performed by the session
and a user has to ensure that only a limited number of processes issue
concurrent requests over the same session.

An established session will be kept alive by the session state machine until
`eipmi:close/1` gets called.

### Asynchronous Events

The API of EIPMI has been designed to be as similar as possible to existing
erlang protocol implementations (e.g. `gen_udp`). However, since sessions may be
share between multiple processes it is not possible to send asynchronous events
to a specific process.

Therefore, EIPMI provides the possibility to distribute asynchronous events for
all currently existing sessions through a `gen_event`. User can subscribe to
EIPMI events using `eipmi:add_handler/2` or `eipmi:add_sup_handler/2`.
Subscription can be cancelled using `eipmi:delete_handler/2`. The functions are
basically wrappers for the known `gen_event` functions. The subscriber must
therefore implement the `gen_event` behaviour and be prepared to receive the
following events on the `handle_event/2` callback:

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
 SELEntry :: eipmi:sel_entry()}
```
A forwarded entry from the System Event Log (only when automatic SEL polling is
enabled).

### Building

If you are using [rebar](http://github.com/basho/rebar) to build your project
and want to use EIPMI just specify it as a dependency in your `rebar.config`.
Please note that the below link will change in the future as soon as EIPMI gets
released for the first time. Do not use non-released (untagged) versions in
production environments!

```erlang
{deps,
 [{eipmi, "1.0.0", {git, "https://github.com/lindenbaum/eipmi.git"}}]}.
```

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
