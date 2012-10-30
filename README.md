EIPMI
=====

EIPMI is a native Erlang/OTP library application for RMCP/IPMI 1.5. The goal is
to implement the remote console part of the IPMI LAN interface, as specified
here [http://www.intel.com/design/servers/ipmi/spec.htm]. We would like to
provide a low threshold for learning and using EIPMI, aiming to make it fit well
with other Erlang/OTP concepts and solutions.

Initial planned features
------------------------

* Simple API and user work-flow
* Extensible design, open for contributions
* RMCP presence ping/pong
* Session initiation and activation
* Some basic IPMI queries
* Support for LAN alerts (events)

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
documentation located under [http://lindenbaum.github.com/eipmi/].

### Sessions & Authentication

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
of a session. If a maximum of 64 pending requests is reached new requests will
be queued and sent as pending requests get completed.

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
{ok, FruInfo} = eipmi:read_fru(Session, 253),
BoardArea = proplists:get_value(board_area, FruInfo),
Name = proplists:get_value(name, BoardArea),
Serial = proplists:get_value(serial_number, BoardArea),
error_logger:info_msg("Board ~s has serial number ~s.~n", [Name, Serial]),
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
