EIPMI
=====

EIPMI is a native Erlang/OTP library application for IPMI/RMCP. The goal is to
provide a simple and pragmatic way for Erlang developers to work with IPMI.
We want to have a very low threshold for learning and using EIPMI, aiming to
make it fit well with other Erlang/OTP concepts and solutions.

Initial planned features
------------------------

* Simple API and user work-flow
* Extensible design, open for contributions
* RMCP presence ping/pong
* Session initiation and activation
* Some basic FRU queries
* Support for LAN events (traps)

We really want to provide a simple enough basic implementation, that is open
for extension, and not going for a complete coverage of the specification
[http://www.intel.com/design/servers/ipmi/spec.htm] right away.

Project structure
-----------------

This is a simple Erlang/OTP-compliant project using a simple structure that
should be trivially buildable using Maven, Rebar or a simple Makefile. We
are preferring Maven at the moment, making it easy to also build deploy and
manage EIPMI as a dependency-package, using the Maven-Erlang-Plugin
[http://erlang-plugin.sourceforge.net/].

To build, test, package and verify EIPMI with Maven, simply to:
```
  mvn verify
'''

Contributing
------------

If you whish to contribute fixes or enhancements, please make your code look
nice, for example using the Emacs Erlang-mode, formatting your code before
committing. Also, you should always write proper `edoc` module documentation.
When writing documentation, please try to keep the tone simple and on a higher
abstraction level, so that users may understand the concepts without having to
know too much of the standard.

Documentation
-------------

Currently there's not much, but we will focus on writing EDoc documentation,
meaning there will be sensible information in the source code modules. We will
also try to author a nice `overview.edoc` document, so that we can generate
an actual single-page programmers guide with links to the module documentation.

More to come...