Reploy
======

Reploy is a rebar plugin for creating *immutable*, *consistent* and
*repeatable* builds.


Problem
=======

Rebar's current method of retrieving dependencies is completely and
*utterly* broken. There is no semblence of repeatability. You cannot
trust that between builds that the author of a project will not change
the world out from under you.


Solution
========

This project is an attempt to fix this. It tries to solve the
repeatability problem by archiving *fixed* and *named* versions of
dependencies in a persistent and immutable manner.

Dependencies and a project's dependency information are stored on a
third-party server.

When the server is queried for a project's code, it returns
prepackaged EZ (Erlang Zip files) back for *each* dependency in the
dependency graph. This means that transitive dependencies will need to
exist in the system. (A problem solved in time)

The server will not be a black box, currently no implementation of the
server exists but
[clear documentation for its API](http://docs.unfuckrebar.apiary.io/)
exists on Apiary.io.

How to use
==========

In your rebar.config you will need to include the base dependency to
reploy as a regular rebar dependency, this is a bootstrapping
intermediary fix and eventually reploy hopes to be its own
application without the need to use rebar.


Deploying
---------

```erlang

{plugins, [reploy]}.
{plugin_dir, "deps/"}.
%% If you are running your own reploy service, put its URI here.
{reploy_endpoint, "http://unfuckrebar.apiary.io"}.
{deps, [{reploy, ".*,
          {git, "http://github.com/AeroNotix/reploy.git", {tag, "0.0.1"}}}]}.
```

And then:

```shell

$ rebar deploy
```

This will read your app.src file and retrieve any metadata about your
project and include it into the hosted EZ file.


Specifying Dependencies
-----------------------

You will need to include the `reploy_deps` field in your rebar.config file:

```erlang

{reploy_deps, [{namespace, name, version}]}.
```

The format is as such:

```erlang

-type reploy_deps() :: [reploy_dep()].
-type reploy_dep() :: {atom(), atom(), string()} |
                      {atom(), atom(), string(), string()}.
```

Where:

* Namespace is usually the name of the user who uploaded the artefact.
* Name is the name of the project you want to specify a dependency on.
* Version which is in [semver format](http://semver.org/).
* Optionally, you can specify a separate remote host to retrieve your
  dependencies from.

Then in your shell run:

```shell

$ rebar deps
```

Which will talk to the third party service you set up and retrieve all
the dependencies.
