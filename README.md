Onan
======

Onan is an Erlang tool for creating *immutable*, *consistent* and
*repeatable* builds.

Server
======

[The server portion of this application lives here](https://github.com/AeroNotix/onan_server/)


Problem
=======

Rebar's current method of retrieving dependencies is completely and
*utterly* broken. There is no semblence of repeatability. You cannot
trust that between builds that the author of a project will not change
the world out from under you. Here's why this doesn't work:

* tagging to a branch means that you always get the latest in that
  branch.
* Tagging to a VCS tag, tags are *not* immutable. (git tag -d $TAG)
* Tagging to a commit is one of the safest methods, but you are still
  at the whim of the maintainer to keep that branch / commit / etc
  around forever. E.g. a commit is ephemeral in the face of rebasing
  or branch deletion.


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
[clear documentation for its API](http://docs.onan.apiary.io/)
exists on Apiary.io.

How to use
==========

Create an `onan.config` file which looks similar to:

```erlang
{name, "onan"}.
{vsn, "0.0.1"}.
{deps, [{"onan", "0.0.1"}]}.
{onan_server, "http://localhost:8080"}.
```

Deploying
---------

TBD

Specifying Dependencies
-----------------------

You will need to include the `deps` field in your onan.config file:

```erlang
{deps, [{"name", "version"}]}.
```


Where:

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


Unordered Goals
---------------

* Immutable artefacts.
* Be transactional. No more partial downloads requiring a rm -rf of
  dependencies.
    * Can be solved by hashing the final set of files and looking for
      this hash (and comparing) on next dependency run.
* Versioned artefacts.
* Use rebar plugins as a bootstrap method (possible eventually a
  separate app). Scratch this. A rebar plugin is a ridiculous notion
  because:
  * Rebar is extremely poorly coded.
  * It uses strage semantics for calling into plugin code.
* Open deployment tool / open archival service. This is to prevent a
  single point of failure in the dependency network.
* Namespaced packages. (e.g. puzza/awesome_lib and rpt/awesome_lib )
    * These are now two separate projects.
* Projects under the same name mean that other projects with the same
  name *must* be forks.
    * Need to think how to enforce that.
    * Is this really necessary?
    * Could require that similarly named projects have their source
      trees available to resolve conflicts.
* Automatic metadata gathering (erts version / etc).
* Windows is not a target.
* When in doubt, do what lein / mvn does.
* Support releases. ( e.g. the whole release for an OTP application,
  such as Riak. )
* Remove irrelevant directories / files from artefacts.
* C code needs some thoughts.
* Arbitrary metadata (k/v).


Inspiration
-----------

This was inspired by working with the awesomeness that is Lein.
