Onan
======

Onan is an Erlang tool for creating *immutable*, *consistent* and
*repeatable* builds.

Server
======

[The server portion of this application lives here](https://github.com/AeroNotix/onan_server/)

How to use
==========

You need to either run `onan bootstrap` on an existing rebarized
project or write a small configuration file specifically for onan.

The format should be familiar to users of rebar, with a couple of
minor differences.

```erlang
{namespace, "AeroNotix"}.
{name, "onan"}.
{vsn, "0.0.1"}.
{description, "Erlang dependency management tool."}.
{deps, [{"nox", "mouture", "0.0.1"},
        {"talentdeficit", "jsx", "1.4.5"}]}.
{server, "http://localhost:45045"}.
```

This is a full example of an onan.config file, in fact, it's the one
for onan itself!

* Namespace
  * This value will be used to provide a top-level grouping for
    repositories for a specific user. Eventually this namespace will
    be used for the username portion for authentication.
* Name
  * Simply the name of the project.
* Vsn
  * The version of a project, which _must_ be in the semantic version
    format.
* Description
  * A string for a small blurb about the project, what it is, why it
    exists.
* Deps
  * A list of tuples which describe a dependency.
  * The format of the tuple is:

```erlang
-type dependency() :: {string(), string(), string()}.
```
* Server
  * This is the remote server which should be queried for dependencies
    which are not available on the local machine and where deployments
    should end up.


Command Line
============

Retrieving dependencies
-----------------------

Once you have an onan.config file describing the project's
dependencies all you will need to do to retrieve your dependencies is
to enter:

```shell
onan deps
```

This will either copy the locally cached dependencies into a `deps`
folder or it will first fetch dependencies from the remote onan server
_then_ copy them into the local folder.

Deploying artefacts
-------------------

To be able to provide the project to a wider-audience, or simply be
able to use it in your other projects, you will need to first make it
available in an onan server.

```shell
onan deploy
```

This will package up the local project and push it over to the onan
server.

Bootstrapping from rebarized projects
-------------------------------------

Onan aims to provide a smooth progression from rebar. To that end, we
provide a command which will traverse a project with its full
dependency graph checked out into the local `deps` directory and push
these dependencies into the onan server (and cache them locally).

This makes it very easy to migrate to post-rebar realities.

```shell
onan bootstrap
```

Since we may not have all information for all dependencies, you may be
prompted for some information, example:

```shell
Which namespace should be used for jsx? =>
```

Whatever you type in here will be used for the namespace portion of
the deployment.

Once onan is established, this step should hopefully no longer be
required.

Why Onan exists
===============

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
* A rebar plugin is a ridiculous notion
  because:
  * Rebar is extremely poorly coded.
  * It uses strage semantics for calling into plugin code.
* Open deployment tool / open archival service. This is to prevent a
  single point of failure in the dependency network.
* SAT solvers will need to be employed for certain dependency graphs.
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
