# Cabal [![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal) [![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal) [![Build Status](https://secure.travis-ci.org/haskell/cabal.svg?branch=master)](http://travis-ci.org/haskell/cabal) [![Windows build status](https://ci.appveyor.com/api/projects/status/yotutrf4i4wn5d9y/branch/master?svg=true)](https://ci.appveyor.com/project/23Skidoo/cabal) [![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)

This Cabal Git repository contains the following packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Installing Cabal (with cabal)
-----------------------------

Assuming that you have a pre-existing, older version of `cabal-install`,
run:

~~~~
cabal install cabal-install
~~~~

To get the latest version of `cabal-install`. (You may want to `cabal
update` first.)

To install the latest version from the Git repository, clone the
Git repository and then run:

~~~~
(cd Cabal; cabal install)
(cd cabal-install; cabal install)
~~~~

Installing Cabal (without cabal)
--------------------------------

Assuming you don't have a pre-existing copy of `cabal-install`, run:

~~~~
cabal-install $ ./bootstrap.sh # running ./bootstrap.sh from within in cabal-install folder.
~~~~

For more details, and non-unix like systems, see the [README.md in cabal-install](cabal-install/README.md)

Building Cabal for hacking
--------------------------

The current recommended way of developing Cabal is to use the
`new-build` feature which [shipped in cabal-install-1.24](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/).  Assuming
that you have a sufficiently recent cabal-install (see above),
it is sufficient to run:

~~~~
cabal new-build cabal
~~~~

To build a local, development copy of cabal-install.  The location
of your build products will vary depending on which version of
cabal-install you use to build; see the documentation section
[Where are my build products?](http://cabal.readthedocs.io/en/latest/nix-local-build.html#where-are-my-build-products)
to find the binary (or just run `find -type f -executable -name cabal`).

Here are some other useful variations on the commands:

~~~~
cabal new-build Cabal # build library only
cabal new-build Cabal:unit-tests # build Cabal's unit test suite
cabal new-build cabal-tests # etc...
~~~~

**Dogfooding HEAD.**
Many of the core developers of Cabal dogfood `cabal-install` HEAD
when doing development on Cabal.  This helps us identify bugs
which were missed by the test suite and easily experiment with new
features.

The recommended workflow in this case is slightly different: you will
maintain two Cabal source trees: your production tree (built with a
released version of Cabal) which always tracks `master` and which you
update only when you want to move to a new version of Cabal to dogfood,
and your development tree (built with your production Cabal) that you
actually do development on.

In more detail, suppose you have checkouts of Cabal at `~/cabal-prod`
and `~/cabal-dev`, and you have a release copy of cabal installed at
`/opt/cabal/1.24/bin/cabal`.  First, build your production tree:

~~~~
cd ~/cabal-prod
/opt/cabal/1.24/bin/cabal new-build cabal
~~~~

This will produce a cabal binary (see also: [Where are my build products?](http://cabal.readthedocs.io/en/latest/nix-local-build.html#where-are-my-build-products)
).  Add this binary to your PATH,
and then use it to build your development copy:

~~~~
cd ~/cabal-dev
cabal new-build cabal
~~~~

Running tests
-------------

**Using Travis and AppVeyor.**
If you are not in a hurry, the most convenient way to run tests on Cabal
is to make a branch on GitHub and then open a pull request; our
continuous integration service on Travis and AppVeyor will build and
test your code.  Title your PR with WIP so we know that it does not need
code review.

Some tips for using Travis effectively:

* Travis builds take a long time.  Use them when you are pretty
  sure everything is OK; otherwise, try to run relevant tests locally
  first.

* Watch over your jobs on the [Travis website](http://travis-ci.org).
  If you know a build of yours is going to fail (because one job has
  already failed), be nice to others and cancel the rest of the jobs,
  so that other commits on the build queue can be processed.

* If you want realtime notification when builds of your PRs finish, we have a [Slack team](https://haskell-cabal.slack.com/). To get issued an invite, fill in your email at [this sign up page](https://haskell-cabal.herokuapp.com).

**How to debug a failing CI test.**
One of the annoying things about running tests on CI is when they
fail, there is often no easy way to further troubleshoot the broken
build.  Here are some guidelines for debugging continuous integration
failures:

1. Can you tell what the problem is by looking at the logs?  The
   `cabal-testsuite` tests run with `-v` logging by default, which
   is dumped to the log upon failure; you may be able to figure out
   what the problem is directly this way.

2. Can you reproduce the problem by running the test locally?
   See the next section for how to run the various test suites
   on your local machine.

3. Is the test failing only for a specific version of GHC, or
   a specific operating system?  If so, try reproducing the
   problem on the specific configuration.

4. Is the test failing on a Travis per-GHC build
   ([for example](https://travis-ci.org/haskell-pushbot/cabal-binaries/builds/208128401))?
   In this case, if you click on "Branch", you can get access to
   the precise binaries that were built by Travis that are being
   tested.  If you have an Ubuntu system, you can download
   the binaries and run them directly.

5. Is the test failing on AppVeyor?  Consider logging in via
   Remote Desktop to the build VM:
   https://www.appveyor.com/docs/how-to/rdp-to-build-worker/

If none of these let you reproduce, there might be some race condition
or continuous integration breakage; please file a bug.

**Running tests locally.**
To run tests locally with `new-build`, you will need to know the
name of the test suite you want.  Cabal and cabal-install have
several.  Also, you'll want to read [Where are my build products?](http://cabal.readthedocs.io/en/latest/nix-local-build.html#where-are-my-build-products)

The most important test suite is `cabal-testsuite`: most user-visible
changes to Cabal should come with a test in this framework.  See
[cabal-testsuite/README.md](cabal-testsuite/README.md) for more
information about how to run tests and write new ones.  Quick
start: use `cabal-tests` to run `Cabal` tests, and `cabal-tests
--with-cabal=/path/to/cabal` to run `cabal-install` tests
(don't forget `--with-cabal`! Your cabal-install tests won't
run without it).

There are also other test suites:

* `Cabal:unit-tests` are small, quick-running unit tests
  on small pieces of functionality in Cabal.  If you are working
  on some utility functions in the Cabal library you should run this
  test suite.

* `cabal-install:unit-tests` are small, quick-running unit tests on
  small pieces of functionality in cabal-install.  If you are working
  on some utility functions in cabal-install you should run this test
  suite.

* `cabal-install:solver-quickcheck` are QuickCheck tests on
  cabal-install's dependency solver.  If you are working
  on the solver you should run this test suite.

* `cabal-install:integration-tests2` are integration tests on some
  top-level API functions inside the `cabal-install` source code.

For these test executables, `-p` which applies a regex filter to the test
names.

Conventions
-----------

* Spaces, not tabs.

* Try to follow style conventions of a file you are modifying, and
  avoid gratuitous reformatting (it makes merges harder!)

* Format your commit messages [in the standard way](https://chris.beams.io/posts/git-commit/#seven-rules).

* A lot of Cabal does not have top-level comments.  We are trying to
  fix this.  If you add new top-level definitions, please Haddock them;
  and if you spend some time understanding what a function does, help
  us out and add a comment.  We'll try to remind you during code review.

* If you do something tricky or non-obvious, add a comment.

* If your commit only touches comments, you can use `[ci skip]`
  anywhere in the body of the commit message to avoid needlessly
  triggering the build bots.

* For local imports (Cabal module importing Cabal module), import lists
  are NOT required (although you may use them at your discretion.)  For
  third-party and standard library imports, please use either qualified imports
  or explicit import lists.

* You can use basically any GHC extension supported by a GHC in our
  support window, except Template Haskell, which would cause
  bootstrapping problems in the GHC compilation process.

* Our GHC support window is five years for the Cabal library and three
  years for cabal-install: that is, the Cabal library must be
  buildable out-of-the-box with the dependencies that shipped with GHC
  for at least five years.  The Travis CI checks this, so most
  developers submit a PR to see if their code works on all these
  versions of GHC.  `cabal-install` must also be buildable on all
  supported GHCs, although it does not have to be buildable
  out-of-the-box. Instead, the `cabal-install/bootstrap.sh` script
  must be able to download and install all of the dependencies (this
  is also checked by CI). Also, self-upgrade to the latest version
  (i.e. `cabal install cabal-install`) must work with all versions of
  `cabal-install` released during the last three years.

* `Cabal` has its own Prelude, in `Distribution.Compat.Prelude`,
  that provides a compatibility layer and exports some commonly
  used additional functions. Use it in all new modules.

* As far as possible, please do not use CPP. If you must use it,
  try to put it in a `Compat` module, and minimize the amount of code
  that is enclosed by CPP.  For example, prefer:
  ```
  f :: Int -> Int
  #ifdef mingw32_HOST_OS
  f = (+1)
  #else
  f = (+2)
  #endif
  ```

  over:
  ```
  #ifdef mingw32_HOST_OS
  f :: Int -> Int
  f = (+1)
  #else
  f :: Int -> Int
  f = (+2)
  #endif
  ```

We like [this style guide][guide].

[guide]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md

Communicating
-------------

There are a few main venues of communication:

* Most developers subscribe to receive messages from [all issues](https://github.com/haskell/cabal/issues); issues can be used to [open discussion](https://github.com/haskell/cabal/issues?q=is%3Aissue+is%3Aopen+custom+label%3A%22type%3A+discussion%22).  If you know someone who should hear about a message, CC them explicitly using the @username GitHub syntax.

* For more organizational concerns, the [mailing
  list](http://www.haskell.org/mailman/listinfo/cabal-devel) is used.

* Many developers idle on `#hackage` on `irc.freenode.net` ([archives](http://ircbrowse.net/browse/hackage)).  `#ghc` ([archives](http://ircbrowse.net/browse/ghc)) is also a decently good bet.

Releases
--------

Notes for how to make a release are at the
wiki page ["Making a release"](https://github.com/haskell/cabal/wiki/Making-a-release).
Currently, @23Skidoo, @rthomas, @tibbe and @dcoutts have access to
`haskell.org/cabal`, and @davean is the point of contact for getting
permissions.

API Documentation
-----------------

Auto-generated API documentation for the `master` branch of Cabal is automatically uploaded here: http://haskell.github.io/cabal-website/doc/html/Cabal/.
