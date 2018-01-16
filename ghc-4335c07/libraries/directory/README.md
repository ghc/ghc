`directory`
===========

[![Hackage][hi]][hl]
[![Build status][bi]][bl]
[![Windows build status][wi]][wl]
[![Dependencies status][di]][dl]

Documentation can be found on [Hackage][hl].
Changes between versions are recorded in the [change log](changelog.md).

Building from Git repository
----------------------------

When building this package directly from the Git repository, one must run
`autoreconf -fi` to generate the `configure` script needed by `cabal
configure`.  This requires [Autoconf][ac] to be installed.

    autoreconf -fi
    cabal install

There is no need to run the `configure` script manually however, as `cabal
configure` does that automatically.

[hi]: https://img.shields.io/hackage/v/directory.svg
[hl]: https://hackage.haskell.org/package/directory
[bi]: https://travis-ci.org/haskell/directory.svg?branch=master
[bl]: https://travis-ci.org/haskell/directory
[wi]: https://ci.appveyor.com/api/projects/status/github/haskell/directory?branch=master&svg=true
[wl]: https://ci.appveyor.com/project/hvr/directory
[di]: https://img.shields.io/hackage-deps/v/directory.svg
[dl]: http://packdeps.haskellers.com/feed?needle=directory
[ac]: https://gnu.org/software/autoconf
