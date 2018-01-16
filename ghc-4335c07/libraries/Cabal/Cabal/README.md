The Cabal library package
=========================

See the [Cabal web site] for more information.

If you also want the `cabal` command-line program, you need the
[cabal-install] package in addition to this library.

[cabal-install]: ../cabal-install/README.md

Installing the Cabal library
============================

If you already have the `cabal` program
---------------------------------------

In this case run:

    $ cabal install

However, if you do not have an existing version of the `cabal` program,
you first must install the Cabal library. To avoid this bootstrapping
problem, you can install the Cabal library directly as described below.


Installing as a user (no root or administrator access)
------------------------------------------------------

    ghc -threaded --make Setup
    ./Setup configure --user
    ./Setup build
    ./Setup install

Note the use of the `--user` flag at the configure step.

Compiling 'Setup' rather than using `runghc Setup` is much faster and
works on Windows. For all packages other than Cabal itself, it is fine
to use `runghc`.

This will install into `$HOME/.cabal/` on Unix and into
`Documents and Settings\$User\Application Data\cabal\` on Windows.
If you want to install elsewhere, use the `--prefix=` flag at the
configure step.


Installing as root or Administrator
-----------------------------------

    ghc -threaded --make Setup
    ./Setup configure
    ./Setup build
    sudo ./Setup install

Compiling Setup rather than using `runghc Setup` is much faster and
works on Windows. For all packages other than Cabal itself, it is fine
to use `runghc`.

This will install into `/usr/local` on Unix, and on Windows it will
install into `$ProgramFiles/Haskell`. If you want to install elsewhere,
use the `--prefix=` flag at the configure step.


Using older versions of GHC and Cabal
======================================

It is recommended that you leave any pre-existing version of Cabal
installed. In particular, it is *essential* you keep the version that
came with GHC itself, since other installed packages require it (for
instance, the "ghc" API package).

Prior to GHC 6.4.2, however, GHC did not deal particularly well with
having multiple versions of packages installed at once. So if you are
using GHC 6.4.1 or older and you have an older version of Cabal
installed, you should probably remove it by running:

    $ ghc-pkg unregister Cabal

or, if you had Cabal installed only for your user account, run:

    $ ghc-pkg unregister Cabal --user

The `filepath` dependency
=========================

Cabal uses the [filepath] package, so it must be installed first.
GHC version 6.6.1 and later come with `filepath`, however, earlier
versions do not by default. If you do not already have `filepath`,
you need to install it. You can use any existing version of Cabal to do
that. If you have neither Cabal nor `filepath`, it is slightly
harder but still possible.

Unpack Cabal and `filepath` into separate directories. For example:

    tar -xzf filepath-1.1.0.0.tar.gz
    tar -xzf Cabal-1.6.0.0.tar.gz

    # rename to make the following instructions simpler:
    mv filepath-1.1.0.0/ filepath/
    mv Cabal-1.6.0.0/ Cabal/

    cd Cabal
    ghc -i../filepath -cpp --make Setup.hs -o ../filepath/setup
    cd ../filepath/
    ./setup configure --user
    ./setup build
    ./setup install

This installs `filepath` so that you can install Cabal with the normal
method.

[filepath]: http://hackage.haskell.org/package/filepath

More information
================

Please see the [Cabal web site] for the [user guide] and [API
documentation]. There is additional information available on the
[development wiki].

[user guide]:        http://www.haskell.org/cabal/users-guide
[API documentation]: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html
[development wiki]:  https://github.com/haskell/cabal/wiki


Bugs
====

Please report bugs and feature requests to Cabal's [bug tracker].


Your help
---------

To help Cabal's development, it is enormously helpful to know from
Cabal's users what their most pressing problems are with Cabal and
[Hackage]. You may have a favourite Cabal bug or limitation. Look at
Cabal's [bug tracker]. Ensure that the problem is reported there and
adequately described. Comment on the issue to report how much of a
problem the bug is for you. Subscribe to the issues's notifications to
discussed requirements and keep informed on progress. For feature
requests, it is helpful if there is a description of how you would
expect to interact with the new feature.

[Hackage]: http://hackage.haskell.org


Source code
===========

You can get the master development branch using:

    $ git clone https://github.com/haskell/cabal.git


Credits
=======

Cabal developers (in alphabetical order):

- Krasimir Angelov
- Bjorn Bringert
- Duncan Coutts
- Isaac Jones
- David Himmelstrup ("Lemmih")
- Simon Marlow
- Ross Patterson
- Thomas Schilling
- Martin Sjögren
- Malcolm Wallace
- and nearly 30 other people have contributed occasional patches

Cabal specification authors:

- Isaac Jones
- Simon Marlow
- Ross Patterson
- Simon Peyton Jones
- Malcolm Wallace


[bug tracker]: https://github.com/haskell/cabal/issues
[Cabal web site]: http://www.haskell.org/cabal/
