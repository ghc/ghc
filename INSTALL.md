Building & Installing
=====================

For full information on building GHC, see the GHC Building Guide [1].
Here follows a summary - if you get into trouble, the Building Guide
has all the answers.

Before building GHC you may need to install some other tools and
libraries.  See "Setting up your system for building GHC" [2].

N.B. in particular you need GHC installed in order to build GHC,
because the compiler is itself written in Haskell.  For instructions
on how to port GHC to a new platform, see the Building Guide [1].

For building library documentation, you'll need Haddock [3].  To build
the compiler documentation, you need [Sphinx](http://www.sphinx-doc.org/) and
XeLaTex (only for PDF output).

Quick start:  the following gives you a default build:

    $ python3 boot
    $ ./configure
    $ make
    $ make install

  On Windows, you need an extra repository containing some build tools.
  These can be downloaded for you by configure. This only needs to be done once by running:

    $ ./configure --enable-tarballs-autodownload

You can use Make's `-jN` option to parallelize the build. It's generally best
to set `N` somewhere around the core count of the build machine.

The `python3 boot` step is only necessary if this is a tree checked out from
git. For source distributions downloaded from GHC's web site, this step has
already been performed.

These steps give you the default build, which includes everything
optimised and built in various ways (eg. profiling libs are built).
It can take a long time.  To customise the build, see the file
`HACKING.md`.

References
==========

 [1] http://www.haskell.org/ghc/
 [2] http://hackage.haskell.org/trac/ghc/wiki/Building/Preparation
 [3] http://www.haskell.org/haddock/
