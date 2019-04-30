Quick `make` guide for GHC
==========================

For a "Getting Started" guide, see:

  - https://gitlab.haskell.org/ghc/ghc/wikis/building/quick-start
  - https://gitlab.haskell.org/ghc/ghc/wikis/building/using
  - https://gitlab.haskell.org/ghc/ghc/wikis/building/standard-targets

Common commands:

  - `make`

    Builds everything: ghc stages 1 and 2, all libraries and tools.

  - `make -j2`

    Parallel build: runs up to 2 commands at a time.

  - `cd <dir>; make`

    Builds everything in the given directory.

  - `cd <dir>; make help`

    Shows the targets available in <dir>

  - `make install`
  - `make install-strip`

    Installs GHC, libraries and tools under $(prefix). The install-strip
    variant strips executable files while installing them.

  - `make sdist`
  - `make binary-dist`

    Builds a source or binary distribution respectively

  - `make show VALUE=<var>`
  - `make show! VALUE=<var>`

    Show the value of make variable <var>. The show! variant works right after
    ./configure (it skips reading package-data.mk files).

  - `make clean`
  - `make distclean`
  - `make maintainer-clean`

    Various levels of cleaning: "clean" restores the tree to the
    state after "./configure", "distclean" restores to the state
    after "python3 boot", and maintainer-clean restores the tree to the
    completely clean checked-out state.

Using `make` in subdirectories
==============================

  - `make`

    Builds everything in this directory (including dependencies elsewhere
    in the tree, if necessary)

  - `make fast`

    The same as 'make', but omits some phases and does not
    recalculate dependencies.  Useful for saving time if you are sure
    the rest of the tree is up to date.

  - `make clean`
  - `make distclean`
  - `make maintainer-clean`

    Clean just this directory

  - `make html`
  - `make pdf`
  - `make ps`

    Make documentation in this directory (if any)

  - `make show VALUE=<var>`
  - `make show! VALUE=<var>`

    Show the value of make variable <var>. The show! variant works right after
    ./configure (it skips reading package-data.mk files).

  - `make <file>`

    Bring a particular file up to date, e.g. make dist/build/Module.o
    The name <file> is relative to the current directory

Useful links:
=============

See also "new Hadrian build system":

  - https://gitlab.haskell.org/ghc/ghc/wikis/building/hadrian
