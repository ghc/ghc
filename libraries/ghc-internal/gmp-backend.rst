GMP
===

ghc-internal's GMP backend depends on the external GMP library (gmplib.org). The
latter provides a header ("gmp.h") and a library to link with.

Linking
-------

Sadly we can't just put a ``extra-libraries: gmp`` field in the Cabal file because
``ghc-internal`` is a boot package that is part of GHC's *binary* distribution.
It means that it won't be rebuilt on each user platform. In particular it can be
used in an environment that doesn't provide GMP.

A solution would be to always link GMP statically with ``ghc-internal``, but:

1. GMP's license is LPGL while GHC's license is BSD

2. Cabal doesn't provide an easy way to build a Haskell library statically
   linked with an external library.
   See https://github.com/haskell/cabal/issues/4042

So, we support the following configurations:

* Dynamically linked GMP
   * Found in usual library paths
   * Found in a specified library path
* Statically linked GMP ("in-tree GMP")
   * Built by GHC's build system

As Cabal can't statically link an external library with a Haskell library,
GHC's build system uses a hack:
   1. it builds libgmp.a
   2. it extracts the objects (.o) from it
   3. it passes these objects as "extra" objects when it links ghc-internal

Note that these objects must be built as position independent code (PIC) because
they end up being used in statically and dynamically linked code (cf #17799).

Configuration
-------------

GHC's build system provides a ``configure`` script that can be used to setup how
GMP is linked:

.. code::

  --with-gmp                     enable GMP backend
  --with-gmp-includes            directory containing gmp.h
  --with-gmp-libraries           directory containing gmp library
  --with-intree-gmp              force using the in-tree GMP
  --with-gmp-framework-preferred on OSX, prefer the GMP framework to the gmp lib

These options are then used when ghc-internal package is configured: in the
.cabal file, we can see the field ``build-type: Configure``, meaning that the
``configure`` script in ``libraries/ghc-internal/`` is executed during the setup
phase of the package.

This script is responsible of creating ``ghc-internal.buildinfo`` (from
``ghc-internal.buildinfo.in``). The fields contained in this file are
merged with the ones already defined in ``ghc-internal.cabal``.

See
https://www.haskell.org/cabal/users-guide/developing-packages.html#system-dependent-parameters.

Headers
-------

When GMP is statically linked (in-tree build), a user of the ghc-internal package
can't have access to the "gmp.h" header file. So GHC's build system copies the
``ghc.h`` header from the in-tree build to ``ghc-internal/include/ghc-gmp.h``. As you
can see in ``ghc-internal.buildinfo[.in]``, ``ghc-gmp.h`` is installed as a
header (``install-includes`` field).

While the commit that introduced it (a9a0dd34dcdfb7309f57bda88435acca14ec54d5)
doesn't document it, it's probably to get access to other GMP functions.

Note that when in-tree GMP build isn't used, ``ghc-gmp.h`` only contains
``#include <gmp.h>``. Hence it imports the header from the HOST platform, which
may not be exactly the same as the one used on the BUILD platform to build the
ghc-internal package.
