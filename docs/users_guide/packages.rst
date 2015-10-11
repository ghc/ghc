.. _packages:

Packages
========

.. index::
   single: packages

A package is a library of Haskell modules known to the compiler. GHC
comes with several packages: see the accompanying `library
documentation <../libraries/index.html>`__. More packages to install can
be obtained from
`HackageDB <http://hackage.haskell.org/packages/hackage.html>`__.

Using a package couldn't be simpler: if you're using ``--make`` or GHCi,
then most of the installed packages will be automatically available to
your program without any further options. The exceptions to this rule
are covered below in :ref:`using-packages`.

Building your own packages is also quite straightforward: we provide the
`Cabal <http://www.haskell.org/cabal/>`__ infrastructure which automates
the process of configuring, building, installing and distributing a
package. All you need to do is write a simple configuration file, put a
few files in the right places, and you have a package. See the `Cabal
documentation <http://www.haskell.org/cabal/users-guide/>`__ for
details, and also the Cabal libraries
(:cabal-ref:`Distribution.Simple <Distribution-Simple.html>`,
for example).

.. _using-packages:

Using Packages
--------------

.. index::
   single: packages; using

GHC only knows about packages that are *installed*. To see which
packages are installed, use the ``ghc-pkg list`` command:

::

    $ ghc-pkg list
    /usr/lib/ghc-6.12.1/package.conf.d:
        Cabal-1.7.4
        array-0.2.0.1
        base-3.0.3.0
        base-4.2.0.0
        bin-package-db-0.0.0.0
        binary-0.5.0.1
        bytestring-0.9.1.4
        containers-0.2.0.1
        directory-1.0.0.2
        (dph-base-0.4.0)
        (dph-par-0.4.0)
        (dph-prim-interface-0.4.0)
        (dph-prim-par-0.4.0)
        (dph-prim-seq-0.4.0)
        (dph-seq-0.4.0)
        extensible-exceptions-0.1.1.0
        ffi-1.0
        filepath-1.1.0.1
        (ghc-6.12.1)
        ghc-prim-0.1.0.0
        haskeline-0.6.2
        haskell98-1.0.1.0
        hpc-0.5.0.2
        integer-gmp-0.1.0.0
        mtl-1.1.0.2
        old-locale-1.0.0.1
        old-time-1.0.0.1
        pretty-1.0.1.0
        process-1.0.1.1
        random-1.0.0.1
        rts-1.0
        syb-0.1.0.0
        template-haskell-2.4.0.0
        terminfo-0.3.1
        time-1.1.4
        unix-2.3.1.0
        utf8-string-0.3.4

An installed package is either *exposed* or *hidden* by default.
Packages hidden by default are listed in parentheses (e.g.
``(lang-1.0)``), or possibly in blue if your terminal supports colour,
in the output of ``ghc-pkg list``. Command-line flags, described below,
allow you to expose a hidden package or hide an exposed one. Only
modules from exposed packages may be imported by your Haskell code; if
you try to import a module from a hidden package, GHC will emit an error
message. If there are a multiple exposed versions of a package, GHC will
prefer the latest one. Additionally, some packages may be broken: that
is, they are missing from the package database, or one of their
dependencies are broken; in this case; these packages are excluded from
the default set of packages.

.. note::
    If you're using Cabal, then the exposed or hidden status of a
    package is irrelevant: the available packages are instead determined by
    the dependencies listed in your ``.cabal`` specification. The
    exposed/hidden status of packages is only relevant when using ``ghc`` or
    ``ghci`` directly.

Similar to a package's hidden status is a package's trusted status. A
package can be either trusted or not trusted (distrusted). By default
packages are distrusted. This property of a package only plays a role
when compiling code using GHC's Safe Haskell feature (see
:ref:`safe-haskell`) with the ``-fpackage-trust`` flag enabled.

To see which modules are provided by a package use the ``ghc-pkg``
command (see :ref:`package-management`):

::

    $ ghc-pkg field network exposed-modules
    exposed-modules: Network.BSD,
                     Network.CGI,
                     Network.Socket,
                     Network.URI,
                     Network

The GHC command line options that control packages are:

``-package ⟨pkg⟩``
    .. index::
       single: -package

    This option causes the installed package ⟨pkg⟩ to be exposed. The
    package ⟨pkg⟩ can be specified in full with its version number (e.g.
    ``network-1.0``) or the version number can be omitted if there is
    only one version of the package installed. If there are multiple
    versions of ⟨pkg⟩ installed and ``-hide-all-packages`` was not
    specified, then all other versions will become hidden. ``-package``
    supports thinning and renaming described in
    :ref:`package-thinning-and-renaming`.

    The ``-package ⟨pkg⟩`` option also causes package ⟨pkg⟩ to be linked into
    the resulting executable or shared object. Whether a packages'
    library is linked statically or dynamically is controlled by the
    flag pair ``-static``/``-dynamic``.

    In ``--make`` mode and ``--interactive`` mode (see :ref:`modes`),
    the compiler normally determines which packages are required by the
    current Haskell modules, and links only those. In batch mode
    however, the dependency information isn't available, and explicit
    ``-package`` options must be given when linking. The one other time
    you might need to use ``-package`` to force linking a package is
    when the package does not contain any Haskell modules (it might
    contain a C library only, for example). In that case, GHC will never
    discover a dependency on it, so it has to be mentioned explicitly.

    For example, to link a program consisting of objects ``Foo.o`` and
    ``Main.o``, where we made use of the ``network`` package, we need to
    give GHC the ``-package`` flag thus:

    ::

        $ ghc -o myprog Foo.o Main.o -package network

    The same flag is necessary even if we compiled the modules from
    source, because GHC still reckons it's in batch mode:

    ::

        $ ghc -o myprog Foo.hs Main.hs -package network

``-package-id ⟨pkg-id⟩``
    .. index::
       single: -package-id

    Exposes a package like ``-package``, but the package is named by its
    installed package ID rather than by name. This is a more robust way
    to name packages, and can be used to select packages that would
    otherwise be shadowed. Cabal passes ``-package-id`` flags to GHC.
    ``-package-id`` supports thinning and renaming described in
    :ref:`package-thinning-and-renaming`.

``-hide-all-packages``
    .. index::
       single: -hide-package

    Ignore the exposed flag on installed packages, and hide them all by
    default. If you use this flag, then any packages you require
    (including ``base``) need to be explicitly exposed using
    ``-package`` options.

    This is a good way to insulate your program from differences in the
    globally exposed packages, and being explicit about package
    dependencies is a Good Thing. Cabal always passes the
    ``-hide-all-packages`` flag to GHC, for exactly this reason.

``-hide-package ⟨pkg⟩``
    .. index::
       single: -hide-package

    This option does the opposite of ``-package``: it causes the
    specified package to be hidden, which means that none of its modules
    will be available for import by Haskell ``import`` directives.

    Note that the package might still end up being linked into the final
    program, if it is a dependency (direct or indirect) of another
    exposed package.

``-ignore-package ⟨pkg⟩``
    .. index::
       single: -ignore-package

    Causes the compiler to behave as if package ⟨pkg⟩, and any packages
    that depend on ⟨pkg⟩, are not installed at all.

    Saying ``-ignore-package ⟨pkg⟩`` is the same as giving ``-hide-package``
    flags for ⟨pkg⟩ and all the packages that depend on ⟨pkg⟩. Sometimes
    we don't know ahead of time which packages will be installed that
    depend on ⟨pkg⟩, which is when the ``-ignore-package`` flag can be
    useful.

``-no-auto-link-packages``
    .. index::
       single: -no-auto-link-packages

    By default, GHC will automatically link in the ``base`` and ``rts``
    packages. This flag disables that behaviour.

``-this-package-key ⟨pkg-key⟩``
    .. index::
       single: -this-package-key

    Tells GHC the the module being compiled forms part of unit ID
    ⟨pkg-key⟩; internally, these keys are used to determine type equality
    and linker symbols.

``-library-name ⟨hash⟩``
    .. index::
       single: -library-name

    Tells GHC that the source of a Backpack file and its textual
    dependencies is uniquely identified by ⟨hash⟩. Library names are
    determined by Cabal; a usual recipe for a library name is that it is
    the hash source package identifier of a package, as well as the
    version hashes of all its textual dependencies. GHC will then use
    this library name to generate more unit IDs.

``-trust ⟨pkg⟩``
    .. index::
       single: -trust

    This option causes the install package ⟨pkg⟩ to be both exposed and
    trusted by GHC. This command functions in the in a very similar way
    to the ``-package`` command but in addition sets the selected
    packaged to be trusted by GHC, regardless of the contents of the
    package database. (see :ref:`safe-haskell`).

``-distrust ⟨pkg⟩``
    .. index::
       single: -distrust

    This option causes the install package ⟨pkg⟩ to be both exposed and
    distrusted by GHC. This command functions in the in a very similar
    way to the ``-package`` command but in addition sets the selected
    packaged to be distrusted by GHC, regardless of the contents of the
    package database. (see :ref:`safe-haskell`).

``-distrust-all``
    .. index::
       single: -distrust-all

    Ignore the trusted flag on installed packages, and distrust them by
    default. If you use this flag and Safe Haskell then any packages you
    require to be trusted (including ``base``) need to be explicitly
    trusted using ``-trust`` options. This option does not change the
    exposed/hidden status of a package, so it isn't equivalent to
    applying ``-distrust`` to all packages on the system. (see
    :ref:`safe-haskell`).

.. _package-main:

The ``main`` package
--------------------

Every complete Haskell program must define ``main`` in module ``Main``
in package ``main``. Omitting the ``-this-package-key`` flag compiles
code for package ``main``. Failure to do so leads to a somewhat obscure
link-time error of the form:

::

    /usr/bin/ld: Undefined symbols:
    _ZCMain_main_closure

.. _package-overlaps:

Consequences of packages for the Haskell language
-------------------------------------------------

It is possible that by using packages you might end up with a program
that contains two modules with the same name: perhaps you used a package
``P`` that has a *hidden* module ``M``, and there is also a module ``M`` in your
program. Or perhaps the dependencies of packages that you used contain
some overlapping modules. Perhaps the program even contains multiple
versions of a certain package, due to dependencies from other packages.

None of these scenarios gives rise to an error on its own [1]_, but they
may have some interesting consequences. For instance, if you have a type
``M.T`` from version 1 of package ``P``, then this is *not* the same as
the type ``M.T`` from version 2 of package ``P``, and GHC will report an
error if you try to use one where the other is expected.

Formally speaking, in Haskell 98, an entity (function, type or class) in
a program is uniquely identified by the pair of the module name in which
it is defined and its name. In GHC, an entity is uniquely defined by a
triple: package, module, and name.

.. _package-thinning-and-renaming:

Thinning and renaming modules
-----------------------------

When incorporating packages from multiple sources, you may end up in a
situation where multiple packages publish modules with the same name.
Previously, the only way to distinguish between these modules was to use
:ref:`package-qualified-imports`. However, since GHC 7.10, the
``-package`` flags (and their variants) have been extended to allow a
user to explicitly control what modules a package brings into scope, by
analogy to the import lists that users can attach to module imports.

The basic syntax is that instead of specifying a package name P to the
package flag ``-package``, instead we specify both a package name and a
parenthesized, comma-separated list of module names to import. For
example, ``-package "base (Data.List, Data.Bool)"`` makes only
``Data.List`` and ``Data.Bool`` visible from package ``base``. We also
support renaming of modules, in case you need to refer to both modules
simultaneously; this is supporting by writing
``OldModName as NewModName``, e.g.
``-package "base (Data.Bool as Bool)``. You can also write
``-package "base with (Data.Bool as Bool)`` to include all of the
original bindings (e.g. the renaming is strictly additive). It's
important to specify quotes so that your shell passes the package name
and thinning/renaming list as a single argument to GHC.

Package imports with thinning/renaming do not hide other versions of the
package: e.g. if containers-0.9 is already exposed,
``-package "containers-0.8 (Data.List as ListV8)"`` will only add an
additional binding to the environment. Similarly,
``-package "base (Data.Bool as Bool)" -package "base (Data.List as List)"``
is equivalent to
``-package "base (Data.Bool as Bool, Data.List as List)"``. Literal
names must refer to modules defined by the original package, so for
example ``-package "base (Data.Bool as Bool, Bool as Baz)"`` is invalid
unless there was a ``Bool`` module defined in the original package.
Hiding a package also clears all of its renamings.

You can use renaming to provide an alternate prelude, e.g.
``-hide-all-packages -package "basic-prelude (BasicPrelude as Prelude)"``,
in lieu of the :ref:`rebindable-syntax` extension.

.. _package-databases:

Package Databases
-----------------

A package database is where the details about installed packages are
stored. It is a directory, usually called ``package.conf.d``, that
contains a file for each package, together with a binary cache of the
package data in the file ``package.cache``. Normally you won't need to
look at or modify the contents of a package database directly; all
management of package databases can be done through the ``ghc-pkg`` tool
(see :ref:`package-management`).

GHC knows about two package databases in particular:

-  The global package database, which comes with your GHC installation,
   e.g. ``/usr/lib/ghc-6.12.1/package.conf.d``.

-  A package database private to each user. On Unix systems this will be
   ``$HOME/.ghc/arch-os-version/package.conf.d``, and on Windows it will
   be something like
   ``C:\Documents And Settings\user\ghc\package.conf.d``. The
   ``ghc-pkg`` tool knows where this file should be located, and will
   create it if it doesn't exist (see :ref:`package-management`).

When GHC starts up, it reads the contents of these two package
databases, and builds up a list of the packages it knows about. You can
see GHC's package table by running GHC with the ``-v`` flag.

Package databases may overlap, and they are arranged in a stack
structure. Packages closer to the top of the stack will override
(*shadow*) those below them. By default, the stack contains just the
global and the user's package databases, in that order.

You can control GHC's package database stack using the following
options:

``-package-db ⟨file⟩``
    .. index::
       single: -package-db

    Add the package database ⟨file⟩ on top of the current stack.
    Packages in additional databases read this way will override those
    in the initial stack and those in previously specified databases.

``-no-global-package-db``
    .. index::
       single: -no-global-package-db

    Remove the global package database from the package database stack.

``-no-user-package-db``
    .. index::
       single: -no-user-package-db

    Prevent loading of the user's local package database in the initial
    stack.

``-clear-package-db``
    .. index::
       single: -clear-package-db

    Reset the current package database stack. This option removes every
    previously specified package database (including those read from the
    ``GHC_PACKAGE_PATH`` environment variable) from the package database
    stack.

``-global-package-db``
    .. index::
       single: -global-package-db

    Add the global package database on top of the current stack. This
    option can be used after ``-no-global-package-db`` to specify the
    position in the stack where the global package database should be
    loaded.

``-user-package-db``
    .. index::
       single: -user-package-db

    Add the user's package database on top of the current stack. This
    option can be used after ``-no-user-package-db`` to specify the
    position in the stack where the user's package database should be
    loaded.

.. _ghc-package-path:

The ``GHC_PACKAGE_PATH`` environment variable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: Environment variable; ``GHC_PACKAGE_PATH``
   single: GHC_PACKAGE_PATH

The ``GHC_PACKAGE_PATH`` environment variable may be set to a
``:``\-separated (``;``\-separated on Windows) list of files containing
package databases. This list of package databases is used by GHC and
ghc-pkg, with earlier databases in the list overriding later ones. This
order was chosen to match the behaviour of the ``PATH`` environment
variable; think of it as a list of package databases that are searched
left-to-right for packages.

If ``GHC_PACKAGE_PATH`` ends in a separator, then the default package
database stack (i.e. the user and global package databases, in that
order) is appended. For example, to augment the usual set of packages
with a database of your own, you could say (on Unix):

::

     $ export GHC_PACKAGE_PATH=$HOME/.my-ghc-packages.conf:

(use ``;`` instead of ``:`` on Windows).

To check whether your ``GHC_PACKAGE_PATH`` setting is doing the right
thing, ``ghc-pkg list`` will list all the databases in use, in the
reverse order they are searched.

.. _package-ids:

Installed package IDs, dependencies, and broken packages
--------------------------------------------------------

Each installed package has a unique identifier (the "installed package
ID"), which distinguishes it from all other installed packages on the
system. To see the installed package IDs associated with each installed
package, use ``ghc-pkg list -v``:

::

    $ ghc-pkg list -v
    using cache: /usr/lib/ghc-6.12.1/package.conf.d/package.cache
    /usr/lib/ghc-6.12.1/package.conf.d
       Cabal-1.7.4 (Cabal-1.7.4-48f5247e06853af93593883240e11238)
       array-0.2.0.1 (array-0.2.0.1-9cbf76a576b6ee9c1f880cf171a0928d)
       base-3.0.3.0 (base-3.0.3.0-6cbb157b9ae852096266e113b8fac4a2)
       base-4.2.0.0 (base-4.2.0.0-247bb20cde37c3ef4093ee124e04bc1c)
       ...

The string in parentheses after the package name is the installed
package ID: it normally begins with the package name and version, and
ends in a hash string derived from the compiled package. Dependencies
between packages are expressed in terms of installed package IDs, rather
than just packages and versions. For example, take a look at the
dependencies of the ``haskell98`` package:

::

    $ ghc-pkg field haskell98 depends
    depends: array-0.2.0.1-9cbf76a576b6ee9c1f880cf171a0928d
             base-4.2.0.0-247bb20cde37c3ef4093ee124e04bc1c
             directory-1.0.0.2-f51711bc872c35ce4a453aa19c799008
             old-locale-1.0.0.1-d17c9777c8ee53a0d459734e27f2b8e9
             old-time-1.0.0.1-1c0d8ea38056e5087ef1e75cb0d139d1
             process-1.0.1.1-d8fc6d3baf44678a29b9d59ca0ad5780
             random-1.0.0.1-423d08c90f004795fd10e60384ce6561

The purpose of the installed package ID is to detect problems caused by
re-installing a package without also recompiling the packages that
depend on it. Recompiling dependencies is necessary, because the newly
compiled package may have a different ABI (Application Binary Interface)
than the previous version, even if both packages were built from the
same source code using the same compiler. With installed package IDs, a
recompiled package will have a different installed package ID from the
previous version, so packages that depended on the previous version are
now orphaned - one of their dependencies is not satisfied. Packages that
are broken in this way are shown in the ``ghc-pkg list`` output either
in red (if possible) or otherwise surrounded by braces. In the following
example, we have recompiled and reinstalled the ``filepath`` package,
and this has caused various dependencies including ``Cabal`` to break:

::

    $ ghc-pkg list
    WARNING: there are broken packages.  Run 'ghc-pkg check' for more details.
    /usr/lib/ghc-6.12.1/package.conf.d:
        {Cabal-1.7.4}
        array-0.2.0.1
        base-3.0.3.0
        ... etc ...

Additionally, ``ghc-pkg list`` reminds you that there are broken
packages and suggests ``ghc-pkg check``, which displays more information
about the nature of the failure:

::

    $ ghc-pkg check
    There are problems in package ghc-6.12.1:
      dependency "filepath-1.1.0.1-87511764eb0af2bce4db05e702750e63" doesn't exist
    There are problems in package haskeline-0.6.2:
      dependency "filepath-1.1.0.1-87511764eb0af2bce4db05e702750e63" doesn't exist
    There are problems in package Cabal-1.7.4:
      dependency "filepath-1.1.0.1-87511764eb0af2bce4db05e702750e63" doesn't exist
    There are problems in package process-1.0.1.1:
      dependency "filepath-1.1.0.1-87511764eb0af2bce4db05e702750e63" doesn't exist
    There are problems in package directory-1.0.0.2:
      dependency "filepath-1.1.0.1-87511764eb0af2bce4db05e702750e63" doesn't exist

    The following packages are broken, either because they have a problem
    listed above, or because they depend on a broken package.
    ghc-6.12.1
    haskeline-0.6.2
    Cabal-1.7.4
    process-1.0.1.1
    directory-1.0.0.2
    bin-package-db-0.0.0.0
    hpc-0.5.0.2
    haskell98-1.0.1.0

To fix the problem, you need to recompile the broken packages against
the new dependencies. The easiest way to do this is to use
``cabal-install``, or download the packages from
`HackageDB <http://hackage.haskell.org/packages/hackage.html>`__ and
build and install them as normal.

Be careful not to recompile any packages that GHC itself depends on, as
this may render the ``ghc`` package itself broken, and ``ghc`` cannot be
simply recompiled. The only way to recover from this would be to
re-install GHC.

.. _package-management:

Package management (the ``ghc-pkg`` command)
--------------------------------------------

.. index::
   single: packages; management

The ``ghc-pkg`` tool is for querying and modifying package databases. To
see what package databases are in use, use ``ghc-pkg list``. The stack
of databases that ``ghc-pkg`` knows about can be modified using the
``GHC_PACKAGE_PATH`` environment variable (see :ref:`ghc-package-path`,
and using ``--package-db`` options on the ``ghc-pkg`` command line.

When asked to modify a database, ``ghc-pkg`` modifies the global
database by default. Specifying ``--user`` causes it to act on the user
database, or ``--package-db`` can be used to act on another database
entirely. When multiple of these options are given, the rightmost one is
used as the database to act upon.

Commands that query the package database (list, latest, describe, field,
dot) operate on the list of databases specified by the flags ``--user``,
``--global``, and ``--package-db``. If none of these flags are given,
the default is ``--global --user``.

If the environment variable ``GHC_PACKAGE_PATH`` is set, and its value
does not end in a separator (``:`` on Unix, ``;`` on Windows), then the
last database is considered to be the global database, and will be
modified by default by ``ghc-pkg``. The intention here is that
``GHC_PACKAGE_PATH`` can be used to create a virtual package environment
into which Cabal packages can be installed without setting anything
other than ``GHC_PACKAGE_PATH``.

The ``ghc-pkg`` program may be run in the ways listed below. Where a
package name is required, the package can be named in full including the
version number (e.g. ``network-1.0``), or without the version number.
Naming a package without the version number matches all versions of the
package; the specified action will be applied to all the matching
packages. A package specifier that matches all version of the package
can also be written ``⟨pkg⟩ -*``, to make it clearer that multiple
packages are being matched. To match against the installed package ID
instead of just package name and version, pass the ``--ipid`` flag.

``ghc-pkg init path``
    Creates a new, empty, package database at ⟨path⟩, which must not
    already exist.

``ghc-pkg register ⟨file⟩``
    Reads a package specification from ⟨file⟩ (which may be "``-``"
    to indicate standard input), and adds it to the database of
    installed packages. The syntax of ⟨file⟩ is given in
    :ref:`installed-pkg-info`.

    The package specification must be a package that isn't already
    installed.

``ghc-pkg update ⟨file⟩``
    The same as ``register``, except that if a package of the same name
    is already installed, it is replaced by the new one.

``ghc-pkg unregister ⟨P⟩``
    Remove the specified package from the database.

``ghc-pkg check``
    Check consistency of dependencies in the package database, and
    report packages that have missing dependencies.

``ghc-pkg expose ⟨P⟩``
    Sets the ``exposed`` flag for package ⟨P⟩ to ``True``.

``ghc-pkg hide ⟨P⟩``
    Sets the ``exposed`` flag for package ⟨P⟩ to ``False``.

``ghc-pkg trust ⟨P⟩``
    Sets the ``trusted`` flag for package ⟨P⟩ to ``True``.

``ghc-pkg distrust ⟨P⟩``
    Sets the ``trusted`` flag for package ⟨P⟩ to ``False``.

``ghc-pkg list [⟨P⟩] [--simple-output]``
    This option displays the currently installed packages, for each of
    the databases known to ``ghc-pkg``. That includes the global
    database, the user's local database, and any further files specified
    using the ``-f`` option on the command line.

    Hidden packages (those for which the ``exposed`` flag is ``False``)
    are shown in parentheses in the list of packages.

    If an optional package identifier ⟨P⟩ is given, then only packages
    matching that identifier are shown.

    If the option ``--simple-output`` is given, then the packages are
    listed on a single line separated by spaces, and the database names
    are not included. This is intended to make it easier to parse the
    output of ``ghc-pkg list`` using a script.

``ghc-pkg find-module ⟨M⟩ [--simple-output]``
    This option lists registered packages exposing module ⟨M⟩. Examples:

    ::

        $ ghc-pkg find-module Var
        c:/fptools/validate/ghc/driver/package.conf.inplace:
            (ghc-6.9.20080428)

        $ ghc-pkg find-module Data.Sequence
        c:/fptools/validate/ghc/driver/package.conf.inplace:
            containers-0.1

    Otherwise, it behaves like ``ghc-pkg list``, including options.

``ghc-pkg latest ⟨P⟩``
    Prints the latest available version of package ⟨P⟩.

``ghc-pkg describe ⟨P⟩``
    Emit the full description of the specified package. The description
    is in the form of an ``InstalledPackageInfo``, the same as the input
    file format for ``ghc-pkg register``. See :ref:`installed-pkg-info`
    for details.

    If the pattern matches multiple packages, the description for each
    package is emitted, separated by the string ``---`` on a line by
    itself.

``ghc-pkg field ⟨P⟩ ⟨field⟩[,⟨field⟩]*``
    Show just a single field of the installed package description for
    ``P``. Multiple fields can be selected by separating them with
    commas

``ghc-pkg dot``
    Generate a graph of the package dependencies in a form suitable for
    input for the `graphviz <http://www.graphviz.org/>`__ tools. For
    example, to generate a PDF of the dependency graph:

    ::

        ghc-pkg dot | tred | dot -Tpdf >pkgs.pdf

``ghc-pkg dump``
    Emit the full description of every package, in the form of an
    ``InstalledPackageInfo``. Multiple package descriptions are
    separated by the string ``---`` on a line by itself.

    This is almost the same as ``ghc-pkg describe '*'``, except that
    ``ghc-pkg dump`` is intended for use by tools that parse the
    results, so for example where ``ghc-pkg describe '*'`` will emit an
    error if it can't find any packages that match the pattern,
    ``ghc-pkg dump`` will simply emit nothing.

``ghc-pkg recache``
    Re-creates the binary cache file ``package.cache`` for the selected
    database. This may be necessary if the cache has somehow become
    out-of-sync with the contents of the database (``ghc-pkg`` will warn
    you if this might be the case).

    The other time when ``ghc-pkg recache`` is useful is for registering
    packages manually: it is possible to register a package by simply
    putting the appropriate file in the package database directory and
    invoking ``ghc-pkg recache`` to update the cache. This method of
    registering packages may be more convenient for automated packaging
    systems.

Substring matching is supported for ⟨M⟩ in ``find-module`` and for ⟨P⟩
in ``list``, ``describe``, and ``field``, where a ``'*'`` indicates open
substring ends (``prefix*``, ``*suffix``, ``*infix*``). Examples (output
omitted):

::

    -- list all regex-related packages
    ghc-pkg list '*regex*' --ignore-case
    -- list all string-related packages
    ghc-pkg list '*string*' --ignore-case
    -- list OpenGL-related packages
    ghc-pkg list '*gl*' --ignore-case
    -- list packages exporting modules in the Data hierarchy
    ghc-pkg find-module 'Data.*'
    -- list packages exporting Monad modules
    ghc-pkg find-module '*Monad*'
    -- list names and maintainers for all packages
    ghc-pkg field '*' name,maintainer
    -- list location of haddock htmls for all packages
    ghc-pkg field '*' haddock-html
    -- dump the whole database
    ghc-pkg describe '*'

Additionally, the following flags are accepted by ``ghc-pkg``:

``-f ⟨file⟩``, ``-package-db ⟨file⟩``
    .. index::
       single: -f; ghc-pkg option
       single: -package-db; ghc-pkg option

    Adds ⟨file⟩ to the stack of package databases. Additionally, ⟨file⟩
    will also be the database modified by a ``register``,
    ``unregister``, ``expose`` or ``hide`` command, unless it is
    overridden by a later ``--package-db``, ``--user`` or ``--global``
    option.

``--force``
    .. index::
       single: --force; ghc-pkg option

    Causes ``ghc-pkg`` to ignore missing dependencies, directories and
    libraries when registering a package, and just go ahead and add it
    anyway. This might be useful if your package installation system
    needs to add the package to GHC before building and installing the
    files.

``--global``
    .. index::
       single: --global; ghc-pkg option

    Operate on the global package database (this is the default). This
    flag affects the ``register``, ``update``, ``unregister``,
    ``expose``, and ``hide`` commands.

``--help``, ``-?``
    .. index::
       single: --help; ghc-pkg option
       single: -?; ghc-pkg option

    Outputs the command-line syntax.

``--user``
    .. index::
       single: --user; ghc-pkg option

    Operate on the current user's local package database. This flag
    affects the ``register``, ``update``, ``unregister``, ``expose``,
    and ``hide`` commands.

``-v [⟨n⟩]``, ``--verbose [=⟨n⟩]``
    .. index::
       single: -v; ghc-pkg option
       single: --verbose; ghc-pkg option

    Control verbosity. Verbosity levels range from 0-2, where the
    default is 1, and ``-v`` alone selects level 2.

``-V``; \ ``--version``
    .. index::
       single: -V; ghc-pkg option
       single: --version; ghc-pkg option

    Output the ``ghc-pkg`` version number.

``--ipid``
    .. index::
       single: --ipid; ghc-pkg option

    Causes ``ghc-pkg`` to interpret arguments as installed package IDs
    (e.g., an identifier like
    ``unix-2.3.1.0-de7803f1a8cd88d2161b29b083c94240``). This is useful
    if providing just the package name and version are ambiguous (in old
    versions of GHC, this was guaranteed to be unique, but this
    invariant no longer necessarily holds).

``--package-key``
    .. index::
       single: --package-key; ghc-pkg option

    Causes ``ghc-pkg`` to interpret arguments as unit IDs (e.g., an
    identifier like ``I5BErHzyOm07EBNpKBEeUv``). Package keys are used
    to prefix symbol names GHC produces (e.g.,
    ``6VWy06pWzzJq9evDvK2d4w6_DataziByteStringziInternal_unsafePackLenChars_info``),
    so if you need to figure out what package a symbol belongs to, use
    ``ghc-pkg`` with this flag.

.. _building-packages:

Building a package from Haskell source
--------------------------------------

.. index::
   single: packages; building

We don't recommend building packages the hard way. Instead, use the
`Cabal <http://www.haskell.org/cabal/users-guide/>`__ infrastructure if
possible. If your package is particularly complicated or requires a lot
of configuration, then you might have to fall back to the low-level
mechanisms, so a few hints for those brave souls follow.

You need to build an "installed package info" file for passing to
``ghc-pkg`` when installing your package. The contents of this file are
described in :ref:`installed-pkg-info`.

The Haskell code in a package may be built into one or more archive
libraries (e.g. ``libHSfoo.a``), or a single shared object (e.g.
``libHSfoo.dll/.so/.dylib``). The restriction to a single shared object
is because the package system is used to tell the compiler when it
should make an inter-shared-object call rather than an
intra-shared-object-call call (inter-shared-object calls require an
extra indirection).

-  Building a static library is done by using the ``ar`` tool, like so:

   ::

       ar cqs libHSfoo-1.0.a A.o B.o C.o ...

   where ``A.o``, ``B.o`` and so on are the compiled Haskell modules,
   and ``libHSfoo.a`` is the library you wish to create. The syntax may
   differ slightly on your system, so check the documentation if you run
   into difficulties.

-  To load a package ``foo``, GHCi can load its ``libHSfoo.a`` library
   directly, but it can also load a package in the form of a single
   ``HSfoo.o`` file that has been pre-linked. Loading the ``.o`` file is
   slightly quicker, but at the expense of having another copy of the
   compiled package. The rule of thumb is that if the modules of the
   package were compiled with ``-split-objs`` then building the
   ``HSfoo.o`` is worthwhile because it saves time when loading the
   package into GHCi. Without ``-split-objs``, there is not much
   difference in load time between the ``.o`` and ``.a`` libraries, so
   it is better to save the disk space and only keep the ``.a`` around.
   In a GHC distribution we provide ``.o`` files for most packages
   except the GHC package itself.

   The ``HSfoo.o`` file is built by Cabal automatically; use
   ``--disable-library-for-ghci`` to disable it. To build one manually,
   the following GNU ``ld`` command can be used:

   ::

       ld -r --whole-archive -o HSfoo.o libHSfoo.a

   (replace ``--whole-archive`` with ``-all_load`` on MacOS X)

-  When building the package as shared library, GHC can be used to
   perform the link step. This hides some of the details out the
   underlying linker and provides a common interface to all shared
   object variants that are supported by GHC (DLLs, ELF DSOs, and Mac OS
   dylibs). The shared object must be named in specific way for two
   reasons: (1) the name must contain the GHC compiler version, so that
   two library variants don't collide that are compiled by different
   versions of GHC and that therefore are most likely incompatible with
   respect to calling conventions, (2) it must be different from the
   static name otherwise we would not be able to control the linker as
   precisely as necessary to make the ``-static``/``-dynamic`` flags
   work, see :ref:`options-linker`.

   ::

       ghc -shared libHSfoo-1.0-ghcGHCVersion.so A.o B.o C.o

   Using GHC's version number in the shared object name allows different
   library versions compiled by different GHC versions to be installed
   in standard system locations, e.g. under \*nix ``/usr/lib``. To obtain
   the version number of GHC invoke ``ghc --numeric-version`` and use
   its output in place of ⟨GHCVersion⟩. See also :ref:`options-codegen`
   on how object files must be prepared for shared object linking.

To compile a module which is to be part of a new package, use the
``-package-name`` (to identify the name of the package) and
``-library-name`` (to identify the version and the version hashes of its
identities.) options (:ref:`using-packages`). Failure to use these
options when compiling a package will probably result in disaster, but
you will only discover later when you attempt to import modules from the
package. At this point GHC will complain that the package name it was
expecting the module to come from is not the same as the package name
stored in the ``.hi`` file.

It is worth noting with shared objects, when each package is built as a
single shared object file, since a reference to a shared object costs an
extra indirection, intra-package references are cheaper than
inter-package references. Of course, this applies to the ``main``
package as well.

.. _installed-pkg-info:

``InstalledPackageInfo``: a package specification
-------------------------------------------------

A package specification is a Haskell record; in particular, it is the
record
:cabal-ref:`InstalledPackageInfo <Distribution-InstalledPackageInfo.html#%tInstalledPackageInfo>`
in the module Distribution.InstalledPackageInfo, which is part of the
Cabal package distributed with GHC.

An ``InstalledPackageInfo`` has a human readable/writable syntax. The
functions ``parseInstalledPackageInfo`` and ``showInstalledPackageInfo``
read and write this syntax respectively. Here's an example of the
``InstalledPackageInfo`` for the ``unix`` package:

::

    $ ghc-pkg describe unix
    name: unix
    version: 2.3.1.0
    id: unix-2.3.1.0-de7803f1a8cd88d2161b29b083c94240
    license: BSD3
    copyright:
    maintainer: libraries@haskell.org
    stability:
    homepage:
    package-url:
    description: This package gives you access to the set of operating system
                 services standardised by POSIX 1003.1b (or the IEEE Portable
                 Operating System Interface for Computing Environments -
                 IEEE Std. 1003.1).
                 .
                 The package is not supported under Windows (except under Cygwin).
    category: System
    author:
    exposed: True
    exposed-modules: System.Posix System.Posix.DynamicLinker.Module
                     System.Posix.DynamicLinker.Prim System.Posix.Directory
                     System.Posix.DynamicLinker System.Posix.Env System.Posix.Error
                     System.Posix.Files System.Posix.IO System.Posix.Process
                     System.Posix.Process.Internals System.Posix.Resource
                     System.Posix.Temp System.Posix.Terminal System.Posix.Time
                     System.Posix.Unistd System.Posix.User System.Posix.Signals
                     System.Posix.Signals.Exts System.Posix.Semaphore
                     System.Posix.SharedMem
    hidden-modules:
    trusted: False
    import-dirs: /usr/lib/ghc-6.12.1/unix-2.3.1.0
    library-dirs: /usr/lib/ghc-6.12.1/unix-2.3.1.0
    hs-libraries: HSunix-2.3.1.0
    extra-libraries: rt util dl
    extra-ghci-libraries:
    include-dirs: /usr/lib/ghc-6.12.1/unix-2.3.1.0/include
    includes: HsUnix.h execvpe.h
    depends: base-4.2.0.0-247bb20cde37c3ef4093ee124e04bc1c
    hugs-options:
    cc-options:
    ld-options:
    framework-dirs:
    frameworks:
    haddock-interfaces: /usr/share/doc/ghc/html/libraries/unix/unix.haddock
    haddock-html: /usr/share/doc/ghc/html/libraries/unix

Here is a brief description of the syntax of this file:

A package description consists of a number of field/value pairs. A field
starts with the field name in the left-hand column followed by a
"``:``", and the value continues until the next line that begins in
the left-hand column, or the end of file.

The syntax of the value depends on the field. The various field types
are:

freeform
    Any arbitrary string, no interpretation or parsing is done.

string
    A sequence of non-space characters, or a sequence of arbitrary
    characters surrounded by quotes ``"...."``.

string list
    A sequence of strings, separated by commas. The sequence may be
    empty.

In addition, there are some fields with special syntax (e.g. package
names, version, dependencies).

The allowed fields, with their types, are:

``name``
    .. index::
       single: name; package specification

    (string) The package's name (without the version).

``id``
    .. index::
       single: id; package specification

    (string) The installed package ID. It is up to you to choose a suitable one.

``version``
    .. index::
       single: version; package specification

    (string) The package's version, usually in the form ``A.B`` (any number of
    components are allowed).

``license``
    .. index::
       single: auto; package specification

    (string) The type of license under which this package is
    distributed. This field is a value of the
    :cabal-ref:`License <Distribution-License.html#t:License>` type.

``license-file``
    .. index::
       single: license-file; package specification

    (optional string) The name of a file giving detailed license
    information for this package.

``copyright``
    .. index::
       single: copyright; package specification

    (optional freeform) The copyright string.

``maintainer``
    .. index::
       single: maintainer; package specification

    (optional freeform) The email address of the package's maintainer.

``stability``
    .. index::
       single: stability; package specification

    (optional freeform) A string describing the stability of the package
    (e.g. stable, provisional or experimental).

``homepage``
    .. index::
       single: homepage; package specification

    (optional freeform) URL of the package's home page.

``package-url``
    .. index::
       single: package-url; package specification

    (optional freeform) URL of a downloadable distribution for this
    package. The distribution should be a Cabal package.

``description``
    .. index::
       single: description; package specification

    (optional freeform) Description of the package.

``category``
    .. index::
       single: category; package specification

    (optional freeform) Which category the package belongs to. This
    field is for use in conjunction with a future centralised package
    distribution framework, tentatively titled Hackage.

``author``
    .. index::
       single: author; package specification

    (optional freeform) Author of the package.

``exposed``
    .. index::
       single: exposed; package specification

    (bool) Whether the package is exposed or not.

``exposed-modules``
    .. index::
       single: exposed-modules; package specification

    (string list) modules exposed by this package.

``hidden-modules``
    .. index::
       single: hidden-modules; package specification

    (string list) modules provided by this package, but not exposed to
    the programmer. These modules cannot be imported, but they are still
    subject to the overlapping constraint: no other package in the same
    program may provide a module of the same name.

``reexported-modules``
    .. index::
       single: reexported-modules; reexport specification

    Modules reexported by this package. This list takes the form of
    ``pkg:OldName as NewName (A@orig-pkg-0.1-HASH)``: the first portion
    of the string is the user-written reexport specification (possibly
    omitting the package qualifier and the renaming), while the
    parenthetical is the original package which exposed the module under
    are particular name. Reexported modules have a relaxed overlap
    constraint: it's permissible for two packages to reexport the same
    module as the same name if the reexported moduleis identical.

``trusted``
    .. index::
       single: trusted; package specification

    (bool) Whether the package is trusted or not.

``import-dirs``
    .. index::
       single: import-dirs; package specification

    (string list) A list of directories containing interface files
    (``.hi`` files) for this package.

    If the package contains profiling libraries, then the interface
    files for those library modules should have the suffix ``.p_hi``. So
    the package can contain both normal and profiling versions of the
    same library without conflict (see also ``library_dirs`` below).

``library-dirs``
    .. index::
       single: library-dirs; package specification

    (string list) A list of directories containing libraries for this
    package.

``hs-libraries``
    .. index::
       single: hs-libraries; package specification

    (string list) A list of libraries containing Haskell code for this
    package, with the ``.a`` or ``.dll`` suffix omitted. When packages
    are built as libraries, the ``lib`` prefix is also omitted.

    For use with GHCi, each library should have an object file too. The
    name of the object file does *not* have a ``lib`` prefix, and has
    the normal object suffix for your platform.

    For example, if we specify a Haskell library as ``HSfoo`` in the
    package spec, then the various flavours of library that GHC actually
    uses will be called:

    ``libHSfoo.a``
        The name of the library on Unix and Windows (mingw) systems.
        Note that we don't support building dynamic libraries of Haskell
        code on Unix systems.

    ``HSfoo.dll``
        The name of the dynamic library on Windows systems (optional).

    ``HSfoo.o``; \ ``HSfoo.obj``
        The object version of the library used by GHCi.

``extra-libraries``
    .. index::
       single: extra-libraries; package specification

    (string list) A list of extra libraries for this package. The
    difference between ``hs-libraries`` and ``extra-libraries`` is that
    ``hs-libraries`` normally have several versions, to support
    profiling, parallel and other build options. The various versions
    are given different suffixes to distinguish them, for example the
    profiling version of the standard prelude library is named
    ``libHSbase_p.a``, with the ``_p`` indicating that this is a
    profiling version. The suffix is added automatically by GHC for
    ``hs-libraries`` only, no suffix is added for libraries in
    ``extra-libraries``.

    The libraries listed in ``extra-libraries`` may be any libraries
    supported by your system's linker, including dynamic libraries
    (``.so`` on Unix, ``.DLL`` on Windows).

    Also, ``extra-libraries`` are placed on the linker command line
    after the ``hs-libraries`` for the same package. If your package has
    dependencies in the other direction (i.e. ``extra-libraries``
    depends on ``hs-libraries``), and the libraries are static, you
    might need to make two separate packages.

``include-dirs``
    .. index::
       single: include-dirs; package specification

    (string list) A list of directories containing C includes for this
    package.

``includes``
    .. index::
       single: includes; package specification

    (string list) A list of files to include for via-C compilations
    using this package. Typically the include file(s) will contain
    function prototypes for any C functions used in the package, in case
    they end up being called as a result of Haskell functions from the
    package being inlined.

``depends``
    .. index::
       single: depends; package specification

    (package id list) Packages on which this package depends.

``hugs-options``
    .. index::
       single: hugs-options; package specification

    (string list) Options to pass to Hugs for this package.

``cc-options``
    .. index::
       single: cc-options; package specification

    (string list) Extra arguments to be added to the gcc command line
    when this package is being used (only for via-C compilations).

``ld-options``
    .. index::
       single: ld-options; package specification

    (string list) Extra arguments to be added to the ``gcc`` command
    line (for linking) when this package is being used.

``framework-dirs``
    .. index::
       single: framework-dirs; package specification

    (string list) On Darwin/MacOS X, a list of directories containing
    frameworks for this package. This corresponds to the
    ``-framework-path`` option. It is ignored on all other platforms.

``frameworks``
    .. index::
       single: frameworks; package specification

    (string list) On Darwin/MacOS X, a list of frameworks to link to.
    This corresponds to the ``-framework`` option. Take a look at
    Apple's developer documentation to find out what frameworks actually
    are. This entry is ignored on all other platforms.

``haddock-interfaces``
    .. index::
       single: haddock-interfaces; package specification

    (string list) A list of filenames containing
    `Haddock <http://www.haskell.org/haddock/>`__ interface files
    (``.haddock`` files) for this package.

``haddock-html``
    .. index::
       single: haddock-html; package specification

    (optional string) The directory containing the Haddock-generated
    HTML for this package.

.. _package-environments:

Package environments
--------------------

.. index::
   single: package environments

A *package environment* is a file that tells ``ghc`` precisely which
packages should be visible. It contains package IDs, one per line:

::

    package_id_1
    package_id_2
    ...
    package_id_n

If a package environment is found, it is equivalent to passing these
command line arguments to ``ghc``:

::

    -hide-all-packages
    -package-id package_id_1
    -package-id package_id_2
    ...
    -package-id package_id_n

In order, ``ghc`` will look for the package environment in the following
locations:

-  File ⟨file⟩ if you pass the option ``-package-env file``.

-  File ``$HOME/.ghc/arch-os-version/environments/name`` if you pass the
   option ``-package-env name``.

-  File ⟨file⟩ if the environment variable ``GHC_ENVIRONMENT`` is set to
   ⟨file⟩.

-  File ``$HOME/.ghc/arch-os-version/environments/name`` if the
   environment variable ``GHC_ENVIRONMENT`` is set to ⟨name⟩.

-  File ``./.ghc.environment`` if it exists.

-  File ``$HOME/.ghc/arch-os-version/environments/default`` if it
   exists.

Package environments can be modified by further command line arguments;
for example, if you specify ``-package foo`` on the command line, then
package ⟨foo⟩ will be visible even if it's not listed in the currently
active package environment.

.. [1] it used to in GHC 6.4, but not since 6.6
