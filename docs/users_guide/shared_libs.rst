.. _using-shared-libs:

Using shared libraries
======================

.. index::
   single: Shared libraries; using
   single: Dynamic libraries; using

On some platforms GHC supports building Haskell code into shared
libraries. Shared libraries are also sometimes known as dynamic
libraries, in particular on Windows they are referred to as dynamic link
libraries (DLLs).

Shared libraries allow a single instance of some pre-compiled code to be
shared between several programs. In contrast, with static linking the
code is copied into each program. Using shared libraries can thus save
disk space. They also allow a single copy of code to be shared in memory
between several programs that use it. Shared libraries are often used as
a way of structuring large projects, especially where different parts
are written in different programming languages. Shared libraries are
also commonly used as a plugin mechanism by various applications. This
is particularly common on Windows using COM.

In GHC version 6.12 building shared libraries is supported for Linux (on
x86 and x86-64 architectures). GHC version 7.0 adds support on Windows
(see :ref:`win32-dlls`), FreeBSD and OpenBSD (x86 and x86-64), Solaris
(x86) and Mac OS X (x86 and PowerPC).

Building and using shared libraries is slightly more complicated than
building and using static libraries. When using Cabal much of the detail
is hidden, just use ``--enable-shared`` when configuring a package to
build it into a shared library, or to link it against other packages
built as shared libraries. The additional complexity when building code
is to distinguish whether the code will be used in a shared library or
will use shared library versions of other packages it depends on. There
is additional complexity when installing and distributing shared
libraries or programs that use shared libraries, to ensure that all
shared libraries that are required at runtime are present in suitable
locations.

Building programs that use shared libraries
-------------------------------------------

To build a simple program and have it use shared libraries for the
runtime system and the base libraries use the :ghc-flag:`-dynamic` flag:

.. code-block:: none

    ghc --make -dynamic Main.hs

This has two effects. The first is to compile the code in such a way
that it can be linked against shared library versions of Haskell
packages (such as base). The second is when linking, to link against the
shared versions of the packages' libraries rather than the static
versions. Obviously this requires that the packages were built with
shared libraries. On supported platforms GHC comes with shared libraries
for all the core packages, but if you install extra packages (e.g. with
Cabal) then they would also have to be built with shared libraries
(``--enable-shared`` for Cabal).

Shared libraries for Haskell packages
-------------------------------------

You can build Haskell code into a shared library and make a package to
be used by other Haskell programs. The easiest way is using Cabal,
simply configure the Cabal package with the ``--enable-shared`` flag.

If you want to do the steps manually or are writing your own build
system then there are certain conventions that must be followed.
Building a shared library that exports Haskell code, to be used by other
Haskell code is a bit more complicated than it is for one that exports a
C API and will be used by C code. If you get it wrong you will usually
end up with linker errors.

In particular Haskell shared libraries *must* be made into packages. You
cannot freely assign which modules go in which shared libraries. The
Haskell shared libraries must match the package boundaries. The reason
for this is that GHC handles references to symbols *within* the same
shared library (or main executable binary) differently from references
to symbols *between* different shared libraries. GHC needs to know for
each imported module if that module lives locally in the same shared lib
or in a separate shared lib. The way it does this is by using packages.
When using :ghc-flag:`-dynamic`, a module from a separate package is assumed to
come from a separate shared lib, while modules from the same package (or
the default "main" package) are assumed to be within the same shared lib
(or main executable binary).

Most of the conventions GHC expects when using packages are described in
:ref:`building-packages`. In addition note that GHC expects the ``.hi``
files to use the extension ``.dyn_hi``. The other requirements are the
same as for C libraries and are described below, in particular the use
of the flags :ghc-flag:`-dynamic`, :ghc-flag:`-fPIC` and :ghc-flag:`-shared`.

Shared libraries that export a C API
------------------------------------

Building Haskell code into a shared library is a good way to include
Haskell code in a larger mixed-language project. While with static
linking it is recommended to use GHC to perform the final link step,
with shared libraries a Haskell library can be treated just like any
other shared library. The linking can be done using the normal system C
compiler or linker.

It is possible to load shared libraries generated by GHC in other
programs not written in Haskell, so they are suitable for using as
plugins. Of course to construct a plugin you will have to use the FFI to
export C functions and follow the rules about initialising the RTS. See
:ref:`ffi-library`. In particular you will probably want to export a C
function from your shared library to initialise the plugin before any
Haskell functions are called.

To build Haskell modules that export a C API into a shared library use
the :ghc-flag:`-dynamic`, :ghc-flag:`-fPIC` and :ghc-flag:`-shared` flags:

.. code-block:: none

    ghc --make -dynamic -shared -fPIC Foo.hs -o libfoo.so

As before, the :ghc-flag:`-dynamic` flag specifies that this library links
against the shared library versions of the ``rts`` and ``base`` package. The
:ghc-flag:`-fPIC` flag is required for all code that will end up in a shared
library. The :ghc-flag:`-shared` flag specifies to make a shared library rather
than a program. To make this clearer we can break this down into separate
compilation and link steps:

.. code-block:: none

    ghc -dynamic -fPIC -c Foo.hs
    ghc -dynamic -shared Foo.o -o libfoo.so

In principle you can use :ghc-flag:`-shared` without :ghc-flag:`-dynamic` in the
link step. That means to statically link the runtime system and all of the base
libraries into your new shared library. This would make a very big, but
standalone shared library. On most platforms however that would require all the
static libraries to have been built with :ghc-flag:`-fPIC` so that the code is
suitable to include into a shared library and we do not do that at the moment.

.. warning::
    If your shared library exports a Haskell API then you cannot
    directly link it into another Haskell program and use that Haskell API.
    You will get linker errors. You must instead make it into a package as
    described in the section above.

.. _finding-shared-libs:

Finding shared libraries at runtime
-----------------------------------

The primary difficulty with managing shared libraries is arranging
things such that programs can find the libraries they need at runtime.
The details of how this works varies between platforms, in particular
the three major systems: Unix ELF platforms, Windows and Mac OS X.

.. _finding-shared-libs-unix:

Unix
~~~~

On Unix there are two mechanisms. Shared libraries can be installed into
standard locations that the dynamic linker knows about. For example
``/usr/lib`` or ``/usr/local/lib`` on most systems. The other mechanism
is to use a "runtime path" or "rpath" embedded into programs and
libraries themselves. These paths can either be absolute paths or on at
least Linux and Solaris they can be paths relative to the program or
library itself. In principle this makes it possible to construct fully
relocatable sets of programs and libraries.

GHC has a ``-dynload`` linking flag to select the method that is used to
find shared libraries at runtime. There are currently two modes:

``sysdep``
    A system-dependent mode. This is also the default mode. On Unix ELF
    systems this embeds :envvar:`RPATH`\/:envvar:`RUNPATH` entries into the shared
    library or executable. In particular it uses absolute paths to where
    the shared libraries for the rts and each package can be found. This
    means the program can immediately be run and it will be able to find
    the libraries it needs. However it may not be suitable for
    deployment if the libraries are installed in a different location on
    another machine.

``deploy``
    This does not embed any runtime paths. It relies on the shared
    libraries being available in a standard location or in a directory
    given by the :envvar:`LD_LIBRARY_PATH` environment variable.

To use relative paths for dependent libraries on Linux and Solaris you
can pass a suitable ``-rpath`` flag to the linker:

.. code-block:: none

    ghc -dynamic Main.hs -o main -lfoo -L. -optl-Wl,-rpath,'$ORIGIN'

This assumes that the library ``libfoo.so`` is in the current directory
and will be able to be found in the same directory as the executable
``main`` once the program is deployed. Similarly it would be possible to
use a subdirectory relative to the executable e.g.
``-optl-Wl,-rpath,'$ORIGIN/lib'``.

This relative path technique can be used with either of the two
``-dynload`` modes, though it makes most sense with the ``deploy`` mode.
The difference is that with the ``deploy`` mode, the above example will
end up with an ELF ``RUNPATH`` of just ``$ORIGIN`` while with the
``sysdep`` mode the ``RUNPATH`` will be ``$ORIGIN`` followed by all the
library directories of all the packages that the program depends on
(e.g. ``base`` and ``rts`` packages etc.) which are typically absolute
paths. The unix tool ``readelf --dynamic`` is handy for inspecting the
``RPATH``/``RUNPATH`` entries in ELF shared libraries and executables.

On most UNIX platforms it is also possible to build executables that can be
``dlopen``\'d like shared libraries using the :ghc-flag:`-pie` flag during
linking.

.. _finding-shared-libs-mac:

Mac OS X
~~~~~~~~

The standard assumption on Darwin/Mac OS X is that dynamic libraries will be
stamped at build time with an "install name", which is the full ultimate
install path of the library file. Any libraries or executables that
subsequently link against it (even if it hasn't been installed yet) will pick
up that path as their runtime search location for it. When compiling with ghc
directly, the install name is set by default to the location where it is built.
You can override this with the :ghc-flag:`-dylib-install-name ⟨path⟩` option
(which passes ``-install_name`` to the Apple linker). Cabal does this for you.
It automatically sets the install name for dynamic libraries to the absolute
path of the ultimate install location.
