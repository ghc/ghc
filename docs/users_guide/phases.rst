.. _options-phases:

Options related to a particular phase
=====================================

.. _replacing-phases:

Replacing the program for one or more phases
--------------------------------------------

.. index::
   single: compilation phases, changing

You may specify that a different program be used for one of the phases
of the compilation system, in place of whatever the ``ghc`` has wired
into it. For example, you might want to try a different assembler. The
following options allow you to change the external program used for a
given compilation phase:

``-pgmL ⟨cmd⟩``
    .. index::
       single: -pgmL

    Use ⟨cmd⟩ as the literate pre-processor.

``-pgmP ⟨cmd⟩``
    .. index::
       single: -pgmP

    Use ⟨cmd⟩ as the C pre-processor (with ``-cpp`` only).

``-pgmc ⟨cmd⟩``
    .. index::
       single: -pgmc

    Use ⟨cmd⟩ as the C compiler.

``-pgmlo ⟨cmd⟩``
    .. index::
       single: -pgmlo

    Use ⟨cmd⟩ as the LLVM optimiser.

``-pgmlc ⟨cmd⟩``
    .. index::
       single: -pgmlc

    Use ⟨cmd⟩ as the LLVM compiler.

``-pgms ⟨cmd⟩``
    .. index::
       single: -pgms

    Use ⟨cmd⟩ as the splitter.

``-pgma ⟨cmd⟩``
    .. index::
       single: -pgma

    Use ⟨cmd⟩ as the assembler.

``-pgml ⟨cmd⟩``
    .. index::
       single: -pgml

    Use ⟨cmd⟩ as the linker.

``-pgmdll ⟨cmd⟩``
    .. index::
       single: -pgmdll

    Use ⟨cmd⟩ as the DLL generator.

``-pgmF ⟨cmd⟩``
    .. index::
       single: -pgmF

    Use ⟨cmd⟩ as the pre-processor (with ``-F`` only).

``-pgmwindres ⟨cmd⟩``
    .. index::
       single: -pgmwindres

    Use ⟨cmd⟩ as the program to use for embedding manifests on Windows.
    Normally this is the program ``windres``, which is supplied with a
    GHC installation. See ``-fno-embed-manifest`` in
    :ref:`options-linker`.

``-pgmlibtool ⟨cmd⟩``
    .. index::
       single: -pgmlibtool

    Use ⟨cmd⟩ as the libtool command (when using ``-staticlib`` only).

.. _forcing-options-through:

Forcing options to a particular phase
-------------------------------------

.. index::
   single: forcing GHC-phase options

Options can be forced through to a particular compilation phase, using
the following flags:

``-optL ⟨option⟩``
    .. index::
       single: -optL

    Pass ⟨option⟩ to the literate pre-processor

``-optP ⟨option⟩``
    .. index::
       single: -optP

    Pass ⟨option⟩ to CPP (makes sense only if ``-cpp`` is also on).

``-optF ⟨option⟩``
    .. index::
       single: -optF

    Pass ⟨option⟩ to the custom pre-processor (see
    :ref:`pre-processor`).

``-optc ⟨option⟩``
    .. index::
       single: -optc

    Pass ⟨option⟩ to the C compiler.

``-optlo ⟨option⟩``
    .. index::
       single: -optlo

    Pass ⟨option⟩ to the LLVM optimiser.

``-optlc ⟨option⟩``
    .. index::
       single: -optlc

    Pass ⟨option⟩ to the LLVM compiler.

``-opta ⟨option⟩``
    .. index::
       single: -opta

    Pass ⟨option⟩ to the assembler.

``-optl ⟨option⟩``
    .. index::
       single: -optl

    Pass ⟨option⟩ to the linker.

``-optdll ⟨option⟩``
    .. index::
       single: -optdll

    Pass ⟨option⟩ to the DLL generator.

``-optwindres ⟨option⟩``
    .. index::
       single: -optwindres

    Pass ⟨option⟩ to ``windres`` when embedding manifests on Windows.
    See ``-fno-embed-manifest`` in :ref:`options-linker`.

So, for example, to force an ``-Ewurble`` option to the assembler, you
would tell the driver ``-opta-Ewurble`` (the dash before the E is
required).

GHC is itself a Haskell program, so if you need to pass options directly
to GHC's runtime system you can enclose them in ``+RTS ... -RTS`` (see
:ref:`runtime-control`).

.. _c-pre-processor:

Options affecting the C pre-processor
-------------------------------------

.. index::
   single: pre-processing: cpp
   single: C pre-processor options
   single: cpp, pre-processing with

``-cpp``
    .. index::
       single: -cpp

    The C pre-processor ``cpp`` is run over your Haskell code only if
    the ``-cpp`` option -cpp option is given. Unless you are building a
    large system with significant doses of conditional compilation, you
    really shouldn't need it.

``-D ⟨symbol⟩[=⟨value⟩]``
    .. index::
       single: -D

    Define macro ⟨symbol⟩ in the usual way. NB: does *not* affect ``-D``
    macros passed to the C compiler when compiling via C! For those, use
    the ``-optc-Dfoo`` hack… (see :ref:`forcing-options-through`).

``-U ⟨symbol⟩``
    .. index::
       single: -U

    Undefine macro ⟨symbol⟩ in the usual way.

``-I ⟨dir⟩``
    .. index::
       single: -I

    Specify a directory in which to look for ``#include`` files, in the
    usual C way.

The GHC driver pre-defines several macros when processing Haskell source
code (``.hs`` or ``.lhs`` files).

The symbols defined by GHC are listed below. To check which symbols are
defined by your local GHC installation, the following trick is useful:

::

    $ ghc -E -optP-dM -cpp foo.hs
    $ cat foo.hspp

(you need a file ``foo.hs``, but it isn't actually used).

``__GLASGOW_HASKELL__``
    .. index::
       single: __GLASGOW_HASKELL__

    For version ``x.y.z`` of GHC, the value of ``__GLASGOW_HASKELL__``
    is the integer ⟨xyy⟩ (if ⟨y⟩ is a single digit, then a leading zero
    is added, so for example in version 6.2 of GHC,
    ``__GLASGOW_HASKELL__==602``). More information in
    :ref:`version-numbering`.

    With any luck, ``__GLASGOW_HASKELL__`` will be undefined in all
    other implementations that support C-style pre-processing.

    .. note::
       The comparable symbols for other systems are:
       ``__HUGS__`` for Hugs, ``__NHC__`` for nhc98, and ``__HBC__`` for
       hbc).

    NB. This macro is set when pre-processing both Haskell source and C
    source, including the C source generated from a Haskell module (i.e.
    ``.hs``, ``.lhs``, ``.c`` and ``.hc`` files).

``__GLASGOW_HASKELL_PATCHLEVEL1__``; \ ``__GLASGOW_HASKELL_PATCHLEVEL2__``
    .. index::
       single: __GLASGOW_HASKELL_PATCHLEVEL2__

    .. index::
       single: __GLASGOW_HASKELL_PATCHLEVEL1__

    These macros are available starting with GHC 7.10.1.

    For three-part GHC version numbers ``x.y.z``, the value of
    ``__GLASGOW_HASKELL_PATCHLEVEL1__`` is the integer ⟨z⟩.

    For four-part GHC version numbers ``x.y.z.z'``, the value of
    ``__GLASGOW_HASKELL_PATCHLEVEL1__`` is the integer ⟨z⟩ while the
    value of ``__GLASGOW_HASKELL_PATCHLEVEL2__`` is set to the integer
    ⟨z'⟩.

    These macros are provided for allowing finer granularity than is
    provided by ``__GLASGOW_HASKELL__``. Usually, this should not be
    necessary as it's expected for most APIs to remain stable between
    patchlevel releases, but occasionally internal API changes are
    necessary to fix bugs. Also conditional compilation on the
    patchlevel can be useful for working around bugs in older releases.

    .. tip::
       These macros are set when pre-processing both Haskell source and
       C source, including the C source generated from a Haskell module
       (i.e. ``.hs``, ``.lhs``, ``.c`` and ``.hc`` files).

``MIN_VERSION_GLASGOW_HASKELL(x,y,z,z')``
    .. index::
       single: MIN_VERSION_GLASGOW_HASKELL

    This macro is available starting with GHC 7.10.1.

    This macro is provided for convenience to write CPP conditionals
    testing whether the GHC version used is version ``x.y.z.z'`` or
    later.

    If compatibility with Haskell compilers (including GHC prior to
    version 7.10.1) which do not define ``MIN_VERSION_GLASGOW_HASKELL``
    is required, the presence of the ``MIN_VERSION_GLASGOW_HASKELL``
    macro needs to be ensured before it is called, e.g.:

    ::

        #ifdef MIN_VERSION_GLASGOW_HASKELL
        #if MIN_VERSION_GLASGOW_HASKELL(7,10,2,0)
        /* code that applies only to GHC 7.10.2 or later */
        #endif
        #endif

    .. tip::
       This macro is set when pre-processing both Haskell source and C
       source, including the C source generated from a Haskell module (i.e.
       ``.hs``, ``.lhs``, ``.c`` and ``.hc`` files).

``__GLASGOW_HASKELL_TH__``
    .. index::
       single: __GLASGOW_HASKELL_TH__

    This is set to ``YES`` when the compiler supports Template Haskell,
    and to ``NO`` when not. The latter is the case for a stage-1
    compiler during bootstrapping, or on architectures where the
    interpreter is not available.

``__GLASGOW_HASKELL_LLVM__``
    .. index::
       single: __GLASGOW_HASKELL_LLVM__

    Only defined when ``-fllvm`` is specified. When GHC is using version
    ``x.y.z`` of LLVM, the value of ``__GLASGOW_HASKELL_LLVM__`` is the
    integer ⟨xy⟩.

``__PARALLEL_HASKELL__``
    .. index::
       single: __PARALLEL_HASKELL__

    Only defined when ``-parallel`` is in use! This symbol is defined
    when pre-processing Haskell (input) and pre-processing C (GHC
    output).

``os_HOST_OS=1``
    This define allows conditional compilation based on the Operating
    System, where⟨os⟩ is the name of the current Operating System (eg.
    ``linux``, ``mingw32`` for Windows, ``solaris``, etc.).

``arch_HOST_ARCH=1``
    This define allows conditional compilation based on the host
    architecture, where⟨arch⟩ is the name of the current architecture
    (eg. ``i386``, ``x86_64``, ``powerpc``, ``sparc``, etc.).

``VERSION_pkgname``
    This macro is available starting GHC 8.0.  It is defined for every
    exposed package, but only if the ``-hide-all-packages`` flag
    is set.  This macro expands to a string recording the
    version of ``pkgname`` that is exposed for module import.
    It is identical in behavior to the ``VERSION_pkgname`` macros
    that Cabal defines.

``MIN_VERSION_pkgname(x,y,z)``
    This macro is available starting GHC 8.0.  It is defined for every
    exposed package, but only if the ``-hide-all-packages`` flag
    is set. This macro is provided for convenience to write CPP
    conditionals testing if a package version is ``x.y.z`` or
    less.  It is identical in behavior to the ``MIN_VERSION_pkgname``
    macros that Cabal defines.

.. _cpp-string-gaps:

CPP and string gaps
~~~~~~~~~~~~~~~~~~~

.. index::
   single: -cpp vs string gaps
   single: string gaps vs -cpp.

A small word of warning: ``-cpp`` is not friendly to "string gaps".
In other words, strings such as the following:

::

    strmod = "\
    \ p \
    \ "

don't work with ``-cpp``; ``/usr/bin/cpp`` elides the backslash-newline
pairs.

However, it appears that if you add a space at the end of the line, then
``cpp`` (at least GNU ``cpp`` and possibly other ``cpp``\ s) leaves the
backslash-space pairs alone and the string gap works as expected.

.. _pre-processor:

Options affecting a Haskell pre-processor
-----------------------------------------

.. index::
   single: pre-processing: custom
   single: pre-processor options

``-F``
    .. index::
       single: -F

    A custom pre-processor is run over your Haskell source file only if
    the ``-F`` option is given.

    Running a custom pre-processor at compile-time is in some settings
    appropriate and useful. The ``-F`` option lets you run a
    pre-processor as part of the overall GHC compilation pipeline, which
    has the advantage over running a Haskell pre-processor separately in
    that it works in interpreted mode and you can continue to take reap
    the benefits of GHC's recompilation checker.

    The pre-processor is run just before the Haskell compiler proper
    processes the Haskell input, but after the literate markup has been
    stripped away and (possibly) the C pre-processor has washed the
    Haskell input.

    Use ``-pgmF ⟨cmd⟩`` to select the program to use as the preprocessor.
    When invoked, the ⟨cmd⟩ pre-processor is given at least three
    arguments on its command-line: the first argument is the name of the
    original source file, the second is the name of the file holding the
    input, and the third is the name of the file where ⟨cmd⟩ should
    write its output to.

    Additional arguments to the pre-processor can be passed in using the
    ``-optF`` option. These are fed to ⟨cmd⟩ on the command line after
    the three standard input and output arguments.

    An example of a pre-processor is to convert your source files to the
    input encoding that GHC expects, i.e. create a script ``convert.sh``
    containing the lines:

    ::

        #!/bin/sh
        ( echo "{-# LINE 1 \"$2\" #-}" ; iconv -f l1 -t utf-8 $2 ) > $3

    and pass ``-F -pgmF convert.sh`` to GHC. The ``-f l1`` option tells
    iconv to convert your Latin-1 file, supplied in argument ``$2``,
    while the "-t utf-8" options tell iconv to return a UTF-8 encoded
    file. The result is redirected into argument ``$3``. The
    ``echo "{-# LINE 1 \"$2\" #-}"`` just makes sure that your error
    positions are reported as in the original source file.

.. _options-codegen:

Options affecting code generation
---------------------------------

``-fasm``
    .. index::
       single: -fasm

    Use GHC's :ref:`native code generator <native-code-gen>` rather than
    compiling via LLVM. ``-fasm`` is the default.

``-fllvm``
    .. index::
       single: -fllvm

    Compile via :ref:`LLVM <llvm-code-gen>` instead of using the native
    code generator. This will generally take slightly longer than the
    native code generator to compile. Produced code is generally the
    same speed or faster than the other two code generators. Compiling
    via LLVM requires LLVM's ``opt`` and ``llc`` executables to be in ``PATH``.

``-fno-code``
    .. index::
       single: -fno-code

    Omit code generation (and all later phases) altogether. This is
    useful if you're only interested in type checking code.

``-fwrite-interface``
    .. index::
       single: -fwrite-interface

    Always write interface files. GHC will normally write interface
    files automatically, but this flag is useful with ``-fno-code``,
    which normally suppresses generation of interface files. This is
    useful if you want to type check over multiple runs of GHC without
    compiling dependencies.

``-fobject-code``
    .. index::
       single: -fobject-code

    Generate object code. This is the default outside of GHCi, and can
    be used with GHCi to cause object code to be generated in preference
    to bytecode.

``-fbyte-code``
    .. index::
       single: -fbyte-code

    Generate byte-code instead of object-code. This is the default in
    GHCi. Byte-code can currently only be used in the interactive
    interpreter, not saved to disk. This option is only useful for
    reversing the effect of ``-fobject-code``.

``-fPIC``
    .. index::
       single: -fPIC

    Generate position-independent code (code that can be put into shared
    libraries). This currently works on Linux x86 and x86-64. On
    Windows, position-independent code is never used so the flag is a
    no-op on that platform.

``-dynamic``
    When generating code, assume that entities imported from a different
    package will reside in a different shared library or binary.

    Note that using this option when linking causes GHC to link against
    shared libraries.

.. _options-linker:

Options affecting linking
-------------------------

.. index::
   single: linker options
   single: ld options

GHC has to link your code with various libraries, possibly including:
user-supplied, GHC-supplied, and system-supplied (``-lm`` math library,
for example).

``-l ⟨lib⟩``
    .. index::
       single: -l

    Link in the ⟨lib⟩ library. On Unix systems, this will be in a file
    called ``liblib.a`` or ``liblib.so`` which resides somewhere on the
    library directories path.

    Because of the sad state of most UNIX linkers, the order of such
    options does matter. If library ⟨foo⟩ requires library ⟨bar⟩, then
    in general ``-l ⟨foo⟩`` should come *before* ``-l ⟨bar⟩`` on the
    command line.

    There's one other gotcha to bear in mind when using external
    libraries: if the library contains a ``main()`` function, then this
    will be linked in preference to GHC's own ``main()`` function (eg.
    ``libf2c`` and ``libl`` have their own ``main()``\ s). This is
    because GHC's ``main()`` comes from the ``HSrts`` library, which is
    normally included *after* all the other libraries on the linker's
    command line. To force GHC's ``main()`` to be used in preference to
    any other ``main()``\ s from external libraries, just add the option
    ``-lHSrts`` before any other libraries on the command line.

``-c``
    .. index::
       single: -c

    Omits the link step. This option can be used with ``--make`` to
    avoid the automatic linking that takes place if the program contains
    a ``Main`` module.

``-package ⟨name⟩``
    .. index::
       single: -package

    If you are using a Haskell “package” (see :ref:`packages`), don't
    forget to add the relevant ``-package`` option when linking the
    program too: it will cause the appropriate libraries to be linked in
    with the program. Forgetting the ``-package`` option will likely
    result in several pages of link errors.

``-framework ⟨name⟩``
    .. index::
       single: -framework

    On Darwin/OS X/iOS only, link in the framework ⟨name⟩. This option
    corresponds to the ``-framework`` option for Apple's Linker. Please
    note that frameworks and packages are two different things -
    frameworks don't contain any Haskell code. Rather, they are Apple's
    way of packaging shared libraries. To link to Apple's “Carbon” API,
    for example, you'd use ``-framework Carbon``.

``-staticlib``
    .. index::
       single: -staticlib

    On Darwin/OS X/iOS only, link all passed files into a static library
    suitable for linking into an iOS (when using a cross-compiler) or
    Mac Xcode project. To control the name, use the ``-o`` ⟨name⟩ option
    as usual. The default name is ``liba.a``. This should nearly always
    be passed when compiling for iOS with a cross-compiler.

``-L ⟨dir⟩``
    .. index::
       single: -L

    Where to find user-supplied libraries… Prepend the directory ⟨dir⟩
    to the library directories path.

``-framework-path ⟨dir⟩``
    .. index::
       single: -framework-path

    On Darwin/OS X/iOS only, prepend the directory ⟨dir⟩ to the
    framework directories path. This option corresponds to the ``-F``
    option for Apple's Linker (``-F`` already means something else for
    GHC).

``-split-objs``
    .. index::
       single: -split-objs

    Tell the linker to split the single object file that would normally
    be generated into multiple object files, one per top-level Haskell
    function or type in the module. This only makes sense for libraries,
    where it means that executables linked against the library are
    smaller as they only link against the object files that they need.
    However, assembling all the sections separately is expensive, so
    this is slower than compiling normally. Additionally, the size of
    the library itself (the ``.a`` file) can be a factor of 2 to 2.5
    larger. We use this feature for building GHC's libraries.

``-split-sections``
    .. index::
       single: -split-sections

    Place each generated function or data item into its own section in the
    output file if the target supports arbitrary sections. The name of the
    function or the name of the data item determines the section's name in the
    output file.

    When linking, the linker can automatically remove all unreferenced sections
    and thus produce smaller executables. The effect is similar to
    ``-split-objs``, but somewhat more efficient - the generated library files
    are about 30% smaller than with ``-split-objs``.

``-static``
    .. index::
       single: -static

    Tell the linker to avoid shared Haskell libraries, if possible. This
    is the default.

``-dynamic``
    .. index::
       single: -dynamic

    This flag tells GHC to link against shared Haskell libraries. This
    flag only affects the selection of dependent libraries, not the form
    of the current target (see -shared). See :ref:`using-shared-libs` on
    how to create them.

    Note that this option also has an effect on code generation (see
    above).

``-shared``
    .. index::
       single: -shared

    Instead of creating an executable, GHC produces a shared object with
    this linker flag. Depending on the operating system target, this
    might be an ELF DSO, a Windows DLL, or a Mac OS dylib. GHC hides the
    operating system details beneath this uniform flag.

    The flags ``-dynamic``/``-static`` control whether the resulting
    shared object links statically or dynamically to Haskell package
    libraries given as ``-package`` option. Non-Haskell libraries are
    linked as gcc would regularly link it on your system, e.g. on most
    ELF system the linker uses the dynamic libraries when found.

    Object files linked into shared objects must be compiled with
    ``-fPIC``, see :ref:`options-codegen`

    When creating shared objects for Haskell packages, the shared object
    must be named properly, so that GHC recognizes the shared object
    when linked against this package. See shared object name mangling.

``-dynload``
    .. index::
       single: -dynload

    This flag selects one of a number of modes for finding shared
    libraries at runtime. See :ref:`finding-shared-libs` for a
    description of each mode.

``-main-is ⟨thing⟩``
    .. index::
       single: -main-is
       single: specifying your own main function

    The normal rule in Haskell is that your program must supply a
    ``main`` function in module ``Main``. When testing, it is often
    convenient to change which function is the "main" one, and the
    ``-main-is`` flag allows you to do so. The ⟨thing⟩ can be one of:

    -  A lower-case identifier ``foo``. GHC assumes that the main
       function is ``Main.foo``.

    -  A module name ``A``. GHC assumes that the main function is
       ``A.main``.

    -  A qualified name ``A.foo``. GHC assumes that the main function is
       ``A.foo``.

    Strictly speaking, ``-main-is`` is not a link-phase flag at all; it
    has no effect on the link step. The flag must be specified when
    compiling the module containing the specified main function (e.g.
    module ``A`` in the latter two items above). It has no effect for
    other modules, and hence can safely be given to ``ghc --make``.
    However, if all the modules are otherwise up to date, you may need
    to force recompilation both of the module where the new "main" is,
    and of the module where the "main" function used to be; ``ghc`` is
    not clever enough to figure out that they both need recompiling. You
    can force recompilation by removing the object file, or by using the
    ``-fforce-recomp`` flag.

``-no-hs-main``
    .. index::
       single: -no-hs-main
       single: linking Haskell libraries with foreign code

    In the event you want to include ghc-compiled code as part of
    another (non-Haskell) program, the RTS will not be supplying its
    definition of ``main()`` at link-time, you will have to. To signal
    that to the compiler when linking, use ``-no-hs-main``. See also
    :ref:`using-own-main`.

    Notice that since the command-line passed to the linker is rather
    involved, you probably want to use ``ghc`` to do the final link of
    your \`mixed-language' application. This is not a requirement
    though, just try linking once with ``-v`` on to see what options the
    driver passes through to the linker.

    The ``-no-hs-main`` flag can also be used to persuade the compiler
    to do the link step in ``--make`` mode when there is no Haskell
    ``Main`` module present (normally the compiler will not attempt
    linking when there is no ``Main``).

    The flags ``-rtsopts`` and ``-with-rtsopts`` have no effect when
    used with ``-no-hs-main``, because they are implemented by changing
    the definition of ``main`` that GHC generates. See
    :ref:`using-own-main` for how to get the effect of ``-rtsopts`` and
    ``-with-rtsopts`` when using your own ``main``.

``-debug``
    .. index::
       single: -debug

    Link the program with a debugging version of the runtime system. The
    debugging runtime turns on numerous assertions and sanity checks,
    and provides extra options for producing debugging output at runtime
    (run the program with ``+RTS -?`` to see a list).

``-threaded``
    .. index::
       single: -threaded

    Link the program with the "threaded" version of the runtime system.
    The threaded runtime system is so-called because it manages multiple
    OS threads, as opposed to the default runtime system which is purely
    single-threaded.

    Note that you do *not* need ``-threaded`` in order to use
    concurrency; the single-threaded runtime supports concurrency
    between Haskell threads just fine.

    The threaded runtime system provides the following benefits:

    -  It enables the ``-N``\ ``-Nx``\ RTS option RTS option to be used,
       which allows threads to run in parallelparallelism on a
       multiprocessormultiprocessorSMP or multicoremulticore machine.
       See :ref:`using-smp`.

    -  If a thread makes a foreign call (and the call is not marked
       ``unsafe``), then other Haskell threads in the program will
       continue to run while the foreign call is in progress.
       Additionally, ``foreign export``\ ed Haskell functions may be
       called from multiple OS threads simultaneously. See
       :ref:`ffi-threads`.

``-eventlog``
    .. index::
       single: -eventlog

    Link the program with the "eventlog" version of the runtime system.
    A program linked in this way can generate a runtime trace of events
    (such as thread start/stop) to a binary file ``program.eventlog``,
    which can then be interpreted later by various tools. See
    :ref:`rts-eventlog` for more information.

    ``-eventlog`` can be used with ``-threaded``. It is implied by
    ``-debug``.

``-rtsopts``
    .. index::
       single: -rtsopts

    This option affects the processing of RTS control options given
    either on the command line or via the ``GHCRTS`` environment
    variable. There are three possibilities:

    ``-rtsopts=none``
        Disable all processing of RTS options. If ``+RTS`` appears
        anywhere on the command line, then the program will abort with
        an error message. If the ``GHCRTS`` environment variable is set,
        then the program will emit a warning message, ``GHCRTS`` will be
        ignored, and the program will run as normal.

    ``-rtsopts=some``
        [this is the default setting] Enable only the "safe" RTS
        options: (Currently only ``-?`` and ``--info``.) Any other RTS
        options on the command line or in the ``GHCRTS`` environment
        variable causes the program with to abort with an error message.

    ``-rtsopts=all`` or just ``-rtsopts``
        Enable *all* RTS option processing, both on the command line and
        through the ``GHCRTS`` environment variable.

    In GHC 6.12.3 and earlier, the default was to process all RTS
    options. However, since RTS options can be used to write logging
    data to arbitrary files under the security context of the running
    program, there is a potential security problem. For this reason, GHC
    7.0.1 and later default to ``-rtsops=some``.

    Note that ``-rtsopts`` has no effect when used with ``-no-hs-main``;
    see :ref:`using-own-main` for details.

``-with-rtsopts``
    .. index::
       single: -with-rtsopts

    This option allows you to set the default RTS options at link-time.
    For example, ``-with-rtsopts="-H128m"`` sets the default heap size
    to 128MB. This will always be the default heap size for this
    program, unless the user overrides it. (Depending on the setting of
    the ``-rtsopts`` option, the user might not have the ability to
    change RTS options at run-time, in which case ``-with-rtsopts``
    would be the *only* way to set them.)

    Note that ``-with-rtsopts`` has no effect when used with
    ``-no-hs-main``; see :ref:`using-own-main` for details.

``-no-rtsopts-suggestions``
    .. index::
       single: -no-rtsopts-suggestions

    This option disables RTS suggestions about linking with ``-rtsopts``
    when they are not available. These suggestions would be unhelpful if
    the users have installed Haskell programs through their package
    managers. With this option enabled, these suggestions will not
    appear. It is recommended for people distributing binaries to build
    with either ``-rtsopts`` or ``-no-rtsopts-suggestions``.

``-fno-gen-manifest``
    .. index::
       single: -fno-gen-manifest

    On Windows, GHC normally generates a manifestmanifest file when
    linking a binary. The manifest is placed in the file
    ``prog.exe.manifest`` where ⟨prog.exe⟩ is the name of the
    executable. The manifest file currently serves just one purpose: it
    disables the "installer detection"installer detectionin Windows
    Vista that attempts to elevate privileges for executables with
    certain names (e.g. names containing "install", "setup" or "patch").
    Without the manifest file to turn off installer detection,
    attempting to run an executable that Windows deems to be an
    installer will return a permission error code to the invoker.
    Depending on the invoker, the result might be a dialog box asking
    the user for elevated permissions, or it might simply be a
    permission denied error.

    Installer detection can be also turned off globally for the system
    using the security control panel, but GHC by default generates
    binaries that don't depend on the user having disabled installer
    detection.

    The ``-fno-gen-manifest`` disables generation of the manifest file.
    One reason to do this would be if you had a manifest file of your
    own, for example.

    In the future, GHC might use the manifest file for more things, such
    as supplying the location of dependent DLLs.

    ``-fno-gen-manifest`` also implies ``-fno-embed-manifest``, see
    below.

``-fno-embed-manifest``
    .. index::
       single: -fno-embed-manifest
       single: windres

    The manifest file that GHC generates when linking a binary on
    Windows is also embedded in the executable itself, by default. This
    means that the binary can be distributed without having to supply
    the manifest file too. The embedding is done by running
    ``windres``; to see exactly what GHC does to embed the
    manifest, use the ``-v`` flag. A GHC installation comes with its own
    copy of ``windres`` for this reason.

    See also ``-pgmwindres`` (:ref:`replacing-phases`) and
    ``-optwindres`` (:ref:`forcing-options-through`).

``-fno-shared-implib``
    .. index::
       single: -fno-shared-implib

    DLLs on Windows are typically linked to by linking to a
    corresponding ``.lib`` or ``.dll.a`` — the so-called import library.
    GHC will typically generate such a file for every DLL you create by
    compiling in ``-shared`` mode. However, sometimes you don't want to
    pay the disk-space cost of creating this import library, which can
    be substantial — it might require as much space as the code itself,
    as Haskell DLLs tend to export lots of symbols.

    As long as you are happy to only be able to link to the DLL using
    ``GetProcAddress`` and friends, you can supply the
    ``-fno-shared-implib`` flag to disable the creation of the import
    library entirely.

``-dylib-install-name path``
    .. index::
       single: -dylib-install-name

    On Darwin/OS X, dynamic libraries are stamped at build time with an
    "install name", which is the ultimate install path of the library
    file. Any libraries or executables that subsequently link against it
    will pick up that path as their runtime search location for it. By
    default, ghc sets the install name to the location where the library
    is built. This option allows you to override it with the specified
    file path. (It passes ``-install_name`` to Apple's linker.) Ignored
    on other platforms.

``-rdynamic``
    .. index::
       single: -rdynamic

    This instructs the linker to add all symbols, not only used ones, to
    the dynamic symbol table. Currently Linux and Windows/MinGW32 only.
    This is equivalent to using ``-optl -rdynamic`` on Linux, and
    ``-optl -export-all-symbols`` on Windows.
