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

.. ghc-flag:: -pgmL ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the literate pre-processor
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the literate pre-processor.

.. ghc-flag:: -pgmP ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the C pre-processor (with :ghc-flag:`-cpp` only)
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the C pre-processor (with :ghc-flag:`-cpp` only).

.. ghc-flag:: -pgmJSP ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the JavaScript C pre-processor (only for javascript-backend)
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the JavaScript C pre-processor (only for javascript-backend).

.. ghc-flag:: -pgmCmmP ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the C-- C pre-processor
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the C-- C pre-processor.

.. ghc-flag:: -pgmc ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the C compiler
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the C compiler.

.. ghc-flag:: -pgmcxx ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the C++ compiler
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the C++ compiler.

.. ghc-flag:: -pgmlo ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the LLVM optimiser
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the LLVM optimiser.

.. ghc-flag:: -pgmlc ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the LLVM compiler
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the LLVM compiler.

.. ghc-flag:: -pgmlas ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the LLVM assembler
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the LLVM assembler

.. ghc-flag:: -pgms ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the splitter
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the splitter.

.. ghc-flag:: -pgma ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the assembler
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the assembler.

.. ghc-flag:: -pgml ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the linker
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the linker.

.. ghc-flag:: -pgmlm ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the linker when merging object files
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the linker when merging object files (e.g. when generating
    joined objects for loading into GHCi).

.. ghc-flag:: -pgmF ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the pre-processor (with :ghc-flag:`-F` only)
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the pre-processor (with :ghc-flag:`-F` only).

.. ghc-flag:: -pgmotool ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the program to inspect mach-o dylibs on macOS
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the program to inspect mach-o dynamic libraries and
    executables to read the dynamic library dependencies.  We will compute
    the necessary ``runpath``s to embed for the dependencies based on the
    result of the ``otool`` call.

.. ghc-flag:: -pgminstall_name_tool ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the program to inject ``runpath`` into mach-o dylibs on macOS
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the program to inject ``runpath``s into mach-o dynamic
    libraries and executables.  As detected by the ``otool`` call.

.. ghc-flag:: -pgmwindres ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the program for embedding manifests on Windows.
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the program to use for embedding manifests on Windows.
    Normally this is the program ``windres``, which is supplied with a
    GHC installation. See ``-fno-embed-manifest`` in
    :ref:`options-linker`.

.. ghc-flag:: -pgmi ⟨cmd⟩
    :shortdesc: Use ⟨cmd⟩ as the external interpreter command.
    :type: dynamic
    :category: phase-programs

    Use ⟨cmd⟩ as the external interpreter command (see
    :ref:`external-interpreter`).  Default: ``ghc-iserv-prof`` if
    :ghc-flag:`-prof` is enabled, ``ghc-iserv-dyn`` if :ghc-flag:`-dynamic` is
    enabled, or ``ghc-iserv`` otherwise.

.. _forcing-options-through:

Forcing options to a particular phase
-------------------------------------

.. index::
   single: forcing GHC-phase options

Options can be forced through to a particular compilation phase, using
the following flags:

.. ghc-flag:: -optL ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the literate pre-processor
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the literate pre-processor

.. ghc-flag:: -optP ⟨option⟩
    :shortdesc: pass ⟨option⟩ to cpp (with :ghc-flag:`-cpp` only)
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to CPP (makes sense only if :ghc-flag:`-cpp` is also on).

.. ghc-flag:: -optJSP ⟨option⟩
    :shortdesc: pass ⟨option⟩ to JavaScript C pre-processor (only for javascript-backend)
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to JavaScript C pre-processor (only for javascript-backend).

.. ghc-flag:: -optCmmP ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the C-- C pre-processor.
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the C-- C pre-processor.

    The C-- C pre-processor also receives C compiler flags.  Those flags will
    come _before_ the flags added by this option.  As a result, the net effect
    of the following pair of flags is zero: :code:`-optCmmP-UFOO -optc-DFOO`.

.. ghc-flag:: -optF ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the custom pre-processor
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the custom pre-processor (see
    :ref:`pre-processor`).

.. ghc-flag:: -optc ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the C compiler
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the C compiler and, for compatibility, C-- pre-processor.

.. ghc-flag:: -pgmc-supports-no-pie
    :shortdesc: *(deprecated)*
        Indicate that the linker supports ``-no-pie``
    :type: dynamic
    :category: phase-options

    Does the same thing as ``-pgml-supports-no-pie``, which replaced it.

.. ghc-flag:: -pgml-supports-no-pie
    :shortdesc: Indicate that the linker supports ``-no-pie``
    :type: dynamic
    :category: phase-options

    When ``-pgml`` is used, GHC by default will never pass the ``-no-pie``
    command line flag. The rationale is that it is not known whether the
    specified compiler used for linking (recall we use a C compiler to
    invoke the linker on our behalf) will support it. This flag can be
    used to indicate that ``-no-pie`` is supported. It has to be passed
    after ``-pgml``.

    This flag is not necessary when ``-pgmc`` is not used, since GHC
    remembers whether the default C compiler supports ``-no-pie`` in
    an internal settings file.

.. ghc-flag:: -optcxx ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the C++ compiler
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the C++ compiler.

.. ghc-flag:: -optlo ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the LLVM optimiser
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the LLVM optimiser.

.. ghc-flag:: -optlc ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the LLVM compiler
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the LLVM compiler.

.. ghc-flag:: -optlas ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the LLVM assembler
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the LLVM assembler (typically clang).

.. ghc-flag:: -opta ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the assembler
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the assembler.

.. ghc-flag:: -optl ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the linker
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the linker.

.. ghc-flag:: -optlm ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the linker when merging object files.
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the linker when merging object files. In the case of a
    standard ``ld``-style linker this should generally include the ``-r`` flag.

.. ghc-flag:: -optwindres ⟨option⟩
    :shortdesc: pass ⟨option⟩ to ``windres``.
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to ``windres`` when embedding manifests on Windows.
    See ``-fno-embed-manifest`` in :ref:`options-linker`.

.. ghc-flag:: -opti ⟨option⟩
    :shortdesc: pass ⟨option⟩ to the interpreter sub-process.
    :type: dynamic
    :category: phase-options

    Pass ⟨option⟩ to the interpreter sub-process (see
    :ref:`external-interpreter`).  A common use for this is to pass
    RTS options e.g., ``-opti+RTS -opti-A64m``, or to enable verbosity
    with ``-opti-v`` to see what messages are being exchanged by GHC
    and the interpreter.

So, for example, to force an ``-Ewurble`` option to the assembler, you
would tell the driver ``-opta-Ewurble`` (the dash before the E is
required).

GHC is itself a Haskell program, so if you need to pass options directly
to GHC's runtime system you can enclose them in ``+RTS ... -RTS`` (see
:ref:`runtime-control`).

.. _c-pre-processor:

Options affecting the C pre-processor
-------------------------------------

.. extension:: CPP
    :shortdesc: Resolve C preprocessor directives.

    :since: 6.8.1

    The :extension:`CPP` language extension enables the C pre-processor.
    This can be turned into a command-line flag by prefixing it with
    ``-X``; For example:

    .. code-block:: sh

        $ ghc -XCPP foo.hs

    The :extension:`CPP` language extension can also be enabled using
    the :ref:`LANGUAGE <language-pragma>` pragma; For example: ::

        {-# LANGUAGE CPP #-}

.. index::
   single: pre-processing: cpp
   single: C pre-processor options
   single: cpp, pre-processing with

.. ghc-flag:: -cpp
    :shortdesc: Run the C pre-processor on Haskell source files
    :type: dynamic
    :category: cpp

    The C pre-processor :command:`cpp` is run over your Haskell code if
    the :ghc-flag:`-cpp` option or :extension:`CPP` extension are given. Unless
    you are building a large system with significant doses of conditional
    compilation, you really shouldn't need it.

.. ghc-flag:: -D⟨symbol⟩[=⟨value⟩]
    :shortdesc: Define a symbol in the C pre-processor
    :type: dynamic
    :reverse: -U⟨symbol⟩
    :category: cpp

    Define macro ⟨symbol⟩ in the usual way. When no value is given, the value is
    taken to be ``1``. For instance, ``-DUSE_MYLIB`` is equivalent to
    ``-DUSE_MYLIB=1``.

    .. note::

        :ghc-flag:`-D⟨symbol⟩[=⟨value⟩]` does *not* affect ``-D``
        macros passed to the C compiler when compiling an unregisterised build! In
        this case use the ``-optc-Dfoo`` hack… (see :ref:`forcing-options-through`).

.. ghc-flag:: -U⟨symbol⟩
    :shortdesc: Undefine a symbol in the C pre-processor
    :type: dynamic
    :category: cpp

    Undefine macro ⟨symbol⟩ in the usual way.

.. ghc-flag:: -I⟨dir⟩
    :shortdesc: Add ⟨dir⟩ to the directory search list for ``#include`` files
    :type: dynamic
    :category: cpp

    Specify a directory in which to look for ``#include`` files, in the
    usual C way.

The GHC driver pre-defines several macros when processing Haskell source
code (``.hs`` or ``.lhs`` files).

.. _standard-cpp-macros:

Standard CPP macros
~~~~~~~~~~~~~~~~~~~

The symbols defined by GHC are listed below. To check which symbols are
defined by your local GHC installation, the following trick is useful:

.. code-block:: sh

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

``__GLASGOW_HASKELL_FULL_VERSION__``
    .. index::
       single: __GLASGOW_HASKELL_FULL_VERSION__

    This macro exposes the full version string.
    For instance: ``__GLASGOW_HASKELL_FULL_VERSION__==8.11.0.20200319``.
    Its value comes from the ``ProjectVersion`` Autotools variable.

    Added in GHC 9.0.1

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

    .. code-block:: c

        #if defined(MIN_VERSION_GLASGOW_HASKELL)
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

    This is set to ``1`` when the compiler supports Template Haskell,
    and to ``0`` when not. The latter is the case for a stage-1
    compiler during bootstrapping, or on architectures where the
    interpreter is not available.

``__GLASGOW_HASKELL_LLVM__``
    .. index::
       single: __GLASGOW_HASKELL_LLVM__

    Only defined when `:ghc-flag:`-fllvm` is specified. When GHC is using version
    ``x.y.z`` of LLVM, the value of ``__GLASGOW_HASKELL_LLVM__`` is the
    integer ⟨xyy⟩ (if ⟨y⟩ is a single digit, then a leading zero
    is added, so for example when using version 3.7 of LLVM,
    ``__GLASGOW_HASKELL_LLVM__==307``).

``__GLASGOW_HASKELL_ASSERTS_IGNORED__``
    .. index::
       single: __GLASGOW_HASKELL_ASSERTS_IGNORED__

    Only defined when :ghc-flag:`-fignore-asserts` is specified.
    This can be used to create your own assertions, see :ref:`assertions`

``os_HOST_OS=1``
    This define allows conditional compilation based on the Operating
    System, where⟨os⟩ is the name of the current Operating System (eg.
    ``linux``, ``mingw32`` for Windows, ``solaris``, etc.).

``arch_HOST_ARCH=1``
    This define allows conditional compilation based on the host
    architecture, where⟨arch⟩ is the name of the current architecture
    (eg. ``i386``, ``x86_64``, ``aarch64``, ``powerpc``, ``sparc``, etc.).

``VERSION_pkgname``
    This macro is available starting GHC 8.0.  It is defined for every
    exposed package. This macro expands to a string recording the
    version of ``pkgname`` that is exposed for module import.
    It is identical in behavior to the ``VERSION_pkgname`` macros
    that Cabal defines.

``MIN_VERSION_pkgname(x,y,z)``
    This macro is available starting GHC 8.0.  It is defined for every
    exposed package. This macro is provided for convenience to write CPP
    conditionals testing if a package version is ``x.y.z`` or
    later.  It is identical in behavior to the ``MIN_VERSION_pkgname``
    macros that Cabal defines.

SIMD macros
    .. index::
        single: SIMD Macros

    These are defined conditionally based on the SIMD
    flags used for compilation:

    ``__SSE__``, ``__SSE2__``, ``__SSE4_2__``, ``__FMA__``,
    ``__AVX__``, ``__AVX2__``, ``__AVX512CD__``, ``__AVX512ER__``, ``__AVX512F__``, ``__AVX512PF__``,

.. _cpp-string-gaps:

CPP and string gaps
~~~~~~~~~~~~~~~~~~~

.. index::
   single: -cpp vs string gaps
   single: string gaps vs -cpp.

A small word of warning: :ghc-flag:`-cpp` is not friendly to "string gaps".
In other words, strings such as the following: ::

    strmod = "\
    \ p \
    \ "

don't work with :ghc-flag:`-cpp`; :command:`/usr/bin/cpp` elides the backslash-newline
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

.. ghc-flag:: -F
    :shortdesc: Enable the use of a :ref:`pre-processor <pre-processor>`
        (set with :ghc-flag:`-pgmF ⟨cmd⟩`)
    :type: dynamic
    :category: phases

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

    Use :ghc-flag:`-pgmF ⟨cmd⟩` to select the program to use as the
    preprocessor.  When invoked, the ⟨cmd⟩ pre-processor is given at least
    three arguments on its command-line: the first argument is the name of the
    original source file, the second is the name of the file holding the input,
    and the third is the name of the file where ⟨cmd⟩ should write its output
    to.

    Additional arguments to the pre-processor can be passed in using the
    :ghc-flag:`-optF ⟨option⟩` option. These are fed to ⟨cmd⟩ on the command
    line after the three standard input and output arguments.

    An example of a pre-processor is to convert your source files to the
    input encoding that GHC expects, i.e. create a script ``convert.sh``
    containing the lines:

    .. code-block:: sh

        #!/bin/sh
        ( echo "{-# LINE 1 \"$1\" #-}" ; iconv -f l1 -t utf-8 $2 ) > $3

    and pass ``-F -pgmF convert.sh`` to GHC. The ``-f l1`` option tells
    iconv to convert your Latin-1 file, supplied in argument ``$2``,
    while the "-t utf-8" options tell iconv to return a UTF-8 encoded
    file. The result is redirected into argument ``$3``. The
    ``echo "{-# LINE 1 \"$1\" #-}"`` just makes sure that your error
    positions are reported as in the original source file.

.. _options-codegen:

Options affecting code generation
---------------------------------

.. ghc-flag:: -fasm
    :shortdesc: Use the :ref:`native code generator <native-code-gen>`
    :type: dynamic
    :reverse: -fllvm
    :category: codegen

    Use GHC's :ref:`native code generator <native-code-gen>` rather than
    compiling via LLVM. ``-fasm`` is the default.

.. ghc-flag:: -fllvm
    :shortdesc: Compile using the :ref:`LLVM code generator <llvm-code-gen>`
    :type: dynamic
    :reverse: -fasm
    :category: codegen

    Compile via :ref:`LLVM <llvm-code-gen>` instead of using the native
    code generator. This will generally take slightly longer than the
    native code generator to compile. Produced code is generally the
    same speed or faster than the other two code generators. Compiling
    via LLVM requires LLVM's :command:`opt` and :command:`llc` executables to be
    in :envvar:`PATH`.

    .. note::

        Note that this GHC release expects an LLVM version between |llvm-version-min|
        and |llvm-version-max|.

.. ghc-flag:: -fno-code
    :shortdesc: Omit code generation
    :type: dynamic
    :category: codegen

    Omit code generation (and all later phases) altogether. This is
    useful if you're only interested in type checking code.

    If a module contains a Template Haskell splice then in ``--make`` mode, code
    generation will be automatically turned on for all dependencies. By default,
    object files are generated, but if ghc-flag:`-fprefer-byte-code` is enabled,
    byte-code will be generated instead.

.. ghc-flag:: -fwrite-interface
    :shortdesc: Always write interface files
    :type: dynamic
    :category: codegen

    Always write interface files. GHC will normally write interface
    files automatically, but this flag is useful with :ghc-flag:`-fno-code`,
    which normally suppresses generation of interface files. This is
    useful if you want to type check over multiple runs of GHC without
    compiling dependencies.

.. ghc-flag:: -fwrite-if-simplified-core
    :shortdesc: Write an interface file containing the simplified core of the module.
    :type: dynamic
    :category: codegen

    The interface file will contain all the bindings for a module. From
    this interface file we can restart code generation to produce byte-code.

    The definition of bindings which are included in this
    depend on the optimisation level. Any definitions which are already included in
    an interface file (via an unfolding for an exported identifier) are reused.


.. ghc-flag:: -fobject-code
    :shortdesc: Generate object code
    :type: dynamic
    :category: codegen

    Generate object code. This is the default outside of GHCi, and can
    be used with GHCi to cause object code to be generated in preference
    to byte-code. Therefore this flag disables :ghc-flag:`-fbyte-code-and-object-code`.

.. ghc-flag:: -fbyte-code
    :shortdesc: Generate byte-code
    :type: dynamic
    :category: codegen

    Generate byte-code instead of object-code. This is the default in
    GHCi. Byte-code can currently only be used in the interactive
    interpreter, not saved to disk. This option is only useful for
    reversing the effect of :ghc-flag:`-fobject-code`.

.. ghc-flag:: -fbyte-code-and-object-code
    :shortdesc: Generate object code and byte-code
    :type: dynamic
    :category: codegen

    Generate object code and byte-code. This is useful with the flags
    :ghc-flag:`-fprefer-byte-code` and :ghc-flag:`-fwrite-if-simplified-core`.

    This flag implies :ghc-flag:`-fwrite-if-simplified-core`.

    :ghc-flag:`-fbyte-code` and :ghc-flag:`-fobject-code` disable this flag as
    they specify that GHC should *only* write object code or byte-code respectively.

.. ghc-flag:: -fPIC
    :shortdesc: Generate position-independent code (where available)
    :type: dynamic
    :category: codegen

    Generate position-independent code (code that can be put into shared
    libraries). This currently works on Linux x86 and x86-64. On
    Windows, position-independent code is never used so the flag is a
    no-op on that platform.

.. ghc-flag:: -fexternal-dynamic-refs
    :shortdesc: Generate code for linking against dynamic libraries
    :type: dynamic
    :category: codegen

    When generating code, assume that entities imported from a
    different module might be dynamically linked.  This flag is enabled
    automatically by :ghc-flag:`-dynamic`.

.. ghc-flag:: -fPIE
    :shortdesc: Generate code for a position-independent executable (where available)
    :type: dynamic
    :category: codegen

    Generate code in such a way to be linkable into a position-independent
    executable This currently works on Linux x86 and x86-64. On Windows,
    position-independent code is never used so the flag is a no-op on that
    platform. To link the final executable use :ghc-flag:`-pie`.

.. ghc-flag:: -dynamic
    :shortdesc: Build dynamically-linked object files and executables
    :type: dynamic
    :category: codegen
    :noindex:

    Build code for dynamic linking.  This can reduce code size
    tremendously, but may slow-down cross-module calls of non-inlined
    functions. There can be some complications combining
    :ghc-flag:`-shared` with this flag relating to linking in the RTS
    under Linux. See :ghc-ticket:`10352`.

    Note that using this option when linking causes GHC to link against
    shared libraries.

.. ghc-flag:: -dynamic-too
    :shortdesc: Build dynamic object files *as well as* static object files
        during compilation
    :type: dynamic
    :category: codegen

    Generates both dynamic and static object files in a single run of
    GHC. This option is functionally equivalent to running GHC twice,
    the second time adding ``-dynamic -osuf dyn_o -hisuf dyn_hi``.

    Although it is equivalent to running GHC twice, using
    ``-dynamic-too`` is more efficient, because the earlier phases of
    the compiler up to code generation are performed just once.

    When using ``-dynamic-too``, the options ``-dyno``, ``-dynosuf``,
    and ``-dynhisuf`` are the counterparts of ``-o``, ``-osuf``, and
    ``-hisuf`` respectively, but applying to the dynamic compilation.

    ``-dynamic-too`` is ignored if :ghc-flag:`-dynamic` is also specified.

.. ghc-flag:: -fexpose-internal-symbols
    :shortdesc: Produce symbols for all functions, including internal functions.
    :type: dynamic
    :category: codegen

    Request that GHC emits verbose symbol tables which include local symbols
    for module-internal functions. These can be useful for tools like
    `perf <https://perf.wiki.kernel.org/>`__ but increase object file sizes.
    This is implied by :ghc-flag:`-g2 <-g>` and above.

    :ghc-flag:`-fno-expose-internal-symbols <-fexpose-internal-symbols>`
    suppresses all non-global symbol table entries, resulting in smaller object
    file sizes at the expense of debuggability.


.. ghc-flag:: -fprefer-byte-code
    :shortdesc: Use byte-code if it is available to evaluate TH splices
    :type: dynamic
    :category: codegen

    If a home package module has byte-code available then use that instead of
    an object file (if that's available) to evaluate and run TH splices.

    This is useful with flags such as :ghc-flag:`-fbyte-code-and-object-code`, which
    tells the compiler to generate byte-code, and :ghc-flag:`-fwrite-if-simplified-core` which
    allows byte-code to be generated from an interface file.

    This flag also interacts with :ghc-flag:`-fno-code`, if this flag is enabled
    then any modules which are required to be compiled for Template Haskell evaluation
    will generate byte-code rather than object code.


.. _options-linker:

Options affecting linking
-------------------------

.. index::
   single: linker options
   single: ld options

GHC has to link your code with various libraries, possibly including:
user-supplied, GHC-supplied, and system-supplied (``-lm`` math library,
for example).

.. ghc-flag:: -l ⟨lib⟩
    :shortdesc: Link in library ⟨lib⟩
    :type: dynamic
    :category: linking

    Link in the ⟨lib⟩ library. On Unix systems, this will be in a file
    called :file:`lib{lib}.a` or :file:`lib{lib}.so` which resides somewhere on the
    library directories path.

    Because of the sad state of most UNIX linkers, the order of such
    options does matter. If library ⟨foo⟩ requires library ⟨bar⟩, then
    in general ``-l ⟨foo⟩`` should come *before* ``-l ⟨bar⟩`` on the
    command line.

    There's one other gotcha to bear in mind when using external
    libraries: if the library contains a ``main()`` function, then this
    will be a link conflict with GHC's own ``main()`` function (eg.
    ``libf2c`` and ``libl`` have their own ``main()``\ s).

    You can use an external main function if you initialize the RTS manually
    and pass ``-no-hs-main``. See also :ref:`using-own-main`.

.. ghc-flag:: -c
    :shortdesc: Stop after generating object (``.o``) file
    :type: mode
    :category: linking

    Omits the link step. This option can be used with :ghc-flag:`--make` to
    avoid the automatic linking that takes place if the program contains
    a ``Main`` module.

.. ghc-flag:: -package ⟨name⟩
    :shortdesc: Expose package ⟨pkg⟩
    :type: dynamic
    :category: linking

    If you are using a Haskell "package" (see :ref:`packages`), don't
    forget to add the relevant ``-package`` option when linking the
    program too: it will cause the appropriate libraries to be linked in
    with the program. Forgetting the ``-package`` option will likely
    result in several pages of link errors.

.. ghc-flag:: -framework ⟨name⟩
    :shortdesc: On Darwin/OS X/iOS only, link in the framework ⟨name⟩. This
        option corresponds to the ``-framework`` option for Apple's Linker.
    :type: dynamic
    :category: linking

    On Darwin/OS X/iOS only, link in the framework ⟨name⟩. This option
    corresponds to the ``-framework`` option for Apple's Linker. Please
    note that frameworks and packages are two different things -
    frameworks don't contain any Haskell code. Rather, they are Apple's
    way of packaging shared libraries. To link to Apple's "Carbon" API,
    for example, you'd use ``-framework Carbon``.

.. ghc-flag:: -staticlib
    :shortdesc: Generate a standalone static library (as opposed to an
        executable). This is useful when cross compiling. The
        library together with all its dependencies ends up in in a
        single static library that can be linked against.
    :type: dynamic
    :category: linking

    :implies: :ghc-flag:`-flink-rts`

    Link all passed files into a static library suitable for linking.
    To control the name, use the :ghc-flag:`-o ⟨file⟩` option
    as usual. The default name is ``liba.a``.

.. ghc-flag:: -L ⟨dir⟩
    :shortdesc: Add ⟨dir⟩ to the list of directories searched for libraries
    :type: dynamic
    :category: linking

    Where to find user-supplied libraries… Prepend the directory ⟨dir⟩
    to the library directories path.

.. ghc-flag:: -fuse-rpaths
    :shortdesc: Set the rpath based on -L flags
    :type: dynamic
    :category: linking

    This flag is enabled by default and will set the rpath of the linked
    object to the library directories of dependent packages.

    When building binaries to distribute it can be useful to pass your own
    linker options to control the rpath and disable the automatic injection of
    rpath entries by disabling this flag.

.. ghc-flag:: -framework-path ⟨dir⟩
    :shortdesc: On Darwin/OS X/iOS only, add ⟨dir⟩ to the list of directories
        searched for frameworks. This option corresponds to the ``-F``
        option for Apple's Linker.
    :type: dynamic
    :category: linking

    On Darwin/OS X/iOS only, prepend the directory ⟨dir⟩ to the
    framework directories path. This option corresponds to the ``-F``
    option for Apple's Linker (``-F`` already means something else for
    GHC).

.. ghc-flag:: -fsplit-sections
              -split-sections
    :shortdesc: Split sections for link-time dead-code stripping
    :type: dynamic
    :category: linking
    :reverse: -fno-split-sections

    Place each generated function or data item into its own section in the
    output file if the target supports arbitrary sections. The name of the
    function or the name of the data item determines the section's name in the
    output file.

    When linking, the linker can automatically remove all unreferenced sections
    and thus produce smaller executables.

.. ghc-flag:: -static
    :shortdesc: Use static Haskell libraries
    :type: dynamic
    :category: linking

    Tell the linker to avoid shared Haskell libraries, if possible. This
    is the default.

.. ghc-flag:: -dynamic
    :shortdesc: Build dynamically-linked object files and executables
    :type: dynamic
    :category: linking

    This flag tells GHC to link against shared Haskell libraries. This
    flag only affects the selection of dependent libraries, not the form
    of the current target (see :ghc-flag:`-shared`).
    See :ref:`using-shared-libs` on how to create them.

    Note that this option also has an effect on code generation (see
    above).

.. ghc-flag:: -shared
    :shortdesc: Generate a shared library (as opposed to an executable)
    :type: dynamic
    :category: linking

    Instead of creating an executable, GHC produces a shared object with
    this linker flag. Depending on the operating system target, this
    might be an ELF DSO, a Windows DLL, or a Mac OS dylib. GHC hides the
    operating system details beneath this uniform flag.

    The flags :ghc-flag:`-dynamic` and :ghc-flag:`-static` control whether the
    resulting shared object links statically or dynamically to Haskell package
    libraries given as :ghc-flag:`-package ⟨pkg⟩` option. Non-Haskell libraries
    are linked as gcc would regularly link it on your system, e.g. on most ELF
    system the linker uses the dynamic libraries when found.

    Object files linked into shared objects must be compiled with
    :ghc-flag:`-fPIC`, see :ref:`options-codegen`

    When creating shared objects for Haskell packages, the shared object
    must be named properly, so that GHC recognizes the shared object
    when linking against this package.
    See :ref:`shared object name mangling <building-packages>` for details.

.. ghc-flag:: -dynload
    :shortdesc: Selects one of a number of modes for finding shared libraries at runtime.
    :type: dynamic
    :category: linking

    This flag selects one of a number of modes for finding shared
    libraries at runtime. See :ref:`finding-shared-libs` for a
    description of each mode.

.. ghc-flag:: -flink-rts
    :shortdesc: Link the runtime when generating a shared or static library
    :type: dynamic
    :category: linking

    When linking shared libraries (:ghc-flag:`-shared`) GHC does not
    automatically link the RTS.  This is to allow choosing the RTS flavour
    (:ghc-flag:`-threaded`, :ghc-flag:`-eventlog`, etc) when linking an
    executable.
    However when the shared library is the intended product it is useful to be
    able to reverse this default. See :ref:`shared-libraries-c-api` for an
    usage example.

    When linking a static library (:ghc-flag:`-staticlib`) GHC links the RTS
    automatically, you can reverse this behaviour by reversing this flag:
    ``-fno-link-rts``.

.. ghc-flag:: -main-is ⟨thing⟩
    :shortdesc: Set main module and function
    :type: dynamic
    :category: linking

    .. index::
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
    :ghc-flag:`-fforce-recomp` flag.

.. ghc-flag:: -no-hs-main
    :shortdesc: Don't assume this program contains ``main``
    :type: dynamic
    :category: linking

    .. index::
       single: linking Haskell libraries with foreign code

    In the event you want to include ghc-compiled code as part of
    another (non-Haskell) program, the RTS will not be supplying its
    definition of ``main()`` at link-time, you will have to. To signal
    that to the compiler when linking, use ``-no-hs-main``. See also
    :ref:`using-own-main`.

    Notice that since the command-line passed to the linker is rather
    involved, you probably want to use ``ghc`` to do the final link of
    your \`mixed-language' application. This is not a requirement
    though, just try linking once with :ghc-flag:`-v` on to see what options the
    driver passes through to the linker.

    The ``-no-hs-main`` flag can also be used to persuade the compiler
    to do the link step in :ghc-flag:`--make` mode when there is no Haskell
    ``Main`` module present (normally the compiler will not attempt
    linking when there is no ``Main``).

    The flags :ghc-flag:`-rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]` and
    :ghc-flag:`-with-rtsopts=⟨opts⟩` have no effect when used with
    :ghc-flag:`-no-hs-main`, because they are implemented by changing the
    definition of ``main`` that GHC generates. See :ref:`using-own-main` for
    how to get the effect of
    :ghc-flag:`-rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]` and
    :ghc-flag:`-with-rtsopts=⟨opts⟩` when using your own ``main``.

.. ghc-flag:: -debug
    :shortdesc: Use the debugging runtime
    :type: dynamic
    :category: linking

    Link the program with a debugging version of the runtime system. The
    debugging runtime turns on numerous assertions and sanity checks,
    and provides extra options for producing debugging output at runtime
    (run the program with ``+RTS -?`` to see a list).

.. ghc-flag:: -threaded
    :shortdesc: Use the threaded runtime
    :type: dynamic
    :category: linking
    :reverse: -single-threaded

    Link the program with the "threaded" version of the runtime system.
    The threaded runtime system is so-called because it manages multiple
    OS threads, as opposed to the default runtime system which is purely
    single-threaded.

    Note that you do *not* need ``-threaded`` in order to use
    concurrency; the single-threaded runtime supports concurrency
    between Haskell threads just fine.

    The threaded runtime system provides the following benefits:

    -  It enables the :rts-flag:`-N ⟨x⟩` RTS option to be used,
       which allows threads to run in parallel on a multiprocessor
       or multicore machine. See :ref:`using-smp`.

    -  If a thread makes a foreign call (and the call is not marked
       ``unsafe``), then other Haskell threads in the program will
       continue to run while the foreign call is in progress.
       Additionally, ``foreign export``\ ed Haskell functions may be
       called from multiple OS threads simultaneously. See
       :ref:`ffi-threads`.

.. ghc-flag:: -single-threaded
    :shortdesc: Use the single-threaded runtime
    :type: dynamic
    :category: linking
    :reverse: -threaded

    :since: 9.8

    Switch to the single threaded (default) version of the runtime.

.. ghc-flag:: -eventlog
    :shortdesc: Enable runtime event tracing
    :type: dynamic
    :category: linking

    :since: Unconditionally enabled with 9.4 and later

    Link the program with the "eventlog" version of the runtime system.
    A program linked in this way can generate a runtime trace of events
    (such as thread start/stop) to a binary file :file:`{program}.eventlog`,
    which can then be interpreted later by various tools. See
    :ref:`rts-eventlog` for more information.

    Note that as of GHC 9.4 and later eventlog support is included in
    the RTS by default and the :ghc-flag:`-eventlog` is deprecated.

.. ghc-flag:: -rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]
    :shortdesc: Control whether the RTS behaviour can be tweaked via command-line
        flags and the ``GHCRTS`` environment variable. Using ``none``
        means no RTS flags can be given; ``some`` means only a minimum
        of safe options can be given (the default, if ``-rtsopts`` is
        not passed); ``all`` means that all RTS flags are permitted (the
        default, if ``-rtsopts`` is passed with no argument); ``ignore``
        means RTS flags can be given, but are treated as regular arguments and
        passed to the Haskell program as arguments; ``ignoreAll`` is the same as
        ``ignore``, but ``GHCRTS`` is also ignored. ``-rtsopts`` does not
        affect ``-with-rtsopts`` behavior; flags passed via ``-with-rtsopts``
        are used regardless of ``-rtsopts``.
    :type: dynamic
    :category: linking

    :default: ``some``, if ``-rtsopts`` is not passed; ``all``, if ``-rtsopts``
        is passed with no argument.

    This option affects the processing of RTS control options given
    either on the command line or via the :envvar:`GHCRTS` environment
    variable. There are six possibilities:

    ``-rtsopts=none``
        Disable all processing of RTS options. If ``+RTS`` appears
        anywhere on the command line, then the program will abort with
        an error message. If the ``GHCRTS`` environment variable is set,
        then the program will emit a warning message, ``GHCRTS`` will be
        ignored, and the program will run as normal.

    ``-rtsopts=ignore``
        Disables all processing of RTS options. Unlike ``none`` this treats
        all RTS flags appearing on the command line the same way as regular
        arguments. (Passing them on to your program as arguments).
        ``GHCRTS`` options will be processed normally.

    ``-rtsopts=ignoreAll``
        Same as ``ignore`` with the exception of ``GHCRTS`` options, which are
        also ignored.

    ``-rtsopts=some``
        [this is the default setting, if ``-rtsopts`` is not passed] Enable only
        the "safe" RTS options: (Currently only ``-?`` and ``--info``.) Any
        other RTS options on the command line or in the ``GHCRTS`` environment
        variable causes the program to abort with an error message.

    ``-rtsopts=all``
        Enable *all* RTS option processing, both on the command line and
        through the ``GHCRTS`` environment variable.

    ``-rtsopts``
        Equivalent to ``-rtsopts=all``.

    In GHC 6.12.3 and earlier, the default was to process all RTS
    options. However, since RTS options can be used to write logging
    data to arbitrary files under the security context of the running
    program, there is a potential security problem. For this reason, GHC
    7.0.1 and later default to ``-rtsopts=some``.

    Note that ``-rtsopts`` has no effect when used with :ghc-flag:`-no-hs-main`;
    see :ref:`using-own-main` for details.

    ``-rtsopts`` does not affect RTS options passed via ``-with-rtsopts``;
    those are used regardless of ``-rtsopts``.

.. ghc-flag:: -with-rtsopts=⟨opts⟩
    :shortdesc: Set the default RTS options to ⟨opts⟩.
    :type: dynamic
    :category: linking

    This option allows you to set the default RTS options at link-time.
    For example, ``-with-rtsopts="-H128m"`` sets the default heap size
    to 128MB. This will always be the default heap size for this
    program, unless the user overrides it. (Depending on the setting of
    the ``-rtsopts`` option, the user might not have the ability to
    change RTS options at run-time, in which case ``-with-rtsopts``
    would be the *only* way to set them.)

    Use the runtime flag :rts-flag:`--info` on the executable program
    to see the options set with ``-with-rtsopts``.

    Note that ``-with-rtsopts`` has no effect when used with
    ``-no-hs-main``; see :ref:`using-own-main` for details.

.. ghc-flag:: -no-rtsopts-suggestions
    :shortdesc: Don't print RTS suggestions about linking with
        :ghc-flag:`-rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]`.
    :type: dynamic
    :category: linking

    This option disables RTS suggestions about linking with
    :ghc-flag:`-rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]` when they are not
    available. These suggestions would be unhelpful if the users have installed
    Haskell programs through their package managers. With this option enabled,
    these suggestions will not appear. It is recommended for people
    distributing binaries to build with either ``-rtsopts`` or
    ``-no-rtsopts-suggestions``.

.. ghc-flag:: -fno-gen-manifest
    :shortdesc: Do not generate a manifest file (Windows only)
    :type: dynamic
    :category: linking

    On Windows, GHC normally generates a manifest file when
    linking a binary. The manifest is placed in the file
    :file:`{prog}.exe.manifest`` where ⟨prog.exe⟩ is the name of the
    executable. The manifest file currently serves just one purpose: it
    disables the "installer detection" in Windows
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

    :ghc-flag:`-fno-gen-manifest` also implies :ghc-flag:`-fno-embed-manifest`, see
    below.

.. ghc-flag:: -fno-embed-manifest
    :shortdesc: Do not embed the manifest in the executable (Windows only)
    :type: dynamic
    :category: linking

    .. index::
       single: windres

    The manifest file that GHC generates when linking a binary on Windows is
    also embedded in the executable itself, by default. This means that the
    binary can be distributed without having to supply the manifest file too.
    The embedding is done by running :command:`windres`; to see exactly what
    GHC does to embed the manifest, use the :ghc-flag:`-v` flag. A GHC
    installation comes with its own copy of ``windres`` for this reason.

    See also :ghc-flag:`-pgmwindres ⟨cmd⟩` (:ref:`replacing-phases`) and
    :ghc-flag:`-optwindres ⟨option⟩` (:ref:`forcing-options-through`).

.. ghc-flag:: -fno-shared-implib
    :shortdesc: Don't generate an import library for a DLL (Windows only)
    :type: dynamic
    :category: linking

    DLLs on Windows are typically linked to by linking to a
    corresponding ``.lib`` or ``.dll.a`` — the so-called import library.
    GHC will typically generate such a file for every DLL you create by
    compiling in :ghc-flag:`-shared` mode. However, sometimes you don't want to
    pay the disk-space cost of creating this import library, which can
    be substantial — it might require as much space as the code itself,
    as Haskell DLLs tend to export lots of symbols.

    As long as you are happy to only be able to link to the DLL using
    ``GetProcAddress`` and friends, you can supply the
    :ghc-flag:`-fno-shared-implib` flag to disable the creation of the import
    library entirely.

.. ghc-flag:: -dylib-install-name ⟨path⟩
    :shortdesc: Set the install name (via ``-install_name`` passed to Apple's
        linker), specifying the full install path of the library file.
        Any libraries or executables that link with it later will pick
        up that path as their runtime search location for it.
        (Darwin/OS X only)
    :type: dynamic
    :category: linking

    On Darwin/OS X, dynamic libraries are stamped at build time with an
    "install name", which is the ultimate install path of the library
    file. Any libraries or executables that subsequently link against it
    will pick up that path as their runtime search location for it. By
    default, ghc sets the install name to the location where the library
    is built. This option allows you to override it with the specified
    file path. (It passes ``-install_name`` to Apple's linker.) Ignored
    on other platforms.

.. ghc-flag:: -rdynamic
    :shortdesc: This instructs the linker to add all symbols, not only used
        ones, to the dynamic symbol table. Currently Linux and
        Windows/MinGW32 only. This is equivalent to using
        ``-optl -rdynamic`` on Linux, and ``-optl -export-all-symbols``
        on Windows.
    :type: dynamic
    :category: linking

    This instructs the linker to add all symbols, not only used ones, to
    the dynamic symbol table. Currently Linux and Windows/MinGW32 only.
    This is equivalent to using ``-optl -rdynamic`` on Linux, and
    ``-optl -export-all-symbols`` on Windows.

.. ghc-flag:: -fwhole-archive-hs-libs
    :shortdesc: When linking a binary executable, this inserts the flag
        ``-Wl,--whole-archive`` before any ``-l`` flags for Haskell
        libraries, and ``-Wl,--no-whole-archive`` afterwards
    :type: dynamic
    :category: linking

    When linking a binary executable, this inserts the flag
    ``-Wl,--whole-archive`` before any ``-l`` flags for Haskell
    libraries, and ``-Wl,--no-whole-archive`` afterwards (on OS X, the
    flag is ``-Wl,-all_load``, there is no equivalent for
    ``-Wl,--no-whole-archive``).  This flag also disables the use of
    ``-Wl,--gc-sections`` (``-Wl,-dead_strip`` on OS X).

    This is for specialist applications that may require symbols
    defined in these Haskell libraries at runtime even though they
    aren't referenced by any other code linked into the executable.
    If you're using ``-fwhole-archive-hs-libs``, you probably also
    want ``-rdynamic``.

.. ghc-flag:: -pie
    :shortdesc: Instruct the linker to produce a position-independent executable.
    :type: dynamic
    :reverse: -no-pie
    :category: linking

    :since: 8.2.2

    This instructs the linker to produce a position-independent executable.
    This flag is only valid while producing executables and all object code
    being linked must have been produced with :ghc-flag:`-fPIE`.

    Position independent executables are required by some platforms as they
    enable address-space layout randomization (ASLR), a common security measure.
    They can also be useful as they can be dynamically loaded and used as shared
    libraries by other executables.

    Position independent executables should be dynamically-linked (e.g. built
    with :ghc-flag:`-dynamic` and only loaded into other dynamically-linked
    executables to ensure that only one ``libHSrts`` is present if
    loaded into the address space of another Haskell process.

    Also, you may need to use the :ghc-flag:`-rdynamic` flag to ensure that
    that symbols are not dropped from your PIE objects.

.. ghc-flag:: -no-pie
    :shortdesc: Don't instruct the linker to produce a position-independent executable.
    :type: dynamic
    :reverse: -pie
    :category: linking

    If required, the C compiler will still produce a PIE. Otherwise, this is the default.
    Refer to -pie for more information about PIEs.

.. ghc-flag:: -fkeep-cafs
    :shortdesc: Do not garbage-collect CAFs (top-level expressions) at runtime
    :type: dynamic
    :category: linking

    :since: 8.8.1

    Disables the RTS's normal behaviour of garbage-collecting CAFs
    (Constant Applicative Forms, in other words top-level
    expressions). This option is useful for specialised applications
    that do runtime dynamic linking, where code dynamically linked in
    the future might require the value of a CAF that would otherwise
    be garbage-collected.

.. ghc-flag:: -fcompact-unwind
    :shortdesc: Instruct the linker to produce a `__compact_unwind` section.
    :type: dynamic
    :category: linking

    :default: on

    :since: 9.4.1

    This instructs the linker to produce an executable that supports Apple's
    compact unwinding sections. These are used by C++ and Objective-C code
    to unwind the stack when an exception occurs.

    In theory, the older `__eh_frame` section should also be usable for this
    purpose, but this does not always work.
