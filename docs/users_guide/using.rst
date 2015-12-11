.. _using-ghc:

Using GHC
=========

.. index::
   single: GHC, using
   single: using GHC

Getting started: compiling programs
-----------------------------------

In this chapter you'll find a complete reference to the GHC command-line
syntax, including all 400+ flags. It's a large and complex system, and
there are lots of details, so it can be quite hard to figure out how to
get started. With that in mind, this introductory section provides a
quick introduction to the basic usage of GHC for compiling a Haskell
program, before the following sections dive into the full syntax.

Let's create a Hello World program, and compile and run it. First,
create a file ``hello.hs`` containing the Haskell code:

::

    main = putStrLn "Hello, World!"

To compile the program, use GHC like this:

::

    $ ghc hello.hs

(where ``$`` represents the prompt: don't type it). GHC will compile the
source file ``hello.hs``, producing an object file ``hello.o`` and an
interface file ``hello.hi``, and then it will link the object file to
the libraries that come with GHC to produce an executable called
``hello`` on Unix/Linux/Mac, or ``hello.exe`` on Windows.

By default GHC will be very quiet about what it is doing, only printing
error messages. If you want to see in more detail what's going on behind
the scenes, add ``-v`` to the command line.

Then we can run the program like this:

::

    $ ./hello
    Hello World!

If your program contains multiple modules, then you only need to tell
GHC the name of the source file containing the ``Main`` module, and GHC
will examine the ``import`` declarations to find the other modules that
make up the program and find their source files. This means that, with
the exception of the ``Main`` module, every source file should be named
after the module name that it contains (with dots replaced by directory
separators). For example, the module ``Data.Person`` would be in the
file ``Data/Person.hs`` on Unix/Linux/Mac, or ``Data\Person.hs`` on
Windows.

Options overview
----------------

GHC's behaviour is controlled by options, which for historical reasons
are also sometimes referred to as command-line flags or arguments.
Options can be specified in three ways:

Command-line arguments
~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: structure, command-line
   single: command-line; arguments
   single: arguments; command-line

An invocation of GHC takes the following form:

::

    ghc [argument...]

Command-line arguments are either options or file names.

Command-line options begin with ``-``. They may *not* be grouped:
``-vO`` is different from ``-v -O``. Options need not precede filenames:
e.g., ``ghc *.o -o foo``. All options are processed and then applied to
all files; you cannot, for example, invoke
``ghc -c -O1 Foo.hs -O2 Bar.hs`` to apply different optimisation levels
to the files ``Foo.hs`` and ``Bar.hs``.

.. _source-file-options:

Command line options in source files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: source-file options

Sometimes it is useful to make the connection between a source file and
the command-line options it requires quite tight. For instance, if a
Haskell source file deliberately uses name shadowing, it should be
compiled with the ``-fno-warn-name-shadowing`` option. Rather than
maintaining the list of per-file options in a ``Makefile``, it is
possible to do this directly in the source file using the
``OPTIONS_GHC`` :ref:`pragma <options-pragma>`.

::

    {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
    module X where
    ...

``OPTIONS_GHC`` is a *file-header pragma* (see :ref:`options-pragma`).

Only *dynamic* flags can be used in an ``OPTIONS_GHC`` pragma (see
:ref:`static-dynamic-flags`).

Note that your command shell does not get to the source file options,
they are just included literally in the array of command-line arguments
the compiler maintains internally, so you'll be desperately disappointed
if you try to glob etc. inside ``OPTIONS_GHC``.

.. note::
   The contents of ``OPTIONS_GHC`` are appended to the command-line
   options, so options given in the source file override those given on the
   command-line.

It is not recommended to move all the contents of your Makefiles into
your source files, but in some circumstances, the ``OPTIONS_GHC`` pragma
is the Right Thing. (If you use ``-keep-hc-file`` and have ``OPTION`` flags in
your module, the ``OPTIONS_GHC`` will get put into the generated ``.hc`` file).

Setting options in GHCi
~~~~~~~~~~~~~~~~~~~~~~~

Options may also be modified from within GHCi, using the ``:set``
command. See :ref:`ghci-set` for more details.

.. _static-dynamic-flags:

Static, Dynamic, and Mode options
---------------------------------

.. index::
   single: static; options
   single: dynamic; options
   single: mode; options

Each of GHC's command line options is classified as static, dynamic or
mode:

    For example, ``--make`` or ``-E``. There may only be a single mode
    flag on the command line. The available modes are listed in
    :ref:`modes`.

    Most non-mode flags fall into this category. A dynamic flag may be
    used on the command line, in a ``OPTIONS_GHC`` pragma in a source
    file, or set using ``:set`` in GHCi.

    A few flags are "static", which means they can only be used on the
    command-line, and remain in force over the entire GHC/GHCi run.

The flag reference tables (:ref:`flag-reference`) lists the status of
each flag.

There are a few flags that are static except that they can also be used
with GHCi's ``:set`` command; these are listed as “static/\ ``:set``\ ”
in the table.

.. _file-suffixes:

Meaningful file suffixes
------------------------

.. index::
   single: suffixes, file
   single: file suffixes for GHC

File names with "meaningful" suffixes (e.g., ``.lhs`` or ``.o``) cause
the "right thing" to happen to those files.

``.hs``
    A Haskell module.

``.lhs``
    .. index::
       single: lhs file extension

    A “literate Haskell” module.

``.hspp``
    A file created by the preprocessor.

``.hi``
    A Haskell interface file, probably compiler-generated.

``.hc``
    Intermediate C file produced by the Haskell compiler.

``.c``
    A C file not produced by the Haskell compiler.

``.ll``
    An llvm-intermediate-language source file, usually produced by the
    compiler.

``.bc``
    An llvm-intermediate-language bitcode file, usually produced by the
    compiler.

``.s``
    An assembly-language source file, usually produced by the compiler.

``.o``
    An object file, produced by an assembler.

Files with other suffixes (or without suffixes) are passed straight to
the linker.

.. _modes:

Modes of operation
------------------

.. index::
   single: help options

GHC's behaviour is firstly controlled by a mode flag. Only one of these
flags may be given, but it does not necessarily need to be the first
option on the command-line.

If no mode flag is present, then GHC will enter make mode
(:ref:`make-mode`) if there are any Haskell source files given on the
command line, or else it will link the objects named on the command line
to produce an executable.

The available mode flags are:

``ghc --interactive``
    .. index::
       single: interactive mode
       single: GHCi

    Interactive mode, which is also available as ``ghci``. Interactive
    mode is described in more detail in :ref:`ghci`.

``ghc --make``
    .. index::
       single: make mode; of GHC
       single: --make

    In this mode, GHC will build a multi-module Haskell program
    automatically, figuring out dependencies for itself. If you have a
    straightforward Haskell program, this is likely to be much easier,
    and faster, than using ``make``. Make mode is described in
    :ref:`make-mode`.

    This mode is the default if there are any Haskell source files
    mentioned on the command line, and in this case the ``--make``
    option can be omitted.

``ghc -e`` ⟨expr⟩
    .. index::
       single: eval mode; of GHC

    Expression-evaluation mode. This is very similar to interactive
    mode, except that there is a single expression to evaluate (⟨expr⟩)
    which is given on the command line. See :ref:`eval-mode` for more
    details.

``ghc -E`` ``ghc -C`` ``ghc -S`` ``ghc -c``
    .. index::
       single: -E; GHC option
       single: -C; GHC option
       single: -S; GHC option
       single: -c; GHC option

    This is the traditional batch-compiler mode, in which GHC can
    compile source files one at a time, or link objects together into an
    executable. See :ref:`options-order`.

``ghc -M``
    .. index::
       single: dependency-generation mode; of GHC

    Dependency-generation mode. In this mode, GHC can be used to
    generate dependency information suitable for use in a ``Makefile``.
    See :ref:`makefile-dependencies`.

``ghc --mk-dll``
    .. index::
       single: DLL-creation mode

    DLL-creation mode (Windows only). See :ref:`win32-dlls-create`.

``ghc --help``, ``ghc -?``
    .. index::
       single: --help; GHC option

    Cause GHC to spew a long usage message to standard output and then
    exit.

``ghc --show-iface ⟨file⟩``
    .. index::
       single: --show-iface; GHC option

    Read the interface in ⟨file⟩ and dump it as text to ``stdout``. For
    example ``ghc --show-iface M.hi``.

``ghc --supported-extensions``, ``ghc --supported-languages``
    .. index::
       single: --supported-extensions; GHC option
       single: --supported-languages; GHC option

    Print the supported language extensions.

``ghc --show-options``
    .. index::
       single: --show-options; GHC option

    Print the supported command line options. This flag can be used for
    autocompletion in a shell.

``ghc --info``
    .. index::
       single: --info

    Print information about the compiler.

``ghc --version``, ``ghc -V``
    .. index::
       single: -V
       single: --version

    Print a one-line string including GHC's version number.

``ghc --numeric-version``
    .. index::
       single: --numeric-version

    Print GHC's numeric version number only.

``ghc --print-libdir``
    .. index::
       single: --print-libdir
       single: libdir

    Print the path to GHC's library directory. This is the top of the
    directory tree containing GHC's libraries, interfaces, and include
    files (usually something like ``/usr/local/lib/ghc-5.04`` on Unix).
    This is the value of ``$libdir`` in the package
    configuration file (see :ref:`packages`).

.. _make-mode:

Using ``ghc`` ``--make``
~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: --make; mode of GHC
   single: separate compilation

In this mode, GHC will build a multi-module Haskell program by following
dependencies from one or more root modules (usually just ``Main``). For
example, if your ``Main`` module is in a file called ``Main.hs``, you
could compile and link the program like this:

::

    ghc --make Main.hs

In fact, GHC enters make mode automatically if there are any Haskell
source files on the command line and no other mode is specified, so in
this case we could just type

::

    ghc Main.hs

Any number of source file names or module names may be specified; GHC
will figure out all the modules in the program by following the imports
from these initial modules. It will then attempt to compile each module
which is out of date, and finally, if there is a ``Main`` module, the
program will also be linked into an executable.

The main advantages to using ``ghc --make`` over traditional
``Makefile``\s are:

-  GHC doesn't have to be restarted for each compilation, which means it
   can cache information between compilations. Compiling a multi-module
   program with ``ghc --make`` can be up to twice as fast as running
   ``ghc`` individually on each source file.

-  You don't have to write a ``Makefile``.

   .. index::
      single: Makefiles; avoiding

-  GHC re-calculates the dependencies each time it is invoked, so the
   dependencies never get out of sync with the source.

-  Using the ``-j`` flag, you can compile modules in parallel. Specify
   ``-j⟨N⟩`` to compile ⟨N⟩ jobs in parallel.

Any of the command-line options described in the rest of this chapter
can be used with ``--make``, but note that any options you give on the
command line will apply to all the source files compiled, so if you want
any options to apply to a single source file only, you'll need to use an
``OPTIONS_GHC`` pragma (see :ref:`source-file-options`).

If the program needs to be linked with additional objects (say, some
auxiliary C code), then the object files can be given on the command
line and GHC will include them when linking the executable.

For backward compatibility with existing make scripts, when used in
combination with ``-c``, the linking phase is omitted (same as
``--make -no-link``).

Note that GHC can only follow dependencies if it has the source file
available, so if your program includes a module for which there is no
source file, even if you have an object and an interface file for the
module, then GHC will complain. The exception to this rule is for
package modules, which may or may not have source files.

The source files for the program don't all need to be in the same
directory; the ``-i`` option can be used to add directories to the
search path (see :ref:`search-path`).

.. _eval-mode:

Expression evaluation mode
~~~~~~~~~~~~~~~~~~~~~~~~~~

This mode is very similar to interactive mode, except that there is a
single expression to evaluate which is specified on the command line as
an argument to the ``-e`` option:

::

    ghc -e expr

Haskell source files may be named on the command line, and they will be
loaded exactly as in interactive mode. The expression is evaluated in
the context of the loaded modules.

For example, to load and run a Haskell program containing a module
``Main``, we might say

::

    ghc -e Main.main Main.hs

or we can just use this mode to evaluate expressions in the context of
the ``Prelude``:

::

    $ ghc -e "interact (unlines.map reverse.lines)"
    hello
    olleh

.. _options-order:

Batch compiler mode
~~~~~~~~~~~~~~~~~~~

In *batch mode*, GHC will compile one or more source files given on the
command line.

The first phase to run is determined by each input-file suffix, and the
last phase is determined by a flag. If no relevant flag is present, then
go all the way through to linking. This table summarises:

+-----------------------------------+------------------------------+----------------------------+---------------------------+
| Phase of the compilation system   | Suffix saying “start here”   | Flag saying “stop after”   | (suffix of) output file   |
+===================================+==============================+============================+===========================+
| literate pre-processor            | ``.lhs``                     |                            | ``.hs``                   |
+-----------------------------------+------------------------------+----------------------------+---------------------------+
| C pre-processor (opt.)            | ``.hs`` (with ``-cpp``)      | ``-E``                     | ``.hspp``                 |
+-----------------------------------+------------------------------+----------------------------+---------------------------+
| Haskell compiler                  | ``.hs``                      | ``-C``, ``-S``             | ``.hc``, ``.s``           |
+-----------------------------------+------------------------------+----------------------------+---------------------------+
| C compiler (opt.)                 | ``.hc`` or ``.c``            | ``-S``                     | ``.s``                    |
+-----------------------------------+------------------------------+----------------------------+---------------------------+
| assembler                         | ``.s``                       | ``-c``                     | ``.o``                    |
+-----------------------------------+------------------------------+----------------------------+---------------------------+
| linker                            | ⟨other⟩                      |                            | ``a.out``                 |
+-----------------------------------+------------------------------+----------------------------+---------------------------+

.. index::
   single: -C
   single: -E
   single: -S
   single: -c

Thus, a common invocation would be:

::

    ghc -c Foo.hs

to compile the Haskell source file ``Foo.hs`` to an object file
``Foo.o``.

.. note::
   What the Haskell compiler proper produces depends on what backend
   code generator is used. See :ref:`code-generators` for more details.

.. note::
   Pre-processing is optional, the ``-cpp``\ ``-cpp`` flag turns it
   on. See :ref:`c-pre-processor` for more details.

.. note::
   The option ``-E`` runs just the pre-processing passes of
   the compiler, dumping the result in a file.

.. note::
   The option ``-C`` is only available when GHC is built in
   unregisterised mode. See :ref:`unreg` for more details.

.. _overriding-suffixes:

Overriding the default behaviour for a file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As described above, the way in which a file is processed by GHC depends
on its suffix. This behaviour can be overridden using the ``-x`` option:

``-x``\ ⟨suffix⟩
    .. index::
       single: -x

    Causes all files following this option on the command line to be
    processed as if they had the suffix ⟨suffix⟩. For example, to
    compile a Haskell module in the file ``M.my-hs``, use
    ``ghc -c -x hs M.my-hs``.

.. _options-help:

Verbosity options
-----------------

.. index::
   single: verbosity options

See also the ``--help``, ``--version``, ``--numeric-version``, and
``--print-libdir`` modes in :ref:`modes`.

``-v``
    .. index::
       single: -v

    The ``-v`` option makes GHC *verbose*: it reports its version number
    and shows (on stderr) exactly how it invokes each phase of the
    compilation system. Moreover, it passes the ``-v`` flag to most
    phases; each reports its version number (and possibly some other
    information).

    Please, oh please, use the ``-v`` option when reporting bugs!
    Knowing that you ran the right bits in the right order is always the
    first thing we want to verify.

``-v⟨n⟩``
    .. index::
       single: -v

    To provide more control over the compiler's verbosity, the ``-v``
    flag takes an optional numeric argument. Specifying ``-v`` on its
    own is equivalent to ``-v3``, and the other levels have the
    following meanings:

    ``-v0``
        Disable all non-essential messages (this is the default).

    ``-v1``
        Minimal verbosity: print one line per compilation (this is the
        default when ``--make`` or ``--interactive`` is on).

    ``-v2``
        Print the name of each compilation phase as it is executed.
        (equivalent to ``-dshow-passes``).

    ``-v3``
        The same as ``-v2``, except that in addition the full command
        line (if appropriate) for each compilation phase is also
        printed.

    ``-v4``
        The same as ``-v3`` except that the intermediate program
        representation after each compilation phase is also printed
        (excluding preprocessed and C/assembly files).

``--fprint-potential-instances``
    .. index::
       single: -fprint-potential-instances

    When GHC can't find an instance for a class, it displays a short
    list of some in the instances it knows about. With this flag it
    prints *all* the instances it knows about.

``-fprint-explicit-foralls, -fprint-explicit-kinds, -fprint-unicode-syntax, -fprint-explicit-coercions, -fprint-equality-relations``
    .. index::
       single: -fprint-explicit-foralls
       single: -fprint-explicit-kinds
       single: -fprint-unicode-syntax
       single: -fprint-explicit-coercions
       single: -fprint-equality-relations

    These three flags control the way in which GHC displays types, in
    error messages and in GHCi. Using ``-fprint-explicit-foralls`` makes
    GHC print explicit ``forall`` quantification at the top level of a
    type; normally this is suppressed. For example, in GHCi:

    ::

        ghci> let f x = x
        ghci> :t f
        f :: a -> a
        ghci> :set -fprint-explicit-foralls
        ghci> :t f
        f :: forall a. a -> a

    However, regardless of the flag setting, the quantifiers are printed
    under these circumstances:

    -  For nested ``foralls``, e.g.

       ::

           ghci> :t GHC.ST.runST
           GHC.ST.runST :: (forall s. GHC.ST.ST s a) -> a

    -  If any of the quantified type variables has a kind that mentions
       a kind variable, e.g.

       ::

           ghci> :i Data.Type.Equality.sym
           Data.Type.Equality.sym ::
             forall (k :: BOX) (a :: k) (b :: k).
             (a Data.Type.Equality.:~: b) -> b Data.Type.Equality.:~: a
                   -- Defined in Data.Type.Equality

    Using ``-fprint-explicit-kinds`` makes GHC print kind arguments in
    types, which are normally suppressed. This can be important when you
    are using kind polymorphism. For example:

    ::

        ghci> :set -XPolyKinds
        ghci> data T a = MkT
        ghci> :t MkT
        MkT :: forall (k :: BOX) (a :: k). T a
        ghci> :set -fprint-explicit-foralls
        ghci> :t MkT
        MkT :: forall (k :: BOX) (a :: k). T k a

    When ``-fprint-unicode-syntax`` is enabled, GHC prints type
    signatures using the unicode symbols from the ``-XUnicodeSyntax``
    extension.

    ::

        ghci> :set -fprint-unicode-syntax
        ghci> :t (>>)
        (>>) :: ∀ (m :: * → *) a b. Monad m ⇒ m a → m b → m b

    Using ``-fprint-explicit-coercions`` makes GHC print coercions in
    types. When trying to prove the equality between types of different
    kinds, GHC uses type-level coercions. Users will rarely need to
    see these, as they are meant to be internal.

    Using ``-fprint-equality-relations`` tells GHC to distinguish between
    its equality relations when printing. For example, ``~`` is homogeneous
    lifted equality (the kinds of its arguments are the same) while
    ``~~`` is heterogeneous lifted equality (the kinds of its arguments
    might be different) and ``~#`` is heterogeneous unlifted equality,
    the internal equality relation used in GHC's solver. Generally,
    users should not need to worry about the subtleties here; ``~`` is
    probably what you want. Without ``-fprint-equality-relations``, GHC
    prints all of these as ``~``.
	
``-fprint-expanded-synonyms``
    .. index::
       single: -fprint-expanded-synonyms

    When enabled, GHC also prints type-synonym-expanded types in type
    errors. For example, with this type synonyms:

    ::

        type Foo = Int
        type Bar = Bool
        type MyBarST s = ST s Bar

    This error message:

    ::

        Couldn't match type 'Int' with 'Bool'
        Expected type: ST s Foo
          Actual type: MyBarST s

    Becomes this:

    ::

        Couldn't match type 'Int' with 'Bool'
        Expected type: ST s Foo
          Actual type: MyBarST s
        Type synonyms expanded:
        Expected type: ST s Int
          Actual type: ST s Bool

``-ferror-spans``
    .. index::
       single: -ferror-spans

    Causes GHC to emit the full source span of the syntactic entity
    relating to an error message. Normally, GHC emits the source
    location of the start of the syntactic entity only.

    For example:

    ::

        test.hs:3:6: parse error on input `where'

    becomes:

    ::

        test296.hs:3:6-10: parse error on input `where'

    And multi-line spans are possible too:

    ::

        test.hs:(5,4)-(6,7):
            Conflicting definitions for `a'
            Bound at: test.hs:5:4
                      test.hs:6:7
            In the binding group for: a, b, a

    Note that line numbers start counting at one, but column numbers
    start at zero. This choice was made to follow existing convention
    (i.e. this is how Emacs does it).

``-H⟨size⟩``
    .. index::
       single: -H

    Set the minimum size of the heap to ⟨size⟩. This option is
    equivalent to ``+RTS -Hsize``, see :ref:`rts-options-gc`.

``-Rghc-timing``
    .. index::
       single: -Rghc-timing

    Prints a one-line summary of timing statistics for the GHC run. This
    option is equivalent to ``+RTS -tstderr``, see
    :ref:`rts-options-gc`.

.. _options-platform:

Platform-specific Flags
-----------------------

.. index::
   single: -m\* options
   single: platform-specific options
   single: machine-specific options

Some flags only make sense for particular target platforms.

``-msse2``
    (x86 only, added in GHC 7.0.1) Use the SSE2 registers and
    instruction set to implement floating point operations when using
    the :ref:`native code generator <native-code-gen>`. This gives a
    substantial performance improvement for floating point, but the
    resulting compiled code will only run on processors that support
    SSE2 (Intel Pentium 4 and later, or AMD Athlon 64 and later). The
    :ref:`LLVM backend <llvm-code-gen>` will also use SSE2 if your
    processor supports it but detects this automatically so no flag is
    required.

    SSE2 is unconditionally used on x86-64 platforms.

``-msse4.2``
    (x86 only, added in GHC 7.4.1) Use the SSE4.2 instruction set to
    implement some floating point and bit operations when using the
    :ref:`native code generator <native-code-gen>`. The resulting compiled
    code will only run on processors that support SSE4.2 (Intel Core i7
    and later). The :ref:`LLVM backend <llvm-code-gen>` will also use
    SSE4.2 if your processor supports it but detects this automatically
    so no flag is required.
