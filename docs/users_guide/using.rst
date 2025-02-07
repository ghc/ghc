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
create a file :file:`hello.hs` containing the Haskell code: ::

    main = putStrLn "Hello, World!"

To compile the program, use GHC like this:

.. code-block:: sh

    $ ghc hello.hs

(where ``$`` represents the prompt: don't type it). GHC will compile the
source file :file:`hello.hs`, producing an object file :file:`hello.o` and an
interface file :file:`hello.hi`, and then it will link the object file to
the libraries that come with GHC to produce an executable called
:file:`hello` on Unix/Linux/Mac, or :file:`hello.exe` on Windows.

By default GHC will be very quiet about what it is doing, only printing
error messages. If you want to see in more detail what's going on behind
the scenes, add :ghc-flag:`-v` to the command line.

Then we can run the program like this:

.. code-block:: sh

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

.. code-block:: none

    ghc [argument...]

Command-line arguments are either options or file names.

Command-line options begin with ``-``. They may *not* be grouped:
``-vO`` is different from ``-v -O``. Options need not precede filenames:
e.g., ``ghc *.o -o foo``. All options are processed and then applied to
all files; you cannot, for example, invoke
``ghc -c -O1 Foo.hs -O2 Bar.hs`` to apply different optimisation levels
to the files ``Foo.hs`` and ``Bar.hs``.

In addition to passing arguments via the command-line, arguments can be passed
via GNU-style response files. For instance,

.. code-block:: bash

    $ cat response-file
    -O1
    Hello.hs
    -o Hello
    $ ghc @response-file

.. note::

    .. index::
       single: command-line; order of arguments

    Note that command-line options are *order-dependent*, with arguments being
    evaluated from left-to-right. This can have seemingly strange effects in the
    presence of flag implication. For instance, consider
    :ghc-flag:`-fno-specialise <-fspecialise>` and :ghc-flag:`-O1` (which implies
    :ghc-flag:`-fspecialise`). These two command lines mean very different
    things:

    ``-fno-specialise -O1``

        ``-fspecialise`` will be enabled as the ``-fno-specialise`` is overridden
        by the ``-O1``.

    ``-O1 -fno-specialise``

        ``-fspecialise`` will not be enabled, since the ``-fno-specialise``
        overrides the ``-fspecialise`` implied by ``-O1``.

.. _source-file-options:

Command line options in source files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: source-file options

Sometimes it is useful to make the connection between a source file and
the command-line options it requires quite tight. For instance, if a
Haskell source file deliberately uses name shadowing, it should be
compiled with the ``-Wno-name-shadowing`` option. Rather than
maintaining the list of per-file options in a ``Makefile``, it is
possible to do this directly in the source file using the
``OPTIONS_GHC`` :ref:`pragma <options-pragma>` ::

    {-# OPTIONS_GHC -Wno-name-shadowing #-}
    module X where
    ...

``OPTIONS_GHC`` is a *file-header pragma* (see :ref:`options-pragma`).

Only *dynamic* flags can be used in an ``OPTIONS_GHC`` pragma (see
:ref:`mode-dynamic-flags`).

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
is the Right Thing. (If you use :ghc-flag:`-keep-hc-file` and have ``OPTION`` flags in
your module, the ``OPTIONS_GHC`` will get put into the generated ``.hc`` file).

Setting options in GHCi
~~~~~~~~~~~~~~~~~~~~~~~

Options may also be modified from within GHCi, using the :ghci-cmd:`:set`
command.

.. _mode-dynamic-flags:

Dynamic and Mode options
------------------------

.. index::
   single: dynamic; options
   single: mode; options

Each of GHC's command line options is classified as dynamic or mode:

    Mode: A mode may be used on the command line only.
    You can pass only one mode flag.
    For example, :ghc-flag:`--make` or :ghc-flag:`-E`.
    The available modes are listed in :ref:`modes`.

    Dynamic: A dynamic flag may be used on the command line,
    in a ``OPTIONS_GHC`` pragma in a source
    file, or set using :ghci-cmd:`:set` in GHCi.

The flag reference tables (:ref:`flag-reference`) lists the status of
each flag.

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

``.hie``
    An extended Haskell interface file, produced by the Haskell compiler.

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
option on the command-line. For instance,

.. code-block:: none

    $ ghc Main.hs --make -o my-application

If no mode flag is present, then GHC will enter :ghc-flag:`--make` mode
(:ref:`make-mode`) if there are any Haskell source files given on the
command line, or else it will link the objects named on the command line
to produce an executable.

The available mode flags are:

.. ghc-flag:: --interactive
    :shortdesc: Interactive mode - normally used by just running ``ghci``;
        see :ref:`ghci` for details.
    :type: mode
    :category: modes

    .. index::
       single: interactive mode
       single: GHCi

    Interactive mode, which is also available as :program:`ghci`. Interactive
    mode is described in more detail in :ref:`ghci`.

.. ghc-flag:: --run ⟨file⟩
    :shortdesc: Run a Haskell program.
    :type: mode
    :category: modes

    .. index::
       single: run mode
       single: GHCi

    Run a script's ``main`` entry-point. Similar to ``runghc``/``runhaskell`` this will by
    default use the bytecode interpreter. If the command-line contains a ``--``
    argument then all arguments that follow will be passed to the script. All
    arguments that precede ``--`` are interpreted as GHC arguments.

.. ghc-flag:: --make
    :shortdesc: Build a multi-module Haskell program, automatically figuring out
        dependencies. Likely to be much easier, and faster, than using
        ``make``; see :ref:`make-mode` for details.
    :type: mode
    :category: modes

    .. index::
       single: make mode; of GHC

    In this mode, GHC will build a multi-module Haskell program
    automatically, figuring out dependencies for itself. If you have a
    straightforward Haskell program, this is likely to be much easier,
    and faster, than using :command:`make`. Make mode is described in
    :ref:`make-mode`.

    This mode is the default if there are any Haskell source files
    mentioned on the command line, and in this case the :ghc-flag:`--make`
    option can be omitted.

.. ghc-flag:: -e ⟨expr⟩
    :shortdesc: Evaluate ``expr``; see :ref:`eval-mode` for details.
    :type: mode
    :category: modes

    .. index::
       single: eval mode; of GHC

    Expression-evaluation mode. This is very similar to interactive
    mode, except that there is a single expression to evaluate (⟨expr⟩)
    which is given on the command line. This flag may be given multiple
    times, in which case each expression is evaluated sequentially.
    See :ref:`eval-mode` for more details.

.. ghc-flag:: -E
    :shortdesc: Stop after preprocessing (``.hspp`` file)
    :type: mode
    :category: phases

    Stop after preprocessing (``.hspp`` file)

.. ghc-flag:: -C
    :shortdesc: Stop after generating C (``.hc`` file)
    :type: mode
    :category: phases

    Stop after generating C (``.hc`` file)

.. ghc-flag:: -S
    :shortdesc: Stop after generating assembly (``.s`` file)
    :type: mode
    :category: phases

    Stop after generating assembly (``.s`` file)

.. ghc-flag:: -c
    :shortdesc: Stop after generating object (``.o``) file
    :type: mode
    :category: phases

    Stop after generating object (``.o``) file

    This is the traditional batch-compiler mode, in which GHC can
    compile source files one at a time, or link objects together into an
    executable. See :ref:`options-order`.

.. ghc-flag:: --merge-objs
    :shortdesc: Merge a set of objects into a GHCi library.
    :type: mode
    :category: phases

    Merge a set of static object files into a library optimised for loading in
    GHCi. See :ref:`building-ghci-libraries`.

.. ghc-flag:: -M
    :shortdesc: generate dependency information suitable for use in a
        ``Makefile``; see :ref:`makefile-dependencies` for details.
    :type: mode
    :category: modes

    .. index::
        single: dependency-generation mode; of GHC

    Dependency-generation mode. In this mode, GHC can be used to
    generate dependency information suitable for use in a ``Makefile``.
    See :ref:`makefile-dependencies`.

.. ghc-flag:: --frontend ⟨module⟩
    :shortdesc: run GHC with the given frontend plugin; see
        :ref:`frontend_plugins` for details.
    :type: mode
    :category: modes

    .. index::
        single: frontend plugins; using

    Run GHC using the given frontend plugin. See :ref:`frontend_plugins` for
    details.

.. ghc-flag:: -shared
    :shortdesc: Create a shared object.
    :type: mode
    :category: modes

    .. index::
       single: DLL-creation mode
       single: Shared-object creation mode

    Create a shared object (or, on Windows, DLL). See :ref:`win32-dlls-create`.

.. ghc-flag:: --help
              -?
    :shortdesc: Display help
    :type: mode
    :category: modes

    Cause GHC to spew a long usage message to standard output and then
    exit.

.. ghc-flag:: --show-iface ⟨file⟩
    :shortdesc: display the contents of an interface file.
    :type: mode
    :category: modes

    Read the interface in ⟨file⟩ and dump it as text to ``stdout``. For
    example ``ghc --show-iface M.hi``.

.. ghc-flag:: --supported-extensions
              --supported-languages
    :shortdesc: display the supported language extensions
    :type: mode
    :category: modes

    Print the supported language extensions.

.. ghc-flag:: --show-options
    :shortdesc: display the supported command line options
    :type: mode
    :category: modes

    Print the supported command line options. This flag can be used for
    autocompletion in a shell.

.. ghc-flag:: --info
    :shortdesc: display information about the compiler
    :type: mode
    :category: modes

    Print information about the compiler.

.. ghc-flag:: --version
              -V
    :shortdesc: display GHC version
    :type: mode
    :category: modes

    Print a one-line string including GHC's version number.

.. ghc-flag:: --numeric-version
    :shortdesc: display GHC version (numeric only)
    :type: mode
    :category: modes

    Print GHC's numeric version number only.

.. ghc-flag:: --print-booter-version
    :shortdesc: display bootstrap compiler version
    :type: mode
    :category: modes

    Print the numeric version of the GHC binary used to
    bootstrap the build of this compiler.

.. ghc-flag:: --print-build-platform
    :shortdesc: display platform on which GHC was built
    :type: mode
    :category: modes

    Print the target string of the build platform, on which GHC was built,
    as generated by GNU Autotools.
    The format is ``cpu-manufacturer-operating_system-(kernel)``, e.g.,
    ``x86_64-unknown-linux``.

.. ghc-flag:: --print-c-compiler-flags
    :shortdesc: C compiler flags used to build GHC
    :type: mode
    :category: modes

    List the flags passed to the C compiler during GHC build.

.. ghc-flag:: --print-c-compiler-link-flags
    :shortdesc: C linker flags used to build GHC
    :type: mode
    :category: modes

    List the flags passed to the C compiler for the linking step
    during GHC build.

.. ghc-flag:: --print-debug-on
    :shortdesc: print whether GHC was built with ``-DDEBUG``
    :type: mode
    :category: modes

    Print ``True`` if GHC was built with ``-DDebug`` flag.
    This enables assertions and extra debug code.
    The flag can be set in ``GhcStage1HcOpts`` and/or ``GhcStage2HcOpts``
    and is automatically set for ``devel1`` and ``devel2`` build flavors.

.. ghc-flag:: --print-global-package-db
    :shortdesc: display GHC's global package database directory
    :type: mode
    :category: modes

    Print the path to GHC's global package database directory.
    A package database stores details about installed packages as a directory
    containing a file for each package.
    This flag prints the path to the global database shipped with GHC, and
    looks something like ``/usr/lib/ghc/package.conf.d`` on Unix.
    There may be other package databases, e.g., the user package databse.
    For more details see :ref:`package-databases`.

.. ghc-flag:: --print-have-interpreter
    :shortdesc: display whether GHC was built with interactive support
    :type: mode
    :category: modes

    Print ``YES`` if GHC was compiled to include the interpreter, ``NO`` otherwise.
    If this GHC does not have the interpreter included, running it in interactive
    mode (see :ghc-flag:`--interactive`) will throw an error.
    This only pertains the use of GHC interactively, not any separate GHCi binaries
    (see :ref:`ghci`).

.. ghc-flag:: --print-have-native-code-generator
    :shortdesc: display whether target platform has NCG support
    :type: mode
    :category: modes

    Print ``YES`` if native code generator supports the target platform,
    ``NO`` otherwise.
    (See :ref:`native-code-gen`)

.. ghc-flag:: --print-host-platform
    :shortdesc: display host platform of GHC
    :type: mode
    :category: modes

    Print the target string of the host platform, i.e.,
    the one on which GHC is supposed to run, as generated by GNU Autotools.
    The format is ``cpu-manufacturer-operating_system-(kernel)``, e.g.,
    ``x86_64-unknown-linux``.

.. ghc-flag:: --print-leading-underscore
    :shortdesc: display use of leading underscores on symbol names
    :type: mode
    :category: modes

    Print ``YES`` if GHC was compiled to use symbols with leading underscores
    in object files, ``NO`` otherwise.
    This is usually atarget platform dependent.

.. ghc-flag:: --print-libdir
    :shortdesc: display GHC library directory
    :type: mode
    :category: modes

    .. index::
       single: libdir

    Print the path to GHC's library directory. This is the top of the
    directory tree containing GHC's libraries, interfaces, and include
    files (usually something like ``/usr/local/lib/ghc-5.04`` on Unix).
    This is the value of ``$libdir`` in the package
    configuration file (see :ref:`packages`).

.. ghc-flag:: --print-ld-flags
    :shortdesc: display linker flags used to compile GHC
    :type: mode
    :category: modes

    Print linke flags used to compile GHC.

.. ghc-flag:: --print-object-splitting-supported
    :shortdesc: display whether GHC supports object splitting
    :type: mode
    :category: modes

    Prints ``NO`` as object splitting is no longer supported. See
    :ghc-flag:`-split-sections` for a more portable and reliable alternative.

.. ghc-flag:: --print-project-git-commit-id
    :shortdesc: display Git commit id GHC is built from
    :type: mode
    :category: modes

    Print the Git commit id from which this GHC was built.
    This can be used to trace the current binary back to a specific
    revision, which is especially useful during development on GHC itself.
    It is set by the configure script.

.. ghc-flag:: --print-project-version
    :shortdesc: display GHC version
    :type: mode
    :category: modes

    Print the version set in the configure script during build.
    This is simply the GHC version.

.. ghc-flag:: --print-rts-ways
    :shortdesc: display which way RTS was built
    :type: mode
    :category: modes

    Packages, like the Runtime System, can be built in a number of ways:
    - profiling - with profiling support
    - dynamic - with dynamic linking
    - logging - RTS event logging
    - threaded - mulithreaded RTS
    - debug - RTS with debug information

    Various combinations of these flavours are possible.

.. ghc-flag:: --print-stage
    :shortdesc: display ``stage`` number of GHC
    :type: mode
    :category: modes

    GHC is built using GHC itself and this build happens in stages,
    which are numbered.

    - Stage 0 is the GHC you have installed.  The "GHC you have installed" is also called "the bootstrap compiler".
    - Stage 1 is the first GHC we build, using stage 0.  Stage 1 is then used to build the packages.
    - Stage 2 is the second GHC we build, using stage 1.  This is the one we normally install when you say make install.
    - Stage 3 is optional, but is sometimes built to test stage 2.

    Stage 1 does not support interactive execution (GHCi) and Template Haskell.

.. ghc-flag:: --print-support-smp
    :shortdesc: display whether GHC was compiled with SMP support
    :type: mode
    :category: modes

    Print ``YES`` if GHC was built with multiporcessor support, ``NO`` otherwise.

.. ghc-flag:: --print-tables-next-to-code
    :shortdesc: display whether GHC was compiled with ``--enable-tables-next-to-code``
    :type: mode
    :category: modes

    Print ``YES`` if GHC was built with the flag ``--enable-tables-next-to-code``, ``NO`` otherwise.
    This option is on by default, as it generates a more efficient code layout.

.. ghc-flag:: --print-target-platform
    :shortdesc: display target platform of GHC
    :type: mode
    :category: modes

    Print the target string of the target platform, i.e.,
    the one on which generated binaries will run, as generated by GNU Autotools.
    The format is ``cpu-manufacturer-operating_system-(kernel)``, e.g.,
    ``x86_64-unknown-linux``.

.. ghc-flag:: --print-unregisterised
    :shortdesc: display whether this GHC was built in unregisterised mode
    :type: mode
    :category: modes

    Print ``YES`` if this GHC was built in unregisterised mode, ``NO`` otherwise.
    "Unregisterised" means that GHC will disable most platform-specific tricks
    and optimisations. Only the LLVM and C code generators will be available.
    See :ref:`unreg` for more details.

.. _make-mode:

Using ``ghc`` ``--make``
~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: --make; mode of GHC
   single: separate compilation

In this mode, GHC will build a multi-module Haskell program by following
dependencies from one or more root modules (usually just ``Main``). For
example, if your ``Main`` module is in a file called :file:`Main.hs`, you
could compile and link the program like this:

.. code-block:: none

    ghc --make Main.hs

In fact, GHC enters make mode automatically if there are any Haskell
source files on the command line and no other mode is specified, so in
this case we could just type

.. code-block:: none

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

-  Using the :ghc-flag:`-j[⟨n⟩]` flag, you can compile modules in parallel.
   Specify ``-j ⟨n⟩`` to compile ⟨n⟩ jobs in parallel. If ⟨n⟩ is omitted, then
   it defaults to the number of processors.

Any of the command-line options described in the rest of this chapter
can be used with ``--make``, but note that any options you give on the
command line will apply to all the source files compiled, so if you want
any options to apply to a single source file only, you'll need to use an
``OPTIONS_GHC`` pragma (see :ref:`source-file-options`).

If the program needs to be linked with additional objects (say, some
auxiliary C code), then the object files can be given on the command
line and GHC will include them when linking the executable.

For backward compatibility with existing make scripts, when used in
combination with :ghc-flag:`-c`, the linking phase is omitted (same as
``--make -no-link``).

Note that GHC can only follow dependencies if it has the source file
available, so if your program includes a module for which there is no
source file, even if you have an object and an interface file for the
module, then GHC will complain. The exception to this rule is for
package modules, which may or may not have source files.

The source files for the program don't all need to be in the same
directory; the :ghc-flag:`-i` option can be used to add directories to the
search path (see :ref:`search-path`).

.. ghc-flag:: -j[⟨n⟩]
    :shortdesc: When compiling with :ghc-flag:`--make`, compile ⟨n⟩ modules
        in parallel.
    :type: dynamic
    :category: misc

    Perform compilation in parallel when possible. GHC will use up to ⟨N⟩
    threads during compilation. If N is omitted, then it defaults to the
    number of processors. Note that compilation of a module may not begin
    until its dependencies have been built.


GHC Jobserver Protocol
~~~~~~~~~~~~~~~~~~~~~~

The GHC Jobserver Protocol was specified in `GHC Proposal #540 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0540-jsem.rst>`__.

This protocol allows
a server to dynamically invoke many instances of a client process,
while restricting all of those instances to use no more than <n> capabilities.
This is achieved by coordination over a system semaphore (either a POSIX
semaphore in the case of Linux and Darwin, or a Win32 semaphore
in the case of Windows platforms).

There are two kinds of participants in the GHC Jobserver protocol:

- The *jobserver* creates a system semaphore with a certain number of
  available tokens.

  Each time the jobserver wants to spawn a new jobclient subprocess, it **must**
  first acquire a single token from the semaphore, before spawning
  the subprocess. This token **must** be released once the subprocess terminates.

  Once work is finished, the jobserver **must** destroy the semaphore it created.

- A *jobclient* is a subprocess spawned by the jobserver or another jobclient.

  Each jobclient starts with one available token (its *implicit token*,
  which was acquired by the parent which spawned it), and can request more
  tokens through the Jobserver Protocol by waiting on the semaphore.

  Each time a jobclient wants to spawn a new jobclient subprocess, it **must**
  pass on a single token to the child jobclient. This token can either be the
  jobclient's implicit token, or another token which the jobclient acquired
  from the semaphore.

  Each jobclient **must** release exactly as many tokens as it has acquired from
  the semaphore (this does not include the implicit tokens).

  GHC itself acts as a jobclient which can be enabled by using the flag ``-jsem``.

.. ghc-flag:: -jsem
    :shortdesc: When compiling with :ghc-flag:`--make`, coordinate with
                other processes through the semaphore ⟨sem⟩ to compile
                modules in parallel.
    :type: dynamic
    :category: misc

    Perform compilation in parallel when possible, coordinating with other
    processes through the semaphore ⟨sem⟩ (specified as a string).
    Error if the semaphore doesn't exist.

    Use of ``-jsem`` will override use of :ghc-flag:`-j[⟨n⟩]`,
    and vice-versa.

.. _multi-home-units:

Multiple Home Units
~~~~~~~~~~~~~~~~~~~

The compiler also has support for building multiple units in a single compiler
invocation. In modern projects it is common to work on multiple interdependent
packages at once, using the support for multiple home units you can load all
these local packages into one ghc session and quickly get feedback about how
changes affect other dependent packages.

In order to specify multiple units, the `-unit @⟨filename⟩`:ghc-flag: is given multiple times
with a response file containing the arguments for each unit. The response file contains
a newline separated list of arguments.

.. code-block:: none

    ghc -unit @unitA -unit @unitB

where the ``unitA`` response file contains the normal arguments that you would
pass to ``--make`` mode.

.. code-block:: none

    -this-unit-id a-0.1.0.0
    -i
    -isrc
    A1
    A2
    ...

Then when the compiler starts in ``--make`` mode it will compile both units ``a`` and ``b``.

There is also very basic support for multiple home units in GHCi, at the moment you can start
a GHCi session with multiple units but only the `:reload`:ghci-cmd: is supported.

.. ghc-flag:: -unit @⟨filename⟩
    :shortdesc: Specify the options to build a specific unit.
    :type: dynamic
    :category: misc

    This option is passed multiple times to inform the compiler about all the
    home units which it will compile. The options for each unit are supplied
    in a response file which contains a newline separated list of normal arguments.

There are a few extra flags which have been introduced to make working with multiple
units easier.

.. ghc-flag:: -working-dir ⟨dir⟩
    :shortdesc: Specify the directory a unit is expected to be compiled in.
    :type: dynamic
    :category:

    It is common to assume that a package is compiled in the directory where its
    cabal file resides. Thus, all paths used in the compiler are assumed to be relative
    to this directory. When there are multiple home units the compiler is often
    not operating in the standard directory and instead where the cabal.project
    file is located. In this case the `-working-dir` option can be passed which specifies
    the path from the current directory to the directory the unit assumes to be its root,
    normally the directory which contains the cabal file.

    When the flag is passed, any relative paths used by the compiler are offset
    by the working directory. Notably this includes `-i`:ghc-flag: and `-I⟨dir⟩`:ghc-flag: flags.


    This option can also be queried by the ``getPackageRoot`` Template Haskell
    function. It is intended to be used with helper functions such as ``makeRelativeToProject``
    which make relative filepaths relative to the compilation directory rather than
    the directory which contains the .cabal file.

.. ghc-flag:: -this-package-name ⟨unit-id⟩
    :shortdesc: The name of the package which this module would be part of when installed.
    :type: dynamic
    :category:

    This flag papers over the awkward interaction of the `PackageImports`:extension:
    and multiple home units. When using ``PackageImports`` you can specify the name
    of the package in an import to disambiguate between modules which appear in multiple
    packages with the same name.

    This flag allows a home unit to be given a package name so that you can also
    disambiguate between multiple home units which provide modules with the same name.

.. ghc-flag:: -hidden-module ⟨module name⟩
    :shortdesc: A module which should not be visible outside its unit.
    :type: dynamic
    :category:

    This flag can be supplied multiple times in order to specify which modules
    in a home unit should not be visible outside of the unit it belongs to.

    The main use of this flag is to be able to recreate the difference between
    an exposed and hidden module for installed packages.

.. ghc-flag:: -reexported-module ⟨reexport-spec⟩
    :shortdesc: A module which should be reexported from this unit.
    :type: dynamic
    :category:

    This flag can be supplied multiple times in order to specify which modules
    are not defined in a unit but should be reexported. The effect is that other
    units will see this module as if it was defined in this unit.

    The simple form of the flag allows the reexport of a single module at the
    same name::

      -reexported-module A

    the complicated version of the flag allows the module to be renamed when
    reexported::

      -reexported-module "A as B"

    The use of this flag is to be able to replicate the reexported modules
    feature of packages with multiple home units.



The home unit closure requirement
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There is one very important closure property which you must ensure when using
multiple home units.

  Any external unit must not depend on any home unit.

This closure property is checked by the compiler but it's up to the tool invoking
GHC to ensure that the supplied list of home units obeys this invariant.

For example, if we have three units, ``p``, ``q`` and ``r``, where ``p`` depends on ``q`` and
``q`` depends on ``r``, then the closure property states that if we load ``p`` and ``r`` as
home units then we must also load ``q``, because ``q`` depends on the home unit ``r`` and we need
``q`` because ``p`` depends on it.


.. _eval-mode:

Expression evaluation mode
~~~~~~~~~~~~~~~~~~~~~~~~~~

This mode is very similar to interactive mode, except that there is a
single expression to evaluate which is specified on the command line as
an argument to the ``-e`` option:

.. code-block:: none

    ghc -e expr

Haskell source files may be named on the command line, and they will be
loaded exactly as in interactive mode. The expression is evaluated in
the context of the loaded modules.

For example, to load and run a Haskell program containing a module
``Main``, we might say:

.. code-block:: none

    ghc -e Main.main Main.hs

or we can just use this mode to evaluate expressions in the context of
the ``Prelude``:

.. code-block:: none

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

.. code-block:: none

    ghc -c Foo.hs

to compile the Haskell source file ``Foo.hs`` to an object file
``Foo.o``.

.. note::
   What the Haskell compiler proper produces depends on what backend
   code generator is used. See :ref:`code-generators` for more details.

.. note::
   Pre-processing is optional, the :ghc-flag:`-cpp` flag turns it
   on. See :ref:`c-pre-processor` for more details.

.. note::
   The option :ghc-flag:`-E` runs just the pre-processing passes of
   the compiler, dumping the result in a file.

.. note::
   The option :ghc-flag:`-C` is only available when GHC is built in
   unregisterised mode. See :ref:`unreg` for more details.

.. _overriding-suffixes:

Overriding the default behaviour for a file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As described above, the way in which a file is processed by GHC depends on its
suffix. This behaviour can be overridden using the :ghc-flag:`-x ⟨suffix⟩`
option:

.. ghc-flag:: -x ⟨suffix⟩
    :shortdesc: Override default behaviour for source files
    :type: dynamic
    :category: phases

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

.. ghc-flag:: -v
    :shortdesc: verbose mode (equivalent to ``-v3``)
    :type: dynamic
    :category: verbosity

    The :ghc-flag:`-v` option makes GHC *verbose*: it reports its version number
    and shows (on stderr) exactly how it invokes each phase of the
    compilation system. Moreover, it passes the ``-v`` flag to most
    phases; each reports its version number (and possibly some other
    information).

    Please, oh please, use the ``-v`` option when reporting bugs!
    Knowing that you ran the right bits in the right order is always the
    first thing we want to verify.

.. ghc-flag:: -v⟨n⟩
    :shortdesc: set verbosity level
    :type: dynamic
    :category: verbosity

    To provide more control over the compiler's verbosity, the ``-v``
    flag takes an optional numeric argument. Specifying ``-v`` on its
    own is equivalent to ``-v3``, and the other levels have the
    following meanings:

    ``-v0``
        Disable all non-essential messages (this is the default).

    ``-v1``
        Minimal verbosity: print one line per compilation (this is the
        default when :ghc-flag:`--make` or :ghc-flag:`--interactive` is on).

    ``-v2``
        Print the name of each compilation phase as it is executed.
        (equivalent to :ghc-flag:`-dshow-passes`).

    ``-v3``
        The same as ``-v2``, except that in addition the full command
        line (if appropriate) for each compilation phase is also
        printed.

    ``-v4``
        The same as ``-v3`` except that the intermediate program
        representation after each compilation phase is also printed
        (excluding preprocessed and C/assembly files).

.. ghc-flag:: -fprint-potential-instances
    :shortdesc: display all available instances in type error messages
    :type: dynamic
    :reverse: -fno-print-potential-instances
    :category: verbosity

    When GHC can't find an instance for a class, it displays a short
    list of some of the instances it knows about. With this flag it
    prints *all* the instances it knows about.

.. ghc-flag:: -fhide-source-paths
    :shortdesc: hide module source and object paths
    :type: dynamic
    :category: verbosity

    Starting with minimal verbosity (``-v1``, see :ghc-flag:`-v`), GHC
    displays the name, the source path and the target path of each compiled
    module. This flag can be used to reduce GHC's output by hiding source paths
    and target paths.

The following flags control the way in which GHC displays types in error
messages and in GHCi:

.. ghc-flag:: -fprint-unicode-syntax
    :shortdesc: Use unicode syntax when printing expressions, types and kinds.
        See also :extension:`UnicodeSyntax`
    :type: dynamic
    :reverse: -fno-print-unicode-syntax
    :category: verbosity

    When enabled GHC prints type signatures using the unicode symbols from the
    :extension:`UnicodeSyntax` extension. For instance,

    .. code-block:: none

        ghci> :set -fprint-unicode-syntax
        ghci> :t +v (>>)
        (>>) ∷ Monad m ⇒ ∀ a b. m a → m b → m b

.. _pretty-printing-types:

.. ghc-flag:: -fprint-explicit-foralls
    :shortdesc: Print explicit ``forall`` quantification in types.
        See also :extension:`ExplicitForAll`
    :type: dynamic
    :reverse: -fno-print-explicit-foralls
    :category: verbosity

    Using :ghc-flag:`-fprint-explicit-foralls` makes
    GHC print explicit ``forall`` quantification at the top level of a
    type; normally this is suppressed. For example, in GHCi:

    .. code-block:: none

        ghci> let f x = x
        ghci> :t f
        f :: a -> a
        ghci> :set -fprint-explicit-foralls
        ghci> :t f
        f :: forall a. a -> a

    However, regardless of the flag setting, the quantifiers are printed
    under these circumstances:

    -  For nested ``foralls``, e.g.

       .. code-block:: none

           ghci> :t GHC.ST.runST
           GHC.ST.runST :: (forall s. GHC.ST.ST s a) -> a

    -  If any of the quantified type variables has a kind that mentions
       a kind variable, e.g.

       .. code-block:: none

           ghci> :i Data.Type.Equality.sym
           Data.Type.Equality.sym ::
             forall k (a :: k) (b :: k).
             (a Data.Type.Equality.:~: b) -> b Data.Type.Equality.:~: a
                   -- Defined in Data.Type.Equality

.. ghc-flag:: -fprint-explicit-kinds
    :shortdesc: Print explicit kind foralls and kind arguments in types.
        See also :extension:`KindSignatures`
    :type: dynamic
    :reverse: -fno-print-explicit-kinds
    :category: verbosity

    Using :ghc-flag:`-fprint-explicit-kinds` makes GHC print kind arguments in
    types, which are normally suppressed. This can be important when you
    are using kind polymorphism. For example:

    .. code-block:: none

           ghci> :set -XPolyKinds
           ghci> data T a (b :: l) = MkT
           ghci> :t MkT
           MkT :: forall k l (a :: k) (b :: l). T a b
           ghci> :set -fprint-explicit-kinds
           ghci> :t MkT
           MkT :: forall k l (a :: k) (b :: l). T @{k} @l a b
           ghci> :set -XNoPolyKinds
           ghci> :t MkT
           MkT :: T @{*} @* a b

    In the output above, observe that ``T`` has two kind variables
    (``k`` and ``l``) and two type variables (``a`` and ``b``). Note that
    ``k`` is an *inferred* variable and ``l`` is a *specified* variable
    (see :ref:`inferred-vs-specified`), so as a result, they are displayed
    using slightly different syntax in the type ``T @{k} @l a b``. The
    application of ``l`` (with ``@l``) is the standard syntax for visible
    type application (see :ref:`visible-type-application`). The application
    of ``k`` (with ``@{k}``), however, uses a hypothetical syntax for visible
    type application of inferred type variables. This syntax is not currently
    exposed to the programmer, but it is nevertheless displayed when
    :ghc-flag:`-fprint-explicit-kinds` is enabled.

.. ghc-flag:: -fprint-explicit-coercions
    :shortdesc: Print coercions in types
    :type: dynamic
    :reverse: -fno-print-explicit-coercions
    :category: verbosity

    Using :ghc-flag:`-fprint-explicit-coercions` makes GHC print coercions in
    types. When trying to prove the equality between types of different
    kinds, GHC uses type-level coercions. Users will rarely need to
    see these, as they are meant to be internal.

.. ghc-flag:: -fprint-axiom-incomps
    :shortdesc: Display equation incompatibilities in closed type families
    :type: dynamic
    :reverse: -fno-print-axiom-incomps
    :category: verbosity

    Using :ghc-flag:`-fprint-axiom-incomps` tells GHC to display
    incompatibilities between closed type families' equations, whenever they
    are printed by :ghci-cmd:`:info` or :ghc-flag:`--show-iface ⟨file⟩`.

    .. code-block:: none

        ghci> :i Data.Type.Equality.==
        type family (==) (a :: k) (b :: k) :: Bool
          where
              (==) (f a) (g b) = (f == g) && (a == b)
              (==) a a = 'True
              (==) _1 _2 = 'False
        ghci> :set -fprint-axiom-incomps
        ghci> :i Data.Type.Equality.==
        type family (==) (a :: k) (b :: k) :: Bool
          where
              {- #0 -} (==) (f a) (g b) = (f == g) && (a == b)
              {- #1 -} (==) a a = 'True
                  -- incompatible with: #0
              {- #2 -} (==) _1 _2 = 'False
                  -- incompatible with: #1, #0

    The equations are numbered starting from 0, and the comment after each
    equation refers to all preceding equations it is incompatible with.

.. ghc-flag:: -fprint-equality-relations
    :shortdesc: Distinguish between equality relations when printing
    :type: dynamic
    :reverse: -fno-print-equality-relations
    :category: verbosity

    Using :ghc-flag:`-fprint-equality-relations` tells GHC to distinguish between
    its equality relations when printing. For example, ``~`` is homogeneous
    lifted equality (the kinds of its arguments are the same) while
    ``~~`` is heterogeneous lifted equality (the kinds of its arguments
    might be different) and ``~#`` is heterogeneous unlifted equality,
    the internal equality relation used in GHC's solver. Generally,
    users should not need to worry about the subtleties here; ``~`` is
    probably what you want. Without :ghc-flag:`-fprint-equality-relations`, GHC
    prints all of these as ``~``. See also :ref:`equality-constraints`.

.. ghc-flag:: -fprint-expanded-synonyms
    :shortdesc: In type errors, also print type-synonym-expanded types.
    :type: dynamic
    :reverse: -fno-print-expanded-synonyms
    :category: verbosity

    When enabled, GHC also prints type-synonym-expanded types in type
    errors. For example, with this type synonyms: ::

        type Foo = Int
        type Bar = Bool
        type MyBarST s = ST s Bar

    This error message:

    .. code-block:: none

        Couldn't match type 'Int' with 'Bool'
        Expected type: ST s Foo
          Actual type: MyBarST s

    Becomes this:

    .. code-block:: none

        Couldn't match type 'Int' with 'Bool'
        Expected type: ST s Foo
          Actual type: MyBarST s
        Type synonyms expanded:
        Expected type: ST s Int
          Actual type: ST s Bool

.. ghc-flag:: -fprint-redundant-promotion-ticks
    :shortdesc: Print redundant :extension:`DataKinds` promotion ticks
    :type: dynamic
    :reverse: -fno-print-redundant-promotion-ticks
    :category: verbosity

    The :extension:`DataKinds` extension allows us to use data constructors at
    the type level::

        type B = True     -- refers to the data constructor True (of type Bool)

    When there is a type constructor of the same name, it takes precedence
    during name resolution::

        data True = MkT
        type B = True     -- now refers to the type constructor (of kind Type)

    We can tell GHC to prefer the data constructor over the type constructor
    using special namespace disambiguation syntax that we call a *promotion
    tick*::

        data True = MkT
        type B = 'True
            -- refers to the data constructor True (of type Bool)
            -- even in the presence of a type constructor of the same name

    Note that the promotion tick is not a promotion operator. Its only purpose
    is to instruct GHC to prefer the promoted data constructor over a type
    constructor in case of a name conflict. Therefore, GHC will not print the
    tick when the name conflict is absent:

    .. code-block:: none

        ghci> type B = False
        ghci> :kind! B
        B :: Bool
        = False          -- no promotion tick here

        ghci> data False -- introduce a name conflict

        ghci> :kind! B
        B :: Bool
        = 'False         -- promotion tick resolves the name conflict

    The :ghc-flag:`-fprint-redundant-promotion-ticks` instructs GHC to print the
    promotion tick unconditionally.

.. ghc-flag:: -fprint-typechecker-elaboration
    :shortdesc: Print extra information from typechecker.
    :type: dynamic
    :reverse: -fno-print-typechecker-elaboration
    :category: verbosity

    When enabled, GHC also prints extra information from the typechecker in
    warnings. For example: ::

        main :: IO ()
        main = do
          return $ let a = "hello" in a
          return ()

    This warning message:

    .. code-block:: none

        A do-notation statement discarded a result of type ‘[Char]’
        Suppress this warning by saying
          ‘_ <- ($) return let a = "hello" in a’
        or by using the flag -fno-warn-unused-do-bind

    Becomes this:

    .. code-block:: none

        A do-notation statement discarded a result of type ‘[Char]’
        Suppress this warning by saying
          ‘_ <- ($)
                  return
                  let
                    AbsBinds [] []
                      {Exports: [a <= a
                                   <>]
                       Exported types: a :: [Char]
                                       [LclId, Str=DmdType]
                       Binds: a = "hello"}
                  in a’
        or by using the flag -fno-warn-unused-do-bind

.. ghc-flag:: -fdefer-diagnostics
    :shortdesc: Defer and group diagnostic messages by severity
    :type: dynamic
    :category: verbosity

    Causes GHC to group diagnostic messages by severity and output them after
    other messages when building a multi-module Haskell program. This flag can
    make diagnostic messages more visible when used in conjunction with
    :ghc-flag:`--make` and :ghc-flag:`-j[⟨n⟩]`. Otherwise, it can be hard to
    find the relevant errors or likely to ignore the warnings when they are
    mixed with many other messages.

.. ghc-flag:: -fdiagnostics-as-json
    :shortdesc: Output diagnostics in Json format specified by JSON schema
    :type: dynamic
    :category: verbosity

    Causes GHC to emit diagnostic messages in a standardized JSON format,
    and output them directly to ``stderr``. The format follows the `JSON Lines <https://jsonlines.org>`_
    convention, where each diagnostic is its own JSON object separated by
    a new line.

    The structure of the output is described by a `JSON Schema <https://json-schema.org/>`_.
    The schema can be downloaded :download:`here <diagnostics-as-json-schema-1_1.json>`.

.. ghc-flag:: -fdiagnostics-color=⟨always|auto|never⟩
    :shortdesc: Use colors in error messages
    :type: dynamic
    :category: verbosity

    Causes GHC to display error messages with colors.  To do this, the
    terminal must have support for ANSI color codes, or else garbled text will
    appear.  The default value is ``auto``, which means GHC will make an
    attempt to detect whether terminal supports colors and choose accordingly.

    The precise color scheme is controlled by the environment variable
    ``GHC_COLORS`` (or ``GHC_COLOURS``).  This can be set to colon-separated
    list of ``key=value`` pairs.  These are the default settings:

    .. code-block:: none

        header=:message=1:warning=1;35:error=1;31:fatal=1;31:margin=1;34

    Each value is expected to be a `Select Graphic Rendition (SGR) substring
    <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR>`_.  The
    formatting of each element can inherit from parent elements.  For example,
    if ``header`` is left empty, it will inherit the formatting of
    ``message``.  Alternatively if ``header`` is set to ``1`` (bold), it will
    be bolded but still inherits the color of ``message``.

    Currently, in the primary message, the following inheritance tree is in
    place:

    - ``message``

      - ``header``

        - ``warning``
        - ``error``
        - ``fatal``

    In the caret diagnostics, there is currently no inheritance at all between
    ``margin``, ``warning``, ``error``, and ``fatal``.

    The environment variable can also be set to the magical values ``never``
    or ``always``, which is equivalent to setting the corresponding
    ``-fdiagnostics-color`` flag but with lower precedence.

.. ghc-flag:: -fdiagnostics-show-caret
    :shortdesc: Whether to show snippets of original source code
    :type: dynamic
    :reverse: -fno-diagnostics-show-caret
    :category: verbosity

    :default: on

    Controls whether GHC displays a line of the original source code where the
    error was detected.  This also affects the associated caret symbol that
    points at the region of code at fault.

.. ghc-flag:: -fshow-error-context
    :shortdesc: Whether to show textual information about error context
    :type: dynamic
    :reverse: -fno-show-error-context
    :category: verbosity

    :default: on

    Controls whether GHC displays information about the context in which an
    error occurred. This controls whether the part of the error message which
    says "In the equation..", "In the pattern.." etc is displayed or not.

.. ghc-flag:: -fprint-error-index-links=⟨always|auto|never⟩
    :shortdesc: Whether to emit diagnostic codes as ANSI hyperlinks to the
                Haskell Error Index.
    :type: dynamic
    :category: verbosity

    :default: auto

    Controls whether GHC will emit error indices as ANSI hyperlinks to the
    `Haskell Error Index <https://errors.haskell.org/>`_. When set to auto, this
    flag will render hyperlinks if the terminal is capable; when set to always,
    this flag will render the hyperlinks regardless of the capabilities of the
    terminal.

.. ghc-flag:: -ferror-spans
    :shortdesc: Output full span in error messages
    :type: dynamic
    :category: verbosity

    Causes GHC to emit the full source span of the syntactic entity
    relating to an error message. Normally, GHC emits the source
    location of the start of the syntactic entity only.

    For example:

    .. code-block:: none

        test.hs:3:6: parse error on input `where'

    becomes:

    .. code-block:: none

        test296.hs:3:6-10: parse error on input `where'

    And multi-line spans are possible too:

    .. code-block:: none

        test.hs:(5,4)-(6,7):
            Conflicting definitions for `a'
            Bound at: test.hs:5:4
                      test.hs:6:7
            In the binding group for: a, b, a

    Note that line numbers start counting at one, but column numbers
    start at zero. This choice was made to follow existing convention
    (i.e. this is how Emacs does it).

.. ghc-flag:: -fkeep-going
    :shortdesc: Continue compilation as far as possible on errors
    :type: dynamic
    :category: verbosity

    :since: 8.10.1

    Causes GHC to continue the compilation if a module has an error.
    Any reverse dependencies are pruned immediately and the whole
    compilation is still flagged as an error.  This option has no
    effect if parallel compilation (:ghc-flag:`-j[⟨n⟩]`) is in use.

.. ghc-flag:: -freverse-errors
    :shortdesc: Output errors in reverse order
    :type: dynamic
    :reverse: -fno-reverse-errors
    :category: verbosity

    Causes GHC to output errors in reverse line-number order, so that
    the errors and warnings that originate later in the file are
    displayed first.

.. ghc-flag:: -Rghc-timing
    :shortdesc: Summarise timing stats for GHC (same as ``+RTS -tstderr``).
    :type: dynamic
    :category: verbosity

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

.. ghc-flag:: -mavx
    :shortdesc: (x86 only) Enable support for AVX SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX instructions.

.. ghc-flag:: -mavx2
    :shortdesc: (x86 only) Enable support for AVX2 SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX2 instructions.

.. ghc-flag:: -mavx512cd
    :shortdesc: (x86 only) Enable support for AVX512-CD SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX512-CD instructions.

.. ghc-flag:: -mavx512er
    :shortdesc: (x86 only) Enable support for AVX512-ER SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX512-ER instructions.

.. ghc-flag:: -mavx512f
    :shortdesc: (x86 only) Enable support for AVX512-F SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX512-F instructions.

.. ghc-flag:: -mavx512pf
    :shortdesc: (x86 only) Enable support for AVX512-PF SIMD extensions
    :type: dynamic
    :category: platform-options

    (x86 only) This flag allows the code generator (whether the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`) to emit x86_64 AVX512-PF instructions.

.. ghc-flag:: -msse
    :shortdesc: (x86 only) Use SSE for floating-point operations
    :type: dynamic
    :category: platform-options

    (x86 only) Use the SSE registers and
    instruction set to implement floating point operations when using
    the :ref:`native code generator <native-code-gen>`. This gives a
    substantial performance improvement for floating point, but the
    resulting compiled code will only run on processors that support
    SSE (Intel Pentium 3 and later, or AMD Athlon XP and later). The
    :ref:`LLVM backend <llvm-code-gen>` will also use SSE if your
    processor supports it but detects this automatically so no flag is
    required.

    Since GHC 8.10, SSE2 is assumed to be present on both
    x86 and x86-64 platforms and will be used by default.
    Even when setting this flag, SSE2 will be used instead.

.. ghc-flag:: -msse2
    :shortdesc: (x86 only) Use SSE2 for floating-point operations
    :type: dynamic
    :category: platform-options

    (x86 only, added in GHC 7.0.1) Use the SSE2 registers and
    instruction set to implement floating point operations when using
    the :ref:`native code generator <native-code-gen>`. This gives a
    substantial performance improvement for floating point, but the
    resulting compiled code will only run on processors that support
    SSE2 (Intel Pentium 4 and later, or AMD Athlon 64 and later). The
    :ref:`LLVM backend <llvm-code-gen>` will also use SSE2 if your
    processor supports it but detects this automatically so no flag is
    required.

    Since GHC 8.10, SSE2 is assumed to be present on both
    x86 and x86-64 platforms and will be used by default.

.. ghc-flag:: -msse3
    :shortdesc: (x86 only) Use SSE3 for floating-point operations
    :type: dynamic
    :category: platform-options

    (x86 only) Use the SSE3 instruction set to
    implement some floating point and bit operations
    (whether using the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`).

.. ghc-flag:: -mssse3
    :shortdesc: (x86 only) Use SSSE3 for vector operations
    :type: dynamic
    :category: platform-options

    :since: 9.14.1

    (x86 only) Use the SSSE3 instruction set to
    implement some vector operations
    (whether using the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`).

.. ghc-flag:: -msse4
    :shortdesc: (x86 only) Use SSE4 for floating-point operations
    :type: dynamic
    :category: platform-options

    (x86 only) Use the SSE4 instruction set to
    implement some floating point and bit operations(whether using the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`).

.. ghc-flag:: -msse4.2
    :shortdesc: (x86 only) Use SSE4.2 for floating-point operations
    :type: dynamic
    :category: platform-options

    (x86 only, added in GHC 7.4.1) Use the SSE4.2 instruction set to
    implement some floating point and bit operations,
    whether using the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`. The resulting compiled
    code will only run on processors that support SSE4.2 (Intel Core i7
    and later).

.. ghc-flag:: -mbmi
    :shortdesc: (x86 only) Use BMI1 for bit manipulation operations
    :type: dynamic
    :category: platform-options

    (x86 only) Use the BMI1 instruction set to implement some bit operations.

    Note that GHC currently does not use BMI specific instructions,
    so this flag has no effect when used with the :ref:`native code generator <native-code-gen>`.

.. ghc-flag:: -mbmi2
    :shortdesc: (x86 only) Use BMI2 for bit manipulation operations
    :type: dynamic
    :category: platform-options

    (x86 only, added in GHC 7.4.1) Use the BMI2 instruction set to
    implement some bit operations, whether using the :ref:`native code generator <native-code-gen>`
    or the :ref:`LLVM backend <llvm-code-gen>`.

    The resulting compiled code will only run on processors that support BMI2
    (Intel Haswell and newer, AMD Excavator, Zen and newer).

.. ghc-flag:: -mfma
    :shortdesc: Use native FMA instructions for fused multiply-add floating-point operations
    :type: dynamic
    :category: platform-options

    :default: off by default, except for Aarch64 where it's on by default.

    :since: 9.8.1

    Use native FMA instructions to implement the fused multiply-add floating-point
    operations of the form ``x * y + z``.
    This allows computing a multiplication and addition in a single instruction,
    without an intermediate rounding step.
    Supported architectures: X86 with the FMA3 instruction set (this includes
    most consumer processors since 2013), PowerPC and AArch64.

    When this flag is disabled, GHC falls back to the C implementation of fused
    multiply-add, which might perform non-IEEE-compliant software emulation on
    some platforms (depending on the implementation of the C standard library).

Haddock
-------

.. index::
   single: haddock

.. ghc-flag:: -haddock
    :shortdesc: With this flag GHC will parse Haddock comments and include them
      in the interface file it produces.
    :type: dynamic
    :reverse: -no-haddock
    :category: haddock

    By default, GHC ignores Haddock comments (``-- | ...`` and ``-- ^ ...``)
    and does not check that they're associated with a valid term, such as a
    top-level type-signature.  With this flag GHC will parse Haddock comments
    and include them in the interface file it produces.

    Consider using :ghc-flag:`-Winvalid-haddock` to be informed about discarded
    documentation comments.

Miscellaneous flags
-------------------

.. index::
   single: miscellaneous flags

Some flags only make sense for a particular use case.

.. ghc-flag:: -ghcversion-file ⟨path to ghcversion.h⟩
    :shortdesc: (GHC as a C compiler only) Use this ``ghcversion.h`` file
    :type: dynamic
    :category: misc

    When GHC is used to compile C files, GHC adds package include paths and
    includes ``ghcversion.h`` directly. The compiler will lookup the path for
    the ``ghcversion.h`` file from the ``rts`` package in the package database.
    In some cases, the compiler's package database does not contain the ``rts``
    package, or one wants to specify a specific ``ghcversions.h`` to be
    included. This option can be used to specify the path to the
    ``ghcversions.h`` file to be included. This is primarily intended to be
    used by GHC's build system.

.. ghc-flag:: -H ⟨size⟩
    :shortdesc: Set the minimum size of the heap to ⟨size⟩
    :type: dynamic
    :category: misc

    Set the minimum size of the heap to ⟨size⟩. This option is
    equivalent to ``+RTS -Hsize``, see :ref:`rts-options-gc`.

Other environment variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: environment variables

GHC can also be configured using various environment variables.

.. envvar:: GHC_NO_UNICODE

    When non-empty, disables Unicode diagnostics output regardless of locale settings.
    GHC can usually determine that locale is not Unicode-capable and fallback to ASCII
    automatically, but in some corner cases (e. g., when GHC output is redirected)
    you might hit ``invalid argument (cannot encode character '\8216')``,
    in which case do set ``GHC_NO_UNICODE``.

.. envvar:: GHC_CHARENC

    When set to ``UTF-8`` the compiler will always print UTF-8-encoded output, regardless
    of the current locale.
