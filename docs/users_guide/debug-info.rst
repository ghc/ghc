Debugging compiled programs
===========================

Since the 7.10 release GHC can emit a debugging information to help debugging
tools understand the code that GHC produces. This debugging information is
useable by most UNIX debugging tools.

.. ghc-flag:: -g
              -g⟨n⟩
    :shortdesc: Produce DWARF debug information in compiled object files.
        ⟨n⟩ can be 0, 1, or 2, with higher numbers producing richer
        output. If ⟨n⟩ is omitted, level 2 is assumed.
    :type: dynamic
    :category: debugging

    :since: 7.10, numeric levels since 8.0

    Emit debug information in object code. Currently only DWARF debug
    information is supported on x86-64 and i386. Currently debug levels 0
    through 3 are accepted, with 0 disabling debug information production
    and higher numbers producing richer output. If ⟨n⟩ is omitted, level 2
    is assumed.


Tutorial
--------

Let's consider a simple example,

.. code-block:: hs
   :linenos:

    -- fib.hs
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    main :: IO ()
    main = print $ fib 50

Let's first see how execution flows through this program. We start by telling
GHC that we want debug information,

.. code-block:: sh

    $ ghc -g -rtsopts fib.hs

Here we used the ``-g`` option to inform GHC that it should add debugging
information in the produced binary. There are three levels of debugging
output: ``-g0`` (no debugging information, the default), ``-g1`` (sufficient for
basic backtraces), ``-g2`` (or just ``-g`` for short; emitting everything GHC knows).
Note that this debugging information does not affect the optimizations performed
by GHC.

.. tip::
   Under Mac OS X debug information is kept apart from the executable. After
   compiling the executable you'll need to use the ``dsymutil`` utility to
   extract the debugging information and place them in the debug archive,

   .. code-block:: sh

      $ dsymutil fib

   This should produce a file named ``fib.dSYM``.

Now let's have a look at the flow of control. For this we can just start our
program under ``gdb`` (or an equivalent debugger) as we would any other native
executable,

.. code-block:: none

    $ gdb --args ./Fib +RTS -V0
    Reading symbols from Fib...done.
    (gdb) run
    Starting program: /opt/exp/ghc/ghc-dwarf/Fib
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".
    ^C
    Program received signal SIGINT, Interrupt.
    0x000000000064fc7c in cfy4_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:424
    424     minusInteger x y = inline plusInteger x (inline negateInteger y)
    (gdb)

Here we have used the runtime system's ``-V0`` option to disable the RTS's
periodic timer which may interfere with our debugging session. Upon breaking
into the program ``gdb`` shows us a location in our source program corresponding
to the current point of execution.

Moreover, we can ask ``gdb`` to tell us the flow of execution that lead us to
this point in the program,

.. code-block:: none

   (gdb) bt
   #0  0x000000000064fc7c in cfy4_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:424
   #1  0x00000000006eb0c0 in ?? ()
   #2  0x000000000064301c in cbuV_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:323
   #3  0x000000000064311b in integerzmgmp_GHCziIntegerziType_eqInteger_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:312
   #4  0x0000000000406eca in roz_info () at Fib.hs:2
   #5  0x00000000006eb0c0 in ?? ()
   #6  0x000000000064f075 in cfru_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:412
   #7  0x00000000006eb0c0 in ?? ()
   #8  0x000000000064f075 in cfru_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:412
   #9  0x00000000006eb0c0 in ?? ()
   #10 0x000000000064eefe in integerzmgmp_GHCziIntegerziType_plusInteger_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:393
   ...
   #64 0x0000000000643ac8 in integerzmgmp_GHCziIntegerziType_ltIntegerzh_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:343
   #65 0x00000000004effcc in base_GHCziShow_zdwintegerToString_info () at libraries/base/GHC/Show.hs:443
   #66 0x00000000004f0795 in base_GHCziShow_zdfShowIntegerzuzdcshow_info () at libraries/base/GHC/Show.hs:145
   #67 0x000000000048892b in cdGW_info () at libraries/base/GHC/IO/Handle/Text.hs:595
   #68 0x0000000000419cb2 in base_GHCziBase_thenIO1_info () at libraries/base/GHC/Base.hs:1072


.. hint::

    Here we notice the first bit of the stack trace has many unidentified stack
    frames at address ``0x006eb0c0``. If we ask ``gdb`` about this location, we
    find that these frames are actually STG update closures,

    .. code-block:: none

        (gdb) print/a 0x006eb0c0
        $1 = 0x6eb0c0 <stg_upd_frame_info>

    The reason ``gdb`` doesn't show this symbol name in the backtrace output is an
    infidelity in its interpretation of debug information, which assumes an
    invariant preserved in C but not Haskell programs. Unfortunately it is
    necessary to work around this manually until this behivior is fixed
    upstream.

.. note::

    Because of the aggressive optimization that GHC performs to the programs it
    compiles it is quite difficult to pin-point exactly which point in the source
    program a given machine instruction should be attributed to. In fact,
    internally GHC associates each instruction with a **set** of source
    locations. When emitting the standard debug information used by ``gdb`` and
    other language-agnostic debugging tools, GHC is forced to heuristically
    choose one location from among this set.

    For this reason we should be cautious when interpretting the source locations
    provided by GDB. While these locations will usually be in some sense
    "correct", they aren't always useful. This is why profiling tools targetting
    Haskell should supplement the standard source location information with
    GHC-specific annotations (emitted with ``-g2``) when assigning costs.

Indeed, we can even set breakpoints,

.. code-block:: none

    (gdb) break fib.hs:4
    Breakpoint 1 at 0x406c60: fib.hs:4. (5 locations)
    (gdb) run
    Starting program: /opt/exp/ghc/ghc-dwarf/Fib

    Breakpoint 1, c1RV_info () at Fib.hs:4
    4        fib n = fib (n-1) + fib (n-2)
    (gdb) bt
    #0  c1RV_info () at Fib.hs:4
    #1  0x00000000006eb0c0 in ?? ()
    #2  0x0000000000643ac8 in integerzmgmp_GHCziIntegerziType_ltIntegerzh_info () at libraries/integer-gmp/src/GHC/Integer/Type.hs:343
    #3  0x00000000004effcc in base_GHCziShow_zdwintegerToString_info () at libraries/base/GHC/Show.hs:443
    #4  0x00000000004f0795 in base_GHCziShow_zdfShowIntegerzuzdcshow_info () at libraries/base/GHC/Show.hs:145
    #5  0x000000000048892b in cdGW_info () at libraries/base/GHC/IO/Handle/Text.hs:595
    #6  0x0000000000419cb2 in base_GHCziBase_thenIO1_info () at libraries/base/GHC/Base.hs:1072
    #7  0x00000000006ebcb0 in ?? () at rts/Exception.cmm:332
    #8  0x00000000006e7320 in ?? ()
    (gdb)

Due to the nature of GHC's heap and the heavy optimization that it performs, it
is quite difficult to probe the values of bindings at runtime. In this way, the
debugging experience of a Haskell program with DWARF support is still a bit
impoverished compared to typical imperative debuggers.

Requesting a stack trace from Haskell code
------------------------------------------

GHC's runtime system has built-in support for collecting stack trace information
from a running Haskell program. This currently requires that the ``libdw``
library from the ``elfutils`` package is available. Of course, the backtrace
will be of little use unless debug information is available in the executable
and its dependent libraries.

Stack trace functionality is exposed for use by Haskell programs in the
:base-ref:`GHC.ExecutionStack.` module. See the Haddock
documentation in this module for details regarding usage.

.. _backtrace-signal:

Requesting a stack trace with ``SIGQUIT``
-----------------------------------------

On POSIX-compatible platforms GHC's runtime system (when built with ``libdw``
support) will produce a stack trace on ``stderr`` when a ``SIGQUIT`` signal is
received (on many systems this signal can be sent using :kbd:`Ctrl-\\`). For
instance (using the same ``fib.hs`` as above),

.. code-block:: sh

    $ ./fib  &  killall -SIGQUIT fib

    Caught SIGQUIT; Backtrace:
    0x7f3176b15dd8    dwfl_thread_getframes (/usr/lib/x86_64-linux-gnu/libdw-0.163.so)
    0x7f3176b1582f    (null) (/usr/lib/x86_64-linux-gnu/libdw-0.163.so)
    0x7f3176b15b57    dwfl_getthreads (/usr/lib/x86_64-linux-gnu/libdw-0.163.so)
    0x7f3176b16150    dwfl_getthread_frames (/usr/lib/x86_64-linux-gnu/libdw-0.163.so)
          0x6dc857    libdwGetBacktrace (rts/Libdw.c:248.0)
          0x6e6126    backtrace_handler (rts/posix/Signals.c:541.0)
    0x7f317677017f    (null) (/lib/x86_64-linux-gnu/libc-2.19.so)
          0x642e1c    integerzmgmp_GHCziIntegerziType_eqIntegerzh_info (libraries/integer-gmp/src/GHC/Integer/Type.hs:320.1)
          0x643023    integerzmgmp_GHCziIntegerziType_eqInteger_info (libraries/integer-gmp/src/GHC/Integer/Type.hs:312.1)
          0x406eca    roz_info (/opt/exp/ghc/ghc-dwarf//Fib.hs:2.1)
          0x6eafc0    stg_upd_frame_info (rts/Updates.cmm:31.1)
          0x64ee06    integerzmgmp_GHCziIntegerziType_plusInteger_info (libraries/integer-gmp/src/GHC/Integer/Type.hs:393.1)
          0x6eafc0    stg_upd_frame_info (rts/Updates.cmm:31.1)
    ...
          0x6439d0    integerzmgmp_GHCziIntegerziType_ltIntegerzh_info (libraries/integer-gmp/src/GHC/Integer/Type.hs:343.1)
          0x4efed4    base_GHCziShow_zdwintegerToString_info (libraries/base/GHC/Show.hs:442.1)
          0x4f069d    base_GHCziShow_zdfShowIntegerzuzdcshow_info (libraries/base/GHC/Show.hs:145.5)
          0x488833    base_GHCziIOziHandleziText_zdwa8_info (libraries/base/GHC/IO/Handle/Text.hs:582.1)
          0x6ebbb0    stg_catch_frame_info (rts/Exception.cmm:370.1)
          0x6e7220    stg_stop_thread_info (rts/StgStartup.cmm:42.1)


Implementor's notes: DWARF annotations
--------------------------------------

.. note::
   Most users don't need to worry about the details described in this section.
   This discussion is primarily targeted at tooling authors who need to
   interpret the GHC-specific DWARF annotations contained in compiled binaries.

When invoked with the ``-g`` flag GHC will produce standard `DWARF v4
<http://dwarfstd.org/>`__ debugging information. This format is used by nearly
all POSIX-compliant targets and can be used by debugging and performance tools
(e.g. ``gdb``, ``lldb``, and ``perf``) to understand the structure of
GHC-compiled programs.

In particular GHC produces the following DWARF sections,

``.debug_info``
  Debug information entities (DIEs) describing all of the basic blocks in the
  compiled program.

``.debug_line``
  Line number information necessary to map instruction addresses to line numbers
  in the source program.

  Note that the line information in this section is not nearly as rich as the
  information provided in ``.debug_info``. Whereas ``.debug_line`` requires that
  each instruction is assigned exactly one source location, the DIEs in
  ``.debug_info`` can be used to identify all relevant sources locations.

``.debug_frames``
  Call frame information (CFI) necessary for stack unwinding to produce a call
  stack trace.

``.debug_arange``
  Address range information necessary for efficient lookup in debug information.


Debugging information entities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC may produce the following standard DIEs in the ``.debug_info`` section,

``DW_TAG_compile_unit``
  Represents a compilation unit (e.g. a Haskell module).

``DW_TAG_subprogram``
  Represents a C-\\- top-level basic block.

``DW_TAG_lexical_block``
  Represents a C-\\- basic block. Note that this is a slight departure from the
  intended meaning of this DIE type as it does not necessarily reflect
  lexical scope in the source program.

As GHC's compilation products don't map perfectly onto DWARF constructs,
GHC takes advantage of the extensibility of the DWARF standard to provide
additional information.

Unfortunately DWARF isn't expressive enough to fully describe the code
that GHC produces. This is most apparent in the case of line
information, where GHC is forced to choose some between a variety of
possible originating source locations. This limits the usefulness of
DWARF information with traditional statistical profiling tools. For
profiling it is recommended that one use the extended debugging
information. See the *Profiling* section below.

In addition to the usual DIEs specified by the DWARF specification, GHC
produces a variety of others using the vendor-extensibility regions of
the tag and attribute space.

``DW_TAG_ghc_src_note``
^^^^^^^^^^^^^^^^^^^^^^^

``DW_TAG_ghc_src_note`` DIEs (tag 0x5b01) are found as children of
``DW_TAG_lexical_block`` DIEs. They describe source spans which gave rise to the
block; formally these spans are causally responsible for produced code: changes
to code in the given span may change the code within the block; conversely
changes outside the span are guaranteed not to affect the code in the block.

Spans are described with the following attributes,

``DW_AT_ghc_span_file`` (0x2b00, string)
  the name of the source file

``DW_AT_ghc_span_start_line`` (0x2b01, integer)
  the line number of the beginning of the span

``DW_AT_ghc_span_start_col`` (0x2b02, integer)
  the column number of the beginning of the span

``DW_AT_ghc_span_end_line`` (0x2b03, integer)
  the line number of the end of the span

``DW_AT_ghc_span_end_col`` (0x2b04, integer)
  the column number of the end of the span


Further Reading
---------------

For more information about the debug information produced by GHC see
Peter Wortmann's PhD thesis, `*Profiling Optimized Haskell: Causal
Analysis and Implementation* <http://etheses.whiterose.ac.uk/8321/>`__.
