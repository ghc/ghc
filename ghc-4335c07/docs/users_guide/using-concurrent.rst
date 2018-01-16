.. _using-concurrent:

Using Concurrent Haskell
------------------------

.. index::
   single: Concurrent Haskell; using

GHC supports Concurrent Haskell by default, without requiring a special
option or libraries compiled in a certain way. To get access to the
support libraries for Concurrent Haskell, just import
:base-ref:`Control.Concurrent.`. More information on Concurrent Haskell is
provided in the documentation for that module.

Optionally, the program may be linked with the :ghc-flag:`-threaded` option (see
:ref:`options-linker`. This provides two benefits:

- It enables the :rts-flag:`-N ⟨x⟩` to be used, which allows threads to run in
  parallelism on a multi-processor or multi-core machine. See :ref:`using-smp`.

- If a thread makes a foreign call (and the call is not marked
  ``unsafe``), then other Haskell threads in the program will continue
  to run while the foreign call is in progress. Additionally,
  ``foreign export``\ ed Haskell functions may be called from multiple
  OS threads simultaneously. See :ref:`ffi-threads`.

The following RTS option(s) affect the behaviour of Concurrent Haskell
programs:

.. index::
   single: RTS options; concurrent

.. rts-flag:: -C ⟨s⟩

    :default: 20 milliseconds

    Sets the context switch interval to ⟨s⟩ seconds.
    A context switch will occur at the next heap block allocation after
    the timer expires (a heap block allocation occurs every 4k of
    allocation). With ``-C0`` or ``-C``, context switches will occur as
    often as possible (at every heap block allocation).

.. _using-smp:

Using SMP parallelism
---------------------

.. index::
   single: parallelism
   single: SMP

GHC supports running Haskell programs in parallel on an SMP (symmetric
multiprocessor).

There's a fine distinction between *concurrency* and *parallelism*:
parallelism is all about making your program run *faster* by making use
of multiple processors simultaneously. Concurrency, on the other hand,
is a means of abstraction: it is a convenient way to structure a program
that must respond to multiple asynchronous events.

However, the two terms are certainly related. By making use of multiple
CPUs it is possible to run concurrent threads in parallel, and this is
exactly what GHC's SMP parallelism support does. But it is also possible
to obtain performance improvements with parallelism on programs that do
not use concurrency. This section describes how to use GHC to compile
and run parallel programs, in :ref:`lang-parallel` we describe the
language features that affect parallelism.

.. _parallel-compile-options:

Compile-time options for SMP parallelism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to make use of multiple CPUs, your program must be linked with
the :ghc-flag:`-threaded` option (see :ref:`options-linker`). Additionally, the
following compiler options affect parallelism:

.. ghc-flag:: -feager-blackholing
    :shortdesc: Turn on :ref:`eager blackholing <parallel-compile-options>`
    :type: dynamic
    :category:
    :noindex:

    Blackholing is the act of marking a thunk (lazy computation) as
    being under evaluation. It is useful for three reasons: firstly it
    lets us detect certain kinds of infinite loop (the
    ``NonTermination`` exception), secondly it avoids certain kinds of
    space leak, and thirdly it avoids repeating a computation in a
    parallel program, because we can tell when a computation is already
    in progress.

    The option :ghc-flag:`-feager-blackholing` causes each thunk to be
    blackholed as soon as evaluation begins. The default is "lazy
    blackholing", whereby thunks are only marked as being under
    evaluation when a thread is paused for some reason. Lazy blackholing
    is typically more efficient (by 1-2% or so), because most thunks
    don't need to be blackholed. However, eager blackholing can avoid
    more repeated computation in a parallel program, and this often
    turns out to be important for parallelism.

    We recommend compiling any code that is intended to be run in
    parallel with the :ghc-flag:`-feager-blackholing` flag.

.. _parallel-options:

RTS options for SMP parallelism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two ways to run a program on multiple processors: call
:base-ref:`Control.Concurrent.setNumCapabilities` from your program, or
use the RTS :rts-flag:`-N ⟨x⟩` options.

.. rts-flag:: -N ⟨x⟩
              -maxN ⟨x⟩

    Use ⟨x⟩ simultaneous threads when running the program.

    The runtime manages a set of virtual processors, which we call
    *capabilities*, the number of which is determined by the ``-N``
    option. Each capability can run one Haskell thread at a time, so the
    number of capabilities is equal to the number of Haskell threads
    that can run physically in parallel. A capability is animated by one
    or more OS threads; the runtime manages a pool of OS threads for
    each capability, so that if a Haskell thread makes a foreign call
    (see :ref:`ffi-threads`) another OS thread can take over that
    capability.

    Normally ⟨x⟩ should be chosen to match the number of CPU cores on
    the machine [1]_. For example, on a dual-core machine we would
    probably use ``+RTS -N2 -RTS``.

    Omitting ⟨x⟩, i.e. ``+RTS -N -RTS``, lets the runtime choose the
    value of ⟨x⟩ itself based on how many processors are in your
    machine.

    With ``-maxN⟨x⟩``, i.e. ``+RTS -maxN3 -RTS``, the runtime will choose
    at most (x), also limited by the number of processors on the system.
    Omitting (x) is an error, if you need a default use option ``-N``.

    Be careful when using all the processors in your machine: if some of
    your processors are in use by other programs, this can actually harm
    performance rather than improve it. Asking GHC to create more capabilities
    than you have physical threads is almost always a bad idea.

    Setting ``-N`` also has the effect of enabling the parallel garbage
    collector (see :ref:`rts-options-gc`).

    The current value of the ``-N`` option is available to the Haskell
    program via ``Control.Concurrent.getNumCapabilities``, and it may be
    changed while the program is running by calling
    ``Control.Concurrent.setNumCapabilities``.

The following options affect the way the runtime schedules threads on
CPUs:

.. rts-flag:: -qa

    Use the OS's affinity facilities to try to pin OS threads to CPU
    cores.

    When this option is enabled, the OS threads for a capability :math:`i` are
    bound to the CPU core :math:`i` using the API provided by the OS for setting
    thread affinity. e.g. on Linux GHC uses ``sched_setaffinity()``.

    Depending on your workload and the other activity on the machine,
    this may or may not result in a performance improvement. We
    recommend trying it out and measuring the difference.

.. rts-flag:: -qm

    Disable automatic migration for load balancing. Normally the runtime
    will automatically try to schedule threads across the available CPUs
    to make use of idle CPUs; this option disables that behaviour. Note
    that migration only applies to threads; sparks created by ``par``
    are load-balanced separately by work-stealing.

    This option is probably only of use for concurrent programs that
    explicitly schedule threads onto CPUs with
    :base-ref:`Control.Concurrent.forkOn`.

Hints for using SMP parallelism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add the :rts-flag:`-s [⟨file⟩]` RTS option when running the program to see
timing stats, which will help to tell you whether your program got faster by
using more CPUs or not. If the user time is greater than the elapsed time, then
the program used more than one CPU. You should also run the program without
:rts-flag:`-N ⟨x⟩` for comparison.

The output of ``+RTS -s`` tells you how many "sparks" were created and
executed during the run of the program (see :ref:`rts-options-gc`),
which will give you an idea how well your ``par`` annotations are
working.

GHC's parallelism support has improved in 6.12.1 as a result of much
experimentation and tuning in the runtime system. We'd still be
interested to hear how well it works for you, and we're also interested
in collecting parallel programs to add to our benchmarking suite.

.. [1] Whether hyperthreading cores should be counted or not is an open
       question; please feel free to experiment and let us know what results you
       find.
