.. _runtime-control:

Running a compiled program
==========================

.. index::
   single: runtime control of Haskell programs
   single: running, compiled program
   single: RTS options

To make an executable program, the GHC system compiles your code and
then links it with a non-trivial runtime system (RTS), which handles
storage management, thread scheduling, profiling, and so on.

The RTS has a lot of options to control its behaviour. For example, you
can change the context-switch interval, the default size of the heap,
and enable heap profiling. These options can be passed to the runtime
system in a variety of different ways; the next section
(:ref:`setting-rts-options`) describes the various methods, and the
following sections describe the RTS options themselves.

.. _setting-rts-options:

Setting RTS options
-------------------

.. index::
   single: RTS options, setting

There are four ways to set RTS options:

-  on the command line between ``+RTS ... -RTS``, when running the
   program (:ref:`rts-opts-cmdline`)

-  at compile-time, using :ghc-flag:`-with-rtsopts=⟨opts⟩`
   (:ref:`rts-opts-compile-time`)

-  with the environment variable :envvar:`GHCRTS`
   (:ref:`rts-options-environment`)

-  by overriding "hooks" in the runtime system (:ref:`rts-hooks`)

.. _rts-opts-cmdline:

Setting RTS options on the command line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: +RTS
   single: -RTS
   single: --RTS

If you set the :ghc-flag:`-rtsopts[=⟨none|some|all⟩]` flag appropriately when
linking (see :ref:`options-linker`), you can give RTS options on the command
line when running your program.

When your Haskell program starts up, the RTS extracts command-line
arguments bracketed between ``+RTS`` and ``-RTS`` as its own. For example:

.. code-block:: none

    $ ghc prog.hs -rtsopts
    [1 of 1] Compiling Main             ( prog.hs, prog.o )
    Linking prog ...
    $ ./prog -f +RTS -H32m -S -RTS -h foo bar

The RTS will snaffle ``-H32m -S`` for itself, and the remaining
arguments ``-f -h foo bar`` will be available to your program if/when it
calls ``System.Environment.getArgs``.

No ``-RTS`` option is required if the runtime-system options extend to
the end of the command line, as in this example:

.. code-block:: none

    % hls -ltr /usr/etc +RTS -A5m

If you absolutely positively want all the rest of the options in a
command line to go to the program (and not the RTS), use a
``--RTS``.

As always, for RTS options that take ⟨size⟩s: If the last character of
⟨size⟩ is a K or k, multiply by 1000; if an M or m, by 1,000,000; if a G
or G, by 1,000,000,000. (And any wraparound in the counters is *your*
fault!)

Giving a ``+RTS -?`` RTS option option will print out the RTS
options actually available in your program (which vary, depending on how
you compiled).

.. note::
    Since GHC is itself compiled by GHC, you can change RTS options in
    the compiler using the normal ``+RTS ... -RTS`` combination. For instance, to set
    the maximum heap size for a compilation to 128M, you would add
    ``+RTS -M128m -RTS`` to the command line.

.. _rts-opts-compile-time:

Setting RTS options at compile time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC lets you change the default RTS options for a program at compile
time, using the ``-with-rtsopts`` flag (:ref:`options-linker`). A common
use for this is to give your program a default heap and/or stack size
that is greater than the default. For example, to set ``-H128m -K64m``,
link with ``-with-rtsopts="-H128m -K64m"``.

.. _rts-options-environment:

Setting RTS options with the ``GHCRTS`` environment variable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: RTS options; from the environment
   single: environment variable; for setting RTS options
   single: GHCRTS environment variable

.. envvar:: GHCRTS

    If the ``-rtsopts`` flag is set to something other than ``none`` or ``ignoreAll``
    when linking, RTS options are also taken from the environment variable
    :envvar:`GHCRTS`. For example, to set the maximum heap size to 2G
    for all GHC-compiled programs (using an ``sh``\-like shell):

    .. code-block:: sh

        GHCRTS='-M2G'
        export GHCRTS

    RTS options taken from the :envvar:`GHCRTS` environment variable can be
    overridden by options given on the command line.

.. tip::
    Setting something like ``GHCRTS=-M2G`` in your environment is a
    handy way to avoid Haskell programs growing beyond the real memory in
    your machine, which is easy to do by accident and can cause the machine
    to slow to a crawl until the OS decides to kill the process (and you
    hope it kills the right one).

.. _rts-hooks:

"Hooks" to change RTS behaviour
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: hooks; RTS
   single: RTS hooks
   single: RTS behaviour, changing

GHC lets you exercise rudimentary control over certain RTS settings for
any given program, by compiling in a "hook" that is called by the
run-time system. The RTS contains stub definitions for these hooks, but
by writing your own version and linking it on the GHC command line, you
can override the defaults.

Owing to the vagaries of DLL linking, these hooks don't work under
Windows when the program is built dynamically.

Runtime events
##############

You can change the messages printed when the runtime system "blows up,"
e.g., on stack overflow. The hooks for these are as follows:

.. c:function:: void OutOfHeapHook (unsigned long, unsigned long)

    The heap-overflow message.

.. c:function:: void StackOverflowHook (long int)

    The stack-overflow message.

.. c:function:: void MallocFailHook (long int)

    The message printed if ``malloc`` fails.

Event log output
################

Furthermore GHC lets you specify the way event log data (see :rts-flag:`-l`) is
written through a custom :c:type:`EventLogWriter`:

.. c:type:: EventLogWriter

    A sink of event-log data.

    .. c:member:: void initEventLogWriter(void)

        Initializes your :c:type:`EventLogWriter`. This is optional.

    .. c:member:: bool writeEventLog(void *eventlog, size_t eventlog_size)

        Hands buffered event log data to your event log writer.
        Required for a custom :c:type:`EventLogWriter`.

    .. c:member:: void flushEventLog(void)

        Flush buffers (if any) of your custom :c:type:`EventLogWriter`. This can
        be ``NULL``.

    .. c:member:: void stopEventLogWriter(void)

        Called when event logging is about to stop. This can be ``NULL``.

.. _rts-options-misc:

Miscellaneous RTS options
-------------------------

.. rts-flag:: --install-signal-handlers=⟨yes|no⟩

    If yes (the default), the RTS installs signal handlers to catch
    things like :kbd:`Ctrl-C`. This option is primarily useful for when you are
    using the Haskell code as a DLL, and want to set your own signal
    handlers.

    Note that even with ``--install-signal-handlers=no``, the RTS
    interval timer signal is still enabled. The timer signal is either
    SIGVTALRM or SIGALRM, depending on the RTS configuration and OS
    capabilities. To disable the timer signal, use the ``-V0`` RTS
    option (see :rts-flag:`-V ⟨secs⟩`).

.. rts-flag:: --install-seh-handlers=⟨yes|no⟩

    If yes (the default), the RTS on Windows installs exception handlers to
    catch unhandled exceptions using the Windows exception handling mechanism.
    This option is primarily useful for when you are using the Haskell code as a
    DLL, and don't want the RTS to ungracefully terminate your application on
    erros such as segfaults.

.. rts-flag:: --generate-crash-dumps

    If yes (the default), the RTS on Windows will generate a core dump on
    any crash. These dumps can be inspected using debuggers such as WinDBG.
    The dumps record all code, registers and threading information at the time
    of the crash. Note that this implies `--install-seh-handlers=yes`.

.. rts-flag:: --generate-stack-traces=<yes|no>

    If yes (the default), the RTS on Windows will generate a stack trace on
    crashes if exception handling are enabled. In order to get more information
    in compiled executables, C code or DLLs symbols need to be available.

.. rts-flag:: -xm ⟨address⟩

    .. index::
       single: -xm; RTS option

    .. warning::

        This option is for working around memory allocation
        problems only. Do not use unless GHCi fails with a message like
        “\ ``failed to mmap() memory below 2Gb``\ ”. If you need to use this
        option to get GHCi working on your machine, please file a bug.

    On 64-bit machines, the RTS needs to allocate memory in the low 2Gb
    of the address space. Support for this across different operating
    systems is patchy, and sometimes fails. This option is there to give
    the RTS a hint about where it should be able to allocate memory in
    the low 2Gb of the address space. For example,
    ``+RTS -xm20000000 -RTS`` would hint that the RTS should allocate
    starting at the 0.5Gb mark. The default is to use the OS's built-in
    support for allocating memory in the low 2Gb if available (e.g.
    ``mmap`` with ``MAP_32BIT`` on Linux), or otherwise ``-xm40000000``.

.. rts-flag:: -xq ⟨size⟩

    :default: 100k

    This option relates to allocation limits; for more about this see
    :base-ref:`GHC.Conc.enableAllocationLimit`.
    When a thread hits its allocation limit, the RTS throws an exception
    to the thread, and the thread gets an additional quota of allocation
    before the exception is raised again, the idea being so that the
    thread can execute its exception handlers. The ``-xq`` controls the
    size of this additional quota.

.. _rts-options-gc:

RTS options to control the garbage collector
--------------------------------------------

.. index::
   single: garbage collector; options
   single: RTS options; garbage collection

There are several options to give you precise control over garbage
collection. Hopefully, you won't need any of these in normal operation,
but there are several things that can be tweaked for maximum
performance.

.. rts-flag:: -A ⟨size⟩

    :default: 1MB

    .. index::
       single: allocation area, size

    Set the allocation area size used by the garbage
    collector. The allocation area (actually generation 0 step 0) is
    fixed and is never resized (unless you use :rts-flag:`-H [⟨size⟩]`, below).

    Increasing the allocation area size may or may not give better
    performance (a bigger allocation area means worse cache behaviour
    but fewer garbage collections and less promotion).

    With only 1 generation (e.g. ``-G1``, see :rts-flag:`-G ⟨generations⟩`) the
    ``-A`` option specifies the minimum allocation area, since the actual size
    of the allocation area will be resized according to the amount of data in
    the heap (see :rts-flag:`-F ⟨factor⟩`, below).

.. rts-flag:: -AL ⟨size⟩

    :default: :rts-flag:`-A <-A ⟨size⟩>` value
    :since: 8.2.1

    .. index::
       single: allocation area for large objects, size

    Sets the limit on the total size of "large objects" (objects
    larger than about 3KB) that can be allocated before a GC is
    triggered. By default this limit is the same as the :rts-flag:`-A <-A
    ⟨size⟩>` value.

    Large objects are not allocated from the normal allocation area
    set by the ``-A`` flag, which is why there is a separate limit for
    these.  Large objects tend to be much rarer than small objects, so
    most programs hit the ``-A`` limit before the ``-AL`` limit.  However,
    the ``-A`` limit is per-capability, whereas the ``-AL`` limit is global,
    so as ``-N`` gets larger it becomes more likely that we hit the
    ``-AL`` limit first.  To counteract this, it might be necessary to
    use a larger ``-AL`` limit when using a large ``-N``.

    To see whether you're making good use of all the memory reseverd
    for the allocation area (``-A`` times ``-N``), look at the output of
    ``+RTS -S`` and check whether the amount of memory allocated between
    GCs is equal to ``-A`` times ``-N``. If not, there are two possible
    remedies: use ``-n`` to set a nursery chunk size, or use ``-AL`` to
    increase the limit for large objects.

.. rts-flag:: -O ⟨size⟩

    :default: 1m

    .. index::
       single: old generation, size

    Set the minimum size of the old generation. The old generation is collected
    whenever it grows to this size or the value of the :rts-flag:`-F ⟨factor⟩`
    option multiplied by the size of the live data at the previous major
    collection, whichever is larger.

.. rts-flag:: -n ⟨size⟩

    :default: 4m with :rts-flag:`-A16m <-A ⟨size⟩>` or larger, otherwise 0.

    .. index::
       single: allocation area, chunk size

    [Example: ``-n4m`` ] When set to a non-zero value, this
    option divides the allocation area (``-A`` value) into chunks of the
    specified size. During execution, when a processor exhausts its
    current chunk, it is given another chunk from the pool until the
    pool is exhausted, at which point a collection is triggered.

    This option is only useful when running in parallel (``-N2`` or
    greater). It allows the processor cores to make better use of the
    available allocation area, even when cores are allocating at
    different rates. Without ``-n``, each core gets a fixed-size
    allocation area specified by the ``-A``, and the first core to
    exhaust its allocation area triggers a GC across all the cores. This
    can result in a collection happening when the allocation areas of
    some cores are only partially full, so the purpose of the ``-n`` is
    to allow cores that are allocating faster to get more of the
    allocation area. This means less frequent GC, leading a lower GC
    overhead for the same heap size.

    This is particularly useful in conjunction with larger ``-A``
    values, for example ``-A64m -n4m`` is a useful combination on larger core
    counts (8+).

.. rts-flag:: -c

    .. index::
       single: garbage collection; compacting
       single: compacting garbage collection

    Use a compacting algorithm for collecting the oldest generation. By
    default, the oldest generation is collected using a copying
    algorithm; this option causes it to be compacted in-place instead.
    The compaction algorithm is slower than the copying algorithm, but
    the savings in memory use can be considerable.

    For a given heap size (using the :ghc-flag:`-H ⟨size⟩` option), compaction
    can in fact reduce the GC cost by allowing fewer GCs to be performed. This
    is more likely when the ratio of live data to heap size is high, say
    greater than 30%.

    .. note::
       Compaction doesn't currently work when a single generation is
       requested using the ``-G1`` option.

.. rts-flag:: -c ⟨n⟩

    :default: 30

    Automatically enable compacting collection when the live data exceeds ⟨n⟩%
    of the maximum heap size (see the :rts-flag:`-M ⟨size⟩` option). Note that
    the maximum heap size is unlimited by default, so this option has no effect
    unless the maximum heap size is set with :rts-flag:`-M ⟨size⟩`.

.. rts-flag:: -F ⟨factor⟩

    :default: 2

    .. index::
       single: heap size, factor

    This option controls the amount of memory reserved for
    the older generations (and in the case of a two space collector the
    size of the allocation area) as a factor of the amount of live data.
    For example, if there was 2M of live data in the oldest generation
    when we last collected it, then by default we'll wait until it grows
    to 4M before collecting it again.

    The default seems to work well here. If you have plenty of memory, it is
    usually better to use ``-H ⟨size⟩`` (see :rts-flag:`-H [⟨size⟩]`) than to
    increase :rts-flag:`-F ⟨factor⟩`.

    The :rts-flag:`-F ⟨factor⟩` setting will be automatically reduced by the garbage
    collector when the maximum heap size (the :rts-flag:`-M ⟨size⟩` setting) is approaching.

.. rts-flag:: -G ⟨generations⟩

    :default: 2

    .. index::
       single: generations, number of

    Set the number of generations used by the garbage
    collector. The default of 2 seems to be good, but the garbage
    collector can support any number of generations. Anything larger
    than about 4 is probably not a good idea unless your program runs
    for a *long* time, because the oldest generation will hardly ever
    get collected.

    Specifying 1 generation with ``+RTS -G1`` gives you a simple 2-space
    collector, as you would expect. In a 2-space collector, the :rts-flag:`-A
    ⟨size⟩` option specifies the *minimum* allocation area size, since the
    allocation area will grow with the amount of live data in the heap. In a
    multi-generational collector the allocation area is a fixed size (unless
    you use the :rts-flag:`-H [⟨size⟩]` option).

.. rts-flag:: -qg ⟨gen⟩

    :default: 0
    :since: 6.12.1

    Use parallel GC in generation ⟨gen⟩ and higher. Omitting ⟨gen⟩ turns off the
    parallel GC completely, reverting to sequential GC.

    The default parallel GC settings are usually suitable for parallel programs
    (i.e. those using :base-ref:`GHC.Conc.par`, Strategies, or with
    multiple threads). However, it is sometimes beneficial to enable the
    parallel GC for a single-threaded sequential program too, especially if the
    program has a large amount of heap data and GC is a significant fraction of
    runtime. To use the parallel GC in a sequential program, enable the parallel
    runtime with a suitable :rts-flag:`-N ⟨x⟩` option, and additionally it might
    be beneficial to restrict parallel GC to the old generation with ``-qg1``.

.. rts-flag:: -qb ⟨gen⟩

    :default: 1 for :rts-flag:`-A <-A ⟨size⟩>` < 32M, 0 otherwise
    :since: 6.12.1

    Use load-balancing in the parallel GC in generation ⟨gen⟩ and higher.
    Omitting ⟨gen⟩ disables load-balancing entirely.

    Load-balancing shares out the work of GC between the available
    cores. This is a good idea when the heap is large and we need to
    parallelise the GC work, however it is also pessimal for the short
    young-generation collections in a parallel program, because it can
    harm locality by moving data from the cache of the CPU where is it
    being used to the cache of another CPU. Hence the default is to do
    load-balancing only in the old-generation. In fact, for a parallel
    program it is sometimes beneficial to disable load-balancing
    entirely with ``-qb``.

.. rts-flag:: -qn ⟨x⟩

    :default: the value of :rts-flag:`-N <-N ⟨x⟩>` or the number of CPU cores,
              whichever is smaller.
    :since: 8.2.1

    .. index::
       single: GC threads, setting the number of

    By default, all of the capabilities participate in parallel
    garbage collection.  If we want to use a very large ``-N`` value,
    however, this can reduce the performance of the GC.  For this
    reason, the ``-qn`` flag can be used to specify a lower number for
    the threads that should participate in GC.  During GC, if there
    are more than this number of workers active, some of them will
    sleep for the duration of the GC.

    The ``-qn`` flag may be useful when running with a large ``-A`` value
    (so that GC is infrequent), and a large ``-N`` value (so as to make
    use of hyperthreaded cores, for example).  For example, on a
    24-core machine with 2 hyperthreads per core, we might use
    ``-N48 -qn24 -A128m`` to specify that the mutator should use
    hyperthreads but the GC should only use real cores.  Note that
    this configuration would use 6GB for the allocation area.

.. rts-flag:: -H [⟨size⟩]

    :default: 0

    .. index::
       single: heap size, suggested

    This option provides a "suggested heap size" for the garbage collector.
    Think of ``-Hsize`` as a variable :rts-flag:`-A ⟨size⟩` option.  It says: I
    want to use at least ⟨size⟩ bytes, so use whatever is left over to increase
    the ``-A`` value.

    This option does not put a *limit* on the heap size: the heap may
    grow beyond the given size as usual.

    If ⟨size⟩ is omitted, then the garbage collector will take the size
    of the heap at the previous GC as the ⟨size⟩. This has the effect of
    allowing for a larger ``-A`` value but without increasing the
    overall memory requirements of the program. It can be useful when
    the default small ``-A`` value is suboptimal, as it can be in
    programs that create large amounts of long-lived data.

.. rts-flag:: -I ⟨seconds⟩

    :default: 0.3 seconds

    .. index::
       single: idle GC

    In the threaded and SMP versions of the RTS (see
    :ghc-flag:`-threaded`, :ref:`options-linker`), a major GC is automatically
    performed if the runtime has been idle (no Haskell computation has
    been running) for a period of time. The amount of idle time which
    must pass before a GC is performed is set by the ``-I ⟨seconds⟩``
    option. Specifying ``-I0`` disables the idle GC.

    For an interactive application, it is probably a good idea to use
    the idle GC, because this will allow finalizers to run and
    deadlocked threads to be detected in the idle time when no Haskell
    computation is happening. Also, it will mean that a GC is less
    likely to happen when the application is busy, and so responsiveness
    may be improved. However, if the amount of live data in the heap is
    particularly large, then the idle GC can cause a significant delay,
    and too small an interval could adversely affect interactive
    responsiveness.

    This is an experimental feature, please let us know if it causes
    problems and/or could benefit from further tuning.

.. rts-flag:: -ki ⟨size⟩

    :default: 1k

    .. index::
       single: stack, initial size

    Set the initial stack size for new threads.

    Thread stacks (including the main thread's stack) live on the heap.
    As the stack grows, new stack chunks are added as required; if the
    stack shrinks again, these extra stack chunks are reclaimed by the
    garbage collector. The default initial stack size is deliberately
    small, in order to keep the time and space overhead for thread
    creation to a minimum, and to make it practical to spawn threads for
    even tiny pieces of work.

    .. note::
        This flag used to be simply ``-k``, but was renamed to ``-ki`` in
        GHC 7.2.1. The old name is still accepted for backwards
        compatibility, but that may be removed in a future version.

.. rts-flag:: -kc ⟨size⟩

    :default: 32k

    .. index::
       single: stack; chunk size

    Set the size of "stack chunks". When a thread's current stack overflows, a
    new stack chunk is created and added to the thread's stack, until the limit
    set by :rts-flag:`-K ⟨size⟩` is reached.

    The advantage of smaller stack chunks is that the garbage collector can
    avoid traversing stack chunks if they are known to be unmodified since the
    last collection, so reducing the chunk size means that the garbage
    collector can identify more stack as unmodified, and the GC overhead might
    be reduced. On the other hand, making stack chunks too small adds some
    overhead as there will be more overflow/underflow between chunks. The
    default setting of 32k appears to be a reasonable compromise in most cases.

.. rts-flag:: -kb ⟨size⟩

    :default: 1k

    .. index::
       single: stack; chunk buffer size

    Sets the stack chunk buffer size. When a stack chunk
    overflows and a new stack chunk is created, some of the data from
    the previous stack chunk is moved into the new chunk, to avoid an
    immediate underflow and repeated overflow/underflow at the boundary.
    The amount of stack moved is set by the ``-kb`` option.

    Note that to avoid wasting space, this value should typically be less than
    10% of the size of a stack chunk (:rts-flag:`-kc ⟨size⟩`), because in a
    chain of stack chunks, each chunk will have a gap of unused space of this
    size.

.. rts-flag:: -K ⟨size⟩

    :default: 80% of physical memory

    .. index::
       single: stack, maximum size

    Set the maximum stack size for
    an individual thread to ⟨size⟩ bytes. If the thread attempts to
    exceed this limit, it will be sent the ``StackOverflow`` exception.
    The limit can be disabled entirely by specifying a size of zero.

    This option is there mainly to stop the program eating up all the
    available memory in the machine if it gets into an infinite loop.

.. rts-flag:: -m ⟨n⟩

    :default: 3%

    .. index::
       single: heap, minimum free

    Minimum % ⟨n⟩ of heap which must be available for allocation.

.. rts-flag:: -M ⟨size⟩

    :default: unlimited

    .. index::
       single: heap size, maximum

    Set the maximum heap size to ⟨size⟩ bytes. The
    heap normally grows and shrinks according to the memory requirements
    of the program. The only reason for having this option is to stop
    the heap growing without bound and filling up all the available swap
    space, which at the least will result in the program being summarily
    killed by the operating system.

    The maximum heap size also affects other garbage collection
    parameters: when the amount of live data in the heap exceeds a
    certain fraction of the maximum heap size, compacting collection
    will be automatically enabled for the oldest generation, and the
    ``-F`` parameter will be reduced in order to avoid exceeding the
    maximum heap size.

.. rts-flag:: -Mgrace=⟨size⟩

    :default: 1M

    .. index::
       single: heap size, grace

    If the program's heap exceeds the value set by :rts-flag:`-M ⟨size⟩`, the
    RTS throws an exception to the program, and the program gets an
    additional quota of allocation before the exception is raised
    again, the idea being so that the program can execute its
    exception handlers. ``-Mgrace=`` controls the size of this
    additional quota.

.. rts-flag:: --numa
              --numa=<mask>

    .. index::
       single: NUMA, enabling in the runtime

    Enable NUMA-aware memory allocation in the runtime (only available
    with ``-threaded``, and only on Linux currently).

    Background: some systems have a Non-Uniform Memory Architecture,
    whereby main memory is split into banks which are "local" to
    specific CPU cores.  Accessing local memory is faster than
    accessing remote memory.  The OS provides APIs for allocating
    local memory and binding threads to particular CPU cores, so that
    we can ensure certain memory accesses are using local memory.

    The ``--numa`` option tells the RTS to tune its memory usage to
    maximize local memory accesses.  In particular, the RTS will:

       - Determine the number of NUMA nodes (N) by querying the OS.
       - Manage separate memory pools for each node.
       - Map capabilities to NUMA nodes.  Capability C is mapped to
         NUMA node C mod N.
       - Bind worker threads on a capability to the appropriate node.
       - Allocate the nursery from node-local memory.
       - Perform other memory allocation, including in the GC, from
         node-local memory.
       - When load-balancing, we prefer to migrate threads to another
         Capability on the same node.

    The ``--numa`` flag is typically beneficial when a program is
    using all cores of a large multi-core NUMA system, with a large
    allocation area (``-A``).  All memory accesses to the allocation
    area will go to local memory, which can save a significant amount
    of remote memory access.  A runtime speedup on the order of 10%
    is typical, but can vary a lot depending on the hardware and the
    memory behaviour of the program.

    Note that the RTS will not set CPU affinity for bound threads and
    threads entering Haskell from C/C++, so if your program uses bound
    threads you should ensure that each bound thread calls the RTS API
    `rts_setInCallCapability(c,1)` from C/C++ before calling into
    Haskell.  Otherwise there could be a mismatch between the CPU that
    the thread is running on and the memory it is using while running
    Haskell code, which will negate any benefits of ``--numa``.

    If given an explicit <mask>, the <mask> is interpreted as a bitmap
    that indicates the NUMA nodes on which to run the program.  For
    example, ``--numa=3`` would run the program on NUMA nodes 0 and 1.

.. rts-flag:: --long-gc-sync
              --long-gc-sync=<seconds>

    .. index::
       single: GC sync time, measuring

    When a GC starts, all the running mutator threads have to stop and
    synchronise.  The period between when the GC is initiated and all
    the mutator threads are stopped is called the GC synchronisation
    phase. If this phase is taking a long time (longer than 1ms is
    considered long), then it can have a severe impact on overall
    throughput.

    A long GC sync can be caused by a mutator thread that is inside an
    ``unsafe`` FFI call, or running in a loop that doesn't allocate
    memory and so doesn't yield.  To fix the former, make the call
    ``safe``, and to fix the latter, either avoid calling the code in
    question or compile it with :ghc-flag:`-fomit-yields`.

    By default, the flag will cause a warning to be emitted to stderr
    when the sync time exceeds the specified time.  This behaviour can
    be overriden, however: the ``longGCSync()`` hook is called when
    the sync time is exceeded during the sync period, and the
    ``longGCSyncEnd()`` hook at the end. Both of these hooks can be
    overriden in the ``RtsConfig`` when the runtime is started with
    ``hs_init_ghc()``. The default implementations of these hooks
    (``LongGcSync()`` and ``LongGCSyncEnd()`` respectively) print
    warnings to stderr.

    One way to use this flag is to set a breakpoint on
    ``LongGCSync()`` in the debugger, and find the thread that is
    delaying the sync. You probably want to use :ghc-flag:`-g` to
    provide more info to the debugger.

    The GC sync time, along with other GC stats, are available by
    calling the ``getRTSStats()`` function from C, or
    ``GHC.Stats.getRTSStats`` from Haskell.

.. _rts-options-statistics:

RTS options to produce runtime statistics
-----------------------------------------

.. rts-flag:: -T
              -t [⟨file⟩]
              -s [⟨file⟩]
              -S [⟨file⟩]
              --machine-readable

    These options produce runtime-system statistics, such as the amount
    of time spent executing the program and in the garbage collector,
    the amount of memory allocated, the maximum size of the heap, and so
    on. The three variants give different levels of detail: ``-T``
    collects the data but produces no output ``-t`` produces a single
    line of output in the same format as GHC's ``-Rghc-timing`` option,
    ``-s`` produces a more detailed summary at the end of the program,
    and ``-S`` additionally produces information about each and every
    garbage collection.

    The output is placed in ⟨file⟩. If ⟨file⟩ is omitted, then the
    output is sent to ``stderr``.

    If you use the ``-T`` flag then, you should access the statistics
    using :base-ref:`GHC.Stats.`.

    If you use the ``-t`` flag then, when your program finishes, you
    will see something like this:

    .. code-block:: none

        <<ghc: 36169392 bytes, 69 GCs, 603392/1065272 avg/max bytes residency (2 samples), 3M in use, 0.00 INIT (0.00 elapsed), 0.02 MUT (0.02 elapsed), 0.07 GC (0.07 elapsed) :ghc>>

    This tells you:

    -  The total number of bytes allocated by the program over the whole
       run.

    -  The total number of garbage collections performed.

    -  The average and maximum "residency", which is the amount of live
       data in bytes. The runtime can only determine the amount of live
       data during a major GC, which is why the number of samples
       corresponds to the number of major GCs (and is usually relatively
       small). To get a better picture of the heap profile of your
       program, use the :rts-flag:`-hT` RTS option (:ref:`rts-profiling`).

    -  The peak memory the RTS has allocated from the OS.

    -  The amount of CPU time and elapsed wall clock time while
       initialising the runtime system (INIT), running the program
       itself (MUT, the mutator), and garbage collecting (GC).

    You can also get this in a more future-proof, machine readable
    format, with ``-t --machine-readable``:

    ::

         [("bytes allocated", "36169392")
         ,("num_GCs", "69")
         ,("average_bytes_used", "603392")
         ,("max_bytes_used", "1065272")
         ,("num_byte_usage_samples", "2")
         ,("peak_megabytes_allocated", "3")
         ,("init_cpu_seconds", "0.00")
         ,("init_wall_seconds", "0.00")
         ,("mutator_cpu_seconds", "0.02")
         ,("mutator_wall_seconds", "0.02")
         ,("GC_cpu_seconds", "0.07")
         ,("GC_wall_seconds", "0.07")
         ]

    If you use the ``-s`` flag then, when your program finishes, you
    will see something like this (the exact details will vary depending
    on what sort of RTS you have, e.g. you will only see profiling data
    if your RTS is compiled for profiling):

    .. code-block:: none

              36,169,392 bytes allocated in the heap
               4,057,632 bytes copied during GC
               1,065,272 bytes maximum residency (2 sample(s))
                  54,312 bytes maximum slop
                       3 MB total memory in use (0 MB lost due to fragmentation)

          Generation 0:    67 collections,     0 parallel,  0.04s,  0.03s elapsed
          Generation 1:     2 collections,     0 parallel,  0.03s,  0.04s elapsed

          SPARKS: 359207 (557 converted, 149591 pruned)

          INIT  time    0.00s  (  0.00s elapsed)
          MUT   time    0.01s  (  0.02s elapsed)
          GC    time    0.07s  (  0.07s elapsed)
          EXIT  time    0.00s  (  0.00s elapsed)
          Total time    0.08s  (  0.09s elapsed)

          %GC time      89.5%  (75.3% elapsed)

          Alloc rate    4,520,608,923 bytes per MUT second

          Productivity  10.5% of total user, 9.1% of total elapsed

    -  The "bytes allocated in the heap" is the total bytes allocated by
       the program over the whole run.

    -  GHC uses a copying garbage collector by default. "bytes copied
       during GC" tells you how many bytes it had to copy during garbage
       collection.

    -  The maximum space actually used by your program is the "bytes
       maximum residency" figure. This is only checked during major
       garbage collections, so it is only an approximation; the number
       of samples tells you how many times it is checked.

    -  The "bytes maximum slop" tells you the most space that is ever
       wasted due to the way GHC allocates memory in blocks. Slop is
       memory at the end of a block that was wasted. There's no way to
       control this; we just like to see how much memory is being lost
       this way.

    -  The "total memory in use" tells you the peak memory the RTS has
       allocated from the OS.

    -  Next there is information about the garbage collections done. For
       each generation it says how many garbage collections were done,
       how many of those collections were done in parallel, the total
       CPU time used for garbage collecting that generation, and the
       total wall clock time elapsed while garbage collecting that
       generation.

    -  The ``SPARKS`` statistic refers to the use of
       ``Control.Parallel.par`` and related functionality in the
       program. Each spark represents a call to ``par``; a spark is
       "converted" when it is executed in parallel; and a spark is
       "pruned" when it is found to be already evaluated and is
       discarded from the pool by the garbage collector. Any remaining
       sparks are discarded at the end of execution, so "converted" plus
       "pruned" does not necessarily add up to the total.

    -  Next there is the CPU time and wall clock time elapsed broken
       down by what the runtime system was doing at the time. INIT is
       the runtime system initialisation. MUT is the mutator time, i.e.
       the time spent actually running your code. GC is the time spent
       doing garbage collection. RP is the time spent doing retainer
       profiling. PROF is the time spent doing other profiling. EXIT is
       the runtime system shutdown time. And finally, Total is, of
       course, the total.

       %GC time tells you what percentage GC is of Total. "Alloc rate"
       tells you the "bytes allocated in the heap" divided by the MUT
       CPU time. "Productivity" tells you what percentage of the Total
       CPU and wall clock elapsed times are spent in the mutator (MUT).

    The ``-S`` flag, as well as giving the same output as the ``-s``
    flag, prints information about each GC as it happens:

    .. code-block:: none

            Alloc    Copied     Live    GC    GC     TOT     TOT  Page Flts
            bytes     bytes     bytes  user  elap    user    elap
           528496     47728    141512  0.01  0.02    0.02    0.02    0    0  (Gen:  1)
        [...]
           524944    175944   1726384  0.00  0.00    0.08    0.11    0    0  (Gen:  0)

    For each garbage collection, we print:

    -  How many bytes we allocated this garbage collection.

    -  How many bytes we copied this garbage collection.

    -  How many bytes are currently live.

    -  How long this garbage collection took (CPU time and elapsed wall
       clock time).

    -  How long the program has been running (CPU time and elapsed wall
       clock time).

    -  How many page faults occurred this garbage collection.

    -  How many page faults occurred since the end of the last garbage
       collection.

    -  Which generation is being garbage collected.

RTS options for concurrency and parallelism
-------------------------------------------

The RTS options related to concurrency are described in
:ref:`using-concurrent`, and those for parallelism in
:ref:`parallel-options`.

.. _rts-profiling:

RTS options for profiling
-------------------------

Most profiling runtime options are only available when you compile your
program for profiling (see :ref:`prof-compiler-options`, and
:ref:`rts-options-heap-prof` for the runtime options). However, there is
one profiling option that is available for ordinary non-profiled
executables:

.. rts-flag:: -hT
              -h

    Generates a basic heap profile, in the file :file:`prog.hp`. To produce the
    heap profile graph, use :command:`hp2ps` (see :ref:`hp2ps`). The basic heap
    profile is broken down by data constructor, with other types of closures
    (functions, thunks, etc.) grouped into broad categories (e.g. ``FUN``,
    ``THUNK``). To get a more detailed profile, use the full profiling support
    (:ref:`profiling`). Can be shortened to :rts-flag:`-h`.

.. rts-flag:: -L ⟨n⟩

    :default: 25 characters

    Sets the maximum length of the cost-centre names listed in the heap profile.

.. _rts-eventlog:

Tracing
-------

.. index::
   single: tracing
   single: events
   single: eventlog files

When the program is linked with the :ghc-flag:`-eventlog` option
(:ref:`options-linker`), runtime events can be logged in several ways:

-  In binary format to a file for later analysis by a variety of tools.
   One such tool is
   `ThreadScope <http://www.haskell.org/haskellwiki/ThreadScope>`__,
   which interprets the event log to produce a visual parallel execution
   profile of the program.

-  In binary format to customized event log writer. This enables live
   analysis of the events while the program is running.

-  As text to standard output, for debugging purposes.

.. rts-flag:: -l ⟨flags⟩

    Log events in binary format. Without any ⟨flags⟩ specified, this
    logs a default set of events, suitable for use with tools like ThreadScope.

    Per default the events are written to :file:`{program}.eventlog` though
    the mechanism for writing event log data can be overriden with a custom
    `EventLogWriter`.

    For some special use cases you may want more control over which
    events are included. The ⟨flags⟩ is a sequence of zero or more
    characters indicating which classes of events to log. Currently
    these the classes of events that can be enabled/disabled:

    - ``s`` — scheduler events, including Haskell thread creation and start/stop
      events. Enabled by default.

    - ``g`` — GC events, including GC start/stop. Enabled by default.

    - ``p`` — parallel sparks (sampled). Enabled by default.

    - ``f`` — parallel sparks (fully accurate). Disabled by default.

    - ``u`` — user events. These are events emitted from Haskell code using
      functions such as ``Debug.Trace.traceEvent``. Enabled by default.

    You can disable specific classes, or enable/disable all classes at
    once:

    - ``a`` — enable all event classes listed above
    - ``-⟨x⟩`` — disable the given class of events, for any event class listed above
    - ``-a`` — disable all classes

    For example, ``-l-ag`` would disable all event classes (``-a``) except for
    GC events (``g``).

    For spark events there are two modes: sampled and fully accurate.
    There are various events in the life cycle of each spark, usually
    just creating and running, but there are some more exceptional
    possibilities. In the sampled mode the number of occurrences of each
    kind of spark event is sampled at frequent intervals. In the fully
    accurate mode every spark event is logged individually. The latter
    has a higher runtime overhead and is not enabled by default.

    The format of the log file is described by the header
    ``EventLogFormat.h`` that comes with GHC, and it can be parsed in
    Haskell using the
    `ghc-events <http://hackage.haskell.org/package/ghc-events>`__
    library. To dump the contents of a ``.eventlog`` file as text, use
    the tool ``ghc-events show`` that comes with the
    `ghc-events <http://hackage.haskell.org/package/ghc-events>`__
    package.

.. rts-flag:: -v [⟨flags⟩]

    Log events as text to standard output, instead of to the
    ``.eventlog`` file. The ⟨flags⟩ are the same as for ``-l``, with the
    additional option ``t`` which indicates that the each event printed
    should be preceded by a timestamp value (in the binary ``.eventlog``
    file, all events are automatically associated with a timestamp).

The debugging options ``-Dx`` also generate events which are logged
using the tracing framework. By default those events are dumped as text
to stdout (``-Dx`` implies ``-v``), but they may instead be stored in
the binary eventlog file by using the ``-l`` option.

.. _rts-options-debugging:

RTS options for hackers, debuggers, and over-interested souls
-------------------------------------------------------------

.. index::
   single: RTS options, hacking/debugging

These RTS options might be used (a) to avoid a GHC bug, (b) to see
"what's really happening", or (c) because you feel like it. Not
recommended for everyday use!

.. rts-flag:: -B

    Sound the bell at the start of each (major) garbage collection.

    Oddly enough, people really do use this option! Our pal in Durham
    (England), Paul Callaghan, writes: “Some people here use it for a
    variety of purposes—honestly!—e.g., confirmation that the
    code/machine is doing something, infinite loop detection, gauging
    cost of recently added code. Certain people can even tell what stage
    [the program] is in by the beep pattern. But the major use is for
    annoying others in the same office…”

.. rts-flag:: -D ⟨x⟩

    An RTS debugging flag; only available if the program was linked with
    the :ghc-flag:`-debug` option. Various values of ⟨x⟩ are provided to enable
    debug messages and additional runtime sanity checks in different
    subsystems in the RTS, for example ``+RTS -Ds -RTS`` enables debug
    messages from the scheduler. Use ``+RTS -?`` to find out which debug
    flags are supported.

    Debug messages will be sent to the binary event log file instead of
    stdout if the :rts-flag:`-l` option is added. This might be useful for
    reducing the overhead of debug tracing.

.. rts-flag:: -r ⟨file⟩

    .. index::
       single: ticky ticky profiling
       single: profiling; ticky ticky

    Produce "ticky-ticky" statistics at the end of the program run (only
    available if the program was linked with :ghc-flag:`-debug`). The ⟨file⟩
    business works just like on the :rts-flag:`-S [⟨file⟩]` RTS option, above.

    For more information on ticky-ticky profiling, see
    :ref:`ticky-ticky`.

.. rts-flag:: -xc

    (Only available when the program is compiled for profiling.) When an
    exception is raised in the program, this option causes a stack trace
    to be dumped to ``stderr``.

    This can be particularly useful for debugging: if your program is
    complaining about a ``head []`` error and you haven't got a clue
    which bit of code is causing it, compiling with
    ``-prof -fprof-auto`` (see :ghc-flag:`-prof`) and running with ``+RTS -xc
    -RTS`` will tell you exactly the call stack at the point the error was
    raised.

    The output contains one report for each exception raised in the
    program (the program might raise and catch several exceptions during
    its execution), where each report looks something like this:

    .. code-block:: none

        *** Exception raised (reporting due to +RTS -xc), stack trace:
          GHC.List.CAF
          --> evaluated by: Main.polynomial.table_search,
          called from Main.polynomial.theta_index,
          called from Main.polynomial,
          called from Main.zonal_pressure,
          called from Main.make_pressure.p,
          called from Main.make_pressure,
          called from Main.compute_initial_state.p,
          called from Main.compute_initial_state,
          called from Main.CAF
          ...

    The stack trace may often begin with something uninformative like
    ``GHC.List.CAF``; this is an artifact of GHC's optimiser, which
    lifts out exceptions to the top-level where the profiling system
    assigns them to the cost centre "CAF". However, ``+RTS -xc`` doesn't
    just print the current stack, it looks deeper and reports the stack
    at the time the CAF was evaluated, and it may report further stacks
    until a non-CAF stack is found. In the example above, the next stack
    (after ``--> evaluated by``) contains plenty of information about
    what the program was doing when it evaluated ``head []``.

    Implementation details aside, the function names in the stack should
    hopefully give you enough clues to track down the bug.

    See also the function ``traceStack`` in the module ``Debug.Trace``
    for another way to view call stacks.

.. rts-flag:: -Z

    Turn *off* "update-frame squeezing" at garbage-collection time.
    (There's no particularly good reason to turn it off, except to
    ensure the accuracy of certain data collected regarding thunk entry
    counts.)

.. _ghc-info:

Getting information about the RTS
---------------------------------

.. index::
   single: RTS

.. rts-flag:: --info

    It is possible to ask the RTS to give some information about itself. To
    do this, use the :rts-flag:`--info` flag, e.g.

    .. code-block:: none

        $ ./a.out +RTS --info
        [("GHC RTS", "YES")
        ,("GHC version", "6.7")
        ,("RTS way", "rts_p")
        ,("Host platform", "x86_64-unknown-linux")
        ,("Host architecture", "x86_64")
        ,("Host OS", "linux")
        ,("Host vendor", "unknown")
        ,("Build platform", "x86_64-unknown-linux")
        ,("Build architecture", "x86_64")
        ,("Build OS", "linux")
        ,("Build vendor", "unknown")
        ,("Target platform", "x86_64-unknown-linux")
        ,("Target architecture", "x86_64")
        ,("Target OS", "linux")
        ,("Target vendor", "unknown")
        ,("Word size", "64")
        ,("Compiler unregisterised", "NO")
        ,("Tables next to code", "YES")
        ]

    The information is formatted such that it can be read as a of type
    ``[(String, String)]``. Currently the following fields are present:

    ``GHC RTS``
        Is this program linked against the GHC RTS? (always "YES").

    ``GHC version``
        The version of GHC used to compile this program.

    ``RTS way``
        The variant (“way”) of the runtime. The most common values are
        ``rts_v`` (vanilla), ``rts_thr`` (threaded runtime, i.e. linked
        using the :ghc-flag:`-threaded` option) and ``rts_p`` (profiling runtime,
        i.e. linked using the :ghc-flag:`-prof` option). Other variants include
        ``debug`` (linked using :ghc-flag:`-debug`), and ``dyn`` (the RTS is linked
        in dynamically, i.e. a shared library, rather than statically linked
        into the executable itself). These can be combined, e.g. you might
        have ``rts_thr_debug_p``.

    ``Target platform``\ ``Target architecture``\ ``Target OS``\ ``Target vendor``
        These are the platform the program is compiled to run on.

    ``Build platform``\ ``Build architecture``\ ``Build OS``\ ``Build vendor``
        These are the platform where the program was built on. (That is, the
        target platform of GHC itself.) Ordinarily this is identical to the
        target platform. (It could potentially be different if
        cross-compiling.)

    ``Host platform``\ ``Host architecture``\ ``Host OS``\ ``Host vendor``
        These are the platform where GHC itself was compiled. Again, this
        would normally be identical to the build and target platforms.

    ``Word size``
        Either ``"32"`` or ``"64"``, reflecting the word size of the target
        platform.

    ``Compiler unregistered``
        Was this program compiled with an :ref:`"unregistered" <unreg>`
        version of GHC? (I.e., a version of GHC that has no
        platform-specific optimisations compiled in, usually because this is
        a currently unsupported platform.) This value will usually be no,
        unless you're using an experimental build of GHC.

    ``Tables next to code``
        Putting info tables directly next to entry code is a useful
        performance optimisation that is not available on all platforms.
        This field tells you whether the program has been compiled with this
        optimisation. (Usually yes, except on unusual platforms.)
