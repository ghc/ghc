.. _eventlog-encodings:

Eventlog encodings
==================

This section documents the encodings of the events emitted to GHC's
:ref:`event log <rts-eventlog>`. These events can include information about the
thread scheduling events, garbage collection statistics, profiling information,
user-defined tracing events.

This section is intended for implementors of tooling which consume these events.
GHC ships with a C header file (``EventlogFormat.h``) which provides symbolic
names for the event type IDs described in this file.


Event log format
----------------

The log format is designed to be extensible: old tools should be
able to parse (but not necessarily understand all of) new versions
of the format, and new tools will be able to understand old log
files.

- The format is endian-independent: all values are represented in
  big-endian order.

- The format is extensible:

  - The header describes each event type and its length.  Tools
    that don't recognise a particular event type can skip those events.

  - There is room for extra information in the event type
    specification, which can be ignored by older tools.

  - Events can have extra information added, but existing fields
    cannot be changed.  Tools should ignore extra fields at the
    end of the event record.

The event-log stream begins with a header describing the event types present in
the file. The header is followed by the event records themselves, each of which
consist of a 64-bit timestamp

.. code-block:: none

    log : EVENT_HEADER_BEGIN
          EventType*
          EVENT_HEADER_END
          EVENT_DATA_BEGIN
          Event*
          EVENT_DATA_END

    EventType :
          EVENT_ET_BEGIN
          Word16         -- unique identifier for this event
          Int16          -- >=0  size of the event in bytes (minus the header)
                         -- -1   variable size
          Word32         -- length of the next field in bytes
          Word8*         -- string describing the event
          Word32         -- length of the next field in bytes
          Word8*         -- extra info (for future extensions)
          EVENT_ET_END

    Event :
          Word16         -- event_type
          Word64         -- time (nanosecs)
          [Word16]       -- length of the rest (for variable-sized events only)
          ... extra event-specific info ...

There are two classes of event types:

 - *Fixed size*: All event records of a fixed-sized type are of the same
   length, given in the header event-log header.

 - *Variable size*: Each event record includes a length field.

Runtime system diagnostics
--------------------------

 * ``ThreadId ~ Word32``
 * ``CapNo ~ Word16``
 * ``CapSetId ~ Word32``

Capability sets
~~~~~~~~~~~~~~~

TODO

Environment information
~~~~~~~~~~~~~~~~~~~~~~~

These events are typically produced during program startup and describe the
environment which the program is being run in.

.. event-type:: RTS_IDENTIFIER

   :tag: 29
   :length: variable
   :field CapSetId: Capability set
   :field String: Runtime system name and version.

   Describes the name and version of the runtime system responsible for the
   indicated capability set.

.. event-type:: PROGRAM_ARGS

   :tag: 30
   :length: variable
   :field CapSetId: Capability set
   :field [String]: The command-line arguments passed to the program

   Describes the command-line used to start the program.

.. event-type:: PROGRAM_ENV

   :tag: 31
   :length: variable
   :field CapSetId: Capability set
   :field [String]: The environment variable name/value pairs. (TODO: encoding?)

   Describes the environment variables present in the program's environment.

Thread and scheduling events
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. event-type:: CREATE_THREAD

   :tag: 0
   :length: fixed
   :field ThreadId: thread id

   Marks the creation of a Haskell thread.


.. event-type:: RUN_THREAD

   :tag: 1
   :length: fixed
   :field ThreadId: thread id

   The indicated thread has started running.


.. event-type:: STOP_THREAD

   :tag: 2
   :length: fixed
   :field ThreadId: thread id
   :field Word16: status

      * 1: HeapOverflow
      * 2: StackOverflow
      * 3: ThreadYielding
      * 4: ThreadBlocked
      * 5: ThreadFinished
      * 6: ForeignCall
      * 7: BlockedOnMVar
      * 8: BlockedOnBlackHole
      * 9: BlockedOnRead
      * 10: BlockedOnWrite
      * 11: BlockedOnDelay
      * 12: BlockedOnSTM
      * 13: BlockedOnDoProc
      * 16: BlockedOnMsgThrowTo

   :field ThreadId: thread id of thread being blocked on (only for some status
                    values)

   The indicated thread has stopped running for the reason given by ``status``.


.. event-type:: THREAD_RUNNABLE

   :tag: 3
   :length: fixed
   :field ThreadId: thread id

   The indicated thread is has been marked as ready to run.


.. event-type:: MIGRATE_THREAD

   :tag: 4
   :length: fixed
   :field ThreadId: thread id
   :field CapNo: capability

   The indicated thread has been migrated to a new capability.


.. event-type:: THREAD_WAKEUP

   :tag: 8
   :length: fixed
   :field ThreadId: thread id
   :field CapNo: other capability

   The indicated thread has been woken up on another capability.

.. event-type:: THREAD_LABEL

   :tag: 44
   :length: fixed
   :field ThreadId: thread id
   :field String: label

   The indicated thread has been given a label (e.g. with
   :base-ref:`GHC.Conc.labelThread`).


.. _gc-events:

Garbage collector events
~~~~~~~~~~~~~~~~~~~~~~~~

The following events mark various points of the lifecycle of a moving garbage
collection.

A typical garbage collection will look something like the following:

1. A capability realizes that it needs a garbage collection (e.g. as a result
   of running out of nursery) and requests a garbage collection.  This is
   marked by :event-type:`REQUEST_SEQ_GC` or :event-type:`REQUEST_PAR_GC`.

2. As other capabilities reach yield points and suspend execution they emit
   :event-type:`STOP_THREAD` events.

3. When all capabilities have suspended execution, collection will begin,
   marked by a :event-type:`GC_START` event.

4. As individual parallel GC threads commence with scavenging they will emit
   :event-type:`GC_WORK` events.

5. If a parallel GC thread runs out of work it will emit a
   :event-type:`GC_IDLE` event. If it is later handed more work it will emit
   another :event-type:`GC_WORK` event.

6. Eventually when scavenging has finished a :event-type:`GC_DONE` event
   will be emitted by each GC thread.

7. A bit of book-keeping is performed.

8. A :event-type:`GC_END` event will be emitted marking the end of the GC cycle.

9. A :event-type:`HEAP_SIZE` event will be emitted giving the
   current size of the heap, in bytes, calculated by how many megablocks
   are allocated.

10. A :event-type:`BLOCKS_SIZE` event will be emitted giving the
    current size of the heap, in bytes, calculated by how many blocks
    are allocated.

11. A :event-type:`GC_STATS_GHC` event will be emitted
    containing various details of the collection and heap state.

12. In the case of a major collection, a
    :event-type:`HEAP_LIVE` event will be emitted describing
    the current size of the live on-heap data.

13. In the case of the :ghc-flag:`-threaded` RTS, a
    :event-type:`SPARK_COUNTERS` event will be emitted giving
    details on how many sparks have been created, evaluated, and GC'd.

14. As mutator threads resume execution they will emit :event-type:`RUN_THREAD`
    events.

15. A :event-type:`MEM_RETURN` event will be emitted containing details about
    currently live mblocks, how many we think we need and whether we could return
    excess to the OS.

Note that in the case of the concurrent non-moving collector additional events
will be emitted during the concurrent phase of collection. These are described
in :ref:`nonmoving-gc-events`.

.. event-type:: GC_START

   :tag: 9
   :length: fixed

   A garbage collection pass has been started.

.. event-type:: GC_END

   :tag: 10
   :length: fixed

   A garbage collection pass has been finished.

.. event-type:: REQUEST_SEQ_GC

   :tag: 11
   :length: fixed

   A sequential garbage collection has been requested by a capability.

.. event-type:: REQUEST_PAR_GC

   :tag: 12
   :length: fixed

   A parallel garbage collection has been requested by a capability.

.. event-type:: GC_IDLE

   :tag: 20
   :length: fixed

   An idle-time garbage collection has been started.

.. event-type:: GC_WORK

   :tag: 21
   :length: fixed

   Marks the start of concurrent scavenging.

.. event-type:: GC_DONE

   :tag: 22
   :length: fixed

   Marks the end of concurrent scavenging.

.. event-type:: GC_STATS_GHC

   :tag: 53
   :length: fixed
   :field CapSetId: heap capability set
   :field Word16: generation of collection
   :field Word64: bytes copied
   :field Word64: bytes of slop found
   :field Word64: bytes of fragmentation, the difference between total mblock size
                  and total block size. When all mblocks are full of full blocks,
                  this number is 0.
   :field Word64: number of parallel garbage collection threads
   :field Word64: maximum number of bytes copied by any single collector thread
   :field Word64: total bytes copied by all collector threads

   Report various information about a major collection.

.. event-type:: GC_GLOBAL_SYNC

   :tag: 54
   :length: fixed

   TODO

.. event-type:: MEM_RETURN

   :tag: 90
   :length: fixed
   :field CapSetId: heap capability set
   :field Word32: currently allocated mblocks
   :field Word32: the number of mblocks we would like to retain
   :field Word32: the number of mblocks which we returned to the OS

   Report information about currently allocation megablocks and attempts
   made to return them to the operating system. If your heap is fragmented
   then the current value will be greater than needed value but returned will
   be less than the difference between the two.


Heap events and statistics
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. event-type:: HEAP_ALLOCATED

   :tag: 49
   :length: fixed
   :field CapSetId: heap capability set
   :field Word64: allocated bytes

   A new chunk of heap has been allocated by the indicated capability set.

.. event-type:: HEAP_SIZE

   :tag: 50
   :length: fixed
   :field CapSetId: heap capability set
   :field Word64: heap size in bytes

   Report the heap size, calculated by the number of megablocks currently allocated.

.. event-type:: BLOCKS_SIZE

   :tag: 91
   :length: fixed
   :field CapSetId: heap capability set
   :field Word64: heap size in bytes

   Report the heap size, calculated by the number of blocks currently allocated.

.. event-type:: HEAP_LIVE

   :tag: 51
   :length: fixed
   :field CapSetId: heap capability set
   :field Word64: heap size in bytes

   Report the live heap size.

.. event-type:: HEAP_INFO_GHC

   :tag: 52
   :length: fixed
   :field CapSetId: heap capability set
   :field Word16: number of garbage collection generations
   :field Word64: maximum heap size
   :field Word64: allocation area size
   :field Word64: MBlock size
   :field Word64: Block size

   Report various information about the heap configuration. Typically produced
   during RTS initialization..

Spark events
~~~~~~~~~~~~

.. event-type:: CREATE_SPARK_THREAD

   :tag: 15
   :length: fixed

   A thread has been created to perform spark evaluation.

.. event-type:: SPARK_COUNTERS

   :tag: 34
   :length: fixed

   A periodic reporting of various statistics of spark evaluation.

.. event-type:: SPARK_CREATE

   :tag: 35
   :length: fixed

   A spark has been added to the spark pool.

.. event-type:: SPARK_DUD

   :tag: 36
   :length: fixed

   TODO

.. event-type:: SPARK_OVERFLOW

   :tag: 37
   :length: fixed

   TODO

.. event-type:: SPARK_RUN

   :tag: 38
   :length: fixed

   Evaluation has started on a spark.

.. event-type:: SPARK_STEAL

   :tag: 39
   :length: fixed
   :field Word16: capability from which the spark was stolen

   A spark has been stolen from another capability for evaluation.

.. event-type:: SPARK_FIZZLE

   :tag: 40
   :length: fixed

   A spark has been GC'd before being evaluated.

.. event-type:: SPARK_GC

   :tag: 41
   :length: fixed

   An unevaluated spark has been garbage collected.

Capability events
~~~~~~~~~~~~~~~~~

.. event-type:: CAP_CREATE

   :tag: 45
   :length: fixed
   :field CapNo: the capability number

   A capability has been started.

.. event-type:: CAP_DELETE

   :tag: 46
   :length: fixed

   A capability has been deleted.

.. event-type:: CAP_DISABLE

   :tag: 47
   :length: fixed

   A capability has been disabled.

.. event-type:: CAP_ENABLE

   :tag: 48
   :length: fixed

   A capability has been enabled.

Task events
~~~~~~~~~~~

.. event-type:: TASK_CREATE

   :tag: 55
   :length: fixed
   :field TaskId: task id
   :field CapNo: capability number
   :field ThreadId: TODO

   Marks the creation of a task.

.. event-type:: TASK_MIGRATE

   :tag: 56
   :length: fixed
   :field TaskId: task id
   :field CapNo: old capability
   :field CapNo: new capability

   Marks the migration of a task to a new capability.

Tracing events
~~~~~~~~~~~~~~

.. event-type:: LOG_MSG

   :tag: 16
   :length: variable
   :field String: The message

   A log message from the runtime system.

.. event-type:: BLOCK_MARKER

   :tag: 18
   :length: variable
   :field Word32: size
   :field Word64: end time in nanoseconds
   :field String: marker name

   TODO

.. event-type:: USER_MSG

   :tag: 19
   :length: variable
   :field String: message

   A user log message (from, e.g., :base-ref:`Control.Concurrent.traceEvent`).

.. event-type:: USER_MARKER

   :tag: 58
   :length: variable
   :field String: marker name

   A user marker (from :base-ref:`Debug.Trace.traceMarker`).


.. _heap-profiler-events:

Heap profiler event log output
------------------------------

The heap profiler can produce output to GHC's event log, allowing samples to
be correlated with other event log events over the program's lifecycle.

This section defines the layout of these events. The ``String`` type below is
defined to be a UTF-8 encoded NUL-terminated string.

Metadata event types
~~~~~~~~~~~~~~~~~~~~

Beginning of sample stream
^^^^^^^^^^^^^^^^^^^^^^^^^^

A single fixed-width event emitted during program start-up describing the samples that follow.

.. event-type:: HEAP_PROF_BEGIN

   :tag: 160
   :length: variable
   :field Word8: profile ID
   :field Word64: sampling period in nanoseconds
   :field Word32: sample breadown type. One of,

      * ``HEAP_PROF_BREAKDOWN_COST_CENTER`` (output from :rts-flag:`-hc`)
      * ``HEAP_PROF_BREAKDOWN_CLOSURE_DESCR`` (output from :rts-flag:`-hd`)
      * ``HEAP_PROF_BREAKDOWN_RETAINER`` (output from :rts-flag:`-hr`)
      * ``HEAP_PROF_BREAKDOWN_MODULE`` (output from :rts-flag:`-hm`)
      * ``HEAP_PROF_BREAKDOWN_TYPE_DESCR`` (output from :rts-flag:`-hy`)
      * ``HEAP_PROF_BREAKDOWN_BIOGRAPHY`` (output from :rts-flag:`-hb`)
      * ``HEAP_PROF_BREAKDOWN_CLOSURE_TYPE`` (output from :rts-flag:`-hT`)

   :field String: module filter
   :field String: closure description filter
   :field String: type description filter
   :field String: cost centre filter
   :field String: cost centre stack filter
   :field String: retainer filter
   :field String: biography filter

Cost centre definitions
^^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet produced once for each cost centre,

.. event-type:: HEAP_PROF_COST_CENTRE

   :tag: 161
   :length: fixed
   :field Word32: cost centre number
   :field String: label
   :field String: module
   :field String: source location
   :field Word8: flags:

     * bit 0: is the cost-centre a CAF?

Info Table Provenance definitions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A message which describes an approximate source position for
info tables. See :ghc-flag:`-finfo-table-map` for more information.

.. event-type:: IPE

   :tag: 169
   :length: fixed
   :field Word64: info table address
   :field String: table name
   :field String: closure type
   :field String: type
   :field String: source position label
   :field String: source position module
   :field String: source position location


Sample event types
^^^^^^^^^^^^^^^^^^

A sample (consisting of a list of break-down classes, e.g. cost centres, and
heap residency sizes), is to be encoded in the body of one or more events.

We normally mark the beginning of a new sample with an ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
event,

.. event-type:: HEAP_PROF_SAMPLE_BEGIN

   :length: fixed
   :field Word64: sample number

   Marks the beginning of a heap profile sample.

Biographical profiling samples start with the ``EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN``
event. These events also include a timestamp which indicates when the sample
was taken. This is because all these samples will appear at the end of
the eventlog due to how the biographical profiling mode works. You can
use the timestamp to reorder the samples relative to the other events.

.. event-type:: HEAP_BIO_PROF_SAMPLE_BEGIN

   :tag: 166
   :length: fixed
   :field Word64: sample number
   :field Word64: eventlog timestamp in ns

A heap residency census will follow. Since events may only be up to 2^16^ bytes
in length a single sample may need to be split among multiple
``EVENT_HEAP_PROF_SAMPLE`` events. The precise format of the census entries is
determined by the break-down type.

At the end of the sample period the ``EVENT_HEAP_PROF_SAMPLE_END`` event if
emitted. This is useful to properly delimit the sampling period and to record
the total time spent profiling.


.. event-type:: HEAP_PROF_SAMPLE_END

   :tag: 165
   :length: fixed
   :field Word64: sample number

   Marks the end of a heap profile sample.

Cost-centre break-down
^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet encoding a heap profile sample broken down by,
 * cost-centre (:rts-flag:`-hc`)


.. event-type:: HEAP_PROF_SAMPLE_COST_CENTRE

   :tag: 163
   :length: variable
   :field Word8: profile ID
   :field Word64: heap residency in bytes
   :field Word8: stack depth
   :field Word32[]: cost centre stack starting with inner-most (cost centre numbers)


String break-down
^^^^^^^^^^^^^^^^^

A variable-length event encoding a heap sample broken down by,

 * type description (:rts-flag:`-hy`)
 * closure description (:rts-flag:`-hd`)
 * module (:rts-flag:`-hm`)

.. event-type:: HEAP_PROF_SAMPLE_STRING

   :tag: 164
   :length: variable
   :field Word8: profile ID
   :field Word64: heap residency in bytes
   :field String: type or closure description, or module name

.. _time-profiler-events:

Time profiler event log output
------------------------------

The time profiling mode enabled by :rts-flag:`-p` also emits
sample events to the eventlog.  At the start of profiling the
tick interval is emitted to the eventlog and then on each tick
the current cost centre stack is emitted. Together these
enable a user to construct an approximate track of the
executation of their program.

Profile begin event
~~~~~~~~~~~~~~~~~~~

.. event-type:: PROF_BEGIN

   :tag: 168
   :length: fixed
   :field Word64: tick interval, in nanoseconds

   Marks the beginning of a time profile.

Profile sample event
~~~~~~~~~~~~~~~~~~~~

A variable-length packet encoding a profile sample.

.. event-type:: PROF_SAMPLE_COST_CENTRE

   :tag: 167
   :length: variable
   :field Word32: capability
   :field Word64: current profiling tick
   :field Word8: stack depth
   :field Word32[]: cost centre stack starting with inner-most (cost centre numbers)

Biographical profile sample event
---------------------------------

A variable-length packet encoding a profile sample.

.. event-type:: BIO_PROF_SAMPLE_BEGIN

   :tag: 166

   TODO

.. _nonmoving-gc-events:

Non-moving GC event output
--------------------------

These events mark various stages of the
:rts-flag:`non-moving collection <--nonmoving-gc>` lifecycle. These are enabled
with the ``+RTS -lg`` event-set.

A typical non-moving collection cycle will look something like the following:

1. The preparatory phase of collection will emit the usual events associated
   with a moving collection. See :ref:`gc-events` for details.

2. The concurrent write barrier is enabled and the concurrent mark thread is
   started. From this point forward mutator threads may emit
   :event-type:`CONC_UPD_REM_SET_FLUSH` events, indicating that they have
   flushed their capability-local update remembered sets.

3. Concurrent marking begins, denoted by a :event-type:`CONC_MARK_BEGIN` event.

4. When the mark queue is depleted a :event-type:`CONC_MARK_END` is emitted.

5. If necessary (e.g. due to weak pointer marking), the marking process will
   continue, returning to step (3) above.

6. When the collector has done as much concurrent marking as it can it will
   enter the post-mark synchronization phase of collection, denoted by a
   :event-type:`CONC_SYNC_BEGIN` event.

7. Mutator threads will suspend execution and, if necessary, flush their update
   remembered sets (indicated by :event-type:`CONC_UPD_REM_SET_FLUSH` events).

8. The collector will do any final marking necessary (indicated by
   :event-type:`CONC_MARK_BEGIN` and :event-type:`CONC_MARK_END` events).

9. The collector will do a small amount of sweeping, disable the write barrier,
   emit a :event-type:`CONC_SYNC_END` event, and allow mutators to resume

10. The collector will begin the concurrent sweep phase, indicated by a
    :event-type:`CONC_SWEEP_BEGIN` event.

11. Once sweeping has concluded a :event-type:`CONC_SWEEP_END` event will be
    emitted and the concurrent collector thread will terminate.

12. A :event-type:`NONMOVING_HEAP_CENSUS` event will be emitted describing the
    fragmentation state of the non-moving heap.


.. event-type:: CONC_MARK_BEGIN

   :tag: 200
   :length: fixed

   Marks the beginning of marking by the concurrent collector.

.. event-type:: CONC_MARK_END

   :tag: 201
   :length: fixed

   Marks the end of marking by the concurrent collector.

.. event-type:: CONC_SYNC_BEGIN

   :tag: 202
   :length: fixed

   Marks the beginning of the concurrent garbage collector's
   post-mark synchronization phase.

.. event-type:: CONC_SYNC_END

   :tag: 203
   :length: fixed

   Marks the end of the concurrent garbage collector's
   post-mark synchronization phase.

.. event-type:: CONC_SWEEP_BEGIN

   :tag: 204
   :length: fixed

   Marks the beginning of the concurrent garbage collector's
   sweep phase.

.. event-type:: CONC_SWEEP_END

   :tag: 205
   :length: fixed

   Marks the end of the concurrent garbage collector's
   sweep phase.

.. event-type:: CONC_UPD_REM_SET_FLUSH

   :tag: 206
   :length: fixed

   Marks a capability flushing its local update remembered set
   accumulator.

Non-moving heap census
~~~~~~~~~~~~~~~~~~~~~~

The non-moving heap census events (enabled with the :rts-flag:`+RTS -ln <-l ⟨flags⟩>`
event-set) are intended to provide insight into fragmentation of the non-moving
heap.

.. event-type:: NONMOVING_HEAP_CENSUS

   :tag: 207
   :length: fixed
   :field Word8: base-2 logarithm of *blk_sz*.
   :field Word32: number of active segments.
   :field Word32: number of filled segments.
   :field Word32: number of live blocks.

   Describes the occupancy of the *blk_sz* sub-heap.

Ticky counters
~~~~~~~~~~~~~~

Programs compiled with :ghc-flag:`-ticky` and :ghc-flag:`-eventlog` and invoked
with :rts-flag:`+RTS -lT <-l ⟨flags⟩>` will emit periodic samples of the ticky
entry counters to the eventlog.

.. event-type:: TICKY_COUNTER_DEF

   :tag: 210
   :length: variable
   :field Word64: counter ID
   :field Word16: arity/field count
   :field String: argument kinds. This is the same as the synonymous field in the
     textual ticky summary.
   :field String: counter name

   Defines a ticky counter.

.. event-type:: TICKY_COUNTER_BEGIN_SAMPLE

   :tag: 212
   :length: fixed

   Denotes the beginning of an atomic set of ticky-ticky profiler counter samples.

.. event-type:: TICKY_COUNTER_SAMPLE

   :tag: 211
   :length: fixed
   :field Word64: counter ID
   :field Word64: number of times closures of this type has been entered.
   :field Word64: number of allocations (words)
   :field Word64: number of times this has been allocated (words). Only
     produced for modules compiled with :ghc-flag:`-ticky-allocd`.

   Records the number of "ticks" recorded by a ticky-ticky counter single the last sample.
