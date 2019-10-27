Eventlog encodings
==================

This section documents the encodings of the events emitted to GHC's
:ref:`event log <rts-eventlog>`. These events can include information about the
thread scheduling events, garbage collection statistics, profiling information,
user-defined tracing events.

This section is intended for implementors of tooling which consume these events.


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

 * ``EVENT_HEAP_PROF_BEGIN``

   * ``Word8``: Profile ID
   * ``Word64``: Sampling period in nanoseconds
   * ``Word32``: Sample break-down type. One of,

      * ``HEAP_PROF_BREAKDOWN_COST_CENTER`` (output from :rts-flag:`-hc`)
      * ``HEAP_PROF_BREAKDOWN_CLOSURE_DESCR`` (output from :rts-flag:`-hd`)
      * ``HEAP_PROF_BREAKDOWN_RETAINER`` (output from :rts-flag:`-hr`)
      * ``HEAP_PROF_BREAKDOWN_MODULE`` (output from :rts-flag:`-hm`)
      * ``HEAP_PROF_BREAKDOWN_TYPE_DESCR`` (output from :rts-flag:`-hy`)
      * ``HEAP_PROF_BREAKDOWN_BIOGRAPHY`` (output from :rts-flag:`-hb`)
      * ``HEAP_PROF_BREAKDOWN_CLOSURE_TYPE`` (output from :rts-flag:`-hT`)

   * ``String``: Module filter
   * ``String``: Closure description filter
   * ``String``: Type description filter
   * ``String``: Cost centre filter
   * ``String``: Cost centre stack filter
   * ``String``: Retainer filter
   * ``String``: Biography filter

Cost centre definitions
^^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet produced once for each cost centre,

 * ``EVENT_HEAP_PROF_COST_CENTRE``

   * ``Word32``: cost centre number
   * ``String``: label
   * ``String``: module
   * ``String``: source location
   * ``Word8``: flags

     * bit 0: is the cost-centre a CAF?


Sample event types
~~~~~~~~~~~~~~~~~~

A sample (consisting of a list of break-down classes, e.g. cost centres, and
heap residency sizes), is to be encoded in the body of one or more events.

We normally mark the beginning of a new sample with an ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
event,

 * ``EVENT_HEAP_PROF_SAMPLE_BEGIN``

   * ``Word64``: sample number

Biographical profiling samples start with the ``EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN``
event. These events also include a timestamp which indicates when the sample
was taken. This is because all these samples will appear at the end of
the eventlog due to how the biographical profiling mode works. You can
use the timestamp to reorder the samples relative to the other events.

 * ``EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN``

   * ``Word64``: sample number
   * ``Word64``: eventlog timestamp in ns


A heap residency census will follow. Since events may only be up to 2^16^ bytes
in length a single sample may need to be split among multiple
``EVENT_HEAP_PROF_SAMPLE`` events. The precise format of the census entries is
determined by the break-down type.

At the end of the sample period the ``EVENT_HEAP_PROF_SAMPLE_END`` event if
emitted. This is useful to properly delimit the sampling period and to record
the total time spent profiling.


 * ``EVENT_HEAP_PROF_SAMPLE_END``
   * ``Word64``: sample number


Cost-centre break-down
^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet encoding a heap profile sample broken down by,
 * cost-centre (``-hc``)


 * ``EVENT_HEAP_PROF_SAMPLE_COST_CENTRE``

   * ``Word8``: Profile ID
   * ``Word64``: heap residency in bytes
   * ``Word8``: stack depth
   * ``Word32[]``: cost centre stack starting with inner-most (cost centre numbers)


String break-down
^^^^^^^^^^^^^^^^^

A variable-length event encoding a heap sample broken down by,

 * type description (``-hy``)
 * closure description (``-hd``)
 * module (``-hm``)

 * ``EVENT_HEAP_PROF_SAMPLE_STRING``

   * ``Word8``: Profile ID
   * ``Word64``: heap residency in bytes
   * ``String``: type or closure description, or module name

.. _time-profiler-events:

Time profiler event log output
------------------------------

The time profiling mode enabled by ``-p`` also emits sample events to the eventlog.
At the start of profiling the tick interval is emitted to the eventlog and then
on each tick the current cost centre stack is emitted. Together these enable
a user to construct an approximate track of the executation of their program.

Profile begin event
~~~~~~~~~~~~~~~~~~~

 * ``EVENT_PROF_BEGIN``

   * ``Word64``: Tick interval, in nanoseconds


Tick sample event
~~~~~~~~~~~~~~~~~

A variable-length packet encoding a profile sample.

  * ``EVENT_PROF_SAMPLE_COST_CENTRE``

    * ``Word32``: Capability
    * ``Word64``: Current profiling tick
    * ``Word8``: stack depth
    * ``Word32[]``: cost centre stack starting with inner-most (cost centre numbers)
