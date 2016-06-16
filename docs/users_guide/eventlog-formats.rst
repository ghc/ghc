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
      * ``SAMPLE_TYPE_COST_CENTER`` (output from ``-hc``)
      * ``SAMPLE_TYPE_CLOSURE_DESCR`` (output from ``-hd``)
      * ``SAMPLE_TYPE_RETAINER`` (output from ``-hr``)
      * ``SAMPLE_TYPE_MODULE`` (output from ``-hm``)
      * ``SAMPLE_TYPE_TYPE_DESCR`` (output from ``-hy``)
      * ``SAMPLE_TYPE_BIOGRAPHY`` (output from ``-hb``)
   * ``String``: Cost centre filter
   * ``String``: Closure description filter
   * ``String``: Retainer filter
   * ``String``: Module filter
   * ``String``: Type description filter

Cost center definitions
^^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet produced once for each cost center,

 * ``EVENT_HEAP_PROF_COST_CENTRE``
   * ``Word32``: cost center number
   * ``String``: label
   * ``String``: module
   * ``String``: source location
   * ``Word8``: flags
     * bit 0: is the cost-center a CAF?


Sample event types
~~~~~~~~~~~~~~~~~~

A sample (consisting of a list of break-down classes, e.g. cost centers, and
heap residency sizes), is to be encoded in the body of one or more events.

We mark the beginning of a new sample with an ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
event,

 * ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
   * ``Word64``: sample number

A heap residency census will follow. Since events may only be up to 2^16^ bytes
in length a single sample may need to be split among multiple
``EVENT_HEAP_PROF_SAMPLE`` events. The precise format of the census entries is
determined by the break-down type.


Cost-center break-down
^^^^^^^^^^^^^^^^^^^^^^

A variable-length packet encoding a heap profile sample broken down by,
 * cost-center (``-hc``)
 * retainer (``-hr``)

 * ``EVENT_HEAP_PROF_SAMPLE``
   * ``Word8``: Profile ID
   * ``Word64``: heap residency in bytes
   * ``Word8``: stack depth
   * ``Word32[]``: cost center stack starting with inner-most (cost center numbers)


String break-down
^^^^^^^^^^^^^^^^^

A variable-length event encoding a heap sample broken down by,
 * type description (``-hy``)
 * closure description (``-hd``)
 * module (``-hm``)

 * ``EVENT_HEAP_PROF_SAMPLE``
   * ``Word8``: Profile ID
   * The event shall contain packed pairs of,
     * ``String``: type description
     * ``Word64``: heap residency in bytes


Biography break-down
^^^^^^^^^^^^^^^^^^^^

A fixed-length event encoding a biography heap sample.

 * ``EVENT_HEAP_PROF_SAMPLE``
   * ``Word8``: Profile ID
   * ``Word64``: Void
   * ``Word64``: Lag
   * ``Word64``: Use
   * ``Word64``: Inherent use
   * ``Word64``: Drag
