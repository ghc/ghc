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

We mark the beginning of a new sample with an ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
event,

 * ``EVENT_HEAP_PROF_SAMPLE_BEGIN``
   * ``Word64``: sample number

A heap residency census will follow. Since events may only be up to 2^16^ bytes
in length a single sample may need to be split among multiple
``EVENT_HEAP_PROF_SAMPLE`` events. The precise format of the census entries is
determined by the break-down type.


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
