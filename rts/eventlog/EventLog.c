/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#if defined(TRACING)

#include "Trace.h"
#include "Capability.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "EventLog.h"

#include <string.h>
#include <stdio.h>
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

bool eventlog_enabled;

static const EventLogWriter *event_log_writer = NULL;

#define EVENT_LOG_SIZE 2 * (1024 * 1024) // 2MB

static int flushCount;

// Struct for record keeping of buffer to store event types and events.
typedef struct _EventsBuf {
  StgInt8 *begin;
  StgInt8 *pos;
  StgInt8 *marker;
  StgWord64 size;
  EventCapNo capno; // which capability this buffer belongs to, or -1
} EventsBuf;

EventsBuf *capEventBuf; // one EventsBuf for each Capability

EventsBuf eventBuf; // an EventsBuf not associated with any Capability
#if defined(THREADED_RTS)
Mutex eventBufMutex; // protected by this mutex
#endif

char *EventDesc[] = {
  [EVENT_CREATE_THREAD]       = "Create thread",
  [EVENT_RUN_THREAD]          = "Run thread",
  [EVENT_STOP_THREAD]         = "Stop thread",
  [EVENT_THREAD_RUNNABLE]     = "Thread runnable",
  [EVENT_MIGRATE_THREAD]      = "Migrate thread",
  [EVENT_THREAD_WAKEUP]       = "Wakeup thread",
  [EVENT_THREAD_LABEL]        = "Thread label",
  [EVENT_CAP_CREATE]          = "Create capability",
  [EVENT_CAP_DELETE]          = "Delete capability",
  [EVENT_CAP_DISABLE]         = "Disable capability",
  [EVENT_CAP_ENABLE]          = "Enable capability",
  [EVENT_GC_START]            = "Starting GC",
  [EVENT_GC_END]              = "Finished GC",
  [EVENT_REQUEST_SEQ_GC]      = "Request sequential GC",
  [EVENT_REQUEST_PAR_GC]      = "Request parallel GC",
  [EVENT_GC_GLOBAL_SYNC]      = "Synchronise stop-the-world GC",
  [EVENT_GC_STATS_GHC]        = "GC statistics",
  [EVENT_HEAP_INFO_GHC]       = "Heap static parameters",
  [EVENT_HEAP_ALLOCATED]      = "Total heap mem ever allocated",
  [EVENT_HEAP_SIZE]           = "Current heap size",
  [EVENT_HEAP_LIVE]           = "Current heap live data",
  [EVENT_CREATE_SPARK_THREAD] = "Create spark thread",
  [EVENT_LOG_MSG]             = "Log message",
  [EVENT_USER_MSG]            = "User message",
  [EVENT_USER_MARKER]         = "User marker",
  [EVENT_GC_IDLE]             = "GC idle",
  [EVENT_GC_WORK]             = "GC working",
  [EVENT_GC_DONE]             = "GC done",
  [EVENT_BLOCK_MARKER]        = "Block marker",
  [EVENT_CAPSET_CREATE]       = "Create capability set",
  [EVENT_CAPSET_DELETE]       = "Delete capability set",
  [EVENT_CAPSET_ASSIGN_CAP]   = "Add capability to capability set",
  [EVENT_CAPSET_REMOVE_CAP]   = "Remove capability from capability set",
  [EVENT_RTS_IDENTIFIER]      = "RTS name and version",
  [EVENT_PROGRAM_ARGS]        = "Program arguments",
  [EVENT_PROGRAM_ENV]         = "Program environment variables",
  [EVENT_OSPROCESS_PID]       = "Process ID",
  [EVENT_OSPROCESS_PPID]      = "Parent process ID",
  [EVENT_WALL_CLOCK_TIME]     = "Wall clock time",
  [EVENT_SPARK_COUNTERS]      = "Spark counters",
  [EVENT_SPARK_CREATE]        = "Spark create",
  [EVENT_SPARK_DUD]           = "Spark dud",
  [EVENT_SPARK_OVERFLOW]      = "Spark overflow",
  [EVENT_SPARK_RUN]           = "Spark run",
  [EVENT_SPARK_STEAL]         = "Spark steal",
  [EVENT_SPARK_FIZZLE]        = "Spark fizzle",
  [EVENT_SPARK_GC]            = "Spark GC",
  [EVENT_TASK_CREATE]         = "Task create",
  [EVENT_TASK_MIGRATE]        = "Task migrate",
  [EVENT_TASK_DELETE]         = "Task delete",
  [EVENT_HACK_BUG_T9003]      = "Empty event for bug #9003",
  [EVENT_HEAP_PROF_BEGIN]     = "Start of heap profile",
  [EVENT_HEAP_PROF_COST_CENTRE]   = "Cost center definition",
  [EVENT_HEAP_PROF_SAMPLE_BEGIN]  = "Start of heap profile sample",
  [EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN]  = "Start of heap profile (biographical) sample",
  [EVENT_HEAP_PROF_SAMPLE_END]    = "End of heap profile sample",
  [EVENT_HEAP_PROF_SAMPLE_STRING] = "Heap profile string sample",
  [EVENT_HEAP_PROF_SAMPLE_COST_CENTRE] = "Heap profile cost-centre sample",
  [EVENT_PROF_SAMPLE_COST_CENTRE] = "Time profile cost-centre stack",
  [EVENT_PROF_BEGIN] = "Start of a time profile",
  [EVENT_USER_BINARY_MSG]     = "User binary message",
  [EVENT_CONC_MARK_BEGIN]        = "Begin concurrent mark phase",
  [EVENT_CONC_MARK_END]          = "End concurrent mark phase",
  [EVENT_CONC_SYNC_BEGIN]        = "Begin concurrent GC synchronisation",
  [EVENT_CONC_SYNC_END]          = "End concurrent GC synchronisation",
  [EVENT_CONC_SWEEP_BEGIN]       = "Begin concurrent sweep",
  [EVENT_CONC_SWEEP_END]         = "End concurrent sweep",
  [EVENT_CONC_UPD_REM_SET_FLUSH] = "Update remembered set flushed",
  [EVENT_NONMOVING_HEAP_CENSUS]  = "Nonmoving heap census"
};

// Event type.

typedef struct _EventType {
  EventTypeNum etNum;  // Event Type number.
  uint32_t   size;     // size of the payload in bytes
  char *desc;     // Description
} EventType;

EventType eventTypes[NUM_GHC_EVENT_TAGS];

static void initEventsBuf(EventsBuf* eb, StgWord64 size, EventCapNo capno);
static void resetEventsBuf(EventsBuf* eb);
static void printAndClearEventBuf (EventsBuf *eventsBuf);

static void postEventType(EventsBuf *eb, EventType *et);

static void postLogMsg(EventsBuf *eb, EventTypeNum type, char *msg, va_list ap);

static void postBlockMarker(EventsBuf *eb);
static void closeBlockMarker(EventsBuf *ebuf);

static StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum);
static StgBool hasRoomForVariableEvent(EventsBuf *eb, uint32_t payload_bytes);

static void ensureRoomForEvent(EventsBuf *eb, EventTypeNum tag);
static int ensureRoomForVariableEvent(EventsBuf *eb, StgWord16 size);

static inline void postWord8(EventsBuf *eb, StgWord8 i)
{
    *(eb->pos++) = i;
}

static inline void postWord16(EventsBuf *eb, StgWord16 i)
{
    postWord8(eb, (StgWord8)(i >> 8));
    postWord8(eb, (StgWord8)i);
}

static inline void postWord32(EventsBuf *eb, StgWord32 i)
{
    postWord16(eb, (StgWord16)(i >> 16));
    postWord16(eb, (StgWord16)i);
}

static inline void postWord64(EventsBuf *eb, StgWord64 i)
{
    postWord32(eb, (StgWord32)(i >> 32));
    postWord32(eb, (StgWord32)i);
}

static inline void postBuf(EventsBuf *eb, StgWord8 *buf, uint32_t size)
{
    memcpy(eb->pos, buf, size);
    eb->pos += size;
}

/* Post a null-terminated string to the event log.
 * It is the caller's responsibility to ensure that there is
 * enough room for strlen(buf)+1 bytes.
 */
static inline void postString(EventsBuf *eb, const char *buf)
{
    if (buf) {
        const int len = strlen(buf);
        ASSERT(eb->begin + eb->size > eb->pos + len);
        memcpy(eb->pos, buf, len);
        eb->pos += len;
    }
    *eb->pos = 0;
    eb->pos++;
}

static inline StgWord64 time_ns(void)
{ return TimeToNS(stat_getElapsedTime()); }

static inline void postEventTypeNum(EventsBuf *eb, EventTypeNum etNum)
{ postWord16(eb, etNum); }

static inline void postTimestamp(EventsBuf *eb)
{ postWord64(eb, time_ns()); }

static inline void postThreadID(EventsBuf *eb, EventThreadID id)
{ postWord32(eb,id); }

static inline void postCapNo(EventsBuf *eb, EventCapNo no)
{ postWord16(eb,no); }

static inline void postCapsetID(EventsBuf *eb, EventCapsetID id)
{ postWord32(eb,id); }

static inline void postCapsetType(EventsBuf *eb, EventCapsetType type)
{ postWord16(eb,type); }

static inline void postOSProcessId(EventsBuf *eb, pid_t pid)
{ postWord32(eb, pid); }

static inline void postKernelThreadId(EventsBuf *eb, EventKernelThreadId tid)
{ postWord64(eb, tid); }

static inline void postTaskId(EventsBuf *eb, EventTaskId tUniq)
{ postWord64(eb, tUniq); }

static inline void postPayloadSize(EventsBuf *eb, EventPayloadSize size)
{ postWord16(eb,size); }

static inline void postEventHeader(EventsBuf *eb, EventTypeNum type)
{
    postEventTypeNum(eb, type);
    postTimestamp(eb);
}

static inline void postInt8(EventsBuf *eb, StgInt8 i)
{ postWord8(eb, (StgWord8)i); }

static inline void postInt32(EventsBuf *eb, StgInt32 i)
{ postWord32(eb, (StgWord32)i); }

#define EVENT_SIZE_DYNAMIC (-1)

static void
initEventLogWriter(void)
{
    if (event_log_writer != NULL &&
            event_log_writer->initEventLogWriter != NULL) {
        event_log_writer->initEventLogWriter();
    }
}

static bool
writeEventLog(void *eventlog, size_t eventlog_size)
{
    if (event_log_writer != NULL &&
            event_log_writer->writeEventLog != NULL) {
        return event_log_writer->writeEventLog(eventlog, eventlog_size);
    } else {
        return false;
    }
}

static void
stopEventLogWriter(void)
{
    if (event_log_writer != NULL &&
            event_log_writer->stopEventLogWriter != NULL) {
        event_log_writer->stopEventLogWriter();
    }
}

void
flushEventLog(void)
{
    if (event_log_writer != NULL &&
            event_log_writer->flushEventLog != NULL) {
        event_log_writer->flushEventLog();
    }
}

static void
init_event_types(void)
{
    for (int t = 0; t < NUM_GHC_EVENT_TAGS; ++t) {
        eventTypes[t].etNum = t;
        eventTypes[t].desc = EventDesc[t];

        switch (t) {
        case EVENT_CREATE_THREAD:   // (cap, thread)
        case EVENT_RUN_THREAD:      // (cap, thread)
        case EVENT_THREAD_RUNNABLE: // (cap, thread)
        case EVENT_CREATE_SPARK_THREAD: // (cap, spark_thread)
            eventTypes[t].size = sizeof(EventThreadID);
            break;

        case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
        case EVENT_THREAD_WAKEUP:   // (cap, thread, other_cap)
            eventTypes[t].size =
                sizeof(EventThreadID) + sizeof(EventCapNo);
            break;

        case EVENT_STOP_THREAD:     // (cap, thread, status)
            eventTypes[t].size = sizeof(EventThreadID)
                               + sizeof(StgWord16)
                               + sizeof(EventThreadID);
            break;

        case EVENT_CAP_CREATE:      // (cap)
        case EVENT_CAP_DELETE:      // (cap)
        case EVENT_CAP_ENABLE:      // (cap)
        case EVENT_CAP_DISABLE:     // (cap)
            eventTypes[t].size = sizeof(EventCapNo);
            break;

        case EVENT_CAPSET_CREATE:   // (capset, capset_type)
            eventTypes[t].size =
                sizeof(EventCapsetID) + sizeof(EventCapsetType);
            break;

        case EVENT_CAPSET_DELETE:   // (capset)
            eventTypes[t].size = sizeof(EventCapsetID);
            break;

        case EVENT_CAPSET_ASSIGN_CAP:  // (capset, cap)
        case EVENT_CAPSET_REMOVE_CAP:
            eventTypes[t].size =
                sizeof(EventCapsetID) + sizeof(EventCapNo);
            break;

        case EVENT_OSPROCESS_PID:   // (cap, pid)
        case EVENT_OSPROCESS_PPID:
            eventTypes[t].size =
                sizeof(EventCapsetID) + sizeof(StgWord32);
            break;

        case EVENT_WALL_CLOCK_TIME: // (capset, unix_epoch_seconds, nanoseconds)
            eventTypes[t].size =
                sizeof(EventCapsetID) + sizeof(StgWord64) + sizeof(StgWord32);
            break;

        case EVENT_SPARK_STEAL:     // (cap, victim_cap)
            eventTypes[t].size =
                sizeof(EventCapNo);
            break;

        case EVENT_REQUEST_SEQ_GC:  // (cap)
        case EVENT_REQUEST_PAR_GC:  // (cap)
        case EVENT_GC_START:        // (cap)
        case EVENT_GC_END:          // (cap)
        case EVENT_GC_IDLE:
        case EVENT_GC_WORK:
        case EVENT_GC_DONE:
        case EVENT_GC_GLOBAL_SYNC:  // (cap)
        case EVENT_SPARK_CREATE:    // (cap)
        case EVENT_SPARK_DUD:       // (cap)
        case EVENT_SPARK_OVERFLOW:  // (cap)
        case EVENT_SPARK_RUN:       // (cap)
        case EVENT_SPARK_FIZZLE:    // (cap)
        case EVENT_SPARK_GC:        // (cap)
            eventTypes[t].size = 0;
            break;

        case EVENT_LOG_MSG:          // (msg)
        case EVENT_USER_MSG:         // (msg)
        case EVENT_USER_MARKER:      // (markername)
        case EVENT_RTS_IDENTIFIER:   // (capset, str)
        case EVENT_PROGRAM_ARGS:     // (capset, strvec)
        case EVENT_PROGRAM_ENV:      // (capset, strvec)
        case EVENT_THREAD_LABEL:     // (thread, str)
            eventTypes[t].size = 0xffff;
            break;

        case EVENT_SPARK_COUNTERS:   // (cap, 7*counter)
            eventTypes[t].size = 7 * sizeof(StgWord64);
            break;

        case EVENT_HEAP_ALLOCATED:    // (heap_capset, alloc_bytes)
        case EVENT_HEAP_SIZE:         // (heap_capset, size_bytes)
        case EVENT_HEAP_LIVE:         // (heap_capset, live_bytes)
            eventTypes[t].size = sizeof(EventCapsetID) + sizeof(StgWord64);
            break;

        case EVENT_HEAP_INFO_GHC:     // (heap_capset, n_generations,
                                      //  max_heap_size, alloc_area_size,
                                      //  mblock_size, block_size)
            eventTypes[t].size = sizeof(EventCapsetID)
                               + sizeof(StgWord16)
                               + sizeof(StgWord64) * 4;
            break;

        case EVENT_GC_STATS_GHC:      // (heap_capset, generation,
                                      //  copied_bytes, slop_bytes, frag_bytes,
                                      //  par_n_threads,
                                      //  par_max_copied, par_tot_copied,
                                      //  par_balanced_copied
                                      //  )
            eventTypes[t].size = sizeof(EventCapsetID)
                               + sizeof(StgWord16)
                               + sizeof(StgWord64) * 3
                               + sizeof(StgWord32)
                               + sizeof(StgWord64) * 3;
            break;

        case EVENT_TASK_CREATE:   // (taskId, cap, tid)
            eventTypes[t].size = sizeof(EventTaskId)
                               + sizeof(EventCapNo)
                               + sizeof(EventKernelThreadId);
            break;

        case EVENT_TASK_MIGRATE:   // (taskId, cap, new_cap)
            eventTypes[t].size =
                sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventCapNo);
            break;

        case EVENT_TASK_DELETE:   // (taskId)
            eventTypes[t].size = sizeof(EventTaskId);
            break;

        case EVENT_BLOCK_MARKER:
            eventTypes[t].size = sizeof(StgWord32) + sizeof(EventTimestamp) +
                sizeof(EventCapNo);
            break;

        case EVENT_HACK_BUG_T9003:
            eventTypes[t].size = 0;
            break;

        case EVENT_HEAP_PROF_BEGIN:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_HEAP_PROF_COST_CENTRE:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_HEAP_PROF_SAMPLE_BEGIN:
            eventTypes[t].size = 8;
            break;

        case EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN:
            eventTypes[t].size = 16;
            break;

        case EVENT_HEAP_PROF_SAMPLE_END:
            eventTypes[t].size = 8;
            break;

        case EVENT_HEAP_PROF_SAMPLE_STRING:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_HEAP_PROF_SAMPLE_COST_CENTRE:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_PROF_SAMPLE_COST_CENTRE:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_PROF_BEGIN:
            eventTypes[t].size = 8;
            break;

        case EVENT_USER_BINARY_MSG:
            eventTypes[t].size = EVENT_SIZE_DYNAMIC;
            break;

        case EVENT_CONC_MARK_BEGIN:
        case EVENT_CONC_SYNC_BEGIN:
        case EVENT_CONC_SYNC_END:
        case EVENT_CONC_SWEEP_BEGIN:
        case EVENT_CONC_SWEEP_END:
            eventTypes[t].size = 0;
            break;

        case EVENT_CONC_MARK_END:
            eventTypes[t].size = 4;
            break;

        case EVENT_CONC_UPD_REM_SET_FLUSH: // (cap)
            eventTypes[t].size =
                sizeof(EventCapNo);
            break;

        case EVENT_NONMOVING_HEAP_CENSUS: // (cap, blk_size, active_segs, filled_segs, live_blks)
            eventTypes[t].size = 13;
            break;

        default:
            continue; /* ignore deprecated events */
        }
    }
}

static void
postHeaderEvents(void)
{
    // Write in buffer: the header begin marker.
    postInt32(&eventBuf, EVENT_HEADER_BEGIN);

    // Mark beginning of event types in the header.
    postInt32(&eventBuf, EVENT_HET_BEGIN);

    for (int t = 0; t < NUM_GHC_EVENT_TAGS; ++t) {
        // Write in buffer: the start event type.
        if (eventTypes[t].desc)
            postEventType(&eventBuf, &eventTypes[t]);
    }

    // Mark end of event types in the header.
    postInt32(&eventBuf, EVENT_HET_END);

    // Write in buffer: the header end marker.
    postInt32(&eventBuf, EVENT_HEADER_END);

    // Prepare event buffer for events (data).
    postInt32(&eventBuf, EVENT_DATA_BEGIN);
}

static uint32_t
get_n_capabilities(void)
{
#if defined(THREADED_RTS)
    // XXX n_capabilities may not have been initialized yet
    return (n_capabilities != 0) ? n_capabilities : RtsFlags.ParFlags.nCapabilities;
#else
    return 1;
#endif
}

void
initEventLogging()
{
    init_event_types();

    int num_descs = sizeof(EventDesc) / sizeof(char*);
    if (num_descs != NUM_GHC_EVENT_TAGS) {
        barf("EventDesc array has the wrong number of elements (%d, NUM_GHC_EVENT_TAGS=%d)",
             num_descs, NUM_GHC_EVENT_TAGS);
    }

    /*
     * Allocate buffer(s) to store events.
     * Create buffer large enough for the header begin marker, all event
     * types, and header end marker to prevent checking if buffer has room
     * for each of these steps, and remove the need to flush the buffer to
     * disk during initialization.
     *
     * Use a single buffer to store the header with event types, then flush
     * the buffer so all buffers are empty for writing events.
     */
    moreCapEventBufs(0, get_n_capabilities());

    initEventsBuf(&eventBuf, EVENT_LOG_SIZE, (EventCapNo)(-1));
#if defined(THREADED_RTS)
    initMutex(&eventBufMutex);
#endif
}

enum EventLogStatus
eventLogStatus(void)
{
    if (eventlog_enabled) {
        return EVENTLOG_RUNNING;
    } else {
        return EVENTLOG_NOT_CONFIGURED;
    }
}

static bool
startEventLogging_(void)
{
    initEventLogWriter();

    postHeaderEvents();

    // Flush capEventBuf with header.
    /*
     * Flush header and data begin marker to the file, thus preparing the
     * file to have events written to it.
     */
    printAndClearEventBuf(&eventBuf);

    for (uint32_t c = 0; c < get_n_capabilities(); ++c) {
        postBlockMarker(&capEventBuf[c]);
    }
    return true;
}

bool
startEventLogging(const EventLogWriter *ev_writer)
{
    if (eventlog_enabled || event_log_writer) {
        return false;
    }

    eventlog_enabled = true;
    event_log_writer = ev_writer;
    return startEventLogging_();
}

// Called during forkProcess in the child to restart the eventlog writer.
void
restartEventLogging(void)
{
    freeEventLogging();
    stopEventLogWriter();
    initEventLogging();  // allocate new per-capability buffers
    if (event_log_writer != NULL) {
        startEventLogging_(); // child starts its own eventlog
    }
}

void
endEventLogging(void)
{
    if (!eventlog_enabled)
        return;

    // Flush all events remaining in the buffers.
    for (uint32_t c = 0; c < n_capabilities; ++c) {
        printAndClearEventBuf(&capEventBuf[c]);
    }
    printAndClearEventBuf(&eventBuf);
    resetEventsBuf(&eventBuf); // we don't want the block marker

    // Mark end of events (data).
    postEventTypeNum(&eventBuf, EVENT_DATA_END);

    // Flush the end of data marker.
    printAndClearEventBuf(&eventBuf);

    stopEventLogWriter();
    event_log_writer = NULL;
    eventlog_enabled = false;
}

void
moreCapEventBufs (uint32_t from, uint32_t to)
{
    if (from > 0) {
        capEventBuf = stgReallocBytes(capEventBuf, to * sizeof(EventsBuf),
                                      "moreCapEventBufs");
    } else {
        capEventBuf = stgMallocBytes(to * sizeof(EventsBuf),
                                     "moreCapEventBufs");
    }

    for (uint32_t c = from; c < to; ++c) {
        initEventsBuf(&capEventBuf[c], EVENT_LOG_SIZE, c);
    }

    // The from == 0 already covered in initEventLogging, so we are interested
    // only in case when we are increasing capabilities number
    if (from > 0) {
        for (uint32_t c = from; c < to; ++c) {
           postBlockMarker(&capEventBuf[c]);
        }
    }
}


void
freeEventLogging(void)
{
    // Free events buffer.
    for (uint32_t c = 0; c < n_capabilities; ++c) {
        if (capEventBuf[c].begin != NULL)
            stgFree(capEventBuf[c].begin);
    }
    if (capEventBuf != NULL)  {
        stgFree(capEventBuf);
    }
}

/*
 * Post an event message to the capability's eventlog buffer.
 * If the buffer is full, prints out the buffer and clears it.
 */
void
postSchedEvent (Capability *cap,
                EventTypeNum tag,
                StgThreadID thread,
                StgWord info1,
                StgWord info2)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, tag);

    postEventHeader(eb, tag);

    switch (tag) {
    case EVENT_CREATE_THREAD:   // (cap, thread)
    case EVENT_RUN_THREAD:      // (cap, thread)
    case EVENT_THREAD_RUNNABLE: // (cap, thread)
    {
        postThreadID(eb,thread);
        break;
    }

    case EVENT_CREATE_SPARK_THREAD: // (cap, spark_thread)
    {
        postThreadID(eb,info1 /* spark_thread */);
        break;
    }

    case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
    case EVENT_THREAD_WAKEUP:   // (cap, thread, other_cap)
    {
        postThreadID(eb,thread);
        postCapNo(eb,info1 /* new_cap | victim_cap | other_cap */);
        break;
   }

    case EVENT_STOP_THREAD:     // (cap, thread, status)
    {
        postThreadID(eb,thread);
        postWord16(eb,info1 /* status */);
        postThreadID(eb,info2 /* blocked on thread */);
        break;
    }

    default:
        barf("postSchedEvent: unknown event tag %d", tag);
    }
}

void
postSparkEvent (Capability *cap,
                EventTypeNum tag,
                StgWord info1)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, tag);

    postEventHeader(eb, tag);

    switch (tag) {
    case EVENT_CREATE_SPARK_THREAD: // (cap, spark_thread)
    {
        postThreadID(eb,info1 /* spark_thread */);
        break;
    }

    case EVENT_SPARK_STEAL:         // (cap, victim_cap)
    {
        postCapNo(eb,info1 /* victim_cap */);
        break;
   }

    case EVENT_SPARK_CREATE:        // (cap)
    case EVENT_SPARK_DUD:           // (cap)
    case EVENT_SPARK_OVERFLOW:      // (cap)
    case EVENT_SPARK_RUN:           // (cap)
    case EVENT_SPARK_FIZZLE:        // (cap)
    case EVENT_SPARK_GC:            // (cap)
    {
        break;
    }

    default:
        barf("postSparkEvent: unknown event tag %d", tag);
    }
}

void
postSparkCountersEvent (Capability *cap,
                        SparkCounters counters,
                        StgWord remaining)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, EVENT_SPARK_COUNTERS);

    postEventHeader(eb, EVENT_SPARK_COUNTERS);
    /* EVENT_SPARK_COUNTERS (crt,dud,ovf,cnv,gcd,fiz,rem) */
    postWord64(eb,counters.created);
    postWord64(eb,counters.dud);
    postWord64(eb,counters.overflowed);
    postWord64(eb,counters.converted);
    postWord64(eb,counters.gcd);
    postWord64(eb,counters.fizzled);
    postWord64(eb,remaining);
}

void
postCapEvent (EventTypeNum  tag,
              EventCapNo    capno)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, tag);

    postEventHeader(&eventBuf, tag);

    switch (tag) {
    case EVENT_CAP_CREATE:   // (cap)
    case EVENT_CAP_DELETE:   // (cap)
    case EVENT_CAP_ENABLE:   // (cap)
    case EVENT_CAP_DISABLE:  // (cap)
    {
        postCapNo(&eventBuf,capno);
        break;
    }

    default:
        barf("postCapEvent: unknown event tag %d", tag);
    }

    RELEASE_LOCK(&eventBufMutex);
}

void postCapsetEvent (EventTypeNum tag,
                      EventCapsetID capset,
                      StgWord info)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, tag);

    postEventHeader(&eventBuf, tag);
    postCapsetID(&eventBuf, capset);

    switch (tag) {
    case EVENT_CAPSET_CREATE:   // (capset, capset_type)
    {
        postCapsetType(&eventBuf, info /* capset_type */);
        break;
    }

    case EVENT_CAPSET_DELETE:   // (capset)
    {
        break;
    }

    case EVENT_CAPSET_ASSIGN_CAP:  // (capset, capno)
    case EVENT_CAPSET_REMOVE_CAP:  // (capset, capno)
    {
        postCapNo(&eventBuf, info /* capno */);
        break;
    }
    case EVENT_OSPROCESS_PID:   // (capset, pid)
    case EVENT_OSPROCESS_PPID:  // (capset, parent_pid)
    {
        postOSProcessId(&eventBuf, info);
        break;
    }
    default:
        barf("postCapsetEvent: unknown event tag %d", tag);
    }

    RELEASE_LOCK(&eventBufMutex);
}

void postCapsetStrEvent (EventTypeNum tag,
                         EventCapsetID capset,
                         char *msg)
{
    int strsize = strlen(msg);
    int size = strsize + sizeof(EventCapsetID);
    if (size > EVENT_PAYLOAD_SIZE_MAX) {
        errorBelch("Event size exceeds EVENT_PAYLOAD_SIZE_MAX, bail out");
        return;
    }

    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForVariableEvent(&eventBuf, size)){
        printAndClearEventBuf(&eventBuf);

        if (!hasRoomForVariableEvent(&eventBuf, size)){
            errorBelch("Event size exceeds buffer size, bail out");
            RELEASE_LOCK(&eventBufMutex);
            return;
        }
    }

    postEventHeader(&eventBuf, tag);
    postPayloadSize(&eventBuf, size);
    postCapsetID(&eventBuf, capset);

    postBuf(&eventBuf, (StgWord8*) msg, strsize);

    RELEASE_LOCK(&eventBufMutex);
}

void postCapsetVecEvent (EventTypeNum tag,
                         EventCapsetID capset,
                         int argc,
                         char *argv[])
{
    int size = sizeof(EventCapsetID);

    for (int i = 0; i < argc; i++) {
        // 1 + strlen to account for the trailing \0, used as separator
        size += 1 + strlen(argv[i]);
    }

    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForVariableEvent(&eventBuf, size)){
        printAndClearEventBuf(&eventBuf);

        if(!hasRoomForVariableEvent(&eventBuf, size)){
            errorBelch("Event size exceeds buffer size, bail out");
            RELEASE_LOCK(&eventBufMutex);
            return;
        }
    }

    postEventHeader(&eventBuf, tag);
    postPayloadSize(&eventBuf, size);
    postCapsetID(&eventBuf, capset);

    for (int i = 0; i < argc; i++) {
        // again, 1 + to account for \0
        postBuf(&eventBuf, (StgWord8*) argv[i], 1 + strlen(argv[i]));
    }

    RELEASE_LOCK(&eventBufMutex);
}

void postWallClockTime (EventCapsetID capset)
{
    StgWord64 ts;
    StgWord64 sec;
    StgWord32 nsec;

    ACQUIRE_LOCK(&eventBufMutex);

    /* The EVENT_WALL_CLOCK_TIME event is intended to allow programs
       reading the eventlog to match up the event timestamps with wall
       clock time. The normal event timestamps measure time since the
       start of the program. To align eventlogs from concurrent
       processes we need to be able to match up the timestamps. One way
       to do this is if we know how the timestamps and wall clock time
       match up (and of course if both processes have sufficiently
       synchronised clocks).

       So we want to make sure that the timestamp that we generate for
       this event matches up very closely with the wall clock time.
       Unfortunately we currently have to use two different APIs to get
       the elapsed time vs the wall clock time. So to minimise the
       difference we just call them very close together.
     */

    getUnixEpochTime(&sec, &nsec);  /* Get the wall clock time */
    ts = time_ns();                 /* Get the eventlog timestamp */
    ensureRoomForEvent(&eventBuf, EVENT_WALL_CLOCK_TIME);

    /* Normally we'd call postEventHeader(), but that generates its own
       timestamp, so we go one level lower so we can write out the
       timestamp we already generated above. */
    postEventTypeNum(&eventBuf, EVENT_WALL_CLOCK_TIME);
    postWord64(&eventBuf, ts);

    /* EVENT_WALL_CLOCK_TIME (capset, unix_epoch_seconds, nanoseconds) */
    postCapsetID(&eventBuf, capset);
    postWord64(&eventBuf, sec);
    postWord32(&eventBuf, nsec);

    RELEASE_LOCK(&eventBufMutex);
}

/*
 * Various GC and heap events
 */
void postHeapEvent (Capability    *cap,
                    EventTypeNum   tag,
                    EventCapsetID  heap_capset,
                    W_           info1)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, tag);

    postEventHeader(eb, tag);

    switch (tag) {
    case EVENT_HEAP_ALLOCATED:     // (heap_capset, alloc_bytes)
    case EVENT_HEAP_SIZE:          // (heap_capset, size_bytes)
    case EVENT_HEAP_LIVE:          // (heap_capset, live_bytes)
    {
        postCapsetID(eb, heap_capset);
        postWord64(eb, info1 /* alloc/size/live_bytes */);
        break;
    }

    default:
        barf("postHeapEvent: unknown event tag %d", tag);
    }
}

void postEventHeapInfo (EventCapsetID heap_capset,
                        uint32_t    gens,
                        W_          maxHeapSize,
                        W_          allocAreaSize,
                        W_          mblockSize,
                        W_          blockSize)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_HEAP_INFO_GHC);

    postEventHeader(&eventBuf, EVENT_HEAP_INFO_GHC);
    /* EVENT_HEAP_INFO_GHC (heap_capset, n_generations,
                            max_heap_size, alloc_area_size,
                            mblock_size, block_size) */
    postCapsetID(&eventBuf, heap_capset);
    postWord16(&eventBuf, gens);
    postWord64(&eventBuf, maxHeapSize);
    postWord64(&eventBuf, allocAreaSize);
    postWord64(&eventBuf, mblockSize);
    postWord64(&eventBuf, blockSize);

    RELEASE_LOCK(&eventBufMutex);
}

void postEventGcStats  (Capability    *cap,
                        EventCapsetID  heap_capset,
                        uint32_t     gen,
                        W_           copied,
                        W_           slop,
                        W_           fragmentation,
                        uint32_t     par_n_threads,
                        W_           par_max_copied,
                        W_           par_tot_copied,
                        W_           par_balanced_copied)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, EVENT_GC_STATS_GHC);

    postEventHeader(eb, EVENT_GC_STATS_GHC);
    /* EVENT_GC_STATS_GHC (heap_capset, generation,
                           copied_bytes, slop_bytes, frag_bytes,
                           par_n_threads, par_max_copied,
                           par_tot_copied, par_balanced_copied) */
    postCapsetID(eb, heap_capset);
    postWord16(eb, gen);
    postWord64(eb, copied);
    postWord64(eb, slop);
    postWord64(eb, fragmentation);
    postWord32(eb, par_n_threads);
    postWord64(eb, par_max_copied);
    postWord64(eb, par_tot_copied);
    postWord64(eb, par_balanced_copied);
}

void postTaskCreateEvent (EventTaskId taskId,
                          EventCapNo capno,
                          EventKernelThreadId tid)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_TASK_CREATE);

    postEventHeader(&eventBuf, EVENT_TASK_CREATE);
    /* EVENT_TASK_CREATE (taskID, cap, tid) */
    postTaskId(&eventBuf, taskId);
    postCapNo(&eventBuf, capno);
    postKernelThreadId(&eventBuf, tid);

    RELEASE_LOCK(&eventBufMutex);
}

void postTaskMigrateEvent (EventTaskId taskId,
                           EventCapNo capno,
                           EventCapNo new_capno)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_TASK_MIGRATE);

    postEventHeader(&eventBuf, EVENT_TASK_MIGRATE);
    /* EVENT_TASK_MIGRATE (taskID, cap, new_cap) */
    postTaskId(&eventBuf, taskId);
    postCapNo(&eventBuf, capno);
    postCapNo(&eventBuf, new_capno);

    RELEASE_LOCK(&eventBufMutex);
}

void postTaskDeleteEvent (EventTaskId taskId)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_TASK_DELETE);

    postEventHeader(&eventBuf, EVENT_TASK_DELETE);
    /* EVENT_TASK_DELETE (taskID) */
    postTaskId(&eventBuf, taskId);

    RELEASE_LOCK(&eventBufMutex);
}

void
postEventNoCap (EventTypeNum tag)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, tag);
    postEventHeader(&eventBuf, tag);
    RELEASE_LOCK(&eventBufMutex);
}

void
postEvent (Capability *cap, EventTypeNum tag)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, tag);
    postEventHeader(eb, tag);
}

void
postEventAtTimestamp (Capability *cap, EventTimestamp ts, EventTypeNum tag)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, tag);

    /* Normally we'd call postEventHeader(), but that generates its own
       timestamp, so we go one level lower so we can write out
       the timestamp we received as an argument. */
    postEventTypeNum(eb, tag);
    postWord64(eb, ts);
}

#define BUF 512

void postLogMsg(EventsBuf *eb, EventTypeNum type, char *msg, va_list ap)
{
    char buf[BUF];
    uint32_t size = vsnprintf(buf, BUF, msg,ap);
    if (size > BUF) {
        buf[BUF-1] = '\0';
        size = BUF;
    }

    ensureRoomForVariableEvent(eb, size);

    postEventHeader(eb, type);
    postPayloadSize(eb, size);
    postBuf(eb,(StgWord8*)buf,size);
}

void postMsg(char *msg, va_list ap)
{
    ACQUIRE_LOCK(&eventBufMutex);
    postLogMsg(&eventBuf, EVENT_LOG_MSG, msg, ap);
    RELEASE_LOCK(&eventBufMutex);
}

void postCapMsg(Capability *cap, char *msg, va_list ap)
{
    postLogMsg(&capEventBuf[cap->no], EVENT_LOG_MSG, msg, ap);
}

void postUserEvent(Capability *cap, EventTypeNum type, char *msg)
{
    const size_t size = strlen(msg);
    if (size > EVENT_PAYLOAD_SIZE_MAX) {
        errorBelch("Event size exceeds EVENT_PAYLOAD_SIZE_MAX, bail out");
        return;
    }

    EventsBuf *eb = &capEventBuf[cap->no];
    if (!hasRoomForVariableEvent(eb, size)){
        printAndClearEventBuf(eb);

        if (!hasRoomForVariableEvent(eb, size)){
            errorBelch("Event size exceeds buffer size, bail out");
            return;
        }
    }

    postEventHeader(eb, type);
    postPayloadSize(eb, size);
    postBuf(eb, (StgWord8*) msg, size);
}

void postUserBinaryEvent(Capability   *cap,
                         EventTypeNum  type,
                         uint8_t      *msg,
                         size_t        size)
{
    if (size > EVENT_PAYLOAD_SIZE_MAX) {
        errorBelch("Event size exceeds EVENT_PAYLOAD_SIZE_MAX, bail out");
        return;
    }

    EventsBuf *eb = &capEventBuf[cap->no];
    if (!hasRoomForVariableEvent(eb, size)){
        printAndClearEventBuf(eb);

        if (!hasRoomForVariableEvent(eb, size)){
            errorBelch("Event size exceeds buffer size, bail out");
            return;
        }
    }

    postEventHeader(eb, type);
    postPayloadSize(eb, size);
    postBuf(eb, (StgWord8*) msg, size);
}

void postThreadLabel(Capability    *cap,
                     EventThreadID  id,
                     char          *label)
{
    const int strsize = strlen(label);
    const int size = strsize + sizeof(EventThreadID);
    if (size > EVENT_PAYLOAD_SIZE_MAX) {
        errorBelch("Event size exceeds EVENT_PAYLOAD_SIZE_MAX, bail out");
        return;
    }

    EventsBuf *eb = &capEventBuf[cap->no];
    if (!hasRoomForVariableEvent(eb, size)){
        printAndClearEventBuf(eb);

        if (!hasRoomForVariableEvent(eb, size)){
            errorBelch("Event size exceeds buffer size, bail out");
            return;
        }
    }

    postEventHeader(eb, EVENT_THREAD_LABEL);
    postPayloadSize(eb, size);
    postThreadID(eb, id);
    postBuf(eb, (StgWord8*) label, strsize);
}

void postConcUpdRemSetFlush(Capability *cap)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, EVENT_CONC_UPD_REM_SET_FLUSH);
    postEventHeader(eb, EVENT_CONC_UPD_REM_SET_FLUSH);
    postCapNo(eb, cap->no);
}

void postConcMarkEnd(StgWord32 marked_obj_count)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_CONC_MARK_END);
    postEventHeader(&eventBuf, EVENT_CONC_MARK_END);
    postWord32(&eventBuf, marked_obj_count);
    RELEASE_LOCK(&eventBufMutex);
}

void postNonmovingHeapCensus(int log_blk_size,
                             const struct NonmovingAllocCensus *census)
{
    ACQUIRE_LOCK(&eventBufMutex);
    postEventHeader(&eventBuf, EVENT_NONMOVING_HEAP_CENSUS);
    postWord8(&eventBuf, log_blk_size);
    postWord32(&eventBuf, census->n_active_segs);
    postWord32(&eventBuf, census->n_filled_segs);
    postWord32(&eventBuf, census->n_live_blocks);
    RELEASE_LOCK(&eventBufMutex);
}

void closeBlockMarker (EventsBuf *ebuf)
{
    if (ebuf->marker)
    {
        // (type:16, time:64, size:32, end_time:64)

        StgInt8* save_pos = ebuf->pos;
        ebuf->pos = ebuf->marker + sizeof(EventTypeNum) +
                    sizeof(EventTimestamp);
        postWord32(ebuf, save_pos - ebuf->marker);
        postTimestamp(ebuf);
        ebuf->pos = save_pos;
        ebuf->marker = NULL;
    }
}


void postBlockMarker (EventsBuf *eb)
{
    ensureRoomForEvent(eb, EVENT_BLOCK_MARKER);

    closeBlockMarker(eb);

    eb->marker = eb->pos;
    postEventHeader(eb, EVENT_BLOCK_MARKER);
    postWord32(eb,0); // these get filled in later by closeBlockMarker();
    postWord64(eb,0);
    postCapNo(eb, eb->capno);
}

static HeapProfBreakdown getHeapProfBreakdown(void)
{
    switch (RtsFlags.ProfFlags.doHeapProfile) {
    case HEAP_BY_CCS:
        return HEAP_PROF_BREAKDOWN_COST_CENTRE;
    case HEAP_BY_MOD:
        return HEAP_PROF_BREAKDOWN_MODULE;
    case HEAP_BY_DESCR:
        return HEAP_PROF_BREAKDOWN_CLOSURE_DESCR;
    case HEAP_BY_TYPE:
        return HEAP_PROF_BREAKDOWN_TYPE_DESCR;
    case HEAP_BY_RETAINER:
        return HEAP_PROF_BREAKDOWN_RETAINER;
    case HEAP_BY_LDV:
        return HEAP_PROF_BREAKDOWN_BIOGRAPHY;
    case HEAP_BY_CLOSURE_TYPE:
        return HEAP_PROF_BREAKDOWN_CLOSURE_TYPE;
    default:
        barf("getHeapProfBreakdown: unknown heap profiling mode");
    }
}

void postHeapProfBegin(StgWord8 profile_id)
{
    ACQUIRE_LOCK(&eventBufMutex);
    PROFILING_FLAGS *flags = &RtsFlags.ProfFlags;
    StgWord modSelector_len   =
        flags->modSelector ? strlen(flags->modSelector) : 0;
    StgWord descrSelector_len =
        flags->descrSelector ? strlen(flags->descrSelector) : 0;
    StgWord typeSelector_len  =
        flags->typeSelector ? strlen(flags->typeSelector) : 0;
    StgWord ccSelector_len    =
        flags->ccSelector ? strlen(flags->ccSelector) : 0;
    StgWord ccsSelector_len   =
        flags->ccsSelector ? strlen(flags->ccsSelector) : 0;
    StgWord retainerSelector_len =
        flags->retainerSelector ? strlen(flags->retainerSelector) : 0;
    StgWord bioSelector_len   =
        flags->bioSelector ? strlen(flags->bioSelector) : 0;
    StgWord len =
        1+8+4 + modSelector_len + descrSelector_len +
        typeSelector_len + ccSelector_len + ccsSelector_len +
        retainerSelector_len + bioSelector_len + 7;
    ensureRoomForVariableEvent(&eventBuf, len);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_BEGIN);
    postPayloadSize(&eventBuf, len);
    postWord8(&eventBuf, profile_id);
    postWord64(&eventBuf, TimeToNS(flags->heapProfileInterval));
    postWord32(&eventBuf, getHeapProfBreakdown());
    postString(&eventBuf, flags->modSelector);
    postString(&eventBuf, flags->descrSelector);
    postString(&eventBuf, flags->typeSelector);
    postString(&eventBuf, flags->ccSelector);
    postString(&eventBuf, flags->ccsSelector);
    postString(&eventBuf, flags->retainerSelector);
    postString(&eventBuf, flags->bioSelector);
    RELEASE_LOCK(&eventBufMutex);
}

void postHeapProfSampleBegin(StgInt era)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_HEAP_PROF_SAMPLE_BEGIN);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_SAMPLE_BEGIN);
    postWord64(&eventBuf, era);
    RELEASE_LOCK(&eventBufMutex);
}


void postHeapBioProfSampleBegin(StgInt era, StgWord64 time)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN);
    postEventHeader(&eventBuf, EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN);
    postWord64(&eventBuf, era);
    postWord64(&eventBuf, time);
    RELEASE_LOCK(&eventBufMutex);
}

void postHeapProfSampleEnd(StgInt era)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_HEAP_PROF_SAMPLE_END);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_SAMPLE_END);
    postWord64(&eventBuf, era);
    RELEASE_LOCK(&eventBufMutex);
}

void postHeapProfSampleString(StgWord8 profile_id,
                              const char *label,
                              StgWord64 residency)
{
    ACQUIRE_LOCK(&eventBufMutex);
    StgWord label_len = strlen(label);
    StgWord len = 1+8+label_len+1;
    ensureRoomForVariableEvent(&eventBuf, len);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_SAMPLE_STRING);
    postPayloadSize(&eventBuf, len);
    postWord8(&eventBuf, profile_id);
    postWord64(&eventBuf, residency);
    postString(&eventBuf, label);
    RELEASE_LOCK(&eventBufMutex);
}

#if defined(PROFILING)
void postHeapProfCostCentre(StgWord32 ccID,
                            const char *label,
                            const char *module,
                            const char *srcloc,
                            StgBool is_caf)
{
    ACQUIRE_LOCK(&eventBufMutex);
    StgWord label_len = strlen(label);
    StgWord module_len = strlen(module);
    StgWord srcloc_len = strlen(srcloc);
    StgWord len = 4+label_len+module_len+srcloc_len+3+1;
    ensureRoomForVariableEvent(&eventBuf, len);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_COST_CENTRE);
    postPayloadSize(&eventBuf, len);
    postWord32(&eventBuf, ccID);
    postString(&eventBuf, label);
    postString(&eventBuf, module);
    postString(&eventBuf, srcloc);
    postWord8(&eventBuf, is_caf);
    RELEASE_LOCK(&eventBufMutex);
}

void postHeapProfSampleCostCentre(StgWord8 profile_id,
                                  CostCentreStack *stack,
                                  StgWord64 residency)
{
    ACQUIRE_LOCK(&eventBufMutex);
    StgWord depth = 0;
    CostCentreStack *ccs;
    for (ccs = stack; ccs != NULL && ccs != CCS_MAIN; ccs = ccs->prevStack)
        depth++;
    if (depth > 0xff) depth = 0xff;

    StgWord len = 1+8+1+depth*4;
    ensureRoomForVariableEvent(&eventBuf, len);
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_SAMPLE_COST_CENTRE);
    postPayloadSize(&eventBuf, len);
    postWord8(&eventBuf, profile_id);
    postWord64(&eventBuf, residency);
    postWord8(&eventBuf, depth);
    for (ccs = stack;
         depth>0 && ccs != NULL && ccs != CCS_MAIN;
         ccs = ccs->prevStack, depth--)
        postWord32(&eventBuf, ccs->cc->ccID);
    RELEASE_LOCK(&eventBufMutex);
}


void postProfSampleCostCentre(Capability *cap,
                              CostCentreStack *stack,
                              StgWord64 tick)
{
    ACQUIRE_LOCK(&eventBufMutex);
    StgWord depth = 0;
    CostCentreStack *ccs;
    for (ccs = stack; ccs != NULL && ccs != CCS_MAIN; ccs = ccs->prevStack)
        depth++;
    if (depth > 0xff) depth = 0xff;

    StgWord len = 4+8+1+depth*4;
    ensureRoomForVariableEvent(&eventBuf, len);
    postEventHeader(&eventBuf, EVENT_PROF_SAMPLE_COST_CENTRE);
    postPayloadSize(&eventBuf, len);
    postWord32(&eventBuf, cap->no);
    postWord64(&eventBuf, tick);
    postWord8(&eventBuf, depth);
    for (ccs = stack;
         depth>0 && ccs != NULL && ccs != CCS_MAIN;
         ccs = ccs->prevStack, depth--)
        postWord32(&eventBuf, ccs->cc->ccID);
    RELEASE_LOCK(&eventBufMutex);
}

// This event is output at the start of profiling so the tick interval can
// be reported. Once the tick interval is reported the total executation time
// can be calculuated from how many samples there are.
void postProfBegin(void)
{
    ACQUIRE_LOCK(&eventBufMutex);
    postEventHeader(&eventBuf, EVENT_PROF_BEGIN);
    // The interval that each tick was sampled, in nanoseconds
    postWord64(&eventBuf, TimeToNS(RtsFlags.MiscFlags.tickInterval));
    RELEASE_LOCK(&eventBufMutex);
}
#endif /* PROFILING */

void printAndClearEventBuf (EventsBuf *ebuf)
{
    closeBlockMarker(ebuf);

    if (ebuf->begin != NULL && ebuf->pos != ebuf->begin)
    {
        size_t elog_size = ebuf->pos - ebuf->begin;
        if (!writeEventLog(ebuf->begin, elog_size)) {
            debugBelch(
                    "printAndClearEventLog: could not flush event log\n"
                );
            resetEventsBuf(ebuf);
            return;
        }

        resetEventsBuf(ebuf);
        flushCount++;

        postBlockMarker(ebuf);
    }
}

void initEventsBuf(EventsBuf* eb, StgWord64 size, EventCapNo capno)
{
    eb->begin = eb->pos = stgMallocBytes(size, "initEventsBuf");
    eb->size = size;
    eb->marker = NULL;
    eb->capno = capno;
}

void resetEventsBuf(EventsBuf* eb)
{
    eb->pos = eb->begin;
    eb->marker = NULL;
}

StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum)
{
  uint32_t size = sizeof(EventTypeNum) + sizeof(EventTimestamp) + eventTypes[eNum].size;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

StgBool hasRoomForVariableEvent(EventsBuf *eb, uint32_t payload_bytes)
{
  uint32_t size = sizeof(EventTypeNum) + sizeof(EventTimestamp) +
      sizeof(EventPayloadSize) + payload_bytes;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

void ensureRoomForEvent(EventsBuf *eb, EventTypeNum tag)
{
    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
}

int ensureRoomForVariableEvent(EventsBuf *eb, StgWord16 size)
{
    if (!hasRoomForVariableEvent(eb, size)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
        if (!hasRoomForVariableEvent(eb, size))
            return 1; // Not enough space
    }
    return 0;
}


void postEventType(EventsBuf *eb, EventType *et)
{
    postInt32(eb, EVENT_ET_BEGIN);
    postEventTypeNum(eb, et->etNum);
    postWord16(eb, (StgWord16)et->size);
    const int desclen = strlen(et->desc);
    postWord32(eb, desclen);
    for (int d = 0; d < desclen; ++d) {
        postInt8(eb, (StgInt8)et->desc[d]);
    }
    postWord32(eb, 0); // no extensions yet
    postInt32(eb, EVENT_ET_END);
}

#else

enum EventLogStatus eventLogStatus(void)
{
    return EVENTLOG_NOT_SUPPORTED;
}

bool startEventLogging(const EventLogWriter *writer STG_UNUSED) {
    return false;
}

void endEventLogging(void) {}

#endif /* TRACING */
