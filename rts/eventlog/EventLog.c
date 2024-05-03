/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#if defined(TRACING)

#include "Trace.h"
#include "Capability.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "EventLog.h"
#include "Schedule.h"

#include <string.h>
#include <stdio.h>
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#define MIN(x,y) ((x) < (y) ? (x) : (y))

Mutex state_change_mutex;
bool eventlog_enabled; // protected by state_change_mutex to ensure
                       // serialisation of calls to
                       // startEventLogging/endEventLogging

/* Note [Eventlog concurrency]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The eventlog is designed to handle high rates of concurrent event posting by
 * multiple capabilities. For this reason, each capability has its own local
 * event buffer, which is flushed when filled.
 *
 * Additionally, there is a single global event buffer (`eventBuf`), which is
 * used for various administrative things (e.g. posting the header) and in
 * cases where we want to post events yet don't hold a capability.
 *
 * Whether or not events are posted is determined by the global flag
 * eventlog_enabled.  Naturally, starting and stopping of logging are a bit
 * subtle. In particular, we need to ensure that the header is the *first*
 * thing to appear in the event stream. To ensure that multiple threads don't
 * race to start/stop logging we protect eventlog_enabled with
 * state_change_mutex. Moreover, we only set eventlog_enabled *after* we have
 * posted the header.
 *
 * Event buffers uphold the invariant that they always begin with a start-block
 * marker. This is enforced by calls to postBlockMarker in:
 *
 *  - initEventsBufs (during buffer initialization)
 *  - printAndClearEventBuf (after the buffer is filled)
 *  - moreCapEventBufs (when the number of capabilities changes)
 *
 * The one place where we *don't* want a block marker is when posting the
 * eventlog header. We achieve this by first resetting the eventlog buffer
 * before posting the header (in postHeader).
 *
 * Stopping eventlogging is a bit involved:
 *
 *  1. first take state_change_mutex, to ensure we don't race with another
 *     thread to stop logging.
 *  2. disable eventlog_enabled, to ensure that no capabilities post further
 *     events
 *  3. request that all capabilities flush their eventlog buffers. This
 *     achieves two things: (a) ensures that all events make it to the output
 *     stream, and (b) serves as a memory barrier, ensuring that all
 *     capabilities see that eventlogging is now disabled
 *  4. wait until all capabilities have flushed.
 *  5. post the end-of-data marker
 *  6. stop the writer
 *  7. release state_change_mutex
 *
 * Note that a corollary of this is that !eventlog_enabled implies that the
 * eventlog buffers are all empty (modulo the block marker that all buffers
 * always have).
 *
 * Changing the number of capabilities is fairly straightforward since we hold
 * all capabilities when the capability count is changed. The one slight corner
 * case is that we must ensure that the buffers of any disabled capabilities are
 * flushed, lest their events are stuck in limbo. This is achieved with a call to
 * flushLocalEventsBuf in traceCapDisable.
 *
 *
 * Note [Maximum event length]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The maximum length of an eventlog event is determined by the maximum event
 * buffer size, EVENT_LOG_SIZE. We must ensure that no variable-length event
 * exceeds this limit. For this reason we impose maximum length limits on
 * fields which may have unbounded values.
 */

static const EventLogWriter *event_log_writer = NULL;

// List of initialisation functions which are called each time the
// eventlog is restarted
static eventlog_init_func_t *eventlog_header_funcs = NULL;

// See Note [Maximum event length]
#define EVENT_LOG_SIZE 2 * (1024 * 1024) // 2MB

static int flushCount = 0;

// Struct for record keeping of buffer to store event types and events.
//
// Invariant: The event buffer will always begin with a block-start marker.
typedef struct _EventsBuf {
  StgInt8 *begin;
  StgInt8 *pos;
  StgInt8 *marker;
  StgWord64 size;
  EventCapNo capno; // which capability this buffer belongs to, or -1
} EventsBuf;

static EventsBuf *capEventBuf; // one EventsBuf for each Capability

static EventsBuf eventBuf; // an EventsBuf not associated with any Capability
#if defined(THREADED_RTS)
static Mutex eventBufMutex; // protected by this mutex
#endif

// Event type
typedef struct _EventType {
  EventTypeNum etNum;  // Event Type number.
  uint32_t   size;     // size of the payload in bytes
  char *desc;     // Description
} EventType;

#include "rts/EventTypes.h"

static void initEventsBuf(EventsBuf* eb, StgWord64 size, EventCapNo capno);
static void resetEventsBuf(EventsBuf* eb);
static void printAndClearEventBuf (EventsBuf *eventsBuf);

static void postEventType(EventsBuf *eb, EventType *et);

static void postLogMsg(EventsBuf *eb, EventTypeNum type, char *msg, va_list ap);

static void postBlockMarker(EventsBuf *eb);
static void closeBlockMarker(EventsBuf *ebuf);

static StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum);
static StgBool hasRoomForVariableEvent(EventsBuf *eb, StgWord payload_bytes);

static void freeEventLoggingBuffer(void);

static void ensureRoomForEvent(EventsBuf *eb, EventTypeNum tag);
static int ensureRoomForVariableEvent(EventsBuf *eb, StgWord size);

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

static inline void postBuf(EventsBuf *eb, const StgWord8 *buf, uint32_t size)
{
    memcpy(eb->pos, buf, size);
    eb->pos += size;
}

/* Post a null-terminated string up to a given length to the event log. It is
 * the caller's responsibility to ensure that there is enough room for
 * len+1 bytes.
 */
static inline void postStringLen(EventsBuf *eb, const char *buf, StgWord len)
{
    if (buf) {
        ASSERT(eb->begin + eb->size > eb->pos + len + 1);
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

static void
flushEventLogWriter(void)
{
    if (event_log_writer != NULL &&
            event_log_writer->flushEventLog != NULL) {
        event_log_writer->flushEventLog();
    }
}

static void
postHeaderEvents(void)
{
    // The header must appear first in the output stream, without the
    // the block start marker we previously added in printAndClearEventBuf.
    resetEventsBuf(&eventBuf);

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

// These events will be reposted every time we restart the eventlog
void
postInitEvent(EventlogInitPost post_init){
    ACQUIRE_LOCK(&state_change_mutex);

    // Add the event to the global list of events that will be rerun when
    // the eventlog is restarted.
    eventlog_init_func_t * new_func;
    new_func = stgMallocBytes(sizeof(eventlog_init_func_t),"eventlog_init_func");
    new_func->init_func = post_init;
    new_func->next = eventlog_header_funcs;
    eventlog_header_funcs = new_func;

    RELEASE_LOCK(&state_change_mutex);
    // Actually post it
    (*post_init)();
    return;
}

// Post events again which happened at the start of the eventlog, added by
// postInitEvent.
static void repostInitEvents(void){
    eventlog_init_func_t * current_event = eventlog_header_funcs;
    for (; current_event != NULL; current_event = current_event->next) {
      (*(current_event->init_func))();
    }
    return;
}

// Clear the eventlog_header_funcs list and free the memory
void resetInitEvents(void){
    eventlog_init_func_t * tmp;
    eventlog_init_func_t * current_event = eventlog_header_funcs;
    for (; current_event != NULL; ) {
      tmp = current_event;
      current_event = current_event->next;
      stgFree(tmp);
    }
    eventlog_header_funcs = NULL;
    return;

}


static uint32_t
get_n_capabilities(void)
{
#if defined(THREADED_RTS)
    // XXX n_capabilities may not have been initialized yet
    unsigned int n = getNumCapabilities();
    return (n != 0) ? n : RtsFlags.ParFlags.nCapabilities;
#else
    return 1;
#endif
}

void
initEventLogging(void)
{
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
    initMutex(&state_change_mutex);
#endif
}

enum EventLogStatus
eventLogStatus(void)
{
    /* This relaxed load is needed as the eventLogStatus is called from
     * handleTick without holding the eventlog state mutex. */
    if (RELAXED_LOAD(&eventlog_enabled)) {
        return EVENTLOG_RUNNING;
    } else {
        return EVENTLOG_NOT_CONFIGURED;
    }
}

static bool
startEventLogging_(void)
{
    initEventLogWriter();

    ACQUIRE_LOCK(&eventBufMutex);
    postHeaderEvents();

    /*
     * Flush header and data begin marker to the file, thus preparing the
     * file to have events written to it.
     */
    printAndClearEventBuf(&eventBuf);

    RELEASE_LOCK(&eventBufMutex);

    return true;
}

bool
startEventLogging(const EventLogWriter *ev_writer)
{
    // Fail early if we race with another thread.
    if (TRY_ACQUIRE_LOCK(&state_change_mutex) != 0) {
        return false;
    }

    // Check whether eventlogging has already been enabled.
    if (eventlog_enabled || event_log_writer) {
        RELEASE_LOCK(&state_change_mutex);
        return false;
    }

    event_log_writer = ev_writer;
    bool ret = startEventLogging_();
    eventlog_enabled = true;
    repostInitEvents();
    RELEASE_LOCK(&state_change_mutex);
    return ret;
}

// Called during forkProcess in the child to restart the eventlog writer.
void
restartEventLogging(void)
{
    freeEventLoggingBuffer();
    stopEventLogWriter();
    initEventLogging();  // allocate new per-capability buffers
    if (event_log_writer != NULL) {
        startEventLogging_(); // child starts its own eventlog
        repostInitEvents();   // Repost the initialisation events
    }
}

// Flush and free capability eventlog buffers in preparation for RTS shutdown.
void
finishCapEventLogging(void)
{
    if (eventlog_enabled) {
        // Flush all events remaining in the capabilities' buffers and free them.
        // N.B. at this point we hold all capabilities.
        for (uint32_t c = 0; c < getNumCapabilities(); ++c) {
            if (capEventBuf[c].begin != NULL) {
                printAndClearEventBuf(&capEventBuf[c]);
                stgFree(capEventBuf[c].begin);
                capEventBuf[c].begin = NULL;
            }
        }
    }
}

void
endEventLogging(void)
{
    ACQUIRE_LOCK(&state_change_mutex);
    if (!eventlog_enabled) {
        RELEASE_LOCK(&state_change_mutex);
        return;
    }

    eventlog_enabled = false;

    // Flush all events remaining in the buffers.
    //
    // N.B. Don't flush if shutting down: this was done in
    // finishCapEventLogging and the capabilities have already been freed.
    if (getSchedState() != SCHED_SHUTTING_DOWN) {
        flushEventLog(NULL);
    }

    ACQUIRE_LOCK(&eventBufMutex);

    // Mark end of events (data).
    postEventTypeNum(&eventBuf, EVENT_DATA_END);

    // Flush the end of data marker.
    printAndClearEventBuf(&eventBuf);

    RELEASE_LOCK(&eventBufMutex);

    stopEventLogWriter();
    event_log_writer = NULL;

    RELEASE_LOCK(&state_change_mutex);
}

/* N.B. we hold all capabilities when this is called */
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

    // Initialize buffers for new capabilities
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

static void
freeEventLoggingBuffer(void)
{
    if (capEventBuf != NULL)  {
        stgFree(capEventBuf);
        capEventBuf = NULL;
    }
}

void
freeEventLogging(void)
{
    freeEventLoggingBuffer();
    resetInitEvents();
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
        int increment = 1 + strlen(argv[i]);
        if (size + increment > EVENT_PAYLOAD_SIZE_MAX) {
            errorBelch("Event size exceeds EVENT_PAYLOAD_SIZE_MAX, record only %"
                       FMT_Word " out of %" FMT_Word " args",
                       (StgWord) i,
                       (StgWord) argc);
            argc = i;
            break;
        } else {
            size += increment;
        }
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
    case EVENT_BLOCKS_SIZE:        // (heap_capset, size_bytes)
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

void postEventMemReturn  (Capability    *cap,
                          EventCapsetID heap_capset,
                          uint32_t current_mblocks,
                          uint32_t needed_mblocks,
                          uint32_t returned_mblocks)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    ensureRoomForEvent(eb, EVENT_MEM_RETURN);

    postEventHeader(eb, EVENT_MEM_RETURN);
    postCapsetID(eb, heap_capset);
    postWord32(eb, current_mblocks);
    postWord32(eb, needed_mblocks);
    postWord32(eb, returned_mblocks);
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
                     char          *label,
                     size_t         len)
{
    const int strsize = (int) len;
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

void postNonmovingHeapCensus(uint16_t blk_size,
                             const struct NonmovingAllocCensus *census)
{
    ACQUIRE_LOCK(&eventBufMutex);
    postEventHeader(&eventBuf, EVENT_NONMOVING_HEAP_CENSUS);
    postWord16(&eventBuf, blk_size);
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
    case HEAP_BY_INFO_TABLE:
        return HEAP_PROF_BREAKDOWN_INFO_TABLE;
    case HEAP_BY_ERA:
        return HEAP_PROF_BREAKDOWN_ERA;
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
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_BEGIN);
    postPayloadSize(&eventBuf, len);
    postWord8(&eventBuf, profile_id);
    postWord64(&eventBuf, TimeToNS(flags->heapProfileInterval));
    postWord32(&eventBuf, getHeapProfBreakdown());
    postStringLen(&eventBuf, flags->modSelector, modSelector_len);
    postStringLen(&eventBuf, flags->descrSelector, descrSelector_len);
    postStringLen(&eventBuf, flags->typeSelector, typeSelector_len);
    postStringLen(&eventBuf, flags->ccSelector, ccSelector_len);
    postStringLen(&eventBuf, flags->ccsSelector, ccsSelector_len);
    postStringLen(&eventBuf, flags->retainerSelector, retainerSelector_len);
    postStringLen(&eventBuf, flags->bioSelector, bioSelector_len);
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
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_SAMPLE_STRING);
    postPayloadSize(&eventBuf, len);
    postWord8(&eventBuf, profile_id);
    postWord64(&eventBuf, residency);
    postStringLen(&eventBuf, label, label_len);
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
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
    postEventHeader(&eventBuf, EVENT_HEAP_PROF_COST_CENTRE);
    postPayloadSize(&eventBuf, len);
    postWord32(&eventBuf, ccID);
    postStringLen(&eventBuf, label, label_len);
    postStringLen(&eventBuf, module, module_len);
    postStringLen(&eventBuf, srcloc, srcloc_len);
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
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
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
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
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
// can be calculated from how many samples there are.
void postProfBegin(void)
{
    ACQUIRE_LOCK(&eventBufMutex);
    postEventHeader(&eventBuf, EVENT_PROF_BEGIN);
    // The interval that each tick was sampled, in nanoseconds
    postWord64(&eventBuf, TimeToNS(RtsFlags.MiscFlags.tickInterval));
    RELEASE_LOCK(&eventBufMutex);
}
#endif /* PROFILING */

#if defined(TICKY_TICKY)
static void postTickyCounterDef(EventsBuf *eb, StgEntCounter *p)
{
    StgWord arg_kinds_len = strlen(p->arg_kinds);
    StgWord str_len = strlen(p->str);
    StgWord ticky_json_len = strlen(p->ticky_json);
    StgWord len = 8 + 2 + arg_kinds_len+1 + str_len+1 + 8 + ticky_json_len+1;
    CHECK(!ensureRoomForVariableEvent(eb, len));
    postEventHeader(eb, EVENT_TICKY_COUNTER_DEF);
    postPayloadSize(eb, len);

    postWord64(eb, (uint64_t)((uintptr_t) p));
    postWord16(eb, (uint16_t) p->arity);
    postStringLen(eb, p->arg_kinds, arg_kinds_len);
    postStringLen(eb, p->str, str_len);
    postWord64(eb, (W_) (INFO_PTR_TO_STRUCT(p->info)));
    postStringLen(eb, p->ticky_json, ticky_json_len);

}

void postTickyCounterDefs(StgEntCounter *counters)
{
    ACQUIRE_LOCK(&eventBufMutex);
    for (StgEntCounter *p = counters; p != NULL; p = p->link) {
        postTickyCounterDef(&eventBuf, p);
    }
    RELEASE_LOCK(&eventBufMutex);
}

static void postTickyCounterSample(EventsBuf *eb, StgEntCounter *p)
{
    if (   p->entry_count == 0
        && p->allocs == 0
        && p->allocd == 0)
        return;

    ensureRoomForEvent(eb, EVENT_TICKY_COUNTER_SAMPLE);
    postEventHeader(eb, EVENT_TICKY_COUNTER_SAMPLE);
    postWord64(eb, (uint64_t)((uintptr_t) p));
    postWord64(eb, p->entry_count);
    postWord64(eb, p->allocs);
    postWord64(eb, p->allocd);

    p->entry_count = 0;
    p->allocs = 0;
    p->allocd = 0;
}

void postTickyCounterSamples(StgEntCounter *counters)
{
    ACQUIRE_LOCK(&eventBufMutex);
    ensureRoomForEvent(&eventBuf, EVENT_TICKY_COUNTER_SAMPLE);
    postEventHeader(&eventBuf, EVENT_TICKY_COUNTER_BEGIN_SAMPLE);
    for (StgEntCounter *p = counters; p != NULL; p = p->link) {
        postTickyCounterSample(&eventBuf, p);
    }
    RELEASE_LOCK(&eventBufMutex);
}
#endif /* TICKY_TICKY */
void postIPE(const InfoProvEnt *ipe)
{
    char closure_desc_buf[CLOSURE_DESC_BUFFER_SIZE] = {};
    formatClosureDescIpe(ipe, closure_desc_buf);

    // See Note [Maximum event length].
    const StgWord MAX_IPE_STRING_LEN = 65535;
    ACQUIRE_LOCK(&eventBufMutex);
    StgWord table_name_len = MIN(strlen(ipe->prov.table_name), MAX_IPE_STRING_LEN);
    StgWord closure_desc_len = MIN(strlen(closure_desc_buf), MAX_IPE_STRING_LEN);
    StgWord ty_desc_len = MIN(strlen(ipe->prov.ty_desc), MAX_IPE_STRING_LEN);
    StgWord label_len = MIN(strlen(ipe->prov.label), MAX_IPE_STRING_LEN);
    StgWord module_len = MIN(strlen(ipe->prov.module), MAX_IPE_STRING_LEN);
    StgWord src_file_len = MIN(strlen(ipe->prov.src_file), MAX_IPE_STRING_LEN);
    StgWord src_span_len = MIN(strlen(ipe->prov.src_span), MAX_IPE_STRING_LEN);

    // 8 for the info word
    // 1 null after each string
    // 1 colon between src_file and src_span
    StgWord extra_comma = 1;
    StgWord len = 8+table_name_len+1+closure_desc_len+1+ty_desc_len+1+label_len+1+module_len+1+src_file_len+1+extra_comma+src_span_len+1;
    CHECK(!ensureRoomForVariableEvent(&eventBuf, len));
    postEventHeader(&eventBuf, EVENT_IPE);
    postPayloadSize(&eventBuf, len);
    postWord64(&eventBuf, (StgWord) INFO_PTR_TO_STRUCT(ipe->info));
    postStringLen(&eventBuf, ipe->prov.table_name, table_name_len);
    postStringLen(&eventBuf, closure_desc_buf, closure_desc_len);
    postStringLen(&eventBuf, ipe->prov.ty_desc, ty_desc_len);
    postStringLen(&eventBuf, ipe->prov.label, label_len);
    postStringLen(&eventBuf, ipe->prov.module, module_len);

    // Manually construct the location field: "<file>:<span>\0"
    postBuf(&eventBuf, (const StgWord8*) ipe->prov.src_file, src_file_len);
    StgWord8 colon = ':';
    postBuf(&eventBuf, &colon, 1);
    postStringLen(&eventBuf, ipe->prov.src_span, src_span_len);

    RELEASE_LOCK(&eventBufMutex);
}

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
            flushEventLogWriter();
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
    postBlockMarker(eb);
}

void resetEventsBuf(EventsBuf* eb)
{
    eb->pos = eb->begin;
    eb->marker = NULL;
}

STG_WARN_UNUSED_RESULT
StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum)
{
  uint32_t size = sizeof(EventTypeNum) + sizeof(EventTimestamp) + eventTypes[eNum].size;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

STG_WARN_UNUSED_RESULT
StgBool hasRoomForVariableEvent(EventsBuf *eb, StgWord payload_bytes)
{
  StgWord size = sizeof(EventTypeNum) + sizeof(EventTimestamp) +
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
        ASSERT(hasRoomForEvent(eb, tag));
    }
}

STG_WARN_UNUSED_RESULT
int ensureRoomForVariableEvent(EventsBuf *eb, StgWord size)
{
    if (!hasRoomForVariableEvent(eb, size)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
        if (!hasRoomForVariableEvent(eb, size)) {
            return 1; // Not enough space
        }
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

void flushLocalEventsBuf(Capability *cap)
{
    EventsBuf *eb = &capEventBuf[cap->no];
    printAndClearEventBuf(eb);
}

// Flush all capabilities' event buffers when we already hold all capabilities.
// Used during forkProcess.
void flushAllCapsEventsBufs(void)
{
    if (!event_log_writer) {
        return;
    }

    ACQUIRE_LOCK(&eventBufMutex);
    printAndClearEventBuf(&eventBuf);
    RELEASE_LOCK(&eventBufMutex);

    for (unsigned int i=0; i < getNumCapabilities(); i++) {
        flushLocalEventsBuf(getCapability(i));
    }
    flushEventLogWriter();
}

void flushEventLog(Capability **cap USED_IF_THREADS)
{
    if (!event_log_writer) {
        return;
    }

    ACQUIRE_LOCK(&eventBufMutex);
    printAndClearEventBuf(&eventBuf);
    RELEASE_LOCK(&eventBufMutex);

#if defined(THREADED_RTS)
    Task *task = getMyTask();
    stopAllCapabilitiesWith(cap, task, SYNC_FLUSH_EVENT_LOG);
    flushAllCapsEventsBufs();
    releaseAllCapabilities(getNumCapabilities(), cap ? *cap : NULL, task);
#else
    flushLocalEventsBuf(getCapability(0));
#endif
    flushEventLogWriter();
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

void flushEventLog(Capability **cap STG_UNUSED) {}

#endif /* TRACING */
