/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#ifdef TRACING

#include "Trace.h"
#include "Capability.h"
#include "RtsUtils.h"
#include "Stats.h"
#include "EventLog.h"

#include <string.h>
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

// PID of the process that writes to event_log_filename (#4512)
static pid_t event_log_pid = -1;

static char *event_log_filename = NULL;

// File for logging events
FILE *event_log_file = NULL;

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
#ifdef THREADED_RTS
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
  [EVENT_STARTUP]             = "Create capabilities",
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
};

// Event type. 

typedef struct _EventType {
  EventTypeNum etNum;  // Event Type number.
  nat   size;     // size of the payload in bytes
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
static StgBool hasRoomForVariableEvent(EventsBuf *eb, nat payload_bytes);

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

static inline void postBuf(EventsBuf *eb, StgWord8 *buf, nat size)
{
    memcpy(eb->pos, buf, size);
    eb->pos += size;
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


void
initEventLogging(void)
{
    StgWord8 t, c;
    nat n_caps;
    char *prog;

    prog = stgMallocBytes(strlen(prog_name) + 1, "initEventLogging");
    strcpy(prog, prog_name);
#ifdef mingw32_HOST_OS
    // on Windows, drop the .exe suffix if there is one
    {
        char *suff;
        suff = strrchr(prog,'.');
        if (suff != NULL && !strcmp(suff,".exe")) {
            *suff = '\0';
        }
    }
#endif

    event_log_filename = stgMallocBytes(strlen(prog)
                                        + 10 /* .%d */
                                        + 10 /* .eventlog */,
                                        "initEventLogging");

    if (sizeof(EventDesc) / sizeof(char*) != NUM_GHC_EVENT_TAGS) {
        barf("EventDesc array has the wrong number of elements");
    }

    if (event_log_pid == -1) { // #4512
        // Single process
        sprintf(event_log_filename, "%s.eventlog", prog);
        event_log_pid = getpid();
    } else {
        // Forked process, eventlog already started by the parent
        // before fork
        event_log_pid = getpid();
        // We don't have a FMT* symbol for pid_t, so we go via Word64
        // to be sure of not losing range. It would be nicer to have a
        // FMT* symbol or similar, though.
        sprintf(event_log_filename, "%s.%" FMT_Word64 ".eventlog", prog, (StgWord64)event_log_pid);
    }
    stgFree(prog);

    /* Open event log file for writing. */
    if ((event_log_file = fopen(event_log_filename, "wb")) == NULL) {
        sysErrorBelch("initEventLogging: can't open %s", event_log_filename);
        stg_exit(EXIT_FAILURE);    
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
#ifdef THREADED_RTS
    // XXX n_capabilities hasn't been initislised yet
    n_caps = RtsFlags.ParFlags.nNodes;
#else
    n_caps = 1;
#endif
    moreCapEventBufs(0,n_caps);

    initEventsBuf(&eventBuf, EVENT_LOG_SIZE, (EventCapNo)(-1));

    // Write in buffer: the header begin marker.
    postInt32(&eventBuf, EVENT_HEADER_BEGIN);

    // Mark beginning of event types in the header.
    postInt32(&eventBuf, EVENT_HET_BEGIN);
    for (t = 0; t < NUM_GHC_EVENT_TAGS; ++t) {

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
            eventTypes[t].size =
                sizeof(EventThreadID) + sizeof(StgWord16) + sizeof(EventThreadID);
            break;

        case EVENT_STARTUP:         // (cap_count)
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
                                      //  par_max_copied, par_tot_copied)
            eventTypes[t].size = sizeof(EventCapsetID)
                               + sizeof(StgWord16)
                               + sizeof(StgWord64) * 3
                               + sizeof(StgWord32)
                               + sizeof(StgWord64) * 2;
            break;

        case EVENT_TASK_CREATE:   // (taskId, cap, tid)
            eventTypes[t].size =
                sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventKernelThreadId);
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

        default:
            continue; /* ignore deprecated events */
        }

        // Write in buffer: the start event type.
        postEventType(&eventBuf, &eventTypes[t]);
    }

    // Mark end of event types in the header.
    postInt32(&eventBuf, EVENT_HET_END);
    
    // Write in buffer: the header end marker.
    postInt32(&eventBuf, EVENT_HEADER_END);
    
    // Prepare event buffer for events (data).
    postInt32(&eventBuf, EVENT_DATA_BEGIN);

    // Flush capEventBuf with header.
    /*
     * Flush header and data begin marker to the file, thus preparing the
     * file to have events written to it.
     */
    printAndClearEventBuf(&eventBuf);

    for (c = 0; c < n_caps; ++c) {
        postBlockMarker(&capEventBuf[c]);
    }

#ifdef THREADED_RTS
    initMutex(&eventBufMutex);
#endif
}

void
endEventLogging(void)
{
    nat c;

    // Flush all events remaining in the buffers.
    for (c = 0; c < n_capabilities; ++c) {
        printAndClearEventBuf(&capEventBuf[c]);
    }
    printAndClearEventBuf(&eventBuf);
    resetEventsBuf(&eventBuf); // we don't want the block marker

    // Mark end of events (data).
    postEventTypeNum(&eventBuf, EVENT_DATA_END);

    // Flush the end of data marker.
    printAndClearEventBuf(&eventBuf);

    if (event_log_file != NULL) {
        fclose(event_log_file);
    }
}

void
moreCapEventBufs (nat from, nat to)
{
    nat c;

    if (from > 0) {
        capEventBuf = stgReallocBytes(capEventBuf, to * sizeof(EventsBuf),
                                      "moreCapEventBufs");
    } else {
        capEventBuf = stgMallocBytes(to * sizeof(EventsBuf),
                                     "moreCapEventBufs");
    }

    for (c = from; c < to; ++c) {
        initEventsBuf(&capEventBuf[c], EVENT_LOG_SIZE, c);
    }
}


void
freeEventLogging(void)
{
    StgWord8 c;
    
    // Free events buffer.
    for (c = 0; c < n_capabilities; ++c) {
        if (capEventBuf[c].begin != NULL) 
            stgFree(capEventBuf[c].begin);
    }
    if (capEventBuf != NULL)  {
        stgFree(capEventBuf);
    }
    if (event_log_filename != NULL) {
        stgFree(event_log_filename);
    }
}

void 
flushEventLog(void)
{
    if (event_log_file != NULL) {
        fflush(event_log_file);
    }
}

void 
abortEventLogging(void)
{
    freeEventLogging();
    if (event_log_file != NULL) {
        fclose(event_log_file);
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
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
    
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
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

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
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, EVENT_SPARK_COUNTERS)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
    
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

    if (!hasRoomForEvent(&eventBuf, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }
    
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

    if (!hasRoomForEvent(&eventBuf, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

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

    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForVariableEvent(&eventBuf, size)){
        printAndClearEventBuf(&eventBuf);

        if (!hasRoomForVariableEvent(&eventBuf, size)){
            // Event size exceeds buffer size, bail out:
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
    int i, size = sizeof(EventCapsetID);

    for (i = 0; i < argc; i++) {
        // 1 + strlen to account for the trailing \0, used as separator
        size += 1 + strlen(argv[i]);
    }

    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForVariableEvent(&eventBuf, size)){
        printAndClearEventBuf(&eventBuf);

        if(!hasRoomForVariableEvent(&eventBuf, size)){
            // Event size exceeds buffer size, bail out:
            RELEASE_LOCK(&eventBufMutex);
            return;
        }
    }

    postEventHeader(&eventBuf, tag);
    postPayloadSize(&eventBuf, size);
    postCapsetID(&eventBuf, capset);

    for( i = 0; i < argc; i++ ) {
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

    if (!hasRoomForEvent(&eventBuf, EVENT_WALL_CLOCK_TIME)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

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
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
    
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
                        nat           gens,
                        W_          maxHeapSize,
                        W_          allocAreaSize,
                        W_          mblockSize,
                        W_          blockSize)
{
    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForEvent(&eventBuf, EVENT_HEAP_INFO_GHC)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

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
                        nat            gen,
                        W_           copied,
                        W_           slop,
                        W_           fragmentation,
                        nat            par_n_threads,
                        W_           par_max_copied,
                        W_           par_tot_copied)
{
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, EVENT_GC_STATS_GHC)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
    
    postEventHeader(eb, EVENT_GC_STATS_GHC);
    /* EVENT_GC_STATS_GHC (heap_capset, generation,
                           copied_bytes, slop_bytes, frag_bytes,
                           par_n_threads, par_max_copied, par_tot_copied) */
    postCapsetID(eb, heap_capset);
    postWord16(eb, gen);
    postWord64(eb, copied);
    postWord64(eb, slop);
    postWord64(eb, fragmentation);
    postWord32(eb, par_n_threads);
    postWord64(eb, par_max_copied);
    postWord64(eb, par_tot_copied);
}

void postTaskCreateEvent (EventTaskId taskId,
                          EventCapNo capno,
                          EventKernelThreadId tid)
{
    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForEvent(&eventBuf, EVENT_TASK_CREATE)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

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

    if (!hasRoomForEvent(&eventBuf, EVENT_TASK_MIGRATE)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

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

    if (!hasRoomForEvent(&eventBuf, EVENT_TASK_DELETE)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

    postEventHeader(&eventBuf, EVENT_TASK_DELETE);
    /* EVENT_TASK_DELETE (taskID) */
    postTaskId(&eventBuf, taskId);

    RELEASE_LOCK(&eventBufMutex);
}

void
postEvent (Capability *cap, EventTypeNum tag)
{
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

    postEventHeader(eb, tag);
}

void
postEventAtTimestamp (Capability *cap, EventTimestamp ts, EventTypeNum tag)
{
    EventsBuf *eb;

    eb = &capEventBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

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
    nat size;

    size = vsnprintf(buf,BUF,msg,ap);
    if (size > BUF) {
        buf[BUF-1] = '\0';
        size = BUF;
    }

    if (!hasRoomForVariableEvent(eb, size)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }

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

void postUserMsg(Capability *cap, char *msg, va_list ap)
{
    postLogMsg(&capEventBuf[cap->no], EVENT_USER_MSG, msg, ap);
}    

void postEventStartup(EventCapNo n_caps)
{
    ACQUIRE_LOCK(&eventBufMutex);

    if (!hasRoomForEvent(&eventBuf, EVENT_STARTUP)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(&eventBuf);
    }

    // Post a STARTUP event with the number of capabilities
    postEventHeader(&eventBuf, EVENT_STARTUP);
    postCapNo(&eventBuf, n_caps);

    RELEASE_LOCK(&eventBufMutex);
}

void postUserMarker(Capability *cap, char *markername)
{
    EventsBuf *eb;
    int size = strlen(markername);

    eb = &capEventBuf[cap->no];

    if (!hasRoomForVariableEvent(eb, size)){
        printAndClearEventBuf(eb);

        if (!hasRoomForVariableEvent(eb, size)){
            // Event size exceeds buffer size, bail out:
            return;
        }
    }

    postEventHeader(eb, EVENT_USER_MARKER);
    postPayloadSize(eb, size);
    postBuf(eb, (StgWord8*) markername, size);
}

void postThreadLabel(Capability    *cap,
                     EventThreadID  id,
                     char          *label)
{
    EventsBuf *eb;
    int strsize = strlen(label);
    int size = strsize + sizeof(EventCapsetID);

    eb = &capEventBuf[cap->no];

    if (!hasRoomForVariableEvent(eb, size)){
        printAndClearEventBuf(eb);

        if (!hasRoomForVariableEvent(eb, size)){
            // Event size exceeds buffer size, bail out:
            return;
        }
    }

    postEventHeader(eb, EVENT_THREAD_LABEL);
    postPayloadSize(eb, size);
    postThreadID(eb, id);
    postBuf(eb, (StgWord8*) label, strsize);
}

void closeBlockMarker (EventsBuf *ebuf)
{
    StgInt8* save_pos;

    if (ebuf->marker)
    {
        // (type:16, time:64, size:32, end_time:64)

        save_pos = ebuf->pos;
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
    if (!hasRoomForEvent(eb, EVENT_BLOCK_MARKER)) {
        printAndClearEventBuf(eb);
    }

    closeBlockMarker(eb);

    eb->marker = eb->pos;
    postEventHeader(eb, EVENT_BLOCK_MARKER);
    postWord32(eb,0); // these get filled in later by closeBlockMarker();
    postWord64(eb,0);
    postCapNo(eb, eb->capno);
}

void printAndClearEventBuf (EventsBuf *ebuf)
{
    StgWord64 numBytes = 0, written = 0;

    closeBlockMarker(ebuf);

    if (ebuf->begin != NULL && ebuf->pos != ebuf->begin)
    {
        numBytes = ebuf->pos - ebuf->begin;
        
        written = fwrite(ebuf->begin, 1, numBytes, event_log_file);
        if (written != numBytes) {
            debugBelch(
                "printAndClearEventLog: fwrite() failed, written=%" FMT_Word64
                " doesn't match numBytes=%" FMT_Word64, written, numBytes);
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
  nat size;

  size = sizeof(EventTypeNum) + sizeof(EventTimestamp) + eventTypes[eNum].size;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

StgBool hasRoomForVariableEvent(EventsBuf *eb, nat payload_bytes)
{
  nat size;

  size = sizeof(EventTypeNum) + sizeof(EventTimestamp) +
      sizeof(EventPayloadSize) + payload_bytes;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}    

void postEventType(EventsBuf *eb, EventType *et)
{
    StgWord8 d;
    nat desclen;

    postInt32(eb, EVENT_ET_BEGIN);
    postEventTypeNum(eb, et->etNum);
    postWord16(eb, (StgWord16)et->size);
    desclen = strlen(et->desc);
    postWord32(eb, desclen);
    for (d = 0; d < desclen; ++d) {
        postInt8(eb, (StgInt8)et->desc[d]);
    }
    postWord32(eb, 0); // no extensions yet
    postInt32(eb, EVENT_ET_END);
}

#endif /* TRACING */
