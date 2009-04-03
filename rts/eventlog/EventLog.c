/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#ifdef EVENTLOG

#include "Rts.h"
#include "EventLog.h"
#include "Capability.h"
#include "Trace.h"
#include "RtsUtils.h"
#include "Stats.h"
#include <string.h> 
#include <stdio.h>

static char *event_log_filename = NULL;

// File for logging events
FILE *event_log_file = NULL;

#define EVENT_LOG_SIZE 2 * (1024 * 1024) // 2MB

static int flushCount;

// Struct for record keeping of buffer to store event types and events.
typedef struct _EventsBuf {
  StgInt8 *begin;
  StgInt8 *pos;
  StgWord64 size;
} EventsBuf;

EventsBuf *eventsBuf;

char *EventDesc[] = {
  "Create thread",
  "Run thread",
  "Stop thread",
  "Thread runnable",
  "Migrate thread",
  "Run spark",
  "Steal spark",
  "Shutdown",
  "Wakeup thread",
  "Starting GC",
  "Finished GC",
  "Request sequential GC",
  "Request parallel GC",
  "Create spark"
};

// Event type. 

typedef struct _EventType {
  EventTypeNum etNum;  // Event Type number.
  nat   size;     // size of the payload in bytes
  char *desc;     // Description
} EventType;

EventType eventTypes[NUM_EVENT_TAGS];

static void printAndClearEventBuf (EventsBuf *eventsBuf);
static void initEventsBuf(EventsBuf* eb, StgWord64 size);
static void resetEventsBuf(EventsBuf* eb);

static void beginHeader(EventsBuf *eb);
static void endHeader(EventsBuf *eb);

static void beginData(EventsBuf *eb);
static void endData(EventsBuf *eb);

static void beginEventTypes(EventsBuf *eb);
static void endEventTypes(EventsBuf *eb);

static void postEventType(EventsBuf *eb, EventType *et);

static StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum);

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

static inline void postEventTypeNum(EventsBuf *eb, EventTypeNum etNum)
{ postWord16(eb, etNum); }

static inline void postEventTypeID(EventsBuf *eb, StgWord16 etID)
{ postWord16(eb, etID); }

static inline void postTimestamp(EventsBuf *eb, Timestamp t)
{ postWord64(eb,t); }

static inline void postInt8(EventsBuf *eb, StgInt8 i)
{ postWord8(eb, (StgWord8)i); }

static inline void postInt16(EventsBuf *eb, StgInt16 i)
{ postWord16(eb, (StgWord16)i); }

static inline void postInt32(EventsBuf *eb, StgInt32 i)
{ postWord32(eb, (StgWord32)i); }

static inline void postInt64(EventsBuf *eb, StgInt64 i)
{ postWord64(eb, (StgWord64)i); }


void
initEventLogging(void)
{
    StgWord8 t, c;

    debugTrace(DEBUG_eventlog, "intiEventLog: start");

    event_log_filename = stgMallocBytes(strlen(prog_name) + 9, 
                                        "initEventLogging");

    if (sizeof(EventDesc) / sizeof(char*) != NUM_EVENT_TAGS) {
        barf("EventDesc array has the wrong number of elements");
    }
  
    sprintf(event_log_filename, "%s.eventlog", prog_name);
    
    /* Open event log file for writing. */
    if ((event_log_file = fopen(event_log_filename, "wb")) == NULL) {
        sysErrorBelch("initEventLoggin: can't open %s", event_log_filename);
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
    eventsBuf = stgMallocBytes(n_capabilities * sizeof(EventsBuf),"initEventLogging");

    for (c = 0; c < n_capabilities; ++c) {
        // Init buffer for events.
        initEventsBuf(&eventsBuf[c], EVENT_LOG_SIZE);
    }

    // Write in buffer: the header begin marker.
    beginHeader(&eventsBuf[0]);

    // Mark beginning of event types in the header.
    beginEventTypes(&eventsBuf[0]);
    for (t = 0; t < NUM_EVENT_TAGS; ++t) {

        eventTypes[t].etNum = t;
        eventTypes[t].desc = EventDesc[t];

        switch (t) {
        case EVENT_CREATE_THREAD:   // (cap, thread)
        case EVENT_RUN_THREAD:      // (cap, thread)
        case EVENT_THREAD_RUNNABLE: // (cap, thread)
        case EVENT_CREATE_SPARK:    // (cap, thread)
        case EVENT_RUN_SPARK:       // (cap, thread)
            eventTypes[t].size = sizeof(CapabilityNum) + sizeof(ThreadID);
            break;

        case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
        case EVENT_STEAL_SPARK:     // (cap, thread, victim_cap)
        case EVENT_THREAD_WAKEUP:   // (cap, thread, other_cap)
            eventTypes[t].size =
                sizeof(CapabilityNum) + sizeof(ThreadID) +
                sizeof(CapabilityNum);
            break;

        case EVENT_STOP_THREAD:     // (cap, thread, status)
            eventTypes[t].size =
                sizeof(CapabilityNum) + sizeof(ThreadID) + sizeof(StgWord16);
            break;

        case EVENT_SHUTDOWN:        // (cap)
        case EVENT_REQUEST_SEQ_GC:  // (cap)
        case EVENT_REQUEST_PAR_GC:  // (cap)
        case EVENT_GC_START:        // (cap)
        case EVENT_GC_END:          // (cap)
            eventTypes[t].size = sizeof(CapabilityNum);
            break;
        }

        // Write in buffer: the start event type.
        postEventType(&eventsBuf[0], &eventTypes[t]);
    }

    // Mark end of event types in the header.
    endEventTypes(&eventsBuf[0]);
    
    // Write in buffer: the header end marker.
    endHeader(&eventsBuf[0]);
    
    // Prepare event buffer for events (data).
    beginData(&eventsBuf[0]);
    
    // Flush eventsBuf with header.
    /*
     * Flush header and data begin marker to the file, thus preparing the
     * file to have events written to it.
     */
    printAndClearEventBuf(&eventsBuf[0]);

    debugTrace(DEBUG_eventlog, "initEventLog: finish"); 
}

void
endEventLogging(void)
{
    nat c;

    debugTrace(DEBUG_eventlog,"endEventLog: start");
    
    // Flush all events remaining in the buffers.
    for (c = 0; c < n_capabilities; ++c) {
        printAndClearEventBuf(&eventsBuf[c]);
    }

    // Mark end of events (data).
    endData(&eventsBuf[0]);

    // Flush the end of data marker.
    printAndClearEventBuf(&eventsBuf[0]);

    if (event_log_file != NULL) {
        fclose(event_log_file);
    }
    
    debugTrace(DEBUG_eventlog,"endEventLog: finish");
}

void 
freeEventLogging(void)
{
    StgWord8 c;

    debugTrace(DEBUG_eventlog,"freeEventLog: start"); 
    
    // Free events buffer.
    for (c = 0; c < n_capabilities; ++c) {
        if (eventsBuf[c].begin != NULL) 
            stgFree(eventsBuf[c].begin);
    }
    if (eventsBuf != NULL)  {
        stgFree(eventsBuf);
    }
    if (event_log_filename != NULL) {
        stgFree(event_log_filename);
    }
    
    debugTrace(DEBUG_eventlog,"freeEventLog: finish"); 
}

/*
 * Post an event message to the capability's eventlog buffer.
 * If the buffer is full, prints out the buffer and clears it.
 */
void
postEvent_(Capability *cap, EventTypeNum tag, StgThreadID thread, nat other_cap)
{
    EventsBuf *eb;

    debugTrace(DEBUG_eventlog,"postEvent: start");
    
    eb = &eventsBuf[cap->no];

    if (!hasRoomForEvent(eb, tag)) {
        // Flush event buffer to make room for new event.
        printAndClearEventBuf(eb);
    }
    
    postEventTypeNum(eb, tag);
    postTimestamp(eb, stat_getElapsedTime() * (1000000000LL/TICKS_PER_SECOND));
    postWord16(eb, cap->no);

    switch (tag) {
    case EVENT_CREATE_THREAD:   // (cap, thread)
    case EVENT_RUN_THREAD:      // (cap, thread)
    case EVENT_THREAD_RUNNABLE: // (cap, thread)
    case EVENT_CREATE_SPARK:    // (cap, thread)
    case EVENT_RUN_SPARK:       // (cap, thread)
    {
        postWord64(eb,thread);
        break;
    }

    case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
    case EVENT_STEAL_SPARK:     // (cap, thread, victim_cap)
    case EVENT_THREAD_WAKEUP:   // (cap, thread, other_cap)
    {
        postWord64(eb,thread);
        postWord16(eb,other_cap);
        break;
    }

    case EVENT_STOP_THREAD:     // (cap, thread, status)
    {
        postWord64(eb,thread);
        postWord16(eb,other_cap);
        break;
    }

    case EVENT_SHUTDOWN:        // (cap)
    case EVENT_REQUEST_SEQ_GC:  // (cap)
    case EVENT_REQUEST_PAR_GC:  // (cap)
    case EVENT_GC_START:        // (cap)
    case EVENT_GC_END:          // (cap)
    {
        break;
    }

    default:
        barf("postEvent: unknown event tag %d", tag);
    }

    debugTrace(DEBUG_eventlog,"postEvent: finish");
}

static void printAndClearEventBuf (EventsBuf *eventsBuf)
{
    StgWord64 numBytes = 0, written = 0;

    if (eventsBuf->begin != NULL && eventsBuf->pos != eventsBuf->begin)
    {
        numBytes = eventsBuf->pos - eventsBuf->begin;
        
        debugTrace(DEBUG_eventlog, 
                   "printAndEventLog: numbytes %" FMT_Word64
                   " bytes to fwrite()",
                   numBytes);
        
        written = fwrite(eventsBuf->begin, 1, numBytes, event_log_file);
        if (written != numBytes) {
            debugBelch(
                "printAndClearEventLog: fwrite() failed, written=%" FMT_Word64
                " doesn't match numBytes=%" FMT_Word64, written, numBytes);
            return;
        }
        
        debugTrace(DEBUG_eventlog,
                   "printAndClearEventLog: fwrite(): %" FMT_Word64 
                   " bytes written", written);
        
        resetEventsBuf(eventsBuf);
        flushCount++;
    }
}

void
printAndClearEventLog(Capability *cap)
{
    debugTrace(DEBUG_eventlog,"printAndClearEventLog: start");
    
    printAndClearEventBuf(&eventsBuf[cap->no]);
    
    debugTrace(DEBUG_eventlog,"printAndClearEventLog: finish");
}

/* -----------------------------------------------------------------------------
   Actual event generation below here
   -------------------------------------------------------------------------- */

void initEventsBuf(EventsBuf* eb, StgWord64 size)
{
    eb->begin = eb->pos = malloc(size);
    eb->size = size;
}

void resetEventsBuf(EventsBuf* eb)
{
    eb->pos = eb->begin;
}

// N.B.: Assuming buffer contains enough space for the header begin marker.
void beginHeader(EventsBuf *eb)
{
    postInt32(eb, EVENT_HEADER_BEGIN);
}

// N.B.: Assuming buffer contains enough space for the header end marker.
void endHeader(EventsBuf *eb)
{
    postInt32(eb, EVENT_HEADER_END);
}

void beginData(EventsBuf *eb)
{
    postInt32(eb, EVENT_DATA_BEGIN);
}

void endData(EventsBuf *eb)
{
    postEventTypeNum(eb, EVENT_DATA_END);
}

void beginEventTypes(EventsBuf *eb)
{
    postInt32(eb, EVENT_HET_BEGIN);
}

void endEventTypes(EventsBuf *eb)
{
    postInt32(eb, EVENT_HET_END);
}

StgBool hasRoomForEvent(EventsBuf *eb, EventTypeNum eNum)
{
  nat size = 0;

  size += sizeof(EventTypeNum) + sizeof(Timestamp) + eventTypes[eNum].size;

  if (eb->pos + size > eb->begin + eb->size) {
      return 0; // Not enough space.
  } else  {
      return 1; // Buf has enough space for the event.
  }
}

static void postEventType(EventsBuf *eb, EventType *et)
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

#endif /* EVENTLOG */
