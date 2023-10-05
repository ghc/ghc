/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Event log format
 *
 * The log format is designed to be extensible: old tools should be
 * able to parse (but not necessarily understand all of) new versions
 * of the format, and new tools will be able to understand old log
 * files.
 *
 * The canonical documentation for the event log format and record layouts is
 * the "Eventlog encodings" section of the GHC User's Guide.
 *
 * To add a new event
 * ------------------
 *
 *  - In this file:
 *    - give it a new number, add a new #define EVENT_XXX
 *      below. Do not reuse event ids from deprecated event types.
 *
 *  - In EventLog.c
 *    - add it to the EventDesc array
 *    - emit the event type in initEventLogging()
 *    - emit the new event in postEvent_()
 *    - generate the event itself by calling postEvent() somewhere
 *
 *  - Describe the meaning and encoding of the event in the users guide
 *    (docs/user_guide/eventlog-formats.rst)
 *
 *  - In the Haskell code to parse the event log file:
 *    - add types and code to read the new event
 *
 * -------------------------------------------------------------------------- */

#pragma once

/*
 * Markers for begin/end of the Header.
 */
#define EVENT_HEADER_BEGIN    0x68647262 /* 'h' 'd' 'r' 'b' */
#define EVENT_HEADER_END      0x68647265 /* 'h' 'd' 'r' 'e' */

#define EVENT_DATA_BEGIN      0x64617462 /* 'd' 'a' 't' 'b' */
#define EVENT_DATA_END        0xffff

/*
 * Markers for begin/end of the list of Event Types in the Header.
 * Header, Event Type, Begin = hetb
 * Header, Event Type, End = hete
 */
#define EVENT_HET_BEGIN       0x68657462 /* 'h' 'e' 't' 'b' */
#define EVENT_HET_END         0x68657465 /* 'h' 'e' 't' 'e' */

#define EVENT_ET_BEGIN        0x65746200 /* 'e' 't' 'b' 0 */
#define EVENT_ET_END          0x65746500 /* 'e' 't' 'e' 0 */

/*
 * Types of events
 *
 * These are defined by gen_event_types.py.
 */
#include "rts/EventLogConstants.h"

/*
 * The highest event code +1 that ghc itself emits. Note that some event
 * ranges higher than this are reserved but not currently emitted by ghc.
 */
#define NUM_GHC_EVENT_TAGS        213

#if 0  /* DEPRECATED EVENTS: */
/* we don't actually need to record the thread, it's implicit */
#define EVENT_RUN_SPARK            5 /* (thread)               */
#define EVENT_STEAL_SPARK          6 /* (thread, victim_cap)   */
/* shutdown replaced by EVENT_CAP_DELETE */
#define EVENT_SHUTDOWN             7 /* ()                     */
/* ghc changed how it handles sparks so these are no longer applicable */
#define EVENT_CREATE_SPARK        13 /* (cap, thread) */
#define EVENT_SPARK_TO_THREAD     14 /* (cap, thread, spark_thread) */
#define EVENT_STARTUP             17 /* (num_capabilities)     */
/* these are used by eden but are replaced by new alternatives for ghc */
#define EVENT_VERSION             23 /* (version_string) */
#define EVENT_PROGRAM_INVOCATION  24 /* (commandline_string) */
#endif

/*
 * Status values for EVENT_STOP_THREAD
 *
 * 1-5 are the StgRun return values (from rts/include/Constants.h):
 *
 * #define HeapOverflow   1
 * #define StackOverflow  2
 * #define ThreadYielding 3
 * #define ThreadBlocked  4
 * #define ThreadFinished 5
 * #define ForeignCall                  6
 * #define BlockedOnMVar                7
 * #define BlockedOnBlackHole           8
 * #define BlockedOnRead                9
 * #define BlockedOnWrite               10
 * #define BlockedOnDelay               11
 * #define BlockedOnSTM                 12
 * #define BlockedOnDoProc              13
 * #define BlockedOnCCall               -- not used (see ForeignCall)
 * #define BlockedOnCCall_NoUnblockExc  -- not used (see ForeignCall)
 * #define BlockedOnMsgThrowTo          16
 */
#define THREAD_SUSPENDED_FOREIGN_CALL 6

/*
 * Capset type values for EVENT_CAPSET_CREATE
 */
#define CAPSET_TYPE_CUSTOM      1  /* reserved for end-user applications */
#define CAPSET_TYPE_OSPROCESS   2  /* caps belong to the same OS process */
#define CAPSET_TYPE_CLOCKDOMAIN 3  /* caps share a local clock/time      */

/*
 * Heap profile breakdown types. See EVENT_HEAP_PROF_BEGIN.
 */
typedef enum {
    HEAP_PROF_BREAKDOWN_COST_CENTRE = 0x1,
    HEAP_PROF_BREAKDOWN_MODULE,
    HEAP_PROF_BREAKDOWN_CLOSURE_DESCR,
    HEAP_PROF_BREAKDOWN_TYPE_DESCR,
    HEAP_PROF_BREAKDOWN_RETAINER,
    HEAP_PROF_BREAKDOWN_BIOGRAPHY,
    HEAP_PROF_BREAKDOWN_CLOSURE_TYPE,
    HEAP_PROF_BREAKDOWN_INFO_TABLE
} HeapProfBreakdown;

#if !defined(EVENTLOG_CONSTANTS_ONLY)

typedef StgWord16 EventTypeNum;
typedef StgWord64 EventTimestamp; /* in nanoseconds */
typedef StgWord32 EventThreadID;
typedef StgWord16 EventCapNo;
typedef StgWord16 EventPayloadSize; /* variable-size events */
typedef StgWord16 EventThreadStatus; /* status for EVENT_STOP_THREAD */
typedef StgWord32 EventCapsetID;
typedef StgWord16 EventCapsetType;   /* types for EVENT_CAPSET_CREATE */
typedef StgWord64 EventTaskId;         /* for EVENT_TASK_* */
typedef StgWord64 EventKernelThreadId; /* for EVENT_TASK_CREATE */

#define EVENT_PAYLOAD_SIZE_MAX STG_WORD16_MAX
#endif
