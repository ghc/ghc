/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging and user-space dtrace probes.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TRACE_H
#define TRACE_H

#include "rts/EventLogFormat.h"
#include "Capability.h"

#if defined(DTRACE)
#include "RtsProbes.h"
#endif /* defined(DTRACE) */

#include "BeginPrivate.h"

// -----------------------------------------------------------------------------
// EventLog API
// -----------------------------------------------------------------------------

#if defined(TRACING)

void initTracing (void);
void endTracing  (void);
void freeTracing (void);
void resetTracing (void);
void tracingAddCapapilities (nat from, nat to);

#endif /* TRACING */

typedef StgWord32 CapsetID;
typedef StgWord16 CapsetType;
enum CapsetType { CapsetTypeCustom = CAPSET_TYPE_CUSTOM,
                  CapsetTypeOsProcess = CAPSET_TYPE_OSPROCESS,
                  CapsetTypeClockdomain = CAPSET_TYPE_CLOCKDOMAIN };
#define CAPSET_OSPROCESS_DEFAULT   ((CapsetID)0)
#define CAPSET_HEAP_DEFAULT        ((CapsetID)0)   /* reusing the same capset */
#define CAPSET_CLOCKDOMAIN_DEFAULT ((CapsetID)1)

// -----------------------------------------------------------------------------
// Message classes
// -----------------------------------------------------------------------------

// debugging flags, set with +RTS -D<something>
extern int DEBUG_sched;
extern int DEBUG_interp;
extern int DEBUG_weak;
extern int DEBUG_gccafs;
extern int DEBUG_gc;
extern int DEBUG_block_alloc;
extern int DEBUG_sanity;
extern int DEBUG_stable;
extern int DEBUG_stm;
extern int DEBUG_prof;
extern int DEBUG_gran;
extern int DEBUG_par;
extern int DEBUG_linker;
extern int DEBUG_squeeze;
extern int DEBUG_hpc;
extern int DEBUG_sparks;

// events
extern int TRACE_sched;
extern int TRACE_gc;
extern int TRACE_spark_sampled;
extern int TRACE_spark_full;
/* extern int TRACE_user; */  // only used in Trace.c

// -----------------------------------------------------------------------------
// Posting events
//
// We use macros rather than inline functions deliberately.  We want
// the not-taken case to be as efficient as possible, a simple
// test-and-jump, and with inline functions gcc seemed to move some of
// the instructions from the branch up before the test.
// 
// -----------------------------------------------------------------------------

#ifdef DEBUG
void traceBegin (const char *str, ...);
void traceEnd (void);
#endif

#ifdef TRACING

/* 
 * Record a scheduler event
 */
#define traceSchedEvent(cap, tag, tso, other)   \
    if (RTS_UNLIKELY(TRACE_sched)) {            \
        traceSchedEvent_(cap, tag, tso, other, 0); \
    }

#define traceSchedEvent2(cap, tag, tso, info1, info2) \
    if (RTS_UNLIKELY(TRACE_sched)) {            \
        traceSchedEvent_(cap, tag, tso, info1, info2); \
    }

void traceSchedEvent_ (Capability *cap, EventTypeNum tag, 
                       StgTSO *tso, StgWord info1, StgWord info2);

/* 
 * Record a GC event
 */
#define traceGcEvent(cap, tag)    \
    if (RTS_UNLIKELY(TRACE_gc)) { \
        traceGcEvent_(cap, tag);  \
    }

void traceGcEvent_ (Capability *cap, EventTypeNum tag);

/* 
 * Record a GC event at the explicitly given timestamp
 */
#define traceGcEventAtT(cap, ts, tag)   \
    if (RTS_UNLIKELY(TRACE_gc)) {       \
        traceGcEventAtT_(cap, ts, tag); \
    }

void traceGcEventAtT_ (Capability *cap, StgWord64 ts, EventTypeNum tag);

/* 
 * Record a heap event
 */
#define traceHeapEvent(cap, tag, heap_capset, info1) \
    if (RTS_UNLIKELY(TRACE_gc)) { \
        traceHeapEvent_(cap, tag, heap_capset, info1);  \
    }
void traceHeapEvent_ (Capability   *cap,
                      EventTypeNum  tag,
                      CapsetID      heap_capset,
                      W_          info1);

void traceEventHeapInfo_ (CapsetID    heap_capset,
                          nat         gens,
                          W_        maxHeapSize,
                          W_        allocAreaSize,
                          W_        mblockSize,
                          W_        blockSize);

void traceEventGcStats_  (Capability *cap,
                          CapsetID    heap_capset,
                          nat         gen,
                          W_        copied,
                          W_        slop,
                          W_        fragmentation,
                          nat         par_n_threads,
                          W_        par_max_copied,
                          W_        par_tot_copied);

/* 
 * Record a spark event
 */
#define traceSparkEvent(cap, tag)         \
    if (RTS_UNLIKELY(TRACE_spark_full)) { \
        traceSparkEvent_(cap, tag, 0);    \
    }

#define traceSparkEvent2(cap, tag, other)  \
    if (RTS_UNLIKELY(TRACE_spark_full)) {  \
        traceSparkEvent_(cap, tag, other); \
    }

void traceSparkEvent_ (Capability *cap, EventTypeNum tag, StgWord info1);

// variadic macros are C99, and supported by gcc.  However, the
// ##__VA_ARGS syntax is a gcc extension, which allows the variable
// argument list to be empty (see gcc docs for details).

/* 
 * Emit a trace message on a particular Capability
 */
#define traceCap(class, cap, msg, ...)          \
    if (RTS_UNLIKELY(class)) {                  \
        traceCap_(cap, msg, ##__VA_ARGS__);     \
    }

void traceCap_(Capability *cap, char *msg, ...);

/* 
 * Emit a trace message
 */
#define trace(class, msg, ...)                  \
    if (RTS_UNLIKELY(class)) {                  \
        trace_(msg, ##__VA_ARGS__);             \
    }

void trace_(char *msg, ...);

/* 
 * A message or event emitted by the program
 * Used by Debug.Trace.{traceEvent, traceEventIO}
 */
void traceUserMsg(Capability *cap, char *msg);

/* 
 * A marker event emitted by the program
 * Used by Debug.Trace.{traceMarker, traceMarkerIO}
 */
void traceUserMarker(Capability *cap, char *msg);

/*
 * An event to record a Haskell thread's label/name
 * Used by GHC.Conc.labelThread
 */
void traceThreadLabel_(Capability *cap,
                       StgTSO     *tso,
                       char       *label);

/* 
 * Emit a debug message (only when DEBUG is defined)
 */
#ifdef DEBUG
#define debugTrace(class, msg, ...)             \
    if (RTS_UNLIKELY(class)) {                  \
        trace_(msg, ##__VA_ARGS__);             \
    }
#else
#define debugTrace(class, str, ...) /* nothing */
#endif

#ifdef DEBUG
#define debugTraceCap(class, cap, msg, ...)      \
    if (RTS_UNLIKELY(class)) {                  \
        traceCap_(cap, msg, ##__VA_ARGS__);     \
    }
#else
#define debugTraceCap(class, cap, str, ...) /* nothing */
#endif

/* 
 * Emit a message/event describing the state of a thread
 */
#define traceThreadStatus(class, tso)           \
    if (RTS_UNLIKELY(class)) {                  \
        traceThreadStatus_(tso);                \
    }

void traceThreadStatus_ (StgTSO *tso);

void traceEventStartup_ (int n_caps);

/*
 * Events for describing capabilities and capability sets in the eventlog
 *
 * Note: unlike other events, these are not conditional on TRACE_sched or
 * similar because capabilities and capability sets are important
 * context for other events. Since other events depend on these events
 * then for simplicity we always emit them, rather than working out if
 * they're necessary . They should be very low volume.
 */
void traceCapEvent (Capability   *cap,
                    EventTypeNum  tag);

void traceCapsetEvent (EventTypeNum tag,
                       CapsetID     capset,
                       StgWord      info);

void traceWallClockTime_(void);

void traceOSProcessInfo_ (void);

void traceSparkCounters_ (Capability *cap,
                          SparkCounters counters,
                          StgWord remaining);

void traceTaskCreate_ (Task       *task,
                       Capability *cap);

void traceTaskMigrate_ (Task       *task,
                        Capability *cap,
                        Capability *new_cap);

void traceTaskDelete_ (Task       *task);

#else /* !TRACING */

#define traceSchedEvent(cap, tag, tso, other) /* nothing */
#define traceSchedEvent2(cap, tag, tso, other, info) /* nothing */
#define traceGcEvent(cap, tag) /* nothing */
#define traceGcEventAtT(cap, ts, tag) /* nothing */
#define traceEventGcStats_(cap, heap_capset, gen, \
                           copied, slop, fragmentation, \
                           par_n_threads, par_max_copied, par_tot_copied) /* nothing */
#define traceHeapEvent(cap, tag, heap_capset, info1) /* nothing */
#define traceEventHeapInfo_(heap_capset, gens, \
                            maxHeapSize, allocAreaSize, \
                            mblockSize, blockSize) /* nothing */
#define traceSparkEvent(cap, tag) /* nothing */
#define traceSparkEvent2(cap, tag, other) /* nothing */
#define traceCap(class, cap, msg, ...) /* nothing */
#define trace(class, msg, ...) /* nothing */
#define debugTrace(class, str, ...) /* nothing */
#define debugTraceCap(class, cap, str, ...) /* nothing */
#define traceThreadStatus(class, tso) /* nothing */
#define traceThreadLabel_(cap, tso, label) /* nothing */
INLINE_HEADER void traceEventStartup_ (int n_caps STG_UNUSED) {};
#define traceCapEvent(cap, tag) /* nothing */
#define traceCapsetEvent(tag, capset, info) /* nothing */
#define traceWallClockTime_() /* nothing */
#define traceOSProcessInfo_() /* nothing */
#define traceSparkCounters_(cap, counters, remaining) /* nothing */
#define traceTaskCreate_(taskID, cap) /* nothing */
#define traceTaskMigrate_(taskID, cap, new_cap) /* nothing */
#define traceTaskDelete_(taskID) /* nothing */

#endif /* TRACING */

// If DTRACE is enabled, but neither DEBUG nor TRACING, we need a C land
// wrapper for the user-msg probe (as we can't expand that in PrimOps.cmm)
//
#if !defined(DEBUG) && !defined(TRACING) && defined(DTRACE)

void dtraceUserMsgWrapper(Capability *cap, char *msg);
void dtraceUserMarkerWrapper(Capability *cap, char *msg);

#endif /* !defined(DEBUG) && !defined(TRACING) && defined(DTRACE) */

// -----------------------------------------------------------------------------
// Aliases for static dtrace probes if dtrace is available
// -----------------------------------------------------------------------------

#if defined(DTRACE)

#define dtraceCreateThread(cap, tid)                    \
    HASKELLEVENT_CREATE_THREAD(cap, tid)
#define dtraceRunThread(cap, tid)                       \
    HASKELLEVENT_RUN_THREAD(cap, tid)
#define dtraceStopThread(cap, tid, status, info)        \
    HASKELLEVENT_STOP_THREAD(cap, tid, status, info)
#define dtraceThreadRunnable(cap, tid)                  \
    HASKELLEVENT_THREAD_RUNNABLE(cap, tid)
#define dtraceMigrateThread(cap, tid, new_cap)          \
    HASKELLEVENT_MIGRATE_THREAD(cap, tid, new_cap)
#define dtraceThreadWakeup(cap, tid, other_cap)         \
    HASKELLEVENT_THREAD_WAKEUP(cap, tid, other_cap)
#define dtraceGcStart(cap)                              \
    HASKELLEVENT_GC_START(cap)
#define dtraceGcEnd(cap)                                \
    HASKELLEVENT_GC_END(cap)
#define dtraceRequestSeqGc(cap)                         \
    HASKELLEVENT_REQUEST_SEQ_GC(cap)
#define dtraceRequestParGc(cap)                         \
    HASKELLEVENT_REQUEST_PAR_GC(cap)
#define dtraceCreateSparkThread(cap, spark_tid)         \
    HASKELLEVENT_CREATE_SPARK_THREAD(cap, spark_tid)
#define dtraceThreadLabel(cap, tso, label)              \
    HASKELLEVENT_THREAD_LABEL(cap, tso, label)
INLINE_HEADER void dtraceStartup (int num_caps) {
    HASKELLEVENT_STARTUP(num_caps);
}
#define dtraceCapCreate(cap)                            \
    HASKELLEVENT_CAP_CREATE(cap)
#define dtraceCapDelete(cap)                            \
    HASKELLEVENT_CAP_DELETE(cap)
#define dtraceCapEnable(cap)                            \
    HASKELLEVENT_CAP_ENABLE(cap)
#define dtraceCapDisable(cap)                           \
    HASKELLEVENT_CAP_DISABLE(cap)
#define dtraceUserMsg(cap, msg)                         \
    HASKELLEVENT_USER_MSG(cap, msg)
#define dtraceUserMarker(cap, msg)                      \
    HASKELLEVENT_USER_MARKER(cap, msg)
#define dtraceGcIdle(cap)                               \
    HASKELLEVENT_GC_IDLE(cap)
#define dtraceGcWork(cap)                               \
    HASKELLEVENT_GC_WORK(cap)
#define dtraceGcDone(cap)                               \
    HASKELLEVENT_GC_DONE(cap)
#define dtraceGcGlobalSync(cap)                         \
    HASKELLEVENT_GC_GLOBAL_SYNC(cap)
#define dtraceEventGcStats(heap_capset, gens,           \
                           copies, slop, fragmentation, \
                           par_n_threads,               \
                           par_max_copied,              \
                           par_tot_copied)              \
    HASKELLEVENT_GC_STATS(heap_capset, gens,            \
                           copies, slop, fragmentation, \
                           par_n_threads,               \
                           par_max_copied,              \
                           par_tot_copied)
#define dtraceHeapInfo(heap_capset, gens,               \
                       maxHeapSize, allocAreaSize,      \
                       mblockSize, blockSize)           \
    HASKELLEVENT_HEAP_INFO(heap_capset, gens,           \
                           maxHeapSize, allocAreaSize,  \
                           mblockSize, blockSize)
#define dtraceEventHeapAllocated(cap, heap_capset,      \
                                 allocated)             \
    HASKELLEVENT_HEAP_ALLOCATED(cap, heap_capset,       \
                                allocated)
#define dtraceEventHeapSize(heap_capset, size)          \
    HASKELLEVENT_HEAP_SIZE(heap_capset, size)
#define dtraceEventHeapLive(heap_capset, live)          \
    HASKELLEVENT_HEAP_LIVE(heap_capset, live)
#define dtraceCapsetCreate(capset, capset_type)         \
    HASKELLEVENT_CAPSET_CREATE(capset, capset_type)
#define dtraceCapsetDelete(capset)                      \
    HASKELLEVENT_CAPSET_DELETE(capset)
#define dtraceCapsetAssignCap(capset, capno)            \
    HASKELLEVENT_CAPSET_ASSIGN_CAP(capset, capno)
#define dtraceCapsetRemoveCap(capset, capno)            \
    HASKELLEVENT_CAPSET_REMOVE_CAP(capset, capno)
#define dtraceSparkCounters(cap, a, b, c, d, e, f, g) \
    HASKELLEVENT_SPARK_COUNTERS(cap, a, b, c, d, e, f, g)
#define dtraceSparkCreate(cap)                         \
    HASKELLEVENT_SPARK_CREATE(cap)
#define dtraceSparkDud(cap)                             \
    HASKELLEVENT_SPARK_DUD(cap)
#define dtraceSparkOverflow(cap)                        \
    HASKELLEVENT_SPARK_OVERFLOW(cap)
#define dtraceSparkRun(cap)                             \
    HASKELLEVENT_SPARK_RUN(cap)
#define dtraceSparkSteal(cap, victim_cap)               \
    HASKELLEVENT_SPARK_STEAL(cap, victim_cap)
#define dtraceSparkFizzle(cap)                          \
    HASKELLEVENT_SPARK_FIZZLE(cap)
#define dtraceSparkGc(cap)                              \
    HASKELLEVENT_SPARK_GC(cap)
#define dtraceTaskCreate(taskID, cap, tid)              \
    HASKELLEVENT_TASK_CREATE(taskID, cap, tid)
#define dtraceTaskMigrate(taskID, cap, new_cap)         \
    HASKELLEVENT_TASK_MIGRATE(taskID, cap, new_cap)
#define dtraceTaskDelete(taskID)                        \
    HASKELLEVENT_TASK_DELETE(taskID)

#else /* !defined(DTRACE) */

#define dtraceCreateThread(cap, tid)                    /* nothing */
#define dtraceRunThread(cap, tid)                       /* nothing */
#define dtraceStopThread(cap, tid, status, info)        /* nothing */
#define dtraceThreadRunnable(cap, tid)                  /* nothing */
#define dtraceMigrateThread(cap, tid, new_cap)          /* nothing */
#define dtraceThreadWakeup(cap, tid, other_cap)         /* nothing */
#define dtraceGcStart(cap)                              /* nothing */
#define dtraceGcEnd(cap)                                /* nothing */
#define dtraceRequestSeqGc(cap)                         /* nothing */
#define dtraceRequestParGc(cap)                         /* nothing */
#define dtraceCreateSparkThread(cap, spark_tid)         /* nothing */
#define dtraceThreadLabel(cap, tso, label)              /* nothing */
INLINE_HEADER void dtraceStartup (int num_caps STG_UNUSED) {};
#define dtraceUserMsg(cap, msg)                         /* nothing */
#define dtraceUserMarker(cap, msg)                      /* nothing */
#define dtraceGcIdle(cap)                               /* nothing */
#define dtraceGcWork(cap)                               /* nothing */
#define dtraceGcDone(cap)                               /* nothing */
#define dtraceGcGlobalSync(cap)                         /* nothing */
#define dtraceEventGcStats(heap_capset, gens,           \
                           copies, slop, fragmentation, \
                           par_n_threads,               \
                           par_max_copied,              \
                           par_tot_copied)              /* nothing */
#define dtraceHeapInfo(heap_capset, gens,               \
                       maxHeapSize, allocAreaSize,      \
                       mblockSize, blockSize)           /* nothing */
#define dtraceEventHeapAllocated(cap, heap_capset,      \
                                 allocated)             /* nothing */
#define dtraceEventHeapSize(heap_capset, size)          /* nothing */
#define dtraceEventHeapLive(heap_capset, live)          /* nothing */
#define dtraceCapCreate(cap)                            /* nothing */
#define dtraceCapDelete(cap)                            /* nothing */
#define dtraceCapEnable(cap)                            /* nothing */
#define dtraceCapDisable(cap)                           /* nothing */
#define dtraceCapsetCreate(capset, capset_type)         /* nothing */
#define dtraceCapsetDelete(capset)                      /* nothing */
#define dtraceCapsetAssignCap(capset, capno)            /* nothing */
#define dtraceCapsetRemoveCap(capset, capno)            /* nothing */
#define dtraceSparkCounters(cap, a, b, c, d, e, f, g)   /* nothing */
#define dtraceSparkCreate(cap)                          /* nothing */
#define dtraceSparkDud(cap)                             /* nothing */
#define dtraceSparkOverflow(cap)                        /* nothing */
#define dtraceSparkRun(cap)                             /* nothing */
#define dtraceSparkSteal(cap, victim_cap)               /* nothing */
#define dtraceSparkFizzle(cap)                          /* nothing */
#define dtraceSparkGc(cap)                              /* nothing */
#define dtraceTaskCreate(taskID, cap, tid)              /* nothing */
#define dtraceTaskMigrate(taskID, cap, new_cap)         /* nothing */
#define dtraceTaskDelete(taskID)                        /* nothing */

#endif

// -----------------------------------------------------------------------------
// Trace probes dispatching to various tracing frameworks
//
// In order to avoid accumulating multiple calls to tracing calls at trace
// points, we define inline probe functions that contain the various
// invocations.
//
// Dtrace - dtrace probes are unconditionally added as probe activation is
//   handled by the dtrace component of the kernel, and inactive probes are
//   very cheap — usually, one no-op.  Consequently, dtrace can be used with
//   all flavours of the RTS.  In addition, we still support logging events to
//   a file, even in the presence of dtrace.  This is, eg, useful when tracing
//   on a server, but browsing trace information with ThreadScope on a local
//   client.
// 
// -----------------------------------------------------------------------------

INLINE_HEADER void traceEventCreateThread(Capability *cap STG_UNUSED, 
                                          StgTSO     *tso STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_CREATE_THREAD, tso, tso->stackobj->stack_size);
    dtraceCreateThread((EventCapNo)cap->no, (EventThreadID)tso->id);
}

INLINE_HEADER void traceEventRunThread(Capability *cap STG_UNUSED, 
                                       StgTSO     *tso STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_RUN_THREAD, tso, tso->what_next);
    dtraceRunThread((EventCapNo)cap->no, (EventThreadID)tso->id);
}

INLINE_HEADER void traceEventStopThread(Capability          *cap    STG_UNUSED, 
                                        StgTSO              *tso    STG_UNUSED, 
                                        StgThreadReturnCode  status STG_UNUSED,
                                        StgWord32           info    STG_UNUSED)
{
    traceSchedEvent2(cap, EVENT_STOP_THREAD, tso, status, info);
    dtraceStopThread((EventCapNo)cap->no, (EventThreadID)tso->id,
                     (EventThreadStatus)status, (EventThreadID)info);
}

// needs to be EXTERN_INLINE as it is used in another EXTERN_INLINE function
EXTERN_INLINE void traceEventThreadRunnable(Capability *cap STG_UNUSED, 
                                            StgTSO     *tso STG_UNUSED);

EXTERN_INLINE void traceEventThreadRunnable(Capability *cap STG_UNUSED, 
                                            StgTSO     *tso STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_THREAD_RUNNABLE, tso, 0);
    dtraceThreadRunnable((EventCapNo)cap->no, (EventThreadID)tso->id);
}

INLINE_HEADER void traceEventMigrateThread(Capability *cap     STG_UNUSED, 
                                           StgTSO     *tso     STG_UNUSED,
                                           nat         new_cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_MIGRATE_THREAD, tso, new_cap);
    dtraceMigrateThread((EventCapNo)cap->no, (EventThreadID)tso->id,
                        (EventCapNo)new_cap);
}

INLINE_HEADER void traceCapCreate(Capability *cap STG_UNUSED)
{
    traceCapEvent(cap, EVENT_CAP_CREATE);
    dtraceCapCreate((EventCapNo)cap->no);
}

INLINE_HEADER void traceCapDelete(Capability *cap STG_UNUSED)
{
    traceCapEvent(cap, EVENT_CAP_DELETE);
    dtraceCapDelete((EventCapNo)cap->no);
}

INLINE_HEADER void traceCapEnable(Capability *cap STG_UNUSED)
{
    traceCapEvent(cap, EVENT_CAP_ENABLE);
    dtraceCapEnable((EventCapNo)cap->no);
}

INLINE_HEADER void traceCapDisable(Capability *cap STG_UNUSED)
{
    traceCapEvent(cap, EVENT_CAP_DISABLE);
    dtraceCapDisable((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventThreadWakeup(Capability *cap       STG_UNUSED, 
                                          StgTSO     *tso       STG_UNUSED,
                                          nat         other_cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_THREAD_WAKEUP, tso, other_cap);
    dtraceThreadWakeup((EventCapNo)cap->no, (EventThreadID)tso->id,
                       (EventCapNo)other_cap);
}

INLINE_HEADER void traceThreadLabel(Capability *cap   STG_UNUSED,
                                    StgTSO     *tso   STG_UNUSED,
                                    char       *label STG_UNUSED)
{
    if (RTS_UNLIKELY(TRACE_sched)) {
        traceThreadLabel_(cap, tso, label);
    }
    dtraceThreadLabel((EventCapNo)cap->no, (EventThreadID)tso->id, label);
}

INLINE_HEADER void traceEventGcStart(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_START);
    dtraceGcStart((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcStartAtT(Capability *cap STG_UNUSED,
                                        StgWord64   ts  STG_UNUSED)
{
    traceGcEventAtT(cap, ts, EVENT_GC_START);
    dtraceGcStart((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcEnd(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_END);
    dtraceGcEnd((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcEndAtT(Capability *cap STG_UNUSED,
                                      StgWord64   ts  STG_UNUSED)
{
    traceGcEventAtT(cap, ts, EVENT_GC_END);
    dtraceGcEnd((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventRequestSeqGc(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_REQUEST_SEQ_GC);
    dtraceRequestSeqGc((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventRequestParGc(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_REQUEST_PAR_GC);
    dtraceRequestParGc((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcIdle(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_IDLE);
    dtraceGcIdle((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcWork(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_WORK);
    dtraceGcWork((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcDone(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_DONE);
    dtraceGcDone((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcGlobalSync(Capability *cap STG_UNUSED)
{
    traceGcEvent(cap, EVENT_GC_GLOBAL_SYNC);
    dtraceGcGlobalSync((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcStats(Capability *cap            STG_UNUSED,
                                     CapsetID    heap_capset    STG_UNUSED,
                                     nat         gen            STG_UNUSED,
                                     W_        copied         STG_UNUSED,
                                     W_        slop           STG_UNUSED,
                                     W_        fragmentation  STG_UNUSED,
                                     nat         par_n_threads  STG_UNUSED,
                                     W_        par_max_copied STG_UNUSED,
                                     W_        par_tot_copied STG_UNUSED)
{
    if (RTS_UNLIKELY(TRACE_gc)) {
        traceEventGcStats_(cap, heap_capset, gen,
                           copied, slop, fragmentation,
                           par_n_threads, par_max_copied, par_tot_copied);
    }
    dtraceEventGcStats(heap_capset, gen,
                       copied, slop, fragmentation,
                       par_n_threads, par_max_copied, par_tot_copied);
}

INLINE_HEADER void traceEventHeapInfo(CapsetID    heap_capset   STG_UNUSED,
                                      nat         gens          STG_UNUSED,
                                      W_        maxHeapSize   STG_UNUSED,
                                      W_        allocAreaSize STG_UNUSED,
                                      W_        mblockSize    STG_UNUSED,
                                      W_        blockSize     STG_UNUSED)
{
    if (RTS_UNLIKELY(TRACE_gc)) {
        traceEventHeapInfo_(heap_capset, gens,
                            maxHeapSize, allocAreaSize,
                            mblockSize, blockSize);
    }
    dtraceHeapInfo(heap_capset, gens,
                   maxHeapSize, allocAreaSize,
                   mblockSize, blockSize);
}

INLINE_HEADER void traceEventHeapAllocated(Capability *cap         STG_UNUSED,
                                           CapsetID    heap_capset STG_UNUSED,
                                           W_        allocated   STG_UNUSED)
{
    traceHeapEvent(cap, EVENT_HEAP_ALLOCATED, heap_capset, allocated);
    dtraceEventHeapAllocated((EventCapNo)cap->no, heap_capset, allocated);
}

INLINE_HEADER void traceEventHeapSize(Capability *cap         STG_UNUSED,
                                      CapsetID    heap_capset STG_UNUSED,
                                      W_        heap_size   STG_UNUSED)
{
    traceHeapEvent(cap, EVENT_HEAP_SIZE, heap_capset, heap_size);
    dtraceEventHeapSize(heap_capset, heap_size);
}

INLINE_HEADER void traceEventHeapLive(Capability *cap         STG_UNUSED,
                                      CapsetID    heap_capset STG_UNUSED,
                                      W_        heap_live   STG_UNUSED)
{
    traceHeapEvent(cap, EVENT_HEAP_LIVE, heap_capset, heap_live);
    dtraceEventHeapLive(heap_capset, heap_live);
}

/* TODO: at some point we should remove this event, it's covered by
 * the cap create/delete events.
 */
INLINE_HEADER void traceEventStartup(void)
{
    int n_caps;
#ifdef THREADED_RTS
    // XXX n_capabilities hasn't been initialised yet
    n_caps = RtsFlags.ParFlags.nNodes;
#else
    n_caps = 1;
#endif

    traceEventStartup_(n_caps);
    dtraceStartup(n_caps);
}

INLINE_HEADER void traceCapsetCreate(CapsetID   capset      STG_UNUSED,
                                     CapsetType capset_type STG_UNUSED)
{
    traceCapsetEvent(EVENT_CAPSET_CREATE, capset, capset_type);
    dtraceCapsetCreate(capset, capset_type);
}

INLINE_HEADER void traceCapsetDelete(CapsetID capset STG_UNUSED)
{
    traceCapsetEvent(EVENT_CAPSET_DELETE, capset, 0);
    dtraceCapsetDelete(capset);
}

INLINE_HEADER void traceCapsetAssignCap(CapsetID capset STG_UNUSED,
                                        nat      capno  STG_UNUSED)
{
    traceCapsetEvent(EVENT_CAPSET_ASSIGN_CAP, capset, capno);
    dtraceCapsetAssignCap(capset, capno);
}

INLINE_HEADER void traceCapsetRemoveCap(CapsetID capset STG_UNUSED,
                                        nat      capno  STG_UNUSED)
{
    traceCapsetEvent(EVENT_CAPSET_REMOVE_CAP, capset, capno);
    dtraceCapsetRemoveCap(capset, capno);
}

INLINE_HEADER void traceWallClockTime(void)
{
    traceWallClockTime_();
    /* Note: no DTrace equivalent because it is available to DTrace directly */
}

INLINE_HEADER void traceOSProcessInfo(void)
{
    traceOSProcessInfo_();
    /* Note: no DTrace equivalent because all this OS process info
     * is available to DTrace directly */
}

INLINE_HEADER void traceEventCreateSparkThread(Capability  *cap      STG_UNUSED, 
                                               StgThreadID spark_tid STG_UNUSED)
{
    traceSparkEvent2(cap, EVENT_CREATE_SPARK_THREAD, spark_tid);
    dtraceCreateSparkThread((EventCapNo)cap->no, (EventThreadID)spark_tid);
}

INLINE_HEADER void traceSparkCounters(Capability *cap STG_UNUSED)
{
#ifdef THREADED_RTS
    if (RTS_UNLIKELY(TRACE_spark_sampled)) {
        traceSparkCounters_(cap, cap->spark_stats, sparkPoolSize(cap->sparks));
    }
    dtraceSparkCounters((EventCapNo)cap->no,
                        cap->spark_stats.created,
                        cap->spark_stats.dud,
                        cap->spark_stats.overflowed,
                        cap->spark_stats.converted,
                        cap->spark_stats.gcd,
                        cap->spark_stats.fizzled,
                        sparkPoolSize(cap->sparks));
#endif
}

INLINE_HEADER void traceEventSparkCreate(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_CREATE);
    dtraceSparkCreate((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventSparkDud(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_DUD);
    dtraceSparkDud((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventSparkOverflow(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_OVERFLOW);
    dtraceSparkOverflow((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventSparkRun(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_RUN);
    dtraceSparkRun((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventSparkSteal(Capability *cap STG_UNUSED,
                                        nat         victim_cap STG_UNUSED)
{
    traceSparkEvent2(cap, EVENT_SPARK_STEAL, victim_cap);
    dtraceSparkSteal((EventCapNo)cap->no, (EventCapNo)victim_cap);
}

INLINE_HEADER void traceEventSparkFizzle(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_FIZZLE);
    dtraceSparkFizzle((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventSparkGC(Capability *cap STG_UNUSED)
{
    traceSparkEvent(cap, EVENT_SPARK_GC);
    dtraceSparkGc((EventCapNo)cap->no);
}

INLINE_HEADER void traceTaskCreate(Task       *task STG_UNUSED,
                                   Capability *cap  STG_UNUSED)
{
    ASSERT(task->cap == cap);
    // TODO: asserting task->cap == NULL would be much stronger
    // (the intention being that the task structure is just created and empty)
    // but would require large changes of traceTaskCreate calls.
    ASSERT(cap != NULL);
    // A new task gets associated with a cap. We also record
    // the kernel thread id of the task, which should never change.
    if (RTS_UNLIKELY(TRACE_sched)) {
        traceTaskCreate_(task, cap);
    }
    dtraceTaskCreate(serialisableTaskId(task),
                     (EventCapNo)cap->no,
                     kernelThreadId());
}

INLINE_HEADER void traceTaskMigrate(Task       *task    STG_UNUSED,
                                    Capability *cap     STG_UNUSED,
                                    Capability *new_cap STG_UNUSED)
{
    ASSERT(task->cap == cap);
    ASSERT(cap != NULL);
    ASSERT(cap != new_cap);
    ASSERT(new_cap != NULL);
    // A task migrates from a cap to another.
    if (RTS_UNLIKELY(TRACE_sched)) {
        traceTaskMigrate_(task, cap, new_cap);
    }
    dtraceTaskMigrate(serialisableTaskId(task), (EventCapNo)cap->no,
                                                (EventCapNo)new_cap->no);
}

INLINE_HEADER void traceTaskDelete(Task *task STG_UNUSED)
{
    ASSERT(task->cap != NULL);
    if (RTS_UNLIKELY(TRACE_sched)) {
        traceTaskDelete_(task);
    }
    dtraceTaskDelete(serialisableTaskId(task));
}

#include "EndPrivate.h"

#endif /* TRACE_H */
