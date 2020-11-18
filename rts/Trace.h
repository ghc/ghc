/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging and user-space dtrace probes.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "eventlog/EventLog.h"
#include "sm/NonMovingCensus.h"
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
void tracingAddCapapilities (uint32_t from, uint32_t to);

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

// shorthand for RtsFlags.DebugFlags.<blah>, useful with debugTrace()
#define DEBUG_sched       RtsFlags.DebugFlags.scheduler
#define DEBUG_interp      RtsFlags.DebugFlags.interp
#define DEBUG_weak        RtsFlags.DebugFlags.weak
#define DEBUG_gccafs      RtsFlags.DebugFlags.gccafs
#define DEBUG_gc          RtsFlags.DebugFlags.gc
#define DEBUG_nonmoving_gc RtsFlags.DebugFlags.nonmoving_gc
#define DEBUG_block_alloc RtsFlags.DebugFlags.alloc
#define DEBUG_sanity      RtsFlags.DebugFlags.sanity
#define DEBUG_zero_on_gc  RtsFlags.DebugFlags.zero_on_gc
#define DEBUG_stable      RtsFlags.DebugFlags.stable
#define DEBUG_stm         RtsFlags.DebugFlags.stm
#define DEBUG_prof        RtsFlags.DebugFlags.prof
#define DEBUG_gran        RtsFlags.DebugFlags.gran
#define DEBUG_par         RtsFlags.DebugFlags.par
#define DEBUG_linker      RtsFlags.DebugFlags.linker
#define DEBUG_squeeze     RtsFlags.DebugFlags.squeeze
#define DEBUG_hpc         RtsFlags.DebugFlags.hpc
#define DEBUG_sparks      RtsFlags.DebugFlags.sparks
#define DEBUG_compact     RtsFlags.DebugFlags.compact

// Event-enabled flags
// These semantically booleans but we use a dense packing to minimize their
// cache impact.
extern uint8_t TRACE_sched;
extern uint8_t TRACE_gc;
extern uint8_t TRACE_nonmoving_gc;
extern uint8_t TRACE_spark_sampled;
extern uint8_t TRACE_spark_full;
extern uint8_t TRACE_cap;
/* extern uint8_t TRACE_user; */  // only used in Trace.c

// -----------------------------------------------------------------------------
// Posting events
//
// We use macros rather than inline functions deliberately.  We want
// the not-taken case to be as efficient as possible, a simple
// test-and-jump, and with inline functions gcc seemed to move some of
// the instructions from the branch up before the test.
//
// -----------------------------------------------------------------------------

#if defined(DEBUG)
void traceBegin (const char *str, ...);
void traceEnd (void);
#endif

#if defined(TRACING)

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

#define traceInitEvent(event) postInitEvent(event)

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
                          uint32_t  gens,
                          W_        maxHeapSize,
                          W_        allocAreaSize,
                          W_        mblockSize,
                          W_        blockSize);

void traceEventGcStats_  (Capability *cap,
                          CapsetID    heap_capset,
                          uint32_t  gen,
                          W_        copied,
                          W_        slop,
                          W_        fragmentation,
                          uint32_t  par_n_threads,
                          W_        par_max_copied,
                          W_        par_tot_copied,
                          W_        par_balanced_copied);

void traceEventMemReturn_  (Capability *cap,
                          uint32_t    current_mblocks,
                          uint32_t    needed_mblocks,
                          uint32_t    returned_mblocks );

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
 * A binary message or event emitted by the program
 */
void traceUserBinaryMsg(Capability *cap, uint8_t *msg, size_t size);

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
#if defined(DEBUG)
#define debugTrace(class, msg, ...)             \
    if (RTS_UNLIKELY(class)) {                  \
        trace_(msg, ##__VA_ARGS__);             \
    }
#else
#define debugTrace(class, str, ...) /* nothing */
#endif

#if defined(DEBUG)
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

/*
 * Events for describing capabilities and capability sets in the eventlog
 */
#define traceCapEvent(cap, tag)                 \
    if (RTS_UNLIKELY(TRACE_cap)) {              \
        traceCapEvent_(cap, tag);               \
    }

void traceCapEvent_ (Capability   *cap,
                    EventTypeNum  tag);

#define traceCapsetEvent(cap, capset, info)     \
    if (RTS_UNLIKELY(TRACE_cap)) {              \
        traceCapsetEvent_(cap, capset, info);   \
    }

void traceCapsetEvent_ (EventTypeNum tag,
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

void traceHeapProfBegin(StgWord8 profile_id);
void traceHeapProfSampleBegin(StgInt era);
void traceHeapBioProfSampleBegin(StgInt era, StgWord64 time);
void traceHeapProfSampleEnd(StgInt era);
void traceHeapProfSampleString(StgWord8 profile_id,
                               const char *label, StgWord residency);
#if defined(PROFILING)
void traceHeapProfCostCentre(StgWord32 ccID,
                             const char *label,
                             const char *module,
                             const char *srcloc,
                             StgBool is_caf);
void traceHeapProfSampleCostCentre(StgWord8 profile_id,
                                   CostCentreStack *stack, StgWord residency);

void traceProfSampleCostCentre(Capability *cap,
                               CostCentreStack *stack, StgWord ticks);
void traceProfBegin(void);
#endif /* PROFILING */

void traceConcMarkBegin(void);
void traceConcMarkEnd(StgWord32 marked_obj_count);
void traceConcSyncBegin(void);
void traceConcSyncEnd(void);
void traceConcSweepBegin(void);
void traceConcSweepEnd(void);
void traceConcUpdRemSetFlush(Capability *cap);
void traceNonmovingHeapCensus(uint32_t log_blk_size,
                              const struct NonmovingAllocCensus *census);

void traceIPE(StgInfoTable *info,
               const char *table_name,
               const char *closure_desc,
               const char *ty_desc,
               const char *label,
               const char *module,
               const char *srcloc );
void flushTrace(void);

#else /* !TRACING */

#define traceSchedEvent(cap, tag, tso, other) /* nothing */
#define traceInitEvent(event) /* nothing */
#define traceSchedEvent2(cap, tag, tso, other, info) /* nothing */
#define traceGcEvent(cap, tag) /* nothing */
#define traceGcEventAtT(cap, ts, tag) /* nothing */
#define traceEventGcStats_(cap, heap_capset, gen, \
                           copied, slop, fragmentation, \
                           par_n_threads, par_max_copied, \
                           par_tot_copied, par_balanced_copied) /* nothing */
#define traceEventMemReturn_(cap, current, needed, returned) /* nothing */
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
#define traceCapEvent(cap, tag) /* nothing */
#define traceCapsetEvent(tag, capset, info) /* nothing */
#define traceWallClockTime_() /* nothing */
#define traceOSProcessInfo_()  /* nothing */
#define traceSparkCounters_(cap, counters, remaining) /* nothing */
#define traceTaskCreate_(taskID, cap) /* nothing */
#define traceTaskMigrate_(taskID, cap, new_cap) /* nothing */
#define traceTaskDelete_(taskID) /* nothing */
#define traceHeapProfBegin(profile_id) /* nothing */
#define traceHeapProfCostCentre(ccID, label, module, srcloc, is_caf) /* nothing */
#define traceIPE(info, table_name, closure_desc, ty_desc, label, module, srcloc) /* nothing */
#define traceHeapProfSampleBegin(era) /* nothing */
#define traceHeapBioProfSampleBegin(era, time) /* nothing */
#define traceHeapProfSampleEnd(era) /* nothing */
#define traceHeapProfSampleCostCentre(profile_id, stack, residency) /* nothing */
#define traceHeapProfSampleString(profile_id, label, residency) /* nothing */

#define traceConcMarkBegin() /* nothing */
#define traceConcMarkEnd(marked_obj_count) /* nothing */
#define traceConcSyncBegin() /* nothing */
#define traceConcSyncEnd() /* nothing */
#define traceConcSweepBegin() /* nothing */
#define traceConcSweepEnd() /* nothing */
#define traceConcUpdRemSetFlush(cap) /* nothing */
#define traceNonmovingHeapCensus(blk_size, census) /* nothing */

#define flushTrace() /* nothing */

#endif /* TRACING */

// If DTRACE is enabled, but neither DEBUG nor TRACING, we need a C land
// wrapper for the user-msg probe (as we can't expand that in GHC.Builtin.PrimOpss.cmm)
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
                           par_tot_copied,              \
                           par_balanced_copied)         \
    HASKELLEVENT_GC_STATS(heap_capset, gens,            \
                           copies, slop, fragmentation, \
                           par_n_threads,               \
                           par_max_copied,              \
                           par_balanced_copied,         \
                           par_tot_copied)
#define dtraceEventMemReturn(current, needed, returned) \
    HASKELLEVENT_MEM_RETURN(current, needed, returned)
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
#define dtraceEventBlocksSize(heap_capset, size)        \
    HASKELLEVENT_BLOCKS_SIZE(heap_capset, size)
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
                           par_tot_copied,              \
                           par_balanced_copied)         /* nothing */
#define dtraceEventMemReturn(current, needed, returned) /* nothing */
#define dtraceHeapInfo(heap_capset, gens,               \
                       maxHeapSize, allocAreaSize,      \
                       mblockSize, blockSize)           /* nothing */
#define dtraceEventHeapAllocated(cap, heap_capset,      \
                                 allocated)             /* nothing */
#define dtraceEventHeapSize(heap_capset, size)          /* nothing */
#define dtraceEventBlocksSize(heap_capset, size)        /* nothing */
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
//   very cheap - usually, one no-op.  Consequently, dtrace can be used with
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

INLINE_HEADER void traceEventMigrateThread(Capability *cap     STG_UNUSED,
                                           StgTSO     *tso     STG_UNUSED,
                                           uint32_t    new_cap STG_UNUSED)
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

    // Ensure that the eventlog buffer is flushed since otherwise its events
    // may never make it to the output stream.
    // See Note [Eventlog concurrency].
#if defined(TRACING)
    if (eventlog_enabled) {
        flushLocalEventsBuf(cap);
    }
# else
    flushLocalEventsBuf(cap);
#endif

}

INLINE_HEADER void traceEventThreadWakeup(Capability *cap       STG_UNUSED,
                                          StgTSO     *tso       STG_UNUSED,
                                          uint32_t    other_cap STG_UNUSED)
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
                                     uint32_t    gen            STG_UNUSED,
                                     W_        copied         STG_UNUSED,
                                     W_        slop           STG_UNUSED,
                                     W_        fragmentation  STG_UNUSED,
                                     uint32_t  par_n_threads  STG_UNUSED,
                                     W_        par_max_copied STG_UNUSED,
                                     W_        par_tot_copied STG_UNUSED,
                                     W_        par_balanced_copied STG_UNUSED)
{
    if (RTS_UNLIKELY(TRACE_gc)) {
        traceEventGcStats_(cap, heap_capset, gen,
                           copied, slop, fragmentation,
                           par_n_threads, par_max_copied,
                           par_tot_copied, par_balanced_copied);
    }
    dtraceEventGcStats(heap_capset, gen,
                       copied, slop, fragmentation,
                       par_n_threads, par_max_copied,
                       par_tot_copied, par_balanced_copied);
}

INLINE_HEADER void traceEventMemReturn(Capability *cap            STG_UNUSED,
                                     uint32_t    current_mblocks STG_UNUSED,
                                     uint32_t    needed_mblocks  STG_UNUSED,
                                     uint32_t    returned_mblocks STG_UNUSED)
{
    if (RTS_UNLIKELY(TRACE_gc)) {
        traceEventMemReturn_(cap, current_mblocks, needed_mblocks, returned_mblocks);
    }
    dtraceEventMemReturn(current_mblocks, needed_mblocks, returned_mblocks);
}

INLINE_HEADER void traceEventHeapInfo(CapsetID    heap_capset   STG_UNUSED,
                                      uint32_t  gens          STG_UNUSED,
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

INLINE_HEADER void traceEventBlocksSize(Capability *cap         STG_UNUSED,
                                        CapsetID    heap_capset STG_UNUSED,
                                        W_        heap_size   STG_UNUSED)
{
    traceHeapEvent(cap, EVENT_BLOCKS_SIZE, heap_capset, heap_size);
    dtraceEventBlocksSize(heap_capset, heap_size);
}

INLINE_HEADER void traceEventHeapLive(Capability *cap         STG_UNUSED,
                                      CapsetID    heap_capset STG_UNUSED,
                                      W_        heap_live   STG_UNUSED)
{
    traceHeapEvent(cap, EVENT_HEAP_LIVE, heap_capset, heap_live);
    dtraceEventHeapLive(heap_capset, heap_live);
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
                                        uint32_t capno  STG_UNUSED)
{
    traceCapsetEvent(EVENT_CAPSET_ASSIGN_CAP, capset, capno);
    dtraceCapsetAssignCap(capset, capno);
}

INLINE_HEADER void traceCapsetRemoveCap(CapsetID capset STG_UNUSED,
                                        uint32_t capno  STG_UNUSED)
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
#if defined(THREADED_RTS)
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
                                        uint32_t    victim_cap STG_UNUSED)
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
