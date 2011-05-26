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

#endif /* TRACING */

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
 * Record a nullary event
 */
#define traceEvent(cap, tag)                    \
    if (RTS_UNLIKELY(TRACE_sched)) {            \
        traceEvent_(cap, tag);                  \
    }

void traceEvent_ (Capability *cap, EventTypeNum tag);

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
 */
void traceUserMsg(Capability *cap, char *msg);

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

#else /* !TRACING */

#define traceSchedEvent(cap, tag, tso, other) /* nothing */
#define traceSchedEvent2(cap, tag, tso, other, info) /* nothing */
#define traceEvent(cap, tag) /* nothing */
#define traceCap(class, cap, msg, ...) /* nothing */
#define trace(class, msg, ...) /* nothing */
#define debugTrace(class, str, ...) /* nothing */
#define debugTraceCap(class, cap, str, ...) /* nothing */
#define traceThreadStatus(class, tso) /* nothing */
#define traceEventStartup_(n_caps) /* nothing */

#endif /* TRACING */

// If DTRACE is enabled, but neither DEBUG nor TRACING, we need a C land
// wrapper for the user-msg probe (as we can't expand that in PrimOps.cmm)
//
#if !defined(DEBUG) && !defined(TRACING) && defined(DTRACE)

void dtraceUserMsgWrapper(Capability *cap, char *msg);

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
#define dtraceRunSpark(cap, tid)                        \
    HASKELLEVENT_RUN_SPARK(cap, tid)
#define dtraceStealSpark(cap, tid, victim_cap)          \
    HASKELLEVENT_STEAL_SPARK(cap, tid, victim_cap)
#define dtraceShutdown(cap)                             \
    HASKELLEVENT_SHUTDOWN(cap)
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
#define dtraceStartup(num_caps)                         \
    HASKELLEVENT_STARTUP(num_caps)
#define dtraceUserMsg(cap, msg)                         \
    HASKELLEVENT_USER_MSG(cap, msg)
#define dtraceGcIdle(cap)                               \
    HASKELLEVENT_GC_IDLE(cap)
#define dtraceGcWork(cap)                               \
    HASKELLEVENT_GC_WORK(cap)
#define dtraceGcDone(cap)                               \
    HASKELLEVENT_GC_DONE(cap)

#else /* !defined(DTRACE) */

#define dtraceCreateThread(cap, tid)                    /* nothing */
#define dtraceRunThread(cap, tid)                       /* nothing */
#define dtraceStopThread(cap, tid, status, info)        /* nothing */
#define dtraceThreadRunnable(cap, tid)                  /* nothing */
#define dtraceMigrateThread(cap, tid, new_cap)          /* nothing */
#define dtraceRunSpark(cap, tid)                        /* nothing */
#define dtraceStealSpark(cap, tid, victim_cap)          /* nothing */
#define dtraceShutdown(cap)                             /* nothing */
#define dtraceThreadWakeup(cap, tid, other_cap)         /* nothing */
#define dtraceGcStart(cap)                              /* nothing */
#define dtraceGcEnd(cap)                                /* nothing */
#define dtraceRequestSeqGc(cap)                         /* nothing */
#define dtraceRequestParGc(cap)                         /* nothing */
#define dtraceCreateSparkThread(cap, spark_tid)         /* nothing */
#define dtraceStartup(num_caps)                         /* nothing */
#define dtraceUserMsg(cap, msg)                         /* nothing */
#define dtraceGcIdle(cap)                               /* nothing */
#define dtraceGcWork(cap)                               /* nothing */
#define dtraceGcDone(cap)                               /* nothing */

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

INLINE_HEADER void traceEventRunSpark(Capability *cap STG_UNUSED, 
                                      StgTSO     *tso STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_RUN_SPARK, tso, 0);
    dtraceRunSpark((EventCapNo)cap->no, (EventThreadID)tso->id);
}

INLINE_HEADER void traceEventStealSpark(Capability *cap        STG_UNUSED, 
                                        StgTSO     *tso        STG_UNUSED,
                                        nat         victim_cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_STEAL_SPARK, tso, victim_cap);
    dtraceStealSpark((EventCapNo)cap->no, (EventThreadID)tso->id,
                     (EventCapNo)victim_cap);
}

INLINE_HEADER void traceEventShutdown(Capability *cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_SHUTDOWN, 0, 0);
    dtraceShutdown((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventThreadWakeup(Capability *cap       STG_UNUSED, 
                                          StgTSO     *tso       STG_UNUSED,
                                          nat         other_cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_THREAD_WAKEUP, tso, other_cap);
    dtraceThreadWakeup((EventCapNo)cap->no, (EventThreadID)tso->id,
                       (EventCapNo)other_cap);
}

INLINE_HEADER void traceEventGcStart(Capability *cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_GC_START, 0, 0);
    dtraceGcStart((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcEnd(Capability *cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_GC_END, 0, 0);
    dtraceGcEnd((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventRequestSeqGc(Capability *cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_REQUEST_SEQ_GC, 0, 0);
    dtraceRequestSeqGc((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventRequestParGc(Capability *cap STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_REQUEST_PAR_GC, 0, 0);
    dtraceRequestParGc((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventCreateSparkThread(Capability  *cap      STG_UNUSED, 
                                               StgThreadID spark_tid STG_UNUSED)
{
    traceSchedEvent(cap, EVENT_CREATE_SPARK_THREAD, 0, spark_tid);
    dtraceCreateSparkThread((EventCapNo)cap->no, (EventThreadID)spark_tid);
}

INLINE_HEADER void traceEventStartup(void)
{
    int n_caps;
#ifdef THREADED_RTS
    // XXX n_capabilities hasn't been initislised yet
    n_caps = RtsFlags.ParFlags.nNodes;
#else
    n_caps = 1;
#endif

    traceEventStartup_(n_caps);
    dtraceStartup(n_caps);
}

INLINE_HEADER void traceEventGcIdle(Capability *cap STG_UNUSED)
{
    traceEvent(cap, EVENT_GC_IDLE);
    dtraceGcIdle((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcWork(Capability *cap STG_UNUSED)
{
    traceEvent(cap, EVENT_GC_WORK);
    dtraceGcWork((EventCapNo)cap->no);
}

INLINE_HEADER void traceEventGcDone(Capability *cap STG_UNUSED)
{
    traceEvent(cap, EVENT_GC_DONE);
    dtraceGcDone((EventCapNo)cap->no);
}

#include "EndPrivate.h"

#endif /* TRACE_H */
