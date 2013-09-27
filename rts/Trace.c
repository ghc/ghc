/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2006-2009
 *
 * Debug and performance tracing
 *
 * ---------------------------------------------------------------------------*/

// external headers
#include "Rts.h"

// internal headers
#include "Trace.h"

#ifdef TRACING

#include "GetTime.h"
#include "GetEnv.h"
#include "Stats.h"
#include "eventlog/EventLog.h"
#include "Threads.h"
#include "Printer.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef DEBUG
// debugging flags, set with +RTS -D<something>
int DEBUG_sched;
int DEBUG_interp;
int DEBUG_weak;
int DEBUG_gccafs;
int DEBUG_gc;
int DEBUG_block_alloc;
int DEBUG_sanity;
int DEBUG_stable;
int DEBUG_stm;
int DEBUG_prof;
int DEBUG_gran;
int DEBUG_par;
int DEBUG_linker;
int DEBUG_squeeze;
int DEBUG_hpc;
int DEBUG_sparks;
#endif

// events
int TRACE_sched;
int TRACE_gc;
int TRACE_spark_sampled;
int TRACE_spark_full;
int TRACE_user;

#ifdef THREADED_RTS
static Mutex trace_utx;
#endif

static rtsBool eventlog_enabled;

/* ---------------------------------------------------------------------------
   Starting up / shuttting down the tracing facilities
 --------------------------------------------------------------------------- */

void initTracing (void)
{
#ifdef THREADED_RTS
    initMutex(&trace_utx);
#endif

#ifdef DEBUG
#define DEBUG_FLAG(name, class) \
    class = RtsFlags.DebugFlags.name ? 1 : 0;

    DEBUG_FLAG(scheduler,    DEBUG_sched);

    DEBUG_FLAG(interpreter,  DEBUG_interp);
    DEBUG_FLAG(weak,         DEBUG_weak);
    DEBUG_FLAG(gccafs,       DEBUG_gccafs);
    DEBUG_FLAG(gc,           DEBUG_gc);
    DEBUG_FLAG(block_alloc,  DEBUG_block_alloc);
    DEBUG_FLAG(sanity,       DEBUG_sanity);
    DEBUG_FLAG(stable,       DEBUG_stable);
    DEBUG_FLAG(stm,          DEBUG_stm);
    DEBUG_FLAG(prof,         DEBUG_prof);
    DEBUG_FLAG(linker,       DEBUG_linker);
    DEBUG_FLAG(squeeze,      DEBUG_squeeze);
    DEBUG_FLAG(hpc,          DEBUG_hpc);
    DEBUG_FLAG(sparks,       DEBUG_sparks);
#endif

    // -Ds turns on scheduler tracing too
    TRACE_sched =
        RtsFlags.TraceFlags.scheduler ||
        RtsFlags.DebugFlags.scheduler;

    // -Dg turns on gc tracing too
    TRACE_gc =
        RtsFlags.TraceFlags.gc ||
        RtsFlags.DebugFlags.gc ||
        RtsFlags.DebugFlags.scheduler;
    if (TRACE_gc && RtsFlags.GcFlags.giveStats == NO_GC_STATS) {
        RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
    }

    TRACE_spark_sampled =
        RtsFlags.TraceFlags.sparks_sampled;

    // -Dr turns on full spark tracing
    TRACE_spark_full =
        RtsFlags.TraceFlags.sparks_full ||
        RtsFlags.DebugFlags.sparks;

    TRACE_user =
        RtsFlags.TraceFlags.user;

    eventlog_enabled = RtsFlags.TraceFlags.tracing == TRACE_EVENTLOG;

    /* Note: we can have any of the TRACE_* flags turned on even when
       eventlog_enabled is off. In the DEBUG way we may be tracing to stderr.
     */

    if (eventlog_enabled) {
        initEventLogging();
    }
}

void endTracing (void)
{
    if (eventlog_enabled) {
        endEventLogging();
    }
}

void freeTracing (void)
{
    if (eventlog_enabled) {
        freeEventLogging();
    }
}

void resetTracing (void)
{
    if (eventlog_enabled) {
        abortEventLogging(); // abort eventlog inherited from parent
        initEventLogging(); // child starts its own eventlog
    }
}

void tracingAddCapapilities (nat from, nat to)
{
    if (eventlog_enabled) {
        moreCapEventBufs(from,to);
    }
}

/* ---------------------------------------------------------------------------
   Emitting trace messages/events
 --------------------------------------------------------------------------- */

#ifdef DEBUG
static void tracePreface (void)
{
#ifdef THREADED_RTS
    debugBelch("%12lx: ", (unsigned long)osThreadId());
#endif
    if (RtsFlags.TraceFlags.timestamp) {
	debugBelch("%9" FMT_Word64 ": ", stat_getElapsedTime());
    }
}
#endif

#ifdef DEBUG
static char *thread_stop_reasons[] = {
    [HeapOverflow]                              = "heap overflow",
    [StackOverflow]                             = "stack overflow",
    [ThreadYielding]                            = "yielding",
    [ThreadBlocked]                             = "blocked",
    [ThreadFinished]                            = "finished",
    [ThreadSwitch]                              = "switched to",
    [THREAD_SUSPENDED_FOREIGN_CALL]             = "suspended while making a foreign call",
    [STOP_EVENT_OFFSET + BlockedOnMVar]         = "blocked on an MVar",
    [STOP_EVENT_OFFSET + BlockedOnMVarRead]     = "blocked on an atomic MVar read",
    [STOP_EVENT_OFFSET + BlockedOnBlackHole]    = "blocked on a black hole",
    [STOP_EVENT_OFFSET + BlockedOnRead]         = "blocked on a read operation",
    [STOP_EVENT_OFFSET + BlockedOnWrite]        = "blocked on a write operation",
    [STOP_EVENT_OFFSET + BlockedOnDelay]        = "blocked on a delay operation",
    [STOP_EVENT_OFFSET + BlockedOnSTM]          = "blocked on STM",
    [STOP_EVENT_OFFSET + BlockedOnDoProc]       = "blocked on asyncDoProc",
    [STOP_EVENT_OFFSET + BlockedOnCCall]        = "blocked on a foreign call",
    [STOP_EVENT_OFFSET + BlockedOnCCall_Interruptible] = "blocked on a foreign call (interruptible)",
    [STOP_EVENT_OFFSET + BlockedOnMsgThrowTo]   = "blocked on throwTo",
    [STOP_EVENT_OFFSET + ThreadMigrating]       = "migrating",
    [STOP_EVENT_OFFSET + BlockedInHaskell]       = "blocked on a user-level concurrent data structure",
    [STOP_EVENT_OFFSET + Yielded]               = "blocked on a user-level scheduler (Yielded)"
};
#endif

#ifdef DEBUG
static void traceSchedEvent_stderr (Capability *cap, EventTypeNum tag,
                                    StgTSO *tso,
                                    StgWord info1 STG_UNUSED,
                                    StgWord info2 STG_UNUSED)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    switch (tag) {
    case EVENT_CREATE_THREAD:   // (cap, thread)
        debugBelch("cap %d: created thread %" FMT_Word "\n",
                   cap->no, (W_)tso->id);
        break;
    case EVENT_RUN_THREAD:      //  (cap, thread)
        debugBelch("cap %d: running thread %" FMT_Word " (%s)\n",
                   cap->no, (W_)tso->id, what_next_strs[tso->what_next]);
        break;
    case EVENT_THREAD_RUNNABLE: // (cap, thread)
        debugBelch("cap %d: thread %" FMT_Word " appended to run queue\n",
                   cap->no, (W_)tso->id);
        break;
    case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
        debugBelch("cap %d: thread %" FMT_Word " migrating to cap %d\n",
                   cap->no, (W_)tso->id, (int)info1);
        break;
    case EVENT_THREAD_WAKEUP:   // (cap, thread, info1_cap)
        debugBelch("cap %d: waking up thread %" FMT_Word " on cap %d\n",
                   cap->no, (W_)tso->id, (int)info1);
        break;

    case EVENT_STOP_THREAD:     // (cap, thread, status)
        if (info1 == STOP_EVENT_OFFSET + BlockedOnBlackHole) {
            debugBelch("cap %d: thread %" FMT_Word " stopped (blocked on black hole owned by thread %lu)\n",
                       cap->no, (W_)tso->id, (long)info2);
        } else {
            debugBelch("cap %d: thread %" FMT_Word " stopped (%s)\n",
                       cap->no, (W_)tso->id, thread_stop_reasons[info1]);
        }
        break;
    default:
        debugBelch("cap %d: thread %" FMT_Word ": event %d\n\n",
                   cap->no, (W_)tso->id, tag);
        break;
    }

    RELEASE_LOCK(&trace_utx);
}
#endif

void traceSchedEvent_ (Capability *cap, EventTypeNum tag,
                       StgTSO *tso, StgWord info1, StgWord info2)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceSchedEvent_stderr(cap, tag, tso, info1, info2);
    } else
#endif
    {
        postSchedEvent(cap,tag,tso ? tso->id : 0, info1, info2);
    }
}

#ifdef DEBUG
static void traceGcEvent_stderr (Capability *cap, EventTypeNum tag)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    switch (tag) {
      case EVENT_REQUEST_SEQ_GC:  // (cap)
          debugBelch("cap %d: requesting sequential GC\n", cap->no);
          break;
      case EVENT_REQUEST_PAR_GC:  // (cap)
          debugBelch("cap %d: requesting parallel GC\n", cap->no);
          break;
      case EVENT_GC_START:        // (cap)
          debugBelch("cap %d: starting GC\n", cap->no);
          break;
      case EVENT_GC_END:          // (cap)
          debugBelch("cap %d: finished GC\n", cap->no);
          break;
      case EVENT_GC_IDLE:         // (cap)
          debugBelch("cap %d: GC idle\n", cap->no);
          break;
      case EVENT_GC_WORK:         // (cap)
          debugBelch("cap %d: GC working\n", cap->no);
          break;
      case EVENT_GC_DONE:         // (cap)
          debugBelch("cap %d: GC done\n", cap->no);
          break;
      case EVENT_GC_GLOBAL_SYNC:  // (cap)
          debugBelch("cap %d: all caps stopped for GC\n", cap->no);
          break;
      default:
          barf("traceGcEvent: unknown event tag %d", tag);
          break;
    }

    RELEASE_LOCK(&trace_utx);
}
#endif

void traceGcEvent_ (Capability *cap, EventTypeNum tag)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceGcEvent_stderr(cap, tag);
    } else
#endif
    {
        /* currently all GC events are nullary events */
        postEvent(cap, tag);
    }
}

void traceGcEventAtT_ (Capability *cap, StgWord64 ts, EventTypeNum tag)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceGcEvent_stderr(cap, tag);
    } else
#endif
    {
        /* assuming nullary events and explicitly inserting a timestamp */
        postEventAtTimestamp(cap, ts, tag);
    }
}

void traceHeapEvent_ (Capability   *cap,
                      EventTypeNum  tag,
                      CapsetID      heap_capset,
                      W_          info1)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* no stderr equivalent for these ones */
    } else
#endif
    {
        postHeapEvent(cap, tag, heap_capset, info1);
    }
}

void traceEventHeapInfo_ (CapsetID    heap_capset,
                          nat         gens,
                          W_        maxHeapSize,
                          W_        allocAreaSize,
                          W_        mblockSize,
                          W_        blockSize)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* no stderr equivalent for these ones */
    } else
#endif
    {
        postEventHeapInfo(heap_capset, gens,
                          maxHeapSize, allocAreaSize,
                          mblockSize, blockSize);
    }
}

void traceEventGcStats_  (Capability *cap,
                          CapsetID    heap_capset,
                          nat         gen,
                          W_        copied,
                          W_        slop,
                          W_        fragmentation,
                          nat         par_n_threads,
                          W_        par_max_copied,
                          W_        par_tot_copied)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* no stderr equivalent for these ones */
    } else
#endif
    {
        postEventGcStats(cap, heap_capset, gen,
                         copied, slop, fragmentation,
                         par_n_threads, par_max_copied, par_tot_copied);
    }
}

void traceCapEvent (Capability   *cap,
                    EventTypeNum  tag)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        ACQUIRE_LOCK(&trace_utx);

        tracePreface();
        switch (tag) {
        case EVENT_CAP_CREATE:   // (cap)
            debugBelch("cap %d: initialised\n", cap->no);
            break;
        case EVENT_CAP_DELETE:   // (cap)
            debugBelch("cap %d: shutting down\n", cap->no);
            break;
        case EVENT_CAP_ENABLE:   // (cap)
            debugBelch("cap %d: enabling capability\n", cap->no);
            break;
        case EVENT_CAP_DISABLE:  // (cap)
            debugBelch("cap %d: disabling capability\n", cap->no);
            break;
        }
        RELEASE_LOCK(&trace_utx);
    } else
#endif
    {
        if (eventlog_enabled) {
            postCapEvent(tag, (EventCapNo)cap->no);
        }
    }
}

void traceCapsetEvent (EventTypeNum tag,
                        CapsetID capset,
                        StgWord info)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_sched)
        // When events go to stderr, it is annoying to see the capset
        // events every time, so we only emit them with -Ds.
    {
        ACQUIRE_LOCK(&trace_utx);

        tracePreface();
        switch (tag) {
        case EVENT_CAPSET_CREATE:   // (capset, capset_type)
            debugBelch("created capset %" FMT_Word " of type %d\n", (W_)capset, (int)info);
            break;
        case EVENT_CAPSET_DELETE:   // (capset)
            debugBelch("deleted capset %" FMT_Word "\n", (W_)capset);
            break;
        case EVENT_CAPSET_ASSIGN_CAP:  // (capset, capno)
            debugBelch("assigned cap %" FMT_Word " to capset %" FMT_Word "\n",
                       (W_)info, (W_)capset);
            break;
        case EVENT_CAPSET_REMOVE_CAP:  // (capset, capno)
            debugBelch("removed cap %" FMT_Word " from capset %" FMT_Word "\n",
                       (W_)info, (W_)capset);
            break;
        }
        RELEASE_LOCK(&trace_utx);
    } else
#endif
    {
        if (eventlog_enabled) {
            postCapsetEvent(tag, capset, info);
        }
    }
}

void traceWallClockTime_(void) {
    if (eventlog_enabled) {
        postWallClockTime(CAPSET_CLOCKDOMAIN_DEFAULT);
    }
}

void traceOSProcessInfo_(void) {
    if (eventlog_enabled) {
        postCapsetEvent(EVENT_OSPROCESS_PID,
                        CAPSET_OSPROCESS_DEFAULT,
                        getpid());

#if !defined(cygwin32_HOST_OS) && !defined (mingw32_HOST_OS)
/* Windows has no strong concept of process heirarchy, so no getppid().
 * In any case, this trace event is mainly useful for tracing programs
 * that use 'forkProcess' which Windows doesn't support anyway.
 */
        postCapsetEvent(EVENT_OSPROCESS_PPID,
                        CAPSET_OSPROCESS_DEFAULT,
                        getppid());
#endif
        {
            char buf[256];
            snprintf(buf, sizeof(buf), "GHC-%s %s", ProjectVersion, RtsWay);
            postCapsetStrEvent(EVENT_RTS_IDENTIFIER,
                               CAPSET_OSPROCESS_DEFAULT,
                               buf);
        }
        {
            int argc = 0; char **argv;
            getFullProgArgv(&argc, &argv);
            if (argc != 0) {
                postCapsetVecEvent(EVENT_PROGRAM_ARGS,
                                   CAPSET_OSPROCESS_DEFAULT,
                                   argc, argv);
            }
        }
        {
            int envc = 0; char **envv;
            getProgEnvv(&envc, &envv);
            if (envc != 0) {
                postCapsetVecEvent(EVENT_PROGRAM_ENV,
                                   CAPSET_OSPROCESS_DEFAULT,
                                   envc, envv);
            }
            freeProgEnvv(envc, envv);
        }
    }
}

#ifdef DEBUG
static void traceSparkEvent_stderr (Capability *cap, EventTypeNum tag,
                                    StgWord info1)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    switch (tag) {

    case EVENT_CREATE_SPARK_THREAD: // (cap, spark_thread)
        debugBelch("cap %d: creating spark thread %lu\n",
                   cap->no, (long)info1);
        break;
    case EVENT_SPARK_CREATE:        // (cap)
        debugBelch("cap %d: added spark to pool\n",
                   cap->no);
        break;
    case EVENT_SPARK_DUD:           //  (cap)
        debugBelch("cap %d: discarded dud spark\n",
                   cap->no);
        break;
    case EVENT_SPARK_OVERFLOW:      // (cap)
        debugBelch("cap %d: discarded overflowed spark\n",
                   cap->no);
        break;
    case EVENT_SPARK_RUN:           // (cap)
        debugBelch("cap %d: running a spark\n",
                   cap->no);
        break;
    case EVENT_SPARK_STEAL:         // (cap, victim_cap)
        debugBelch("cap %d: stealing a spark from cap %d\n",
                   cap->no, (int)info1);
        break;
    case EVENT_SPARK_FIZZLE:        // (cap)
        debugBelch("cap %d: fizzled spark removed from pool\n",
                   cap->no);
        break;
    case EVENT_SPARK_GC:            // (cap)
        debugBelch("cap %d: GCd spark removed from pool\n",
                   cap->no);
        break;
    default:
        barf("traceSparkEvent: unknown event tag %d", tag);
        break;
    }

    RELEASE_LOCK(&trace_utx);
}
#endif

void traceSparkEvent_ (Capability *cap, EventTypeNum tag, StgWord info1)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceSparkEvent_stderr(cap, tag, info1);
    } else
#endif
    {
        postSparkEvent(cap,tag,info1);
    }
}

void traceSparkCounters_ (Capability *cap,
                          SparkCounters counters,
                          StgWord remaining)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* we currently don't do debug tracing of spark stats but we must
           test for TRACE_STDERR because of the !eventlog_enabled case. */
    } else
#endif
    {
        postSparkCountersEvent(cap, counters, remaining);
    }
}

void traceTaskCreate_ (Task       *task,
                       Capability *cap)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* We currently don't do debug tracing of tasks but we must
           test for TRACE_STDERR because of the !eventlog_enabled case. */
    } else
#endif
    {
        EventTaskId         taskid = serialisableTaskId(task);
        EventKernelThreadId tid    = kernelThreadId();
        postTaskCreateEvent(taskid, cap->no, tid);
    }
}

void traceTaskMigrate_ (Task       *task,
                        Capability *cap,
                        Capability *new_cap)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* We currently don't do debug tracing of tasks but we must
           test for TRACE_STDERR because of the !eventlog_enabled case. */
    } else
#endif
    {
        EventTaskId taskid = serialisableTaskId(task);
        postTaskMigrateEvent(taskid, cap->no, new_cap->no);
    }
}

void traceTaskDelete_ (Task *task)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* We currently don't do debug tracing of tasks but we must
           test for TRACE_STDERR because of the !eventlog_enabled case. */
    } else
#endif
    {
        EventTaskId taskid = serialisableTaskId(task);
        postTaskDeleteEvent(taskid);
    }
}

#ifdef DEBUG
static void traceCap_stderr(Capability *cap, char *msg, va_list ap)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    debugBelch("cap %d: ", cap->no);
    vdebugBelch(msg,ap);
    debugBelch("\n");

    RELEASE_LOCK(&trace_utx);
}
#endif

void traceCap_(Capability *cap, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);

#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceCap_stderr(cap, msg, ap);
    } else
#endif
    {
        postCapMsg(cap, msg, ap);
    }

    va_end(ap);
}

#ifdef DEBUG
static void trace_stderr(char *msg, va_list ap)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    vdebugBelch(msg,ap);
    debugBelch("\n");

    RELEASE_LOCK(&trace_utx);
}
#endif

void trace_(char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);

#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        trace_stderr(msg, ap);
    } else
#endif
    {
        postMsg(msg, ap);
    }

    va_end(ap);
}

static void traceFormatUserMsg(Capability *cap, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);

    /* Note: normally we don't check the TRACE_* flags here as they're checked
       by the wrappers in Trace.h. But traceUserMsg is special since it has no
       wrapper (it's called from cmm code), so we check TRACE_user here
     */
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_user) {
        traceCap_stderr(cap, msg, ap);
    } else
#endif
    {
        if (eventlog_enabled && TRACE_user) {
            postUserMsg(cap, msg, ap);
        }
    }
    dtraceUserMsg(cap->no, msg);
}

void traceUserMsg(Capability *cap, char *msg)
{
    traceFormatUserMsg(cap, "%s", msg);
}

void traceUserMarker(Capability *cap, char *markername)
{
    /* Note: traceUserMarker is special since it has no wrapper (it's called
       from cmm code), so we check eventlog_enabled and TRACE_user here.
     */
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_user) {
        ACQUIRE_LOCK(&trace_utx);
        tracePreface();
        debugBelch("cap %d: User marker: %s\n", cap->no, markername);
        RELEASE_LOCK(&trace_utx);
    } else
#endif
    {
        if (eventlog_enabled && TRACE_user) {
            postUserMarker(cap, markername);
        }
    }
    dtraceUserMarker(cap->no, markername);
}


void traceThreadLabel_(Capability *cap,
                       StgTSO     *tso,
                       char       *label)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        ACQUIRE_LOCK(&trace_utx);
        tracePreface();
        debugBelch("cap %d: thread %" FMT_Word " has label %s\n",
                   cap->no, (W_)tso->id, label);
        RELEASE_LOCK(&trace_utx);
    } else
#endif
    {
        postThreadLabel(cap, tso->id, label);
    }
}

void traceThreadStatus_ (StgTSO *tso USED_IF_DEBUG)
{
#ifdef DEBUG
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        printThreadStatus(tso);
    } else
#endif
    {
        /* nothing - no event for this one yet */
    }
}

void traceEventStartup_(int nocaps)
{
    if (eventlog_enabled) {
        postEventStartup(nocaps);
    }
}

#ifdef DEBUG
void traceBegin (const char *str, ...)
{
    va_list ap;
    va_start(ap,str);

    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    vdebugBelch(str,ap);
}

void traceEnd (void)
{
    debugBelch("\n");
    RELEASE_LOCK(&trace_utx);
}
#endif /* DEBUG */

#endif /* TRACING */

// If DTRACE is enabled, but neither DEBUG nor TRACING, we need a C land
// wrapper for the user-msg probe (as we can't expand that in PrimOps.cmm)
//
#if !defined(DEBUG) && !defined(TRACING) && defined(DTRACE)

void dtraceUserMsgWrapper(Capability *cap, char *msg)
{
    dtraceUserMsg(cap->no, msg);
}

void dtraceUserMarkerWrapper(Capability *cap, char *msg)
{
    dtraceUserMarker(cap->no, msg);
}

#endif /* !defined(DEBUG) && !defined(TRACING) && defined(DTRACE) */
