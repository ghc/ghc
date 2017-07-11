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

#if defined(TRACING)

#include "GetTime.h"
#include "GetEnv.h"
#include "Stats.h"
#include "eventlog/EventLog.h"
#include "rts/EventLogWriter.h"
#include "Threads.h"
#include "Printer.h"
#include "RtsFlags.h"

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

// events
int TRACE_sched;
int TRACE_gc;
int TRACE_spark_sampled;
int TRACE_spark_full;
int TRACE_user;
int TRACE_cap;

#if defined(THREADED_RTS)
static Mutex trace_utx;
#endif

static bool eventlog_enabled;

/* ---------------------------------------------------------------------------
   Starting up / shutting down the tracing facilities
 --------------------------------------------------------------------------- */

static const EventLogWriter *getEventLogWriter(void)
{
    return rtsConfig.eventlog_writer;
}

void initTracing (void)
{
    const EventLogWriter *eventlog_writer = getEventLogWriter();

#if defined(THREADED_RTS)
    initMutex(&trace_utx);
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

    // We trace cap events if we're tracing anything else
    TRACE_cap =
        TRACE_sched ||
        TRACE_gc ||
        TRACE_spark_sampled ||
        TRACE_spark_full ||
        TRACE_user;

    eventlog_enabled = RtsFlags.TraceFlags.tracing == TRACE_EVENTLOG &&
                        eventlog_writer != NULL;

    /* Note: we can have any of the TRACE_* flags turned on even when
       eventlog_enabled is off. In the DEBUG way we may be tracing to stderr.
     */

    if (eventlog_enabled) {
        initEventLogging(eventlog_writer);
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
    const EventLogWriter *eventlog_writer;
    eventlog_writer = getEventLogWriter();

    if (eventlog_enabled) {
        abortEventLogging(); // abort eventlog inherited from parent
        if (eventlog_writer != NULL) {
            initEventLogging(eventlog_writer); // child starts its own eventlog
        }
    }
}

void tracingAddCapapilities (uint32_t from, uint32_t to)
{
    if (eventlog_enabled) {
        moreCapEventBufs(from,to);
    }
}

/* ---------------------------------------------------------------------------
   Emitting trace messages/events
 --------------------------------------------------------------------------- */

#if defined(DEBUG)
static void tracePreface (void)
{
#if defined(THREADED_RTS)
    debugBelch("%12lx: ", (unsigned long)osThreadId());
#endif
    if (RtsFlags.TraceFlags.timestamp) {
        debugBelch("%9" FMT_Word64 ": ", stat_getElapsedTime());
    }
}
#endif

#if defined(DEBUG)
static char *thread_stop_reasons[] = {
    [HeapOverflow] = "heap overflow",
    [StackOverflow] = "stack overflow",
    [ThreadYielding] = "yielding",
    [ThreadBlocked] = "blocked",
    [ThreadFinished] = "finished",
    [THREAD_SUSPENDED_FOREIGN_CALL] = "suspended while making a foreign call",
    [6 + BlockedOnMVar]         = "blocked on an MVar",
    [6 + BlockedOnMVarRead]     = "blocked on an atomic MVar read",
    [6 + BlockedOnBlackHole]    = "blocked on a black hole",
    [6 + BlockedOnRead]         = "blocked on a read operation",
    [6 + BlockedOnWrite]        = "blocked on a write operation",
    [6 + BlockedOnDelay]        = "blocked on a delay operation",
    [6 + BlockedOnSTM]          = "blocked on STM",
    [6 + BlockedOnDoProc]       = "blocked on asyncDoProc",
    [6 + BlockedOnCCall]        = "blocked on a foreign call",
    [6 + BlockedOnCCall_Interruptible] = "blocked on a foreign call (interruptible)",
    [6 + BlockedOnMsgThrowTo]   =  "blocked on throwTo",
    [6 + ThreadMigrating]       =  "migrating"
};
#endif

#if defined(DEBUG)
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
        if (info1 == 6 + BlockedOnBlackHole) {
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
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceSchedEvent_stderr(cap, tag, tso, info1, info2);
    } else
#endif
    {
        postSchedEvent(cap,tag,tso ? tso->id : 0, info1, info2);
    }
}

#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* no stderr equivalent for these ones */
    } else
#endif
    {
        postHeapEvent(cap, tag, heap_capset, info1);
    }
}

void traceEventHeapInfo_ (CapsetID    heap_capset,
                          uint32_t  gens,
                          W_        maxHeapSize,
                          W_        allocAreaSize,
                          W_        mblockSize,
                          W_        blockSize)
{
#if defined(DEBUG)
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
                          uint32_t  gen,
                          W_        copied,
                          W_        slop,
                          W_        fragmentation,
                          uint32_t  par_n_threads,
                          W_        par_max_copied,
                          W_        par_tot_copied,
                          W_        par_balanced_copied)
{
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        /* no stderr equivalent for these ones */
    } else
#endif
    {
        postEventGcStats(cap, heap_capset, gen,
                         copied, slop, fragmentation,
                         par_n_threads, par_max_copied,
                         par_tot_copied, par_balanced_copied);
    }
}

void traceCapEvent_ (Capability   *cap,
                     EventTypeNum  tag)
{
#if defined(DEBUG)
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

void traceCapsetEvent_ (EventTypeNum tag,
                        CapsetID     capset,
                        StgWord      info)
{
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_sched)
        // When events go to stderr, it is annoying to see the capset
        // events every time, so we only emit them with -Ds.
    {
        ACQUIRE_LOCK(&trace_utx);

        tracePreface();
        switch (tag) {
        case EVENT_CAPSET_CREATE:   // (capset, capset_type)
            debugBelch("created capset %" FMT_Word32 " of type %d\n", capset,
                       (int)info);
            break;
        case EVENT_CAPSET_DELETE:   // (capset)
            debugBelch("deleted capset %" FMT_Word32 "\n", capset);
            break;
        case EVENT_CAPSET_ASSIGN_CAP:  // (capset, capno)
            debugBelch("assigned cap %" FMT_Word " to capset %" FMT_Word32 "\n",
                       info, capset);
            break;
        case EVENT_CAPSET_REMOVE_CAP:  // (capset, capno)
            debugBelch("removed cap %" FMT_Word " from capset %" FMT_Word32
                       "\n", info, capset);
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

#if !defined (mingw32_HOST_OS)
/* Windows has no strong concept of process hierarchy, so no getppid().
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

#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
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
#if defined(DEBUG)
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

void traceHeapProfBegin(StgWord8 profile_id)
{
    if (eventlog_enabled) {
        postHeapProfBegin(profile_id);
    }
}

void traceHeapProfSampleBegin(StgInt era)
{
    if (eventlog_enabled) {
        postHeapProfSampleBegin(era);
    }
}

void traceHeapProfSampleString(StgWord8 profile_id,
                               const char *label, StgWord residency)
{
    if (eventlog_enabled) {
        postHeapProfSampleString(profile_id, label, residency);
    }
}

#if defined(PROFILING)
void traceHeapProfCostCentre(StgWord32 ccID,
                             const char *label,
                             const char *module,
                             const char *srcloc,
                             StgBool is_caf)
{
    if (eventlog_enabled) {
        postHeapProfCostCentre(ccID, label, module, srcloc, is_caf);
    }
}

void traceHeapProfSampleCostCentre(StgWord8 profile_id,
                                   CostCentreStack *stack, StgWord residency)
{
    if (eventlog_enabled) {
        postHeapProfSampleCostCentre(profile_id, stack, residency);
    }
}
#endif

#if defined(DEBUG)
static void vtraceCap_stderr(Capability *cap, char *msg, va_list ap)
{
    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    debugBelch("cap %d: ", cap->no);
    vdebugBelch(msg,ap);
    debugBelch("\n");

    RELEASE_LOCK(&trace_utx);
}

static void traceCap_stderr(Capability *cap, char *msg, ...)
{
  va_list ap;
  va_start(ap,msg);
  vtraceCap_stderr(cap, msg, ap);
  va_end(ap);
}
#endif

void traceCap_(Capability *cap, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);

#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        vtraceCap_stderr(cap, msg, ap);
    } else
#endif
    {
        postCapMsg(cap, msg, ap);
    }

    va_end(ap);
}

#if defined(DEBUG)
static void vtrace_stderr(char *msg, va_list ap)
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

#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        vtrace_stderr(msg, ap);
    } else
#endif
    {
        postMsg(msg, ap);
    }

    va_end(ap);
}

void traceUserMsg(Capability *cap, char *msg)
{
    /* Note: normally we don't check the TRACE_* flags here as they're checked
       by the wrappers in Trace.h. But traceUserMsg is special since it has no
       wrapper (it's called from cmm code), so we check TRACE_user here
     */
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_user) {
        // Use "%s" as format string to ignore format specifiers in msg (#3874).
        traceCap_stderr(cap, "%s", msg);
    } else
#endif
    {
        if (eventlog_enabled && TRACE_user) {
            postUserEvent(cap, EVENT_USER_MSG, msg);
        }
    }
    dtraceUserMsg(cap->no, msg);
}

void traceUserMarker(Capability *cap, char *markername)
{
    /* Note: traceUserMarker is special since it has no wrapper (it's called
       from cmm code), so we check eventlog_enabled and TRACE_user here.
     */
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR && TRACE_user) {
        traceCap_stderr(cap, "User marker: %s", markername);
    } else
#endif
    {
        if (eventlog_enabled && TRACE_user) {
            postUserEvent(cap, EVENT_USER_MARKER, markername);
        }
    }
    dtraceUserMarker(cap->no, markername);
}


void traceThreadLabel_(Capability *cap,
                       StgTSO     *tso,
                       char       *label)
{
#if defined(DEBUG)
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
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        printThreadStatus(tso);
    } else
#endif
    {
        /* nothing - no event for this one yet */
    }
}

#if defined(DEBUG)
void traceBegin (const char *str, ...)
{
    va_list ap;
    va_start(ap,str);

    ACQUIRE_LOCK(&trace_utx);

    tracePreface();
    vdebugBelch(str,ap);
    va_end(ap);
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
