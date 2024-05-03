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
#include "ThreadLabels.h"

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

// events
uint8_t TRACE_sched;
uint8_t TRACE_gc;
uint8_t TRACE_nonmoving_gc;
uint8_t TRACE_spark_sampled;
uint8_t TRACE_spark_full;
uint8_t TRACE_user;
uint8_t TRACE_cap;

#if defined(THREADED_RTS)
static Mutex trace_utx;
#endif

#if defined(DEBUG)
static void traceCap_stderr(Capability *cap, char *msg, ...);
#endif

/* ---------------------------------------------------------------------------
   Starting up / shutting down the tracing facilities
 --------------------------------------------------------------------------- */

/*
 * Update the TRACE_* globals. Must be called whenever RtsFlags.TraceFlags is
 * modified.
 */
static void updateTraceFlagCache (void)
{
    // -Ds turns on scheduler tracing too
    TRACE_sched =
        RtsFlags.TraceFlags.scheduler ||
        RtsFlags.DebugFlags.scheduler;

    // -Dg turns on gc tracing too
    TRACE_gc =
        RtsFlags.TraceFlags.gc ||
        RtsFlags.DebugFlags.gc ||
        RtsFlags.DebugFlags.scheduler;

    TRACE_nonmoving_gc =
        RtsFlags.TraceFlags.nonmoving_gc;

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
}

void initTracing (void)
{
#if defined(THREADED_RTS)
    initMutex(&trace_utx);
#endif

    updateTraceFlagCache();

    if (TRACE_gc && RtsFlags.GcFlags.giveStats == NO_GC_STATS) {
        RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
    }

    /* Note: we can have any of the TRACE_* flags turned on even when
       eventlog_enabled is off. In the DEBUG way we may be tracing to stderr.
     */
    initEventLogging();

    if (RtsFlags.TraceFlags.tracing == TRACE_EVENTLOG
            && RtsFlags.TraceFlags.nullWriter) {
        startEventLogging(&NullEventLogWriter);
    }
    else if (RtsFlags.TraceFlags.tracing == TRACE_EVENTLOG
            && rtsConfig.eventlog_writer != NULL) {
        startEventLogging(rtsConfig.eventlog_writer);
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

// Used to reset tracing in a forked child
void resetTracing (void)
{
    restartEventLogging();
}

void flushTrace (void)
{
    if (eventlog_enabled) {
        flushEventLog(NULL);
    }
}

void tracingAddCapabilities (uint32_t from, uint32_t to)
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
    int threadLabelLen = 0;
    char *threadLabel = "";
    if (tso->label) {
        threadLabelLen = (int) tso->label->bytes;
        threadLabel = (char *) tso->label->payload;
    }

    switch (tag) {
    case EVENT_CREATE_THREAD:   // (cap, thread)
        debugBelch("cap %d: created thread %" FMT_Word "[\"%.*s\"]" "\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel);
        break;
    case EVENT_RUN_THREAD:      //  (cap, thread)
        debugBelch("cap %d: running thread %" FMT_Word "[\"%.*s\"]"" (%s)\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel, what_next_strs[tso->what_next]);
        break;
    case EVENT_THREAD_RUNNABLE: // (cap, thread)
        debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]"" appended to run queue\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel);
        break;
    case EVENT_MIGRATE_THREAD:  // (cap, thread, new_cap)
        debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]" " migrating to cap %d\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel, (int)info1);
        break;
    case EVENT_THREAD_WAKEUP:   // (cap, thread, info1_cap)
        debugBelch("cap %d: waking up thread %" FMT_Word "[\"%.*s\"]" " on cap %d\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel, (int)info1);
        break;

    case EVENT_STOP_THREAD:     // (cap, thread, status)
        if (info1 == 6 + BlockedOnBlackHole) {
            debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]" " stopped (blocked on black hole owned by thread %lu)\n",
                       cap->no, (W_)tso->id, threadLabelLen, threadLabel, (long)info2);
        } else if (info1 == StackOverflow) {
            debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]"
                       " stopped (stack overflow, size %lu)\n",
                      cap->no, (W_)tso->id, threadLabelLen, threadLabel, (long)info2);

        } else {
            debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]" " stopped (%s)\n",
                       cap->no, (W_)tso->id, threadLabelLen, threadLabel, thread_stop_reasons[info1]);
        }
        break;
    default:
        debugBelch("cap %d: thread %" FMT_Word "[\"%.*s\"]" ": event %d\n\n",
                   cap->no, (W_)tso->id, threadLabelLen, threadLabel, tag);
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

void traceEventMemReturn_ (Capability *cap,
                          uint32_t    current_mblocks,
                          uint32_t    needed_mblocks,
                          uint32_t    returned_mblocks)
{
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        traceCap_stderr(cap, "Memory Return (Current: %u) (Needed: %u) (Returned: %u)"
                       , current_mblocks, needed_mblocks, returned_mblocks);
    } else
#endif
    {
        postEventMemReturn( cap, CAPSET_HEAP_DEFAULT
                          , current_mblocks, needed_mblocks, returned_mblocks);
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

#if defined(HAVE_GETPID)
        postCapsetEvent(EVENT_OSPROCESS_PID,
                        CAPSET_OSPROCESS_DEFAULT,
                        getpid());
#endif

#if !defined(mingw32_HOST_OS) && defined(HAVE_GETPID)
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
#if !defined(HAVE_GETPID)
        EventKernelThreadId tid    = 0;
#else
        EventKernelThreadId tid    = kernelThreadId();
#endif
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
void traceHeapBioProfSampleBegin(StgInt era, StgWord64 time)
{
    if (eventlog_enabled) {
        postHeapBioProfSampleBegin(era, time);
    }
}

void traceHeapProfSampleBegin(StgInt era)
{
    if (eventlog_enabled) {
        postHeapProfSampleBegin(era);
    }
}

void traceHeapProfSampleEnd(StgInt era)
{
    if (eventlog_enabled) {
        postHeapProfSampleEnd(era);
    }
}

void traceHeapProfSampleString(StgWord8 profile_id,
                               const char *label, StgWord residency)
{
    if (eventlog_enabled) {
        postHeapProfSampleString(profile_id, label, residency);
    }
}

void traceIPE(const InfoProvEnt *ipe)
{
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        ACQUIRE_LOCK(&trace_utx);

        char closure_desc_buf[CLOSURE_DESC_BUFFER_SIZE] = {};
        formatClosureDescIpe(ipe, closure_desc_buf);

        tracePreface();
        debugBelch("IPE: table_name %s, closure_desc %s, ty_desc %s, label %s, unit %s, module %s, srcloc %s:%s\n",
                   ipe->prov.table_name, closure_desc_buf, ipe->prov.ty_desc,
                   ipe->prov.label, ipe->prov.unit_id, ipe->prov.module,
                   ipe->prov.src_file, ipe->prov.src_span);

        RELEASE_LOCK(&trace_utx);
    } else
#endif
    if (eventlog_enabled) {
        postIPE(ipe);
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

// This one is for .hp samples
void traceHeapProfSampleCostCentre(StgWord8 profile_id,
                                   CostCentreStack *stack, StgWord residency)
{
    if (eventlog_enabled) {
        postHeapProfSampleCostCentre(profile_id, stack, residency);
    }
}

// This one is for .prof samples
void traceProfSampleCostCentre(Capability *cap,
                               CostCentreStack *stack, StgWord tick)
{
    if (eventlog_enabled) {
        postProfSampleCostCentre(cap, stack, tick);
    }
}
void traceProfBegin(void)
{
    if (eventlog_enabled) {
        postProfBegin();
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

void traceUserBinaryMsg(Capability *cap, uint8_t *msg, size_t size)
{
    /* Note: normally we don't check the TRACE_* flags here as they're checked
       by the wrappers in Trace.h. But traceUserMsg is special since it has no
       wrapper (it's called from cmm code), so we check TRACE_user here
     */
    if (eventlog_enabled && TRACE_user) {
        postUserBinaryEvent(cap, EVENT_USER_BINARY_MSG, msg, size);
    }
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
                       char       *label,
                       size_t      len)
{
#if defined(DEBUG)
    if (RtsFlags.TraceFlags.tracing == TRACE_STDERR) {
        ACQUIRE_LOCK(&trace_utx);
        tracePreface();
        debugBelch("cap %d: thread %" FMT_Word " has label %.*s\n",
                   cap->no, (W_)tso->id, (int) len, label);
        RELEASE_LOCK(&trace_utx);
    } else
#endif
    {
        postThreadLabel(cap, tso->id, label, len);
    }
}

void traceConcMarkBegin(void)
{
    if (eventlog_enabled)
        postEventNoCap(EVENT_CONC_MARK_BEGIN);
}

void traceConcMarkEnd(StgWord32 marked_obj_count)
{
    if (eventlog_enabled)
        postConcMarkEnd(marked_obj_count);
}

void traceConcSyncBegin(void)
{
    if (eventlog_enabled)
        postEventNoCap(EVENT_CONC_SYNC_BEGIN);
}

void traceConcSyncEnd(void)
{
    if (eventlog_enabled)
        postEventNoCap(EVENT_CONC_SYNC_END);
}

void traceConcSweepBegin(void)
{
    if (eventlog_enabled)
        postEventNoCap(EVENT_CONC_SWEEP_BEGIN);
}

void traceConcSweepEnd(void)
{
    if (eventlog_enabled)
        postEventNoCap(EVENT_CONC_SWEEP_END);
}

void traceConcUpdRemSetFlush(Capability *cap)
{
    if (eventlog_enabled)
        postConcUpdRemSetFlush(cap);
}

void traceNonmovingHeapCensus(uint16_t blk_size,
                              const struct NonmovingAllocCensus *census)
{
    if (eventlog_enabled && TRACE_nonmoving_gc)
        postNonmovingHeapCensus(blk_size, census);
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
