/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2006
 *
 * Debug and performance tracing
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "OSThreads.h"
#include "Trace.h"
#include "RtsFlags.h"
#include "GetTime.h"
#include "Stats.h"

/*
  Features we want:
    - multiple log message classes
    - outpout thread ID & time on each message
    - thread-safe
    - trace source locations?
    - break into the debugger?
*/

StgWord32 classes_enabled; // not static due to inline funcs

#ifdef THREADED_RTS
static Mutex trace_utx;
#endif

#ifdef DEBUG
#define DEBUG_FLAG(name, class) \
    if (RtsFlags.DebugFlags.name) classes_enabled |= class;
#else
#define DEBUG_FLAG(name, class) \
    /* nothing */
#endif

#ifdef PAR
#define PAR_FLAG(name, class) \
    if (RtsFlags.ParFlags.Debug.name) classes_enabled |= class;
#else
#define PAR_FLAG(name, class) \
    /* nothing */
#endif

#ifdef GRAN
#define GRAN_FLAG(name, class) \
    if (RtsFlags.GranFlags.Debug.name) classes_enabled |= class;
#else
#define GRAN_FLAG(name, class) \
    /* nothing */
#endif

#define TRACE_FLAG(name, class) \
    if (RtsFlags.TraceFlags.name) classes_enabled |= class;


void initTracing (void)
{
#ifdef THREADED_RTS
    initMutex(&trace_utx);
#endif

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
    DEBUG_FLAG(gran,         DEBUG_gran);
    DEBUG_FLAG(par,          DEBUG_par);
    DEBUG_FLAG(linker,       DEBUG_linker);
    DEBUG_FLAG(squeeze,      DEBUG_squeeze);

    PAR_FLAG(verbose,        PAR_DEBUG_verbose);
    PAR_FLAG(bq,             PAR_DEBUG_bq);
    PAR_FLAG(schedule,       PAR_DEBUG_schedule);
    PAR_FLAG(free,           PAR_DEBUG_free);
    PAR_FLAG(resume,         PAR_DEBUG_resume);
    PAR_FLAG(weight,         PAR_DEBUG_weight);
    PAR_FLAG(fetch,          PAR_DEBUG_fetch);
    PAR_FLAG(fish,           PAR_DEBUG_fish);
    PAR_FLAG(tables,         PAR_DEBUG_tables);
    PAR_FLAG(packet,         PAR_DEBUG_packet);
    PAR_FLAG(pack,           PAR_DEBUG_pack);
    PAR_FLAG(paranoia,       PAR_DEBUG_paranoia);

    GRAN_FLAG(event_trace,   GRAN_DEBUG_event_trace);
    GRAN_FLAG(event_stats,   GRAN_DEBUG_event_stats);
    GRAN_FLAG(bq,            GRAN_DEBUG_bq);
    GRAN_FLAG(pack,          GRAN_DEBUG_pack);
    GRAN_FLAG(checkSparkQ,   GRAN_DEBUG_checkSparkQ);
    GRAN_FLAG(thunkStealing, GRAN_DEBUG_thunkStealing);
    GRAN_FLAG(randomSteal,   GRAN_DEBUG_randomSteal);
    GRAN_FLAG(findWork,      GRAN_DEBUG_findWork);
    GRAN_FLAG(unused,        GRAN_DEBUG_unused);
    GRAN_FLAG(pri,           GRAN_DEBUG_pri);
    GRAN_FLAG(checkLight,    GRAN_DEBUG_checkLight);
    GRAN_FLAG(sortedQ,       GRAN_DEBUG_sortedQ);
    GRAN_FLAG(blockOnFetch,  GRAN_DEBUG_blockOnFetch);
    GRAN_FLAG(packBuffer,    GRAN_DEBUG_packBuffer);
    GRAN_FLAG(blockedOnFetch_sanity, GRAN_DEBUG_BOF_sanity);

    TRACE_FLAG(sched, TRACE_sched);
}

static void tracePreface (void)
{
#ifdef THREADED_RTS
    debugBelch("%12lx: ", (unsigned long)osThreadId());
#endif
    if (RtsFlags.TraceFlags.timestamp) {
	debugBelch("%9" FMT_Word64 ": ", stat_getElapsedTime());
    }
}

void trace (StgWord32 class, const char *str, ...)
{
    va_list ap;
    va_start(ap,str);

    ACQUIRE_LOCK(&trace_utx);

    if ((classes_enabled & class) != 0) {
	tracePreface();
	vdebugBelch(str,ap);
	debugBelch("\n");
    }

    RELEASE_LOCK(&trace_utx);

    va_end(ap);
}

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
