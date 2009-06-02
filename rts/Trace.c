/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2006-2009
 *
 * Debug and performance tracing
 *
 * ---------------------------------------------------------------------------*/

#ifdef DEBUG

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

#define DEBUG_FLAG(name, class) \
    if (RtsFlags.DebugFlags.name) classes_enabled |= class;

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
    DEBUG_FLAG(eventlog,     DEBUG_eventlog);
    DEBUG_FLAG(linker,       DEBUG_linker);
    DEBUG_FLAG(squeeze,      DEBUG_squeeze);
    DEBUG_FLAG(hpc,          DEBUG_hpc);
}

static void tracePreface (void)
{
#ifdef THREADED_RTS
    debugBelch("%12lx: ", (unsigned long)osThreadId());
#endif
    if (RtsFlags.DebugFlags.timestamp) {
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

#endif
