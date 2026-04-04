/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Profiling.h"
#include "Proftimer.h"
#include "Capability.h"
#include "Trace.h"

/*
 * N.B. These flags must all always be accessed via atomics since even in the
 * non-threaded runtime the timer may be provided by way of a signal.
 */

#if defined(PROFILING)
static bool do_prof_ticks = false;       // enable profiling ticks
#endif

static bool do_heap_prof_ticks = false;  // Whether the timer is currently ticking down
static bool heap_prof_timer_active = false;  // Whether the timer is enabled at all

/* The heap_prof_timer_active flag controls whether heap profiling is enabled
at all, once it is enabled, the `do_heap_prof_ticks` flag controls whether the
counter is currently counting down. This is paused, for example, in Schedule.c. */

// Sampling of Ticky-Ticky profiler to eventlog
#if defined(TICKY_TICKY) && defined(TRACING)
static int ticks_to_ticky_sample = 0;
bool performTickySample = false;
#endif

// Number of ticks until next heap census
static int ticks_to_heap_profile;

// Time for a heap profile on the next context switch
bool performHeapProfile;

void
stopProfTimer( void )
{
#if defined(PROFILING)
    atomic_store_explicit(&do_prof_ticks, false, memory_order_relaxed);
#endif
}

void
startProfTimer( void )
{
#if defined(PROFILING)
    atomic_store_explicit(&do_prof_ticks, true, memory_order_relaxed);
#endif
}

void
stopHeapProfTimer( void )
{
  if (RtsFlags.ProfFlags.doHeapProfile){
      atomic_store_explicit(&heap_prof_timer_active, false, memory_order_relaxed);
    pauseHeapProfTimer();
  }
}

void
startHeapProfTimer( void )
{
  if (RtsFlags.ProfFlags.doHeapProfile){
      atomic_store_explicit(&heap_prof_timer_active, true, memory_order_relaxed);
    resumeHeapProfTimer();
  }
}

void
pauseHeapProfTimer ( void ) {
    atomic_store_explicit(&do_heap_prof_ticks, false, memory_order_relaxed);
}


void
resumeHeapProfTimer ( void ) {
    if (RtsFlags.ProfFlags.doHeapProfile &&
        RtsFlags.ProfFlags.heapProfileIntervalTicks > 0) {
        atomic_store_explicit(&do_heap_prof_ticks, true, memory_order_relaxed);
    }
}

void
requestHeapCensus( void ){
  // If no profiling mode is passed then just ignore the call.
  if (RtsFlags.ProfFlags.doHeapProfile){
      atomic_store_explicit(&performHeapProfile, true, memory_order_relaxed);
  }
}

void
initProfTimer( void )
{
    atomic_store_explicit(&performHeapProfile, false, memory_order_relaxed);

    ticks_to_heap_profile = RtsFlags.ProfFlags.heapProfileIntervalTicks;

    /* This might look a bit strange but the heap profile timer can
      be toggled on/off from within Haskell by calling the startHeapProf
      function from within Haskell */
    if (RtsFlags.ProfFlags.startHeapProfileAtStartup){
      startHeapProfTimer();
    }
}

uint32_t total_ticks = 0;

void
handleProfTick(void)
{
#if defined(PROFILING)
    total_ticks++;
    if (atomic_load_explicit(&do_prof_ticks, memory_order_relaxed)) {
        uint32_t n;
        for (n=0; n < getNumCapabilities(); n++) {
            Capability *cap = getCapability(n);
            CostCentreStack *ccs = RELAXED_LOAD(&cap->r.rCCCS);
            ccs->time_ticks++;
            traceProfSampleCostCentre(cap, cap->r.rCCCS, total_ticks);
        }
    }
#endif

#if defined(TICKY_TICKY) && defined(TRACING)
    if (RtsFlags.TraceFlags.ticky) {
        ticks_to_ticky_sample--;
        if (ticks_to_ticky_sample <= 0) {
            ticks_to_ticky_sample = RtsFlags.ProfFlags.heapProfileIntervalTicks;
            atomic_store_explicit(&performTickySample, true, memory_order_relaxed);
        }
    }
#endif

    if (atomic_load_explicit(&do_heap_prof_ticks, memory_order_relaxed) &&
        atomic_load_explicit(&heap_prof_timer_active, memory_order_relaxed))  {
        ticks_to_heap_profile--;
        if (ticks_to_heap_profile <= 0) {
            ticks_to_heap_profile = RtsFlags.ProfFlags.heapProfileIntervalTicks;
            atomic_store_explicit(&performHeapProfile, true, memory_order_relaxed);
        }
    }
}
