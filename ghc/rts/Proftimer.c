/* -----------------------------------------------------------------------------
 * $Id: Proftimer.c,v 1.8 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#if defined (PROFILING)

#include "PosixSource.h"

#include "Rts.h"
#include "Profiling.h"
#include "Itimer.h"
#include "Proftimer.h"
#include "RtsFlags.h"

static rtsBool do_prof_ticks = rtsFalse;       // enable profiling ticks
static rtsBool do_heap_prof_ticks = rtsFalse;  // enable heap profiling ticks

// Number of ticks until next heap census
static int ticks_to_heap_profile;

// Time for a heap profile on the next context switch
rtsBool performHeapProfile;

void
stopProfTimer( void )
{
    if (time_profiling) {
	do_prof_ticks = rtsFalse;
    }
}

void
startProfTimer( void )
{
    if (time_profiling) {
	do_prof_ticks = rtsTrue;
    }
}

void
stopHeapProfTimer( void )
{
    do_heap_prof_ticks = rtsFalse;
}

void
startHeapProfTimer( void )
{
    if (RtsFlags.ProfFlags.doHeapProfile) {
	do_heap_prof_ticks = rtsTrue;
    }
}

void
initProfTimer( void )
{
    performHeapProfile = rtsFalse;

    RtsFlags.ProfFlags.profileIntervalTicks = 
	RtsFlags.ProfFlags.profileInterval / TICK_MILLISECS;

    ticks_to_heap_profile = RtsFlags.ProfFlags.profileIntervalTicks;

    startHeapProfTimer();
}
    

void
handleProfTick(void)
{
    if (do_prof_ticks) {
	CCS_TICK(CCCS);
    }

    if (do_heap_prof_ticks) {
	ticks_to_heap_profile--;
	if (ticks_to_heap_profile <= 0) {
	    ticks_to_heap_profile = RtsFlags.ProfFlags.profileIntervalTicks;
	    performHeapProfile = rtsTrue;
	}
    }
}

#endif /* PROFILING */
