/* -----------------------------------------------------------------------------
 * $Id: Proftimer.c,v 1.6 2000/04/03 15:54:49 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#if defined (PROFILING)

#include "Rts.h"
#include "Profiling.h"
#include "Itimer.h"
#include "Proftimer.h"

rtsBool do_prof_ticks = rtsFalse;       /* enable profiling ticks */

void
stopProfTimer(void)
{				/* Stops time profile */
    if (time_profiling) {
	do_prof_ticks = rtsFalse;
    }
};

void
startProfTimer(void)
{				/* Starts time profile */
    if (time_profiling) {
	do_prof_ticks = rtsTrue;
    }
};

void
handleProfTick(void)
{
    if (do_prof_ticks) {
	CCS_TICK(CCCS);
    }
}
#endif /* PROFILING */
