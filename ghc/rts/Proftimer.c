/* -----------------------------------------------------------------------------
 * $Id: Proftimer.c,v 1.5 1999/08/25 16:11:49 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

/* Only have cost centres etc if PROFILING defined */

#if defined (PROFILING)

#include "Rts.h"
#include "ProfRts.h"
#include "Itimer.h"
#include "Proftimer.h"

nat current_interval = 1;               /* Current interval number -- 
					   stored in AGE */

nat interval_ticks = DEFAULT_INTERVAL;  /* No of ticks in an interval */

nat previous_ticks = 0;                 /* ticks in previous intervals */
nat current_ticks = 0;                  /* ticks in current interval */

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

#endif /* PROFILING */
