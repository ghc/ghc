/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-independent interface to time measurement
 *
 * ---------------------------------------------------------------------------*/

#ifndef GETTIME_H
#define GETTIME_H

#include "BeginPrivate.h"

// We'll use a fixed resolution of usec for now.  The machine
// dependent implementation may have a different resolution, but we'll
// normalise to this for the machine independent interface.
#define TICKS_PER_SECOND 1000000
typedef StgInt64 Ticks;

Ticks getProcessCPUTime     (void);
Ticks getThreadCPUTime      (void);
Ticks getProcessElapsedTime (void);
void  getProcessTimes       (Ticks *user, Ticks *elapsed);

/* Get the current date and time.
   Uses seconds since the Unix epoch, plus nanoseconds
 */
void  getUnixEpochTime      (StgWord64 *sec, StgWord32 *nsec);

// Not strictly timing, but related
nat   getPageFaults         (void);

#include "EndPrivate.h"

#endif /* GETTIME_H */
