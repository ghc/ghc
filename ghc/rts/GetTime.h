/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-independent interface to time measurement
 *
 * ---------------------------------------------------------------------------*/

#ifndef GETTIME_H
#define GETTIME_H

// We'll use a fixed resolution of usec for now.  The machine
// dependent implementation may have a different resolution, but we'll
// normalise to this for the machine independent interface.
#define TICKS_PER_SECOND 1000000
typedef StgInt64 Ticks;

Ticks getProcessCPUTime     (void);
Ticks getThreadCPUTime      (void);
Ticks getProcessElapsedTime (void);
void  getProcessTimes       (Ticks *user, Ticks *elapsed);

// Not strictly timing, but related
nat   getPageFaults         (void);

#endif /* GETTIME_H */
