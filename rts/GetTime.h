/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-independent interface to time measurement
 *
 * See also the public functions includes/rts/GetTime.h
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* Get the current time in a monotonic clock.
 *
 * The time is represented as nanoseconds since an arbitrary epoch.
 */
Time getMonotonicTime(void);

/* Get the current wall clock time.
 *
 * The time is represented as nanoseconds since the Unix epoch
 * (1970-01-01 00:00).
 */
Time getWallClockTime(void);

Time getProcessCPUTime(void);
Time getCurrentThreadCPUTime(void);

#include "EndPrivate.h"
