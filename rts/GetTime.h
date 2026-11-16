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

/* Note [Meaning of clocks]
 * ~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Clock terminology
 *
 *  1. monotonic time: non-decreasing. This is for measuring intervals, not
 *     subject to time jumps (e.g. due to NTP or user fixing the time).
 *  2. real / wall clock time: corresponding to real world UTC time.
 *  3. CPU time: time spent by threads/processes running on a CPU.
 *  4. steady clock: (following the C++11 library definition): the time
 *     between ticks of this clock is constant. In particular this implies it
 *     _does_ keep time even when the system is suspended/hibernated. Note that
 *     a wall clock must be steady, whereas a monotonic clock need not be.
 *
 * An ideal clock would be:
 *  1. accurate: able to keep time over long periods;
 *  2. precise: a high resolution of clock measurement;
 *  3. fast: the time duration to measure the clock.
 *
 * Real clocks available on platforms must make compromises.
 *
 * We provide a choice of clocks that pick the best platform clocks given
 * different priorities between these desires.
 *
 * 1. A monotonic, steady, accurate clock: getMonotonicTime
 * 2. A precise monotonic clock: getHighResolutionTime
 * 3. An accurate real time clock: getWallClockTime
 *
 * We could also (but do not currently) provide:
 * 4. A fast monotonic clock (but not comparable to the accurate clock)
 * 5. A fast real time clock (a less precise real time clock, and comparable)
 *
 * In general these clocks are _not_ comparable. The only ones that _could_ be
 * comparable (if we were to need and provide them) are low res clocks that are
 * (guaranteed) to be low resolution measures of the same clock.
 */

/* Get the current time in a monotonic, steady, accurate clock.
 * See Note [Meaning of clocks].
 *
 * The time is represented as nanoseconds since an arbitrary epoch.
 *
 * This is the clock used for user timeouts like threadDelay.
 */
Time getMonotonicTime(void);

/* Get the time in a monotonic, high resolution clock.
 * See Note [Meaning of clocks].
 *
 * This is intended to be the most high resolution / precise clock available on
 * the platform. It is suitable for timing operations with maximum available
 * precision.
 *
 * It is not necessarily accurate at keeping time over long time periods (i.e.
 * may be subject to time drift). It is not necessarily steady: it may or may
 * not include time while the system is suspend.
 *
 * This is the clock used for measuring timing of activities within the RTS:
 * RTS stats, profiling and eventlog timestamps.
 */
Time getHighResolutionTime(void);

/* Get the time in a real time, accurate clock.
 * See Note [Meaning of clocks].
 *
 * The time is represented as nanoseconds since the Unix epoch
 * (1970-01-01 00:00).
 *
 * This clock is not used much, but is emitted into eventlogs on start to
 * enable matching the eventlog timestamps with wall clock time.
 */
Time getWallClockTime(void);

Time getProcessCPUTime(void);
Time getCurrentThreadCPUTime(void);

#include "EndPrivate.h"
