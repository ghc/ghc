/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2012, 2026
 *
 * The choice of POSIX clocks used in the RTS
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#define RTS_CLOCK_MONOTONIC CLOCK_MONOTONIC
#define RTS_CLOCK_REALTIME  CLOCK_REALTIME
#define RTS_CLOCK_CPUTIME_THREAD CLOCK_THREAD_CPUTIME_ID
#define RTS_CLOCK_CPUTIME_PROCESS CLOCK_PROCESS_CPUTIME_ID
