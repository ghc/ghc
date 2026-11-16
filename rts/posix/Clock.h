/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2012, 2026
 *
 * The choice of POSIX clocks used in the RTS
 *
 * ---------------------------------------------------------------------------*/

#pragma once


#if defined(linux_HOST_OS) || defined(linux_android_HOST_OS)
/* On Linux the CLOCK_MONOTONIC does not count time while the machine is
 * suspend. We have defined getMonotonicTime to be a steady clock (which does
 * count time while suspend). So on Linux we use CLOCK_BOOTTIME:
 *
 * > A  nonsettable  system-wide  clock  that is identical to CLOCK_MONOTONIC,
 * > except that it also includes any time that the system is suspended.
 *
 * The only downside is that there is no CLOCK_BOOTTIME_COARSE, only
 * CLOCK_MONOTONIC_COARSE.
 */
#define RTS_CLOCK_MONOTONIC CLOCK_BOOTTIME

#else
/* On (most?) other POSIX platforms the CLOCK_MONOTONIC counts time while the
 * machine is suspend. In particular this is true on FreeBSD and macOS.
 */
#define RTS_CLOCK_MONOTONIC CLOCK_MONOTONIC
#endif


#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
/* On macOS CLOCK_UPTIME_RAW is fast, and precise but not steady */
#define RTS_CLOCK_HIGHRES   CLOCK_UPTIME_RAW

#elif defined(linux_HOST_OS) || defined(linux_android_HOST_OS)
/* On Linux CLOCK_MONOTONIC and CLOCK_BOOTTIME are precise. We use
 * CLOCK_BOOTTIME for consistency with RTS_CLOCK_MONOTONIC above.
 */
#define RTS_CLOCK_HIGHRES   CLOCK_BOOTTIME

#else
#define RTS_CLOCK_HIGHRES   CLOCK_MONOTONIC
#endif


#define RTS_CLOCK_REALTIME  CLOCK_REALTIME
#define RTS_CLOCK_CPUTIME_THREAD CLOCK_THREAD_CPUTIME_ID
#define RTS_CLOCK_CPUTIME_PROCESS CLOCK_PROCESS_CPUTIME_ID
