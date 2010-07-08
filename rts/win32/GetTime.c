/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "GetTime.h"

#include <windows.h>

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#define HNS_PER_SEC 10000000LL /* FILETIMES are in units of 100ns */
/* Convert FILETIMEs into secs */

static INLINE_ME Ticks
fileTimeToTicks(FILETIME ft)
{
    Ticks t;
    t = ((Ticks)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
    t = (t * TICKS_PER_SECOND) / HNS_PER_SEC;
    return t;
}    

static int is_win9x = -1;

static INLINE_ME rtsBool
isWin9x(void)
{
    if (is_win9x < 0) {
	/* figure out whether we're on a Win9x box or not. */
	OSVERSIONINFO oi;
	BOOL b;
	
	/* Need to init the size field first.*/
	oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	b = GetVersionEx(&oi);
      
	is_win9x = ( (b && (oi.dwPlatformId & VER_PLATFORM_WIN32_WINDOWS)) ? 1 : 0);
    }
    return is_win9x;
}


void
getProcessTimes(Ticks *user, Ticks *elapsed)
{
    *user    = getProcessCPUTime();
    *elapsed = getProcessElapsedTime();
}

Ticks
getProcessCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (isWin9x()) return getProcessElapsedTime();

    if (!GetProcessTimes(GetCurrentProcess(), &creationTime,
			 &exitTime, &kernelTime, &userTime)) {
	return 0;
    }

    return fileTimeToTicks(userTime);
}

// getProcessElapsedTime relies on QueryPerformanceFrequency 
// which should be available on any Windows computer thay you
// would want to run Haskell on. Satnam Singh, 5 July 2010.

Ticks
getProcessElapsedTime(void)
{
    // frequency represents the number of ticks per second
    // used by the QueryPerformanceFrequency implementaiton
    // and is represented by a 64-bit union type initially set to 0
    // and updated just once (hence use of static).
    static LARGE_INTEGER frequency = {.QuadPart = 0} ;  

    // system_time is a 64-bit union type used to represent the
    // tick count returned by QueryPerformanceCounter
    LARGE_INTEGER system_time ;

    // If this is the first time we are calling getProcessElapsedTime
    // then record the ticks per second used by QueryPerformanceCounter
    if (frequency.QuadPart == 0) {
      QueryPerformanceFrequency(&frequency);
    }
    
    // Get the tick count.
    QueryPerformanceCounter(&system_time) ;

    // Return the tick count as a millisecond value. 
    // Using double to compute the intermediate value, because a 64-bit
    // int would overflow when multiplied by TICKS_PER_SECOND in about 81 days.
    return (Ticks)((TICKS_PER_SECOND * (double)system_time.QuadPart) / (double)frequency.QuadPart) ;
}

Ticks
getThreadCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (isWin9x()) return getProcessCPUTime();

    if (!GetThreadTimes(GetCurrentThread(), &creationTime,
			&exitTime, &kernelTime, &userTime)) {
	return 0;
    }

    return fileTimeToTicks(userTime);
}

nat
getPageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
    return 0;
}
