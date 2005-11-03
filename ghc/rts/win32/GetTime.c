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

Ticks
getProcessElapsedTime(void)
{
    FILETIME system_time;
    GetSystemTimeAsFileTime(&system_time);
    return fileTimeToTicks(system_time);
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
