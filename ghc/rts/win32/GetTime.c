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

/* elapsedtime() -- The current elapsed time in seconds */

#define HNS_PER_SEC 10000000LL /* FILETIMES are in units of 100ns */
/* Convert FILETIMEs into secs */
#define FT2longlong(ll,ft)    \
    (ll)=(ft).dwHighDateTime; \
    (ll) <<= 32;              \
    (ll) |= (ft).dwLowDateTime; \
    (ll) /= (unsigned long long) (HNS_PER_SEC / CLOCKS_PER_SEC)

/* cygwin32 or mingw32 version */
void
getProcessTimes( Ticks *user, Ticks *elapsed )
{
    static int is_win9x = -1;
    static Ticks elapsed_start = 0;

    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};
    long long int kT, uT;
    
    if (is_win9x < 0) {
      /* figure out whether we're on a Win9x box or not. */
      OSVERSIONINFO oi;
      BOOL b;

      /* Need to init the size field first.*/
      oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
      b = GetVersionEx(&oi);
      
      is_win9x = ( (b && (oi.dwPlatformId & VER_PLATFORM_WIN32_WINDOWS)) ? 1 : 0);
    }
 
    if (is_win9x) {
      /* On Win9x, just attribute all running time to the user. */
      SYSTEMTIME st;

      GetSystemTime(&st);
      SystemTimeToFileTime(&st,&userTime);
    } else {
      /* ToDo: pin down elapsed times to just the OS thread(s) that
	 are evaluating/managing Haskell code.
      */
      if (!GetProcessTimes (GetCurrentProcess(), &creationTime,
		          &exitTime, &kernelTime, &userTime)) {
	/* Probably on a Win95 box..*/
	*elapsed = 0;
	*user = 0;
	return;
      }
    }

    FT2longlong(kT,kernelTime);
    FT2longlong(uT,userTime);
    *elapsed = uT + kT;
    *user = uT;

    if (is_win9x) {
	/* User time is assumed to start at zero, so adjust for the fact
	   that we're using system time & not process time on Win9x.  */
	if (elapsed_start == 0) {
	    elapsed_start = *elapsed;
	}
	*user -= elapsed_start;
    }
}

Ticks getProcessCPUTime(void)
{
    Ticks user, elapsed;
    getProcessTimes(&user,&elapsed);
    return user;
}

Ticks getProcessElapsedTime(void)
{
    Ticks user, elapsed;
    getProcessTimes(&user,&elapsed);
    return elapsed;
}

nat getPageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
    return 0;
}
