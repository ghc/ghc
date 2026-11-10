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
#include <realtimeapiset.h>

#include <time.h>

/* Convert FILETIMEs into secs */
static inline Time fileTimeToRtsTime(FILETIME ft)
{
    Time t;
    t = ((Time)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
    t = NSToTime(t * 100);
    /* FILETIMES are in units of 100ns */
    return t;
}

Time getCurrentThreadCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (!GetThreadTimes(GetCurrentThread(), &creationTime,
                        &exitTime, &kernelTime, &userTime)) {
        sysErrorBelch("getCurrentThreadCPUTime: Win32 error %lu", GetLastError());
        return 0;
    }

    return fileTimeToRtsTime(userTime);
}

Time getProcessCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (!GetProcessTimes(GetCurrentProcess(), &creationTime,
                         &exitTime, &kernelTime, &userTime)) {
        return 0;
    }

    return fileTimeToRtsTime(userTime);
}

void initializeTimer(void)
{
}

/* Note [Windows time/clock functions]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A couple useful references for time/clocks:
https://learn.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps
https://devblogs.microsoft.com/oldnewthing/20170921-00/?p=97057/

The advice in the first article is plainly to use QueryPerformanceCounter. It
says this is now always supported in Windows XP and later, and reliable from
Vista, and fast from Windows 7 and 8 (relying on reliably TSCs). The internet
is full of reports that QPC is unreliable, but these do all seem to be from
older Windows versions (or folk lore based on earlier Windows versions or older
hardware).

Now on the other hand it is worth noting that the article is about the use
cases of high resolution timestamps or measuring time intervals. It's not
really clear that it means its ideal for a monotonic clock, which should be
accurate (not just precise) over longer periods of time.

In the second article, Raymond notes:

> And then there are the so-called precise variants of the system timer
> functions. These take the system timer value and combine it with the
> system performance counter to get a high-accuracy timestamp. [...]
>
>  + GetSystemTimePreciseAsFileTime
>  + QueryInterruptTimePrecise, QueryUnbiasedInterruptTimePrecise
>
> These high-accuracy functions give you the best of both worlds: Time
> correlated with real-world clocks, but with the accuracy of the system
> performance counter. But as noted, it comes at a performance cost.

The idea here is that the system timer is very accurate (over long time
periods), but is (very) coarse grained, while the TSC is very high frequency
but (at least historically) has been subject to drift over longer periods,
so by combining the two one can get the best of both worlds.

This makes the "Precise" functions seem like a good choice for a monotonic
clock. Also, they have the benefit of having correspond not-precise ones that
are comparable time sources but coarse and faster. It may well be useful to
have fast coarse clocks, but only if they are coarse measures of the same clock
rather than unrelated clocks.

Finally, the difference between Query­Unbiased­Interrupt­Time­Precise and
Query­Interrupt­Time­Precise is that the "unbiased" clock does not count time when
the system is suspended. We're following the "steady clock" semantics, so we do
want to include suspended time, hence we use Query­Interrupt­Time­Precise.
*/
Time getMonotonicTime(void)
{
    unsigned long long interrupt_time;
    /* See Note [Windows time/clock functions]
     * The "interrupt time" count is units of 100 nanoseconds.
     */
    QueryInterruptTimePrecise(&interrupt_time);
    Time time = interrupt_time * 100ll; /* Convert to nanoseconds */
    return time;
}

Time getWallClockTime(void)
{
    /* Windows has a bunch of time APIs but none that directly give
       us unix epoch time, so we have to do a little dance. */

    FILETIME filetime;
    ULARGE_INTEGER unixtime;

    /* See Note [Windows time/clock functions]
       Windows FILETIME timestamps use an epoch-based time,
       using a 64bit unsigned word. The time is measured in
       units of 100 nanoseconds since an epoch of 1601. */
    GetSystemTimePreciseAsFileTime(&filetime);

    /* FILETIME isn't directly a 64bit word, but a struct with
       a pair of 32bit words, so we have to convert via a
       ULARGE_INTEGER struct which is a handy union type */
    unixtime.LowPart  = filetime.dwLowDateTime;
    unixtime.HighPart = filetime.dwHighDateTime;

    /* We have to do an epoch conversion, since FILETIME uses 1601
       while we want unix epoch of 1970. In case you were wondering,
       there were 11,644,473,600 seconds between 1601 and 1970, then
       multiply by 100 for units of 100 nanoseconds. */
    return ((Time)unixtime.QuadPart - 116444736000000000ull) * 100ull;
}
