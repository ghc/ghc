/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: toClockSec.c,v 1.6 2000/06/19 13:28:35 simonmar Exp $
 *
 * toClockSec Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "timezone.h"

StgInt
toClockSec(I_ year, I_ mon, I_ mday, I_ hour, I_ min, I_ sec, I_ tz, I_ isdst, StgByteArray res)
{
    struct tm tm;
    time_t t;

    tm.tm_year = year - 1900;
    tm.tm_mon = mon;
    tm.tm_mday = mday;
    tm.tm_hour = hour;
    tm.tm_min = min;
    tm.tm_sec = sec;
    tm.tm_isdst = isdst;

#ifdef HAVE_MKTIME
    t = mktime(&tm);
#elif defined(HAVE_TIMELOCAL)
    t = timelocal(&tm);
#else
    t = (time_t) -1;
#endif
    if (t == (time_t) -1)
	return 0;
    /*
     * mktime expects its argument to be in the local timezone, but
     * toUTCTime makes UTC-encoded CalendarTime's ...
     *
     * Since there is no any_tz_struct_tm-to-time_t conversion
     * function, we have to fake one... :-) If not in all, it works in
     * most cases (before, it was the other way round...)
     *
     * Luckily, mktime tells us, what it *thinks* the timezone is, so,
     * to compensate, we add the timezone difference to mktime's
     * result.
     */
    *(time_t *)res = t + tz - GMTOFF(&tm);
    return 1;
}
