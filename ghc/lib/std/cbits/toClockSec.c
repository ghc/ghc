/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: toClockSec.c,v 1.4 1999/09/19 19:21:22 sof Exp $
 *
 * toClockSec Runtime Support
 */

#include "Rts.h"
#include "timezone.h"
#include "stgio.h"

StgInt
toClockSec(I_ year, I_ mon, I_ mday, I_ hour, I_ min, I_ sec, I_ isdst, StgByteArray res)
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

    *(time_t *)res = t;
    return 1;
}
