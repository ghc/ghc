/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: toLocalTime.c,v 1.5 2000/05/28 17:47:27 panne Exp $
 *
 * toCalendarTime Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "timezone.h"

StgInt
toLocalTime(I_ size, StgByteArray d, StgByteArray res)
{
    time_t t;
    struct tm *tm,*tmp=(struct tm *)res;

    switch(size) {
	default:
	    return 0;
	case 0:
	    t = 0;
	    break;
	case -1:
	    t = - (time_t) ((StgInt *)d)[0];
	    if (t > 0) 
		return 0;
	    break;
	case 1:
	    t = (time_t) ((StgInt *)d)[0];
	    if (t < 0) 
		return 0;
	    break;
	}
    tm = localtime(&t);
    
    if (tm == NULL)
	return 0;

    /*
      localtime() may return a ptr to statically allocated storage,
      so to make toLocalTime reentrant, we manually copy
      the structure into the (struct tm *) passed in.
    */
    tmp->tm_sec    = tm->tm_sec;
    tmp->tm_min    = tm->tm_min;
    tmp->tm_hour   = tm->tm_hour;
    tmp->tm_mday   = tm->tm_mday;
    tmp->tm_mon    = tm->tm_mon;
    tmp->tm_year   = tm->tm_year;
    tmp->tm_wday   = tm->tm_wday;
    tmp->tm_yday   = tm->tm_yday;
    tmp->tm_isdst  = tm->tm_isdst;
    /*
      If you don't have tm_zone in (struct tm), but
      you get at it via the shared tmzone[], you'll
      lose. Same goes for the tm_gmtoff field.
    
    */
#if HAVE_TM_ZONE
    strcpy(tmp->tm_zone,tm->tm_zone);
    tmp->tm_gmtoff = tm->tm_gmtoff;
#endif

    return 1;
}

StgInt
prim_toLocalTime(StgInt64 d, StgByteArray res)
{
    time_t t;
    struct tm *tm,*tmp=(struct tm *)res;

    t = (time_t) d;
    if (t < 0) 
        return 0;

    tm = localtime(&t);
    
    if (tm == NULL)
	return 0;

    /*
      localtime() may return a ptr to statically allocated storage,
      so to make toLocalTime reentrant, we manually copy
      the structure into the (struct tm *) passed in.
    */
    tmp->tm_sec    = tm->tm_sec;
    tmp->tm_min    = tm->tm_min;
    tmp->tm_hour   = tm->tm_hour;
    tmp->tm_mday   = tm->tm_mday;
    tmp->tm_mon    = tm->tm_mon;
    tmp->tm_year   = tm->tm_year;
    tmp->tm_wday   = tm->tm_wday;
    tmp->tm_yday   = tm->tm_yday;
    tmp->tm_isdst  = tm->tm_isdst;
    /*
      If you don't have tm_zone in (struct tm), but
      you get at it via the shared tmzone[], you'll
      lose. Same goes for the tm_gmtoff field.
    
    */
#if HAVE_TM_ZONE
    strcpy(tmp->tm_zone,tm->tm_zone);
    tmp->tm_gmtoff = tm->tm_gmtoff;
#endif

    return 1;
}
