#ifndef TIMEZONE_H
#define TIMEZONE_H

#define _OSF_SOURCE

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if HAVE_TM_ZONE
#define ZONE(x)	    (((struct tm *)x)->tm_zone)
#define GMTOFF(x)   (((struct tm *)x)->tm_gmtoff)
#else 
#if HAVE_TZNAME
extern time_t timezone, altzone;
extern char *tmzone[2];
#define ZONE(x)	    (((struct tm *)x)->tm_isdst ? tmzone[1] : tmzone[0])
#define GMTOFF(x)   (((struct tm *)x)->tm_isdst ? altzone : timezone)
#endif
#endif

#endif