#ifndef TIMEZONE_H
#define TIMEZONE_H

#define _OSF_SOURCE

#if linux_TARGET_OS
/* Sigh, RedHat 5 has the TM_ZONE stuff, but only when _BSD_SOURCE is
 * on.  The configure script erroneously says we've got TM_ZONE, so
 * make sure we use the TZNAME stuff instead.
 *
 * Aside: tzname is POSIX, whereas tm_zone is BSD.  We should be using
 *  tzname by preference, but the GNU configure stuff gives us HAVE_TM_ZONE
 *  in preference to HAVE_TZNAME.  More sighs.
 */
# undef  HAVE_TM_ZONE
# define HAVE_TZNAME  1
#endif

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
#define ZONE(x)	         (((struct tm *)x)->tm_zone)
#define SETZONE(x,z)     (((struct tm *)x)->tm_zone = z)
#define GMTOFF(x)        (((struct tm *)x)->tm_gmtoff)
#else /* ! HAVE_TM_ZONE */
# if HAVE_TZNAME || cygwin32_TARGET_OS
#if cygwin32_TARGET_OS
extern char *tzname;
#else
extern char *tzname[2];
#endif
#  define ZONE(x)	 (((struct tm *)x)->tm_isdst ? tzname[1] : tzname[0])
#  define SETZONE(x,z)
# else /* ! HAVE_TZNAME */
/* We're in trouble. If you should end up here, please report this as a bug. */
#  error Dont know how to get at timezone name on your OS.
# endif /* ! HAVE_TZNAME */
/* Get the offset in secs from UTC, if (struct tm) doesn't supply it. */

extern TYPE_TIMEZONE timezone;

# if HAVE_ALTZONE
extern time_t altzone;
#  define GMTOFF(x)   	 (((struct tm *)x)->tm_isdst ? altzone : timezone)
# else /* ! HAVE_ALTZONE */
/* Assume that DST offset is 1 hour ... */
#  define GMTOFF(x) (((struct tm *)x)->tm_isdst ? (timezone - 3600) : timezone)
# endif /* ! HAVE_ALTZONE */
#endif  /* ! HAVE_TM_ZONE */

#endif /* TIMEZONE_H */
