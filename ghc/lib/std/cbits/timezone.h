/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: timezone.h,v 1.8 1999/03/03 17:17:05 simonm Exp $
 *
 * Time-zone support header
 */

#ifndef TIMEZONE_H
#define TIMEZONE_H

#define _OSF_SOURCE

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

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

/* Double sigh.  The timezone variable is only available under Linux
 * when we're compiling NON_POSIX_SOURCE (or _GNU_SOURCE or whatever),
 * but to make that work we have to make sure NON_POSIX_SOURCE is
 * defined before anything from /usr/include is included.  To avoid
 * infecting too much source with NON_POSIX_SOURCE, we frob it
 * below...
 */
#undef HAVE_TIMEZONE

/* The correct solution to this problem would appear to be to ditch
 * the standard GNU configure tests for the time stuff, and hack up
 * our own that test for POSIX-compliant time support first, then
 * BSD-style time stuff.
 */
#endif

#ifdef solaris2_TARGET_OS
#undef HAVE_TIMEZONE
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
#  if cygwin32_TARGET_OS
#   define tzname _tzname
#  endif
#  ifndef mingw32_TARGET_OS
extern char *tzname[2];
#  endif

#  define ZONE(x)	 (((struct tm *)x)->tm_isdst ? tzname[1] : tzname[0])
#  define SETZONE(x,z)
# else /* ! HAVE_TZNAME */
/* We're in trouble. If you should end up here, please report this as a bug. */
#  error Dont know how to get at timezone name on your OS.
# endif /* ! HAVE_TZNAME */
/* Get the offset in secs from UTC, if (struct tm) doesn't supply it. */

#ifdef mingw32_TARGET_OS
#define timezone _timezone
#else
# ifdef cygwin32_TARGET_OS
#  define timezone _timezone
# endif
#endif

#ifndef HAVE_TIMEZONE
extern TYPE_TIMEZONE timezone;
#endif

# if HAVE_ALTZONE
extern time_t altzone;
#  define GMTOFF(x)   	 (((struct tm *)x)->tm_isdst ? altzone : timezone )
# else /* ! HAVE_ALTZONE */
/* Assume that DST offset is 1 hour ... */
#  define GMTOFF(x) (((struct tm *)x)->tm_isdst ? (timezone - 3600) : timezone )
# endif /* ! HAVE_ALTZONE */
#endif  /* ! HAVE_TM_ZONE */

#endif /* TIMEZONE_H */
