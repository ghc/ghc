#include "HsTime.h"
#include <stdio.h>

long int get_current_timezone_seconds (time_t t,int* pdst,char const* * pname)
{
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
    // When compiling with MinGW (which does not provide a full POSIX
    // layer as opposed to CygWin) it's better to use the CRT's
    // underscore-prefixed `_tzset()` variant to avoid linker issues
    // as Microsoft considers the POSIX named `tzset()` function
    // deprecated (see http://msdn.microsoft.com/en-us/library/ms235384.aspx)
    _tzset();
#else
    tzset();
#endif

#if HAVE_LOCALTIME_R
    struct tm tmd;
    struct tm* ptm = localtime_r(&t,&tmd);
#else
    struct tm* ptm = localtime(&t);
#endif
    if (ptm)
    {
        int dst = ptm -> tm_isdst;
        *pdst = dst;
#if HAVE_TM_ZONE
        *pname = ptm -> tm_zone;
        return ptm -> tm_gmtoff;
#elif defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
        // We don't have a better API to use on Windows, the logic to
        // decide whether a given date/time falls within DST is
        // implemented as part of localtime() in the CRT.  This is_dst
        // flag is all we need here.
        *pname = dst ? _tzname[1] : _tzname[0];
        return - (dst ? _timezone - 3600 : _timezone);
#else
# if HAVE_TZNAME
        *pname = *tzname;
# else
#  error "Don't know how to get timezone name on your OS"
# endif
# if HAVE_DECL_ALTZONE
        return dst ? altzone : timezone;
# else
        return dst ? timezone - 3600 : timezone;
# endif
#endif // HAVE_TM_ZONE
    }
    else return 0x80000000;
}
