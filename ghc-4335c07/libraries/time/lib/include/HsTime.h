#ifndef __HSTIME_H__
#define __HSTIME_H__

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define HAVE_TIME_H 1
#else

#include "HsTimeConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#endif

#if HAVE_TIME_H
#include <time.h>
#endif

long int get_current_timezone_seconds (time_t,int* pdst,char const* * pname);

#endif
