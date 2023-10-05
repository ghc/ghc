/*
 * $XConsortium: Xos.h,v 1.47 91/08/17 17:14:38 rws Exp $
 * 
 * Copyright 1987 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 */

/* This is a collection of things to try and minimize system dependencies
 * in a "significant" number of source files.
 */

#pragma once

#include "lndir-Xosdefs.h"

/*
 * Get major data types (esp. caddr_t)
 */

#if defined(USG)
#if !defined(__TYPES__)
#if defined(CRAY)
#define word word_t
#endif /* CRAY */
#include <sys/types.h>			/* forgot to protect it... */
#define __TYPES__
#endif /* __TYPES__ */
#else /* USG */
#if defined(_POSIX_SOURCE) && defined(MOTOROLA)
#undef _POSIX_SOURCE
#include <sys/types.h>
#define _POSIX_SOURCE
#else
#include <sys/types.h>
#endif
#endif /* USG */


/*
 * Just about everyone needs the strings routines.  We provide both forms here,
 * index/rindex and strchr/strrchr, so any systems that don't provide them all
 * need to have #defines here.
 */

#if !defined(X_NOT_STDC_ENV)
#include <string.h>
#define index strchr
#define rindex strrchr
#else
#if defined(SYSV)
#include <string.h>
#define index strchr
#define rindex strrchr
#else
#include <strings.h>
#define strchr index
#define strrchr rindex
#endif
#endif


/*
 * Get open(2) constants
 */
#if defined(X_NOT_POSIX)
#include <fcntl.h>
#if defined(USL)
#include <unistd.h>
#endif /* USL */
#if defined(CRAY)
#include <unistd.h>
#endif /* CRAY */
#if defined(MOTOROLA)
#include <unistd.h>
#endif /* MOTOROLA */
#if defined(SYSV386)
#include <unistd.h>
#endif /* SYSV386 */
#include <sys/file.h>
#else /* X_NOT_POSIX */
#if !defined(_POSIX_SOURCE) && defined(macII)
#define _POSIX_SOURCE
#include <fcntl.h>
#undef _POSIX_SOURCE
#else
#include <fcntl.h>
#endif
#include <unistd.h>
#endif /* X_NOT_POSIX else */

/*
 * Get struct timeval
 */

#if defined(SYSV)

#if !defined(USL)
#include <sys/time.h>
#endif
#include <time.h>
#if defined(CRAY)
#undef word
#endif /* CRAY */
#if defined(USG) && !defined(CRAY) && !defined(MOTOROLA)
struct timeval {
    long tv_sec;
    long tv_usec;
};
#if !defined(USL_SHARELIB)
struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};
#endif /* USL_SHARELIB */
#endif /* USG */

#else /* not SYSV */

#if defined(_POSIX_SOURCE) && defined(SVR4)
/* need to omit _POSIX_SOURCE in order to get what we want in SVR4 */
#undef _POSIX_SOURCE
#include <sys/time.h>
#define _POSIX_SOURCE
#else
#include <sys/time.h>
#endif

#endif /* SYSV */

/* use POSIX name for signal */
#if defined(X_NOT_POSIX) && defined(SYSV) && !defined(SIGCHLD)
#define SIGCHLD SIGCLD
#endif

#if defined(ISC)
#include <sys/bsdtypes.h>
#endif
