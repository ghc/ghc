/* -----------------------------------------------------------------------------
 * $Id: HsCore.h,v 1.6 2002/01/02 14:40:11 simonmar Exp $
 *
 * (c) The University of Glasgow 2001-2002
 *
 * Definitions for package `core' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSCORE_H
#define HSCORE_H

#include "config.h"
#include "HsFFI.h"

#include <stdio.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#if defined(HAVE_GETTIMEOFDAY)
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif defined(HAVE_GETCLOCK)
# ifdef HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if defined(HAVE_TIME_H)
# include <time.h>
#endif
#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_WINSOCK_H
#include <winsock.h>
#endif

#if !defined(mingw32_TARGET_OS) && !defined(irix_TARGET_OS)
# if defined(HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
# endif
#endif

#ifdef hpux_TARGET_OS
#include <sys/syscall.h>
#define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#define HAVE_GETRUSAGE
#endif

/* For System */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
#include "lockFile.h"
#include "dirUtils.h"
#include "errUtils.h"

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

/* in ghc_errno.c */
int *ghcErrno(void);

/* in system.c */
HsInt systemCmd(HsAddr cmd);

/* in inputReady.c */
int inputReady(int fd, int msecs, int isSock);

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#ifndef INLINE
#define INLINE extern inline
#endif

INLINE int __hscore_s_isreg(m)  { return S_ISREG(m);  }
INLINE int __hscore_s_isdir(m)  { return S_ISDIR(m);  }
INLINE int __hscore_s_isfifo(m) { return S_ISFIFO(m); }
INLINE int __hscore_s_isblk(m)  { return S_ISBLK(m);  }
INLINE int __hscore_s_ischr(m)  { return S_ISCHR(m);  }
#ifdef S_ISSOCK
INLINE int __hscore_s_issock(m) { return S_ISSOCK(m); }
#endif

#ifndef mingw32_TARGET_OS
INLINE void
__hscore_sigemptyset( sigset_t *set )
{ sigemptyset(set); }
#endif

INLINE void *
__hscore_memcpy_dst_off( char *dst, int dst_off, char *src, size_t sz )
{ return memcpy(dst+dst_off, src, sz); }

INLINE void *
__hscore_memcpy_src_off( char *dst, char *src, int src_off, size_t sz )
{ return memcpy(dst, src+src_off, sz); }

INLINE HsBool
__hscore_supportsTextMode()
{
#if defined(mingw32_TARGET_OS)
  return HS_BOOL_FALSE;
#else
  return HS_BOOL_TRUE;
#endif
}

INLINE HsInt
__hscore_bufsiz()
{
  return BUFSIZ;
}

INLINE HsInt
__hscore_seek_cur()
{
  return SEEK_CUR;
}

INLINE HsInt
__hscore_o_binary()
{
#ifdef HAVE_O_BINARY
  return O_BINARY;
#else
  return 0;
#endif
}

INLINE HsInt
__hscore_seek_set()
{
  return SEEK_SET;
}

INLINE HsInt
__hscore_seek_end()
{
  return SEEK_END;
}

INLINE HsInt
__hscore_setmode( HsInt fd, HsBool toBin )
{
#ifdef _WIN32
  return setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif  
}

INLINE HsInt
__hscore_PrelHandle_write( HsInt fd, HsBool isSock, HsAddr ptr, 
			   HsInt off, int sz )
{
#ifdef _WIN32
  if (isSock) {
    return send(fd,ptr + off, sz, 0);
  }
#endif
  return write(fd,ptr + off, sz);
}

INLINE HsInt
__hscore_PrelHandle_read( HsInt fd, HsBool isSock, HsAddr ptr, 
			  HsInt off, int sz )
{
#ifdef _WIN32
  if (isSock) {
    return recv(fd,ptr + off, sz, 0);
  }
#endif
  return read(fd,ptr + off, sz);

}

#endif

