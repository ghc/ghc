/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2001-2004
 *
 * Definitions for package `base' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __HSBASE_H__
#define __HSBASE_H__

#include "HsBaseConfig.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

/* Needed to get the macro version of errno on some OSs (eg. Solaris).
   We must do this, because these libs are only compiled once, but
   must work in both single-threaded and multi-threaded programs. */
#define _REENTRANT 1

#include "HsFFI.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if HAVE_TERMIOS_H
#include <termios.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
/* Ultra-ugly: OpenBSD uses broken macros for sigemptyset and sigfillset (missing casts) */
#if __OpenBSD__
#undef sigemptyset
#undef sigfillset
#endif
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_UTIME_H
#include <utime.h>
#endif
#if HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#if HAVE_GETTIMEOFDAY
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif HAVE_GETCLOCK
# if HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIMEB_H && !defined(__FreeBSD__)
#include <sys/timeb.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#if HAVE_WINSOCK_H && defined(_WIN32)
#include <winsock.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#elif HAVE_STDINT_H
# include <stdint.h>
#endif
#if HAVE_CLOCK_GETTIME
# ifdef _POSIX_MONOTONIC_CLOCK
#  define CLOCK_ID CLOCK_MONOTONIC
# else
#  define CLOCK_ID CLOCK_REALTIME
# endif
#elif defined(darwin_HOST_OS)
# include <mach/mach.h>
# include <mach/mach_time.h>
#endif

#if !defined(_WIN32) && !defined(irix_HOST_OS)
# if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
# endif
#endif

#if !HAVE_GETRUSAGE && HAVE_SYS_SYSCALL_H
# include <sys/syscall.h>
# if defined(SYS_GETRUSAGE)	/* hpux_HOST_OS */
#  define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#  define HAVE_GETRUSAGE 1
# endif
#endif

/* For System */
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#include "WCsubst.h"

#if defined(_WIN32)
/* in Win32Utils.c */
extern void maperrno (void);
extern int maperrno_func(DWORD dwErrorCode);
extern HsWord64 getMonotonicUSec(void);
#endif

#if defined(_WIN32)
#include <io.h>
#include <fcntl.h>
#include <shlobj.h>
#include <share.h>
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/* in inputReady.c */
extern int fdReady(int fd, int write, int msecs, int isSock);

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
# endif
#endif

INLINE int __hscore_get_errno(void) { return errno; }
INLINE void __hscore_set_errno(int e) { errno = e; }

INLINE HsInt
__hscore_bufsiz(void)
{
  return BUFSIZ;
}

INLINE int
__hscore_o_binary(void)
{
#if defined(_MSC_VER)
  return O_BINARY;
#else
  return CONST_O_BINARY;
#endif
}

INLINE int
__hscore_o_rdonly(void)
{
#ifdef O_RDONLY
  return O_RDONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_wronly( void )
{
#ifdef O_WRONLY
  return O_WRONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_rdwr( void )
{
#ifdef O_RDWR
  return O_RDWR;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_append( void )
{
#ifdef O_APPEND
  return O_APPEND;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_creat( void )
{
#ifdef O_CREAT
  return O_CREAT;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_excl( void )
{
#ifdef O_EXCL
  return O_EXCL;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_trunc( void )
{
#ifdef O_TRUNC
  return O_TRUNC;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_noctty( void )
{
#ifdef O_NOCTTY
  return O_NOCTTY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_nonblock( void )
{
#ifdef O_NONBLOCK
  return O_NONBLOCK;
#else
  return 0;
#endif
}

INLINE int
__hscore_ftruncate( int fd, off_t where )
{
#if defined(HAVE_FTRUNCATE)
  return ftruncate(fd,where);
#elif defined(HAVE__CHSIZE)
  return _chsize(fd,where);
#else
// ToDo: we should use _chsize_s() on Windows which allows a 64-bit
// offset, but it doesn't seem to be available from mingw at this time 
// --SDM (01/2008)
#error at least ftruncate or _chsize functions are required to build
#endif
}

INLINE int
__hscore_setmode( int fd, HsBool toBin )
{
#if defined(_WIN32)
  return _setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif
}

#if defined(_WIN32)
// We want the versions of stat/fstat/lseek that use 64-bit offsets,
// and you have to ask for those explicitly.  Unfortunately there
// doesn't seem to be a 64-bit version of truncate/ftruncate, so while
// hFileSize and hSeek will work with large files, hSetFileSize will not.
typedef struct _stati64 struct_stat;
typedef off64_t stsize_t;
#else
typedef struct stat struct_stat;
typedef off_t stsize_t;
#endif

INLINE HsInt
__hscore_sizeof_stat( void )
{
  return sizeof(struct_stat);
}

INLINE time_t __hscore_st_mtime ( struct_stat* st ) { return st->st_mtime; }
INLINE stsize_t __hscore_st_size  ( struct_stat* st ) { return st->st_size; }
#if !defined(_MSC_VER)
INLINE mode_t __hscore_st_mode  ( struct_stat* st ) { return st->st_mode; }
INLINE dev_t  __hscore_st_dev  ( struct_stat* st ) { return st->st_dev; }
INLINE ino_t  __hscore_st_ino  ( struct_stat* st ) { return st->st_ino; }
#endif

#if defined(_WIN32)
INLINE int __hscore_stat(wchar_t *file, struct_stat *buf) {
	return _wstati64(file,buf);
}

INLINE int __hscore_fstat(int fd, struct_stat *buf) {
	return _fstati64(fd,buf);
}
INLINE int __hscore_lstat(wchar_t *fname, struct_stat *buf )
{
	return _wstati64(fname,buf);
}
#else
INLINE int __hscore_stat(char *file, struct_stat *buf) {
	return stat(file,buf);
}

INLINE int __hscore_fstat(int fd, struct_stat *buf) {
	return fstat(fd,buf);
}

INLINE int __hscore_lstat( const char *fname, struct stat *buf )
{
#if HAVE_LSTAT
  return lstat(fname, buf);
#else
  return stat(fname, buf);
#endif
}
#endif

#if HAVE_TERMIOS_H
INLINE tcflag_t __hscore_lflag( struct termios* ts ) { return ts->c_lflag; }

INLINE void
__hscore_poke_lflag( struct termios* ts, tcflag_t t ) { ts->c_lflag = t; }

INLINE unsigned char*
__hscore_ptr_c_cc( struct termios* ts )
{ return (unsigned char*) &ts->c_cc; }

INLINE HsInt
__hscore_sizeof_termios( void )
{
#ifndef _WIN32
  return sizeof(struct termios);
#else
  return 0;
#endif
}
#endif

#if !defined(_WIN32)
INLINE HsInt
__hscore_sizeof_sigset_t( void )
{
  return sizeof(sigset_t);
}
#endif

INLINE int
__hscore_echo( void )
{
#ifdef ECHO
  return ECHO;
#else
  return 0;
#endif

}

INLINE int
__hscore_tcsanow( void )
{
#ifdef TCSANOW
  return TCSANOW;
#else
  return 0;
#endif

}

INLINE int
__hscore_icanon( void )
{
#ifdef ICANON
  return ICANON;
#else
  return 0;
#endif
}

INLINE int __hscore_vmin( void )
{
#ifdef VMIN
  return VMIN;
#else
  return 0;
#endif
}

INLINE int __hscore_vtime( void )
{
#ifdef VTIME
  return VTIME;
#else
  return 0;
#endif
}

INLINE int __hscore_sigttou( void )
{
#ifdef SIGTTOU
  return SIGTTOU;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_block( void )
{
#ifdef SIG_BLOCK
  return SIG_BLOCK;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_setmask( void )
{
#ifdef SIG_SETMASK
  return SIG_SETMASK;
#else
  return 0;
#endif
}

#ifndef _WIN32
INLINE size_t __hscore_sizeof_siginfo_t (void)
{
    return sizeof(siginfo_t);
}
#endif

INLINE int
__hscore_f_getfl( void )
{
#ifdef F_GETFL
  return F_GETFL;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_setfl( void )
{
#ifdef F_SETFL
  return F_SETFL;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_setfd( void )
{
#ifdef F_SETFD
  return F_SETFD;
#else
  return 0;
#endif
}

INLINE long
__hscore_fd_cloexec( void )
{
#ifdef FD_CLOEXEC
  return FD_CLOEXEC;
#else
  return 0;
#endif
}

// defined in rts/RtsStartup.c.
extern void* __hscore_get_saved_termios(int fd);
extern void __hscore_set_saved_termios(int fd, void* ts);

#ifdef _WIN32
INLINE int __hscore_open(wchar_t *file, int how, mode_t mode) {
	if ((how & O_WRONLY) || (how & O_RDWR) || (how & O_APPEND))
	  return _wsopen(file,how | _O_NOINHERIT,_SH_DENYNO,mode);
          // _O_NOINHERIT: see #2650
	else
	  return _wsopen(file,how | _O_NOINHERIT,_SH_DENYNO,mode);
          // _O_NOINHERIT: see #2650
}
#else
INLINE int __hscore_open(char *file, int how, mode_t mode) {
	return open(file,how,mode);
}
#endif

#if darwin_HOST_OS
// You should not access _environ directly on Darwin in a bundle/shared library.
// See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
#include <crt_externs.h>
INLINE char **__hscore_environ(void) { return *(_NSGetEnviron()); }
#else
/* ToDo: write a feature test that doesn't assume 'environ' to
 *    be in scope at link-time. */
extern char** environ;
INLINE char **__hscore_environ(void) { return environ; }
#endif

/* lossless conversions between pointers and integral types */
INLINE void *    __hscore_from_uintptr(uintptr_t n) { return (void *)n; }
INLINE void *    __hscore_from_intptr (intptr_t n)  { return (void *)n; }
INLINE uintptr_t __hscore_to_uintptr  (void *p)     { return (uintptr_t)p; }
INLINE intptr_t  __hscore_to_intptr   (void *p)     { return (intptr_t)p; }

void errorBelch2(const char*s, char *t);
void debugBelch2(const char*s, char *t);

#endif /* __HSBASE_H__ */

