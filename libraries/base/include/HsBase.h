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
#if HAVE_DIRENT_H
#include <dirent.h>
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
#if HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#if HAVE_WINSOCK_H && defined(__MINGW32__)
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

#if !defined(__MINGW32__) && !defined(irix_HOST_OS)
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
#include "lockFile.h"
#include "dirUtils.h"
#include "WCsubst.h"

#include "runProcess.h"

#if defined(__MINGW32__)
/* in Win32Utils.c */
extern void maperrno (void);
extern HsWord64 getUSecOfDay(void);
#endif

#if defined(__MINGW32__)
#include <io.h>
#include <fcntl.h>
#include "timeUtils.h"
#include <shlobj.h>
#include <share.h>
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/* in inputReady.c */
int inputReady(int fd, int msecs, int isSock);

/* in Signals.c */
extern HsInt nocldstop;

#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
/* in execvpe.c */
extern int execvpe(char *name, char *const argv[], char **envp);
extern void pPrPr_disableITimers (void);
#endif

/* -----------------------------------------------------------------------------
   64-bit operations, defined in longlong.c
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

StgInt stg_gtWord64 (StgWord64, StgWord64);
StgInt stg_geWord64 (StgWord64, StgWord64);
StgInt stg_eqWord64 (StgWord64, StgWord64);
StgInt stg_neWord64 (StgWord64, StgWord64);
StgInt stg_ltWord64 (StgWord64, StgWord64);
StgInt stg_leWord64 (StgWord64, StgWord64);

StgInt stg_gtInt64 (StgInt64, StgInt64);
StgInt stg_geInt64 (StgInt64, StgInt64);
StgInt stg_eqInt64 (StgInt64, StgInt64);
StgInt stg_neInt64 (StgInt64, StgInt64);
StgInt stg_ltInt64 (StgInt64, StgInt64);
StgInt stg_leInt64 (StgInt64, StgInt64);

StgWord64 stg_remWord64  (StgWord64, StgWord64);
StgWord64 stg_quotWord64 (StgWord64, StgWord64);

StgInt64 stg_remInt64    (StgInt64, StgInt64);
StgInt64 stg_quotInt64   (StgInt64, StgInt64);
StgInt64 stg_negateInt64 (StgInt64);
StgInt64 stg_plusInt64   (StgInt64, StgInt64);
StgInt64 stg_minusInt64  (StgInt64, StgInt64);
StgInt64 stg_timesInt64  (StgInt64, StgInt64);

StgWord64 stg_and64  (StgWord64, StgWord64);
StgWord64 stg_or64   (StgWord64, StgWord64);
StgWord64 stg_xor64  (StgWord64, StgWord64);
StgWord64 stg_not64  (StgWord64);

StgWord64 stg_uncheckedShiftL64   (StgWord64, StgInt);
StgWord64 stg_uncheckedShiftRL64  (StgWord64, StgInt);
StgInt64  stg_uncheckedIShiftL64  (StgInt64, StgInt);
StgInt64  stg_uncheckedIShiftRL64 (StgInt64, StgInt);
StgInt64  stg_uncheckedIShiftRA64 (StgInt64, StgInt);

StgInt64  stg_intToInt64    (StgInt);
StgInt    stg_int64ToInt    (StgInt64);
StgWord64 stg_int64ToWord64 (StgInt64);

StgWord64 stg_wordToWord64  (StgWord);
StgWord   stg_word64ToWord  (StgWord64);
StgInt64  stg_word64ToInt64 (StgWord64);

StgInt64  stg_integerToInt64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da);
StgWord64 stg_integerToWord64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da);

#endif /* SUPPORT_LONG_LONGS */

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

#if !defined(_MSC_VER)
INLINE int __hscore_s_isreg(m)  { return S_ISREG(m);  }
INLINE int __hscore_s_isdir(m)  { return S_ISDIR(m);  }
INLINE int __hscore_s_isfifo(m) { return S_ISFIFO(m); }
INLINE int __hscore_s_isblk(m)  { return S_ISBLK(m);  }
INLINE int __hscore_s_ischr(m)  { return S_ISCHR(m);  }
#ifdef S_ISSOCK
INLINE int __hscore_s_issock(m) { return S_ISSOCK(m); }
#endif
#endif

#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
INLINE int
__hscore_sigemptyset( sigset_t *set )
{ return sigemptyset(set); }

INLINE int
__hscore_sigfillset( sigset_t *set )
{ return sigfillset(set); }

INLINE int
__hscore_sigaddset( sigset_t * set, int s )
{ return sigaddset(set,s); }

INLINE int
__hscore_sigdelset( sigset_t * set, int s )
{ return sigdelset(set,s); }

INLINE int
__hscore_sigismember( sigset_t * set, int s )
{ return sigismember(set,s); }
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
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
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
#if defined(_MSC_VER)
  return O_BINARY;
#else
  return CONST_O_BINARY;
#endif
}

INLINE int
__hscore_o_rdonly()
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

INLINE HsInt
__hscore_seek_set( void )
{
  return SEEK_SET;
}

INLINE HsInt
__hscore_seek_end( void )
{
  return SEEK_END;
}

INLINE int
__hscore_ftruncate( int fd, off_t where )
{
#if defined(HAVE_FTRUNCATE)
  return ftruncate(fd,where);
#elif defined(HAVE__CHSIZE)
  return _chsize(fd,where);
#else
#error at least ftruncate or _chsize functions are required to build
#endif
}

INLINE HsInt
__hscore_setmode( HsInt fd, HsBool toBin )
{
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
  return setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif
}

#if __GLASGOW_HASKELL__

INLINE HsInt
__hscore_PrelHandle_write( HsInt fd, HsAddr ptr, HsInt off, int sz )
{
  return write(fd,(char *)ptr + off, sz);
}

INLINE HsInt
__hscore_PrelHandle_read( HsInt fd, HsAddr ptr, HsInt off, int sz )
{
  return read(fd,(char *)ptr + off, sz);

}

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
INLINE HsInt
__hscore_PrelHandle_send( HsInt fd, HsAddr ptr, HsInt off, int sz )
{
    return send(fd,(char *)ptr + off, sz, 0);
}

INLINE HsInt
__hscore_PrelHandle_recv( HsInt fd, HsAddr ptr, HsInt off, int sz )
{
    return recv(fd,(char *)ptr + off, sz, 0);
}
#endif

#endif /* __GLASGOW_HASKELL__ */

INLINE HsInt
__hscore_mkdir( HsAddr pathName, HsInt mode )
{
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
  return mkdir(pathName);
#else
  return mkdir(pathName,mode);
#endif
}

INLINE HsInt
__hscore_lstat( HsAddr fname, HsAddr st )
{
#if HAVE_LSTAT
  return lstat((const char*)fname, (struct stat*)st);
#else
  return stat((const char*)fname, (struct stat*)st);
#endif
}

#ifdef PATH_MAX
/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
INLINE HsInt __hscore_long_path_size() { return PATH_MAX; }
#else
INLINE HsInt __hscore_long_path_size() { return 4096; }
#endif

#ifdef R_OK
INLINE mode_t __hscore_R_OK() { return R_OK; }
#endif
#ifdef W_OK
INLINE mode_t __hscore_W_OK() { return W_OK; }
#endif
#ifdef X_OK
INLINE mode_t __hscore_X_OK() { return X_OK; }
#endif

#ifdef S_IRUSR
INLINE mode_t __hscore_S_IRUSR() { return S_IRUSR; }
#endif
#ifdef S_IWUSR
INLINE mode_t __hscore_S_IWUSR() { return S_IWUSR; }
#endif
#ifdef S_IXUSR
INLINE mode_t __hscore_S_IXUSR() { return S_IXUSR; }
#endif

INLINE HsAddr
__hscore_d_name( struct dirent* d )
{
  return (HsAddr)(d->d_name);
}

INLINE HsInt
__hscore_end_of_dir( void )
{
  return READDIR_ERRNO_EOF;
}

INLINE void
__hscore_free_dirent(HsAddr dEnt)
{
#if HAVE_READDIR_R
  free(dEnt);
#endif
}

INLINE HsInt
__hscore_sizeof_stat( void )
{
  return sizeof(struct stat);
}

INLINE time_t __hscore_st_mtime ( struct stat* st ) { return st->st_mtime; }
INLINE off_t  __hscore_st_size  ( struct stat* st ) { return st->st_size; }
#if !defined(_MSC_VER)
INLINE mode_t __hscore_st_mode  ( struct stat* st ) { return st->st_mode; }
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
#ifndef __MINGW32__
  return sizeof(struct termios);
#else
  return 0;
#endif
}
#endif

#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
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

// defined in rts/RtsStartup.c.
extern void* __hscore_get_saved_termios(int fd);
extern void __hscore_set_saved_termios(int fd, void* ts);

INLINE int __hscore_hs_fileno (FILE *f) { return fileno (f); }

INLINE int __hscore_open(char *file, int how, mode_t mode) {
#ifdef __MINGW32__
	if ((how & O_WRONLY) || (how & O_RDWR) || (how & O_APPEND))
	  return _sopen(file,how,_SH_DENYRW,mode);
	else
	  return _sopen(file,how,_SH_DENYWR,mode);
#else
	return open(file,how,mode);
#endif
}

// These are wrapped because on some OSs (eg. Linux) they are
// macros which redirect to the 64-bit-off_t versions when large file
// support is enabled.
//
INLINE off_t __hscore_lseek(int fd, off_t off, int whence) {
	return (lseek(fd,off,whence));
}

INLINE int __hscore_stat(char *file, struct stat *buf) {
	return (stat(file,buf));
}

INLINE int __hscore_fstat(int fd, struct stat *buf) {
	return (fstat(fd,buf));
}

// select-related stuff

#if !defined(__MINGW32__)
INLINE int  hsFD_SETSIZE(void) { return FD_SETSIZE; }
INLINE void hsFD_CLR(int fd, fd_set *fds) { FD_CLR(fd, fds); }
INLINE int  hsFD_ISSET(int fd, fd_set *fds) { return FD_ISSET(fd, fds); }
INLINE void hsFD_SET(int fd, fd_set *fds) { FD_SET(fd, fds); }
INLINE int  sizeof_fd_set(void) { return sizeof(fd_set); }
extern void hsFD_ZERO(fd_set *fds);
#endif

// gettimeofday()-related

#if !defined(__MINGW32__)

INLINE HsInt sizeofTimeVal(void) { return sizeof(struct timeval); }

INLINE HsWord64 getUSecOfDay(void)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    // Don't forget to cast *before* doing the arithmetic, otherwise
    // the arithmetic happens at the type of tv_sec, which is probably
    // only 'int'.
    return ((HsWord64)tv.tv_sec * 1000000 + (HsWord64)tv.tv_usec);
}

INLINE void setTimevalTicks(struct timeval *p, HsWord64 usecs)
{
    p->tv_sec  = usecs / 1000000;
    p->tv_usec = usecs % 1000000;
}
#endif /* !defined(__MINGW32__) */

// Directory-related

#if defined(__MINGW32__)

/* Make sure we've got the reqd CSIDL_ constants in scope;
 * w32api header files are lagging a bit in defining the full set.
 */
#if !defined(CSIDL_APPDATA)
#define CSIDL_APPDATA 0x001a
#endif
#if !defined(CSIDL_PERSONAL)
#define CSIDL_PERSONAL 0x0005
#endif
#if !defined(CSIDL_PROFILE)
#define CSIDL_PROFILE 0x0028
#endif
#if !defined(CSIDL_WINDOWS)
#define CSIDL_WINDOWS 0x0024
#endif

INLINE int __hscore_CSIDL_PROFILE()  { return CSIDL_PROFILE;  }
INLINE int __hscore_CSIDL_APPDATA()  { return CSIDL_APPDATA;  }
INLINE int __hscore_CSIDL_WINDOWS()  { return CSIDL_WINDOWS;  }
INLINE int __hscore_CSIDL_PERSONAL() { return CSIDL_PERSONAL; }
#endif

#if defined(__MINGW32__)
INLINE unsigned int __hscore_get_osver(void) { return _osver; }
#endif

/* ToDo: write a feature test that doesn't assume 'environ' to
 *    be in scope at link-time. */
extern char** environ;
INLINE char **__hscore_environ() { return environ; }

/* lossless conversions between pointers and integral types */
INLINE void *    __hscore_from_uintptr(uintptr_t n) { return (void *)n; }
INLINE void *    __hscore_from_intptr (intptr_t n)  { return (void *)n; }
INLINE uintptr_t __hscore_to_uintptr  (void *p)     { return (uintptr_t)p; }
INLINE intptr_t  __hscore_to_intptr   (void *p)     { return (intptr_t)p; }

#endif /* __HSBASE_H__ */

