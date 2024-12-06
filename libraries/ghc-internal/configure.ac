AC_PREREQ([2.60])
AC_INIT([Haskell ghc-internal package], [1.0], [libraries@haskell.org], [ghc-internal])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsBase.h])

AC_CONFIG_HEADERS([include/HsBaseConfig.h include/EventConfig.h])

AC_PROG_CC
dnl make extensions visible to allow feature-tests to detect them later on
AC_USE_SYSTEM_EXTENSIONS

AC_MSG_CHECKING(for WINDOWS platform)
case $host_alias in
    *mingw32*|*mingw64*|*cygwin*|*msys*|*windows*)
        WINDOWS=YES;;
    *)
        WINDOWS=NO;;
esac
AC_MSG_RESULT($WINDOWS)

# do we have long longs?
AC_CHECK_TYPES([long long])

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([ctype.h errno.h fcntl.h inttypes.h limits.h signal.h sys/file.h sys/resource.h sys/select.h sys/stat.h sys/syscall.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/types.h sys/utsname.h sys/wait.h termios.h time.h unistd.h utime.h windows.h winsock.h langinfo.h poll.h sys/epoll.h sys/event.h sys/eventfd.h sys/socket.h])

# Enable large file support. Do this before testing the types ino_t, off_t, and
# rlim_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** check for wide-char classifications
dnl FreeBSD has an empty wctype.h, so test one of the affected
dnl functions if it's really there.
AC_CHECK_HEADERS([wctype.h], [AC_CHECK_FUNCS(iswspace)])

AC_CHECK_FUNCS([lstat])
AC_CHECK_LIB([rt], [clock_gettime])
AC_CHECK_FUNCS([clock_gettime])
AC_CHECK_DECLS([CLOCK_PROCESS_CPUTIME_ID], [], [], [[#include <time.h>]])
AC_CHECK_FUNCS([getclock getrusage times])
AC_CHECK_FUNCS([_chsize_s ftruncate])

AC_CHECK_FUNCS([strerror_r strerror_s])

if test "$ac_cv_func_strerror_r" = no && test "$ac_cv_func_strerror_s" = no; then
  AC_MSG_ERROR([Either strerror_r or strerror_s must be available])
fi

# event-related fun
# The line below already defines HAVE_KQUEUE and HAVE_POLL, so technically some of the
# subsequent portions that redefine them could be skipped. However, we keep those portions
# to keep kqueue/poll in line with HAVE_EPOLL and possible other additions in the future. You
# should be aware of this peculiarity if you try to simulate not having kqueue or poll by
# moving away header files (see also https://gitlab.haskell.org/ghc/ghc/-/issues/9283)
AC_CHECK_FUNCS([epoll_ctl eventfd kevent kevent64 kqueue poll])

if test "$ac_cv_header_sys_epoll_h" = yes && test "$ac_cv_func_epoll_ctl" = yes; then
  AC_DEFINE([HAVE_EPOLL], [1], [Define if you have epoll support.])
fi

if test "$ac_cv_header_sys_event_h" = yes && test "$ac_cv_func_kqueue" = yes; then
  AC_DEFINE([HAVE_KQUEUE], [1], [Define if you have kqueue support.])

  AC_CHECK_SIZEOF([kev.filter], [], [#include <sys/event.h>
struct kevent kev;])

  AC_CHECK_SIZEOF([kev.flags], [], [#include <sys/event.h>
struct kevent kev;])
fi

if test "$ac_cv_header_poll_h" = yes && test "$ac_cv_func_poll" = yes; then
  AC_DEFINE([HAVE_POLL], [1], [Define if you have poll support.])
fi

# Linux open file descriptor locks
AC_CHECK_DECL([F_OFD_SETLK], [
  AC_DEFINE([HAVE_OFD_LOCKING], [1], [Define if you have open file descriptor lock support.])
], [], [
  #include <unistd.h>
  #include <fcntl.h>
])

# flock
AC_CHECK_FUNCS([flock])
if test "$ac_cv_header_sys_file_h" = yes && test "$ac_cv_func_flock" = yes; then
  AC_DEFINE([HAVE_FLOCK], [1], [Define if you have flock support.])
fi

# unsetenv
AC_CHECK_FUNCS([unsetenv])

AC_CHECK_FUNCS([chmod])
AC_CHECK_FUNCS([dup])
AC_CHECK_FUNCS([fork])
AC_CHECK_FUNCS([getpid])
AC_CHECK_FUNCS([mkfifo])
AC_CHECK_FUNCS([pipe])
AC_CHECK_FUNCS([umask])

AC_CHECK_TYPE([struct rlimit],[AC_DEFINE([HAVE_STRUCT_RLIMIT],[1],[HAVE_STRUCT_RLIMIT])],[],[#include <sys/resource.h>])

###  POSIX.1003.1 unsetenv returns 0 or -1 (EINVAL), but older implementations
###  in common use return void.
AC_CACHE_CHECK([return type of unsetenv], fptools_cv_func_unsetenv_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+unsetenv>changequote([, ]),
                   stdlib.h,
                   [fptools_cv_func_unsetenv_return_type=void],
                   [fptools_cv_func_unsetenv_return_type=int])])
case "$fptools_cv_func_unsetenv_return_type" in
  "void" )
    AC_DEFINE([UNSETENV_RETURNS_VOID], [1], [Define if stdlib.h declares unsetenv to return void.])
  ;;
esac

dnl--------------------------------------------------------------------
dnl * Deal with arguments telling us iconv is somewhere odd
dnl--------------------------------------------------------------------

AC_ARG_WITH([iconv-includes],
  [AS_HELP_STRING([--with-iconv-includes],
    [directory containing iconv.h])],
    [ICONV_INCLUDE_DIRS=$withval; CPPFLAGS="-I$withval $CPPFLAGS"],
    [ICONV_INCLUDE_DIRS=])

AC_ARG_WITH([iconv-libraries],
  [AS_HELP_STRING([--with-iconv-libraries],
    [directory containing iconv library])],
    [ICONV_LIB_DIRS=$withval; LDFLAGS="-L$withval $LDFLAGS"],
    [ICONV_LIB_DIRS=])

AC_SUBST(ICONV_INCLUDE_DIRS)
AC_SUBST(ICONV_LIB_DIRS)

dnl--------------------------------------------------------------------
dnl * Deal with arguments telling us gmp is somewhere odd
dnl--------------------------------------------------------------------

AC_ARG_WITH([gmp],
  [AS_HELP_STRING([--with-gmp],
    [Enable GMP backend])],
    [GMP_ENABLED=YES],
    [GMP_ENABLED=NO])

AC_ARG_WITH([gmp-includes],
  [AS_HELP_STRING([--with-gmp-includes],
    [directory containing gmp.h])],
    [GMP_INCLUDE_DIRS=$withval; CPPFLAGS="-I$withval"],
    [GMP_INCLUDE_DIRS=])

AC_ARG_WITH([gmp-libraries],
  [AS_HELP_STRING([--with-gmp-libraries],
    [directory containing gmp library])],
    [GMP_LIB_DIRS=$withval; LDFLAGS="-L$withval"],
    [GMP_LIB_DIRS=])

AC_ARG_WITH([gmp-framework-preferred],
  [AS_HELP_STRING([--with-gmp-framework-preferred],
    [on OSX, prefer the GMP framework to the gmp lib])],
    [GMP_PREFER_FRAMEWORK=YES],
    [GMP_PREFER_FRAMEWORK=NO])

AC_ARG_WITH([intree-gmp],
  [AS_HELP_STRING([--with-intree-gmp],
    [force using the in-tree GMP])],
    [GMP_FORCE_INTREE=YES],
    [GMP_FORCE_INTREE=NO])

if test "$GMP_ENABLED" = "YES"
then

dnl--------------------------------------------------------------------
dnl * Detect gmp
dnl--------------------------------------------------------------------

   HaveLibGmp=NO
   GMP_LIBS=
   HaveFrameworkGMP=NO
   GMP_FRAMEWORK=
   HaveSecurePowm=0

   if test "$GMP_FORCE_INTREE" != "YES"
   then
       if test "$GMP_PREFER_FRAMEWORK" = "YES"
       then
           LOOK_FOR_GMP_FRAMEWORK
           LOOK_FOR_GMP_LIB
       else
           LOOK_FOR_GMP_LIB
           LOOK_FOR_GMP_FRAMEWORK
       fi
   fi

   AC_MSG_CHECKING([whether to use in-tree GMP])
   if test "$HaveFrameworkGMP" = "YES" || test "$HaveLibGmp" = "YES"
   then
       AC_MSG_RESULT([no])
       UseIntreeGmp=0
       AC_CHECK_HEADER([gmp.h], , [AC_MSG_ERROR([Cannot find gmp.h])])

       AC_MSG_CHECKING([GMP version])
       AC_COMPUTE_INT(GhcGmpVerMj, __GNU_MP_VERSION, [#include <gmp.h>],
           AC_MSG_ERROR([Unable to get value of __GNU_MP_VERSION]))
       AC_COMPUTE_INT(GhcGmpVerMi, __GNU_MP_VERSION_MINOR, [#include <gmp.h>],
           AC_MSG_ERROR([Unable to get value of __GNU_MP_VERSION_MINOR]))
       AC_COMPUTE_INT(GhcGmpVerPl, __GNU_MP_VERSION_PATCHLEVEL, [#include <gmp.h>],
           AC_MSG_ERROR([Unable to get value of __GNU_MP_VERSION_PATCHLEVEL]))
       AC_MSG_RESULT([$GhcGmpVerMj.$GhcGmpVerMi.$GhcGmpVerPl])

   else
       AC_MSG_RESULT([yes])
       UseIntreeGmp=1
       HaveSecurePowm=1

       AC_MSG_CHECKING([GMP version])
       GhcGmpVerMj=6
       GhcGmpVerMi=1
       GhcGmpVerPl=2
       AC_MSG_RESULT([$GhcGmpVerMj.$GhcGmpVerMi.$GhcGmpVerPl])
   fi

   GMP_INSTALL_INCLUDES="HsIntegerGmp.h ghc-gmp.h"
fi


AC_SUBST(GMP_INCLUDE_DIRS)
AC_SUBST(GMP_LIBS)
AC_SUBST(GMP_LIB_DIRS)
AC_SUBST(GMP_FRAMEWORK)
AC_SUBST(GMP_INSTALL_INCLUDES)
AC_SUBST(HaveLibGmp)
AC_SUBST(HaveFrameworkGMP)
AC_SUBST(HaveSecurePowm)
AC_SUBST(UseIntreeGmp)
AC_SUBST(GhcGmpVerMj)
AC_SUBST(GhcGmpVerMi)
AC_SUBST(GhcGmpVerPl)

# Compute offsets/sizes used by jsbits/base.js
if test "$host" = "javascript-ghcjs"
then
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_MODE],    [stat], [st_mode],    [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_DEV],     [stat], [st_dev],     [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_UID],     [stat], [st_uid],     [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_GID],     [stat], [st_gid],     [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_NLINK],   [stat], [st_nlink],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_RDEV],    [stat], [st_rdev],    [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_SIZE],    [stat], [st_size],    [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_BLKSIZE], [stat], [st_blksize], [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_BLOCKS],  [stat], [st_blocks],  [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_INO],     [stat], [st_ino],     [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_ATIME],   [stat], [st_atime],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_ATIM_TV_NSEC],   [stat], [st_atim.tv_nsec],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_MTIME],   [stat], [st_mtime],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_MTIM_TV_NSEC],   [stat], [st_mtim.tv_nsec],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_CTIME],   [stat], [st_ctime],   [#include <sys/stat.h>])
  FP_COMPUTE_OFFSET([OFFSET_STAT_ST_CTIM_TV_NSEC],   [stat], [st_ctim.tv_nsec],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_MODE],    [stat], [st_mode],    [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_DEV],     [stat], [st_dev],     [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_UID],     [stat], [st_uid],     [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_GID],     [stat], [st_gid],     [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_NLINK],   [stat], [st_nlink],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_RDEV],    [stat], [st_rdev],    [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_SIZE],    [stat], [st_size],    [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_BLKSIZE], [stat], [st_blksize], [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_BLOCKS],  [stat], [st_blocks],  [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_INO],     [stat], [st_ino],     [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_ATIME],   [stat], [st_atime],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_ATIM_TV_NSEC], [stat], [st_atim.tv_nsec],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_MTIME],   [stat], [st_mtime],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_MTIM_TV_NSEC],   [stat], [st_mtim.tv_nsec],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_CTIME],   [stat], [st_ctime],   [#include <sys/stat.h>])
  FP_COMPUTE_SIZE([SIZEOF_STAT_ST_CTIM_TV_NSEC],   [stat], [st_ctim.tv_nsec],   [#include <sys/stat.h>])
  AC_CHECK_SIZEOF([struct stat], [], [#include <sys/stat.h>])

  FP_COMPUTE_OFFSET([OFFSET_UTIMBUF_ACTIME],  [utimbuf], [actime],  [#include <utime.h>])
  FP_COMPUTE_OFFSET([OFFSET_UTIMBUF_MODTIME], [utimbuf], [modtime], [#include <utime.h>])
  FP_COMPUTE_SIZE([SIZEOF_UTIMBUF_ACTIME],  [utimbuf], [actime],  [#include <utime.h>])
  FP_COMPUTE_SIZE([SIZEOF_UTIMBUF_MODTIME], [utimbuf], [modtime], [#include <utime.h>])
  AC_CHECK_SIZEOF([struct utimbuf], [], [#include <utime.h>])
fi

# map standard C types and ISO types to Haskell types
FPTOOLS_CHECK_HTYPE(char)
FPTOOLS_CHECK_HTYPE(signed char)
FPTOOLS_CHECK_HTYPE(unsigned char)
FPTOOLS_CHECK_HTYPE(short)
FPTOOLS_CHECK_HTYPE(unsigned short)
FPTOOLS_CHECK_HTYPE(int)
FPTOOLS_CHECK_HTYPE(unsigned int)
FPTOOLS_CHECK_HTYPE(long)
FPTOOLS_CHECK_HTYPE(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FPTOOLS_CHECK_HTYPE(long long)
FPTOOLS_CHECK_HTYPE(unsigned long long)
fi
FPTOOLS_CHECK_HTYPE(bool)
FPTOOLS_CHECK_HTYPE(float)
FPTOOLS_CHECK_HTYPE(double)
FPTOOLS_CHECK_HTYPE(ptrdiff_t)
FPTOOLS_CHECK_HTYPE(size_t)
FPTOOLS_CHECK_HTYPE(wchar_t)
FPTOOLS_CHECK_HTYPE(sig_atomic_t)
FPTOOLS_CHECK_HTYPE(clock_t)
FPTOOLS_CHECK_HTYPE(time_t)
FPTOOLS_CHECK_HTYPE(useconds_t)
FPTOOLS_CHECK_HTYPE_ELSE(suseconds_t,
                         [if test "$WINDOWS" = "YES"
                          then
                              AC_CV_NAME=Int32
                              AC_CV_NAME_supported=yes
                          else
                              AC_MSG_ERROR([type not found])
                          fi])
FPTOOLS_CHECK_HTYPE(dev_t)
FPTOOLS_CHECK_HTYPE(ino_t)
FPTOOLS_CHECK_HTYPE(mode_t)
FPTOOLS_CHECK_HTYPE(off_t)
FPTOOLS_CHECK_HTYPE(pid_t)
FPTOOLS_CHECK_HTYPE(gid_t)
FPTOOLS_CHECK_HTYPE(uid_t)
FPTOOLS_CHECK_HTYPE(cc_t)
FPTOOLS_CHECK_HTYPE(speed_t)
FPTOOLS_CHECK_HTYPE(tcflag_t)
FPTOOLS_CHECK_HTYPE(nlink_t)
FPTOOLS_CHECK_HTYPE(ssize_t)
FPTOOLS_CHECK_HTYPE(rlim_t)
FPTOOLS_CHECK_HTYPE(blksize_t)
FPTOOLS_CHECK_HTYPE(blkcnt_t)
FPTOOLS_CHECK_HTYPE(clockid_t)
FPTOOLS_CHECK_HTYPE(fsblkcnt_t)
FPTOOLS_CHECK_HTYPE(fsfilcnt_t)
FPTOOLS_CHECK_HTYPE(id_t)
FPTOOLS_CHECK_HTYPE(key_t)
FPTOOLS_CHECK_HTYPE(timer_t)
FPTOOLS_CHECK_HTYPE(socklen_t)
FPTOOLS_CHECK_HTYPE(nfds_t)

FPTOOLS_CHECK_HTYPE(intptr_t)
FPTOOLS_CHECK_HTYPE(uintptr_t)
FPTOOLS_CHECK_HTYPE(intmax_t)
FPTOOLS_CHECK_HTYPE(uintmax_t)

# test errno values
FP_CHECK_CONSTS([E2BIG EACCES EADDRINUSE EADDRNOTAVAIL EADV EAFNOSUPPORT EAGAIN EALREADY EBADF EBADMSG EBADRPC EBUSY ECHILD ECOMM ECONNABORTED ECONNREFUSED ECONNRESET EDEADLK EDESTADDRREQ EDIRTY EDOM EDQUOT EEXIST EFAULT EFBIG EFTYPE EHOSTDOWN EHOSTUNREACH EIDRM EILSEQ EINPROGRESS EINTR EINVAL EIO EISCONN EISDIR ELOOP EMFILE EMLINK EMSGSIZE EMULTIHOP ENAMETOOLONG ENETDOWN ENETRESET ENETUNREACH ENFILE ENOBUFS ENODATA ENODEV ENOENT ENOEXEC ENOLCK ENOLINK ENOMEM ENOMSG ENONET ENOPROTOOPT ENOSPC ENOSR ENOSTR ENOSYS ENOTBLK ENOTCONN ENOTDIR ENOTEMPTY ENOTSOCK ENOTTY ENXIO EOPNOTSUPP EPERM EPFNOSUPPORT EPIPE EPROCLIM EPROCUNAVAIL EPROGMISMATCH EPROGUNAVAIL EPROTO EPROTONOSUPPORT EPROTOTYPE ERANGE EREMCHG EREMOTE EROFS ERPCMISMATCH ERREMOTE ESHUTDOWN ESOCKTNOSUPPORT ESPIPE ESRCH ESRMNT ESTALE ETIME ETIMEDOUT ETOOMANYREFS ETXTBSY EUSERS EWOULDBLOCK EXDEV ENOCIGAR ENOTSUP], [#include <stdio.h>
#include <errno.h>])

# we need SIGINT in TopHandler.lhs
FP_CHECK_CONSTS([SIGINT], [
#if HAVE_SIGNAL_H
#include <signal.h>
#endif])

dnl ** can we open files in binary mode?
FP_CHECK_CONST([O_BINARY], [#include <fcntl.h>], [0])

FP_CHECK_ENVIRON

# We don't use iconv or libcharset on Windows, but if configure finds
# them then it can cause problems. So we don't even try looking if
# we are on Windows.
# See http://www.haskell.org/pipermail/cvs-ghc/2011-September/065980.html
if test "$WINDOWS" = "NO"
then

# We can't just use AC_SEARCH_LIBS for this, as on OpenBSD the iconv.h
# header needs to be included as iconv_open is #define'd to something
# else. We therefore use our own FP_SEARCH_LIBS_PROTO, which allows us
# to give prototype text.
FP_SEARCH_LIBS_PROTO(iconv,
                     [
#include <stddef.h>
#include <iconv.h>
                      ],
                     [iconv_t cd;
                      cd = iconv_open("", "");
                      iconv(cd,NULL,NULL,NULL,NULL);
                      iconv_close(cd);],
                     iconv,
                     [EXTRA_LIBS="$EXTRA_LIBS $ac_lib"],
                     [AC_MSG_ERROR([iconv is required on non-Windows platforms])])

# If possible, we use libcharset instead of nl_langinfo(CODESET) to
# determine the current locale's character encoding.  Allow the user
# to disable this with --without-libcharset if they don't want a
# dependency on libcharset.
AC_ARG_WITH([libcharset],
  [AS_HELP_STRING([--with-libcharset],
    [Use libcharset [default=only if available]])],
  [],
  [with_libcharset=check])

AS_IF([test "x$with_libcharset" != xno],
  FP_SEARCH_LIBS_PROTO(
    [locale_charset],
    [#include <libcharset.h>],
    [const char* charset = locale_charset();],
    [charset],
    [AC_DEFINE([HAVE_LIBCHARSET], [1], [Define to 1 if you have libcharset.])
     EXTRA_LIBS="$EXTRA_LIBS $ac_lib"]
  ))

fi

dnl Calling AC_CHECK_TYPE(T) makes AC_CHECK_SIZEOF(T) abort on failure
dnl instead of considering sizeof(T) as 0.
AC_CHECK_TYPE([struct MD5Context], [], [AC_MSG_ERROR([internal error])], [#include "include/md5.h"])
AC_CHECK_SIZEOF([struct MD5Context], [], [#include "include/md5.h"])

AC_SUBST(EXTRA_LIBS)
AC_CONFIG_FILES([ghc-internal.buildinfo include/HsIntegerGmp.h])

AC_OUTPUT
