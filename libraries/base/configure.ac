AC_INIT([Haskell base package], [1.0], [libraries@haskell.org], [base])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsBase.h])

AC_CONFIG_HEADERS([include/HsBaseConfig.h include/EventConfig.h])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_MSG_CHECKING(for WINDOWS platform)
case $host in
    *mingw32*|*cygwin*)
        WINDOWS=YES;;
    *)
        WINDOWS=NO;;
esac
AC_MSG_RESULT($WINDOWS)

# do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([ctype.h errno.h fcntl.h inttypes.h limits.h signal.h sys/resource.h sys/select.h sys/stat.h sys/syscall.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/types.h sys/utsname.h sys/wait.h termios.h time.h unistd.h utime.h windows.h winsock.h langinfo.h poll.h sys/epoll.h sys/event.h sys/eventfd.h])

# Enable large file support. Do this before testing the types ino_t, off_t, and
# rlim_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** check for wide-char classifications
dnl FreeBSD has an emtpy wctype.h, so test one of the affected
dnl functions if it's really there.
AC_CHECK_HEADERS([wctype.h], [AC_CHECK_FUNCS(iswspace)])

AC_CHECK_FUNCS([lstat])
AC_CHECK_LIB([rt], [clock_gettime])
AC_CHECK_FUNCS([clock_gettime])
AC_CHECK_FUNCS([getclock getrusage times])
AC_CHECK_FUNCS([_chsize ftruncate])

AC_CHECK_FUNCS([epoll_ctl eventfd kevent kevent64 kqueue poll])

# event-related fun

if test "$ac_cv_header_sys_epoll_h" = yes -a "$ac_cv_func_epoll_ctl" = yes; then
  AC_DEFINE([HAVE_EPOLL], [1], [Define if you have epoll support.])
fi

if test "$ac_cv_header_sys_event_h" = yes -a "$ac_cv_func_kqueue" = yes; then
  AC_DEFINE([HAVE_KQUEUE], [1], [Define if you have kqueue support.])

  AC_CHECK_SIZEOF([kev.filter], [], [#include <sys/event.h>
struct kevent kev;])

  AC_CHECK_SIZEOF([kev.flags], [], [#include <sys/event.h>
struct kevent kev;])
fi

if test "$ac_cv_header_poll_h" = yes -a "$ac_cv_func_poll" = yes; then
  AC_DEFINE([HAVE_POLL], [1], [Define if you have poll support.])
fi

# unsetenv
AC_CHECK_FUNCS([unsetenv])

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
  [AC_HELP_STRING([--with-iconv-includes],
    [directory containing iconv.h])],
    [ICONV_INCLUDE_DIRS=$withval; CPPFLAGS="-I$withval"],
    [ICONV_INCLUDE_DIRS=])

AC_ARG_WITH([iconv-libraries],
  [AC_HELP_STRING([--with-iconv-libraries],
    [directory containing iconv library])],
    [ICONV_LIB_DIRS=$withval; LDFLAGS="-L$withval"],
    [ICONV_LIB_DIRS=])

AC_SUBST(ICONV_INCLUDE_DIRS)
AC_SUBST(ICONV_LIB_DIRS)

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
# determine the current locale's character encoding.
FP_SEARCH_LIBS_PROTO(
    [locale_charset],
    [#include <libcharset.h>],
    [const char* charset = locale_charset();],
    [charset],
    [AC_DEFINE([HAVE_LIBCHARSET], [1], [Define to 1 if you have libcharset.])
     EXTRA_LIBS="$EXTRA_LIBS $ac_lib"])

fi

# Hack - md5.h needs HsFFI.h.  Is there a better way to do this?
CFLAGS="-I../../includes $CFLAGS"
AC_CHECK_SIZEOF([struct MD5Context], ,[#include "include/md5.h"])

AC_SUBST(EXTRA_LIBS)
AC_CONFIG_FILES([base.buildinfo])

AC_OUTPUT
