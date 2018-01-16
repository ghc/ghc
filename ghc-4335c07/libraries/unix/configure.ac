AC_PREREQ([2.60])
AC_INIT([Haskell unix package], [2.0], [libraries@haskell.org], [unix])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsUnix.h])

AC_PROG_CC

dnl make extensions visible to allow feature-tests to detect them lateron
AC_USE_SYSTEM_EXTENSIONS

AC_CONFIG_HEADERS([include/HsUnixConfig.h])

# Is this a Unix system?
AC_CHECK_HEADER([dlfcn.h], [BUILD_PACKAGE_BOOL=True], [BUILD_PACKAGE_BOOL=False])
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_C_CONST

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
dnl
dnl WARNING: It's essential this check agrees with HsBaseConfig.h as otherwise
dnl          the definitions of COff/coff_t don't line up
AC_SYS_LARGEFILE

AC_CHECK_HEADERS([dirent.h fcntl.h grp.h limits.h pwd.h signal.h string.h])
AC_CHECK_HEADERS([sys/resource.h sys/stat.h sys/times.h sys/time.h])
AC_CHECK_HEADERS([sys/utsname.h sys/wait.h])
AC_CHECK_HEADERS([bsd/libutil.h libutil.h pty.h utmp.h])
AC_CHECK_HEADERS([termios.h time.h unistd.h utime.h])

AC_CHECK_FUNCS([getgrgid_r getgrnam_r getpwnam_r getpwuid_r getpwnam getpwuid])
AC_CHECK_FUNCS([getpwent getgrent])
AC_CHECK_FUNCS([lchown setenv sysconf unsetenv clearenv])
AC_CHECK_FUNCS([nanosleep])
AC_CHECK_FUNCS([ptsname])
AC_CHECK_FUNCS([setitimer])
AC_CHECK_FUNCS([readdir_r])

dnl not available on android so check for it
AC_CANONICAL_TARGET
AS_CASE([$target_os],[*-android*],[],[AC_CHECK_FUNCS([telldir seekdir])])

dnl When available, _NSGetEnviron() (defined in <crt_externs.h>) is
dnl the preferred way to access environ(7)
AC_CHECK_FUNCS([_NSGetEnviron])

dnl This is e.g. available as a GNU extension in glibc 2.11+
AC_CHECK_DECLS([execvpe])
AC_CHECK_FUNCS([execvpe])

AC_CHECK_MEMBERS([struct stat.st_atim])
AC_CHECK_MEMBERS([struct stat.st_mtim])
AC_CHECK_MEMBERS([struct stat.st_ctim])
AC_CHECK_MEMBERS([struct stat.st_atimespec])
AC_CHECK_MEMBERS([struct stat.st_mtimespec])
AC_CHECK_MEMBERS([struct stat.st_ctimespec])
AC_CHECK_MEMBERS([struct stat.st_atimensec])
AC_CHECK_MEMBERS([struct stat.st_mtimensec])
AC_CHECK_MEMBERS([struct stat.st_ctimensec])
AC_CHECK_MEMBERS([struct stat.st_atime_n])
AC_CHECK_MEMBERS([struct stat.st_mtime_n])
AC_CHECK_MEMBERS([struct stat.st_ctime_n])
AC_CHECK_MEMBERS([struct stat.st_uatime])
AC_CHECK_MEMBERS([struct stat.st_umtime])
AC_CHECK_MEMBERS([struct stat.st_uctime])

AC_CHECK_MEMBER([struct passwd.pw_gecos], [], [AC_DEFINE([HAVE_NO_PASSWD_PW_GECOS],[],[Ignore the pw_gecos member of passwd where it does not exist])], [[#include <pwd.h>]])

# Functions for changing file timestamps
AC_CHECK_FUNCS([utimensat futimens])
AC_CHECK_FUNCS([lutimes futimes])

# Additional temp functions
dnl androids bionic doesn't have mkstemps
# We explicilty check for android, as the check AC_CHECK_FUNCS performs returns "yes" for mkstemps
# when targetting android. See similar conditionals for seekdir and telldir.
AS_CASE([$target_os],[*-android*],[AC_CHECK_FUNCS([mkdtemp])],[AC_CHECK_FUNCS([mkstemps mkdtemp])])

# Functions for file synchronization and allocation control
AC_CHECK_FUNCS([fsync])

# On OSX linking against 'fdatasync' succeeds, but that doesn't pick
# the expected the POSIX 'fdatasync' function.  So make sure that we
# also have a function declaration in scope, in addition to being able
# to link against 'fdatasync'.
AC_CHECK_DECLS([fdatasync],[AC_CHECK_FUNCS([fdatasync])])


AC_CHECK_FUNCS([posix_fadvise posix_fallocate])

# Some termios(3) functions known to be missing sometimes (see also #55)
AC_CHECK_DECLS([tcdrain],[AC_DEFINE([HAVE_TCDRAIN],[1],[Define to 1 if you have the `tcdrain' function.])],[],[AC_INCLUDES_DEFAULT
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
])

AC_CHECK_DECLS([ctermid],[AC_DEFINE([HAVE_CTERMID],[1],[Define to 1 if you have the `ctermid' function.])],[],[AC_INCLUDES_DEFAULT
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
])

# Avoid adding rt if absent or unneeded
# shm_open needs -lrt on linux
AC_SEARCH_LIBS(shm_open, rt, [AC_CHECK_FUNCS([shm_open shm_unlink])])
AS_IF([test "x$ac_cv_search_shm_open" = x-lrt], [EXTRA_LIBS="$EXTRA_LIBS rt"])

FP_CHECK_CONSTS([SIGABRT SIGALRM SIGBUS SIGCHLD SIGCONT SIGFPE SIGHUP SIGILL SIGINT SIGKILL SIGPIPE SIGQUIT SIGSEGV SIGSTOP SIGTERM SIGTSTP SIGTTIN SIGTTOU SIGUSR1 SIGUSR2 SIGPOLL SIGPROF SIGSYS SIGTRAP SIGURG SIGVTALRM SIGXCPU SIGXFSZ SIG_BLOCK SIG_SETMASK SIG_UNBLOCK SIGINFO SIGWINCH], [
#if HAVE_SIGNAL_H
#include <signal.h>
#endif])

AC_MSG_CHECKING([for _SC_GETGR_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETGR_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETGR_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETGR_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for _SC_GETPW_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETPW_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETPW_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETPW_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

dnl ---------- usleep ----------
dnl --- stolen from guile configure ---

### On some systems usleep has no return value.  If it does have one,
### we'd like to return it; otherwise, we'll fake it.
AC_CACHE_CHECK([return type of usleep], fptools_cv_func_usleep_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+usleep>changequote([, ]),
                   unistd.h,
                   [fptools_cv_func_usleep_return_type=void],
                   [fptools_cv_func_usleep_return_type=int])])
case "$fptools_cv_func_usleep_return_type" in
  "void" )
    AC_DEFINE([USLEEP_RETURNS_VOID], [1], [Define if the system headers declare usleep to return void.])
  ;;
esac

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

dnl On some hosts (e.g. SuSe and Ubuntu Linux) RTLD_NEXT and RTLD_DEFAULT are
dnl not visible without setting _GNU_SOURCE, which we really don't want to.
dnl Also see comments in System/Posix/DynamicLinker/Prim.hsc.
AC_MSG_CHECKING(for RTLD_NEXT from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_NEXT
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDNEXT], [1], [Define to 1 if we can see RTLD_NEXT in dlfcn.h.])
], [
  AC_MSG_RESULT(no)
  ])    

AC_MSG_CHECKING(for RTLD_DEFAULT from dlfcn.h)
AC_EGREP_CPP(yes,
[
 #include <dlfcn.h>
 #ifdef RTLD_DEFAULT
        yes
 #endif
], [
  AC_MSG_RESULT(yes)
  AC_DEFINE([HAVE_RTLDDEFAULT], [1], [Define to 1 if RTLD_DEFAULT is available.])
], [
  AC_MSG_RESULT(no)
  ])    

AC_CHECK_FUNCS(openpty,,
   AC_CHECK_LIB(util,openpty,
     [AC_DEFINE(HAVE_OPENPTY) EXTRA_LIBS="$EXTRA_LIBS util"],
     AC_CHECK_LIB(bsd,openpty, [AC_DEFINE(HAVE_OPENPTY) EXTRA_LIBS="$EXTRA_LIBS bsd"])
   )
)

AC_MSG_CHECKING(for /dev/ptmx)
if test -r /dev/ptmx
then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_DEV_PTMX, 1,
  [Define if we have /dev/ptmx.])
else
  AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(for /dev/ptc)
if test -r /dev/ptc
then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_DEV_PTC, 1,
  [Define if we have /dev/ptc.])
else
  AC_MSG_RESULT(no)
fi

# Avoid adding dl if absent or unneeded
AC_SEARCH_LIBS([dlopen], [dl], [EXTRA_LIBS="$EXTRA_LIBS $ac_lib"])

# -{l,}pthread goo
AC_CANONICAL_TARGET

AC_SEARCH_LIBS(sem_close, pthread,
    [EXTRA_LIBS="$EXTRA_LIBS $ac_lib"],
    [AC_MSG_NOTICE([Not found])])

AC_SUBST([EXTRA_LIBS])
AC_CONFIG_FILES([unix.buildinfo])

AC_OUTPUT
