dnl FP_CHECK_PTHREADS
dnl ----------------------------------
dnl Check various aspects of the platform's pthreads support
AC_DEFUN([FP_CHECK_PTHREADS],
[
  dnl Some platforms (e.g. Android's Bionic) have pthreads support available
  dnl without linking against libpthread. Check whether -lpthread is necessary
  dnl to use pthreads.
  dnl
  dnl Note that it is important that this happens before we AC_CHECK_LIB(thread)
  AC_MSG_CHECKING(whether -lpthread is needed for pthreads)
  AC_CHECK_FUNC(pthread_create,
      [
          AC_MSG_RESULT(no)
          need_lpthread=0
      ],
      [
          AC_CHECK_LIB(pthread, pthread_create,
              [
                  AC_MSG_RESULT(yes)
                  need_lpthread=1
              ],
              [
                  AC_MSG_RESULT([no pthreads support found.])
                  need_lpthread=0
              ])
      ])
  AC_DEFINE_UNQUOTED([NEED_PTHREAD_LIB], [$need_lpthread],
      [Define 1 if we need to link code using pthreads with -lpthread])

  dnl Setting thread names
  dnl ~~~~~~~~~~~~~~~~~~~~
  dnl The portability situation here is complicated:
  dnl
  dnl * FreeBSD supports pthread_set_name_np in <pthread_np.h>
  dnl   and (if not _POSIX_SOURCE) pthread_setname_np() in <pthread.h>
  dnl   because of the conditional visibility, we prefer the former.
  dnl * glibc supports pthread_setname_np
  dnl * Darwin supports pthread_setname_np but does not take a
  dnl   pthread_t argument.
  dnl
  AC_CHECK_HEADERS([pthread_np.h])

  dnl ** pthread_setname_np is a recent addition to glibc, and OS X has
  dnl    a different single-argument version.
  AC_CHECK_LIB(pthread, pthread_setname_np)

  AC_MSG_CHECKING([for pthread_setname_np (Darwin)])
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM(
      [[
      #include <pthread.h>
      ]],
      [[pthread_setname_np("name");]]
    )],
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_PTHREAD_SETNAME_NP_DARWIN], [1],
        [Define to 1 if you have the Darwin version of pthread_setname_np])
    ],
    AC_MSG_RESULT(no)
  )

  dnl glibc
  AC_MSG_CHECKING([for pthread_setname_np (glibc)])
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM(
      [[
      #define _GNU_SOURCE
      #include <pthread.h>
      ]],
      [[pthread_setname_np(pthread_self(), "name");]]
    )],
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_PTHREAD_SETNAME_NP], [1],
        [Define to 1 if you have the glibc version of pthread_setname_np])
    ],
    AC_MSG_RESULT(no)
  )

  dnl NetBSD
  AC_MSG_CHECKING([for pthread_setname_np (NetBSD)])
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM(
      [[
      #include <pthread.h>
      ]],
      [[pthread_setname_np(pthread_self(), "%s", "name");]]
    )],
    [
      AC_MSG_RESULT([yes])
      AC_DEFINE([HAVE_PTHREAD_SETNAME_NP_NETBSD], [1],
        [Define to 1 if you have the NetBSD version of pthread_setname_np])
    ],
    AC_MSG_RESULT([no])
  )

  dnl FreeBSD
  AC_MSG_CHECKING([for pthread_set_name_np])
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM(
      [[
      #include <pthread_np.h>
      ]],
      [[pthread_set_name_np(pthread_self(), "name");]]
    )],
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_PTHREAD_SET_NAME_NP], [1],
        [Define to 1 if you have pthread_set_name_np])
    ],
    AC_MSG_RESULT(no)
  )

  AC_CHECK_FUNCS_ONCE([pthread_condattr_setclock])
])
