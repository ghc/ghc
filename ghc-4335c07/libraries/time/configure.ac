AC_INIT([Haskell time package], [1.8.0.2], [ashley@semantic.org], [time])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([lib/include/HsTime.h])

# These are to silence warnings with older Cabal versions
AC_ARG_WITH([gcc],[Gnu C compiler])
AC_ARG_WITH([compiler],[Haskell compiler])

AC_PROG_CC()

AC_USE_SYSTEM_EXTENSIONS

AC_CONFIG_HEADERS([lib/include/HsTimeConfig.h])

AC_CHECK_HEADERS([time.h])
AC_CHECK_FUNCS([gmtime_r localtime_r])

AC_CHECK_FUNCS([clock_gettime])

AC_STRUCT_TM
AC_STRUCT_TIMEZONE

FP_DECL_ALTZONE

AC_OUTPUT
