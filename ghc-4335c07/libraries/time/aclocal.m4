# FP_DECL_ALTZONE
# ---------------
# Defines HAVE_DECL_ALTZONE to 1 if declared, 0 otherwise.
#
# Used by base package.
AC_DEFUN([FP_DECL_ALTZONE],
[AC_REQUIRE([AC_HEADER_TIME])dnl
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_DECLS([altzone], [], [],[#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif])
])# FP_DECL_ALTZONE
