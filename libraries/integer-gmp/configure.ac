AC_INIT([Haskell integer (GMP)], [0.1], [libraries@haskell.org], [integer])

AC_CANONICAL_TARGET

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()


dnl--------------------------------------------------------------------
dnl * Deal with arguments telling us gmp is somewhere odd
dnl--------------------------------------------------------------------

AC_ARG_WITH([gmp-includes],
  [AC_HELP_STRING([--with-gmp-includes],
    [directory containing gmp.h])],
    [GMP_INCLUDE_DIRS=$withval; CPPFLAGS="-I$withval"],
    [GMP_INCLUDE_DIRS=])

AC_ARG_WITH([gmp-libraries],
  [AC_HELP_STRING([--with-gmp-libraries],
    [directory containing gmp library])],
    [GMP_LIB_DIRS=$withval; LDFLAGS="-L$withval"],
    [GMP_LIB_DIRS=])

AC_ARG_WITH([gmp-framework-preferred],
  [AC_HELP_STRING([--with-gmp-framework-preferred],
    [on OSX, prefer the GMP framework to the gmp lib])],
    [GMP_PREFER_FRAMEWORK=YES],
    [GMP_PREFER_FRAMEWORK=NO])

dnl--------------------------------------------------------------------
dnl * Detect gmp
dnl--------------------------------------------------------------------

HaveLibGmp=NO
GMP_LIBS=
HaveFrameworkGMP=NO
GMP_FRAMEWORK=

if test "$GMP_PREFER_FRAMEWORK" = "YES"
then
    LOOK_FOR_GMP_FRAMEWORK
    LOOK_FOR_GMP_LIB
else
    LOOK_FOR_GMP_LIB
    LOOK_FOR_GMP_FRAMEWORK
fi
if test "$HaveFrameworkGMP" = "YES" || test "$HaveLibGmp" = "YES"
then
    AC_CHECK_HEADER([gmp.h], , [AC_MSG_ERROR([Cannot find gmp.h])])
fi

dnl--------------------------------------------------------------------
dnl * Make sure we got some form of gmp
dnl--------------------------------------------------------------------

AC_SUBST(GMP_INCLUDE_DIRS)
AC_SUBST(GMP_LIBS)
AC_SUBST(GMP_LIB_DIRS)
AC_SUBST(GMP_FRAMEWORK)
AC_SUBST(HaveLibGmp)
AC_SUBST(HaveFrameworkGMP)

dnl GMP_INT_TO_CONST(int_expr, var_name)
AC_DEFUN([GMP_INT_TO_VAR],
[
    AC_MSG_CHECKING([for $1 size])
    AC_COMPUTE_INT([$2], [$1],[[#include <stdio.h>
#include <stddef.h>
#include "gmp.h"

#define FIELD_OFFSET(s_type, field) offsetof(s_type, field)
#define FIELD_SIZE_BITS(s_type, field) (unsigned long)sizeof (__typeof__(((((s_type*)0)->field)))) * 8
]], AC_MSG_ERROR([Failed to compute size of $1]))
    AC_MSG_RESULT($$2)
    AC_SUBST($2)
])

GMP_INT_TO_VAR([[sizeof (MP_INT)]], [SIZEOF_MP_INT])
GMP_INT_TO_VAR([[FIELD_OFFSET(MP_INT,_mp_alloc)]],    [OFFSET_MP_INT__mp_alloc])
GMP_INT_TO_VAR([[FIELD_SIZE_BITS(MP_INT,_mp_alloc)]], [REP_MP_INT__mp_alloc])
GMP_INT_TO_VAR([[FIELD_OFFSET(MP_INT,_mp_size)]],     [OFFSET_MP_INT__mp_size])
GMP_INT_TO_VAR([[FIELD_SIZE_BITS(MP_INT,_mp_size)]],  [REP_MP_INT__mp_size])
GMP_INT_TO_VAR([[FIELD_OFFSET(MP_INT,_mp_d)]],        [OFFSET_MP_INT__mp_d])
GMP_INT_TO_VAR([[FIELD_SIZE_BITS(MP_INT,_mp_d)]],     [REP_MP_INT__mp_d])

AC_CONFIG_FILES([integer-gmp.buildinfo gmp/config.mk cbits/GmpDerivedConstants.h])

dnl--------------------------------------------------------------------
dnl * Generate the header cbits/GmpDerivedConstants.h
dnl--------------------------------------------------------------------

AC_OUTPUT
