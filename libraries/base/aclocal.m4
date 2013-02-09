# FP_COMPUTE_INT(EXPRESSION, VARIABLE, INCLUDES, IF-FAILS)
# --------------------------------------------------------
# Assign VARIABLE the value of the compile-time EXPRESSION using INCLUDES for
# compilation. Execute IF-FAILS when unable to determine the value. Works for
# cross-compilation, too.
#
# Implementation note: We are lazy and use an internal autoconf macro, but it
# is supported in autoconf versions 2.50 up to the actual 2.57, so there is
# little risk.
# The public AC_COMPUTE_INT macro isn't supported by some versions of
# autoconf.
AC_DEFUN([FP_COMPUTE_INT],
[_AC_COMPUTE_INT([$2], [$1], [$3], [$4])[]dnl
])# FP_COMPUTE_INT


# FP_CHECK_CONST(EXPRESSION, [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -------------------------------------------------------------------------------
# Defines CONST_EXPRESSION to the value of the compile-time EXPRESSION, using
# INCLUDES. If the value cannot be determined, use VALUE-IF-FAIL.
AC_DEFUN([FP_CHECK_CONST],
[AS_VAR_PUSHDEF([fp_Cache], [fp_cv_const_$1])[]dnl
AC_CACHE_CHECK([value of $1], fp_Cache,
[FP_COMPUTE_INT(fp_check_const_result, [$1], [AC_INCLUDES_DEFAULT([$2])],
                [fp_check_const_result=m4_default([$3], ['-1'])])
AS_VAR_SET(fp_Cache, [$fp_check_const_result])])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP([CONST_$1]), AS_VAR_GET(fp_Cache), [The value of $1.])[]dnl
AS_VAR_POPDEF([fp_Cache])[]dnl
])# FP_CHECK_CONST


# FP_CHECK_CONSTS_TEMPLATE(EXPRESSION...)
# ---------------------------------------
# autoheader helper for FP_CHECK_CONSTS
m4_define([FP_CHECK_CONSTS_TEMPLATE],
[AC_FOREACH([fp_Const], [$1],
  [AH_TEMPLATE(AS_TR_CPP(CONST_[]fp_Const),
               [The value of ]fp_Const[.])])[]dnl
])# FP_CHECK_CONSTS_TEMPLATE


# FP_CHECK_CONSTS(EXPRESSION..., [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -----------------------------------------------------------------------------------
# List version of FP_CHECK_CONST
AC_DEFUN([FP_CHECK_CONSTS],
[FP_CHECK_CONSTS_TEMPLATE([$1])dnl
for fp_const_name in $1
do
FP_CHECK_CONST([$fp_const_name], [$2], [$3])
done
])# FP_CHECK_CONSTS


dnl FPTOOLS_HTYPE_INCLUDES
AC_DEFUN([FPTOOLS_HTYPE_INCLUDES],
[
#include <stdio.h>
#include <stddef.h>

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_TERMIOS_H
# include <termios.h>
#endif

#if HAVE_STRING_H
# include <string.h>
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif

#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#include <stdlib.h>
])


dnl ** Map an arithmetic C type to a Haskell type.
dnl    Based on autconf's AC_CHECK_SIZEOF.

dnl FPTOOLS_CHECK_HTYPE_ELSE(TYPE, WHAT_TO_DO_IF_TYPE_DOES_NOT_EXIST)
AC_DEFUN([FPTOOLS_CHECK_HTYPE_ELSE],[
    changequote(<<, >>)
    dnl The name to #define.
    define(<<AC_TYPE_NAME>>, translit(htype_$1, [a-z *], [A-Z_P]))
    dnl The cache variable names.
    define(<<AC_CV_NAME>>, translit(fptools_cv_htype_$1, [ *], [_p]))
    define(<<AC_CV_NAME_supported>>, translit(fptools_cv_htype_sup_$1, [ *], [_p]))
    changequote([, ])

    AC_MSG_CHECKING(Haskell type for $1)
    AC_CACHE_VAL(AC_CV_NAME,[
        AC_CV_NAME_supported=yes
        FP_COMPUTE_INT([HTYPE_IS_INTEGRAL],
                       [($1)0.2 - ($1)0.4 < 0 ? 0 : 1],
                       [FPTOOLS_HTYPE_INCLUDES],[HTYPE_IS_INTEGRAL=0])

        if test "$HTYPE_IS_INTEGRAL" -eq 0
        then
            FP_COMPUTE_INT([HTYPE_IS_FLOAT],[sizeof($1) == sizeof(float)],
                           [FPTOOLS_HTYPE_INCLUDES],
                           [AC_CV_NAME_supported=no])
            FP_COMPUTE_INT([HTYPE_IS_DOUBLE],[sizeof($1) == sizeof(double)],
                           [FPTOOLS_HTYPE_INCLUDES],
                           [AC_CV_NAME_supported=no])
            FP_COMPUTE_INT([HTYPE_IS_LDOUBLE],[sizeof($1) == sizeof(long double)],
                           [FPTOOLS_HTYPE_INCLUDES],
                           [AC_CV_NAME_supported=no])
            if test "$HTYPE_IS_FLOAT" -eq 1
            then
                AC_CV_NAME=Float
            elif test "$HTYPE_IS_DOUBLE" -eq 1
            then
                AC_CV_NAME=Double
            elif test "$HTYPE_IS_LDOUBLE" -eq 1
            then
                AC_CV_NAME=LDouble
            else
                AC_CV_NAME_supported=no
            fi
        else
            FP_COMPUTE_INT([HTYPE_IS_SIGNED],[(($1)(-1)) < (($1)0)],
                           [FPTOOLS_HTYPE_INCLUDES],
                           [AC_CV_NAME_supported=no])
            FP_COMPUTE_INT([HTYPE_SIZE],[sizeof($1) * 8],
                           [FPTOOLS_HTYPE_INCLUDES],
                           [AC_CV_NAME_supported=no])
            if test "$HTYPE_IS_SIGNED" -eq 0
            then
                AC_CV_NAME="Word$HTYPE_SIZE"
            else
                AC_CV_NAME="Int$HTYPE_SIZE"
            fi
        fi
    ])
    if test "$AC_CV_NAME_supported" = no
    then
        $2
    fi

    dnl Note: evaluating dollar-2 can change the value of
    dnl $AC_CV_NAME_supported, so we might now get a different answer
    if test "$AC_CV_NAME_supported" = yes; then
        AC_MSG_RESULT($AC_CV_NAME)
        AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME,
                           [Define to Haskell type for $1])
    fi
    undefine([AC_TYPE_NAME])dnl
    undefine([AC_CV_NAME])dnl
    undefine([AC_CV_NAME_supported])dnl
])

dnl FPTOOLS_CHECK_HTYPE(TYPE)
AC_DEFUN([FPTOOLS_CHECK_HTYPE],[
    FPTOOLS_CHECK_HTYPE_ELSE([$1],[
        AC_CV_NAME=NotReallyAType
        AC_MSG_RESULT([not supported])
    ])
])


# FP_SEARCH_LIBS_PROTO(WHAT, PROTOTYPE, FUNCTION, SEARCH-LIBS,
#                [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                [OTHER-LIBRARIES])
# --------------------------------------------------------
# Search for a library defining FUNC, if it's not already available.
# This is a copy of the AC_SEARCH_LIBS definition, but extended to take
# the name of the thing we are looking for as its first argument, and
# prototype text as its second argument. It also calls AC_LANG_PROGRAM
# instead of AC_LANG_CALL
AC_DEFUN([FP_SEARCH_LIBS_PROTO],
[AS_VAR_PUSHDEF([ac_Search], [ac_cv_search_$1])dnl
AC_CACHE_CHECK([for library containing $1], [ac_Search],
[ac_func_search_save_LIBS=$LIBS
AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
for ac_lib in '' $4; do
  if test -z "$ac_lib"; then
    ac_res="none required"
  else
    ac_res=-l$ac_lib
    LIBS="-l$ac_lib $7 $ac_func_search_save_LIBS"
  fi
  AC_LINK_IFELSE([], [AS_VAR_SET([ac_Search], [$ac_res])])
  AS_VAR_SET_IF([ac_Search], [break])
done
AS_VAR_SET_IF([ac_Search], , [AS_VAR_SET([ac_Search], [no])])
rm conftest.$ac_ext
LIBS=$ac_func_search_save_LIBS])
ac_res=AS_VAR_GET([ac_Search])
AS_IF([test "$ac_res" != no],
  [test "$ac_res" = "none required" || LIBS="$ac_res $LIBS"
  $5],
      [$6])dnl
AS_VAR_POPDEF([ac_Search])dnl
])
