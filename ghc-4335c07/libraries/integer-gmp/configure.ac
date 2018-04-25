AC_PREREQ(2.60)
AC_INIT([Haskell integer (GMP)], [1.0], [libraries@haskell.org], [integer])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([cbits/wrappers.c])

AC_CANONICAL_TARGET

AC_PROG_CC
dnl make extensions visible to allow feature-tests to detect them lateron
AC_USE_SYSTEM_EXTENSIONS


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

AC_ARG_WITH([intree-gmp],
  [AC_HELP_STRING([--with-intree-gmp],
    [force using the in-tree GMP])],
    [GMP_FORCE_INTREE=YES],
    [GMP_FORCE_INTREE=NO])

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
    GhcGmpVerMj=5
    GhcGmpVerMi=0
    GhcGmpVerPl=4
    AC_MSG_RESULT([$GhcGmpVerMj.$GhcGmpVerMi.$GhcGmpVerPl])
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
AC_SUBST(HaveSecurePowm)
AC_SUBST(UseIntreeGmp)
AC_SUBST(GhcGmpVerMj)
AC_SUBST(GhcGmpVerMi)
AC_SUBST(GhcGmpVerPl)

AC_CONFIG_FILES([integer-gmp.buildinfo gmp/config.mk include/HsIntegerGmp.h])

dnl--------------------------------------------------------------------
dnl * Generate output files
dnl--------------------------------------------------------------------

AC_OUTPUT
