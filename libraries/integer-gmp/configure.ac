AC_INIT([Haskell integer (GMP)], [0.1], [libraries@haskell.org], [integer])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([cbits/gmp-wrappers.cmm])

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
AC_SUBST(HaveSecurePowm)

AC_CONFIG_FILES([integer-gmp.buildinfo gmp/config.mk include/HsIntegerGmp.h])

dnl--------------------------------------------------------------------
dnl * Generate the header cbits/GmpDerivedConstants.h
dnl--------------------------------------------------------------------

AC_OUTPUT
