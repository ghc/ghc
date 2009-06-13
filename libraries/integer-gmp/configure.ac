AC_INIT([Haskell integer (GMP)], [0.1], [libraries@haskell.org], [integer])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([cbits/mkGmpDerivedConstants.c])

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


dnl--------------------------------------------------------------------
dnl * Check whether this machine has gmp/gmp3 installed
dnl--------------------------------------------------------------------

AC_CHECK_LIB([gmp],  [__gmpz_fdiv_qr],
             [HAVE_GMP=YES; GMP_LIBS=gmp],
             [HAVE_GMP=NO;  GMP_LIBS=])
if test "$HAVE_GMP" = "NO"; then
AC_CHECK_LIB([gmp3], [__gmpz_fdiv_qr],
             [HAVE_GMP=YES; GMP_LIBS=gmp3],
             [HAVE_GMP=NO;  GMP_LIBS=])
fi

dnl--------------------------------------------------------------------
dnl * Mac OS X only: check for GMP.framework
dnl--------------------------------------------------------------------

case $target_os in
  darwin*)
    AC_MSG_CHECKING([for GMP.framework])
    save_libs="$LIBS"
    LIBS="-framework GMP"
    AC_TRY_LINK_FUNC(__gmpz_fdiv_qr,
      [HAVE_GMP_FRAMEWORK=yes; GMP_FRAMEWORK=GMP; GMP_LIBS=; HAVE_GMP=YES],
      [HAVE_GMP_FRAMEWORK=no])
    LIBS="$save_libs"
    AC_MSG_RESULT([$HAVE_GMP_FRAMEWORK])
    ;;
esac

dnl--------------------------------------------------------------------
dnl * Make sure we got some form of gmp
dnl--------------------------------------------------------------------

if test "$HAVE_GMP" = "NO"; then
  AC_MSG_ERROR([cannot find the gmp library on the system.]
    [If you have gmp installed in a non-standard location re-run ./configure]
    [and specify the flags --with-gmp-includes= and/or --with-gmp-libraries=])
fi


AC_SUBST(GMP_INCLUDE_DIRS)
AC_SUBST(GMP_LIBS)
AC_SUBST(GMP_LIB_DIRS)
AC_SUBST(GMP_FRAMEWORK)

AC_CONFIG_FILES([integer.buildinfo])

dnl--------------------------------------------------------------------
dnl * Generate the header cbits/GmpDerivedConstants.h
dnl--------------------------------------------------------------------

AC_MSG_NOTICE([generating GmpDerivedConstants.h])
${CC} cbits/mkGmpDerivedConstants.c -o cbits/mkGmpDerivedConstants
cbits/mkGmpDerivedConstants > cbits/GmpDerivedConstants.h
rm cbits/mkGmpDerivedConstants

AC_OUTPUT
