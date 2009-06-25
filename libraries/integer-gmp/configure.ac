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
             [HaveLibGmp=YES; GMP_LIBS=gmp],
             [HaveLibGmp=NO;  GMP_LIBS=])
if test "$HaveLibGmp" = "NO"; then
AC_CHECK_LIB([gmp3], [__gmpz_fdiv_qr],
             [HaveLibGmp=YES; GMP_LIBS=gmp3],
             [HaveLibGmp=NO;  GMP_LIBS=])
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
      [HaveFrameworkGMP=YES; GMP_FRAMEWORK=GMP; GMP_LIBS=],
      [HaveFrameworkGMP=NO])
    LIBS="$save_libs"
    AC_MSG_RESULT([$HaveFrameworkGMP])
    ;;
esac

AC_CHECK_HEADER([gmp.h], , [HaveFrameworkGMP=NO; HaveLibGmp=NO; GMP_LIBS=])

dnl--------------------------------------------------------------------
dnl * Make sure we got some form of gmp
dnl--------------------------------------------------------------------

AC_SUBST(GMP_INCLUDE_DIRS)
AC_SUBST(GMP_LIBS)
AC_SUBST(GMP_LIB_DIRS)
AC_SUBST(GMP_FRAMEWORK)
AC_SUBST(HaveLibGmp)
AC_SUBST(HaveFrameworkGMP)

AC_CONFIG_FILES([integer-gmp.buildinfo gmp/config.mk])

dnl--------------------------------------------------------------------
dnl * Generate the header cbits/GmpDerivedConstants.h
dnl--------------------------------------------------------------------

AC_OUTPUT
