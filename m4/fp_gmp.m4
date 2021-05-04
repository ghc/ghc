# FP_GMP
# -------------
AC_DEFUN([FP_GMP],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us gmp is somewhere odd
  dnl--------------------------------------------------------------------

  AC_ARG_WITH([gmp-includes],
    [AS_HELP_STRING([--with-gmp-includes],
      [directory containing gmp.h])],
      [GMP_INCLUDE_DIRS=$withval])

  AC_ARG_WITH([gmp-libraries],
    [AS_HELP_STRING([--with-gmp-libraries],
      [directory containing gmp library])],
      [GMP_LIB_DIRS=$withval])

  AC_ARG_WITH([intree-gmp],
    [AS_HELP_STRING([--with-intree-gmp],
      [force using the in-tree GMP])],
      [GMP_FORCE_INTREE=YES],
      [GMP_FORCE_INTREE=NO])

  AC_ARG_WITH([gmp-framework-preferred],
    [AS_HELP_STRING([--with-gmp-framework-preferred],
      [on OSX, prefer the GMP framework to the gmp lib])],
      [GMP_PREFER_FRAMEWORK=YES],
      [GMP_PREFER_FRAMEWORK=NO])

  AC_SUBST(GMP_INCLUDE_DIRS)
  AC_SUBST(GMP_LIB_DIRS)
  AC_SUBST(GMP_FORCE_INTREE)
  AC_SUBST(GMP_PREFER_FRAMEWORK)
])# FP_GMP
