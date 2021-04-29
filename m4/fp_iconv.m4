# FP_ICONV
# -------------
AC_DEFUN([FP_ICONV],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us iconv is somewhere odd
  dnl--------------------------------------------------------------------

  dnl Note: ICONV_LIB_DIRS and ICONV_INCLUDE_DIRS are not predefined
  dnl to the empty string to allow them to be overridden from the
  dnl environment.

  AC_ARG_WITH([iconv-includes],
    [AS_HELP_STRING([--with-iconv-includes],
      [directory containing iconv.h])],
      [ICONV_INCLUDE_DIRS=$withval])

  AC_ARG_WITH([iconv-libraries],
    [AS_HELP_STRING([--with-iconv-libraries],
      [directory containing iconv library])],
      [ICONV_LIB_DIRS=$withval])

  AC_SUBST(ICONV_INCLUDE_DIRS)
  AC_SUBST(ICONV_LIB_DIRS)
])# FP_ICONV
