# FP_CURSES
# -------------
AC_DEFUN([FP_CURSES],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us curses is somewhere odd
  dnl--------------------------------------------------------------------

  AC_ARG_WITH([curses-includes],
    [AS_HELP_STRING([--with-curses-includes],
      [directory containing curses headers])],
      [CURSES_INCLUDE_DIRS=$withval])

  AC_ARG_WITH([curses-libraries],
    [AS_HELP_STRING([--with-curses-libraries],
      [directory containing curses libraries])],
      [CURSES_LIB_DIRS=$withval])

  AC_SUBST(CURSES_INCLUDE_DIRS)
  AC_SUBST(CURSES_LIB_DIRS)
])# FP_CURSES
