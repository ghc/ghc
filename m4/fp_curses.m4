# FP_CURSES
# -------------
AC_DEFUN([FP_CURSES],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us curses is somewhere odd
  dnl--------------------------------------------------------------------

  AC_ARG_WITH([curses-libraries],
    [AS_HELP_STRING([--with-curses-libraries],
      [directory containing curses libraries])],
      [CURSES_LIB_DIRS=$withval])

  AC_SUBST(CURSES_LIB_DIRS)
])# FP_CURSES
