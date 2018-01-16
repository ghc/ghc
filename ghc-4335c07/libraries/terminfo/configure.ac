AC_INIT([Haskell terminfo package], [0.2], [judah dot jacobson at gmail dot com], [terminfo])

AC_ARG_WITH([curses-libraries],
  [AC_HELP_STRING([--with-curses-libraries],
    [directory containing curses library])],
    [curses_libraries=$withval],
    [curses_libraries=NONE])


TERMINFO_LIB_DIRS=
if test "x$curses_libraries" != "xNONE"; then
  LDFLAGS="-L$curses_libraries $LDFLAGS"
  TERMINFO_LIB_DIRS=$curses_libraries
fi

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_CHECK_LIB(tinfow, setupterm, HaveLibCurses=YES; LibCurses=tinfow,
  [AC_CHECK_LIB(tinfo, setupterm, HaveLibCurses=YES; LibCurses=tinfo,
    [AC_CHECK_LIB(ncursesw, setupterm, HaveLibCurses=YES; LibCurses=ncursesw,
      [AC_CHECK_LIB(ncurses, setupterm, HaveLibCurses=YES; LibCurses=ncurses,
        [AC_CHECK_LIB(curses, setupterm, HaveLibCurses=YES; LibCurses=curses,
          HaveLibCurses=NO; LibCurses=not-installed)])])])])

if test "x$HaveLibCurses" = "xNO" ; then
    AC_MSG_FAILURE([curses library not found, so this package cannot be built])
else
    TERMINFO_LIB=$LibCurses
fi


AC_SUBST(TERMINFO_LIB_DIRS)
AC_SUBST(TERMINFO_LIB)


AC_CONFIG_FILES([terminfo.buildinfo])
AC_OUTPUT
