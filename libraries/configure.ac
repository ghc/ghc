# Initialise and check sanity.
AC_INIT([Haskell hierarchical libraries], [1.0], [libraries@haskell.org], [libraries])
AC_CONFIG_SRCDIR([libraries-footer.txt])

if test -d base; then
  AC_CONFIG_SUBDIRS(base)
fi
if test -d readline; then
  AC_CONFIG_SUBDIRS(readline)
fi
if test -d network; then
  AC_CONFIG_SUBDIRS(network)
fi
if test -d X11; then
  AC_CONFIG_SUBDIRS(X11)
fi
if test -d HGL; then
  AC_CONFIG_SUBDIRS(HGL)
fi
if test -d OpenGL; then
  AC_CONFIG_SUBDIRS(OpenGL)
fi
if test -d GLUT; then
  AC_CONFIG_SUBDIRS(GLUT)
fi
if test -d OpenAL; then
  AC_CONFIG_SUBDIRS(OpenAL)
fi
if test -d ALUT; then
  AC_CONFIG_SUBDIRS(ALUT)
fi
if test -d time; then
  AC_CONFIG_SUBDIRS(time)
fi
if test -d regex-posix; then
  AC_CONFIG_SUBDIRS(regex-posix)
fi

AC_OUTPUT
