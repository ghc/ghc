# FP_PROG_CYGPATH
# ----------------
# Record cygpath path for later use by hls on windows.
AC_DEFUN([FP_PROG_CYGPATH],
[
  if test "$HostOS" == "mingw32"; then
    AC_PATH_PROG([CYGPATH_POSIX], [cygpath])

    if test -z "$CYGPATH_POSIX"; then
      AC_MSG_ERROR([cygpath not found; Windows path conversion unavailable])
      CYGPATH=""
    else
      # Convert the POSIX path of the cygpath executable into a Windows path
      CYGPATH=`"$CYGPATH_POSIX" -m "$CYGPATH_POSIX"`
      AC_MSG_RESULT([using cygpath at $CYGPATH])
    fi

    AC_SUBST([CYGPATH])
  fi
])