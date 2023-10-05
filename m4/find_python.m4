# FIND_PYTHON
# -----------
# Find the version of `python` to use (for the testsuite driver)
#
AC_DEFUN([FIND_PYTHON],[
    dnl Prefer the mingw64 distribution on Windows due to #17483.
    AC_PATH_PROG([PYTHON], [python3], [], [/mingw64/bin $PATH])
    if test "$HostOS" = "mingw32"
    then
      PythonCmd=$(cygpath -m "$PYTHON")
    else
      PythonCmd="$PYTHON"
    fi
    AC_SUBST([PythonCmd])
])
