# FIND_PYTHON
# -----------
# Find the version of `python` to use (for the testsuite driver)
#
AC_DEFUN([FIND_PYTHON],[
    dnl Prefer the mingw64 distribution on Windows due to #17483.
    AC_PATH_PROG([PYTHON], [python3], [], [/mingw64/bin $PATH])
    PythonCmd="$PYTHON"
    AC_SUBST([PythonCmd])
])
