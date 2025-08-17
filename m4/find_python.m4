# FIND_PYTHON
# -----------
# Find a usable Python interpreter and enforce a minimum version.
# Exports:
#   - PYTHON: Path to the interpreter for build/test scripts
#   - PythonCmd: Canonicalised path used by Hadrian and config files
#   - PythonVersion: Detected Python version (X.Y.Z)
#
AC_DEFUN([FIND_PYTHON],[
    dnl Prefer the mingw64 distribution on Windows due to #17483.
    AC_PATH_PROG([PYTHON], [python3], [], [/mingw64/bin $PATH])

    dnl Fall back to "python" if python3 isn’t found (version checked below)
    AS_IF([test -z "$PYTHON"], [
        AC_PATH_PROG([PYTHON], [python], [], [/mingw64/bin $PATH])
    ])

    dnl If still not found, hard error: we require Python >= 3.7
    AS_IF([test -z "$PYTHON"], [
        AC_MSG_ERROR([Python 3.7 or later is required but no python interpreter was found. Please install Python >= 3.7 and re-run configure.])
    ])

    dnl Query the version string (X.Y.Z) of the selected interpreter
    AC_CACHE_CHECK([for version of python], [fp_cv_python_version], [
        changequote(, )dnl
        fp_cv_python_version=`"$PYTHON" -c "import sys; sys.stdout.write('%d.%d.%d' % sys.version_info[:3])" 2>/dev/null`
        changequote([, ])dnl
    ])
    PythonVersion=$fp_cv_python_version
    AC_SUBST([PythonVersion])

    dnl Enforce minimum version 3.7.0
    AS_IF([test -z "$PythonVersion"], [
        AC_MSG_ERROR([Failed to determine Python version for $PYTHON])
    ])
    FP_COMPARE_VERSIONS([$PythonVersion], [-lt], [3.7.0], [
        AC_MSG_ERROR([Python 3.7 or later is required, but $PYTHON reports $PythonVersion])
    ])

    dnl Canonicalise path for Windows
    if test "$HostOS" = "mingw32"
    then
      PythonCmd=$(cygpath -m "$PYTHON")
    else
      PythonCmd="$PYTHON"
    fi
    AC_SUBST([PythonCmd])
])
