# FP_PROG_AR
# ----------
# Sets fp_prog_ar to a path to ar. Exits if no ar can be found
# The host normalization on Windows breaks autoconf, it no longer
# thinks that target == host so it never checks the unqualified
# tools for Windows. See #14274.
AC_DEFUN([FP_PROG_AR],
[AC_SUBST(fp_prog_ar,$AR)
if test -z "$fp_prog_ar"; then
  if test "$HostOS" = "mingw32"
  then
    AC_PATH_PROG([fp_prog_ar], [ar])
    FP_CANONICALISE_WIN_PATH([fp_prog_ar])
  else
    AC_CHECK_TARGET_TOOL([AR], [ar])
    fp_prog_ar="$AR"
  fi
fi
if test -z "$fp_prog_ar"; then
  AC_MSG_ERROR([cannot find ar in your PATH, no idea how to make a library])
fi
])# FP_PROG_AR
