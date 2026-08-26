# FP_CANONICALISE_WIN_PATH(VARIABLE)
# ----------------------------------
# On Windows, rewrite the value of the path VARIABLE to the mixed form:
#   C:/foo/bar
#
# See Note [MSYS paths] in hadrian/src/Hadrian/Utilities.hs.
#
# Requires FPTOOLS_SET_PLATFORMS_VARS to have been run, for $windows.
AC_DEFUN([FP_CANONICALISE_WIN_PATH],
[AS_IF([test "$windows" = "YES" && test -n "$$1"],
       [$1=`cygpath -m "$$1"`])])# FP_CANONICALISE_WIN_PATH
