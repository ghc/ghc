# FP_PROG_CONTEXT_DIFF
# --------------------
# Figure out how to do context diffs. Sets the output variable ContextDiffCmd.
#
# Note: NeXTStep thinks diff'ing a file against itself is "trouble".
AC_DEFUN([FP_PROG_CONTEXT_DIFF],
[AC_CACHE_CHECK([for a working context diff], [fp_cv_context_diff],
[echo foo > conftest1
echo foo > conftest2
fp_cv_context_diff=no
for fp_var in '-U 1' '-u1' '-C 1' '-c1'
do
  if diff $fp_var conftest1 conftest2 > /dev/null 2>&1; then
    fp_cv_context_diff="diff $fp_var"
    break
  fi
done])
if test x"$fp_cv_context_diff" = xno; then
   AC_MSG_ERROR([cannot figure out how to do context diffs])
fi
AC_SUBST(ContextDiffCmd, [$fp_cv_context_diff])
])# FP_PROG_CONTEXT_DIFF

