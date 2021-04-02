# FP_PROG_LD_IS_GNU
# -----------------
# Sets the output variable LdIsGNULd to YES or NO, depending on whether it is
# GNU ld or not.
AC_DEFUN([FP_PROG_LD_IS_GNU],[
AC_CACHE_CHECK([whether ld is GNU ld], [fp_cv_gnu_ld],
[[if ${LdCmd} --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_gnu_ld=YES
else
  fp_cv_gnu_ld=NO
fi]])
AC_SUBST([LdIsGNULd],["$fp_cv_gnu_ld"])
])# FP_PROG_LD_IS_GNU
