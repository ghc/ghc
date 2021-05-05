# FP_PROG_AR_IS_GNU
# -----------------
# Sets fp_prog_ar_is_gnu to yes or no, depending on whether it is GNU ar or not.
AC_DEFUN([FP_PROG_AR_IS_GNU],
[AC_REQUIRE([FP_PROG_AR])
AC_CACHE_CHECK([whether $fp_prog_ar is GNU ar], [fp_cv_prog_ar_is_gnu],
[if "$fp_prog_ar" --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_prog_ar_is_gnu=yes
else
  fp_cv_prog_ar_is_gnu=no
fi])
fp_prog_ar_is_gnu=$fp_cv_prog_ar_is_gnu
AC_SUBST([ArIsGNUAr], [`echo $fp_prog_ar_is_gnu | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_IS_GNU
