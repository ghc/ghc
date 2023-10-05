# FP_PROG_AR_SUPPORTS_DASH_L
# -----------------
# Sets fp_prog_ar_supports_dash_l to yes or no, depending on whether
# or not it supports the llvm-ar's -L flag to merge archives.
AC_DEFUN([FP_PROG_AR_SUPPORTS_DASH_L],
[
  AC_REQUIRE([FP_PROG_AR])
  AC_REQUIRE([FP_PROG_AR_ARGS])
  AC_CACHE_CHECK([whether $fp_prog_ar supports -L], [fp_cv_prog_ar_supports_dash_l],
    [
      rm -f conftest*
      touch conftest.file
      touch conftest.a0 conftest.a1 conftest.b0 conftest.b1
      dnl Build two archives, merge them, and check that the result contains the
      dnl original files not the two archives.
      "$fp_prog_ar" qc conftest-a.a conftest.a0 conftest.a1
      "$fp_prog_ar" qc conftest-b.a conftest.b0 conftest.b1
      "$fp_prog_ar" qcL conftest.a conftest-a.a conftest-b.a 2>/dev/null
      if "$fp_prog_ar" t conftest.a | grep -s "conftest.a1" > /dev/null
      then
        fp_cv_prog_ar_supports_dash_l=yes
      else
        fp_cv_prog_ar_supports_dash_l=no
      fi
      rm -f conftest*
    ])
  fp_prog_ar_supports_dash_l=$fp_cv_prog_ar_supports_dash_l
  AC_SUBST([ArSupportsDashL], [`echo $fp_prog_ar_supports_dash_l | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_SUPPORTS_DASH_L

