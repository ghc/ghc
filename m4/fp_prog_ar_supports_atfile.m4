# FP_PROG_AR_SUPPORTS_ATFILE
# -----------------
# Sets fp_prog_ar_supports_atfile to yes or no, depending on whether
# or not it supports the @file syntax
AC_DEFUN([FP_PROG_AR_SUPPORTS_ATFILE],
[AC_REQUIRE([FP_PROG_AR])
 AC_REQUIRE([FP_PROG_AR_ARGS])
AC_CACHE_CHECK([whether $fp_prog_ar supports @file], [fp_cv_prog_ar_supports_atfile],
[
rm -f conftest*
touch conftest.file
echo conftest.file  > conftest.atfile
echo conftest.file >> conftest.atfile
"$fp_prog_ar" $fp_prog_ar_args conftest.a @conftest.atfile > /dev/null 2>&1
fp_prog_ar_supports_atfile_tmp=`"$fp_prog_ar" t conftest.a 2> /dev/null | grep -c conftest.file`
rm -f conftest*
if test "$fp_prog_ar_supports_atfile_tmp" -eq 2
then
  fp_cv_prog_ar_supports_atfile=yes
else
  fp_cv_prog_ar_supports_atfile=no
fi])
fp_prog_ar_supports_atfile=$fp_cv_prog_ar_supports_atfile
AC_SUBST([ArSupportsAtFile], [`echo $fp_prog_ar_supports_atfile | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_SUPPORTS_ATFILE

