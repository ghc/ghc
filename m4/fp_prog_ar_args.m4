# FP_PROG_AR_ARGS
# ---------------
# Sets fp_prog_ar_args to the arguments for ar and the output variable ArCmd
# to an invocation of ar including these arguments.
AC_DEFUN([FP_PROG_AR_ARGS],
[AC_REQUIRE([FP_PROG_AR_IS_GNU])
AC_CACHE_CHECK([for ar arguments], [fp_cv_prog_ar_args],
[
# GNU ar needs special treatment: it appears to have problems with
# object files with the same name if you use the 's' modifier, but
# simple 'ar q' works fine, and doesn't need a separate ranlib.
if test $fp_prog_ar_is_gnu = yes; then
  fp_cv_prog_ar_args="q"
else
  touch conftest.dummy
  for fp_var in qclsZ qcls qcs qcl qc ; do
     rm -f conftest.a
     if "$fp_prog_ar" $fp_var conftest.a conftest.dummy > /dev/null 2> /dev/null ; then
       # Also check that a result was created; it seems some llvm-ar versions
       # exit with code zero even if they fail to parse the command line.
       if test -f conftest.a ; then
         fp_cv_prog_ar_args=$fp_var
         break
       fi
     fi
  done
  rm -f conftest*
  if test -z "$fp_cv_prog_ar_args"; then
    AC_MSG_ERROR([cannot figure out how to use your $fp_prog_ar])
  fi
fi])
fp_prog_ar_args=$fp_cv_prog_ar_args
if test "$HostOS" = "mingw32"
then
  ArCmd=$(cygpath -m "$fp_prog_ar")
else
  ArCmd="$fp_prog_ar"
fi
AC_SUBST([ArCmd])
AC_SUBST([ArArgs], ["$fp_prog_ar_args"])

])# FP_PROG_AR_ARGS
