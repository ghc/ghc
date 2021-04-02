# FP_PROG_LD_FLAG
# ---------------
# Sets the output variable $2 to $1 if ld supports the $1 flag.
# Otherwise the variable's value is empty.
AC_DEFUN([FP_PROG_LD_FLAG],
[
AC_CACHE_CHECK([whether ld understands $1], [fp_cv_$2],
[echo 'int foo() { return 0; }' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r $1 -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_$2=$1
else
   fp_cv_$2=
fi
rm -rf conftest*])
$2=$fp_cv_$2
])# FP_PROG_LD_FLAG
