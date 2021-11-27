# FP_PROG_LD_BUILD_ID
# ------------
# Sets the output variable LdHasBuildId to YES if ld supports
# --build-id, or NO otherwise.
AC_DEFUN([FP_PROG_LD_BUILD_ID],
[
AC_CACHE_CHECK([whether ld understands --build-id], [fp_cv_ld_build_id],
[echo 'int foo() { return 0; }' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r --build-id=none -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_ld_build_id=yes
else
   fp_cv_ld_build_id=no
fi
rm -rf conftest*])
FP_CAPITALIZE_YES_NO(["$fp_cv_ld_build_id"], [LdHasBuildId])
AC_SUBST([LdHasBuildId])
])# FP_PROG_LD_BUILD_ID


