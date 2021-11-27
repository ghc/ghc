# FP_PROG_LD_FILELIST
# -------------------
# Sets the output variable LdHasFilelist to YES if ld supports
# -filelist, or NO otherwise.
AC_DEFUN([FP_PROG_LD_FILELIST],
[
AC_CACHE_CHECK([whether ld understands -filelist], [fp_cv_ld_has_filelist],
[
    echo 'int foo() { return 0; }' > conftest1.c
    echo 'int bar() { return 0; }' > conftest2.c
    ${CC-cc} -c conftest1.c
    ${CC-cc} -c conftest2.c
    echo conftest1.o  > conftest.o-files
    echo conftest2.o >> conftest.o-files
    if ${LdCmd} -r -filelist conftest.o-files -o conftest.o > /dev/null 2>&1
    then
        fp_cv_ld_has_filelist=yes
    else
        fp_cv_ld_has_filelist=no
    fi
    rm -rf conftest*
])
FP_CAPITALIZE_YES_NO(["$fp_cv_ld_has_filelist"], [LdHasFilelist])
AC_SUBST([LdHasFilelist])
])# FP_PROG_LD_FILELIST
