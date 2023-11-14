# FP_PROG_LD_NO_COMPACT_UNWIND
# ----------------------------
# Sets the output variable LdHasNoCompactUnwind to YES if ld supports
# -no_compact_unwind, or NO otherwise.
AC_DEFUN([FP_PROG_LD_NO_COMPACT_UNWIND],
[
AC_CACHE_CHECK([whether ld understands -no_compact_unwind], [fp_cv_ld_no_compact_unwind],
[
case $target in
  *-darwin)
    echo 'int foo() { return 0; }' > conftest.c
    "${CC-cc}" -c conftest.c
    if "$LD" -r -no_compact_unwind -o conftest2.o conftest.o > /dev/null 2>&1; then
      fp_cv_ld_no_compact_unwind=yes
    else
      fp_cv_ld_no_compact_unwind=no
    fi
    rm -rf conftest* ;;
  *)
    fp_cv_ld_no_compact_unwind=no ;;
esac
])
FP_CAPITALIZE_YES_NO(["$fp_cv_ld_no_compact_unwind"], [LdHasNoCompactUnwind])
AC_SUBST([LdHasNoCompactUnwind])
])# FP_PROG_LD_NO_COMPACT_UNWIND
