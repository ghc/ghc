# FP_PROG_LD_SINGLE_MODULE
# ----------------------------
# Sets the output variable LdHasSingleModule to YES if the darwin ld supports
# -single_module, or NO otherwise.
#
# In XCode 15, -single_module is a default and passing it as a flag raises a
# warning.
AC_DEFUN([FP_PROG_LD_SINGLE_MODULE],
[
AC_CACHE_CHECK([whether ld supports -single_module], [fp_cv_ld_single_module],
[
case $target in
  *-darwin)
    echo 'int foo(int x) { return x*x; }' > conftest.c
    echo 'extern int foo(int); int main() { return foo(5); }' > conftestmain.c
    "$CC" -c -o conftestmain.o conftestmain.c
    "$CC" -shared -o conftest.dylib conftest.c
    if "$CC" -Wl,-single_module -o conftest conftestmain.o conftest.dylib 2>&1 | grep obsolete > /dev/null; then
      fp_cv_ld_single_module=no
    else
      fp_cv_ld_single_module=yes
    fi
    rm -rf conftest* ;;
  *)
    fp_cv_ld_single_module=no ;;
esac
])
FP_CAPITALIZE_YES_NO(["$fp_cv_ld_single_module"], [LdHasSingleModule])
AC_SUBST([LdHasSingleModule])
])# FP_PROG_LD_SINGLE_MODULE
