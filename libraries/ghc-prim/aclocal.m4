# FP_GCC_SUPPORTS__ATOMICS
# ------------------------
# Does gcc support the __atomic_* family of builtins?
AC_DEFUN([FP_GCC_SUPPORTS__ATOMICS],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether GCC supports __atomic_ builtins])
   echo 'int test(int *x) { int y; __atomic_load(&x, &y, __ATOMIC_SEQ_CST); return x; }' > conftest.c
   if $CC -c conftest.c > /dev/null 2>&1; then
       CONF_GCC_SUPPORTS__ATOMICS=YES
       AC_MSG_RESULT([yes])
   else
       CONF_GCC_SUPPORTS__ATOMICS=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest.o
])
