# FP_MUSTTAIL
# ----------------------------------
# Is the musttail attribute supported?
AC_DEFUN([FP_MUSTTAIL],
[
    AC_MSG_CHECKING([whether __attribute__((musttail)) is supported])
    echo 'extern int foo(void); int bar(void) { __attribute__((musttail)) return foo(); }' > conftest.c
    if $CC -c conftest.c -o conftest.o > /dev/null 2>&1
    then
        AC_MSG_RESULT([yes])
        AC_DEFINE(HAS_MUSTTAIL, 1, [Has musttail])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest.o
])
