# FP_VISIBILITY_HIDDEN
# ----------------------------------
# Is the visibility hidden attribute supported?
AC_DEFUN([FP_VISIBILITY_HIDDEN],
[
    AC_MSG_CHECKING([whether __attribute__((visibility("hidden"))) is supported])
    echo '__attribute__((visibility("hidden"))) void foo(void) {}' > conftest.c
    if $CC -Wall -Werror -c conftest.c > /dev/null 2>&1
    then
        AC_MSG_RESULT([yes])
        AC_DEFINE(HAS_VISIBILITY_HIDDEN, 1, [Has visibility hidden])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest.o
])
