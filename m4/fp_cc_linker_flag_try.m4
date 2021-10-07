# FP_CC_LINKER_FLAG_TRY()
# --------------------
# Try a particular linker to see whether we can use it. In particular, determine
# whether we can convince gcc to use it via a -fuse-ld=... flag.
#
# $1 = the name of the linker to try
# $2 = the variable to set with the appropriate GHC flag if the linker is
# found to be usable
AC_DEFUN([FP_CC_LINKER_FLAG_TRY], [
    AC_REQUIRE([FP_GCC_SUPPORTS_NO_PIE])
    AC_MSG_CHECKING([whether C compiler supports -fuse-ld=$1])
    args=""
    echo 'int main(void) {return 0;}' > conftest.c
    if $CC -o conftest.o $args -fuse-ld=$1 conftest.c > /dev/null 2>&1
    then
        AC_MSG_RESULT([yes])

        # ld.gold's treatment of position independent code is broken in ways
        # that miscompile musl. See GHC #17508 and binutils issue #23856.
        AC_MSG_CHECKING([whether $1 linker is affected by binutils issue 23856])
        if [ "$CONF_GCC_SUPPORTS_NO_PIE" = "YES" ]; then
            args="$args -no-pie"
        fi
        echo '#include <stdio.h>' > conftest.c
        echo 'int main(void) {printf("hello world\n"); return 0;}' >> conftest.c
        if $CC -o conftest $args -fuse-ld=$1 conftest.c > /dev/null 2>&1 && ./conftest; then
            AC_MSG_RESULT([not affected])
            $2="-fuse-ld=$1"
        else
            AC_MSG_RESULT([affected])
        fi
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest conftest.o
])
