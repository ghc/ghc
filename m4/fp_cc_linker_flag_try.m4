# FP_CC_LINKER_FLAG_TRY()
# --------------------
# Try a particular linker to see whether we can use it. In particular, determine
# whether we can convince gcc to use it via a -fuse-ld=... flag.
#
# $1 = the name of the linker to try
# $2 = the variable to set with the appropriate GHC flag if the linker is
# found to be usable
AC_DEFUN([FP_CC_LINKER_FLAG_TRY], [
    AC_MSG_CHECKING([whether C compiler supports -fuse-ld=$1])
    echo 'int main(void) {return 0;}' > conftest.c
    if $CC -o conftest.o -fuse-ld=$1 $LDFLAGS conftest.c > /dev/null 2>&1
    then
        $2="-fuse-ld=$1"
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest.o
])
