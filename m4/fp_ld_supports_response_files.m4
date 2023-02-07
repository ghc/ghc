# FP_LD_SUPPORTS_RESPONSE_FILES
# --------------------
# See if whether we are using a version of ld which
# supports response files.
AC_DEFUN([FP_LD_SUPPORTS_RESPONSE_FILES], [
    AC_MSG_CHECKING([whether $LD supports response files])
    echo 'int main(void) {return 0;}' > conftest.c
    $CC -c -o conftest.o conftest.c > /dev/null 2>&1
    if $LD @<(printf '%q\n' -shared -o conftest conftest.o) > /dev/null 2>&1 || $LD @<(printf '%q\n' -dylib -o conftest conftest.o) > /dev/null 2>&1
    then
        LdSupportsResponseFiles=YES
        AC_MSG_RESULT([yes])
    else
        LdSupportsResponseFiles=NO
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest
    AC_SUBST(LdSupportsResponseFiles)
])
