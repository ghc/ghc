# FP_LD_SUPPORTS_RESPONSE_FILES
# --------------------
# See if whether we are using a version of ld which supports response files.
AC_DEFUN([FP_LD_SUPPORTS_RESPONSE_FILES], [
    AC_MSG_CHECKING([whether $LD supports response files])
    echo 'int main(void) {return 0;}' > conftest.c
    "$CC" -c -o conftest.o conftest.c > /dev/null 2>&1
    printf '%q\n' -o conftest conftest.o > args.txt
    if "$LD" -shared @args.txt > /dev/null 2>&1 || "$LD" -dylib @args.txt > /dev/null 2>&1
    then
        LdSupportsResponseFiles=YES
        AC_MSG_RESULT([yes])
    else
        LdSupportsResponseFiles=NO
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest args.txt
    AC_SUBST(LdSupportsResponseFiles)
])
