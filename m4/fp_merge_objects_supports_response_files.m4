# FP_MERGE_OBJECTS_SUPPORTS_RESPONSE_FILES
# --------------------
# See if whether we are using a version of the merge objects tool which supports response files.
AC_DEFUN([FP_MERGE_OBJECTS_SUPPORTS_RESPONSE_FILES], [
    AC_MSG_CHECKING([whether $LD supports response files])
    echo 'int funA(int x) {return x;}' > conftesta.c
    echo 'int funB(int x) {return x;}' > conftestb.c
    "$CC" -c -o conftesta.o conftesta.c > /dev/null 2>&1
    "$CC" -c -o conftestb.o conftestb.c > /dev/null 2>&1
    printf -- "-o\nconftest.o\nconftesta.o\nconftestb.o\n" > args.txt
    "$MergeObjsCmd" "$MergeObjsArgs" @args.txt > /dev/null 2>&1
    if ("$NM" conftest.o | grep "funA" > /dev/null 2>&1) && ("$NM" conftest.o | grep "funB" > /dev/null 2>&1)
    then
        MergeObjsSupportsResponseFiles=YES
        AC_MSG_RESULT([yes])
    else
        MergeObjsSupportsResponseFiles=NO
        AC_MSG_RESULT([no])
    fi
    rm -f conftesta.c conftestb.c conftesta.o conftestb.o conftest.o args.txt
    AC_SUBST(MergeObjsSupportsResponseFiles)
])
