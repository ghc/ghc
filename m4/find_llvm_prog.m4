# FIND_LLVM_PROG()
# --------------------------------
# Find where the llvm tools are. We have a special function to handle when they
# are installed with a version suffix (e.g., llc-7, llc-7.0, llc7) and without (e.g.
# llc).
#
# $1 = the variable to set
# $2 = the command to look for
# $3 = the lower bound version of the command to look for
# $4 = the upper bound version of the command to look for.
#
AC_DEFUN([FIND_LLVM_PROG],[
    # Test for program with and without version name.
    PROG_VERSION_CANDIDATES=$(for llvmVersion in `seq $(($4-1)) -1 $3`; do echo "$2-$llvmVersion $2-$llvmVersion.0 $2$llvmVersion"; done)
    AC_CHECK_TOOLS([$1], [$PROG_VERSION_CANDIDATES $2], [])
    AS_IF([test x"$$1" != x],[
        PROG_VERSION=`$$1 --version | sed -n -e 's/.*version \(\([[0-9]][[0-9]]*\.\)\([[0-9]][[0-9]]*\.\)*[[0-9]][[0-9]]*\).*/\1/gp'`
        AS_IF([test x"$PROG_VERSION" = x],
          [AC_MSG_RESULT(no)
           $1=""
           AC_MSG_NOTICE([We only support llvm $3 upto $4 (non-inclusive) (no version found).])],
          [AC_MSG_CHECKING([$$1 version ($PROG_VERSION) is between $3 and $4])
           AX_COMPARE_VERSION([$PROG_VERSION], [lt], [$3],
            [AC_MSG_RESULT(no)
             $1=""
             AC_MSG_NOTICE([We only support llvm $3 upto $4 (non-inclusive) (found $PROG_VERSION).])],
            [AX_COMPARE_VERSION([$PROG_VERSION], [ge], [$4],
             [AC_MSG_RESULT(no)
              $1=""
              AC_MSG_NOTICE([We only support llvm $3 upto $4 (non-inclusive) (found $PROG_VERSION).])],
             [AC_MSG_RESULT(yes)])])])])
])
