# FIND_GHC_BOOTSTRAP_PROG()
# --------------------------------
# Parse the bootstrap GHC's compiler settings file for the location of things
# like the `llc` and `opt` commands.
#
# $1 = the variable to set
# $2 = The bootstrap compiler.
# $3 = The string to grep for to find the correct line.
#
AC_DEFUN([FIND_GHC_BOOTSTRAP_PROG],[
    BootstrapTmpCmd=`grep $3 $($2 --print-libdir)/settings 2>/dev/null | sed 's/.*", "//;s/".*//'`
    if test -n "$BootstrapTmpCmd" && test `basename $BootstrapTmpCmd` = $BootstrapTmpCmd ; then
        AC_PATH_PROG([$1], [$BootstrapTmpCmd], "")
    else
        $1=$BootstrapTmpCmd
    fi
])
