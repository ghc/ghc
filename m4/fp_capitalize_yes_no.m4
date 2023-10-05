# FP_CAPITALIZE_YES_NO(scrutinee, out_var)
# ---------------------------------------------------
# Helper for converting a yes/no to a YES/NO.
#
# $1 = shell expression evaluating to yes/no.
# $2 = name of variable to set wit YES/NO.
AC_DEFUN([FP_CAPITALIZE_YES_NO],
[AS_CASE(
    [x$1],
    [x"yes"], [$2=YES],
    [x"no"], [$2=NO],
    [AC_MSG_ERROR([invalid value:] $1 [is not "yes" or "no"])])
])
