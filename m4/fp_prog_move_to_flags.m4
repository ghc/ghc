# MOVE_TO_FLAGS
# --------------------------------
# Split off flags from $1 (the compiler) to $2 (the flags).
# This works around autoconf setting $CC and $CXX to be a program plus the C11
# `-std=...11` flag (#24324), starting from autotools 2.70.
AC_DEFUN([MOVE_TO_FLAGS],[

dnl Use IFS=' ' to split off the command from the arguments in $1.
dnl By expanding $$1, set accounts for quoting correctly, such that splitting
dnl e.g. '"A B/C" D' results in "A B/C" and "D".
tmp_IFS="$IFS"
IFS=' '
eval set -- $$1
IFS="$tmp_IFS"

$1="[$]1"
shift
$2="[$]@ $$2"
])
