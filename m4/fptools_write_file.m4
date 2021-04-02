# FPTOOLS_WRITE_FILE
# ------------------
# Write $2 to the file named $1.
AC_DEFUN([FPTOOLS_WRITE_FILE],
[
cat >$1 <<ACEOF
$2
ACEOF
])
