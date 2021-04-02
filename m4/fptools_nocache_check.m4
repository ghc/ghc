dnl FPTOOLS_NOCACHE_CHECK prints a message, then sets the
dnl values of the second argument to the result of running
dnl the commands given by the third. It does not cache its
dnl result, so it is suitable for checks which should be
dnl run every time.
dnl
AC_DEFUN([FPTOOLS_NOCACHE_CHECK],
[AC_MSG_CHECKING([$1])
 $3
 AC_MSG_RESULT([$][$2])
])
