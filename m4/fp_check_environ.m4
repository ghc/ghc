# FP_CHECK_ENVIRON
# -----------------
AC_DEFUN([FP_CHECK_ENVIRON],
[
  dnl--------------------------------------------------------------------
  dnl * Check whether the libc headers provide a declaration for the
  dnl environ symbol. If not then we will provide one in RtsSymbols.c. 
  dnl See #20512, #20577, #20861.
  dnl--------------------------------------------------------------------
  AC_CHECK_DECLS([environ], [], [], [
    #include <unistd.h>
  ])
])

