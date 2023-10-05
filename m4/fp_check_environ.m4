# FP_CHECK_ENVIRON
# -----------------
AC_DEFUN([FP_CHECK_ENVIRON],
[
  dnl--------------------------------------------------------------------
  dnl * Check whether the libc headers provide a declaration for the
  dnl environ symbol. If not then we will provide one in RtsSymbols.c.
  dnl See #20512, #20577, #20861.
  dnl
  dnl N.B. Windows declares environ in <stdlib.h>; most others declare it
  dnl in <unistd.h>.
  dnl--------------------------------------------------------------------
  AC_CHECK_DECLS([environ], [], [], [
    #include <stdlib.h>
    #include <unistd.h>
  ])
])
