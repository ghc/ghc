dnl Check to see if the C compiler is clang or llvm-gcc
dnl
AC_DEFUN([FP_CC_LLVM_BACKEND],
  [AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether C compiler has an LLVM back end])
   $CC -x c /dev/null -dM -E > conftest.txt 2>&1
   if grep "__llvm__" conftest.txt >/dev/null 2>&1; then
     CcLlvmBackend=YES
     AC_MSG_RESULT([yes])
   else
     CcLlvmBackend=NO
     AC_MSG_RESULT([no])
   fi

   rm -f conftest.txt
])
