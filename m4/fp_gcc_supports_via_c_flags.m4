# FP_GCC_SUPPORTS_VIA_C_FLAGS
# ---------------------------
# Make sure GCC supports the flags passed by GHC when compiling via C
AC_DEFUN([FP_GCC_SUPPORTS_VIA_C_FLAGS],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether CC supports flags passed by GHC when compiling via C])
   echo 'int main() { return 0; }' > conftest.c
   if $CC -fwrapv -fno-builtin -Werror -x c conftest.c -o conftest > conftest.txt 2>&1 && ! grep -i unrecognized conftest.txt > /dev/null 2>&1; then
       AC_MSG_RESULT([yes])
   else
       AC_MSG_RESULT([no])
       AC_MSG_ERROR([gcc must support the flags -fwrapv and -fno-builtin])
   fi
   rm -f conftest.c conftest.o conftest
])

