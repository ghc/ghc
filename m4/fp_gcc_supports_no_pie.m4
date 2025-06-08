# FP_GCC_SUPPORTS_NO_PIE
# ----------------------
# Does gcc support the -no-pie option? If so we should pass it to gcc when
# joining objects since -pie may be enabled by default.
AC_DEFUN([FP_GCC_SUPPORTS_NO_PIE],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether CC supports -no-pie])
   echo 'int main() { return 0; }' > conftest.c
   "$CC" -c conftest.c
   # Some GCC versions only warn when passed an unrecognized flag.
   if "$CC" -no-pie -Werror conftest.o -o conftest > conftest.txt 2>&1 && ! grep -i unrecognized conftest.txt > /dev/null 2>&1; then
       CONF_GCC_SUPPORTS_NO_PIE=YES
       AC_MSG_RESULT([yes])
   else
       CONF_GCC_SUPPORTS_NO_PIE=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest.o conftest
])
