# FP_BINUTILS_T23856()
# -------------------
# Check for binutils bug #23856 present in some versions of ld.gold
# Used by FIND_LD.
#
# $1 = the platform
#
AC_DEFUN([FP_BINUTILS_T23856],[
  AC_REQUIRE([FP_GCC_SUPPORTS_NO_PIE])
  if test "$CONF_GCC_SUPPORTS_NO_PIE" = "YES"; then
      AC_MSG_CHECKING([whether ld.gold exhibits weak relocation bug (binutils 23856)])
      echo 'int main() { return 0; }' > conftest.c
      $CC -fuse-ld=gold -no-pie conftest.c -o conftest || AC_MSG_ERROR([Failed to compile test])
      if ./conftest; then
        AC_MSG_RESULT([not affected])
      else
        AC_MSG_RESULT([affected])
        AC_MSG_ERROR([The ld.gold linker is affected by binutils bug 23856; please specify a non-broken linker by passing LD=/path/to/ld to ./configure])
      fi
  fi
])
