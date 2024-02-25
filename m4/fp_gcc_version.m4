# FP_GCC_VERSION
# -----------
# Extra testing of the result AC_PROG_CC, testing the gcc version no. Sets the
# (unsubstituted) output variable GccVersion.
AC_DEFUN([FP_GCC_VERSION], [
  AC_REQUIRE([AC_PROG_CC])
  if test -z "$CC"; then
    AC_MSG_ERROR([C compiler is required])
  fi

  if $CC --version | grep -qi gcc; then
    AC_CACHE_CHECK([version of gcc], [fp_cv_gcc_version],
    [
        # Be sure only to look at the first occurrence of the "version " string;
        # Some Apple compilers emit multiple messages containing this string.
        AC_MSG_CHECKING([version of gcc])
        fp_cv_gcc_version="`$CC -v 2>&1 | sed -n -e '1,/version /s/.*version [[^0-9]]*\([[0-9.]]*\).*/\1/p'`"
        AC_MSG_RESULT([$fp_cv_gcc_version])
        # 4.7 is needed for __atomic_ builtins.
        FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [4.7],
                            [AC_MSG_ERROR([Need at least gcc version 4.7 (newer recommended)])])
    ])
    AC_SUBST([GccVersion], [$fp_cv_gcc_version])
  else
    AC_MSG_NOTICE([\$CC is not gcc; assuming it's a reasonably new C compiler])
  fi
])# FP_GCC_VERSION
