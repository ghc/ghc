# FP_GCC_EXTRA_FLAGS
# ------------------
# Determine which extra flags we need to pass gcc when we invoke it
# to compile .hc code.
#
# -fwrapv is needed for gcc to emit well-behaved code in the presence of
# integer wrap around. (#952)
#
AC_DEFUN([FP_GCC_EXTRA_FLAGS],
[AC_REQUIRE([FP_GCC_VERSION])
AC_CACHE_CHECK([for extra options to pass gcc when compiling via C], [fp_cv_gcc_extra_opts],
[
 if test "$Unregisterised" = "YES"; then
   # These used to be conditioned on gcc version but we no longer support
   # GCC versions which lack support for these flags
   fp_cv_gcc_extra_opts="-fwrapv -fno-builtin"
 fi
])
AC_SUBST([GccExtraViaCOpts],$fp_cv_gcc_extra_opts)
])
