# FP_RISCV_CHECK_GCC_VERSION
#
# We cannot use all GCC versions that are generally supported: Up to
# (including) GCC 13, GCC does not support the expected C calling convention
# for vectors. Thus, we require at least GCC 14.
#
# Details: GCC 13 expects vector arguments to be passed on stack / by
# reference, though the "Standard Vector Calling Convention Variant"
# (https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-cc.adoc#standard-vector-calling-convention-variant)
# - which is the new default (e.g. for GCC 14) - expects vector arguments in
# registers v8 to v23. I guess, this is due to the "Standard Vector Calling
# Convention Variant" being pretty new. And, the GCC implementors had to make
# up design decissions before this part of the standard has been ratified.
# As long as the calling convention is consistently used for all code, this
# isn't an issue. But, we have to be able to call C functions compiled by GCC
# with code emitted by GHC.

AC_DEFUN([FP_RISCV_CHECK_GCC_VERSION], [
  AC_REQUIRE([FP_GCC_VERSION])
  AC_REQUIRE([AC_CANONICAL_TARGET])
  #
  # Check if target is RISC-V
  case "$target" in
    riscv64*-*-*)
      AC_MSG_NOTICE([Assert GCC version for RISC-V. Detected version is $GccVersion])
      if test -n "$GccVersion"; then
        AC_CACHE_CHECK([risc-v version of gcc], [fp_riscv_check_gcc_version], [
            FP_COMPARE_VERSIONS([$GccVersion], [-lt], [14.0],
                                [AC_MSG_ERROR([Need at least GCC version 14 for RISC-V])],
                                [AC_MSG_RESULT([good])]
                                )
        ])
      fi
      ;;
    # Ignore riscv32*-*-* as we don't have a NCG for RISC-V 32bit targets
  esac
])
