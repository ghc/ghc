dnl --------------------------------------------------------------------------
dnl Set RISC-V architecture with vector extension (RVV)
dnl
dnl This macro checks if the target is RISC-V and if so, sets -march=rv64gcv /
dnl -march=rv32gcv (general, compressed, vector) to enable vector extension
dnl --------------------------------------------------------------------------

# FP_RISCV_MARCH(compiler_flags_var)
# ------------------------------------------
#
# Example usage:
# FP_RISCV_MARCH([CONF_CC_OPTS_STAGE2])
#
AC_DEFUN([FP_RISCV_MARCH],
[
  AC_REQUIRE([AC_CANONICAL_TARGET])

  # Check if target is RISC-V
  case "$target" in
    riscv64*-*-*)
      AC_MSG_NOTICE([add -march=rv64gcv to $1])

      # Add vector extension flag to the specified variable
      $1="$$1 -march=rv64gcv"
      ;;
 
    riscv32*-*-*)
      AC_MSG_NOTICE([add -march=rv64gcv to $1])

      # Add vector extension flag to the specified variable
      $1="$$1 -march=rv32gcv"
      ;;
  esac
])
