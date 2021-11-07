# GHC_UNREGISTERISED
# --------------------------------
#  Do an unregisterised build?
AC_DEFUN([GHC_UNREGISTERISED],
[
  AC_MSG_CHECKING(whether target supports a registerised ABI)
  case "$TargetArch" in
      i386|x86_64|powerpc|powerpc64|powerpc64le|s390x|arm|aarch64|riscv64)
          UnregisterisedDefault=NO
          AC_MSG_RESULT([yes])
          ;;
      *)
          UnregisterisedDefault=YES
          AC_MSG_RESULT([no])
          ;;
  esac
  FP_DEFAULT_CHOICE_OVERRIDE_CHECK(
    [unregisterised],
    [unregisterised],
    [registerised],
    [Unregisterised],
    [Build a toolchain with the unregisterised ABI (disabled by default on platforms with registerised support)],
    [NO],
    [YES],
    [no])
])
