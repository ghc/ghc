# GHC_UNREGISTERISED
# --------------------------------
#  Do an unregisterised build?
AC_DEFUN([GHC_UNREGISTERISED],
[
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
