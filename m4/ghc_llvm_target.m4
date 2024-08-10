# GHC_LLVM_TARGET(target, target_cpu, target_vendor, target_os, llvm_target_var)
# --------------------------------
# converts the canonicalized target into something llvm can understand
AC_DEFUN([GHC_LLVM_TARGET], [
  llvm_target_cpu=$2
  case "$1" in
    *-freebsd*-gnueabihf)
      llvm_target_vendor="unknown"
      llvm_target_os="freebsd-gnueabihf"
      ;;
    *-hardfloat-*eabi)
      llvm_target_vendor="unknown"
      llvm_target_os="$4""hf"
      ;;
    *-mingw32|*-mingw64|*-msys)
      llvm_target_vendor="unknown"
      llvm_target_os="windows"
      ;;
    # apple is a bit about their naming scheme for
    # aarch64; and clang on macOS doesn't know that
    # aarch64 would be arm64. So for LLVM we'll need
    # to call it arm64; while we'll refer to it internally
    # as aarch64 for consistency and sanity.
    aarch64-apple-*|arm64-apple-*)
      llvm_target_cpu="arm64"
      GHC_CONVERT_VENDOR([$3],[llvm_target_vendor])
      GHC_CONVERT_OS([$4],[$2],[llvm_target_os])
      ;;
    *)
      GHC_CONVERT_VENDOR([$3],[llvm_target_vendor])
      GHC_CONVERT_OS([$4],[$2],[llvm_target_os])
      ;;
  esac
  case "$4" in
      # retain any android and gnueabi linux flavours
      # for the LLVM Target. Otherwise these would be
      # turned into just `-linux` and fail to be found
      # in the `llvm-targets` file.
      *-android*|*-gnueabi*|*-musleabi*)
        GHC_CONVERT_VENDOR([$3],[llvm_target_vendor])
        llvm_target_os="$4"
        ;;
  esac
  $5="$llvm_target_cpu-$llvm_target_vendor-$llvm_target_os"
])

# GHC_LLVM_TARGET_SET_VAR
# -----------------------
# Sets the cannonical target variable. This stub exists so other macros can
# require it.
AC_DEFUN([GHC_LLVM_TARGET_SET_VAR], [
  AC_REQUIRE([FPTOOLS_SET_PLATFORMS_VARS])
  if test "$bootstrap_llvm_target" != ""
  then
    LlvmTarget=$bootstrap_llvm_target
  else
    GHC_LLVM_TARGET([$target],[$target_cpu],[$target_vendor],[$target_os],[LlvmTarget])
  fi
])

# GHC_LLVM_HOST_TARGET_SET_VAR
# -----------------------
#
# Sets the cannonical target variable of the host. This stub exists so other
# macros can require it.
AC_DEFUN([GHC_LLVM_HOST_TARGET_SET_VAR], [
  AC_REQUIRE([FPTOOLS_SET_PLATFORMS_VARS])
  GHC_LLVM_TARGET([$host],[$host_cpu],[$host_vendor],[$host_os],[LlvmHostTarget])
])
