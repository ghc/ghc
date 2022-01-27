# GHC_CONVERT_CPU(cpu, target_var)
# --------------------------------
# Converts cpu from gnu to ghc naming, and assigns the result to $target_var.
# Should you modify this list, you are invited to reflect the changes in
# `libraries/base/System/Info.hs`'s documentation.
AC_DEFUN([GHC_CONVERT_CPU],[
case "$1" in
  aarch64*|arm64*)
    $2="aarch64"
    ;;
  alpha*)
    $2="alpha"
    ;;
  arm*)
    $2="arm"
    ;;
  hppa1.1*)
    $2="hppa1_1"
    ;;
  hppa*)
    $2="hppa"
    ;;
  i386|i486|i586|i686)
    $2="i386"
    ;;
  ia64)
    $2="ia64"
    ;;
  m68k*)
    $2="m68k"
    ;;
  mipseb*)
    $2="mipseb"
    ;;
  mipsel*)
    $2="mipsel"
    ;;
  mips*)
    $2="mips"
    ;;
  nios2)
    $2="nios2"
    ;;
  powerpc64le*)
    $2="powerpc64le"
    ;;
  powerpc64*)
    $2="powerpc64"
    ;;
  powerpc*)
    $2="powerpc"
    ;;
  riscv64*)
    $2="riscv64"
    ;;
  riscv|riscv32*)
    $2="riscv32"
    ;;
  rs6000)
    $2="rs6000"
    ;;
  s390x*)
    $2="s390x"
    ;;
  s390*)
    $2="s390"
    ;;
  sh4)
    $2="sh4"
    ;;
  vax)
    $2="vax"
    ;;
  x86_64|amd64)
    $2="x86_64"
    ;;
  *)
    echo "Unknown CPU $1"
    exit 1
    ;;
  esac
])
