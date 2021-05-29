# GHC_CONVERT_VENDOR(vendor, target_var)
# --------------------------------
# converts vendor from gnu to ghc naming, and assigns the result to $target_var
AC_DEFUN([GHC_CONVERT_VENDOR],[
  case "$1" in
  pc|gentoo|w64) # like i686-pc-linux-gnu, i686-gentoo-freebsd8, x86_64-w64-mingw32
    $2="unknown"
    ;;
  softfloat) # like armv5tel-softfloat-linux-gnueabi
    $2="unknown"
    ;;
  hardfloat) # like armv7a-hardfloat-linux-gnueabi
    $2="unknown"
    ;;
  *)
    #pass thru by default
    $2="$1"
    ;;
  esac
])
