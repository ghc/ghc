dnl GHC_ADJUSTORS_METHOD(Platform)
dnl --------------------------------------------------------------
dnl Use libffi for adjustors?
AC_DEFUN([GHC_ADJUSTORS_METHOD],
[
  case [$]{$1[Arch]} in
      i386|x86_64|javascript)
          # We have native adjustor support on these platforms
          HaveNativeAdjustor=yes
          ;;
      *)
          HaveNativeAdjustor=no
          ;;
  esac

  AC_ARG_ENABLE(libffi-adjustors,
      [AS_HELP_STRING(
          [--enable-libffi-adjustors],
          [Force use of libffi for adjustors, even on platforms which have support for more efficient, native adjustors.])],
      UseLibffiForAdjustors=$enableval,
      dnl do nothing
  )

  AC_MSG_CHECKING([whether to use libffi for adjustors])
  if test "$UseLibffiForAdjustors" = "yes" ; then
      # Use libffi is the user explicitly requested it
      AdjustorType="libffi"
  elif test "$HaveNativeAdjustor" = "yes"; then
      # Otherwise if we have a native adjustor implementation use that
      AdjustorType="native"
  else
      # If we don't have a native adjustor implementation then default to libffi
      AdjustorType="libffi"
  fi

  case "$AdjustorType" in
  libffi)
      UseLibffiForAdjustors=YES
      AC_MSG_RESULT([yes])
      ;;
  native)
      UseLibffiForAdjustors=NO
      AC_MSG_RESULT([no])
      ;;
  *)
      AC_MSG_ERROR([Internal error: Invalid AdjustorType])
      exit 1
  esac
])
