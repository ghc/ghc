# FP_FIND_LIBDW
# --------------------------------------------------------------
# Should we used libdw? (yes, no, or auto.)
#
# Sets variables:
#   - UseLibdw: [YES|NO]
#   - LibdwLibDir: optional path
#   - LibdwIncludeDir: optional path
AC_DEFUN([FP_FIND_LIBDW],
[
  AC_ARG_WITH([libdw-libraries],
    [AS_HELP_STRING([--with-libdw-libraries=ARG],
      [Find libraries for libdw in ARG [default=system default]])
    ],
    [
      LibdwLibDir="$withval"
      LIBDW_LDFLAGS="-L$withval"
    ])

  AC_ARG_WITH([libdw-includes],
    [AS_HELP_STRING([--with-libdw-includes=ARG],
      [Find includes for libdw in ARG [default=system default]])
    ],
    [
      LibdwIncludeDir="$withval"
      LIBDW_CFLAGS="-I$withval"
    ])

  AC_ARG_ENABLE(dwarf-unwind,
    [AS_HELP_STRING([--enable-dwarf-unwind],
      [Enable DWARF unwinding support in the runtime system via elfutils' libdw [default=no]])],
    [],
    [enable_dwarf_unwind=no])

  UseLibdw=NO
  if test "$enable_dwarf_unwind" != "no" ; then
    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBDW_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBDW_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADER([elfutils/libdwfl.h],
      [AC_CHECK_LIB(dw, dwfl_attach_state,
        [UseLibdw=YES])])

    if test "x:$enable_dwarf_unwind:$UseLibdw" = "x:yes:NO" ; then
      AC_MSG_ERROR([Cannot find system libdw (required by --enable-dwarf-unwind)])
    fi

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi
])
