dnl ** Have libdw?
dnl --------------------------------------------------------------
dnl Sets UseLibdw.
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

  AC_SUBST(LibdwLibDir)

  AC_ARG_WITH([libdw-includes],
    [AS_HELP_STRING([--with-libdw-includes=ARG],
      [Find includes for libdw in ARG [default=system default]])
    ],
    [
      LibdwIncludeDir="$withval"
      LIBDW_CFLAGS="-I$withval"
    ])

  AC_SUBST(LibdwIncludeDir)

  UseLibdw=NO
  USE_LIBDW=0
  AC_ARG_ENABLE(dwarf-unwind,
      [AS_HELP_STRING([--enable-dwarf-unwind],
          [Enable DWARF unwinding support in the runtime system via elfutils' libdw [default=no]])])
  if test "$enable_dwarf_unwind" = "yes" ; then
    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBDW_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBDW_LDFLAGS $LDFLAGS"

    AC_CHECK_LIB(dw, dwfl_attach_state,
        [AC_CHECK_HEADERS([elfutils/libdw.h], [break], [])
         UseLibdw=YES],
        [AC_MSG_ERROR([Cannot find system libdw (required by --enable-dwarf-unwind)])])

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi

  AC_SUBST(UseLibdw)
  if test $UseLibdw = "YES" ; then
    USE_LIBDW=1
  fi
  AC_DEFINE_UNQUOTED([USE_LIBDW], [$USE_LIBDW], [Set to 1 to use libdw])
])

