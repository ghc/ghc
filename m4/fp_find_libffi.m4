dnl ** Have libffi?
dnl --------------------------------------------------------------
dnl Sets UseSystemLibFFI.
AC_DEFUN([FP_FIND_LIBFFI],
[
  # system libffi

  AC_ARG_WITH([system-libffi],
  [AS_HELP_STRING([--with-system-libffi],
    [Use system provided libffi for RTS [default=no]])
  ])

  AS_IF([test "x$with_system_libffi" = "xyes"],
    [UseSystemLibFFI="YES"],
    [UseSystemLibFFI="NO"]
  )

  AC_ARG_WITH([ffi-includes],
  [AS_HELP_STRING([--with-ffi-includes=ARG],
    [Find includes for libffi in ARG [default=system default]])
  ],
  [
   if test "x$UseSystemLibFFI" != "xYES"; then
      AC_MSG_WARN([--with-ffi-includes will be ignored, --with-system-libffi not set])
   else
      FFIIncludeDir="$withval"
      LIBFFI_CFLAGS="-I$withval"
   fi
  ])

  AC_SUBST(FFIIncludeDir)

  AC_ARG_WITH([ffi-libraries],
  [AS_HELP_STRING([--with-ffi-libraries=ARG],
    [Find libffi in ARG [default=system default]])
  ],
  [
   if test "x$UseSystemLibFFI" != "xYES"; then
      AC_MSG_WARN([--with-ffi-libraries will be ignored, --with-system-libffi not set])
   else
      FFILibDir="$withval" LIBFFI_LDFLAGS="-L$withval"
   fi
  ])

  AC_SUBST(FFILibDir)

  AS_IF([test "$UseSystemLibFFI" = "YES"], [
   CFLAGS2="$CFLAGS"
   CFLAGS="$LIBFFI_CFLAGS $CFLAGS"
   LDFLAGS2="$LDFLAGS"
   LDFLAGS="$LIBFFI_LDFLAGS $LDFLAGS"

   if test "$HostOS" = "openbsd";
   then
     # OpenBSD's libffi is not directly linked to the libpthread but
     # still requires pthread functionality. This means that any client
     # binary which links with libffi also needs to link with
     # libpthread. If it does not, then linking fails with unresolved
     # symbols.
     LDFLAGS="$LDFLAGS -lpthread"
   fi

   AC_CHECK_LIB(ffi, ffi_call,
    [AC_CHECK_HEADERS(
      [ffi.h],
      [AC_DEFINE([HAVE_SYSTEM_LIBFFI], [1], [Define to 1 if you have libffi.])],
      [AC_MSG_ERROR([Cannot find ffi.h for system libffi])]
     )],
    [AC_MSG_ERROR([Cannot find system libffi])]
   )

   CFLAGS="$CFLAGS2"
   LDFLAGS="$LDFLAGS2"
  ])
])

