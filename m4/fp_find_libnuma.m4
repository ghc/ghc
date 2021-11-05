# FP_FIND_LIBNUMA
# --------------------------------------------------------------
# Should we used libnuma? (yes, no, or auto.)
#
# Sets variables:
#   - UseLibNuma: [YES|NO]
#   - LibNumaLibDir: optional path
#   - LibNumaIncludeDir: optional path
AC_DEFUN([FP_FIND_LIBNUMA],
[
  AC_ARG_WITH([libnuma-libraries],
    [AS_HELP_STRING([--with-libnuma-libraries=ARG],
      [Find libraries for libnuma in ARG [default=system default]])
    ],
    [
      LibNumaLibDir="$withval"
      LIBNUMA_LDFLAGS="-L$withval"
    ])

  AC_ARG_WITH([libnuma-includes],
    [AS_HELP_STRING([--with-libnuma-includes=ARG],
      [Find includes for libnuma in ARG [default=system default]])
    ],
    [
      LibNumaIncludeDir="$withval"
      LIBNUMA_CFLAGS="-I$withval"
    ])

  AC_ARG_ENABLE(numa,
    [AS_HELP_STRING([--enable-numa],
      [Enable NUMA memory policy and thread affinity support in the
       runtime system via numactl's libnuma [default=auto]])],
    [],
    [enable_numa=auto])

  UseLibNuma=NO
  if test "$enable_numa" != "no" ; then
    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBNUMA_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBNUMA_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([numa.h numaif.h])

    if test "$ac_cv_header_numa_h:$ac_cv_header_numaif_h" = "yes:yes" ; then
      AC_CHECK_LIB([numa], [numa_available], [UseLibNuma=YES])
    fi
    if test "$enable_numa:$UseLibNuma" = "yes:NO" ; then
      AC_MSG_ERROR([Cannot find system libnuma (required by --enable-numa)])
    fi

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi
])
