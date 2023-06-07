AC_DEFUN([FP_FIND_LIBNUMA],
[
  dnl ** Have libnuma?
  dnl --------------------------------------------------------------
  AC_ARG_WITH([libnuma-libraries],
    [AS_HELP_STRING([--with-libnuma-libraries=ARG],
      [Find libraries for libnuma in ARG [default=system default]])
    ],
    [
      LibNumaLibDir="$withval"
      LIBNUMA_LDFLAGS="-L$withval"
    ])

  AC_SUBST(LibNumaLibDir)

  AC_ARG_WITH([libnuma-includes],
    [AS_HELP_STRING([--with-libnuma-includes=ARG],
      [Find includes for libnuma in ARG [default=system default]])
    ],
    [
      LibNumaIncludeDir="$withval"
      LIBNUMA_CFLAGS="-I$withval"
    ])

  AC_SUBST(LibNumaIncludeDir)

  HaveLibNuma=0
  AC_ARG_ENABLE(numa,
      [AS_HELP_STRING([--enable-numa],
          [Enable NUMA memory policy and thread affinity support in the
           runtime system via numactl's libnuma [default=auto]])])

  if test "$enable_numa" = "yes" ; then
    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBNUMA_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBNUMA_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([numa.h numaif.h])

    if test "$ac_cv_header_numa_h$ac_cv_header_numaif_h" = "yesyes" ; then
      AC_CHECK_LIB(numa, numa_available,HaveLibNuma=1)
    fi
    if test "$HaveLibNuma" = "0" ; then
        AC_MSG_ERROR([Cannot find system libnuma (required by --enable-numa)])
    fi

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi

  AC_DEFINE_UNQUOTED([HAVE_LIBNUMA], [$HaveLibNuma], [Define to 1 if you have libnuma])
  if test $HaveLibNuma = "1" ; then
    AC_SUBST([UseLibNuma],[YES])
    AC_SUBST([CabalHaveLibNuma],[True])
  else
    AC_SUBST([UseLibNuma],[NO])
    AC_SUBST([CabalHaveLibNuma],[False])
  fi
])
