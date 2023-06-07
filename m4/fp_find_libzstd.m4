AC_DEFUN([FP_FIND_LIBZSTD],
[
  dnl ** Is IPE data compression enabled?
  dnl --------------------------------------------------------------
  AC_ARG_ENABLE(
      ipe-data-compression,
      [AS_HELP_STRING(
          [--enable-ipe-data-compression],
          [Enable compression of info table provenance map entries using the
          zstd compression library [default=no]]
        )],
      [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableIpeDataCompression])],
      [EnableIpeDataCompression=NO]
    )

  if test "$EnableIpeDataCompression" = "YES"; then
    dnl ** Have zstd?
    dnl --------------------------------------------------------------
    AC_ARG_WITH(
        libzstd-libraries,
        [AS_HELP_STRING(
            [--with-libzstd-libraries=ARG],
            [Find libraries for libzstd in ARG [default=system default]]
          )],
        [
          LibZstdLibDir="$withval"
          LIBZSTD_LDFLAGS="-L$withval"
        ]
      )

    AC_SUBST(LibZstdLibDir)

    AC_ARG_WITH(
        libzstd-includes,
        [AS_HELP_STRING(
            [--with-libzstd-includes=ARG],
            [Find includes for libzstd in ARG [default=system default]]
          )],
        [
          LibZstdIncludeDir="$withval"
          LIBZSTD_CFLAGS="-I$withval"
        ]
      )

    AC_SUBST(LibZstdIncludeDir)

    HaveLibZstd=0

    CFLAGS2="$CFLAGS"
    CFLAGS="$LIBZSTD_CFLAGS $CFLAGS"
    LDFLAGS2="$LDFLAGS"
    LDFLAGS="$LIBZSTD_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([zstd.h])

    if test "$ac_cv_header_zstd_h" = "yes" ; then
      AC_CHECK_LIB(zstd,ZSTD_versionString,HaveLibZstd=1)
    fi
    if test "$HaveLibZstd" = "0" ; then
      AC_MSG_ERROR(
            [Cannot find system libzstd (required by
            --enable-ipe-data-compression)]
          )
    fi

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi


  AC_DEFINE_UNQUOTED([HAVE_LIBZSTD], [$HaveLibZstd], [Define to 1 if you
  wish to compress IPE data in compiler results (requires libzstd)])
  if test $HaveLibZstd = "1" ; then
    AC_SUBST([UseLibZstd],[YES])
    AC_SUBST([CabalHaveLibZstd],[True])
  else
    AC_SUBST([UseLibZstd],[NO])
    AC_SUBST([CabalHaveLibZstd],[False])
  fi
])
