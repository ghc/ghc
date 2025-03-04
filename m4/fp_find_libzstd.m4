# FP_FIND_LIBZSTD
# --------------------------------------------------------------
# Check whether we are we want IPE data compression, whether we have
# libzstd in order to do it, and whether zstd will be statically linked.
#
# Sets variables:
#   - UseLibZstd: [YES|NO]
#   - UseStaticLibZstd: [YES|NO]
#   - LibZstdLibDir: optional path
#   - LibZstdIncludeDir: optional path
AC_DEFUN([FP_FIND_LIBZSTD],
[
  dnl ** Is IPE data compression enabled?
  dnl --------------------------------------------------------------
  AC_ARG_ENABLE(
      ipe-data-compression,
      [AS_HELP_STRING(
          [--enable-ipe-data-compression],
          [Enable compression of info table provenance entries using the
          zstd compression library [default=no]]
        )],
      [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableIpeDataCompression])],
      [EnableIpeDataCompression=NO]
    )

  StaticLibZstd=0
  AC_ARG_ENABLE(
      static-libzstd,
      [AS_HELP_STRING(
          [--enable-static-libzstd],
          [Statically link the libzstd compression library with the compiler
          (not compatible with darwin) [default=no]]
        )],
      [StaticLibZstd=1],
      [StaticLibZstd=0]
    )

  HaveLibZstd=0
  if test "$EnableIpeDataCompression" = "YES"; then
    dnl ** Have zstd >= 1.4.0?
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

    # libzstd >= 1.4.0 is required for IPE data compression

    CFLAGS="$CFLAGS2"
    LDFLAGS="$LDFLAGS2"
  fi

  if test $HaveLibZstd = "1" ; then
    UseLibZstd=YES
    if test $StaticLibZstd = "1" ; then
      case "${host_os}" in
          darwin*)
            AC_MSG_ERROR(
                  [--enable-static-libzstd is not compatible with darwin]
                )
      esac
      UseStaticLibZstd=YES
    else
      UseStaticLibZstd=NO
    fi
  else
    UseLibZstd=NO
  fi
])
