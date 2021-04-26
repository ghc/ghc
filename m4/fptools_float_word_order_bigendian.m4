# FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN
# ----------------------------------
# Little endian ARM on Linux with some ABIs has big endian word order
# in doubles. Define FLOAT_WORDS_BIGENDIAN if this is the case.
AC_DEFUN([FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN],
  [AC_CACHE_CHECK([whether float word order is big endian], [fptools_cv_float_word_order_bigendian],
    [AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM(
        [#include <endian.h>],
        [#if defined(__FLOAT_WORD_ORDER) && __FLOAT_WORD_ORDER == BIG_ENDIAN
             return 0;
         #else
             not float word order big endian
         #endif]
      )],
      [fptools_cv_float_word_order_bigendian=yes],
      [fptools_cv_float_word_order_bigendian=no])
    ])
  case $fptools_cv_float_word_order_bigendian in
      yes)
          AC_DEFINE([FLOAT_WORDS_BIGENDIAN], 1,
          [Define to 1 if your processor stores words of floats with
           the most significant byte first]) ;;
  esac
])
