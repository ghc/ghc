dnl FP_CC_SUPPORTS__ATOMICS
dnl ------------------------
dnl Does C compiler support C11 atomics?
AC_DEFUN([FP_CC_SUPPORTS__ATOMICS],
[
    AC_REQUIRE([AC_PROG_CC])
    AC_MSG_CHECKING([whether C compiler supports C11 atomics])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
        [[#include <stdatomic.h>
          int x;]],
        [[atomic_load(&x); return x;]])],
    [
        AC_MSG_RESULT(yes)

        need_latomic=0
        AC_MSG_CHECKING(whether -latomic is needed for sub-word-sized atomic operations)
        AC_LINK_IFELSE([AC_LANG_PROGRAM(
                [[#include <stdatomic.h>
                  unsigned char a;]],
                [[atomic_fetch_or_explicit(&a, 1, memory_order_relaxed);]])],
        [
            AC_MSG_RESULT(no)
        ],
        [
            _save_LIBS="$LIBS"
            LIBS="-latomic"
            AC_LINK_IFELSE([AC_LANG_PROGRAM(
                [[#include <stdatomic.h>
                  unsigned char a;]],
                [[atomic_fetch_or_explicit(&a, 1, memory_order_relaxed);]])],
            [
                AC_MSG_RESULT(yes)
                need_latomic=1
            ],
            [
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR([sub-word-sized atomic operations are not available.])
            ])
            LIBS="$_save_LIBS"
        ])
        AC_MSG_CHECKING(whether -latomic is needed for 64-bit atomic operations)
        AC_LINK_IFELSE([AC_LANG_PROGRAM(
            [[
            #include <inttypes.h>
            #include <stdatomic.h>
            uint64_t a;
            ]], [[atomic_fetch_or_explicit(&a, 1, memory_order_relaxed);]])],
        [
            AC_MSG_RESULT(no)
        ],
        [
            _save_LIBS="$LIBS"
            LIBS="-latomic"
            AC_LINK_IFELSE([AC_LANG_PROGRAM(
                [[
                #include <inttypes.h>
                #include <stdatomic.h>
                uint64_t a;
                ]], [[atomic_fetch_or_explicit(&a, 1, memory_order_relaxed);]])],
            [
                AC_MSG_RESULT(yes)
                need_latomic=1
            ],
            [
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR([64-bit atomic operations are not available.])
            ])
            LIBS="$_save_LIBS"
        ])
    ],
    [
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([C compiler needs to support C11 atomic primitives.])
    ])
])
