dnl FP_GCC_SUPPORTS__ATOMICS
dnl ------------------------
dnl Does gcc support the __atomic_* family of builtins?
AC_DEFUN([FP_GCC_SUPPORTS__ATOMICS],
[
    AC_REQUIRE([AC_PROG_CC])
    AC_MSG_CHECKING([whether GCC supports __atomic_ builtins])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[int x, y;]], [[__atomic_load(&x, &y, __ATOMIC_SEQ_CST); return y]])],
    [
        AC_MSG_RESULT(yes)
        AC_MSG_CHECKING(whether -latomic is needed for sub-word-sized atomic operations)
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[unsigned char a;]], [[__atomic_fetch_or(&a, 1, __ATOMIC_RELAXED);]])],
        [
            AC_MSG_RESULT(no)
            AC_SUBST([CabalNeedLibatomic],[False])
            need_latomic=0
        ],
        [
            _save_LIBS=$LIBS
            LIBS="-latomic"
            AC_LINK_IFELSE([AC_LANG_PROGRAM([[unsigned char a;]], [[__atomic_fetch_or(&a, 1, __ATOMIC_RELAXED);]])],
            [
                AC_MSG_RESULT(yes)
                AC_SUBST([CabalNeedLibatomic],[True])
                need_latomic=1
            ],
            [
                AC_MSG_RESULT(failed)
                AC_MSG_ERROR([sub-word-sized atomic operations are not available.])
            ])
            LIBS=$_save_LIBS
        ])
    ],
    [
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([C compiler needs to support __atomic primitives.])
    ])
    AC_DEFINE([HAVE_C11_ATOMICS], [1], [Does GCC support __atomic primitives?])
    AC_DEFINE_UNQUOTED([NEED_ATOMIC_LIB], [$need_latomic],
        [Define to 1 if we need -latomic for sub-word atomic operations.])
])
