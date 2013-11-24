
dnl--------------------------------------------------------------------
dnl * Check whether this machine has gmp/gmp3 installed
dnl--------------------------------------------------------------------

AC_DEFUN([LOOK_FOR_GMP_LIB],[
    if test "$HaveFrameworkGMP" = "NO"
    then
        AC_CHECK_LIB([gmp],  [__gmpz_powm_sec],
                     [HaveLibGmp=YES; GMP_LIBS=gmp])
        if test "$HaveLibGmp" = "NO"
        then
            AC_CHECK_LIB([gmp3], [__gmpz_powm_sec],
                         [HaveLibGmp=YES; GMP_LIBS=gmp3])
        fi
    fi
])

dnl--------------------------------------------------------------------
dnl * Mac OS X only: check for GMP.framework
dnl--------------------------------------------------------------------

AC_DEFUN([LOOK_FOR_GMP_FRAMEWORK],[
    if test "$HaveLibGmp" = "NO"
    then
        case $target_os in
        darwin*)
            AC_MSG_CHECKING([for GMP.framework])
            save_libs="$LIBS"
            LIBS="-framework GMP"
            AC_TRY_LINK_FUNC(__gmpz_powm_sec,
                             [HaveFrameworkGMP=YES; GMP_FRAMEWORK=GMP])
            LIBS="$save_libs"
            AC_MSG_RESULT([$HaveFrameworkGMP])
            ;;
        esac
    fi
])

