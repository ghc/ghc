# FP_PROG_AR_NEEDS_RANLIB
# -----------------------
# Sets the output variable RANLIB_CMD to "ranlib" if it is needed and
# found, to "true" otherwise. Sets REAL_RANLIB_CMD to the ranlib program,
# even if we don't need ranlib (libffi might still need it).
AC_DEFUN([FP_PROG_AR_NEEDS_RANLIB],[
    AC_REQUIRE([FP_PROG_AR_IS_GNU])
    AC_REQUIRE([FP_PROG_AR_ARGS])
    AC_REQUIRE([AC_PROG_CC])

    AC_PROG_RANLIB

    if test $fp_prog_ar_is_gnu = yes
    then
        fp_cv_prog_ar_needs_ranlib=no
    elif test "$TargetVendor_CPP" = "apple"
    then
        # It's quite tedious to check for Apple's crazy timestamps in
        # .a files, so we hardcode it.
        fp_cv_prog_ar_needs_ranlib=yes
    else
        case $fp_prog_ar_args in
        *s*)
            fp_cv_prog_ar_needs_ranlib=no;;
        *)
            fp_cv_prog_ar_needs_ranlib=yes;;
        esac
    fi

    # workaround for AC_PROG_RANLIB which sets RANLIB to `:' when
    # ranlib is missing on the target OS. The problem is that
    # other programs cannot execute `:' which is a shell built-in but can
    # execute `true' which is usually simple program supported by the
    # OS.
    # Fixes #8795
    if test "$RANLIB" = ":"
    then
        RANLIB="true"
    fi
    REAL_RANLIB_CMD="$RANLIB"
    if test $fp_cv_prog_ar_needs_ranlib = yes
    then
        RANLIB_CMD="$RANLIB"
    else
        RANLIB_CMD="true"
    fi
    AC_SUBST([REAL_RANLIB_CMD])
    AC_SUBST([RANLIB_CMD])
    AC_SUBST([ArNeedsRanLib],[`echo $fp_cv_prog_ar_needs_ranlib | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_NEEDS_RANLIB
