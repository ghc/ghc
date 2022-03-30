# FP_SET_CFLAGS_C99
# ----------------------------------
# figure out which CFLAGS are needed to place the compiler into C99 mode
# $1 is name of CC variable (unmodified)
# $2 is name of CC flags variable (augmented if needed)
# $3 is name of CPP flags variable (augmented if needed)
AC_DEFUN([FP_SET_CFLAGS_C99],
[
    AC_PROG_CC
    FP_COPY_SHELLVAR([CC],[fp_save_CC])
    FP_COPY_SHELLVAR([CFLAGS],[fp_save_CFLAGS])
    FP_COPY_SHELLVAR([CPPFLAGS],[fp_save_CPPFLAGS])
    FP_COPY_SHELLVAR([ac_cv_prog_cc_c99],[fp_save_cc_c99])

    dnl set local state
    CC="$$1"
    CFLAGS="$$2"
    CPPFLAGS="$$3"
    unset ac_cv_prog_cc_c99

    dnl perform detection
    AC_PROG_CC
    fp_cc_c99="$ac_cv_prog_cc_c99"
    case "x$ac_cv_prog_cc_c99" in
      x)   ;; # noop
      xno) AC_MSG_ERROR([$CC does not appear to be C99-compatible]) ;;
      *)   $2="$$2 $ac_cv_prog_cc_c99"
           $3="$$3 $ac_cv_prog_cc_c99"
           ;;
    esac

    dnl restore saved state
    FP_COPY_SHELLVAR([fp_save_CC],[CC])
    FP_COPY_SHELLVAR([fp_save_CFLAGS],[CFLAGS])
    FP_COPY_SHELLVAR([fp_save_CPPFLAGS],[CPPFLAGS])
    FP_COPY_SHELLVAR([fp_save_cc_c99],[ac_cv_prog_cc_c99])

    dnl cleanup
    unset fp_save_CC
    unset fp_save_CFLAGS
    unset fp_save_cc_c99
])
