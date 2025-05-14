# FIND_LD
# ---------
# Find the version of `ld` to use and figure out how to get gcc to use it for
# linking (if --enable-ld-override is enabled). This is used in both in the top
# level configure.ac and in distrib/configure.ac.in.
#
# $1 = the platform
# $2 = the variable to set with GHC options to configure gcc to use the chosen linker
#
AC_DEFUN([FIND_LD],[
    AC_ARG_ENABLE(ld-override,
      [AS_HELP_STRING([--disable-ld-override],
        [Prevent GHC from overriding the default linker used by gcc. If ld-override is enabled GHC will try to tell gcc to use whichever linker is selected by the LD environment variable. [default=override enabled]])],
      [],
      [enable_ld_override=yes])

    find_ld() {
        # Make sure the user didn't specify LD manually.
        if test "z$LD" != "z"; then
            AC_CHECK_TARGET_TOOL([LD], [ld])
            return
        fi

        linkers="ld.lld ld"

        # Manually iterate over possible names since we want to ensure that, e.g.,
        # if ld.lld is installed but gcc doesn't support -fuse-ld=lld, that we
        # then still try ld.gold and -fuse-ld=gold.
        for possible_ld in $linkers; do
            TmpLd="" # In case the user set LD
            AC_CHECK_TARGET_TOOL([TmpLd], [$possible_ld])
            if test "x$TmpLd" = "x"; then continue; fi

            out=`$TmpLd --version`
            case $out in
              "GNU ld"*)
                   FP_CC_LINKER_FLAG_TRY(bfd, $2) ;;
              "GNU gold"*)
                   FP_CC_LINKER_FLAG_TRY(gold, $2)
                   if test "$cross_compiling" = "yes"; then
                       AC_MSG_NOTICE([Using ld.gold and assuming that it is not affected by binutils issue 22266]);
                   fi
                   ;;
              "LLD"*)
                   FP_CC_LINKER_FLAG_TRY(lld, $2) ;;
              *" LLD "*)
                   FP_CC_LINKER_FLAG_TRY(lld, $2) ;;
              *) AC_MSG_NOTICE([unknown linker version $out]) ;;
            esac
            if test "z$$2" = "z"; then
                AC_MSG_NOTICE([unable to convince '$CC' to use linker '$TmpLd'])
                # a terrible hack to prevent autoconf from caching the previous
                # AC_CHECK_TARGET_TOOL result since next time we'll be looking
                # for another ld variant.
                $as_unset ac_cv_prog_ac_ct_TmpLd
            else
                LD="$TmpLd"
                return
            fi
        done

        # Fallback
        AC_CHECK_TARGET_TOOL([LD], [ld])
    }

    case "$target" in
    *-darwin)
        dnl N.B. Don't even try to find a more efficient linker on Darwin where
        dnl broken setups (e.g. unholy mixtures of Homebrew and the native
        dnl toolchain) are far too easy to come across.
        dnl
        dnl See #21712.
        AC_CHECK_TARGET_TOOL([LD], [ld])
        ;;
    *-linux*|*-mingw32)
        if test "x$enable_ld_override" = "xyes"; then
            find_ld
        else
            AC_CHECK_TARGET_TOOL([LD], [ld])
        fi
        ;;
    *)
        AC_CHECK_TARGET_TOOL([LD], [ld])
        ;;
    esac
    CHECK_LD_COPY_BUG([$1])
])

