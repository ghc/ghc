# FP_FIND_NM
# ---------------------
# Find nm and verify that it works.
AC_DEFUN([FP_FIND_NM],
[
    if test "$HostOS" != "mingw32"; then
        AC_CHECK_TARGET_TOOL([NM], [nm])
        if test "$NM" = ":"; then
            AC_MSG_ERROR([cannot find nm in your PATH])
        fi
    fi
    if test "$HostOS" = "mingw32"
    then
      NmCmd=$(cygpath -m "$NM")
    else
      NmCmd="$NM"
    fi
    AC_SUBST([NmCmd])
    if test "$HostOS" != "mingw32"; then
        AC_CHECK_TOOL([NM_STAGE0], [nm])
        if test "$NM_STAGE0" = ":"; then
            AC_MSG_ERROR([cannot find nm stage0 in your PATH])
        fi
    fi
    NmCmdStage0="$NM_STAGE0"
    AC_SUBST([NmCmdStage0])
	

    if test "$TargetOS_CPP" = "darwin"
    then
        AC_MSG_CHECKING(whether nm program is broken)
        # Some versions of Xcode ship a broken version of `nm`. Detect and work
        # around this issue. See : https://gitlab.haskell.org/ghc/ghc/issues/11744
        nmver=$(${NM} --version | grep version | sed 's/ //g')
        case "$nmver" in
            LLVMversion7.3.0|LLVMversion7.3.1)
                AC_MSG_RESULT(yes)
                echo "The detected nm program is broken."
                echo
                echo "See: https://gitlab.haskell.org/ghc/ghc/issues/11744"
                echo
                echo "Try re-running configure with:"
                echo
                echo '   NM=$(xcrun --find nm-classic) ./configure'
                echo
                exit 1
                ;;
            *)
                AC_MSG_RESULT(no)
                ;;
            esac
    fi
])
