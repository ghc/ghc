dnl EMSDK_VERSION()
dnl ---------------
dnl
dnl Check the version of emsdk, if we're building the JavaScript backend
dnl (the test is skipped if the target is not JavaScript)
dnl
dnl If you change the version requirements, please also update:
dnl https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/tools
dnl
dnl $1 = minimum version (inclusive), or "" for no minimum
dnl $2 = maximum version (not inclusive), or "" for no maximum
dnl $3 = exact version required, or "" for no exact version (used for bindist)

AC_DEFUN([EMSDK_VERSION],
[
    if test "$TargetArch_CPP" = "javascript"
    then
        AC_MSG_CHECKING(emsdk version)
        EmsdkVersion=`$CC -v 2>&1 | sed -n -e 's/^emcc[[^0-9]]*\([[0-9.]]*\).*/\1/gp'`
        if test "$EmsdkVersion" = ""
        then
            AC_MSG_ERROR([could not determine emsdk version. Perhaps CC is not emcc?])
        else
            AC_MSG_RESULT($EmsdkVersion)
            if test "$1" != ""
            then
                FP_COMPARE_VERSIONS([$EmsdkVersion],[-lt],[$1],
                   [AC_MSG_ERROR([emsdk version $1 or later is required to compile the GHC JavaScript backend.])])[]
            fi
            if test "$2" != ""
            then
                FP_COMPARE_VERSIONS([$EmsdkVersion],[-ge],[$2],
                   [AC_MSG_ERROR([emsdk version earlier than $2 is required to compile the GHC JavaScript backend.])])[]
            fi
            if test "$3" != ""
            then
                FP_COMPARE_VERSIONS([$EmsdkVersion],[-ne],[$3],
                   [AC_MSG_ERROR([emsdk version $3 is required for this build of GHC])])[]
            fi
        fi
    fi
])
