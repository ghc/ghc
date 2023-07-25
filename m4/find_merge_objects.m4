# FIND_MERGE_OBJECTS
# ------------------
# Find which linker to use to merge object files.
#
# See Note [Merging object files for GHCi] in GHC.Driver.Pipeline.
AC_DEFUN([FIND_MERGE_OBJECTS],[
    AC_REQUIRE([FIND_LD])

    if test -z "$MergeObjsCmd"; then
        MergeObjsCmd="$LD"
    fi
    if test -z "$MergeObjsArgs"; then
        MergeObjsArgs="-r"
    fi

    CHECK_FOR_GOLD_T22266($MergeObjsCmd)
    if test "$result" = "1"; then
        AC_MSG_NOTICE([$MergeObjsCmd is broken due to binutils 22266, looking for another linker...])
        MergeObjsCmd=""
        AC_CHECK_TARGET_TOOL([MergeObjsCmd], [ld])
        CHECK_FOR_GOLD_T22266($MergeObjsCmd)
        if test "$result" = "1"; then
            AC_MSG_ERROR([Linker is affected by binutils 22266 but couldn't find another unaffected linker. Please set the MergeObjsCmd variable to a functional linker.])
        fi
    fi

    AC_SUBST([MergeObjsCmd])
    AC_SUBST([MergeObjsArgs])
])
