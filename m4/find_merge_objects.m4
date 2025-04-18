# FIND_MERGE_OBJECTS
# ------------------
# Find which linker to use to merge object files.
#
# See Note [Merging object files for GHCi] in GHC.Driver.Pipeline.


AC_DEFUN([CHECK_MERGE_OBJECTS],[
  AC_REQUIRE([FP_FIND_NM])
  AC_MSG_NOTICE([Checking whether $MergeObjsCmd can merge objects])
  echo 'int funA(int x) {return x;}' > conftesta.c
  echo 'int funB(int x) {return x;}' > conftestb.c
  "$CC" -c -o conftesta.o conftesta.c
  "$CC" -c -o conftestb.o conftestb.c
  $MergeObjsCmd $MergeObjsArgs conftesta.o conftestb.o -o conftestc.o || AC_MSG_ERROR([ $MergeObjsCmd could not merge objects ])

  # Check the resulting object file has both functions.
  "$NM" conftestc.o | grep funA > /dev/null 2>&1 || AC_MSG_ERROR([ $MergeObjsCmd could not merge objects ])
  "$NM" conftestc.o | grep funB > /dev/null 2>&1 || AC_MSG_ERROR([ $MergeObjsCmd could not merge objects ])

  rm -r conftest*.c conftest*.o
])

AC_DEFUN([FIND_MERGE_OBJECTS],[
    AC_REQUIRE([FIND_LD])

    if test -z ${MergeObjsCmd+x}; then
        AC_MSG_NOTICE([Setting cmd])
        MergeObjsCmd="$LD"
    fi
    if test -z ${MergeObjsArgs+x}; then
        MergeObjsArgs="-r"
    fi


    # Note [Empty MergeObjsCmd]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~
    # If MergeObjsCmd="" then we assume that the user is explicitly telling us that
    # they do not want to configure the MergeObjsCmd, this is particularly important for
    # the bundled windows toolchain.
    if test -z "$MergeObjsCmd"; then
      AC_MSG_NOTICE([No command for merging objects as explicitly instructed by user])

    else
      # Check first that gold works
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

      # Now just check that merging objects works at all
      CHECK_MERGE_OBJECTS()

    fi

    AC_SUBST([MergeObjsCmd])
    AC_SUBST([MergeObjsArgs])
])
