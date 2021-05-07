# --------------------------------------------------------------
# Calculate absolute path to build tree
# --------------------------------------------------------------

AC_DEFUN([FP_FIND_ROOT],[
AC_MSG_CHECKING(for path to top of build tree)
    if test "$windows" = YES
    then
      dnl Make sure this is a c:/foo/bar (mixed) style path. Some parts of
      dnl the build system might depend on it (such as the sed expression
      dnl `"s|$(TOP)/||i"` in addCFileDeps in rules/build-dependencies.mk).
      hardtop=$(cygpath -m "$(pwd)")
    else
      hardtop=$(pwd)
    fi

    dnl Remove common automounter nonsense
    hardtop=`echo $hardtop | sed 's|^/tmp_mnt.*\(/local/.*\)$|\1|' | sed 's|^/tmp_mnt/|/|'`

    if ! test -d "$hardtop"; then
        AC_MSG_ERROR([cannot determine current directory])
    fi

    dnl We don't support building in directories with spaces.
    case "$hardtop" in
    *' '*)
        AC_MSG_ERROR([
        The build system does not support building in a directory
        containing space characters.
        Suggestion: move the build tree somewhere else.])
        ;;
    esac

    AC_SUBST(hardtop)

    AC_MSG_RESULT($hardtop)
])
