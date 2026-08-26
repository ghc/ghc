# --------------------------------------------------------------
# Calculate absolute path to build tree
# --------------------------------------------------------------

AC_DEFUN([FP_FIND_ROOT],[
AC_MSG_CHECKING(for path to top of build tree)
    hardtop=$(pwd)
    FP_CANONICALISE_WIN_PATH([hardtop])

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
