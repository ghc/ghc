AC_INIT([Haskell directory package], [1.0], [libraries@haskell.org], [directory])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([System/Directory.hs])

AC_CONFIG_HEADERS([HsDirectoryConfig.h])

# Autoconf chokes on spaces, but we may receive a path from Cabal containing
# spaces.  In that case, we just ignore Cabal's suggestion.
set_with_gcc() {
    case $withval in
        *" "*)
            AC_MSG_WARN([--with-gcc ignored due to presence of spaces]);;
        *)
            CC=$withval
    esac
}

# Legacy support for setting the C compiler with Cabal<1.24
# Newer versions use Autoconf's native `CC=...` facility
AC_ARG_WITH([gcc],
            [C compiler],
            [set_with_gcc])
# avoid warnings when run via Cabal
AC_ARG_WITH([compiler],
            [GHC compiler],
            [])
AC_PROG_CC()

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([fcntl.h limits.h sys/types.h sys/stat.h time.h])

AC_CHECK_FUNCS([utimensat])
AC_CHECK_FUNCS([CreateSymbolicLinkW])
AC_CHECK_FUNCS([GetFinalPathNameByHandleW])

# EXTEXT is defined automatically by AC_PROG_CC;
# we just need to capture it in the header file
AC_DEFINE_UNQUOTED([EXE_EXTENSION], ["$EXEEXT"],
                   [Filename extension of executable files])

AC_OUTPUT
