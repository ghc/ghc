AC_INIT([base], [], [libraries@haskell.org], [base])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsBase.h])

AC_CONFIG_HEADERS([include/HsBaseConfig.h])

dnl ** Check for idiosyncracies in some mingw impls of directory handling.
FP_READDIR_EOF_ERRNO
FP_DIRENT_FLAT_LAYOUT

AC_OUTPUT
