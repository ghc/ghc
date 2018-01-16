AC_INIT([Haskell zlib package], [1.1], [libraries@haskell.org], [zlib])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([zlib.cabal])

# Header file to place defines in
AC_CONFIG_HEADERS([include/HsZlibConfig.h])

# Check for zlib include
AC_CHECK_HEADER(zlib.h, [ZLIB_HEADER=yes], [], [])

# Build the package if we found X11 stuff
if test "x$ZLIB_HEADER" = "x"
then BUILD_PACKAGE_BOOL=False
else BUILD_PACKAGE_BOOL=True
fi
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([zlib.buildinfo])
AC_OUTPUT
