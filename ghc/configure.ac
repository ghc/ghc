# Initialise and check sanity.
AC_INIT(ghc.spec.in)

# Compute the version number.
version=`sed -e 's/.*\([[0-9]]\)\.\([[0-9]]*\).*/\1.\2/' VERSION`
patchlevel=1

# These are needed by the .spec file.
AC_SUBST(version)
AC_SUBST(patchlevel)

# Uncomment links to unix docs if necessary.
# NOTE: This is a slight hack: It needs to be synchronized with
# fptools/configure.ac and fptools/libraries/Makefile and should probably use
# target features instead of host.
AC_CANONICAL_HOST

case $host in
i[[3456]]86-*-mingw32*) fp_supports_unix=no  ;;
*)                      fp_supports_unix=yes ;;
esac
FP_HTML_COMMENT([test x"$fp_supports_unix" = xyes], [unix])

# Uncomment links to ObjectIO docs if necessary.
AC_ARG_ENABLE([objectio],
  AC_HELP_STRING([--enable-objectio],
                 [Build ObjectIO, a portable GUI library for Haskell.]))
FP_HTML_COMMENT([test x"$enable_objectio" = xyes], [ObjectIO])

# Uncomment links to HOpenGL docs if necessary
AC_ARG_ENABLE([hopengl],
  AC_HELP_STRING([--enable-hopengl],
                 [Build HOpenGL, a Haskell binding for OpenGL/GLUT.]))
FP_HTML_COMMENT([{ test x"$enable_hopengl" = xyes || test x"$enable_hopengl" = xx11; }], [HOpenGL])

# Write the results...
AC_OUTPUT(ghc.spec docs/libraries.html)
