# Initialise and check sanity.
AC_INIT([ghc.spec.in])

# Compute the version number.
version=`sed -e 's/.*\([[0-9]]\)\.\([[0-9]]*\).*/\1.\2/' VERSION`
patchlevel=1

# These are needed by the .spec file.
AC_SUBST(version)
AC_SUBST(patchlevel)

# Write the results...
AC_OUTPUT([ghc.spec])
