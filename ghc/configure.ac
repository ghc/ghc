# Initialise and check sanity.
AC_INIT([Glasgow Haskell Compiler], [6.3], [glasgow-haskell-bugs@haskell.org], [ghc])
AC_CONFIG_SRCDIR([ghc.spec.in])

# Compute the version number and the release, they are needed by the .spec file.
AC_SUBST([version], [[`sed 's/.* version \([0-9][0-9.]*\).*/\1/' VERSION`]])
AC_SUBST([release], [1])

# Write the results...
AC_CONFIG_FILES([ghc.spec])
AC_OUTPUT
