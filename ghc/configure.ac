# Initialise and check sanity.
AC_INIT([The Glorious Glasgow Haskell Compilation System], [6.3], [glasgow-haskell-bugs@haskell.org], [ghc])
AC_CONFIG_SRCDIR([ghc.spec.in])

FP_SETUP_PROJECT_INFO

# Hmmm, we fix the RPM release number to 1 here... Is this convenient?
AC_SUBST([release], [1])

# Check whether this GHC has readline installed
FP_GHC_HAS_READLINE

# Write the results...
AC_CONFIG_FILES([ghc.spec VERSION docs/users_guide/ug-book.xml mk/config.mk mk/version.mk])
AC_OUTPUT
