# Initialise and check sanity.
AC_INIT([Glasgow Haskell Compiler], [6.3.20040927], [glasgow-haskell-bugs@haskell.org], [ghc])
AC_CONFIG_SRCDIR([ghc.spec.in])

# Calculate project version as an integer, using 2 digits for minor version
case $PACKAGE_VERSION in
  *.?)  ProjectVersionInt=`echo "$PACKAGE_VERSION" | sed 's,^\(.*\)\.\(.\)$,\10\2,'` ;;
  *.??) ProjectVersionInt=`echo "$PACKAGE_VERSION" | sed 's,^\(.*\)\.\(..\)$,\1\2,'` ;;
  *.?.*)  ProjectVersionInt=`echo "$PACKAGE_VERSION" | sed 's,^\(.*\)\.\(.\)\.\(.*\)$,\10\2,'` ;;
  *.??.*) ProjectVersionInt=`echo "$PACKAGE_VERSION" | sed 's,^\(.*\)\.\(..\)\.\(.*\)$,\1\2,'` ;;
  *) AC_MSG_ERROR([bad package version $PACKAGE_VERSION]) ;;
esac
AC_SUBST([ProjectVersionInt])

# Hmmm, we fix the RPM release number to 1 here... Is this convenient?
AC_SUBST([release], [1])

# Write the results...
AC_CONFIG_FILES([ghc.spec VERSION docs/users_guide/ug-book.xml mk/version.mk])
AC_OUTPUT
