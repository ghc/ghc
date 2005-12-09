# Initialise and check sanity.
AC_INIT([The Glorious Glasgow Haskell Compilation System], [6.5.20051208], [glasgow-haskell-bugs@haskell.org], [ghc])
AC_CONFIG_SRCDIR([ghc.spec.in])

# duplicate from ../configure.ac
AC_ARG_WITH([ghc],
[AC_HELP_STRING([--with-ghc=ARG],
  [Use ARG as the path to GHC  [default=autodetect]])],
  [WithGhc="$withval"],
  [if test "$GHC" = ""; then
    AC_PATH_PROG([GHC], [ghc])
  fi
  WithGhc="$GHC"])
AC_SUBST([WithGhc])

FP_SETUP_PROJECT_INFO

# Hmmm, we fix the RPM release number to 1 here... Is this convenient?
AC_SUBST([release], [1])

FP_GCC_NEEDS_NO_OMIT_LFPTR

# Check whether this GHC has readline installed
FP_GHC_HAS_READLINE

# test for GTK+
AC_PATH_PROGS([GTK_CONFIG], [pkg-config])
if test -n "$GTK_CONFIG"; then
  if $GTK_CONFIG gtk+-2.0 --atleast-version=2.0; then
    GTK_CONFIG="$GTK_CONFIG gtk+-2.0"
  else
    AC_MSG_WARN([GTK+ not usable, need at least version 2.0])
    GTK_CONFIG=
  fi
fi
AC_SUBST([GTK_CONFIG])

# Write the results...
AC_CONFIG_FILES([ghc.spec VERSION docs/users_guide/ug-book.xml mk/config.mk mk/version.mk])
AC_OUTPUT
