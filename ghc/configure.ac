# Initialise and check sanity.
AC_INIT([The Glorious Glasgow Haskell Compilation System], [6.5], [glasgow-haskell-bugs@haskell.org], [ghc])
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
AC_PATH_PROGS([GTK_CONFIG], [gtk-config gtk12-config])
if test -n "$GTK_CONFIG"; then
  AC_CACHE_CHECK([for version of GTK+], [fp_cv_gtk_version],
    [fp_cv_gtk_version=`$GTK_CONFIG --version`])
  FP_COMPARE_VERSIONS([$fp_cv_gtk_version], [-lt], [1.2],
    [AC_MSG_WARN([GTK+ not usable, need at least version 1.2])
     GTK_CONFIG=])
fi
AC_SUBST([GTK_CONFIG])

# Write the results...
AC_CONFIG_FILES([ghc.spec VERSION docs/users_guide/ug-book.xml mk/config.mk mk/version.mk])
AC_OUTPUT
