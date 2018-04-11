AC_INIT([ghc-prim package], [2.1], [glasgow-haskell-bugs@haskell.org], [ghc-prim])

AC_CONFIG_SRCDIR([ghc-prim.cabal])

# -------------------------------------------------------------------------
dnl ** Used to determine how to compile ghc-prim's atomics.c, used by
dnl    unregisterised, Sparc, and PPC backends.
FP_GCC_SUPPORTS__ATOMICS
AC_DEFINE([HAVE_C11_ATOMICS], [$CONF_GCC_SUPPORTS__ATOMICS], [Does GCC support __atomic primitives?])

if test "x$CONF_GCC_SUPPORTS__ATOMICS" = YES
then PRIM_CFLAGS=-DHAVE_C11_ATOMICS
     PRIM_EXTRA_LIBRARIES=atomic
fi
AC_SUBST([PRIM_CFLAGS])
AC_SUBST([PRIM_EXTRA_LIBRARIES])
AC_CONFIG_FILES([ghc-prim.buildinfo])
AC_OUTPUT
