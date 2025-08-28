# configure.ac
AC_PREREQ([2.69])
AC_INIT([ghc-builder], [0.1.0], [your-email@example.com])
AC_CONFIG_SRCDIR([.]) # A representative .in file
AC_CONFIG_AUX_DIR([build-aux]) # Recommended place for config.guess, config.sub
AC_CONFIG_MACRO_DIR([m4])      # For any custom m4 macros
# AM_INIT_AUTOMAKE([-Wall -Werror foreign]) # Using some automake conventions

# --- Define GHC Build Options ---
# Usage: ./configure ProjectVersion=X.Y ...
AC_ARG_WITH([project-version], [AS_HELP_STRING([--with-project-version=VER], [GHC version (default: 9.13)])], [ProjectVersion="$withval"], [ProjectVersion="9.13"])
AC_ARG_WITH([project-version-int], [AS_HELP_STRING([--with-project-version-int=VER], [GHC version as int (default: 913)])], [ProjectVersionInt="$withval"], [ProjectVersionInt="913"])
AC_ARG_WITH([project-version-munged], [AS_HELP_STRING([--with-project-version-munged=VER], [GHC version "munged" (default: 9.13)])], [ProjectVersionMunged="$withval"], [ProjectVersionMunged="9.13"])
AC_ARG_WITH([project-version-for-lib], [AS_HELP_STRING([--with-project-version-for-lib=VER], [GHC version for libraries (default: 9.1300)])], [ProjectVersionForLib="$withval"], [ProjectVersionForLib="9.1300"])
AC_ARG_WITH([project-patch-level], [AS_HELP_STRING([--with-project-patch-level=VER], [GHC patchlevel version (default: 0)])], [ProjectPatchLevel="$withval"], [ProjectPatchLevel="0"])
AC_ARG_WITH([project-patch-level1], [AS_HELP_STRING([--with-project-patch-level1=VER], [GHC patchlevel1 version (default: 0)])], [ProjectPatchLevel1="$withval"], [ProjectPatchLevel1="0"])
AC_ARG_WITH([project-patch-level2], [AS_HELP_STRING([--with-project-patch-level2=VER], [GHC patchlevel2 version (default: 0)])], [ProjectPatchLevel2="$withval"], [ProjectPatchLevel2="0"])

# Export these variables for substitution by AC_SUBST
AC_SUBST([ProjectVersion])
AC_SUBST([ProjectVersionInt])
AC_SUBST([ProjectVersionMunged])
AC_SUBST([ProjectVersionForLib])
AC_SUBST([ProjectPatchLevel])
AC_SUBST([ProjectPatchLevel1])
AC_SUBST([ProjectPatchLevel2])

# For ghc-boot-th.cabal.in
AC_SUBST([Suffix],[""])
AC_SUBST([SourceRoot],["."])

# --- Define Programs ---
# We don't need to check for CC, MAKE_SET, and others for now, we only want substitution.
# AC_PROG_CC
# AC_PROG_MAKE_SET # To ensure 'set' works in Makefiles for undefined variables
AC_CHECK_PROGS([PYTHON], [python3 python python2])
AS_IF([test "x$PYTHON" = x], [AC_MSG_ERROR([Python interpreter not found. Please install Python or set PYTHON environment variable.])])
AC_SUBST([PYTHON])

# --- Files to generate ---
# config.status will create these files by substituting @VAR@ placeholders.
AC_CONFIG_FILES([
    ghc/ghc-bin.cabal:ghc/ghc-bin.cabal.in
    compiler/ghc.cabal:compiler/ghc.cabal.in
    libraries/ghc-boot/ghc-boot.cabal:libraries/ghc-boot/ghc-boot.cabal.in
    libraries/ghc-boot-th/ghc-boot-th.cabal:libraries/ghc-boot-th/ghc-boot-th.cabal.in
    libraries/ghc-heap/ghc-heap.cabal:libraries/ghc-heap/ghc-heap.cabal.in
    libraries/template-haskell/template-haskell.cabal:libraries/template-haskell/template-haskell.cabal.in
    libraries/ghci/ghci.cabal:libraries/ghci/ghci.cabal.in
    utils/ghc-pkg/ghc-pkg.cabal:utils/ghc-pkg/ghc-pkg.cabal.in
    utils/ghc-iserv/ghc-iserv.cabal:utils/ghc-iserv/ghc-iserv.cabal.in
    utils/runghc/runghc.cabal:utils/runghc/runghc.cabal.in
    libraries/ghc-internal/ghc-internal.cabal:libraries/ghc-internal/ghc-internal.cabal.in
    libraries/ghc-experimental/ghc-experimental.cabal:libraries/ghc-experimental/ghc-experimental.cabal.in
    libraries/base/base.cabal:libraries/base/base.cabal.in
    rts/include/ghcversion.h:rts/include/ghcversion.h.in
])

AC_OUTPUT

# After running ./configure, the following command can be used to see configured values:
# ./config.status --config
