# LIBRARY_VERSION(lib, [cabal_file])
# --------------------------------
# Gets the version number of a library.
# If $1 is ghc-prim, then we define LIBRARY_ghc_prim_VERSION as 1.2.3
# $2 points to the directory under libraries/
AC_DEFUN([LIBRARY_VERSION],[
cabal_file=m4_default([$2],[$1/$1.cabal])
LIBRARY_[]translit([$1], [-], [_])[]_VERSION=`grep -i "^version:" libraries/${cabal_file} | sed "s/.* //"`
AC_SUBST(LIBRARY_[]translit([$1], [-], [_])[]_VERSION)
])
