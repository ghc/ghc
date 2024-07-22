dnl
dnl Check for Happy and version:
dnl
dnl 1. Use happy specified in env var HAPPY
dnl 2. Find happy in path
dnl 3. Check happy version
dnl
dnl If you increase the minimum version requirement, please also update:
dnl https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/tools
dnl
AC_DEFUN([FPTOOLS_HAPPY],
[AC_PATH_PROG(HAPPY,[happy],)
AC_SUBST(HappyCmd,$HAPPY)
AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
changequote(, )dnl
[
if test x"$HappyCmd" != x; then
   fptools_cv_happy_version=`"$HappyCmd" -v |
              grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'` ;
else
   fptools_cv_happy_version="";
fi;
changequote([, ])dnl
])
if test ! -f compiler/GHC/Parser.hs || test ! -f compiler/GHC/Cmm/Parser.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.20.0],
      [AC_MSG_ERROR([Happy version 1.20 or later is required to compile GHC.])])[]
    FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-ge],[3.0],
      [AC_MSG_ERROR([Happy version 1.20 or earlier is required to compile GHC.])])[]
fi
HappyVersion=$fptools_cv_happy_version;
AC_SUBST(HappyVersion)
])
