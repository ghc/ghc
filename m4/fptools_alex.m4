dnl
dnl Check for Alex and version.
dnl
dnl 1. Use alex specified in env var ALEX
dnl 2. Find alex in path
dnl 3. Check alex version
dnl
dnl If you increase the minimum version requirement, please also update:
dnl https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/tools
dnl
AC_DEFUN([FPTOOLS_ALEX],
[AC_PATH_PROG(ALEX,[alex],)
AC_SUBST(AlexCmd,$ALEX)
AC_CACHE_CHECK([for version of alex], fptools_cv_alex_version,
changequote(, )dnl
[if test x"$AlexCmd" != x; then
   fptools_cv_alex_version=`"$AlexCmd" -v |
              grep 'Alex [Vv]ersion' | sed -e 's/Alex [Vv]ersion \([0-9\.]*\).*/\1/g'` ;
else
   fptools_cv_alex_version="";
fi;
changequote([, ])dnl
])
if test ! -f compiler/GHC/Parser/Lexer.hs || test ! -f compiler/GHC/Cmm/Lexer.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[3.2.6],
      [AC_MSG_ERROR([Alex >= 3.2.6 && < 4 is required to compile GHC.])])[]
    FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-ge],[4.0.0],
      [AC_MSG_ERROR([Alex >= 3.2.6 && < 4 is required to compile GHC.])])[]
fi
AlexVersion=$fptools_cv_alex_version;
AC_SUBST(AlexVersion)
])
