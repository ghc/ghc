# FP_SETUP_PROJECT_INFO
# ---------------------
AC_DEFUN([FP_SETUP_PROJECT_INFO],
[# Some renamings
AC_SUBST([ProjectName], [$PACKAGE_NAME])
AC_SUBST([ProjectNameShort], [$PACKAGE_TARNAME])
AC_SUBST([ProjectVersion], [$PACKAGE_VERSION])

# Split PACKAGE_VERSION into (possibly empty) parts
VERSION_MAJOR=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
VERSION_TMP=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`
VERSION_MINOR=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
ProjectPatchLevel=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`

# Calculate project version as an integer, using 2 digits for minor version
case $VERSION_MINOR in
  ?) ProjectVersionInt=${VERSION_MAJOR}0${VERSION_MINOR} ;;
  ??) ProjectVersionInt=${VERSION_MAJOR}${VERSION_MINOR} ;;
  *) AC_MSG_ERROR([bad minor version in $PACKAGE_VERSION]) ;;
esac
AC_SUBST([ProjectVersionInt])

# The project patchlevel is zero unless stated otherwise
test -z "$ProjectPatchLevel" && ProjectPatchLevel=0

# Remove dots from the patch level; this allows us to have versions like 6.4.1.20050508
ProjectPatchLevel=`echo $ProjectPatchLevel | sed 's/\.//'`

AC_SUBST([ProjectPatchLevel])
])# FP_SETUP_PROJECT_INFO


# FP_PROG_GHC_PKG
# ----------------
# Try to find a ghc-pkg matching the ghc mentioned in the environment variable
# WithGhc. If the latter is unset or no matching ghc-pkg can be found, try to
# find a plain ghc-pkg. Sets the output variable GhcPkgCmd.
AC_DEFUN([FP_PROG_GHC_PKG],
[AC_CACHE_CHECK([for ghc-pkg matching $WithGhc], fp_cv_matching_ghc_pkg,
[fp_ghc_pkg_guess=`echo $WithGhc | sed 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$fp_ghc_pkg_guess" -l > /dev/null 2>&1; then
  fp_cv_matching_ghc_pkg=$fp_ghc_pkg_guess
else
  fp_cv_matching_ghc_pkg=no
fi])
if test x"$fp_cv_matching_ghc_pkg" = xno; then
  AC_PATH_PROG([GhcPkgCmd], [ghc-pkg])
else
  GhcPkgCmd=$fp_cv_matching_ghc_pkg
fi])# FP_PROG_GHC_PKG


# FP_GHC_HAS_READLINE
# -------------------
AC_DEFUN(FP_GHC_HAS_READLINE,
[AC_REQUIRE([FP_PROG_GHC_PKG])
AC_CACHE_CHECK([whether ghc has readline package], [fp_cv_ghc_has_readline],
[if "${GhcPkgCmd-ghc-pkg}" --show-package readline >/dev/null 2>&1; then
  fp_cv_ghc_has_readline=yes
else
  fp_cv_ghc_has_readline=no
 fi])
AC_SUBST([GhcHasReadline], [`echo $fp_cv_ghc_has_readline | sed 'y/yesno/YESNO/'`])
])# FP_GHC_HAS_READLINE


# FP_COMPARE_VERSIONS(VERSION1, TEST, VERSION2, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ----------------------------------------------------------------------------------
# Compare dotted version numbers VERSION1 and VERSION2 lexicographically according
# to TEST (one of -eq, -ne, -lt, -le, -gt, or -ge).
AC_DEFUN([FP_COMPARE_VERSIONS],
[fp_version1=$1; fp_version2=$3
fp_save_IFS=$IFS; IFS='.'
while test x"$fp_version1" != x || test x"$fp_version2" != x
do

  set dummy $fp_version1; shift
  fp_num1=""
  test $[@%:@] = 0 || { fp_num1="[$]1"; shift; }
  test x"$fp_num1" = x && fp_num1="0"
  fp_version1="[$]*"

  set dummy $fp_version2; shift
  fp_num2=""
  test $[@%:@] = 0 || { fp_num2="[$]1"; shift; }
  test x"$fp_num2" = x && fp_num2="0"
  fp_version2="[$]*"

  test "$fp_num1" = "$fp_num2" || break;
done
IFS=$fp_save_IFS
AS_IF([test "$fp_num1" $2 "$fp_num2"], [$4], [$5])[]dnl
])# FP_COMPARE_VERSIONS


# FP_HAVE_GCC
# -----------
# Extra testing of the result AC_PROG_CC, testing the gcc version no. Sets the
# output variables HaveGcc and GccVersion.
AC_DEFUN([FP_HAVE_GCC],
[AC_REQUIRE([AC_PROG_CC])
AC_CACHE_CHECK([whether your gcc is OK], [fp_cv_have_gcc],
[if test -z "$GCC"; then
  fp_cv_have_gcc='no'
  AC_MSG_WARN([You would be better off with gcc, perhaps it is already installed, but not in your PATH?])
else
  fp_cv_have_gcc='yes'
  gcc_version_str="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [[^0-9]]*\([[0-9]][[0-9]]*\)\.\([[0-9]][[0-9]]*\).*/\1\.\2/g' `"
  FP_COMPARE_VERSIONS([$gcc_version_str], [-lt], [2.0],
    [AC_MSG_ERROR([Need at least gcc version 2.0 (2.95.3 recommend)])])
fi])
AC_SUBST([HaveGcc], [`echo $fp_cv_have_gcc | sed 'y/yesno/YESNO/'`])
AC_SUBST([GccVersion], [`gcc --version | grep mingw | cut -f 3 -d ' '`])
])# FP_HAVE_GCC


# FP_GCC_NEEDS_NO_OMIT_LFPTR
# --------------------------
# Some OSs (Mandrake Linux, in particular) configure GCC with
# -momit-leaf-frame-pointer on by default. If this is the case, we need to turn
# it off for mangling to work. The test is currently a bit crude, using only the
# version number of gcc. Defines HAVE_GCC_MNO_OMIT_LFPTR.
AC_DEFUN([FP_GCC_NEEDS_NO_OMIT_LFPTR],
[AC_REQUIRE([FP_HAVE_GCC])
AC_CACHE_CHECK([whether gcc needs -mno-omit-leaf-frame-pointer], [fp_cv_gcc_needs_no_omit_lfptr],
[FP_COMPARE_VERSIONS([$gcc_version_str], [-ge], [3.2],
  [fp_cv_gcc_needs_no_omit_lfptr=yes],
  [fp_cv_gcc_needs_no_omit_lfptr=no])])
if test "$fp_cv_gcc_needs_no_omit_lfptr" = "yes"; then
   AC_DEFINE([HAVE_GCC_MNO_OMIT_LFPTR], [1], [Define to 1 if gcc supports -mno-omit-leaf-frame-pointer.])
fi])# FP_GCC_NEEDS_NO_OMIT_LFPTR
