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
