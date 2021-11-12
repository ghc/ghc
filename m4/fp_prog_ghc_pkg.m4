# FP_PROG_GHC_PKG
# ----------------
# Try to find a ghc-pkg matching the ghc mentioned in the environment variable
# WithGhc. Sets the output variable GhcPkgCmd.
AC_DEFUN([FP_PROG_GHC_PKG],
[AC_CACHE_CHECK([for ghc-pkg matching $WithGhc], fp_cv_matching_ghc_pkg,
[
# If we are told to use ghc-stage1, then we're using an in-tree
# compiler. In this case, we just want ghc-pkg, not ghc-pkg-stage2,
# so we sed off -stage[0-9]$. However, if we are told to use
# ghc-6.12.1 then we want to use ghc-pkg-6.12.1, so we keep any
# other suffix.
fp_ghc_pkg_guess=`echo "$WithGhc" | sed -e 's/-stage@<:@0-9@:>@$//' -e 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$fp_ghc_pkg_guess" list > /dev/null 2>&1; then
  fp_cv_matching_ghc_pkg=$fp_ghc_pkg_guess
else
  AC_MSG_ERROR([Cannot find matching ghc-pkg])
fi])
GhcPkgCmd=$fp_cv_matching_ghc_pkg
AC_SUBST([GhcPkgCmd])
])# FP_PROG_GHC_PKG
