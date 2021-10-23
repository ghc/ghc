# FP_PROG_SH
# ------------
# Find a functional Bourne shell
AC_DEFUN([FP_PROG_SH],
[
 AC_REQUIRE([FPTOOLS_SET_PLATFORMS_VARS]) dnl for $windows
 AC_ARG_VAR(SH,[Use as the full path to a Bourne shell. [default=autodetect]])
 AC_PATH_PROGS([SH], [sh bash])
 if test "$windows" = "YES"; then
   dnl use mixed (-m) mode to get C:/mingw64/... with forward slashes.
   dnl     windows (-w) mode will give us C:\... and mess with escaping.
   SH=`cygpath -m "$SH"`
 fi
 AC_SUBST([SH])[]dnl
])# FP_PROG_SH
