# FP_PROG_SH
# ------------
# Find a functional Bourne shell
AC_DEFUN([FP_PROG_SH],
[
 AC_REQUIRE([FPTOOLS_SET_PLATFORM_VARS]) dnl for $windows
 AC_ARG_VAR(SH,[Use as the full path to a Bourne shell. [default=autodetect]])
 AC_PATH_PROGS([SH], [sh bash])
 if test "$windows" = "YES"; then
   SH=`cygpath -w "$SH"`
 fi
 AC_SUBST([SH])[]dnl
])# FP_PROG_SH

