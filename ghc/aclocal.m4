# FP_HTML_COMMENT(VARIABLE, PREFIX)
# ---------------------------------
AC_DEFUN([FP_HTML_COMMENT],
[AS_IF([$1],
       [$2HTMLStart="";     $2HTMLEnd=""],
       [$2HTMLStart="<!--"; $2HTMLEnd="-->"])[]dnl
AC_SUBST([$2HTMLStart])[]dnl
AC_SUBST([$2HTMLEnd])[]dnl
])# FP_HTML_COMMENT
