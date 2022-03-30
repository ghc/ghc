# FP_CHECK_ALIGNMENT(TYPE, [IGNORED], [INCLUDES = DEFAULT-INCLUDES])
# ------------------------------------------------------------------
# A variation of AC_CHECK_SIZEOF for computing the alignment restrictions of a
# given type. Defines ALIGNMENT_TYPE.
AC_DEFUN([FP_CHECK_ALIGNMENT],
[AS_LITERAL_IF(m4_translit([[$1]], [*], [p]), [],
               [m4_fatal([$0: requires literal arguments])])[]dnl
AC_CHECK_TYPE([$1], [], [], [$3])[]dnl
m4_pushdef([fp_Cache], [AS_TR_SH([fp_cv_alignment_$1])])[]dnl
AC_CACHE_CHECK([alignment of $1], [fp_Cache],
[if test "$AS_TR_SH([ac_cv_type_$1])" = yes; then
  FP_COMPUTE_INT([offsetof(struct { char c; $1 ty; },ty)],
                 [fp_Cache],
                 [AC_INCLUDES_DEFAULT([$3])],
                 [AC_MSG_ERROR([cannot compute alignment ($1)
See `config.log' for more details.], [77])])
else
  fp_Cache=0
fi])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP(alignment_$1), $fp_Cache, [The alignment of a `$1'.])[]dnl
m4_popdef([fp_Cache])[]dnl
])# FP_CHECK_ALIGNMENT
