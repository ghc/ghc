# Helper macros to accumulate package fields and constraints without leading newline.
# Usage: APPEND_PKG_FIELD([shared: True])
#        APPEND_CONSTRAINT([rts +dynamic])

AC_DEFUN([APPEND_PKG_FIELD], [
  AS_IF([test "x$ALL_PACKAGES" = "x"], [
    AS_VAR_SET([ALL_PACKAGES], ["$1"])
  ], [
    AS_VAR_APPEND([ALL_PACKAGES], ["
$1"])
  ])
])

AC_DEFUN([APPEND_CONSTRAINT], [
  AS_IF([test "x$CONSTRAINTS" = "x"], [
    AS_VAR_SET([CONSTRAINTS], ["$1"])
  ], [
    AS_VAR_APPEND([CONSTRAINTS], ["
$1"])
  ])
])
