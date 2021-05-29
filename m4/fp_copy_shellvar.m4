# Helper for cloning a shell variable's state
AC_DEFUN([FP_COPY_SHELLVAR],
[if test -n "${$1+set}"; then $2="$$1"; else unset $2; fi ])
