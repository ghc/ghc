# GHC_IOMANAGER_ENABLE(iomgr_name, enable_var, output_cpp_var, action_detect)
# ---------------------------------------------------------------------
AC_DEFUN([GHC_IOMANAGER_ENABLE], [
  $4
  AC_MSG_CHECKING( if the the $1 I/O manager should be built)
  if test "${$2}" = "YES"; then
    AC_MSG_RESULT(yes)
    AC_DEFINE([$3], [1], [Define to 1 if the $1 I/O manager should be built])
  else
    AC_MSG_RESULT(no)
  fi
])


# GHC_IOMANAGER_DEFAULT_SELECT(default_var, iomgr_name, enable_var)
# ---------------------------------------------------------------------
AC_DEFUN([GHC_IOMANAGER_DEFAULT_SELECT], [
  if test "${$3}" = "YES"; then
    $1=$2
  fi
])


# GHC_IOMANAGER_DEFAULT_CHECK_NOT_EMPTY(default_var, way)
# ---------------------------------------------------------------------
AC_DEFUN([GHC_IOMANAGER_DEFAULT_CHECK_NOT_EMPTY], [
  if test "${$1}" = ""; then
    AC_MSG_ERROR([no suitable I/O manager enabled for the $2 RTS])
  fi
])

# GHC_IOMANAGER_DEFAULT_AC_DEFINE(default_var, way, iomgr_name, output_cpp_var)
# ---------------------------------------------------------------------
AC_DEFUN([GHC_IOMANAGER_DEFAULT_AC_DEFINE], [
  if test "${$1}" = "$3"; then
    AC_DEFINE([$4], [1],
             [Define to 1 if the $3 I/O manager is the default for the $2 RTS])
  fi
])

