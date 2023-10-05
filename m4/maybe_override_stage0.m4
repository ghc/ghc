AC_DEFUN([MAYBE_OVERRIDE_STAGE0],[
  if test ! -z "$With_$1" -a "$CrossCompiling" != "YES"; then
      AC_MSG_NOTICE([Not cross-compiling, so --with-$1 also sets $2])
      $2=$With_$1
  fi
])
