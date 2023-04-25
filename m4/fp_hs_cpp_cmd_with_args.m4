# FP_HSCPP_CMD_WITH_ARGS()
# ----------------------
# sets HS CPP command and its arguments from args --with-hs-cpp and --with-hs-cpp-flags
#
# $1 = the variable to set to HS CPP command
# $2 = the variable to set to HS CPP command arguments

AC_DEFUN([FP_HSCPP_CMD_WITH_ARGS],[
AC_ARG_WITH(hs-cpp,
[AS_HELP_STRING([--with-hs-cpp=ARG],
      [Path to the Haskell (C) preprocessor for Haskell files [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        $1=$withval
    fi
],[])
AC_ARG_WITH(hs-cpp-flags,
[AS_HELP_STRING([--with-hs-cpp-flags=ARG],
  [Flags to the Haskell (C) preprocessor for Haskell files [default=autodetect]])],
[
  if test "$HostOS" = "mingw32"
  then
      AC_MSG_WARN([Request to use $withval will be ignored])
  else
      $2=$withval
  fi
],[])

])

