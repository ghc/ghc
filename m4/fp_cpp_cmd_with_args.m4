# FP_CPP_CMD_WITH_ARGS()
# ----------------------
# Sets CPP command and its arguments from args --with-cpp and --with-cpp-flags
#
# $1 = the variable to set to CPP command
# $2 = the variable to set to CPP command arguments
#
# The reason for using the non-standard --with-cpp and --with-cpp-flags instead
# of the standard CPP and CPPFLAGS is that autoconf sets CPP to "$CC -E",
# whereas we expect the CPP command to be configured as a standalone executable
# rather than a command. These are symmetrical with --with-hs-cpp and
# --with-hs-cpp-flags.
AC_DEFUN([FP_CPP_CMD_WITH_ARGS],[
dnl ** what cpp to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(hs-cpp,
[AS_HELP_STRING([--with-hs-cpp=ARG],
      [Path to the (C) preprocessor for Haskell files [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        $1="$withval"
    fi
],[])

dnl ** what cpp flags to use?
dnl -----------------------------------------------------------
AC_ARG_WITH(hs-cpp-flags,
  [AS_HELP_STRING([--with-hs-cpp-flags=ARG],
      [Flags to the (C) preprocessor for Haskell files [default=autodetect]])],
  [
      if test "$HostOS" = "mingw32"
      then
          AC_MSG_WARN([Request to use $withval will be ignored])
      else
          HS_CPP_ARGS=$withval
      fi
  ],
[
  $HS_CPP_CMD -x c /dev/null -dM -E > conftest.txt 2>&1
  if grep "__clang__" conftest.txt >/dev/null 2>&1; then
    HS_CPP_ARGS="-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"
  else
      $2="$withval"
  fi
],[])

# Clear CPP_CMD and CPP_ARGS
unset CPP_CMD
unset CPP_ARGS

])

