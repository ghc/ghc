# FP_HSCPP_CMD_WITH_ARGS()
# ----------------------
# sets HS CPP command and its arguments
#
# $1 = the variable to set to HS CPP command
# $2 = the variable to set to HS CPP command arguments

AC_DEFUN([FP_HSCPP_CMD_WITH_ARGS],[
dnl ** what hs-cpp to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(hs-cpp,
[AS_HELP_STRING([--with-hs-cpp=ARG],
      [Path to the Haskell (C) preprocessor for Haskell files [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        HS_CPP_CMD=$withval
    fi
],
[

    # We can't use $CPP here, since HS_CPP_CMD is expected to be a single
    # command (no flags), and AC_PROG_CPP defines CPP as "/usr/bin/gcc -E".
    HS_CPP_CMD=$CC

]
)

dnl ** what hs-cpp flags to use?
dnl -----------------------------------------------------------
AC_ARG_WITH(hs-cpp-flags,
  [AS_HELP_STRING([--with-hs-cpp-flags=ARG],
      [Flags to the Haskell (C) preprocessor for Haskell files [default=autodetect]])],
  [
      if test "$HostOS" = "mingw32"
      then
          AC_MSG_WARN([Request to use $withval will be ignored])
      else
          HS_CPP_ARGS=$withval
          USER_HS_CPP_ARGS=$withval
      fi
  ],
[
  $HS_CPP_CMD -x c /dev/null -dM -E > conftest.txt 2>&1
  if grep "__clang__" conftest.txt >/dev/null 2>&1; then
    HS_CPP_ARGS="-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"
  else
      $HS_CPP_CMD  -v > conftest.txt 2>&1
      if  grep "gcc" conftest.txt >/dev/null 2>&1; then
          HS_CPP_ARGS="-E -undef -traditional"
        else
          $HS_CPP_CMD  --version > conftest.txt 2>&1
          if grep "cpphs" conftest.txt >/dev/null 2>&1; then
            HS_CPP_ARGS="--cpp -traditional"
          else
            AC_MSG_WARN([configure can't recognize your CPP program, you may need to set --with-hs-cpp-flags=FLAGS explicitly])
            HS_CPP_ARGS=""
          fi
      fi
  fi
  ]
)

$1=$HS_CPP_CMD
$2="$$2 $HS_CPP_ARGS"

])
