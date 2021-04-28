# FP_CPP_CMD_WITH_ARGS()
# ----------------------
# sets CPP command and its arguments
#
# $1 = the variable to set to CPP command
# $2 = the variable to set to CPP command arguments

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
        HS_CPP_CMD=$withval
    fi
],
[

    # We can't use $CPP here, since HS_CPP_CMD is expected to be a single
    # command (no flags), and AC_PROG_CPP defines CPP as "/usr/bin/gcc -E".
    HS_CPP_CMD=$CC

    SOLARIS_GCC_CPP_BROKEN=NO
    SOLARIS_FOUND_GOOD_CPP=NO
    case $host in
        i386-*-solaris2)
        GCC_MAJOR_MINOR=`$CC --version|grep "gcc (GCC)"|cut -d ' ' -f 3-3|cut -d '.' -f 1-2`
        if test "$GCC_MAJOR_MINOR" != "3.4"; then
          # this is not 3.4.x release so with broken CPP
          SOLARIS_GCC_CPP_BROKEN=YES
        fi
        ;;
    esac

    if test "$SOLARIS_GCC_CPP_BROKEN" = "YES"; then
      # let's try to find if GNU C 3.4.x is installed
      if test -x /usr/sfw/bin/gcc; then
        # something executable is in expected path so let's
        # see if it's really GNU C
        NEW_GCC_MAJOR_MINOR=`/usr/sfw/bin/gcc --version|grep "gcc (GCC)"|cut -d ' ' -f 3-3|cut -d '.' -f 1-2`
        if test "$NEW_GCC_MAJOR_MINOR" = "3.4"; then
          # this is GNU C 3.4.x which provides non-broken CPP on Solaris
          # let's use it as CPP then.
          HS_CPP_CMD=/usr/sfw/bin/gcc
          SOLARIS_FOUND_GOOD_CPP=YES
        fi
      fi
      if test "$SOLARIS_FOUND_GOOD_CPP" = "NO"; then
        AC_MSG_WARN([Your GNU C provides broken CPP and you do not have GNU C 3.4.x installed.])
        AC_MSG_WARN([Please install GNU C 3.4.x to solve this issue. It will be used as CPP only.])
      fi
    fi
]
)

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
$2=$HS_CPP_ARGS

])

