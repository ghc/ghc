# FP_JSCPP_CMD_WITH_ARGS()
# ----------------------
# sets JS CPP command and its arguments
#
# $1 = the variable to set to JS CPP command
# $2 = the variable to set to JS CPP command arguments

AC_DEFUN([FP_JSCPP_CMD_WITH_ARGS],[
dnl ** what js-cpp to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(js-cpp,
[AS_HELP_STRING([--with-js-cpp=ARG],
[Path to the JavaScript (EMCC) preprocessor for JavaScript files [default=autodetect]])],
[
  if test "$target_cpu" = "javascript" ; then
    JS_CPP_CMD=$withval
  else
    AC_MSG_WARN([Request to use $withval will be ignored])
    JS_CPP_CMD=$CC
  fi
],
[
  if test "$target_cpu" = "javascript" ; then
    if "$CC" --version 2> /dev/null | grep "emcc" > /dev/null 2>&1;
    then
      JS_CPP_CMD=$CC
    else
      AC_MSG_ERROR([configure can not recognize your CPP program for JavaScript preprocessing, you need to set --with-js-cpp=ARG explicitly])
      JS_CPP_CMD=""
    fi
  else
    JS_CPP_CMD=$CC
  fi
]
)

dnl ** what js-cpp flags to use?
dnl -----------------------------------------------------------
AC_ARG_WITH(js-cpp-flags,
[AS_HELP_STRING([--with-js-cpp-flags=ARG],
[Flags to the JavaScript (EMCC) preprocessor for JavaScript files [default=autodetect]])],
[
  if test "$target_cpu" = "javascript" ; then
    JS_CPP_ARGS=$withval
    USER_JS_CPP_ARGS=$withval
  else
    AC_MSG_WARN([Request to use $withval will be ignored])
  fi
],
[
  # See: https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html#index-CC
  # See: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-CC
  # Emscripten supports -C and -CC options same as GCC and CLang
  # We have to use -nostdinc to prevent adding copyright headers in gcc output.
  # This issue is known and discussed here: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=59566
  JS_CPP_ARGS="-E -CC -Wno-unicode -nostdinc"

  AC_MSG_CHECKING([whether $JS_CPP_CMD preprocessor supports flags $JS_CPP_ARGS])

  cat <<EOT >> ./conftest.js
#define DEF_TEST
#ifdef DEF_TEST
// 1
#endif
EOT

  "$JS_CPP_CMD" $JS_CPP_ARGS -P -x assembler-with-cpp ./conftest.js -o ./conftest.pp.js > conftest.txt 2>&1

  if test $? -eq 0 ; then
    # We trim \n due of difference between Emscripten CC and clang.
    # Clang version preserves whitespace lines, but emcc removes.
    if test "$(cat ./conftest.pp.js | tr -d '\n')" = "// 1" ; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
      echo "processed result:"
      cat ./conftest.pp.js
      AC_MSG_ERROR([$JS_CPP_CMD must produce correct output $JS_CPP_ARGS])
    fi
  else
    AC_MSG_RESULT([no])
    echo "command execution output:"
    cat ./conftest.txt
    AC_MSG_ERROR([$JS_CPP_CMD must support the flags $JS_CPP_ARGS])
  fi

  rm -f conftest.js conftest.pp.js conftest.txt
]
)

$1=$JS_CPP_CMD
$2="$$2 $JS_CPP_ARGS"

])
