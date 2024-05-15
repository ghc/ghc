# FP_CMM_CPP_CMD_WITH_ARGS()
# --------------------------
# sets CMM_CPP command and its arguments
#
# $1 = the path to the C compiler
# $2 = the variable to set to Cmm CPP command
# $3 = the variable to set to Cmm CPP command arguments
# $4 = whether Cmm CPP command supports -g0

AC_DEFUN([FP_CMM_CPP_CMD_WITH_ARGS],[

AC_ARG_WITH(cmm-cpp,
[AS_HELP_STRING([--with-cmm-cpp=ARG],
      [Path to the Cmm (C) preprocessor [default=autodetect].
       If you set --with-cmm-cpp=CC, ensure -E is included in --with-cmm-cpp-flags])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        CMM_CPP_CMD="$withval"
    fi
],
[
    # We can't use the CPP var here, since CPP_CMD is expected to be a single
    # command (no flags), and autoconf defines CPP as "/usr/bin/gcc -E".
    # So we use CC with -E by default
    CMM_CPP_CMD="$1"
]
)

AC_ARG_WITH(cmm-cpp-flags,
[AS_HELP_STRING([--with-cmm-cpp-flags=ARG],
  [Flags to the Cmm (C) preprocessor [default=autodetect]])],
[
  if test "$HostOS" = "mingw32"
  then
      AC_MSG_WARN([Request to use $withval will be ignored])
  else
      # Use whatever flags were manually set, ignoring previously configured
      # flags; and add CPP_ARGS (which will be -E if CPP_CMD was not specified)
      CMM_CPP_ARGS="$withval"
  fi
],
[
  CMM_CPP_ARGS="-E"
])

AC_MSG_CHECKING([whether the C-- preprocessor "$CMM_CPP_CMD" $CMM_CPP_ARGS supports -g0])
: > conftest.c
if "$CMM_CPP_CMD" $CMM_CPP_ARGS conftest.c -o conftest -g0 >/dev/null 2>&1; then
  $4=True
  AC_MSG_RESULT([yes])
else
  $4=False
  AC_MSG_RESULT([no])
fi

AC_MSG_CHECKING([the C-- preprocessor for C99 support])
cat > conftest.c <<EOF
#include <stdio.h>
#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L
# error "Compiler does not advertise C99 conformance"
#endif
EOF
if "$CMM_CPP_CMD" $CMM_CPP_ARGS conftest.c -o conftest -g0 >/dev/null 2>&1; then
  AC_MSG_RESULT([yes])
else
    # Try -std=gnu99
    if "$CMM_CPP_CMD" -std=gnu99 $CMM_CPP_ARGS conftest.c -o conftest -g0 >/dev/null 2>&1; then
      $3="-std=gnu99 $$3"
      AC_MSG_RESULT([needs -std=gnu99])
    else
      AC_MSG_ERROR([C99-compatible compiler needed])
    fi
fi
rm -f conftest.c conftest.o conftest


$2="$CMM_CPP_CMD"
$3="$$3 $CMM_CPP_ARGS"

# Clear CMM_CPP_CMD and CMM_CPP_ARGS
unset CMM_CPP_CMD
unset CMM_CPP_ARGS

])

