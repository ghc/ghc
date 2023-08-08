# FP_CPP_CMD_WITH_ARGS()
# ----------------------
# sets CPP command and its arguments
#
# $1 = CC (unmodified)
# $2 = the variable to set to CPP command
# $3 = the variable to set to CPP command arguments
#
# The reason for using the non-standard --with-cpp and --with-cpp-flags instead
# of the standard CPP and CPPFLAGS is that autoconf sets CPP to "$CC -E",
# whereas we expect the CPP command to be configured as a standalone executable
# rather than a command. These are symmetrical with --with-hs-cpp and
# --with-hs-cpp-flags.
AC_DEFUN([FP_CPP_CMD_WITH_ARGS],[

AC_ARG_WITH(cpp,
[AS_HELP_STRING([--with-cpp=ARG],
      [Path to the (C) preprocessor [default=autodetect].
       If you set --with-cpp=CC, ensure -E is included in --with-cpp-flags])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        CPP_CMD="$withval"
    fi
],
[
    # We can't use the CPP var here, since CPP_CMD is expected to be a single
    # command (no flags), and autoconf defines CPP as "/usr/bin/gcc -E".
    # So we use CC with -E by default
    CPP_CMD="$1"
    CPP_ARGS="-E"
]
)

AC_ARG_WITH(cpp-flags,
[AS_HELP_STRING([--with-cpp-flags=ARG],
  [Flags to the (C) preprocessor [default=autodetect]])],
[
  if test "$HostOS" = "mingw32"
  then
      AC_MSG_WARN([Request to use $withval will be ignored])
  else
      # Use whatever flags were manually set, ignoring previously configured
      # flags; and add CPP_ARGS (which will be -E if CPP_CMD was not specified)
      CPP_ARGS="$CPP_ARGS $withval"
      USER_CPP_ARGS="$withval"
  fi
],
[
  # Augment CPP_ARGS with whatever flags were previously configured and passed
  # as an argument.
  CPP_ARGS="$$3 $CPP_ARGS"
])

$2="$CPP_CMD"
$3="$CPP_ARGS"

# Clear CPP_CMD and CPP_ARGS
unset CPP_CMD
unset CPP_ARGS

])

