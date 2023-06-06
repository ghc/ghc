# FP_CC_IGNORE_UNUSED_ARGS
# ------------------------
# GHC tends to produce command-lines with unused arguments that elicit
# warnings from Clang. Clang offers the @-Qunused-arguments@ flag to silence
# these. See #11684.
#
# The primary effect of this is updating CONF_CC_OPTS_STAGE[12] to explicitly
# pass -Qunused-arguments to Clang, since otherwise Cc invocations by GHC will
# be very noisy
#
# $1 = CC
# $2 = CC_OPTS variable
AC_DEFUN([FP_CC_IGNORE_UNUSED_ARGS],
[
   AC_MSG_CHECKING([whether $1 supports -Qunused-arguments])
   echo 'int main() { return 0; }' > conftest.c
   if $1 -Qunused-arguments -Werror conftest.c > /dev/null 2>&1 ; then
       CONF_CC_SUPPORTS_TARGET=YES
       AC_MSG_RESULT([yes])
   else
       CONF_CC_SUPPORTS_TARGET=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest

   if test $CONF_CC_SUPPORTS_TARGET = YES ; then
       $2="$$2 -Qunused-arguments"
   fi
])


