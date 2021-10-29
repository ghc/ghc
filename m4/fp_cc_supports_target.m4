# FP_CC_SUPPORTS_TARGET
# ---------------------
# Does CC support the --target=<triple> option? If so, we should pass it
# whenever possible to avoid ambiguity and potential compile-time errors (e.g.
# see #20162).
#
# The primary effect of this is updating CONF_CC_OPTS_STAGE[12] to
# explicitly ask the compiler to generate code for the $TargetPlatform.
AC_DEFUN([FP_CC_SUPPORTS_TARGET],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_REQUIRE([FPTOOLS_SET_PLATFORM_VARS])
   AC_MSG_CHECKING([whether $1 CC supports --target])
   echo 'int main() { return 0; }' > conftest.c
   if $CC --target=$LlvmTarget -Werror conftest.c >& /dev/null ; then
       CONF_CC_SUPPORTS_TARGET=YES
       AC_MSG_RESULT([yes])
   else
       CONF_CC_SUPPORTS_TARGET=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest

   if test $CONF_CC_SUPPORTS_TARGET = YES ; then
       CONF_CC_OPTS_STAGE1="--target=$LlvmTarget $CONF_CC_OPTS_STAGE1"
       CONF_CC_OPTS_STAGE2="--target=$LlvmTarget $CONF_CC_OPTS_STAGE2"
       CONF_CXX_OPTS_STAGE1="--target=$LlvmTarget $CONF_CXX_OPTS_STAGE1"
       CONF_CXX_OPTS_STAGE2="--target=$LlvmTarget $CONF_CXX_OPTS_STAGE2"
       CONF_GCC_LINKER_OPTS_STAGE1="--target=$LlvmTarget $CONF_GCC_LINKER_OPTS_STAGE1"
       CONF_GCC_LINKER_OPTS_STAGE2="--target=$LlvmTarget $CONF_GCC_LINKER_OPTS_STAGE2"
   fi
])

