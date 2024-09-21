# FP_CC_SUPPORTS_TARGET
# ---------------------
# Does CC support the --target=<triple> option? If so, we should pass it
# whenever possible to avoid ambiguity and potential compile-time errors (e.g.
# see #20162).
#
# The primary effect of this is updating CONF_CC_OPTS_STAGE[12] to
# explicitly ask the compiler to generate code for the $TargetPlatform.
#
# $1 = CC
# $2 = CC_OPTS variable
# $3 = CXX_OPTS variable
AC_DEFUN([FP_CC_SUPPORTS_TARGET],
[
   AC_REQUIRE([GHC_LLVM_TARGET_SET_VAR])
   AC_MSG_CHECKING([whether $1 supports --target $LlvmTarget])

   echo 'int main() { return 0; }' > conftest.c
   if test "$target_cpu" = "javascript" ; then
       # See Note [Don't pass --target to emscripten toolchain] in GHC.Toolchain.Program
       CONF_CC_SUPPORTS_TARGET=NO
       AC_MSG_RESULT([no])
   elif $1 --target=$LlvmTarget -Werror conftest.c > /dev/null 2>&1 ; then
       CONF_CC_SUPPORTS_TARGET=YES
       AC_MSG_RESULT([yes])
   else
       CONF_CC_SUPPORTS_TARGET=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest

   if test $CONF_CC_SUPPORTS_TARGET = YES ; then
       $2="--target=$LlvmTarget $$2"
       $3="--target=$LlvmTarget $$3"
       AC_MSG_NOTICE([Result 2: $2])
       AC_MSG_NOTICE([Result 3: $3])
   fi
])

