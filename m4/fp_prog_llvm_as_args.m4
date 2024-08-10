# FP_PROG_LLVM_AS_ARGS
# ---------------
#
# Sets the arguments of the LLVM assembler ($LLVMAS; usually clang.)
AC_DEFUN([FP_PROG_LLVM_AS_ARGS],
[
AC_REQUIRE([GHC_LLVM_TARGET_SET_VAR])
AC_REQUIRE([GHC_LLVM_HOST_TARGET_SET_VAR])
# Cross-compiling: We need to define the target triple for the LLVM assembler.
# Otherwise, LLVMAS tries to build for the host architecture. Defining the host
# target is not strictly necessary, but it usually helps to be specific about
# the build options.
LlvmAsArgsTarget="--target=$LlvmTarget"
LlvmAsArgsHost="--target=$LlvmHostTarget"

# Create a minimal LLVM IR file for testing
cat > test.ll <<EOF
define i32 @main() {
entry:
  ret i32 0
}
EOF

AC_MSG_CHECKING([whether \$LLVMAS supports host flags passed by GHC when compiling])
if $LLVMAS $LlvmAsArgsHost test.ll > /dev/null 2>&1 ; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([\$LLVMAS ($LLVMAS) does not support host flags: $LlvmAsArgsHost])
fi

AC_MSG_CHECKING([whether \$LLVMAS supports target flags passed by GHC when compiling])
if $LLVMAS $LlvmAsArgsTarget test.ll > /dev/null 2>&1 ; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([\$LLVMAS ($LLVMAS) does not support target flags: $LlvmAsArgsTarget])
fi

AC_SUBST(LlvmAsArgsHost)
AC_SUBST(LlvmAsArgsTarget)
])# FP_PROG_LLVM_AS_ARGS
