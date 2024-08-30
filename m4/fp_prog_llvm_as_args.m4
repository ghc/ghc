# FP_PROG_LLVM_AS_ARGS
# ---------------
#
# Sets the arguments of the LLVM assembler ($LLVMAS; usually clang.)
AC_DEFUN([FP_PROG_LLVM_AS_ARGS],
[
AC_REQUIRE([GHC_LLVM_TARGET_SET_VAR])
AC_REQUIRE([GHC_LLVM_HOST_TARGET_SET_VAR])
# Cross-compiling: We need to define the target triple for the LLVM assembler.
# Otherwise, $LLVMAS tries to build for the host architecture.
LlvmAsArgsTarget="--target=$LlvmTarget"
# Usually, the installed LLVMAS should be able to assemble for the host. There
# may be special setups for cross-compiling where this is not the case, because
# only the last stage uses the LLVM backend (and thus LLVMAS.) If this happens,
# we just set this variable to empty such that it will succeed as long as LLVMAS
# exists.
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
    AC_MSG_WARN([\$LLVMAS ($LLVMAS) does not support host flags: $LlvmAsArgsHost.])
    AC_MSG_WARN([Falling back to no flags (won't be able to build stages for the host architecture with \$LLVMAS).])
    # Here LLVMAS cannot assemble for the host. I.e. we won't be able to use it
    # to build intermediate GHC stages (with host target). This ressembles old
    # behaviour and is added for backwards compatibility.
    LlvmAsArgsHost=""
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
