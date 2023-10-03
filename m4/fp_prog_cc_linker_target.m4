# FP_PROG_CC_LINKER_TARGET
# -------------------
# Check to see if the C compiler used as a linker supports `--target`
#
# $1 - The compiler
# $2 - Variable which contains the options passed to the C compiler when compiling a C file
# $3 - Variable which contains the options passed to the C compiler when used as
#      a linker
AC_DEFUN([FP_PROG_CC_LINKER_TARGET],
[
    AC_MSG_CHECKING([whether $CC used as a linker understands --target])

    echo 'int foo() { return 0; }' > conftest1.c
    echo 'int main() { return 0; }' > conftest2.c
    "$1" $$2 -c conftest1.c || AC_MSG_ERROR([Failed to compile conftest1.c])
    "$1" $$2 -c conftest2.c || AC_MSG_ERROR([Failed to compile conftest2.c])

    if test "$target_cpu" = "javascript"
    then
        # See Note [Don't pass --target to emscripten toolchain] in GHC.Toolchain.Program
        CONF_CC_SUPPORTS_TARGET=NO
        AC_MSG_RESULT([no])
    elif "$CC" $$3 --target=$LlvmTarget -o conftest conftest1.o conftest2.o;
    then
        $3="--target=$LlvmTarget $$3"
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    rm -rf conftest*
])# FP_PROG_CC_LINKER_TARGET
