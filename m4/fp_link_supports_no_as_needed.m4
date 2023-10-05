# FP_LINK_SUPPORTS_NO_AS_NEEDED
# ----------------------------------
# Set the Cc linker flag -Wl,--no-as-needed if it is supported
# $1 is the name of the linker flags variable when linking with gcc
# See also Note [ELF needed shared libs]
AC_DEFUN([FP_LINK_SUPPORTS_NO_AS_NEEDED],
[
    AC_MSG_CHECKING([whether Cc linker supports -Wl,--no-as-needed])
    echo 'int f(int a) {return 2*a;}' > conftest.a.c
    echo 'int f(int a); int main(int argc, char **argv) {return f(0);}' > conftest.b.c
    $CC -c -o conftest.a.o conftest.a.c  2>&1
    $CC -c -o conftest.b.o conftest.b.c  2>&1
    if "$CC" ${$1:+$$1} -Wl,--no-as-needed -o conftest conftest.a.o conftest.b.o 2>&1
    then
        $1="$$1 -Wl,--no-as-needed"
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest*
])

# Note [ELF needed shared libs]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some distributions change the link editor's default handling of
# ELF DT_NEEDED tags to include only those shared objects that are
# needed to resolve undefined symbols. For Template Haskell we need
# the last temporary shared library also if it is not needed for the
# currently linked temporary shared library. We specify --no-as-needed
# to override the default. This flag exists in GNU ld and GNU gold.
#
# The flag is only needed on ELF systems. On Windows (PE) and Mac OS X
# (Mach-O) the flag is not needed.
