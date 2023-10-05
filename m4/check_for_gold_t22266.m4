# CHECK_FOR_GOLD_T22266
# ----------------------
#
# Test for binutils #22266. This bug manifested as GHC bug #14328 (see also:
# #14675, #14291).
# Uses test from
# https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;h=033bfb739b525703bfe23f151d09e9beee3a2afe
#
# $1 = linker to test
# Sets $result to 0 if not affected, 1 otherwise
AC_DEFUN([CHECK_FOR_GOLD_T22266],[
    AC_MSG_CHECKING([for ld.gold object merging bug (binutils 22266)])
    if ! $1 --version | grep -q "GNU gold" 2>/dev/null; then
        # Not gold
        result=0
    elif test "$cross_compiling" = "yes"; then
        AC_MSG_RESULT([cross-compiling, assuming LD can merge objects correctly.])
        result=0
    else
        FPTOOLS_WRITE_FILE([conftest.a.c], [
          __attribute__((section(".data.a")))
          static int int_from_a_1 = 0x11223344;

          __attribute__((section(".data.rel.ro.a")))
          int *p_int_from_a_2 = &int_from_a_1;

          const char *hello (void);

          const char *
          hello (void)
          {
            return "XXXHello, world!" + 3;
          }
        ])

        FPTOOLS_WRITE_FILE([conftest.main.c], [
          #include <stdlib.h>
          #include <string.h>

          extern int *p_int_from_a_2;
          extern const char *hello (void);

          int main (void) {
            if (*p_int_from_a_2 != 0x11223344)
              abort ();
            if (strcmp(hello(), "Hello, world!") != 0)
              abort ();
            return 0;
          }
        ])

        FPTOOLS_WRITE_FILE([conftest.t], [
          SECTIONS
          {
              .text : {
                  *(.text*)
              }
              .rodata :
              {
                  *(.rodata .rodata.* .gnu.linkonce.r.*)
              }
              .data.rel.ro : {
                  *(.data.rel.ro*)
              }
              .data : {
                  *(.data*)
              }
              .bss : {
                  *(.bss*)
              }
          }
        ])

        $CC -c -o conftest.a.o conftest.a.c || AC_MSG_ERROR([Failed to compile test])
        $MergeObjsCmd $MergeObjsArgs -T conftest.t conftest.a.o -o conftest.ar.o || AC_MSG_ERROR([Failed to merge test object])

        $CC -c -o conftest.main.o conftest.main.c || AC_MSG_ERROR([Failed to compile test driver])
        $CC conftest.ar.o conftest.main.o -o conftest || AC_MSG_ERROR([Failed to link test driver])

        if ./conftest; then
            AC_MSG_RESULT([not affected])
            result=0
        else
            AC_MSG_RESULT([affected])
            result=1
        fi
        rm -f conftest.a.o conftest.a.c  conttest.ar.o conftest.main.c conftest.main.o conftest
    fi
])
