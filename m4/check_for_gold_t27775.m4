# CHECK_FOR_GOLD_T27775
# ----------------------
#
# Test for binutils #27775.
#
# Uses test from
# https://sourceware.org/bugzilla/show_bug.cgi?id=27775
#
# $1 = linker to test
# Sets $result to 0 if not affected, 1 otherwise
AC_DEFUN([CHECK_FOR_GOLD_T27775],[
    AC_REQUIRE([FIND_LD])
    AC_MSG_CHECKING([for ld.gold gc-sections with note section bug (binutils 27775)])
    if ! $1 --version | grep -q "GNU gold"; then
        # Not gold
        result=0
    else
        FPTOOLS_WRITE_FILE([conftest.a.s], [
          .section .note.stapsdt,"?","note"
          .dc.a _.stapsdt.base
          .section .stapsdt.base,"aG","progbits",.stapsdt.base,comdat
        _.stapsdt.base: .space 1
          .size _.stapsdt.base,1
        ])

        FPTOOLS_WRITE_FILE([conftest.b.s], [
          .text
          .global start	/* Used by SH targets.  */
        start:
          .global _start
        _start:
          .global __start
        __start:
          .global main	/* Used by HPPA targets.  */
        main:
          .dc.a 0
        ])

        $CC -c -o conftest.a.o conftest.a.s || AC_MSG_ERROR([Failed to compile test])
        $CC -c -o conftest.b.o conftest.b.s || AC_MSG_ERROR([Failed to compile test])
        if $1 --gc-sections -o conftest conftest.a.o conftest.b.o; then
            AC_MSG_RESULT([not affected])
            result=0
        else
            AC_MSG_RESULT([affected])
            result=1
        fi

        rm -f conftest.a.o conftest.a.s  conttest.b.o conftest.b.c conftest

        if test "$result" = "1"; then
            AC_MSG_ERROR([ld.gold suffers from bugs with dtrace probes, turn off dtrace or use another linker])
        fi
    fi
])
