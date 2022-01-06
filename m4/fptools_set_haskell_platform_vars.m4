# FPTOOLS_SET_HASKELL_PLATFORM_VARS_SHELL_FUNCTIONS
# ----------------------------------
# Drop in shell functions used by FPTOOLS_SET_HASKELL_PLATFORM_VARS
AC_DEFUN([FPTOOLS_SET_HASKELL_PLATFORM_VARS_SHELL_FUNCTIONS],
[
    checkArch() {
        case [$]1 in
        i386)
            test -z "[$]2" || eval "[$]2=ArchX86"
            ;;
        x86_64|amd64)
            test -z "[$]2" || eval "[$]2=ArchX86_64"
            ;;
        powerpc)
            test -z "[$]2" || eval "[$]2=ArchPPC"
            ;;
        powerpc64*)
            GHC_GET_POWER_ABI()
            test -z "[$]2" || eval "[$]2=\"ArchPPC_64 $POWER_ABI\""
            ;;
        s390x)
            test -z "[$]2" || eval "[$]2=ArchS390X"
            ;;
        arm)
            GET_ARM_ISA()
            test -z "[$]2" || eval "[$]2=\"ArchARM \$ARM_ISA \$ARM_ISA_EXT \$ARM_ABI\""
            ;;
        aarch64)
            test -z "[$]2" || eval "[$]2=ArchAArch64"
            ;;
        alpha)
            test -z "[$]2" || eval "[$]2=ArchAlpha"
            ;;
        mips|mipseb)
            test -z "[$]2" || eval "[$]2=ArchMipseb"
            ;;
        mipsel)
            test -z "[$]2" || eval "[$]2=ArchMipsel"
            ;;
        riscv64)
            test -z "[$]2" || eval "[$]2=ArchRISCV64"
            ;;
        wasm32)
            test -z "[$]2" || eval "[$]2=ArchWasm32"
            ;;
        loongarch64)
            test -z "[$]2" || eval "[$]2=ArchLoongArch64"
            ;;
        hppa|hppa1_1|ia64|m68k|nios2|riscv32|loongarch32|rs6000|s390|sh4|sparc|sparc64|vax)
            test -z "[$]2" || eval "[$]2=ArchUnknown"
            ;;
        javascript)
            test -z "[$]2" || eval "[$]2=ArchJavaScript"
            ;;
        *)
            echo "Unknown arch [$]1"
            exit 1
            ;;
        esac
    }

    checkVendor() {
        case [$]1 in
        dec|none|unknown|hp|apple|next|sun|sgi|ibm|montavista|portbld|alpine)
            ;;
        *)
            AC_MSG_WARN([Unknown vendor [$]1])
            ;;
        esac
    }

    checkOS() {
        case [$]1 in
        linux|linux-android)
            test -z "[$]2" || eval "[$]2=OSLinux"
            ;;
        darwin|ios|watchos|tvos)
            test -z "[$]2" || eval "[$]2=OSDarwin"
            ;;
        solaris2)
            test -z "[$]2" || eval "[$]2=OSSolaris2"
            ;;
        mingw32|mingw64|windows)
            test -z "[$]2" || eval "[$]2=OSMinGW32"
            ;;
        freebsd)
            test -z "[$]2" || eval "[$]2=OSFreeBSD"
            ;;
        dragonfly)
            test -z "[$]2" || eval "[$]2=OSDragonFly"
            ;;
        kfreebsdgnu)
            test -z "[$]2" || eval "[$]2=OSKFreeBSD"
            ;;
        openbsd)
            test -z "[$]2" || eval "[$]2=OSOpenBSD"
            ;;
        netbsd)
            test -z "[$]2" || eval "[$]2=OSNetBSD"
            ;;
        haiku)
            test -z "[$]2" || eval "[$]2=OSHaiku"
            ;;
        nto-qnx)
            test -z "[$]2" || eval "[$]2=OSQNXNTO"
            ;;
        wasi)
            test -z "[$]2" || eval "[$]2=OSWasi"
            ;;
        dragonfly|hpux|linuxaout|freebsd2|nextstep2|nextstep3|sunos4|ultrix)
            test -z "[$]2" || eval "[$]2=OSUnknown"
            ;;
        aix)
            test -z "[$]2" || eval "[$]2=OSAIX"
            ;;
        gnu)
            test -z "[$]2" || eval "[$]2=OSHurd"
            ;;
        ghcjs|js)
            test -z "[$]2" || eval "[$]2=OSGhcjs"
            ;;
        *)
            echo "Unknown OS '[$]1'"
            exit 1
            ;;
        esac
    }
])

# Note [autoconf assembler checks and -flto]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Autoconf's AC_COMPILE_IFELSE macro is fragile in the case of checks
# which require that the assembler is run. Specifically, GCC does not run
# the assembler if invoked with `-c -flto`; it merely dumps its internal
# AST to the object file, to be compiled and assembled during the final
# link.
#
# This can cause configure checks like that for the
# .subsections_via_symbols directive to pass unexpected (see #16440),
# leading the build system to incorrectly conclude that the directive is
# supported.
#
# For this reason, it is important that configure checks that rely on the
# assembler failing use AC_LINK_IFELSE rather than AC_COMPILE_IFELSE,
# ensuring that the assembler sees the check.

# GHC_SUBSECTIONS_VIA_SYMBOLS
# ----------------------------------
# check for Apple-style dead-stripping support
# (.subsections-via-symbols assembler directive)
AC_DEFUN([GHC_SUBSECTIONS_VIA_SYMBOLS],
[
    AC_MSG_CHECKING(for .subsections_via_symbols)
    dnl See Note [autoconf assembler checks and -flto]
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([], [__asm__ (".subsections_via_symbols");])],
        [AC_MSG_RESULT(yes)
         if test x"$TargetArch" = xaarch64; then
            dnl subsections via symbols is busted on arm64
            TargetHasSubsectionsViaSymbols=NO
         else
            TargetHasSubsectionsViaSymbols=YES
         fi
        ],
        [TargetHasSubsectionsViaSymbols=NO
         AC_MSG_RESULT(no)])
])

# GHC_IDENT_DIRECTIVE
# ----------------------------------
# check for .ident assembler directive
AC_DEFUN([GHC_IDENT_DIRECTIVE],
[
    AC_MSG_CHECKING(whether your assembler supports .ident directive)
    dnl See Note [autoconf assembler checks and -flto]
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([__asm__ (".ident \"GHC x.y.z\"");], [])],
        [AC_MSG_RESULT(yes)
         TargetHasIdentDirective=YES],
        [AC_MSG_RESULT(no)
         TargetHasIdentDirective=NO])
])

# GHC_GNU_NONEXEC_STACK
# ----------------------------------
# *** check for GNU non-executable stack note support (ELF only)
#     (.section .note.GNU-stack,"",@progbits)
#
# This test doesn't work with "gcc -g" in gcc 4.4 (GHC trac #3889:
#     Error: can't resolve `.note.GNU-stack' {.note.GNU-stack section} - `.Ltext0' {.text section}
# so we empty CFLAGS while running this test
AC_DEFUN([GHC_GNU_NONEXEC_STACK],
[
    CFLAGS2="$CFLAGS"
    CFLAGS=
    case $TargetArch in
      arm)
        dnl See #13937.
        progbits="%progbits"
        ;;
      *)
        progbits="@progbits"
        ;;
    esac
    AC_MSG_CHECKING(for GNU non-executable stack support)
    dnl See Note [autoconf assembler checks and -flto]
    AC_LINK_IFELSE(
       dnl the `main` function is placed after the .note.GNU-stack directive
       dnl so we need to ensure that the active segment is correctly set,
       dnl otherwise `main` will be placed in the wrong segment.
        [AC_LANG_PROGRAM([
           __asm__ (".section .note.GNU-stack,\"\",$progbits");
           __asm__ (".section .text");
         ], [0])],
        [AC_MSG_RESULT(yes)
         TargetHasGnuNonexecStack=YES],
        [AC_MSG_RESULT(no)
         TargetHasGnuNonexecStack=NO])
    CFLAGS="$CFLAGS2"
])

# FPTOOLS_SET_HASKELL_PLATFORM_VARS
# ----------------------------------
# Set the Haskell platform variables
AC_DEFUN([FPTOOLS_SET_HASKELL_PLATFORM_VARS],
[
    AC_REQUIRE([FPTOOLS_SET_HASKELL_PLATFORM_VARS_SHELL_FUNCTIONS])
    checkArch "[$]$1Arch" "Haskell$1Arch"
    checkVendor "[$]$1Vendor"
    checkOS "[$]$1OS" "Haskell$1Os"
])
