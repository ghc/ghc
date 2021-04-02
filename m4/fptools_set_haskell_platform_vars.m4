# FPTOOLS_SET_HASKELL_PLATFORM_VARS
# ----------------------------------
# Set the Haskell platform variables
AC_DEFUN([FPTOOLS_SET_HASKELL_PLATFORM_VARS],
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
        powerpc64)
            test -z "[$]2" || eval "[$]2=\"ArchPPC_64 ELF_V1\""
            ;;
        powerpc64le)
            test -z "[$]2" || eval "[$]2=\"ArchPPC_64 ELF_V2\""
            ;;
        s390x)
            test -z "[$]2" || eval "[$]2=ArchS390X"
            ;;
        sparc)
            test -z "[$]2" || eval "[$]2=ArchSPARC"
            ;;
        sparc64)
            test -z "[$]2" || eval "[$]2=ArchSPARC64"
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
        hppa|hppa1_1|ia64|m68k|nios2|riscv32|rs6000|s390|sh4|vax)
            test -z "[$]2" || eval "[$]2=ArchUnknown"
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
        mingw32)
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
        dragonfly|hpux|linuxaout|freebsd2|nextstep2|nextstep3|sunos4|ultrix)
            test -z "[$]2" || eval "[$]2=OSUnknown"
            ;;
        aix)
            test -z "[$]2" || eval "[$]2=OSAIX"
            ;;
        gnu)
            test -z "[$]2" || eval "[$]2=OSHurd"
            ;;
        *)
            echo "Unknown OS '[$]1'"
            exit 1
            ;;
        esac
    }

    dnl Note [autoconf assembler checks and -flto]
    dnl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dnl
    dnl Autoconf's AC_COMPILE_IFELSE macro is fragile in the case of checks
    dnl which require that the assembler is run. Specifically, GCC does not run
    dnl the assembler if invoked with `-c -flto`; it merely dumps its internal
    dnl AST to the object file, to be compiled and assembled during the final
    dnl link.
    dnl
    dnl This can cause configure checks like that for the
    dnl .subsections_via_symbols directive to pass unexpected (see #16440),
    dnl leading the build system to incorrectly conclude that the directive is
    dnl supported.
    dnl
    dnl For this reason, it is important that configure checks that rely on the
    dnl assembler failing use AC_LINK_IFELSE rather than AC_COMPILE_IFELSE,
    dnl ensuring that the assembler sees the check.
    dnl

    dnl ** check for Apple-style dead-stripping support
    dnl    (.subsections-via-symbols assembler directive)

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
            AC_DEFINE([HAVE_SUBSECTIONS_VIA_SYMBOLS],[1],
                   [Define to 1 if Apple-style dead-stripping is supported.])
         fi
        ],
        [TargetHasSubsectionsViaSymbols=NO
         AC_MSG_RESULT(no)])

    dnl ** check for .ident assembler directive

    AC_MSG_CHECKING(whether your assembler supports .ident directive)
    dnl See Note [autoconf assembler checks and -flto]
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([__asm__ (".ident \"GHC x.y.z\"");], [])],
        [AC_MSG_RESULT(yes)
         TargetHasIdentDirective=YES],
        [AC_MSG_RESULT(no)
         TargetHasIdentDirective=NO])

    dnl *** check for GNU non-executable stack note support (ELF only)
    dnl     (.section .note.GNU-stack,"",@progbits)

    dnl This test doesn't work with "gcc -g" in gcc 4.4 (GHC trac #3889:
    dnl     Error: can't resolve `.note.GNU-stack' {.note.GNU-stack section} - `.Ltext0' {.text section}
    dnl so we empty CFLAGS while running this test
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

    checkArch "$BuildArch" "HaskellBuildArch"
    checkVendor "$BuildVendor"
    checkOS "$BuildOS" ""

    checkArch "$HostArch" "HaskellHostArch"
    checkVendor "$HostVendor"
    checkOS "$HostOS" "HaskellHostOs"

    checkArch "$TargetArch" "HaskellTargetArch"
    checkVendor "$TargetVendor"
    checkOS "$TargetOS" "HaskellTargetOs"

    AC_SUBST(HaskellHostArch)
    AC_SUBST(HaskellHostOs)
    AC_SUBST(HaskellTargetArch)
    AC_SUBST(HaskellTargetOs)
    AC_SUBST(TargetHasSubsectionsViaSymbols)
    AC_SUBST(TargetHasIdentDirective)
    AC_SUBST(TargetHasGnuNonexecStack)
])
