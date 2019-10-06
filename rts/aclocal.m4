# Note [autoconf assembler checks and -flto]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
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
         HasSubsectionsViaSymbols=YES
         AC_DEFINE([HAVE_SUBSECTIONS_VIA_SYMBOLS],[1],
                   [Define to 1 if Apple-style dead-stripping is supported.])
        ],
        [HasSubsectionsViaSymbols=NO
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
         HasIdentDirective=YES],
        [AC_MSG_RESULT(no)
         HasIdentDirective=NO])
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
    case $HostArch in
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
         HasGnuNonexecStack=YES],
        [AC_MSG_RESULT(no)
         HasGnuNonexecStack=NO])
    CFLAGS="$CFLAGS2"
])

# GHC_LLVM_TARGET(target_cpu, target_vendor, target_os, llvm_target_var)
# --------------------------------
# converts the canonicalized target into someting llvm can understand
AC_DEFUN([GHC_LLVM_TARGET], [
  case "$2-$3" in
    *-freebsd*-gnueabihf)
      llvm_target_vendor="unknown"
      llvm_target_os="freebsd-gnueabihf"
      ;;
    hardfloat-*eabi)
      llvm_target_vendor="unknown"
      llvm_target_os="$3""hf"
      ;;
    *-mingw32|*-mingw64|*-msys)
      llvm_target_vendor="unknown"
      llvm_target_os="windows"
      ;;
    # retain any android and gnueabi linux flavours
    # for the LLVM Target. Otherwise these would be
    # turned into just `-linux` and fail to be found
    # in the `llvm-targets` file.
    *-android*|*-gnueabi*|*-musleabi*)
      GHC_CONVERT_VENDOR([$2],[llvm_target_vendor])
      llvm_target_os="$3"
      ;;
    *)
      GHC_CONVERT_VENDOR([$2],[llvm_target_vendor])
      GHC_CONVERT_OS([$3],[$1],[llvm_target_os])
      ;;
  esac
  $4="$1-$llvm_target_vendor-$llvm_target_os"
])

# GHC_RTS_LINKER(platform)
# ----------------------------------------
# Does platform have runtime linker support?
AC_DEFUN([GHC_RTS_LINKER], [
    case "$1" in
        powerpc64-*|powerpc64le-*|powerpc-ibm-aix*)
            TargetHasRTSLinker=NO
            ;;
        *)
            TargetHasRTSLinker=YES
            ;;
    esac
])
