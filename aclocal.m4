# Extra autoconf macros for the Glasgow fptools
#
# To be a good autoconf citizen, names of local macros have prefixed with FP_ to
# ensure we don't clash with any pre-supplied autoconf ones.


AC_DEFUN([GHC_SELECT_FILE_EXTENSIONS],
[
    $2=''
    $3='.so'
    case $1 in
    *-unknown-cygwin32)
        AC_MSG_WARN([GHC does not support the Cygwin target at the moment])
        AC_MSG_WARN([I'm assuming you wanted to build for i386-unknown-mingw32])
        exit 1
        ;;
    *-unknown-mingw32)
        windows=YES
        $2='.exe'
        $3='.dll'
        ;;
    i386-apple-darwin|powerpc-apple-darwin)
        $3='.dylib'
        ;;
    x86_64-apple-darwin)
        $3='.dylib'
        ;;
    arm-apple-darwin10|i386-apple-darwin11)
        $2='.a'
        $3='.dylib'
        ;;
    esac
])

# FPTOOLS_SET_PLATFORM_VARS
# ----------------------------------
# Set the platform variables
AC_DEFUN([FPTOOLS_SET_PLATFORM_VARS],
[
    # If no argument was given for a configuration variable, then discard
    # the guessed canonical system and use the configuration of the
    # bootstrapping ghc. If an argument was given, map it from gnu format
    # to ghc format.
    #
    # For why we do it this way, see: #3637, #1717, #2951
    #
    # In bindists, we haven't called AC_CANONICAL_{BUILD,HOST,TARGET}
    # so this justs uses $bootstrap_target.

    if test "$build_alias" = ""
    then
        if test "$bootstrap_target" != ""
        then
            build=$bootstrap_target
            echo "Build platform inferred as: $build"
        else
            echo "Can't work out build platform"
            exit 1
        fi

        BuildArch=`echo "$build" | sed 's/-.*//'`
        BuildVendor=`echo "$build" | sed -e 's/.*-\(.*\)-.*/\1/'`
        BuildOS=`echo "$build" | sed 's/.*-//'`
    else
        GHC_CONVERT_CPU([$build_cpu], [BuildArch])
        GHC_CONVERT_VENDOR([$build_vendor], [BuildVendor])
        GHC_CONVERT_OS([$build_os], [$BuildArch], [BuildOS])
    fi

    if test "$host_alias" = ""
    then
        if test "$bootstrap_target" != ""
        then
            host=$bootstrap_target
            echo "Host platform inferred as: $host"
        else
            echo "Can't work out host platform"
            exit 1
        fi

        HostArch=`echo "$host" | sed 's/-.*//'`
        HostVendor=`echo "$host" | sed -e 's/.*-\(.*\)-.*/\1/'`
        HostOS=`echo "$host" | sed 's/.*-//'`
    else
        GHC_CONVERT_CPU([$host_cpu], [HostArch])
        GHC_CONVERT_VENDOR([$host_vendor], [HostVendor])
        GHC_CONVERT_OS([$host_os], [$HostArch], [HostOS])
    fi

    if test "$target_alias" = ""
    then
        if test "$host_alias" != ""
        then
            GHC_CONVERT_CPU([$host_cpu], [TargetArch])
            GHC_CONVERT_VENDOR([$host_vendor], [TargetVendor])
            GHC_CONVERT_OS([$host_os], [$TargetArch],[TargetOS])
        else
            if test "$bootstrap_target" != ""
            then
                target=$bootstrap_target
                echo "Target platform inferred as: $target"
            else
                echo "Can't work out target platform"
                exit 1
            fi

            TargetArch=`echo "$target" | sed 's/-.*//'`
            TargetVendor=`echo "$target" | sed -e 's/.*-\(.*\)-.*/\1/'`
            TargetOS=`echo "$target" | sed 's/.*-//'`
        fi
    else
        GHC_CONVERT_CPU([$target_cpu], [TargetArch])
        GHC_CONVERT_VENDOR([$target_vendor], [TargetVendor])
        GHC_CONVERT_OS([$target_os], [$TargetArch], [TargetOS])
    fi

    GHC_SELECT_FILE_EXTENSIONS([$host], [exeext_host], [soext_host])
    GHC_SELECT_FILE_EXTENSIONS([$target], [exeext_target], [soext_target])
    windows=NO
    case $host in
    *-unknown-mingw32)
        windows=YES
        ;;
    esac

    BuildPlatform="$BuildArch-$BuildVendor-$BuildOS"
    BuildPlatform_CPP=`echo "$BuildPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildArch_CPP=`    echo "$BuildArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildVendor_CPP=`  echo "$BuildVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildOS_CPP=`      echo "$BuildOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    HostPlatform="$HostArch-$HostVendor-$HostOS"
    HostPlatform_CPP=`echo "$HostPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostArch_CPP=`    echo "$HostArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostVendor_CPP=`  echo "$HostVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostOS_CPP=`      echo "$HostOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    TargetPlatform="$TargetArch-$TargetVendor-$TargetOS"
    TargetPlatform_CPP=`echo "$TargetPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetArch_CPP=`    echo "$TargetArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetVendor_CPP=`  echo "$TargetVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetOS_CPP=`      echo "$TargetOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    echo "GHC build  : $BuildPlatform"
    echo "GHC host   : $HostPlatform"
    echo "GHC target : $TargetPlatform"

    AC_SUBST(BuildPlatform)
    AC_SUBST(HostPlatform)
    AC_SUBST(TargetPlatform)
    AC_SUBST(HostPlatform_CPP)
    AC_SUBST(BuildPlatform_CPP)
    AC_SUBST(TargetPlatform_CPP)

    AC_SUBST(HostArch_CPP)
    AC_SUBST(BuildArch_CPP)
    AC_SUBST(TargetArch_CPP)

    AC_SUBST(HostOS_CPP)
    AC_SUBST(BuildOS_CPP)
    AC_SUBST(TargetOS_CPP)

    AC_SUBST(HostVendor_CPP)
    AC_SUBST(BuildVendor_CPP)
    AC_SUBST(TargetVendor_CPP)

    AC_SUBST(exeext_host)
    AC_SUBST(exeext_target)
    AC_SUBST(soext_host)
    AC_SUBST(soext_target)
])


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
        x86_64)
            test -z "[$]2" || eval "[$]2=ArchX86_64"
            ;;
        powerpc)
            test -z "[$]2" || eval "[$]2=ArchPPC"
            ;;
        powerpc64)
            test -z "[$]2" || eval "[$]2=ArchPPC_64"
            ;;
        sparc)
            test -z "[$]2" || eval "[$]2=ArchSPARC"
            ;;
        arm)
            GET_ARM_ISA()
            test -z "[$]2" || eval "[$]2=\"ArchARM {armISA = \$ARM_ISA, armISAExt = \$ARM_ISA_EXT, armABI = \$ARM_ABI}\""
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
        hppa|hppa1_1|ia64|m68k|rs6000|s390|s390x|sparc64|vax)
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
        dec|unknown|hp|apple|next|sun|sgi|ibm|montavista|portbld)
            ;;
        *)
            echo "Unknown vendor [$]1"
            exit 1
            ;;
        esac
    }

    checkOS() {
        case [$]1 in
        linux)
            test -z "[$]2" || eval "[$]2=OSLinux"
            ;;
        ios)
            test -z "[$]2" || eval "[$]2=OSiOS"
            ;;
        darwin)
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
        osf3)
            test -z "[$]2" || eval "[$]2=OSOsf3"
            ;;
        nto-qnx)
            test -z "[$]2" || eval "[$]2=OSQNXNTO"
            ;;
        dragonfly|osf1|hpux|linuxaout|freebsd2|cygwin32|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix)
            test -z "[$]2" || eval "[$]2=OSUnknown"
            ;;
        linux-android)
            test -z "[$]2" || eval "[$]2=OSAndroid"
            ;;
        *)
            echo "Unknown OS '[$]1'"
            exit 1
            ;;
        esac
    }

    dnl ** check for Apple-style dead-stripping support
    dnl    (.subsections-via-symbols assembler directive)

    AC_MSG_CHECKING(for .subsections_via_symbols)
    AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([], [__asm__ (".subsections_via_symbols");])],
        [AC_MSG_RESULT(yes)
         HaskellHaveSubsectionsViaSymbols=True
         AC_DEFINE([HAVE_SUBSECTIONS_VIA_SYMBOLS],[1],
                   [Define to 1 if Apple-style dead-stripping is supported.])
        ],
        [HaskellHaveSubsectionsViaSymbols=False
         AC_MSG_RESULT(no)])

    dnl ** check for .ident assembler directive

    AC_MSG_CHECKING(whether your assembler supports .ident directive)
    AC_COMPILE_IFELSE(
        [AC_LANG_SOURCE([__asm__ (".ident \"GHC x.y.z\"");])],
        [AC_MSG_RESULT(yes)
         HaskellHaveIdentDirective=True],
        [AC_MSG_RESULT(no)
         HaskellHaveIdentDirective=False])

    dnl *** check for GNU non-executable stack note support (ELF only)
    dnl     (.section .note.GNU-stack,"",@progbits)

    dnl This test doesn't work with "gcc -g" in gcc 4.4 (GHC trac #3889:
    dnl     Error: can't resolve `.note.GNU-stack' {.note.GNU-stack section} - `.Ltext0' {.text section}
    dnl so we empty CFLAGS while running this test
    CFLAGS2="$CFLAGS"
    CFLAGS=
    AC_MSG_CHECKING(for GNU non-executable stack support)
    AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([__asm__ (".section .note.GNU-stack,\"\",@progbits");], [0])],
        [AC_MSG_RESULT(yes)
         HaskellHaveGnuNonexecStack=True],
        [AC_MSG_RESULT(no)
         HaskellHaveGnuNonexecStack=False])
    CFLAGS="$CFLAGS2"

    checkArch "$BuildArch" "HaskellBuildArch"
    checkVendor "$BuildVendor"
    checkOS "$BuildOS" ""

    checkArch "$HostArch" "HaskellHostArch"
    checkVendor "$HostVendor"
    checkOS "$HostOS" ""

    checkArch "$TargetArch" "HaskellTargetArch"
    checkVendor "$TargetVendor"
    checkOS "$TargetOS" "HaskellTargetOs"

    AC_SUBST(HaskellTargetArch)
    AC_SUBST(HaskellTargetOs)
    AC_SUBST(HaskellHaveSubsectionsViaSymbols)
    AC_SUBST(HaskellHaveIdentDirective)
    AC_SUBST(HaskellHaveGnuNonexecStack)
])


# GET_ARM_ISA
# ----------------------------------
# Get info about the ISA on the ARM arch
AC_DEFUN([GET_ARM_ISA],
[
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [],
            [#if defined(__ARM_ARCH_2__)  || \
                 defined(__ARM_ARCH_3__)  || \
                 defined(__ARM_ARCH_3M__) || \
                 defined(__ARM_ARCH_4__)  || \
                 defined(__ARM_ARCH_4T__) || \
                 defined(__ARM_ARCH_5__)  || \
                 defined(__ARM_ARCH_5T__) || \
                 defined(__ARM_ARCH_5E__) || \
                 defined(__ARM_ARCH_5TE__)
                 return 0;
             #else
                 not pre arm v6
             #endif]
        )],
        [AC_DEFINE(arm_HOST_ARCH_PRE_ARMv6, 1, [ARM pre v6])
         AC_DEFINE(arm_HOST_ARCH_PRE_ARMv7, 1, [ARM pre v7])
         changequote(, )dnl
         ARM_ISA=ARMv5
         ARM_ISA_EXT="[]"
         changequote([, ])dnl
        ],
        [
            AC_COMPILE_IFELSE([
                AC_LANG_PROGRAM(
                    [],
                    [#if defined(__ARM_ARCH_6__)   || \
                         defined(__ARM_ARCH_6J__)  || \
                         defined(__ARM_ARCH_6T2__) || \
                         defined(__ARM_ARCH_6Z__)  || \
                         defined(__ARM_ARCH_6ZK__) || \
                         defined(__ARM_ARCH_6M__)
                         return 0;
                     #else
                         not pre arm v7
                     #endif]
                )],
                [AC_DEFINE(arm_HOST_ARCH_PRE_ARMv7, 1, [ARM pre v7])
                 ARM_ISA=ARMv6
                 AC_COMPILE_IFELSE([
                        AC_LANG_PROGRAM(
                                [],
                                [#if defined(__VFP_FP__)
                                     return 0;
                                #else
                                     no vfp
                                #endif]
                        )],
                        [changequote(, )dnl
                         ARM_ISA_EXT="[VFPv2]"
                         changequote([, ])dnl
                        ],
                        [changequote(, )dnl
                         ARM_ISA_EXT="[]"
                         changequote([, ])dnl
                        ]
                )],
                [changequote(, )dnl
                 ARM_ISA=ARMv7
                 ARM_ISA_EXT="[VFPv3,NEON]"
                 changequote([, ])dnl
                ])
        ])

        AC_COMPILE_IFELSE(
               [AC_LANG_PROGRAM(
                       [],
                       [#if defined(__SOFTFP__)
                            return 0;
                       #else
                            not softfp
                       #endif]
               )],
               [changequote(, )dnl
                ARM_ABI="SOFT"
                changequote([, ])dnl
               ],
               [AC_COMPILE_IFELSE(
                    [AC_LANG_PROGRAM(
                       [],
                       [#if defined(__ARM_PCS_VFP)
                            return 0;
                       #else
                            no hard float ABI
                       #endif]
                    )],
                    [ARM_ABI="HARD"],
                    [ARM_ABI="SOFTFP"]
               )]
        )
])


# FP_SETTINGS
# ----------------------------------
# Set the variables used in the settings file
AC_DEFUN([FP_SETTINGS],
[
    if test "$windows" = YES
    then
        mingw_bin_prefix=mingw/bin/
        SettingsCCompilerCommand="\$topdir/../${mingw_bin_prefix}gcc.exe"
        SettingsLdCommand="\$topdir/../${mingw_bin_prefix}ld.exe"
        SettingsArCommand="\$topdir/../${mingw_bin_prefix}ar.exe"
        SettingsPerlCommand='$topdir/../perl/perl.exe'
        SettingsDllWrapCommand="\$topdir/../${mingw_bin_prefix}dllwrap.exe"
        SettingsWindresCommand="\$topdir/../${mingw_bin_prefix}windres.exe"
        SettingsTouchCommand='$topdir/touchy.exe'
    else
        SettingsCCompilerCommand="$WhatGccIsCalled"
        SettingsLdCommand="$LdCmd"
        SettingsArCommand="$ArCmd"
        SettingsPerlCommand="$PerlCmd"
        SettingsDllWrapCommand="/bin/false"
        SettingsWindresCommand="/bin/false"
        SettingsTouchCommand='touch'
        if test -z "$LlcCmd"
        then
          SettingsLlcCommand="llc"
        else
          SettingsLlcCommand="$LlcCmd"
        fi
        if test -z "$OptCmd"
        then
          SettingsOptCommand="opt"
        else
          SettingsOptCommand="$OptCmd"
        fi
    fi
    SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
    SettingsCCompilerLinkFlags="$CONF_GCC_LINKER_OPTS_STAGE2"
    SettingsLdFlags="$CONF_LD_LINKER_OPTS_STAGE2"
    AC_SUBST(SettingsCCompilerCommand)
    AC_SUBST(SettingsCCompilerFlags)
    AC_SUBST(SettingsCCompilerLinkFlags)
    AC_SUBST(SettingsLdCommand)
    AC_SUBST(SettingsLdFlags)
    AC_SUBST(SettingsArCommand)
    AC_SUBST(SettingsPerlCommand)
    AC_SUBST(SettingsDllWrapCommand)
    AC_SUBST(SettingsWindresCommand)
    AC_SUBST(SettingsTouchCommand)
    AC_SUBST(SettingsLlcCommand)
    AC_SUBST(SettingsOptCommand)
])


# FPTOOLS_SET_C_LD_FLAGS
# ----------------------------------
# Set the C, LD and CPP flags for a given platform
# $1 is the platform
# $2 is the name of the CC flags variable
# $3 is the name of the linker flags variable when linking with gcc
# $4 is the name of the linker flags variable when linking with ld
# $5 is the name of the CPP flags variable
AC_DEFUN([FPTOOLS_SET_C_LD_FLAGS],
[
    AC_MSG_CHECKING([Setting up $2, $3, $4 and $5])
    case $$1 in
    i386-*)
        # Workaround for #7799
        $2="$$2 -U__i686"
        ;;
    esac

    case $$1 in
    i386-apple-darwin)
        $2="$$2 -m32"
        $3="$$3 -m32"
        $4="$$4 -arch i386"
        $5="$$5 -m32"
        ;;
    x86_64-apple-darwin)
        $2="$$2 -m64"
        $3="$$3 -m64"
        $4="$$4 -arch x86_64"
        $5="$$5 -m64"
        ;;
    alpha-*)
        # For now, to suppress the gcc warning "call-clobbered
        # register used for global register variable", we simply
        # disable all warnings altogether using the -w flag. Oh well.
        $2="$$2 -w -mieee -D_REENTRANT"
        $3="$$3 -w -mieee -D_REENTRANT"
        $5="$$5 -w -mieee -D_REENTRANT"
        ;;
    hppa*)
        # ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        # (very nice, but too bad the HP /usr/include files don't agree.)
        $2="$$2 -D_HPUX_SOURCE"
        $3="$$3 -D_HPUX_SOURCE"
        $5="$$5 -D_HPUX_SOURCE"
        ;;
    esac

    # If gcc knows about the stack protector, turn it off.
    # Otherwise the stack-smash handler gets triggered.
    echo 'int main(void) {return 0;}' > conftest.c
    if $CC -c conftest.c -fno-stack-protector > /dev/null 2>&1
    then
        $2="$$2 -fno-stack-protector"
    fi

    rm -f conftest.c conftest.o
    AC_MSG_RESULT([done])
])


AC_DEFUN([FP_PATH_PROG],[
    AC_PATH_PROG($1,$2,$3,$4,$5,$6)
    # If we have a cygwin path for something, and we try to run it
    # from cabal or python, then it'll fail. So we convert to a
    # native path.
    if test "$HostOS"     = "mingw32" && \
       test "${OSTYPE}"  != "msys"    && \
       test "${$1}" != ""
    then
        # Canonicalise to <drive>:/path/to/gcc
        $1=`cygpath -m "${$1}"`
    fi
])


# FP_VISIBILITY_HIDDEN
# ----------------------------------
# Is the visibility hidden attribute supported?
AC_DEFUN([FP_VISIBILITY_HIDDEN],
[
    AC_MSG_CHECKING([whether __attribute__((visibility("hidden"))) is supported])
    echo '__attribute__((visibility("hidden"))) void foo(void) {}' > conftest.c
    if $CC -Wall -Werror -c conftest.c > /dev/null 2>&1
    then
        AC_MSG_RESULT([yes])
        AC_DEFINE(HAS_VISIBILITY_HIDDEN, 1, [Has visibility hidden])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest.o
])


# FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN
# ----------------------------------
# Little endian ARM on Linux with some ABIs has big endian word order
# in doubles. Define FLOAT_WORDS_BIGENDIAN if this is the case.
AC_DEFUN([FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN],
  [AC_CACHE_CHECK([whether float word order is big endian], [fptools_cv_float_word_order_bigendian],
    [AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM(
        [#include <endian.h>],
        [#if defined(__FLOAT_WORD_ORDER) && __FLOAT_WORD_ORDER == BIG_ENDIAN
             return 0;
         #else
             not float word order big endian
         #endif]
      )],
      [fptools_cv_float_word_order_bigendian=yes],
      [fptools_cv_float_word_order_bigendian=no])
    ])
  case $fptools_cv_float_word_order_bigendian in
      yes)
          AC_DEFINE([FLOAT_WORDS_BIGENDIAN], 1,
          [Define to 1 if your processor stores words of floats with
           the most significant byte first]) ;;
  esac
])


# FP_ARG_WITH_PATH_GNU_PROG
# --------------------
# Find the specified command on the path or allow a user to set it manually
# with a --with-<command> option. An error will be thrown if the command isn't
# found.
#
# This is ignored on the mingw32 platform.
#
# $1 = the variable to set
# $2 = the with option name
# $3 = the command to look for
#
AC_DEFUN([FP_ARG_WITH_PATH_GNU_PROG],
[
AC_ARG_WITH($2,
[AC_HELP_STRING([--with-$2=ARG],
        [Use ARG as the path to $2 [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        $1=$withval
    fi
],
[
    if test "$HostOS" != "mingw32"
    then
        if test "$target_alias" = "" ; then
            AC_PATH_PROG([$1], [$3])
        else
            AC_PATH_PROG([$1], [$target_alias-$3])
        fi
        if test -z "$$1"
        then
            AC_MSG_ERROR([cannot find $3 in your PATH])
        fi
    fi
]
)
]) # FP_ARG_WITH_PATH_GNU_PROG


# FP_ARG_WITH_PATH_GNU_PROG_OPTIONAL
# --------------------
# Same as FP_ARG_WITH_PATH_GNU_PROG but no error will be thrown if the command
# isn't found.
#
# This is ignored on the mingw32 platform.
#
# $1 = the variable to set
# $2 = the with option name
# $3 = the command to look for
#
AC_DEFUN([FP_ARG_WITH_PATH_GNU_PROG_OPTIONAL],
[
AC_ARG_WITH($2,
[AC_HELP_STRING([--with-$2=ARG],
        [Use ARG as the path to $2 [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        $1=$withval
    fi
],
[
    if test "$HostOS" != "mingw32"
    then
        AC_PATH_PROG([$1], [$3])
    fi
]
)
]) # FP_ARG_WITH_PATH_GNU_PROG_OPTIONAL

# FP_PROG_CONTEXT_DIFF
# --------------------
# Figure out how to do context diffs. Sets the output variable ContextDiffCmd.
#
# Note: NeXTStep thinks diff'ing a file against itself is "trouble".
AC_DEFUN([FP_PROG_CONTEXT_DIFF],
[AC_CACHE_CHECK([for a working context diff], [fp_cv_context_diff],
[echo foo > conftest1
echo foo > conftest2
fp_cv_context_diff=no
for fp_var in '-U 1' '-u1' '-C 1' '-c1'
do
  if diff $fp_var conftest1 conftest2 > /dev/null 2>&1; then
    fp_cv_context_diff="diff $fp_var"
    break
  fi
done])
if test x"$fp_cv_context_diff" = xno; then
   AC_MSG_ERROR([cannot figure out how to do context diffs])
fi
AC_SUBST(ContextDiffCmd, [$fp_cv_context_diff])
])# FP_PROG_CONTEXT_DIFF


# FP_COMPUTE_INT(EXPRESSION, VARIABLE, INCLUDES, IF-FAILS)
# --------------------------------------------------------
# Assign VARIABLE the value of the compile-time EXPRESSION using INCLUDES for
# compilation. Execute IF-FAILS when unable to determine the value. Works for
# cross-compilation, too.
#
# Implementation note: We are lazy and use an internal autoconf macro, but it
# is supported in autoconf versions 2.50 up to the actual 2.57, so there is
# little risk.
AC_DEFUN([FP_COMPUTE_INT],
[_AC_COMPUTE_INT([$1], [$2], [$3], [$4])[]dnl
])# FP_COMPUTE_INT


# FP_CHECK_ALIGNMENT(TYPE, [IGNORED], [INCLUDES = DEFAULT-INCLUDES])
# ------------------------------------------------------------------
# A variation of AC_CHECK_SIZEOF for computing the alignment restrictions of a
# given type. Defines ALIGNMENT_TYPE.
AC_DEFUN([FP_CHECK_ALIGNMENT],
[AS_LITERAL_IF(m4_translit([[$1]], [*], [p]), [],
               [AC_FATAL([$0: requires literal arguments])])[]dnl
AC_CHECK_TYPE([$1], [], [], [$3])[]dnl
m4_pushdef([fp_Cache], [AS_TR_SH([fp_cv_alignment_$1])])[]dnl
AC_CACHE_CHECK([alignment of $1], [fp_Cache],
[if test "$AS_TR_SH([ac_cv_type_$1])" = yes; then
  FP_COMPUTE_INT([offsetof(struct { char c; $1 ty; },ty)],
                 [fp_Cache],
                 [AC_INCLUDES_DEFAULT([$3])],
                 [AC_MSG_ERROR([cannot compute alignment ($1)
See `config.log' for more details.], [77])])
else
  fp_Cache=0
fi])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP(alignment_$1), $fp_Cache, [The alignment of a `$1'.])[]dnl
m4_popdef([fp_Cache])[]dnl
])# FP_CHECK_ALIGNMENT


# FP_LEADING_UNDERSCORE
# ---------------------
# Test for determining whether symbol names have a leading underscore. We assume
# that they _haven't_ if anything goes wrong. Sets the output variable
# LeadingUnderscore to YES or NO and defines LEADING_UNDERSCORE correspondingly.
#
# Some nlist implementations seem to try to be compatible by ignoring a leading
# underscore sometimes (eg. FreeBSD). We therefore have to work around this by
# checking for *no* leading underscore first. Sigh.  --SDM
#
# Similarly on OpenBSD, but this test doesn't help. -- dons
AC_DEFUN([FP_LEADING_UNDERSCORE],
[AC_CHECK_LIB([elf], [nlist], [LIBS="-lelf $LIBS"])
AC_CACHE_CHECK([leading underscore in symbol names], [fptools_cv_leading_underscore], [
# Hack!: nlist() under Digital UNIX insist on there being an _,
# but symbol table listings shows none. What is going on here?!?
#
# Another hack: cygwin doesn't come with nlist.h , so we hardwire
# the underscoredness of that "platform"
case $HostPlatform in
*openbsd*) # x86 openbsd is ELF from 3.4 >, meaning no leading uscore
  case $build in
    i386-*2\.@<:@0-9@:>@ | i386-*3\.@<:@0-3@:>@ ) fptools_cv_leading_underscore=yes ;;
    *) fptools_cv_leading_underscore=no ;;
  esac ;;
alpha-dec-osf*) fptools_cv_leading_underscore=no;;
*cygwin32) fptools_cv_leading_underscore=yes;;
i386-unknown-mingw32) fptools_cv_leading_underscore=yes;;
x86_64-unknown-mingw32) fptools_cv_leading_underscore=no;;

    # HACK: Apple doesn't seem to provide nlist in the 64-bit-libraries
x86_64-apple-darwin*) fptools_cv_leading_underscore=yes;;
*-apple-ios) fptools_cv_leading_underscore=yes;;

*) AC_RUN_IFELSE([AC_LANG_SOURCE([[#ifdef HAVE_NLIST_H
#include <nlist.h>
struct nlist xYzzY1[] = {{"xYzzY1", 0},{0}};
struct nlist xYzzY2[] = {{"_xYzzY2", 0},{0}};
#endif

int main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY1) == 0 && xYzzY1[0].n_value != 0)
        exit(1);
    if(nlist(argv[0], xYzzY2) == 0 && xYzzY2[0].n_value != 0)
        exit(0);
#endif
    exit(1);
}]])],[fptools_cv_leading_underscore=yes],[fptools_cv_leading_underscore=no],[fptools_cv_leading_underscore=no])
;;
esac]);
AC_SUBST([LeadingUnderscore], [`echo $fptools_cv_leading_underscore | sed 'y/yesno/YESNO/'`])
if test x"$fptools_cv_leading_underscore" = xyes; then
   AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.])
fi])# FP_LEADING_UNDERSCORE


# FP_COMPARE_VERSIONS(VERSION1, TEST, VERSION2, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ----------------------------------------------------------------------------------
# Compare dotted version numbers VERSION1 and VERSION2 lexicographically according
# to TEST (one of -eq, -ne, -lt, -le, -gt, or -ge).
AC_DEFUN([FP_COMPARE_VERSIONS],
[fp_version1=$1; fp_version2=$3
fp_save_IFS=$IFS; IFS='.'
while test x"$fp_version1" != x || test x"$fp_version2" != x
do

  set dummy $fp_version1; shift
  fp_num1=""
  test $[@%:@] = 0 || { fp_num1="[$]1"; shift; }
  test x"$fp_num1" = x && fp_num1="0"
  fp_version1="[$]*"

  set dummy $fp_version2; shift
  fp_num2=""
  test $[@%:@] = 0 || { fp_num2="[$]1"; shift; }
  test x"$fp_num2" = x && fp_num2="0"
  fp_version2="[$]*"

  test "$fp_num1" = "$fp_num2" || break;
done
IFS=$fp_save_IFS
AS_IF([test "$fp_num1" $2 "$fp_num2"], [$4], [$5])[]dnl
])# FP_COMPARE_VERSIONS


dnl
dnl Check for Happy and version.  If we're building GHC, then we need
dnl at least Happy version 1.14.  If there's no installed Happy, we look
dnl for a happy source tree and point the build system at that instead.
dnl
AC_DEFUN([FPTOOLS_HAPPY],
[FP_PATH_PROG(HappyCmd,happy,)

AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
changequote(, )dnl
[if test x"$HappyCmd" != x; then
   fptools_cv_happy_version=`"$HappyCmd" -v |
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'` ;
else
   fptools_cv_happy_version="";
fi;
changequote([, ])dnl
])
if test ! -f compiler/parser/Parser.hs || test ! -f compiler/cmm/CmmParse.hs || test ! -f compiler/parser/ParserCore.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.16],
      [AC_MSG_ERROR([Happy version 1.16 or later is required to compile GHC.])])[]
fi
HappyVersion=$fptools_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl Check for Alex and version.  If we're building GHC, then we need
dnl at least Alex version 2.1.1.
dnl
AC_DEFUN([FPTOOLS_ALEX],
[
FP_PATH_PROG(AlexCmd,alex,)

AC_CACHE_CHECK([for version of alex], fptools_cv_alex_version,
changequote(, )dnl
[if test x"$AlexCmd" != x; then
   fptools_cv_alex_version=`"$AlexCmd" -v |
			  grep 'Alex [Vv]ersion' | sed -e 's/Alex [Vv]ersion \([0-9\.]*\).*/\1/g'` ;
else
   fptools_cv_alex_version="";
fi;
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-ge],[3.0],
  [Alex3=YES],[Alex3=NO])
if test ! -f compiler/cmm/CmmLex.hs || test ! -f compiler/parser/Lexer.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[2.1.0],
      [AC_MSG_ERROR([Alex version 2.1.0 or later is required to compile GHC.])])[]
fi
if test ! -f utils/haddock/src/Haddock/Lex.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[3.0],
      [AC_MSG_ERROR([Alex version 3.0 or later is required to compile Haddock.])])[]
fi
AlexVersion=$fptools_cv_alex_version;
AC_SUBST(AlexVersion)
AC_SUBST(Alex3)
])


# FP_PROG_LD_FLAG
# ---------------
# Sets the output variable $2 to $1 if ld supports the $1 flag.
# Otherwise the variable's value is empty.
AC_DEFUN([FP_PROG_LD_FLAG],
[
AC_CACHE_CHECK([whether ld understands $1], [fp_cv_$2],
[echo 'int foo() { return 0; }' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r $1 -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_$2=$1
else
   fp_cv_$2=
fi
rm -rf conftest*])
$2=$fp_cv_$2
])# FP_PROG_LD_FLAG


# FP_PROG_LD_BUILD_ID
# ------------

# Sets the output variable LdHasBuildId to YES if ld supports
# --build-id, or NO otherwise.
AC_DEFUN([FP_PROG_LD_BUILD_ID],
[
AC_CACHE_CHECK([whether ld understands --build-id], [fp_cv_ld_build_id],
[echo 'int foo() { return 0; }' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r --build-id=none -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_ld_build_id=yes
else
   fp_cv_ld_build_id=no
fi
rm -rf conftest*])
if test "$fp_cv_ld_build_id" = yes; then
  LdHasBuildId=YES
else
  LdHasBuildId=NO
fi
AC_SUBST([LdHasBuildId])
])# FP_PROG_LD_BUILD_ID


# FP_PROG_LD_IS_GNU
# -----------------
# Sets the output variable LdIsGNULd to YES or NO, depending on whether it is
# GNU ld or not.
AC_DEFUN([FP_PROG_LD_IS_GNU],
[
AC_CACHE_CHECK([whether ld is GNU ld], [fp_cv_gnu_ld],
[if ${LdCmd} --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_gnu_ld=yes
else
  fp_cv_gnu_ld=no
fi])
AC_SUBST([LdIsGNULd], [`echo $fp_cv_gnu_ld | sed 'y/yesno/YESNO/'`])
])# FP_PROG_LD_IS_GNU


# FP_PROG_LD_NO_COMPACT_UNWIND
# ----------------------------

# Sets the output variable LdHasNoCompactUnwind to YES if ld supports
# -no_compact_unwind, or NO otherwise.
AC_DEFUN([FP_PROG_LD_NO_COMPACT_UNWIND],
[
AC_CACHE_CHECK([whether ld understands -no_compact_unwind], [fp_cv_ld_no_compact_unwind],
[echo 'int foo() { return 0; }' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r -no_compact_unwind -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_ld_no_compact_unwind=yes
else
   fp_cv_ld_no_compact_unwind=no
fi
rm -rf conftest*])
if test "$fp_cv_ld_no_compact_unwind" = yes; then
  LdHasNoCompactUnwind=YES
else
  LdHasNoCompactUnwind=NO
fi
AC_SUBST([LdHasNoCompactUnwind])
])# FP_PROG_LD_NO_COMPACT_UNWIND


# FP_PROG_LD_FILELIST
# -------------------

# Sets the output variable LdHasFilelist to YES if ld supports
# -filelist, or NO otherwise.
AC_DEFUN([FP_PROG_LD_FILELIST],
[
AC_CACHE_CHECK([whether ld understands -filelist], [fp_cv_ld_has_filelist],
[
    echo 'int foo() { return 0; }' > conftest1.c
    echo 'int bar() { return 0; }' > conftest2.c
    ${CC-cc} -c conftest1.c
    ${CC-cc} -c conftest2.c
    echo conftest1.o  > conftest.o-files
    echo conftest2.o >> conftest.o-files
    if ${LdCmd} -r -filelist conftest.o-files -o conftest.o > /dev/null 2>&1
    then
        fp_cv_ld_has_filelist=yes
    else
        fp_cv_ld_has_filelist=no
    fi
    rm -rf conftest*
])
if test "$fp_cv_ld_has_filelist" = yes; then
    LdHasFilelist=YES
else
    LdHasFilelist=NO
fi
AC_SUBST([LdHasFilelist])
])# FP_PROG_LD_FILELIST


# FP_PROG_AR
# ----------
# Sets fp_prog_ar to a (non-Cygwin) path to ar. Exits if no ar can be found
AC_DEFUN([FP_PROG_AR],
[FP_PATH_PROG([fp_prog_ar], [ar])
if test -z "$fp_prog_ar"; then
  AC_MSG_ERROR([cannot find ar in your PATH, no idea how to make a library])
fi
])# FP_PROG_AR


# FP_PROG_AR_IS_GNU
# -----------------
# Sets fp_prog_ar_is_gnu to yes or no, depending on whether it is GNU ar or not.
AC_DEFUN([FP_PROG_AR_IS_GNU],
[AC_REQUIRE([FP_PROG_AR])
AC_CACHE_CHECK([whether $fp_prog_ar is GNU ar], [fp_cv_prog_ar_is_gnu],
[if "$fp_prog_ar" --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_prog_ar_is_gnu=yes
else
  fp_cv_prog_ar_is_gnu=no
fi])
fp_prog_ar_is_gnu=$fp_cv_prog_ar_is_gnu
AC_SUBST([ArIsGNUAr], [`echo $fp_prog_ar_is_gnu | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_IS_GNU


# FP_PROG_AR_SUPPORTS_ATFILE
# -----------------
# Sets fp_prog_ar_supports_atfile to yes or no, depending on whether
# or not it supports the @file syntax
AC_DEFUN([FP_PROG_AR_SUPPORTS_ATFILE],
[AC_REQUIRE([FP_PROG_AR])
 AC_REQUIRE([FP_PROG_AR_ARGS])
AC_CACHE_CHECK([whether $fp_prog_ar supports @file], [fp_cv_prog_ar_supports_atfile],
[
rm -f conftest*
touch conftest.file
echo conftest.file  > conftest.atfile
echo conftest.file >> conftest.atfile
"$fp_prog_ar" $fp_prog_ar_args conftest.a @conftest.atfile > /dev/null 2>&1
fp_prog_ar_supports_atfile_tmp=`"$fp_prog_ar" t conftest.a 2> /dev/null | grep -c conftest.file`
rm -f conftest*
if test "$fp_prog_ar_supports_atfile_tmp" -eq 2
then
  fp_cv_prog_ar_supports_atfile=yes
else
  fp_cv_prog_ar_supports_atfile=no
fi])
fp_prog_ar_supports_atfile=$fp_cv_prog_ar_supports_atfile
AC_SUBST([ArSupportsAtFile], [`echo $fp_prog_ar_supports_atfile | tr 'a-z' 'A-Z'`])
])# FP_PROG_AR_SUPPORTS_ATFILE

# FP_PROG_AR_ARGS
# ---------------
# Sets fp_prog_ar_args to the arguments for ar and the output variable ArCmd
# to a non-Cygwin invocation of ar including these arguments.
AC_DEFUN([FP_PROG_AR_ARGS],
[AC_REQUIRE([FP_PROG_AR_IS_GNU])
AC_CACHE_CHECK([for ar arguments], [fp_cv_prog_ar_args],
[
# GNU ar needs special treatment: it appears to have problems with
# object files with the same name if you use the 's' modifier, but
# simple 'ar q' works fine, and doesn't need a separate ranlib.
if test $fp_prog_ar_is_gnu = yes; then
  fp_cv_prog_ar_args="q"
else
  touch conftest.dummy
  for fp_var in clqsZ clqs cqs clq cq ; do
     rm -f conftest.a
     if "$fp_prog_ar" $fp_var conftest.a conftest.dummy > /dev/null 2> /dev/null; then
        fp_cv_prog_ar_args=$fp_var
        break
     fi
  done
  rm -f conftest*
  if test -z "$fp_cv_prog_ar_args"; then
    AC_MSG_ERROR([cannot figure out how to use your $fp_prog_ar])
  fi
fi])
fp_prog_ar_args=$fp_cv_prog_ar_args
AC_SUBST([ArCmd], ["$fp_prog_ar"])
AC_SUBST([ArArgs], ["$fp_prog_ar_args"])

])# FP_PROG_AR_ARGS


# FP_PROG_AR_NEEDS_RANLIB
# -----------------------
# Sets the output variable RANLIB_CMD to "ranlib" if it is needed and
# found, to "true" otherwise. Sets REAL_RANLIB_CMD to the ranlib program,
# even if we don't need ranlib (libffi might still need it).
AC_DEFUN([FP_PROG_AR_NEEDS_RANLIB],[
    AC_REQUIRE([FP_PROG_AR_IS_GNU])
    AC_REQUIRE([FP_PROG_AR_ARGS])
    AC_REQUIRE([AC_PROG_CC])

    AC_PROG_RANLIB

    if test $fp_prog_ar_is_gnu = yes
    then
        fp_cv_prog_ar_needs_ranlib=no
    elif test "$TargetOS_CPP" = "darwin"
    then
        # It's quite tedious to check for Apple's crazy timestamps in
        # .a files, so we hardcode it.
        fp_cv_prog_ar_needs_ranlib=yes
    else
        case $fp_prog_ar_args in
        *s*)
            fp_cv_prog_ar_needs_ranlib=no;;
        *)
            fp_cv_prog_ar_needs_ranlib=yes;;
        esac
    fi

    REAL_RANLIB_CMD="$RANLIB"
    if test $fp_cv_prog_ar_needs_ranlib = yes
    then
        RANLIB_CMD="$RANLIB"
    else
        RANLIB_CMD="true"
    fi
    AC_SUBST([REAL_RANLIB_CMD])
    AC_SUBST([RANLIB_CMD])
])# FP_PROG_AR_NEEDS_RANLIB


# FP_GCC_VERSION
# -----------
# Extra testing of the result AC_PROG_CC, testing the gcc version no. Sets the
# output variable GccVersion.
AC_DEFUN([FP_GCC_VERSION],
[AC_REQUIRE([AC_PROG_CC])
if test -z "$CC"
then
  AC_MSG_ERROR([gcc is required])
fi
GccLT34=NO
GccLT46=NO
AC_CACHE_CHECK([version of gcc], [fp_cv_gcc_version],
[
    fp_cv_gcc_version="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [[^0-9]]*\([[0-9.]]*\).*/\1/g'`"
    FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [3.0],
                        [AC_MSG_ERROR([Need at least gcc version 3.0 (3.4+ recommended)])])
    # See #2770: gcc 2.95 doesn't work any more, apparently.  There probably
    # isn't a very good reason for that, but for now just make configure
    # fail.
    FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [3.4], GccLT34=YES)
    FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [4.6], GccLT46=YES)
])
AC_SUBST([GccVersion], [$fp_cv_gcc_version])
AC_SUBST(GccLT34)
AC_SUBST(GccLT46)
])# FP_GCC_VERSION

dnl Check to see if the C compiler is clang or llvm-gcc
dnl
AC_DEFUN([FP_CC_LLVM_BACKEND],
[AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING([whether C compiler is clang])
$CC -x c /dev/null -dM -E > conftest.txt 2>&1
if grep "__clang__" conftest.txt >/dev/null 2>&1; then
  AC_SUBST([CC_CLANG_BACKEND], [1])
  AC_SUBST([CC_LLVM_BACKEND], [1])
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
  AC_MSG_CHECKING([whether C compiler has an LLVM back end])
  if grep "__llvm__" conftest.txt >/dev/null 2>&1; then
    AC_SUBST([CC_CLANG_BACKEND], [0])
    AC_SUBST([CC_LLVM_BACKEND], [1])
    AC_MSG_RESULT([yes])
  else
    AC_SUBST([CC_CLANG_BACKEND], [0])
    AC_SUBST([CC_LLVM_BACKEND], [0])
    AC_MSG_RESULT([no])
  fi
fi

rm -f conftest.txt
])

dnl Small feature test for perl version. Assumes PerlCmd
dnl contains path to perl binary.
dnl
dnl (Perl versions prior to v5.6 does not contain the string "v5";
dnl instead they display version strings such as "version 5.005".)
dnl
AC_DEFUN([FPTOOLS_CHECK_PERL_VERSION],
[$PerlCmd -v >conftest.out 2>&1
   if grep "v5" conftest.out >/dev/null 2>&1; then
      :
   else
      AC_MSG_ERROR([your version of perl probably won't work, try upgrading it.])
   fi
rm -fr conftest*
])


# FP_CHECK_PROG(VARIABLE, PROG-TO-CHECK-FOR,
#               [VALUE-IF-NOT-FOUND], [PATH], [REJECT])
# -----------------------------------------------------
# HACK: A small wrapper around AC_CHECK_PROG, setting VARIABLE to the full path
# of PROG-TO-CHECK-FOR when found.
AC_DEFUN([FP_CHECK_PROG],
[AC_CHECK_PROG([$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [$3], [$4], [$5])][]dnl
)# FP_CHECK_PROC


# FP_PROG_FIND
# ------------
# Find a non-WinDoze version of the "find" utility.
AC_DEFUN([FP_PROG_FIND],
[AC_PATH_PROGS([fp_prog_find], [gfind find], find)
echo foo > conftest.txt
$fp_prog_find conftest.txt -print > conftest.out 2>&1
if grep '^conftest.txt$' conftest.out > /dev/null 2>&1 ; then
  # OK, looks like a real "find".
  case $HostPlatform in
    *mingw32)
      if test x${OSTYPE} != xmsys
      then
 	    fp_prog_find="`cygpath --mixed ${fp_prog_find}`"
        AC_MSG_NOTICE([normalized find command to $fp_prog_find])
      fi ;;
    *) ;;
  esac
  FindCmd="$fp_prog_find"
else
  # Found a poor WinDoze version of "find", ignore it.
  AC_MSG_WARN([$fp_prog_find looks like a non-*nix find, ignoring it])
  FP_CHECK_PROG([FindCmd], [find], [], [], [$fp_prog_find])
fi
rm -f conftest.txt conftest.out
AC_SUBST([FindCmd])[]dnl
])# FP_PROG_FIND


# FP_PROG_SORT
# ------------
# Find a Unix-like sort
AC_DEFUN([FP_PROG_SORT],
[AC_PATH_PROG([fp_prog_sort], [sort])
echo conwip > conftest.txt
$fp_prog_sort -f conftest.txt > conftest.out 2>&1
if grep 'conwip' conftest.out > /dev/null 2>&1 ; then
  # The goods
  SortCmd="$fp_prog_sort"
else
  # Summink else..pick next one.
  AC_MSG_WARN([$fp_prog_sort looks like a non-*nix sort, ignoring it])
  FP_CHECK_PROG([SortCmd], [sort], [], [], [$fp_prog_sort])
fi
rm -f conftest.txt conftest.out
AC_SUBST([SortCmd])[]dnl
])# FP_PROG_SORT


dnl
dnl FPTOOLS_NOCACHE_CHECK prints a message, then sets the
dnl values of the second argument to the result of running
dnl the commands given by the third. It does not cache its
dnl result, so it is suitable for checks which should be
dnl run every time.
dnl
AC_DEFUN([FPTOOLS_NOCACHE_CHECK],
[AC_MSG_CHECKING([$1])
 $3
 AC_MSG_RESULT([$][$2])
])

dnl
dnl FPTOOLS_GHC_VERSION(version)
dnl FPTOOLS_GHC_VERSION(major, minor [, patchlevel])
dnl FPTOOLS_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
AC_DEFUN([FPTOOLS_GHC_VERSION],
[FPTOOLS_NOCACHE_CHECK([version of ghc], [fptools_version_of_ghc],
["${WithGhc-ghc}" --version > conftestghc 2>&1
  cat conftestghc >&AS_MESSAGE_LOG_FD
#Useless Use Of cat award...
  fptools_version_of_ghc=`cat conftestghc | sed -n -e 's/, patchlevel *\([[0-9]]\)/.\1/;s/.* version \([[0-9]][[0-9.]]*\).*/\1/p'`
  rm -fr conftest*
  if test "[$]fptools_version_of_ghc" = ""
  then
    fptools_version_of_ghc='unknown'
  fi
fptools_version_of_ghc[_major]=`echo [$]fptools_version_of_ghc | sed -e 's/^\([[0-9]]\).*/\1/'`
fptools_version_of_ghc[_minor]=`echo [$]fptools_version_of_ghc | sed -e 's/^[[0-9]]\.\([[0-9]]*\).*/\1/'`
fptools_version_of_ghc[_pl]=`echo [$]fptools_version_of_ghc | sed -n -e 's/^[[0-9]]\.[[0-9]]*\.\([[0-9]]*\)/\1/p'`
#
if test "[$]fptools_version_of_ghc[_pl]" = ""
then
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor]"
  fptools_version_of_ghc[_pl]="0"
else
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor].[$]fptools_version_of_ghc[_pl]"
fi
#
ifelse($#, [1], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
], $#, [2], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
], $#, [3], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
[$3]="[$]fptools_version_of_ghc[_pl]"
], $#, [4], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
[$2]="[$]fptools_version_of_ghc[_major]"
[$3]="[$]fptools_version_of_ghc[_minor]"
[$4]="[$]fptools_version_of_ghc[_pl]"
])
])
])dnl


# FP_CHECK_FUNC(FUNCTION, PROLOGUE, BODY, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# ---------------------------------------------------------------------------------
# A variant of AC_CHECK_FUNCS, limited to a single FUNCTION, but with the
# additional flexibility of specifying the PROLOGUE and BODY.
AC_DEFUN([FP_CHECK_FUNC],
[AS_VAR_PUSHDEF([fp_func], [fp_cv_func_$1])dnl
AC_CACHE_CHECK([for $1], fp_func,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
                [AS_VAR_SET(fp_func, yes)],
                [AS_VAR_SET(fp_func, no)])])
AS_IF([test AS_VAR_GET(fp_func) = yes],
      [AC_DEFINE(AS_TR_CPP(HAVE_$1), [1],
                [Define to 1 if you have the `]$1[' function.]) $4],
      [$5])dnl
AS_VAR_POPDEF([fp_func])dnl
])# FP_CHECK_FUNC


# FP_GEN_DOCBOOK_XML
# ------------------
# Generates a DocBook XML V4.5 document in conftest.xml.
#
# It took a lot of experimentation to find a document that will cause
# xsltproc to fail with an error code when the relevant
# stylesheets/DTDs are not found.  I couldn't make xsltproc fail with
# a single-file document, it seems a multi-file document is needed.
# -- SDM 2009-06-03
#
AC_DEFUN([FP_GEN_DOCBOOK_XML],
[rm -f conftest.xml conftest-book.xml
cat > conftest.xml << EOF
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
   "http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd" [[
<!ENTITY conftest-book SYSTEM "conftest-book.xml">
]]>
<book id="test">
&conftest-book;
</book>
EOF
cat >conftest-book.xml << EOF
<?xml version="1.0" encoding="iso-8859-1"?>
  <title>A DocBook &ldquo;Test Document&rdquo;</title>
  <chapter id="id-one">
    <title>A Chapter Title</title>
    <para>This is a paragraph, referencing <xref linkend="id-two"/>.</para>
  </chapter>
  <chapter id="id-two">
    <title>Another Chapter Title</title>
    <para>This is another paragraph, referencing <xref linkend="id-one"/>.</para>
  </chapter>
EOF
]) # FP_GEN_DOCBOOK_XML


# FP_PROG_DBLATEX
# ----------------
# Sets the output variable DblatexCmd to the full path of dblatex,
# which we use for building PDF and PS docs.
# DblatexCmd is empty if dblatex could not be found.
AC_DEFUN([FP_PROG_DBLATEX],
[FP_PATH_PROG([DblatexCmd], [dblatex])
if test -z "$DblatexCmd"; then
  AC_MSG_WARN([cannot find dblatex in your PATH, you will not be able to build the PDF and PS documentation])
fi
])# FP_PROG_DBLATEX


# FP_PROG_XSLTPROC
# ----------------
# Sets the output variable XsltprocCmd to the full path of the XSLT processor
# xsltproc. XsltprocCmd is empty if xsltproc could not be found.
AC_DEFUN([FP_PROG_XSLTPROC],
[FP_PATH_PROG([XsltprocCmd], [xsltproc])
if test -z "$XsltprocCmd"; then
  AC_MSG_WARN([cannot find xsltproc in your PATH, you will not be able to build the HTML documentation])
fi
])# FP_PROG_XSLTPROC


# FP_DOCBOOK_XSL
# ----------------------------
# Check that we can process a DocBook XML document to HTML using xsltproc.
AC_DEFUN([FP_DOCBOOK_XSL],
[AC_REQUIRE([FP_PROG_XSLTPROC])dnl
if test -n "$XsltprocCmd"; then
  AC_CACHE_CHECK([for DocBook XSL stylesheet], fp_cv_dir_docbook_xsl,
  [FP_GEN_DOCBOOK_XML
  fp_cv_dir_docbook_xsl=no
  if $XsltprocCmd --nonet http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl conftest.xml > /dev/null 2>&1; then
     fp_cv_dir_docbook_xsl=yes
  fi
  rm -rf conftest*])
fi
if test x"$fp_cv_dir_docbook_xsl" = xno; then
  AC_MSG_WARN([cannot find DocBook XSL stylesheets, you will not be able to build the documentation])
  HAVE_DOCBOOK_XSL=NO
else
  HAVE_DOCBOOK_XSL=YES
fi
AC_SUBST([HAVE_DOCBOOK_XSL])
])# FP_DOCBOOK_XSL


# FP_PROG_XMLLINT
# ----------------
# Sets the output variable XmllintCmd to the full path of the XSLT processor
# xmllint. XmllintCmd is empty if xmllint could not be found.
AC_DEFUN([FP_PROG_XMLLINT],
[FP_PATH_PROG([XmllintCmd], [xmllint])
if test -z "$XmllintCmd"; then
  AC_MSG_WARN([cannot find xmllint in your PATH, you will not be able to validate your documentation])
fi
])# FP_PROG_XMLLINT


# FP_CHECK_DOCBOOK_DTD
# --------------------
AC_DEFUN([FP_CHECK_DOCBOOK_DTD],
[AC_REQUIRE([FP_PROG_XMLLINT])dnl
if test -n "$XmllintCmd"; then
  AC_MSG_CHECKING([for DocBook DTD])
  FP_GEN_DOCBOOK_XML
  if $XmllintCmd --nonet --valid --noout conftest.xml ; then
    AC_MSG_RESULT([ok])
  else
    AC_MSG_RESULT([failed])
    AC_MSG_WARN([cannot find a DTD for DocBook XML V4.5, you will not be able to validate your documentation])
    AC_MSG_WARN([check your XML_CATALOG_FILES environment variable and/or /etc/xml/catalog])
  fi
  rm -rf conftest*
fi
])# FP_CHECK_DOCBOOK_DTD


# FP_PROG_GHC_PKG
# ----------------
# Try to find a ghc-pkg matching the ghc mentioned in the environment variable
# WithGhc. Sets the output variable GhcPkgCmd.
AC_DEFUN([FP_PROG_GHC_PKG],
[AC_CACHE_CHECK([for ghc-pkg matching $WithGhc], fp_cv_matching_ghc_pkg,
[
# If we are told to use ghc-stage2, then we're using an in-tree
# compiler. In this case, we just want ghc-pkg, not ghc-pkg-stage2,
# so we sed off -stage[0-9]$. However, if we are told to use
# ghc-6.12.1 then we want to use ghc-pkg-6.12.1, so we keep any
# other suffix.
fp_ghc_pkg_guess=`echo $WithGhc | sed -e 's/-stage@<:@0-9@:>@$//' -e 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$fp_ghc_pkg_guess" list > /dev/null 2>&1; then
  fp_cv_matching_ghc_pkg=$fp_ghc_pkg_guess
else
  AC_MSG_ERROR([Cannot find matching ghc-pkg])
fi])
GhcPkgCmd=$fp_cv_matching_ghc_pkg
AC_SUBST([GhcPkgCmd])
])# FP_PROG_GHC_PKG


# FP_GCC_EXTRA_FLAGS
# ------------------
# Determine which extra flags we need to pass gcc when we invoke it
# to compile .hc code.
#
# -fwrapv is needed for gcc to emit well-behaved code in the presence of
# integer wrap around. (Trac #952)
#
AC_DEFUN([FP_GCC_EXTRA_FLAGS],
[AC_REQUIRE([FP_GCC_VERSION])
AC_CACHE_CHECK([for extra options to pass gcc when compiling via C], [fp_cv_gcc_extra_opts],
[fp_cv_gcc_extra_opts=
 FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-ge], [3.4],
  [fp_cv_gcc_extra_opts="$fp_cv_gcc_extra_opts -fwrapv"],
  [])
])
AC_SUBST([GccExtraViaCOpts],$fp_cv_gcc_extra_opts)
])


# FP_SETUP_PROJECT_VERSION
# ---------------------
AC_DEFUN([FP_SETUP_PROJECT_VERSION],
[
if test "$RELEASE" = "NO"; then
    AC_MSG_CHECKING([for GHC version date])
    if test -f VERSION_DATE; then
        PACKAGE_VERSION=${PACKAGE_VERSION}.`cat VERSION_DATE`
        AC_MSG_RESULT(given $PACKAGE_VERSION)
    elif test -d .git; then
        changequote(, )dnl
        ver_date=`git log -n 1 --date=short --pretty=format:%ci | cut -d ' ' -f 1 | tr -d -`
        if echo $ver_date | grep '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$' 2>&1 >/dev/null; then true; else
        changequote([, ])dnl
                AC_MSG_ERROR([failed to detect version date: check that git is in your path])
        fi
        PACKAGE_VERSION=${PACKAGE_VERSION}.$ver_date
        AC_MSG_RESULT(inferred $PACKAGE_VERSION)
    elif test -f VERSION; then
        PACKAGE_VERSION=`cat VERSION`
        AC_MSG_RESULT(given $PACKAGE_VERSION)
    else
        AC_MSG_WARN([cannot determine snapshot version: no .git directory and no VERSION file])
        dnl We'd really rather this case didn't happen, but it might
        dnl do (in particular, people using lndir trees may find that
        dnl the build system can't find any other date). If it does
        dnl happen, then we use the current date.
        dnl This way we get some idea about how recent a build is.
        dnl It also means that packages built for 2 different builds
        dnl will probably use different version numbers, so things are
        dnl less likely to go wrong.
        PACKAGE_VERSION=${PACKAGE_VERSION}.`date +%Y%m%d`
    fi
fi

# Some renamings
AC_SUBST([ProjectName], [$PACKAGE_NAME])
AC_SUBST([ProjectVersion], [$PACKAGE_VERSION])

# Split PACKAGE_VERSION into (possibly empty) parts
VERSION_MAJOR=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
VERSION_TMP=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`
VERSION_MINOR=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
ProjectPatchLevel=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`

# Calculate project version as an integer, using 2 digits for minor version
case $VERSION_MINOR in
  ?) ProjectVersionInt=${VERSION_MAJOR}0${VERSION_MINOR} ;;
  ??) ProjectVersionInt=${VERSION_MAJOR}${VERSION_MINOR} ;;
  *) AC_MSG_ERROR([bad minor version in $PACKAGE_VERSION]) ;;
esac
AC_SUBST([ProjectVersionInt])

# The project patchlevel is zero unless stated otherwise
test -z "$ProjectPatchLevel" && ProjectPatchLevel=0

# Remove dots from the patch level; this allows us to have versions like 6.4.1.20050508
ProjectPatchLevel=`echo $ProjectPatchLevel | sed 's/\.//'`

AC_SUBST([ProjectPatchLevel])
])# FP_SETUP_PROJECT_VERSION


# Check for a working timer_create().  We need a pretty detailed check
# here, because there exist partially-working implementations of
# timer_create() in certain versions of Linux (see bug #1933).
#
AC_DEFUN([FP_CHECK_TIMER_CREATE],[
AC_CHECK_FUNC([timer_create],[HAVE_timer_create=yes],[HAVE_timer_create=no])

if test "$HAVE_timer_create" = "yes"
then
  if test "$cross_compiling" = "yes"
  then
    # We can't test timer_create when we're cross-compiling, so we
    # optimistiaclly assume that it actually works properly.
    AC_DEFINE([USE_TIMER_CREATE], 1,  [Define to 1 if we can use timer_create(CLOCK_PROCESS_CPUTIME_ID,...)])
  else
  AC_CACHE_CHECK([for a working timer_create(CLOCK_REALTIME)],
    [fptools_cv_timer_create_works],
    [AC_TRY_RUN([
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static volatile int tock = 0;
static void handler(int i)
{
   tock = 1;
}

static void timeout(int i)
{
  // timer_settime() has been known to hang, so just in case
  // we install a 1-second timeout (see #2257)
  exit(99);
}

int main(int argc, char *argv[])
{

    struct sigevent ev;
    timer_t timer;
    struct itimerspec it;
    struct sigaction action;
    int m,n,count = 0;

    ev.sigev_notify = SIGEV_SIGNAL;
    ev.sigev_signo  = SIGVTALRM;

    action.sa_handler = handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGVTALRM, &action, NULL) == -1) {
        fprintf(stderr,"SIGVTALRM problem\n");
        exit(3);
    }

    action.sa_handler = timeout;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGALRM, &action, NULL) == -1) {
      fprintf(stderr,"SIGALRM problem\n");
      exit(3);
    }
    alarm(1);

    if (timer_create(CLOCK_PROCESS_CPUTIME_ID, &ev, &timer) != 0) {
        fprintf(stderr,"No CLOCK_PROCESS_CPUTIME_ID timer\n");
       exit(1);
    }

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 1;
    it.it_interval = it.it_value;
    if (timer_settime(timer, 0, &it, NULL) != 0) {
        fprintf(stderr,"settime problem\n");
        exit(4);
    }

    tock = 0;

    for(n = 3; n < 20000; n++){
        for(m = 2; m <= n/2; m++){
            if (!(n%m)) count++;
            if (tock) goto out;
        }
    }
out:

    if (!tock) {
        fprintf(stderr,"no CLOCK_PROCESS_CPUTIME_ID signal\n");
        exit(5);
    }

    timer_delete(timer);

    if (timer_create(CLOCK_REALTIME, &ev, &timer) != 0) {
        fprintf(stderr,"No CLOCK_REALTIME timer\n");
        exit(2);
    }

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 1000000;
    it.it_interval = it.it_value;
    if (timer_settime(timer, 0, &it, NULL) != 0) {
        fprintf(stderr,"settime problem\n");
        exit(4);
    }

    tock = 0;

    usleep(3000);

    if (!tock) {
        fprintf(stderr,"no CLOCK_REALTIME signal\n");
        exit(5);
    }

    timer_delete(timer);

    exit(0);
}
     ],
     [fptools_cv_timer_create_works=yes],
     [fptools_cv_timer_create_works=no])
  ])
case $fptools_cv_timer_create_works in
    yes) AC_DEFINE([USE_TIMER_CREATE], 1,
                   [Define to 1 if we can use timer_create(CLOCK_PROCESS_CPUTIME_ID,...)]);;
esac
  fi
fi
])

# FP_ICONV
# -------------
AC_DEFUN([FP_ICONV],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us iconv is somewhere odd
  dnl--------------------------------------------------------------------

  dnl Note: ICONV_LIB_DIRS and ICONV_INCLUDE_DIRS are not predefined
  dnl to the empty string to allow them to be overridden from the
  dnl environment.

  AC_ARG_WITH([iconv-includes],
    [AC_HELP_STRING([--with-iconv-includes],
      [directory containing iconv.h])],
      [ICONV_INCLUDE_DIRS=$withval])

  AC_ARG_WITH([iconv-libraries],
    [AC_HELP_STRING([--with-iconv-libraries],
      [directory containing iconv library])],
      [ICONV_LIB_DIRS=$withval])

  AC_SUBST(ICONV_INCLUDE_DIRS)
  AC_SUBST(ICONV_LIB_DIRS)
])# FP_ICONV

# FP_GMP
# -------------
AC_DEFUN([FP_GMP],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us gmp is somewhere odd
  dnl--------------------------------------------------------------------

  AC_ARG_WITH([gmp-includes],
    [AC_HELP_STRING([--with-gmp-includes],
      [directory containing gmp.h])],
      [GMP_INCLUDE_DIRS=$withval])

  AC_ARG_WITH([gmp-libraries],
    [AC_HELP_STRING([--with-gmp-libraries],
      [directory containing gmp library])],
      [GMP_LIB_DIRS=$withval])

  AC_SUBST(GMP_INCLUDE_DIRS)
  AC_SUBST(GMP_LIB_DIRS)
])# FP_GMP

# --------------------------------------------------------------
# Calculate absolute path to build tree
# --------------------------------------------------------------

AC_DEFUN([FP_INTREE_GHC_PWD],[
AC_MSG_NOTICE(Building in-tree ghc-pwd)
    dnl This would be
    dnl     make -C utils/ghc-pwd clean && make -C utils/ghc-pwd
    dnl except we don't want to have to know what make is called. Sigh.
    rm -rf utils/ghc-pwd/dist-boot
    mkdir  utils/ghc-pwd/dist-boot
    if ! "$WithGhc" -v0 -no-user-$GHC_PACKAGE_DB_FLAG -hidir utils/ghc-pwd/dist-boot -odir utils/ghc-pwd/dist-boot -stubdir utils/ghc-pwd/dist-boot --make utils/ghc-pwd/Main.hs -o utils/ghc-pwd/dist-boot/ghc-pwd
    then
        AC_MSG_ERROR([Building ghc-pwd failed])
    fi

    GHC_PWD=utils/ghc-pwd/dist-boot/ghc-pwd
])

AC_DEFUN([FP_BINDIST_GHC_PWD],[
    GHC_PWD=utils/ghc-pwd/dist-install/build/tmp/ghc-pwd-bindist
])

AC_DEFUN([FP_FIND_ROOT],[
AC_MSG_CHECKING(for path to top of build tree)
    hardtop=`$GHC_PWD`

    dnl Remove common automounter nonsense
    hardtop=`echo $hardtop | sed 's|^/tmp_mnt.*\(/local/.*\)$|\1|' | sed 's|^/tmp_mnt/|/|'`

    if ! test -d "$hardtop"; then
        AC_MSG_ERROR([cannot determine current directory])
    fi

    dnl We don't support building in directories with spaces.
    case "$hardtop" in
    *' '*)
        AC_MSG_ERROR([
        The build system does not support building in a directory
        containing space characters.
        Suggestion: move the build tree somewhere else.])
        ;;
    esac

    AC_SUBST(hardtop)

    AC_MSG_RESULT($hardtop)
])

# GHC_CONVERT_CPU(cpu, target_var)
# --------------------------------
# converts cpu from gnu to ghc naming, and assigns the result to $target_var
AC_DEFUN([GHC_CONVERT_CPU],[
case "$1" in
  alpha*)
    $2="alpha"
    ;;
  arm*)
    $2="arm"
    ;;
  hppa1.1*)
    $2="hppa1_1"
    ;;
  hppa*)
    $2="hppa"
    ;;
  i386|i486|i586|i686)
    $2="i386"
    ;;
  ia64)
    $2="ia64"
    ;;
  m68k*)
    $2="m68k"
    ;;
  mipseb*)
    $2="mipseb"
    ;;
  mipsel*)
    $2="mipsel"
    ;;
  mips*)
    $2="mips"
    ;;
  powerpc64*)
    $2="powerpc64"
    ;;
  powerpc*)
    $2="powerpc"
    ;;
  rs6000)
    $2="rs6000"
    ;;
  s390x*)
    $2="s390x"
    ;;
  s390*)
    $2="s390"
    ;;
  sparc64*)
    $2="sparc64"
    ;;
  sparc*)
    $2="sparc"
    ;;
  vax)
    $2="vax"
    ;;
  x86_64)
    $2="x86_64"
    ;;
  *)
    echo "Unknown CPU $1"
    exit 1
    ;;
  esac
])

# GHC_CONVERT_VENDOR(vendor, target_var)
# --------------------------------
# converts vendor from gnu to ghc naming, and assigns the result to $target_var
AC_DEFUN([GHC_CONVERT_VENDOR],[
  case "$1" in
  pc|gentoo|w64) # like i686-pc-linux-gnu, i686-gentoo-freebsd8, x86_64-w64-mingw32
    $2="unknown"
    ;;
  softfloat) # like armv5tel-softfloat-linux-gnueabi
    $2="unknown"
    ;;
  *)
    #pass thru by default
    $2="$1"
    ;;
  esac
])

# GHC_CONVERT_OS(os, converted_cpu, target_var)
# --------------------------------
# converts os from gnu to ghc naming, and assigns the result to $target_var
AC_DEFUN([GHC_CONVERT_OS],[
case "$1-$2" in
  darwin10-arm|darwin11-i386)
    $3="ios"
    ;;
  *)
    case "$1" in
      linux-android*)
        $3="linux-android"
        ;;
      linux-*|linux)
        $3="linux"
        ;;
      # As far as I'm aware, none of these have relevant variants
      freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku)
        $3="$1"
        ;;
      freebsd*) # like i686-gentoo-freebsd7
                #      i686-gentoo-freebsd8
                #      i686-gentoo-freebsd8.2
        $3="freebsd"
        ;;
      nto-qnx*)
        $3="nto-qnx"
        ;;
      *)
        echo "Unknown OS $1"
        exit 1
        ;;
      esac
      ;;
  esac
])

# BOOTSTRAPPING_GHC_INFO_FIELD
# --------------------------------
# Set the variable $1 to the value of the ghc --info field $2.
AC_DEFUN([BOOTSTRAPPING_GHC_INFO_FIELD],[
$1=`"$WithGhc" --info | grep "^ ,(\"$2\"," | sed -e 's/.*","//' -e 's/")$//'`
tmp=${$1#\$topdir/}
if test "${$1}" != "$tmp"
then
    topdir=`"$WithGhc" --print-libdir | sed 's#\\\\#/#g'`
    $1="$topdir/$tmp"
fi
AC_SUBST($1)
])

# LIBRARY_VERSION(lib, [dir])
# --------------------------------
# Gets the version number of a library.
# If $1 is ghc-prim, then we define LIBRARY_ghc_prim_VERSION as 1.2.3
# $2 points to the directory under libraries/
AC_DEFUN([LIBRARY_VERSION],[
dir=m4_default([$2],[$1])
LIBRARY_[]translit([$1], [-], [_])[]_VERSION=`grep -i "^version:" libraries/${dir}/$1.cabal | sed "s/.* //"`
AC_SUBST(LIBRARY_[]translit([$1], [-], [_])[]_VERSION)
])

# XCODE_VERSION()
# --------------------------------
# Gets the version number of XCode, if on a Mac
AC_DEFUN([XCODE_VERSION],[
    if test "$TargetOS_CPP" = "darwin"
    then
        AC_MSG_CHECKING(XCode version)
        XCodeVersion=`xcodebuild -version | grep Xcode | sed "s/Xcode //"`
        # Old XCode versions don't actually give the XCode version
        if test "$XCodeVersion" = ""
        then
            AC_MSG_RESULT(not found (too old?))
            XCodeVersion1=0
            XCodeVersion2=0
        else
            AC_MSG_RESULT($XCodeVersion)
            XCodeVersion1=`echo "$XCodeVersion" | sed 's/\..*//'`
            changequote(, )dnl
            XCodeVersion2=`echo "$XCodeVersion" | sed 's/[^.]*\.\([^.]*\).*/\1/'`
            changequote([, ])dnl
            AC_MSG_NOTICE(XCode version component 1: $XCodeVersion1)
            AC_MSG_NOTICE(XCode version component 2: $XCodeVersion2)
        fi
    fi
])

# FIND_LLVM_PROG()
# --------------------------------
# Find where the llvm tools are. We have a special function to handle when they
# are installed with a version suffix (e.g., llc-3.1).
#
# $1 = the variable to set
# $2 = the with option name
# $3 = the command to look for
#
AC_DEFUN([FIND_LLVM_PROG],[
    FP_ARG_WITH_PATH_GNU_PROG_OPTIONAL([$1], [$2], [$3])
    if test "$$1" == ""; then
        save_IFS=$IFS
        IFS=":;"
        for p in ${PATH}; do
            if test -d "${p}"; then
                $1=`${FindCmd} "${p}" -type f -perm +111 -maxdepth 1 -regex '.*/$3-[[0-9]]\.[[0-9]]' -or -type l -perm +111 -maxdepth 1 -regex '.*/$3-[[0-9]]\.[[0-9]]' | ${SortCmd} -n | tail -1`
                if test -n "$$1"; then
                    break
                fi
            fi
        done
        IFS=$save_IFS
    fi
])

# FIND_GCC()
# --------------------------------
# Finds where gcc is
#
# $1 = the variable to set
# $2 = the with option name
# $3 = the command to look for
AC_DEFUN([FIND_GCC],[
    if test "$TargetOS_CPP" = "darwin" &&
       test "$XCodeVersion1" -eq 4 &&
       test "$XCodeVersion2" -lt 2
    then
        # In Xcode 4.1, 'gcc-4.2' is the gcc legacy backend (rather
        # than the LLVM backend). We prefer the legacy gcc, but in
        # Xcode 4.2 'gcc-4.2' was removed.
        FP_ARG_WITH_PATH_GNU_PROG([$1], [gcc-4.2], [gcc-4.2])
    elif test "$windows" = YES
    then
        $1="$CC"
    else
        FP_ARG_WITH_PATH_GNU_PROG([$1], [$2], [$3])
    fi
    AC_SUBST($1)
])

# LocalWords:  fi
