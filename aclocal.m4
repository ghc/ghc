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
    # examples: i386-unknown-mingw32, i686-w64-mingw32, x86_64-w64-mingw32
    *-mingw32)
        windows=YES
        $2='.exe'
        $3='.dll'
        ;;
    # apple platform uses .dylib (macOS, iOS, ...)
    *-apple-*)
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

    GHC_LLVM_TARGET([$target_cpu],[$target_vendor],[$target_os],[LlvmTarget])

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

    # we intend to pass trough --targets to llvm as is.
    LLVMTarget_CPP=`    echo "$LlvmTarget"`

    echo "GHC build  : $BuildPlatform"
    echo "GHC host   : $HostPlatform"
    echo "GHC target : $TargetPlatform"
    echo "LLVM target: $LlvmTarget"

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
    AC_SUBST(LLVMTarget_CPP)

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
        x86_64|amd64)
            test -z "[$]2" || eval "[$]2=ArchX86_64"
            ;;
        powerpc)
            test -z "[$]2" || eval "[$]2=ArchPPC"
            ;;
        powerpc64)
            test -z "[$]2" || eval "[$]2=\"ArchPPC_64 {ppc_64ABI = ELF_V1}\""
            ;;
        powerpc64le)
            test -z "[$]2" || eval "[$]2=\"ArchPPC_64 {ppc_64ABI = ELF_V2}\""
            ;;
        sparc)
            test -z "[$]2" || eval "[$]2=ArchSPARC"
            ;;
        sparc64)
            test -z "[$]2" || eval "[$]2=ArchSPARC64"
            ;;
        arm)
            GET_ARM_ISA()
            test -z "[$]2" || eval "[$]2=\"ArchARM {armISA = \$ARM_ISA, armISAExt = \$ARM_ISA_EXT, armABI = \$ARM_ABI}\""
            ;;
        aarch64)
            test -z "[$]2" || eval "[$]2=ArchARM64"
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
        hppa|hppa1_1|ia64|m68k|nios2|rs6000|s390|s390x|sh4|vax)
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
        darwin|ios)
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
        dragonfly|hpux|linuxaout|freebsd2|gnu|nextstep2|nextstep3|sunos4|ultrix)
            test -z "[$]2" || eval "[$]2=OSUnknown"
            ;;
        aix)
            test -z "[$]2" || eval "[$]2=OSAIX"
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
    AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([__asm__ (".section .note.GNU-stack,\"\",$progbits");], [0])],
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
                         defined(__ARM_ARCH_6K__)  || \
                         defined(__ARM_ARCH_6KZ__) || \
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

        AC_SUBST(ARM_ISA)
])


# FP_SETTINGS
# ----------------------------------
# Set the variables used in the settings file
AC_DEFUN([FP_SETTINGS],
[
    if test "$windows" = YES -a "$EnableDistroToolchain" = "NO"
    then
        mingw_bin_prefix=mingw/bin/
        SettingsCCompilerCommand="\$topdir/../${mingw_bin_prefix}gcc.exe"
        SettingsHaskellCPPCommand="\$topdir/../${mingw_bin_prefix}gcc.exe"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="\$topdir/../${mingw_bin_prefix}ld.exe"
        SettingsArCommand="\$topdir/../${mingw_bin_prefix}ar.exe"
        SettingsRanlibCommand="\$topdir/../${mingw_bin_prefix}ranlib.exe"
        SettingsPerlCommand='$topdir/../perl/perl.exe'
        SettingsDllWrapCommand="\$topdir/../${mingw_bin_prefix}dllwrap.exe"
        SettingsWindresCommand="\$topdir/../${mingw_bin_prefix}windres.exe"
        SettingsTouchCommand='$topdir/bin/touchy.exe'
    elif test "$EnableDistroToolchain" = "YES"
    then
        SettingsCCompilerCommand="$(basename $CC)"
        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
        SettingsHaskellCPPCommand="$(basename $HaskellCPPCmd)"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="$(basename $LdCmd)"
        SettingsArCommand="$(basename $ArCmd)"
        SettingsPerlCommand="$(basename $PerlCmd)"
        SettingsDllWrapCommand="$(basename $DllWrapCmd)"
        SettingsWindresCommand="$(basename $WindresCmd)"
        SettingsTouchCommand='$topdir/bin/touchy.exe'
    else
        SettingsCCompilerCommand="$CC"
        SettingsHaskellCPPCommand="$HaskellCPPCmd"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="$LdCmd"
        SettingsArCommand="$ArCmd"
        SettingsRanlibCommand="$RanlibCmd"
        SettingsPerlCommand="$PerlCmd"
        if test -z "$DllWrapCmd"
        then
            SettingsDllWrapCommand="/bin/false"
        else
            SettingsDllWrapCommand="$DllWrapCmd"
        fi
        if test -z "$WindresCmd"
        then
            SettingsWindresCommand="/bin/false"
        else
            SettingsWindresCommand="$WindresCmd"
        fi
       SettingsTouchCommand='touch'
    fi
    if test -z "$LibtoolCmd"
    then
      SettingsLibtoolCommand="libtool"
    else
      SettingsLibtoolCommand="$LibtoolCmd"
    fi
    if test -z "$ClangCmd"
    then
        SettingsClangCommand="clang"
    else
        SettingsClangCommand="$ClangCmd"
    fi
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
    SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
    SettingsCCompilerLinkFlags="$CONF_GCC_LINKER_OPTS_STAGE2"
    SettingsCCompilerSupportsNoPie="$CONF_GCC_SUPPORTS_NO_PIE"
    SettingsLdFlags="$CONF_LD_LINKER_OPTS_STAGE2"
    AC_SUBST(SettingsCCompilerCommand)
    AC_SUBST(SettingsHaskellCPPCommand)
    AC_SUBST(SettingsHaskellCPPFlags)
    AC_SUBST(SettingsCCompilerFlags)
    AC_SUBST(SettingsCCompilerLinkFlags)
    AC_SUBST(SettingsCCompilerSupportsNoPie)
    AC_SUBST(SettingsLdCommand)
    AC_SUBST(SettingsLdFlags)
    AC_SUBST(SettingsArCommand)
    AC_SUBST(SettingsRanlibCommand)
    AC_SUBST(SettingsPerlCommand)
    AC_SUBST(SettingsDllWrapCommand)
    AC_SUBST(SettingsWindresCommand)
    AC_SUBST(SettingsLibtoolCommand)
    AC_SUBST(SettingsTouchCommand)
    AC_SUBST(SettingsClangCommand)
    AC_SUBST(SettingsLlcCommand)
    AC_SUBST(SettingsOptCommand)
])

# Helper for cloning a shell variable's state
AC_DEFUN([FP_COPY_SHELLVAR],
[if test -n "${$1+set}"; then $2="$$1"; else unset $2; fi ])

# FP_SET_CFLAGS_C99
# ----------------------------------
# figure out which CFLAGS are needed to place the compiler into C99 mode
# $1 is name of CC variable (unmodified)
# $2 is name of CC flags variable (augmented if needed)
# $3 is name of CPP flags variable (augmented if needed)
AC_DEFUN([FP_SET_CFLAGS_C99],
[
    dnl save current state of AC_PROG_CC_C99
    FP_COPY_SHELLVAR([CC],[fp_save_CC])
    FP_COPY_SHELLVAR([CFLAGS],[fp_save_CFLAGS])
    FP_COPY_SHELLVAR([CPPFLAGS],[fp_save_CPPFLAGS])
    FP_COPY_SHELLVAR([ac_cv_prog_cc_c99],[fp_save_cc_c99])
    dnl set local state
    CC="$$1"
    CFLAGS="$$2"
    CPPFLAGS="$$3"
    unset ac_cv_prog_cc_c99
    dnl perform detection
    _AC_PROG_CC_C99
    fp_cc_c99="$ac_cv_prog_cc_c99"
    case "x$ac_cv_prog_cc_c99" in
      x)   ;; # noop
      xno) AC_MSG_ERROR([C99-compatible compiler needed]) ;;
      *)   $2="$$2 $ac_cv_prog_cc_c99"
           $3="$$3 $ac_cv_prog_cc_c99"
           ;;
    esac
    dnl restore saved state
    FP_COPY_SHELLVAR([fp_save_CC],[CC])
    FP_COPY_SHELLVAR([fp_save_CFLAGS],[CFLAGS])
    FP_COPY_SHELLVAR([fp_save_CPPFLAGS],[CPPFLAGS])
    FP_COPY_SHELLVAR([fp_save_cc_c99],[ac_cv_prog_cc_c99])
    dnl cleanup
    unset fp_save_CC
    unset fp_save_CFLAGS
    unset fp_save_cc_c99
])

# FPTOOLS_SET_C_LD_FLAGS
# ----------------------------------
# Set the C, LD and CPP flags for a given platform.
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
    i386-unknown-mingw32)
        $2="$$2 -march=i686"
        ;;
    i386-portbld-freebsd*)
        $2="$$2 -march=i686"
        ;;
    x86_64-unknown-solaris2)
        $2="$$2 -m64"
        $3="$$3 -m64"
        $4="$$4 -m64"
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
    arm*linux*)
        # On arm/linux and arm/android, tell gcc to generate Arm
        # instructions (ie not Thumb).
        $2="$$2 -marm"
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;

    aarch64*linux*)
        $3="$$3 -Wl,-z,noexecstack"
        $4="$$4 -z noexecstack"
        ;;

    powerpc-ibm-aix*)
        # We need `-D_THREAD_SAFE` to unlock the thread-local `errno`.
        $2="$$2 -D_THREAD_SAFE"
        $3="$$3 -D_THREAD_SAFE -Wl,-bnotextro"
        $4="$$4 -bnotextro"
        $5="$$5 -D_THREAD_SAFE"
        ;;

    x86_64-*-openbsd*)
        # We need -z wxneeded at least to link ghc-stage2 to workaround
        # W^X issue in GHCi on OpenBSD current (as of Aug 2016)
        $3="$$3 -Wl,-z,wxneeded"
        $4="$$4 -z wxneeded"
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



# FP_CHECK_SIZEOF_AND_ALIGNMENT(TYPE)
# ------------------------------------------------------------------
# Combines AC_CHECK_SIZEOF and FP_CHECK_ALIGNMENT.
AC_DEFUN([FP_CHECK_SIZEOF_AND_ALIGNMENT],
[AC_CHECK_SIZEOF([$1])
FP_CHECK_ALIGNMENT([$1])
])# FP_CHECK_SIZEOF_AND_ALIGNMENT


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
#
AC_DEFUN([FP_LEADING_UNDERSCORE],
[AC_CHECK_LIB([elf], [nlist], [LIBS="-lelf $LIBS"])
AC_CACHE_CHECK([leading underscore in symbol names], [fptools_cv_leading_underscore], [
# Hack!: nlist() under Digital UNIX insist on there being an _,
# but symbol table listings shows none. What is going on here?!?
case $TargetPlatform in
    # Apples mach-o platforms use leading underscores
    *-apple-*) fptools_cv_leading_underscore=yes;;
    *linux-android*) fptools_cv_leading_underscore=no;;
    *openbsd*) # x86 openbsd is ELF from 3.4 >, meaning no leading uscore
      case $build in
        i386-*2\.@<:@0-9@:>@ | i386-*3\.@<:@0-3@:>@ ) fptools_cv_leading_underscore=yes ;;
        *) fptools_cv_leading_underscore=no ;;
      esac ;;
    i386-unknown-mingw32) fptools_cv_leading_underscore=yes;;
    x86_64-unknown-mingw32) fptools_cv_leading_underscore=no;;
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
   AC_SUBST([CabalLeadingUnderscore],[True])
   AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.])
else
   AC_SUBST([CabalLeadingUnderscore],[False])
fi
])# FP_LEADING_UNDERSCORE


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
dnl Check for Happy and version.
dnl If there's no installed Happy, we look
dnl for a happy source tree and point the build system at that instead.
dnl If you increase the minimum version requirement, please also update:
dnl https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools
dnl
AC_DEFUN([FPTOOLS_HAPPY],
[AC_PATH_PROG(HappyCmd,happy,)

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
if test ! -f compiler/parser/Parser.hs || test ! -f compiler/cmm/CmmParse.hs
then
    FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.19.4],
      [AC_MSG_ERROR([Happy version 1.19.4 or later is required to compile GHC.])])[]
fi
HappyVersion=$fptools_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl Check for Alex and version.
dnl If you increase the minimum version requirement, please also update:
dnl https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools
dnl
AC_DEFUN([FPTOOLS_ALEX],
[
AC_PATH_PROG(AlexCmd,alex,)

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
    FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[3.1.0],
      [AC_MSG_ERROR([Alex version 3.1.0 or later is required to compile GHC.])])[]
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
AC_DEFUN([FP_PROG_LD_IS_GNU],[
AC_CACHE_CHECK([whether ld is GNU ld], [fp_cv_gnu_ld],
[[if ${LdCmd} --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_gnu_ld=YES
else
  fp_cv_gnu_ld=NO
fi]])
AC_SUBST([LdIsGNULd],["$fp_cv_gnu_ld"])
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
# Sets fp_prog_ar to a path to ar. Exits if no ar can be found
# The host normalization on Windows breaks autoconf, it no longer
# thinks that target == host so it never checks the unqualified
# tools for Windows. See #14274.
AC_DEFUN([FP_PROG_AR],
[if test -z "$fp_prog_ar"; then
  if test "$HostOS" = "mingw32"
  then
    AC_PATH_PROG([fp_prog_ar], [ar])
    if test -n "$fp_prog_ar"; then
      fp_prog_ar=$(cygpath -m $fp_prog_ar)
    fi
  else
    AC_CHECK_TARGET_TOOL([fp_prog_ar], [ar])
  fi
fi
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
# to an invocation of ar including these arguments.
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
  for fp_var in qclsZ qcls qcs qcl qc ; do
     rm -f conftest.a
     if "$fp_prog_ar" $fp_var conftest.a conftest.dummy > /dev/null 2> /dev/null ; then
       # Also check that a result was created; it seems some llvm-ar versions
       # exit with code zero even if they fail to parse the command line.
       if test -f conftest.a ; then
         fp_cv_prog_ar_args=$fp_var
         break
       fi
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
    elif test "$TargetVendor_CPP" = "apple"
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

    # workaround for AC_PROG_RANLIB which sets RANLIB to `:' when
    # ranlib is missing on the target OS. The problem is that
    # ghc-cabal cannot execute `:' which is a shell built-in but can
    # execute `true' which is usually simple program supported by the
    # OS.
    # Fixes #8795
    if test "$RANLIB" = ":"
    then
        RANLIB="true"
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
GccLT46=NO
AC_CACHE_CHECK([version of gcc], [fp_cv_gcc_version],
[
    # Be sure only to look at the first occurrence of the "version " string;
    # Some Apple compilers emit multiple messages containing this string.
    fp_cv_gcc_version="`$CC -v 2>&1 | sed -n -e '1,/version /s/.*version [[^0-9]]*\([[0-9.]]*\).*/\1/p'`"
    FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [4.4],
                        [AC_MSG_ERROR([Need at least gcc version 4.4 (4.7+ recommended)])])
    FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-lt], [4.6], GccLT46=YES)
])
AC_SUBST([GccVersion], [$fp_cv_gcc_version])
AC_SUBST(GccLT46)
])# FP_GCC_VERSION

dnl Check to see if the C compiler is clang or llvm-gcc
dnl
GccIsClang=NO
AC_DEFUN([FP_CC_LLVM_BACKEND],
[AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING([whether C compiler is clang])
$CC -x c /dev/null -dM -E > conftest.txt 2>&1
if grep "__clang__" conftest.txt >/dev/null 2>&1; then
  AC_SUBST([CC_CLANG_BACKEND], [1])
  AC_SUBST([CC_LLVM_BACKEND], [1])
  GccIsClang=YES
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
AC_SUBST(GccIsClang)

rm -f conftest.txt
])

# FP_GCC_SUPPORTS__ATOMICS
# ------------------------
# Does gcc support the __atomic_* family of builtins?
AC_DEFUN([FP_GCC_SUPPORTS__ATOMICS],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether GCC supports __atomic_ builtins])
   echo 'int test(int *x) { int y; __atomic_load(&x, &y, __ATOMIC_SEQ_CST); return x; }' > conftest.c
   if $CC -c conftest.c > /dev/null 2>&1; then
       CONF_GCC_SUPPORTS__ATOMICS=YES
       AC_MSG_RESULT([yes])
   else
       CONF_GCC_SUPPORTS__ATOMICS=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest.o
])

# FP_GCC_SUPPORTS_NO_PIE
# ----------------------
# Does gcc support the -no-pie option? If so we should pass it to gcc when
# joining objects since -pie may be enabled by default.
AC_DEFUN([FP_GCC_SUPPORTS_NO_PIE],
[
   AC_REQUIRE([AC_PROG_CC])
   AC_MSG_CHECKING([whether GCC supports -no-pie])
   echo 'int main() { return 0; }' > conftest.c
   # Some GCC versions only warn when passed an unrecognized flag.
   if $CC -no-pie -x c /dev/null -dM -E > conftest.txt 2>&1 && ! grep -i unrecognized conftest.txt > /dev/null 2>&1; then
       CONF_GCC_SUPPORTS_NO_PIE=YES
       AC_MSG_RESULT([yes])
   else
       CONF_GCC_SUPPORTS_NO_PIE=NO
       AC_MSG_RESULT([no])
   fi
   rm -f conftest.c conftest.o conftest
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
fp_ghc_pkg_guess=`echo "$WithGhc" | sed -e 's/-stage@<:@0-9@:>@$//' -e 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
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
 FP_COMPARE_VERSIONS([$fp_cv_gcc_version], [-ge], [4.0],
  [fp_cv_gcc_extra_opts="$fp_cv_gcc_extra_opts -fno-builtin"],
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
        ver_posixtime=`git log -1 --pretty=format:%ct`
        ver_date=`perl -MPOSIX -e "print strftime('%Y%m%d', gmtime($ver_posixtime));"`
        if echo $ver_date | grep '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$' 2>&1 >/dev/null; then true; else
        changequote([, ])dnl
                AC_MSG_ERROR([failed to detect version date: check that git and perl are in your path])
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

    AC_MSG_CHECKING([for GHC Git commit id])
    if test -d .git; then
        git_commit_id=`git rev-parse HEAD`
        if test -n "$git_commit_id" 2>&1 >/dev/null; then true; else
            AC_MSG_ERROR([failed to detect revision: check that git is in your path])
        fi
        PACKAGE_GIT_COMMIT_ID=$git_commit_id
        AC_MSG_RESULT(inferred $PACKAGE_GIT_COMMIT_ID)
    elif test -f GIT_COMMIT_ID; then
        PACKAGE_GIT_COMMIT_ID=`cat GIT_COMMIT_ID`
        AC_MSG_RESULT(given $PACKAGE_GIT_COMMIT_ID)
    else
        AC_MSG_WARN([cannot determine snapshot revision: no .git directory and no 'GIT_COMMIT_ID' file])
        PACKAGE_GIT_COMMIT_ID="0000000000000000000000000000000000000000"
    fi


# Some renamings
AC_SUBST([ProjectName], [$PACKAGE_NAME])
AC_SUBST([ProjectVersion], [$PACKAGE_VERSION])
AC_SUBST([ProjectGitCommitId], [$PACKAGE_GIT_COMMIT_ID])

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

# Save split version of ProjectPatchLevel
ProjectPatchLevel1=`echo $ProjectPatchLevel | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1/'`
ProjectPatchLevel2=`echo $ProjectPatchLevel | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3/'`

AC_SUBST([ProjectPatchLevel1])
AC_SUBST([ProjectPatchLevel2])

# Remove dots from the patch level; this allows us to have versions like 6.4.1.20050508
ProjectPatchLevel=`echo $ProjectPatchLevel | sed 's/\.//'`

AC_SUBST([ProjectPatchLevel])

# The version of the GHC package changes every day, since the
# patchlevel is the current date.  We don't want to force
# recompilation of the entire compiler when this happens, so for
# GHC HEAD we omit the patchlevel from the package version number.
#
# The ProjectPatchLevel1 > 20000000 iff GHC HEAD. If it's for a stable
# release like 7.10.1 or for a release candidate such as 7.10.1.20141224
# then we don't omit the patchlevel components.

ProjectVersionMunged="$ProjectVersion"
if test "$ProjectPatchLevel1" -gt 20000000; then
  ProjectVersionMunged="${VERSION_MAJOR}.${VERSION_MINOR}"
fi
AC_SUBST([ProjectVersionMunged])
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
    AC_DEFINE([USE_TIMER_CREATE], 1,  [Define to 1 if we can use timer_create(CLOCK_REALTIME,...)])
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

    if (timer_create(CLOCK_REALTIME, &ev, &timer) != 0) {
        fprintf(stderr,"No CLOCK_REALTIME timer\n");
        exit(2);
    }

    tock = 0;

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 1000000; // 1ms
    it.it_interval = it.it_value;
    if (timer_settime(timer, 0, &it, NULL) != 0) {
        fprintf(stderr,"settime problem\n");
        exit(4);
    }

    // some environments have coarse scheduler/timer granularity of ~10ms and worse
    usleep(100000); // 100ms

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
                   [Define to 1 if we can use timer_create(CLOCK_REALTIME,...)]);;
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

# FP_CURSES
# -------------
AC_DEFUN([FP_CURSES],
[
  dnl--------------------------------------------------------------------
  dnl * Deal with arguments telling us curses is somewhere odd
  dnl--------------------------------------------------------------------

  AC_ARG_WITH([curses-libraries],
    [AC_HELP_STRING([--with-curses-libraries],
      [directory containing curses libraries])],
      [CURSES_LIB_DIRS=$withval])

  AC_SUBST(CURSES_INCLUDE_DIRS)
  AC_SUBST(CURSES_LIB_DIRS)
])# FP_CURSES

# --------------------------------------------------------------
# Calculate absolute path to build tree
# --------------------------------------------------------------

AC_DEFUN([FP_FIND_ROOT],[
AC_MSG_CHECKING(for path to top of build tree)
    if test "$windows" = YES
    then
      dnl Make sure this is a c:/foo/bar (mixed) style path. Some parts of
      dnl the build system might depend on it (such as the sed expression
      dnl `"s|$(TOP)/||i"` in addCFileDeps in rules/build-dependencies.mk).
      hardtop=$(cygpath -m "$(pwd)")
    else
      hardtop=$(pwd)
    fi

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
  aarch64*)
    $2="aarch64"
    ;;
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
  nios2)
    $2="nios2"
    ;;
  powerpc64le*)
    $2="powerpc64le"
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
  sh4)
    $2="sh4"
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
  x86_64|amd64)
    $2="x86_64"
    ;;
  *)
    echo "Unknown CPU $1"
    exit 1
    ;;
  esac
])

# GHC_LLVM_TARGET(target_cpu, target_vendor, target_os, llvm_target_var)
# --------------------------------
# converts the canonicalized target into someting llvm can understand
AC_DEFUN([GHC_LLVM_TARGET], [
  case "$2-$3" in
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
    *-android*|*-gnueabi*)
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
  hardfloat) # like armv7a-hardfloat-linux-gnueabi
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
    case "$1" in
      # watchos and tvos are ios variants as of May 2017.
      ios|watchos|tvos)
        $3="ios"
        ;;
      linux-android*)
        $3="linux-android"
        ;;
      linux-*|linux)
        $3="linux"
        ;;
      # As far as I'm aware, none of these have relevant variants
      freebsd|netbsd|openbsd|dragonfly|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|haiku)
        $3="$1"
        ;;
      aix*) # e.g. powerpc-ibm-aix7.1.3.0
        $3="aix"
        ;;
      darwin*) # e.g. aarch64-apple-darwin14
        $3="darwin"
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
    if test "$TargetVendor_CPP" = "apple"
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
# $2 = the command to look for
# $3 = the version of the command to look for
#
AC_DEFUN([FIND_LLVM_PROG],[
    # Test for program with and without version name.
    AC_CHECK_TOOLS([$1], [$2-$3 $2], [:])
    if test "$$1" != ":"; then
        AC_MSG_CHECKING([$$1 is version $3])
        if test `$$1 --version | grep -c "version $3"` -gt 0 ; then
            AC_MSG_RESULT(yes)
        else
            AC_MSG_RESULT(no)
            $1=""
        fi
    else
        $1=""
    fi
])

# CHECK_LD_COPY_BUG()
# -------------------
# Check for binutils bug #16177 present in some versions of the bfd ld
# implementation affecting ARM relocations.
# https://sourceware.org/bugzilla/show_bug.cgi?id=16177
#
# $1 = the platform
#
AC_DEFUN([CHECK_LD_COPY_BUG],[
    case $1 in
      arm*linux*)
        AC_CHECK_TARGET_TOOL([READELF], [readelf])
        AC_CHECK_TARGET_TOOL([AS], [as])
        AC_MSG_CHECKING([for ld bug 16177])
        cat >actest.s <<-EOF
          .globl _start
          .p2align 4
        _start:
          bkpt

        .data
          .globl data_object
        object_reference:
          .long data_object
          .size object_reference, 4
EOF

        cat >aclib.s <<-EOF
          .data
          .globl data_object
          .type data_object, %object
          .size data_object, 4
        data_object:
            .long 123
EOF

        $AS -o aclib.o aclib.s
        $LD -shared -o aclib.so aclib.o

        $AS -o actest.o actest.s
        $LD -o actest actest.o aclib.so

        if $READELF -r actest | grep R_ARM_COPY > /dev/null; then
            AC_MSG_RESULT([affected])
            AC_MSG_ERROR(
              [Your linker is affected by binutils #16177, which
               critically breaks linkage of GHC objects. Please either upgrade
               binutils or supply a different linker with the LD environment
               variable.])
        else
            AC_MSG_RESULT([unaffected])
        fi

        rm -f aclib.s aclib.o aclib.so actest.s actest.o actest
        ;;
      *)
        ;;
    esac
])

# FIND_GHC_BOOTSTRAP_PROG()
# --------------------------------
# Parse the bootstrap GHC's compier settings file for the location of things
# like the `llc` and `opt` commands.
#
# $1 = the variable to set
# $2 = The bootstrap compiler.
# $3 = The string to grep for to find the correct line.
#
AC_DEFUN([FIND_GHC_BOOTSTRAP_PROG],[
    BootstrapTmpCmd=`grep $3 $($2 --print-libdir)/settings 2>/dev/null | sed 's/.*", "//;s/".*//'`
    if test -n "$BootstrapTmpCmd" && test `basename $BootstrapTmpCmd` = $BootstrapTmpCmd ; then
        AC_PATH_PROG([$1], [$BootstrapTmpCmd], "")
    else
        $1=$BootstrapTmpCmd
    fi
])


AC_DEFUN([MAYBE_OVERRIDE_STAGE0],[
  if test ! -z "$With_$1" -a "$CrossCompiling" != "YES"; then
      AC_MSG_NOTICE([Not cross-compiling, so --with-$1 also sets $2])
      $2=$With_$1
  fi
])


# FP_CPP_CMD_WITH_ARGS()
# ----------------------
# sets CPP command and its arguments
#
# $1 = the variable to set to CPP command
# $2 = the variable to set to CPP command arguments

AC_DEFUN([FP_CPP_CMD_WITH_ARGS],[
dnl ** what cpp to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(hs-cpp,
[AC_HELP_STRING([--with-hs-cpp=ARG],
      [Path to the (C) preprocessor for Haskell files [default=autodetect]])],
[
    if test "$HostOS" = "mingw32"
    then
        AC_MSG_WARN([Request to use $withval will be ignored])
    else
        HS_CPP_CMD=$withval
    fi
],
[

    # We can't use $CPP here, since HS_CPP_CMD is expected to be a single
    # command (no flags), and AC_PROG_CPP defines CPP as "/usr/bin/gcc -E".
    HS_CPP_CMD=$CC

    SOLARIS_GCC_CPP_BROKEN=NO
    SOLARIS_FOUND_GOOD_CPP=NO
    case $host in
        i386-*-solaris2)
        GCC_MAJOR_MINOR=`$CC --version|grep "gcc (GCC)"|cut -d ' ' -f 3-3|cut -d '.' -f 1-2`
        if test "$GCC_MAJOR_MINOR" != "3.4"; then
          # this is not 3.4.x release so with broken CPP
          SOLARIS_GCC_CPP_BROKEN=YES
        fi
        ;;
    esac

    if test "$SOLARIS_GCC_CPP_BROKEN" = "YES"; then
      # let's try to find if GNU C 3.4.x is installed
      if test -x /usr/sfw/bin/gcc; then
        # something executable is in expected path so let's
        # see if it's really GNU C
        NEW_GCC_MAJOR_MINOR=`/usr/sfw/bin/gcc --version|grep "gcc (GCC)"|cut -d ' ' -f 3-3|cut -d '.' -f 1-2`
        if test "$NEW_GCC_MAJOR_MINOR" = "3.4"; then
          # this is GNU C 3.4.x which provides non-broken CPP on Solaris
          # let's use it as CPP then.
          HS_CPP_CMD=/usr/sfw/bin/gcc
          SOLARIS_FOUND_GOOD_CPP=YES
        fi
      fi
      if test "$SOLARIS_FOUND_GOOD_CPP" = "NO"; then
        AC_MSG_WARN([Your GNU C provides broken CPP and you do not have GNU C 3.4.x installed.])
        AC_MSG_WARN([Please install GNU C 3.4.x to solve this issue. It will be used as CPP only.])
      fi
    fi
]
)



dnl ** what cpp flags to use?
dnl -----------------------------------------------------------
AC_ARG_WITH(hs-cpp-flags,
  [AC_HELP_STRING([--with-hs-cpp-flags=ARG],
      [Flags to the (C) preprocessor for Haskell files [default=autodetect]])],
  [
      if test "$HostOS" = "mingw32"
      then
          AC_MSG_WARN([Request to use $withval will be ignored])
      else
          HS_CPP_ARGS=$withval
      fi
  ],
[
  $HS_CPP_CMD -x c /dev/null -dM -E > conftest.txt 2>&1
  if grep "__clang__" conftest.txt >/dev/null 2>&1; then
    HS_CPP_ARGS="-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"
  else
      $HS_CPP_CMD  -v > conftest.txt 2>&1
      if  grep "gcc" conftest.txt >/dev/null 2>&1; then
          HS_CPP_ARGS="-E -undef -traditional"
        else
          $HS_CPP_CMD  --version > conftest.txt 2>&1
          if grep "cpphs" conftest.txt >/dev/null 2>&1; then
            HS_CPP_ARGS="--cpp -traditional"
          else
            AC_MSG_WARN([configure can't recognize your CPP program, you may need to set --with-hs-cpp-flags=FLAGS explicitly])
            HS_CPP_ARGS=""
          fi
      fi
  fi
  ]
)

$1=$HS_CPP_CMD
$2=$HS_CPP_ARGS

])

# FP_BFD_SUPPORT()
# ----------------------
# whether to use libbfd for debugging RTS
AC_DEFUN([FP_BFD_SUPPORT], [
    AC_SUBST([CabalHaveLibbfd], [False])
    AC_ARG_ENABLE(bfd-debug,
        [AC_HELP_STRING([--enable-bfd-debug],
              [Enable symbol resolution for -debug rts ('+RTS -Di') via binutils' libbfd [default=no]])],
        [
            # don't pollute general LIBS environment
            save_LIBS="$LIBS"
            AC_CHECK_HEADERS([bfd.h])
            dnl ** check whether this machine has BFD and libiberty installed (used for debugging)
            dnl    the order of these tests matters: bfd needs libiberty
            AC_CHECK_LIB(iberty, xmalloc)
            dnl 'bfd_init' is a rare non-macro in libbfd
            AC_CHECK_LIB(bfd,    bfd_init)

            AC_TRY_LINK([#include <bfd.h>],
                        [
                                /* mimic our rts/Printer.c */
                                bfd* abfd;
                                const char * name;
                                char **matching;

                                name = "some.executable";
                                bfd_init();
                                abfd = bfd_openr(name, "default");
                                bfd_check_format_matches (abfd, bfd_object, &matching);
                                {
                                    long storage_needed;
                                    storage_needed = bfd_get_symtab_upper_bound (abfd);
                                }
                                {
                                    asymbol **symbol_table;
                                    long number_of_symbols;
                                    symbol_info info;

                                    number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);
                                    bfd_get_symbol_info(abfd,symbol_table[0],&info);
                                }
                        ],
                        [AC_SUBST([CabalHaveLibbfd], [True])],dnl bfd seems to work
                        [AC_MSG_ERROR([can't use 'bfd' library])])
            LIBS="$save_LIBS"
        ]
    )
])


# FP_CC_LINKER_FLAG_TRY()
# --------------------
# Try a particular linker to see whether we can use it. In particular, determine
# whether we can convince gcc to use it via a -fuse-ld=... flag.
#
# $1 = the name of the linker to try
# $2 = the variable to set with the appropriate GHC flag if the linker is
# found to be usable
AC_DEFUN([FP_CC_LINKER_FLAG_TRY], [
    AC_MSG_CHECKING([whether C compiler supports -fuse-ld=$1])
    echo 'int main(void) {return 0;}' > conftest.c
    if $CC -o conftest.o -fuse-ld=$1 conftest.c > /dev/null 2>&1
    then
        $2="-fuse-ld=$1"
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    rm -f conftest.c conftest.o
])

# FIND_LD
# ---------
# Find the version of `ld` to use and figure out how to get gcc to use it for
# linking (if --enable-ld-override is enabled). This is used in both in the top
# level configure.ac and in distrib/configure.ac.in.
#
# $1 = the platform
# $2 = the variable to set with GHC options to configure gcc to use the chosen linker
#
AC_DEFUN([FIND_LD],[
    AC_ARG_ENABLE(ld-override,
      [AC_HELP_STRING([--disable-ld-override],
        [Prevent GHC from overriding the default linker used by gcc. If ld-override is enabled GHC will try to tell gcc to use whichever linker is selected by the LD environment variable. [default=override enabled]])],
      [],
      [enable_ld_override=yes])

    find_ld() {
        # Make sure the user didn't specify LD manually.
        if test "z$LD" != "z"; then
            AC_CHECK_TARGET_TOOL([LD], [ld])
            return
        fi

        # Manually iterate over possible names since we want to ensure that, e.g.,
        # if ld.lld is installed but gcc doesn't support -fuse-ld=lld, that we
        # then still try ld.gold and -fuse-ld=gold.
        for possible_ld in ld.lld ld.gold ld; do
            TmpLd="" # In case the user set LD
            AC_CHECK_TARGET_TOOL([TmpLd], [$possible_ld])
            if test "x$TmpLd" = "x"; then continue; fi

            out=`$TmpLd --version`
            case $out in
              "GNU ld"*)   FP_CC_LINKER_FLAG_TRY(bfd, $2) ;;
              "GNU gold"*) FP_CC_LINKER_FLAG_TRY(gold, $2) ;;
              "LLD"*)      FP_CC_LINKER_FLAG_TRY(lld, $2) ;;
              *) AC_MSG_NOTICE([unknown linker version $out]) ;;
            esac
            if test "z$$2" = "z"; then
                AC_MSG_NOTICE([unable to convince '$CC' to use linker '$TmpLd'])
                # a terrible hack to prevent autoconf from caching the previous
                # AC_CHECK_TARGET_TOOL result since next time we'll be looking
                # for another ld variant.
                $as_unset ac_cv_prog_ac_ct_TmpLd
            else
                LD="$TmpLd"
                return
            fi
        done

        # Fallback
        AC_CHECK_TARGET_TOOL([LD], [ld])
    }

    if test "x$enable_ld_override" = "xyes"; then
        find_ld
    else
        AC_CHECK_TARGET_TOOL([LD], [ld])
    fi

    CHECK_LD_COPY_BUG([$1])
])

# LocalWords:  fi
