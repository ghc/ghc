# FP_SETTINGS
# ----------------------------------
# Set the variables used in the settings file
# See Note [tooldir: How GHC finds mingw on Windows]
AC_DEFUN([FP_SETTINGS],
[
    if test "$windows" = YES -a "$EnableDistroToolchain" = "NO"
    then
        mingw_bin_prefix='$$tooldir/mingw/bin/'
        SettingsCCompilerCommand="${mingw_bin_prefix}clang.exe"
        SettingsCCompilerFlags="--rtlib=compiler-rt"
        SettingsCxxCompilerFlags="--rtlib=compiler-rt"
        SettingsCCompilerLinkFlags="--rtlib=compiler-rt"
        SettingsHaskellCPPCommand="${mingw_bin_prefix}clang.exe"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="${mingw_bin_prefix}ld.lld.exe"
        # Overrides FIND_MERGE_OBJECTS in order to avoid hard-coding linker
        # path on Windows (#18550).
        SettingsMergeObjectsCommand="${mingw_bin_prefix}ld.bfd.exe"
        SettingsMergeObjectsFlags="-r --oformat=pe-bigobj-x86-64"
        SettingsArCommand="${mingw_bin_prefix}llvm-ar.exe"
        SettingsRanlibCommand="${mingw_bin_prefix}llvm-ranlib.exe"
        SettingsDllWrapCommand="${mingw_bin_prefix}llvm-dllwrap.exe"
        SettingsWindresCommand="${mingw_bin_prefix}llvm-windres.exe"
        SettingsTouchCommand='$$topdir/bin/touchy.exe'
    elif test "$EnableDistroToolchain" = "YES"
    then
        SettingsCCompilerCommand="$(basename $CC)"
        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
        SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2"
        SettingsHaskellCPPCommand="$(basename $HaskellCPPCmd)"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="$(basename $LdCmd)"
        SettingsMergeObjectsCommand="$(basename $MergeObjsCmd)"
        SettingsMergeObjectsFlags="$MergeObjsArgs"
        SettingsArCommand="$(basename $ArCmd)"
        SettingsDllWrapCommand="$(basename $DllWrapCmd)"
        SettingsWindresCommand="$(basename $WindresCmd)"
        SettingsTouchCommand='$$topdir/bin/touchy.exe'
    else
        SettingsCCompilerCommand="$CC"
        SettingsHaskellCPPCommand="$HaskellCPPCmd"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsLdCommand="$LdCmd"
        SettingsMergeObjectsCommand="$MergeObjsCmd"
        SettingsMergeObjectsFlags="$MergeObjsArgs"
        SettingsArCommand="$ArCmd"
        SettingsRanlibCommand="$RanlibCmd"
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
    if test -z "$OtoolCmd"
    then
      SettingsOtoolCommand="otool"
    else
      SettingsOtoolCommand="$OtoolCmd"
    fi
    if test -z "$InstallNameToolCmd"
    then
      SettingsInstallNameToolCommand="install_name_tool"
    else
      SettingsInstallNameToolCommand="$InstallNameToolCmd"
    fi
    SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
    SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2"
    SettingsCCompilerLinkFlags="$CONF_GCC_LINKER_OPTS_STAGE2"
    SettingsCCompilerSupportsNoPie="$CONF_GCC_SUPPORTS_NO_PIE"
    SettingsLdFlags="$CONF_LD_LINKER_OPTS_STAGE2"
    SettingsUseDistroMINGW="$EnableDistroToolchain"
    AC_SUBST(SettingsCCompilerCommand)
    AC_SUBST(SettingsHaskellCPPCommand)
    AC_SUBST(SettingsHaskellCPPFlags)
    AC_SUBST(SettingsCCompilerFlags)
    AC_SUBST(SettingsCxxCompilerFlags)
    AC_SUBST(SettingsCCompilerLinkFlags)
    AC_SUBST(SettingsCCompilerSupportsNoPie)
    AC_SUBST(SettingsLdCommand)
    AC_SUBST(SettingsLdFlags)
    AC_SUBST(SettingsMergeObjectsCommand)
    AC_SUBST(SettingsMergeObjectsFlags)
    AC_SUBST(SettingsArCommand)
    AC_SUBST(SettingsRanlibCommand)
    AC_SUBST(SettingsOtoolCommand)
    AC_SUBST(SettingsInstallNameToolCommand)
    AC_SUBST(SettingsDllWrapCommand)
    AC_SUBST(SettingsWindresCommand)
    AC_SUBST(SettingsLibtoolCommand)
    AC_SUBST(SettingsTouchCommand)
    AC_SUBST(SettingsClangCommand)
    AC_SUBST(SettingsLlcCommand)
    AC_SUBST(SettingsOptCommand)
    AC_SUBST(SettingsUseDistroMINGW)
])
