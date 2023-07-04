# FP_SETTINGS
# ----------------------------------
# Set the variables used in the settings file
AC_DEFUN([FP_SETTINGS],
[
    SettingsUseDistroMINGW="$EnableDistroToolchain"

    if test "$windows" = YES -a "$EnableDistroToolchain" = "NO"; then
        # Handle the Windows toolchain installed in FP_SETUP_WINDOWS_TOOLCHAIN.
        # See Note [tooldir: How GHC finds mingw on Windows]
        mingw_bin_prefix='$$tooldir/mingw/bin/'
        SettingsCCompilerCommand="${mingw_bin_prefix}clang.exe"
        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2 -I\$\$tooldir/mingw/include"
        SettingsCxxCompilerCommand="${mingw_bin_prefix}clang++.exe"
        SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2 -I\$\$tooldir/mingw/include"
        SettingsCCompilerLinkFlags="$CONF_GCC_LINKER_OPTS_STAGE2 -L\$\$tooldir/mingw/lib -L\$\$tooldir/mingw/x86_64-w64-mingw32/lib"
        SettingsCPPCommand="${mingw_bin_prefix}clang.exe"
        SettingsCPPFlags="$CONF_CPP_OPTS_STAGE2 -I\$\$tooldir/mingw/include"
        SettingsHaskellCPPCommand="${mingw_bin_prefix}clang.exe"
        SettingsHaskellCPPFlags="$HaskellCPPArgs -I\$\$tooldir/mingw/include"
        # LLD does not support object merging (#21068)
        SettingsMergeObjectsCommand=""
        SettingsMergeObjectsFlags=""
        SettingsArCommand="${mingw_bin_prefix}llvm-ar.exe"
        SettingsRanlibCommand="${mingw_bin_prefix}llvm-ranlib.exe"
        SettingsDllWrapCommand="${mingw_bin_prefix}llvm-dllwrap.exe"
        SettingsWindresCommand="${mingw_bin_prefix}llvm-windres.exe"
        SettingsTouchCommand='$$topdir/bin/touchy.exe'

    else
        # This case handles the "normal" platforms (e.g. not Windows) where we
        # don't provide the toolchain.

        SettingsCCompilerCommand="$CC"
        SettingsCCompilerFlags="$CONF_CC_OPTS_STAGE2"
        SettingsCxxCompilerCommand="$CXX"
        SettingsCxxCompilerFlags="$CONF_CXX_OPTS_STAGE2"
        SettingsCPPCommand="$CPPCmd"
        SettingsCPPFlags="$CONF_CPP_OPTS_STAGE2"
        SettingsHaskellCPPCommand="$HaskellCPPCmd"
        SettingsHaskellCPPFlags="$HaskellCPPArgs"
        SettingsCCompilerLinkFlags="$CONF_GCC_LINKER_OPTS_STAGE2"
        SettingsArCommand="$ArCmd"
        SettingsRanlibCommand="$RanlibCmd"
        SettingsMergeObjectsCommand="$MergeObjsCmd"
        SettingsMergeObjectsFlags="$MergeObjsArgs"

        if test -z "$DllWrapCmd"; then
            SettingsDllWrapCommand="/bin/false"
        else
            SettingsDllWrapCommand="$DllWrapCmd"
        fi
        if test -z "$WindresCmd"; then
            SettingsWindresCommand="/bin/false"
        else
            SettingsWindresCommand="$WindresCmd"
        fi

        if test "$HostOS" = "mingw32"; then
            SettingsTouchCommand='$$topdir/bin/touchy.exe'
        else
            SettingsTouchCommand='touch'
        fi

        if test "$EnableDistroToolchain" = "YES"; then
            # If the user specified --enable-distro-toolchain then we just use the
            # executable names, not paths.
            SettingsCCompilerCommand="$(basename $SettingsCCompilerCommand)"
            SettingsHaskellCPPCommand="$(basename $SettingsHaskellCPPCommand)"
            SettingsLdCommand="$(basename $SettingsLdCommand)"
            SettingsMergeObjectsCommand="$(basename $SettingsMergeObjectsCommand)"
            SettingsArCommand="$(basename $SettingsArCommand)"
            SettingsDllWrapCommand="$(basename $SettingsDllWrapCommand)"
            SettingsWindresCommand="$(basename $SettingsWindresCommand)"
        fi
    fi

    # LLVM backend tools
    if test -z "$LlcCmd"; then
        LlcCmd="llc"
    fi
    SettingsLlcCommand="$LlcCmd"

    if test -z "$OptCmd"; then
        OptCmd="opt"
    fi
    SettingsOptCommand="$OptCmd"

    # Mac-only tools
    if test -z "$OtoolCmd"; then
        OtoolCmd="otool"
    fi
    SettingsOtoolCommand="$OtoolCmd"

    if test -z "$InstallNameToolCmd"; then
        InstallNameToolCmd="install_name_tool"
    fi
    SettingsInstallNameToolCommand="$InstallNameToolCmd"

    SettingsCCompilerSupportsNoPie="$CONF_GCC_SUPPORTS_NO_PIE"

    AC_SUBST(SettingsCCompilerCommand)
    AC_SUBST(SettingsCxxCompilerCommand)
    AC_SUBST(SettingsCPPCommand)
    AC_SUBST(SettingsCPPFlags)
    AC_SUBST(SettingsHaskellCPPCommand)
    AC_SUBST(SettingsHaskellCPPFlags)
    AC_SUBST(SettingsCCompilerFlags)
    AC_SUBST(SettingsCxxCompilerFlags)
    AC_SUBST(SettingsCCompilerLinkFlags)
    AC_SUBST(SettingsCCompilerSupportsNoPie)
    AC_SUBST(SettingsMergeObjectsCommand)
    AC_SUBST(SettingsMergeObjectsFlags)
    AC_SUBST(SettingsArCommand)
    AC_SUBST(SettingsRanlibCommand)
    AC_SUBST(SettingsOtoolCommand)
    AC_SUBST(SettingsInstallNameToolCommand)
    AC_SUBST(SettingsDllWrapCommand)
    AC_SUBST(SettingsWindresCommand)
    AC_SUBST(SettingsTouchCommand)
    AC_SUBST(SettingsLlcCommand)
    AC_SUBST(SettingsOptCommand)
    AC_SUBST(SettingsUseDistroMINGW)
])
