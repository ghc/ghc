# Download and install the windows toolchain
AC_DEFUN([FP_INSTALL_WINDOWS_TOOLCHAIN],[
    # Find the mingw-w64 archive file to extract.
    if test "$HostArch" = "x86_64"
    then
        mingw_arch="x86_64"
        tarball_dest_dir="mingw-w64/x86_64"
        tarball_mingw_dir="clang64"
    fi

    set_up_tarballs() {
        AC_MSG_NOTICE([Checking for Windows toolchain tarballs...])
        local action
        if test "$TarballsAutodownload" = "NO"
        then
            action="verify"
        else
            action="download"
        fi
        $PYTHON mk/get-win32-tarballs.py $action $mingw_arch > missing-win32-tarballs
        case $? in
            0)
            rm missing-win32-tarballs
            ;;
            2)
            echo
            echo "Error:"
            echo "Needed msys2 tarballs are missing. You have a few options to get them,"
            echo
            echo "  * run configure with the --enable-tarballs-autodownload option"
            echo
            echo "  * run mk/get-win32-tarballs.py download $mingw_arch"
            echo
            echo "  * manually download the files listed in ./missing-win32-tarballs and place"
            echo "    them in the ghc-tarballs directory."
            echo
            exit 1
            ;;
            *)
            echo
            echo "Error fetching msys2 tarballs; see errors above."
            exit 1
            ;;
        esac

        # Extract all the tarballs in one go
        if ! test -d inplace/mingw
        then
            AC_MSG_NOTICE([Extracting Windows toolchain from archives (may take a while)...])
            rm -rf inplace/mingw
            local base_dir="../ghc-tarballs/${tarball_dest_dir}"
            ( cd inplace &&
            find "${base_dir}" -name "*.tar.xz" -exec tar --xz -xf {} \; &&
            find "${base_dir}" -name "*.tar.zst" -exec tar --zstd -xf {} \; &&
            rm ".MTREE" &&
            rm ".PKGINFO" &&
            cd .. ) || AC_MSG_ERROR([Could not extract Windows toolchains.])

            mv "inplace/${tarball_mingw_dir}" inplace/mingw &&
            touch inplace/mingw
            AC_MSG_NOTICE([In-tree MingW-w64 tree created])
        fi
    }

    # See Note [How we configure the bundled windows toolchain]
    # and Note [tooldir: How GHC finds mingw on Windows]
    test -d inplace || mkdir inplace

    # NB. Download and extract the MingW-w64 distribution if required
    set_up_tarballs

])

# Set up the environment variables
# $1 The actual location of the windows toolchain (before install)
# $2 the location that the windows toolchain will be installed in relative to the libdir
AC_DEFUN([FP_SETUP_WINDOWS_TOOLCHAIN],[

    # N.B. The parameters which get plopped in the `settings` file used by the
    # resulting compiler are computed in `FP_SETTINGS`. Specifically, we use
    # $$topdir-relative paths instead of fullpaths to the toolchain, by replacing
    # occurrences of $hardtop/inplace/mingw with $$tooldir/mingw

    mingw_prefix="$1"
    mingw_install_prefix="$2"

    # Our Windows toolchain is based around Clang and LLD. We use compiler-rt
    # for the runtime, libc++ and libc++abi for the C++ standard library
    # implementation, and libunwind for C++ unwinding.
    mingwbin="$mingw_prefix/bin/"
    mingwlib="$mingw_prefix/lib"
    mingwinclude="$mingw_prefix/include"
    mingw_mingw32_lib="$mingw_prefix/x86_64-w64-mingw32/lib"

    CC="${mingwbin}clang.exe"
    CXX="${mingwbin}clang++.exe"

    # Signal that we are linking against UCRT with the _UCRT macro. This is
    # necessary to ensure correct behavior when MinGW-w64 headers are in the
    # header include path (#22159).
    cflags="--rtlib=compiler-rt -D_UCRT"
    CFLAGS="$cflags -I$mingwinclude"
    CONF_CC_OPTS_STAGE1="$cflags -I$mingwinclude"
    CONF_CC_OPTS_STAGE2="$cflags -I$mingwinclude"

    cxxflags=""
    CXXFLAGS="$cxxflags -I$mingwinclude"
    CONF_CXX_OPTS_STAGE1="$cxxflags -I$mingwinclude"
    CONF_CXX_OPTS_STAGE2="$cxxflags -I$mingwinclude"

    CONF_CPP_OPTS_STAGE1="$CONF_CPP_OPTS_STAGE1 -I$mingwinclude"
    CONF_CPP_OPTS_STAGE2="$CONF_CPP_OPTS_STAGE2 -I$mingwinclude"

    HaskellCPPArgs="$HaskellCPPArgs -I$mingwinclude"
    JavaScriptCPPCmd="$JavaScriptCPPCmd -I$mingwinclude"
    CmmCPPArgs="$CmmCPPArgs -I$mingwinclude"

    CONF_GCC_LINKER_OPTS_STAGE1="-fuse-ld=lld $cflags -L$mingwlib -L$mingw_mingw32_lib"
    CONF_GCC_LINKER_OPTS_STAGE2="-fuse-ld=lld $cflags -L$mingwlib -L$mingw_mingw32_lib"

    # N.BOn Windows we can't easily dynamically-link against libc++ since there is
    # no RPATH support, meaning that the loader will have no way of finding our
    # libc++.dll
    CXX_STD_LIB_LIBS=":libc++.a :libc++abi.a"
    CXX_STD_LIB_LIB_DIRS="\$topdir"

    LD="${mingwbin}ld.lld.exe"
    NM="${mingwbin}llvm-nm.exe"
    AR="${mingwbin}llvm-ar.exe"
    RANLIB="${mingwbin}llvm-ranlib.exe"
    OBJDUMP="${mingwbin}llvm-objdump.exe"
    DLLTOOL="${mingwbin}llvm-dlltool.exe"
    WindresCmd="${mingwbin}llvm-windres.exe"

    # N.B. LLD does not support -r
    MergeObjsCmd=""
    MergeObjsArgs=""
    AC_PATH_PROG([Genlib],[genlib])


    dnl We override the USER_* flags here since the user delegated
    dnl configuration to the bundled windows toolchain, and these are the
    dnl options required by the bundled windows toolchain.
    USER_CFLAGS="$CFLAGS"
    USER_CPP_ARGS="$CONF_CPP_OPTS_STAGE2"
    USER_CXXFLAGS="$CXXFLAGS"
    USER_HS_CPP_ARGS="$HaskellCPPArgs"
    USER_LDFLAGS="$CONF_GCC_LINKER_OPTS_STAGE2"
    USER_JS_CPP_ARGS="$JavaScriptCPPArgs"
    USER_CMM_CPP_ARGS="$CmmCPPArgs"
])
