AC_DEFUN([FP_SETUP_WINDOWS_TOOLCHAIN],[
    # Find the mingw-w64 archive file to extract.
    if test "$HostArch" = "i386"
    then
        mingw_arch="i686"
        tarball_dest_dir="mingw-w64/i686"
        tarball_mingw_dir="clang32"
    else
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

    # See Note [tooldir: How GHC finds mingw on Windows]
    test -d inplace || mkdir inplace

    # NB. Download and extract the MingW-w64 distribution if required
    set_up_tarballs

    # N.B. The parameters which get plopped in the `settings` file used by the
    # resulting compiler are computed in `FP_SETTINGS`.

    # Our Windows toolchain is based around Clang and LLD. We use compiler-rt
    # for the runtime, libc++ and libc++abi for the C++ standard library
    # implementation, and libunwind for C++ unwinding.
    mingwbin="$hardtop/inplace/mingw/bin/"
    mingwlib="$hardtop/inplace/mingw/lib/"

    # TODO
    CC="${mingwbin}clang.exe"
    CXX="${mingwbin}clang++.exe"

    # Signal that we are linking against UCRT with the _UCRT macro. This is
    # necessary to ensure correct behavior when MinGW-w64 headers are in the
    # header include path (#22159).
    cflags="--rtlib=compiler-rt -D_UCRT"
    CFLAGS="$cflags"
    CONF_CC_OPTS_STAGE1="$cflags"
    CONF_CC_OPTS_STAGE2="$cflags"

    cxxflags=""
    CXXFLAGS="$cxxflags"
    CONF_CXX_OPTS_STAGE1="$cxxflags"
    CONF_CXX_OPTS_STAGE2="$cxxflags"

    CONF_GCC_LINKER_OPTS_STAGE1="-fuse-ld=lld $cflags"
    CONF_GCC_LINKER_OPTS_STAGE2="-fuse-ld=lld $cflags"

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

    # N.B. LLD does not support -r
    MergeObjsCmd=""
    MergeObjsArgs=""
    AC_PATH_PROG([Genlib],[genlib])
])
