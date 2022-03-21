AC_DEFUN([FP_SETUP_WINDOWS_TOOLCHAIN],[
    # Find the mingw-w64 7z file to extract.
    # NB. If you update the tarballs to a new version of gcc, don't
    # forget to tweak the paths in driver/gcc/gcc.c.
    if test "$HostArch" = "i386"
    then
        mingw_arch="i686"
        tarball_dest_dir="mingw-w64/i686"
        tarball_mingw_dir="mingw32"
    else
        mingw_arch="x86_64"
        tarball_dest_dir="mingw-w64/x86_64"
        tarball_mingw_dir="mingw64"
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

            # NB. Now since the GCC is hardcoded to use /mingw32 we need to
            # make a wrapper around it to give it the proper paths
            mv inplace/mingw/bin/gcc.exe inplace/mingw/bin/realgcc.exe
            PATH=`pwd`/inplace/mingw/bin:$PATH
            inplace/mingw/bin/realgcc.exe driver/gcc/gcc.c driver/utils/cwrapper.c driver/utils/getLocation.c -Idriver/utils -o inplace/mingw/bin/gcc.exe

            AC_MSG_NOTICE([In-tree MingW-w64 tree created])
        fi
    }

    # See Note [tooldir: How GHC finds mingw on Windows]
    test -d inplace || mkdir inplace

    # NB. Download and extract the MingW-w64 distribution if required
    set_up_tarballs

    mingwbin="$hardtop/inplace/mingw/bin/"
    CC="${mingwbin}clang.exe"
    CFLAGS="-fuse-ld=lld --rtlib=compiler-rt"
    CONF_CC_OPTS_STAGE1="--rtlib=compiler-rt"
    CONF_CC_OPTS_STAGE2="--rtlib=compiler-rt"
    CONF_GCC_LINKER_OPTS_STAGE1="-fuse-ld=lld --rtlib=compiler-rt"
    CONF_GCC_LINKER_OPTS_STAGE2="-fuse-ld=lld --rtlib=compiler-rt"
    LD="${mingwbin}ld.lld.exe"
    NM="${mingwbin}llvm-nm.exe"
    AR="${mingwbin}llvm-ar.exe"
    RANLIB="${mingwbin}llvm-ranlib.exe"
    OBJDUMP="${mingwbin}llvm-objdump.exe"
    DLLTOOL="${mingwbin}llvm-dlltool.exe"
    MergeObjsCmd="${mingwbin}ld.bfd.exe"
    MergeObjsArgs="-r --oformat=pe-bigobj-x86-64"
    AC_PATH_PROG([Genlib],[genlib])
])