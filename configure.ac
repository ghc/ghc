dnl == autoconf source for the Glasgow FP tools ==
dnl (run "grep '^dnl \*' configure.ac | sed -e 's/dnl / /g; s/\*\*/   +/g;'"
dnl  (or some such) to see the outline of this file)
dnl
#
# (c) The University of Glasgow 1994-2012
#
# Configure script template for GHC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([The Glorious Glasgow Haskell Compilation System], [8.9.0], [glasgow-haskell-bugs@haskell.org], [ghc-AC_PACKAGE_VERSION])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=NO}

# The primary version (e.g. 7.5, 7.4.1) is set in the AC_INIT line
# above.  If this is not a released version, then we will append the
# date to the version number (e.g. 7.4.20111220).  The date is
# constructed by finding the date of the most recent patch in the
# git repository.  If this is a source distribution (not a git
# checkout), then we ship a file 'VERSION' containing the full version
# when the source distribution was created.

dnl this makes sure `./configure --target=<cross-compile-target>`
dnl works as expected, since we're slightly modifying how Autoconf
dnl interprets build/host/target and how this interacts with $CC tests
dnl TODO use CC_FOR_TARGET and CC_FOR_BUILD instead, and get rid of this hack.
test -n "$target_alias" && ac_tool_prefix=$target_alias-

dnl ----------------------------------------------------------
dnl ** Find unixy sort and find commands,
dnl ** which are needed by FP_SETUP_PROJECT_VERSION

dnl ** Find find command (for Win32's benefit)
FP_PROG_FIND
FP_PROG_SORT

dnl ----------------------------------------------------------
FP_SETUP_PROJECT_VERSION

# Hmmm, we fix the RPM release number to 1 here... Is this convenient?
AC_SUBST([release], [1])

# First off, a distrib sanity check..
AC_CONFIG_SRCDIR([mk/config.mk.in])

dnl * We require autoconf version 2.69 due to
dnl   https://bugs.ruby-lang.org/issues/8179. Also see #14910.
dnl * We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl * We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
dnl * Using autoconf 2.59 started to give nonsense like this
dnl     #define SIZEOF_CHAR 0
dnl   recently.
AC_PREREQ([2.69])

# No, semi-sadly, we don't do `--srcdir'...
if test x"$srcdir" != 'x.' ; then
    echo "This configuration does not support the \`--srcdir' option.."
    exit 1
fi

dnl --------------------------------------------------------------
dnl * Project specific configuration options
dnl --------------------------------------------------------------
dnl What follows is a bunch of options that can either be configured
dnl through command line options to the configure script or by
dnl supplying defns in the build tree's mk/build.mk. Having the option to
dnl use either is considered a Feature.

dnl ** What command to use to compile compiler sources ?
dnl --------------------------------------------------------------

AC_ARG_VAR(GHC,[Use as the path to GHC [default=autodetect]])
AC_PATH_PROG([GHC], [ghc])
AC_ARG_WITH([ghc],
        AS_HELP_STRING([--with-ghc=ARG], [Use ARG as the path to ghc (obsolete, use GHC=ARG instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-ghc=$withval is obsolete (use './configure GHC=$withval' or 'GHC=$withval ./configure' instead)]))
AC_SUBST(WithGhc,$GHC)

AC_ARG_ENABLE(bootstrap-with-devel-snapshot,
[AC_HELP_STRING([--enable-bootstrap-with-devel-snapshot],
                [Allow bootstrapping using a development snapshot of GHC.  This is not guaranteed to work.])],
        EnableBootstrapWithDevelSnaphost=YES,
        EnableBootstrapWithDevelSnaphost=NO
)

AC_ARG_ENABLE(tarballs-autodownload,
[AC_HELP_STRING([--enable-tarballs-autodownload],
                [Automatically download Windows distribution binaries if needed.])],
        TarballsAutodownload=YES,
        TarballsAutodownload=NO
)

AC_ARG_ENABLE(distro-toolchain,
[AC_HELP_STRING([--enable-distro-toolchain],
                [Do not use bundled Windows toolchain binaries.])],
        EnableDistroToolchain=YES,
        EnableDistroToolchain=NO
)

if test "$EnableDistroToolchain" = "YES"; then
  TarballsAutodownload=NO
fi

dnl CC_STAGE0 is like the "previous" variable CC (inherited by CC_STAGE[123])
dnl but instead used by stage0 for bootstrapping stage1
AC_ARG_VAR(CC_STAGE0, [C compiler command (bootstrap)])

if test "$WithGhc" != ""; then
  FPTOOLS_GHC_VERSION([GhcVersion], [GhcMajVersion], [GhcMinVersion], [GhcPatchLevel])dnl

  if test "$GhcMajVersion" = "unknown" || test "$GhcMinVersion" = "unknown"; then
     AC_MSG_ERROR([Cannot determine the version of $WithGhc.  Is it really GHC?])
  fi

  AC_SUBST(GhcVersion)dnl
  AC_SUBST(GhcMajVersion)dnl
  AC_SUBST(GhcMinVersion)dnl
  AC_SUBST(GhcPatchLevel)dnl
  GhcMinVersion2=`echo "$GhcMinVersion" | sed 's/^\\(.\\)$/0\\1/'`
  GhcCanonVersion="$GhcMajVersion$GhcMinVersion2"

  dnl infer CC_STAGE0 from `ghc --info` unless explicitly set by user
  if test -z "$CC_STAGE0"; then
    BOOTSTRAPPING_GHC_INFO_FIELD([CC_STAGE0],[C compiler command])
  fi

  BOOTSTRAPPING_GHC_INFO_FIELD([LD_STAGE0],[ld command])
  BOOTSTRAPPING_GHC_INFO_FIELD([AR_STAGE0],[ar command])
  BOOTSTRAPPING_GHC_INFO_FIELD([AR_OPTS_STAGE0],[ar flags])
  BOOTSTRAPPING_GHC_INFO_FIELD([ArSupportsAtFile_STAGE0],[ar supports at file])
fi

dnl ** Must have GHC to build GHC
if test "$WithGhc" = ""
then
    AC_MSG_ERROR([GHC is required.])
fi
FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[8.6],
    [AC_MSG_ERROR([GHC version 8.6 or later is required to compile GHC.])])

if test `expr $GhcMinVersion % 2` = "1"
then
    if test "$EnableBootstrapWithDevelSnaphost" = "NO"
    then
        AC_MSG_ERROR([
     $WithGhc is a development snapshot of GHC, version $GhcVersion.
     Bootstrapping using this version of GHC is not supported, and may not
     work.  Use --enable-bootstrap-with-devel-snapshot to try it anyway,
     or 'GHC=' to specify a different GHC to use.])
    fi
fi

# GHC is passed to Cabal, so we need a native path
if test "${WithGhc}" != ""
then
    ghc_host_os=`"${WithGhc}" +RTS --info | grep 'Host OS' | sed -e 's/.*, "//' -e 's/")//'`

    if test "$ghc_host_os" = "mingw32"
    then
        if test "${OSTYPE}" = "msys"
        then
            WithGhc=`echo "${WithGhc}" | sed "s#^/\([a-zA-Z]\)/#\1:/#"`
        else
            # Canonicalise to <drive>:/path/to/ghc
            WithGhc=`cygpath -m "${WithGhc}"`
        fi
        echo "GHC path canonicalised to: ${WithGhc}"
    fi
fi
AC_SUBST([WithGhc])

dnl ** Without optimization some INLINE trickery fails for GHCi
SRC_CC_OPTS="-O"

dnl--------------------------------------------------------------------
dnl * Choose host(/target/build) platform
dnl--------------------------------------------------------------------
dnl If we aren't explicitly told what values to use with configure flags,
dnl we ask the bootstrapping compiler what platform it is for

if test "${WithGhc}" != ""
then
        bootstrap_host=`"${WithGhc}" +RTS --info | grep '^ ,("Host platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
        bootstrap_target=`"${WithGhc}" +RTS --info | grep '^ ,("Target platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
        if test "$bootstrap_host" != "$bootstrap_target"
        then
                echo "Bootstrapping GHC is a cross compiler. This probably isn't going to work"
        fi
fi

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

FPTOOLS_SET_PLATFORMS_VARS

# Verify that the installed (bootstrap) GHC is capable of generating
# code for the requested build platform.
if test "$BuildPlatform" != "$bootstrap_target"
then
    echo "This GHC (${WithGhc}) does not generate code for the build platform"
    echo "   GHC target platform    : $bootstrap_target"
    echo "   Desired build platform : $BuildPlatform"
    exit 1
fi

# Testing if we shall enable shared libs support on Solaris.
# Anything older than SunOS 5.11 aka Solaris 11 (Express) is broken.

SOLARIS_BROKEN_SHLD=NO

case $host in
     i386-*-solaris2)
     # here we go with the test
     MINOR=`uname -r|cut -d '.' -f 2-`
     if test "$MINOR" -lt "11"; then
         SOLARIS_BROKEN_SHLD=YES
     fi
     ;;
esac

AC_SUBST(SOLARIS_BROKEN_SHLD)

dnl ** Do an unregisterised build?
dnl --------------------------------------------------------------
case "$TargetArch" in
    i386|x86_64|powerpc|powerpc64|powerpc64le|arm|aarch64)
        UnregisterisedDefault=NO
        ;;
    *)
        UnregisterisedDefault=YES
        ;;
esac
AC_ARG_ENABLE(unregisterised,
[AC_HELP_STRING([--enable-unregisterised],
[Build an unregisterised compiler (enabled by default on platforms without registerised support) [default="$UnregisterisedDefault"]])],
[ if test x"$enableval" = x"yes"; then
        Unregisterised=YES
  else
        Unregisterised=NO
  fi
],
[Unregisterised="$UnregisterisedDefault"]
)
AC_SUBST(Unregisterised)

dnl ** Do a build with tables next to code?
dnl
dnl Whether the target architecture supports placing info tables
dnl directly before the entry code (see TABLES_NEXT_TO_CODE in the RTS).
dnl Whether we actually compile for TABLES_NEXT_TO_CODE depends on
dnl whether we're building unregisterised code or not, which may be
dnl decided by options to the compiler later.
dnl
dnl See https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects#tables_next_to_code
dnl --------------------------------------------------------------
case "$Unregisterised" in
    NO)
        case "$TargetArch" in
            ia64|powerpc64|powerpc64le)
                TablesNextToCodeDefault=NO
                ;;
            *)
                TablesNextToCodeDefault=YES
                ;;
        esac
        ;;
    YES)
        TablesNextToCodeDefault=NO
        ;;
esac
AC_ARG_ENABLE(tables-next-to-code,
[AC_HELP_STRING([--enable-tables-next-to-code],
[Build a tables-next-to-code compiler (enabled by default on platforms without registerised support) [default="$TablesNextToCodeDefault"]])],
[ if test x"$enableval" = x"yes"; then
        TablesNextToCode=YES
        AC_DEFINE([TABLES_NEXT_TO_CODE], [1], [Define to 1 if info tables are layed out next to code])
  else
        TablesNextToCode=NO
  fi
],
[TablesNextToCode="$TablesNextToCodeDefault"]
)

fail() {
    echo >&2
    echo "$1" >&2
    exit 1
}

if test "$TablesNextToCodeDefault" = "NO" && test "$TablesNextToCode" = "YES"; then
    fail "Error: tables next to code was requested but is not supported"
fi

AC_SUBST(TablesNextToCode)

# Requires FPTOOLS_SET_PLATFORM_VARS to be run first.
FP_FIND_ROOT

if test "$HostOS" = "mingw32"
then
    # Find the mingw-w64 7z file to extract.
    # NB. If you update the tarballs to a new version of gcc, don't
    # forget to tweak the paths in driver/gcc/gcc.c.
    if test "$HostArch" = "i386"
    then
        mingw_arch="i686"
        tarball_dest_dir="mingw-w64/x86"
        tarball_mingw_dir="mingw32"
    else
        mingw_arch="x86_64"
        tarball_dest_dir="mingw-w64/x86_64"
        tarball_mingw_dir="mingw64"
    fi
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
    mk/get-win32-tarballs.sh $action $HostArch > missing-win32-tarballs
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
           echo "  * run mk/get-win32-tarballs.sh download ${HostArch}"
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
        find "${base_dir}" -name "*.tar.xz" -exec tar xfJ {} \; &&
        rm ".MTREE" &&
        rm ".PKGINFO" &&
        cd .. ) || fail "Error: Could not extract Windows toolchains."

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

if test "$HostOS" = "mingw32" -a "$EnableDistroToolchain" = "NO"
then
    test -d inplace || mkdir inplace

    # NB. Download and extract the MingW-w64 distribution if required
    set_up_tarballs

    mingwbin="$hardtop/inplace/mingw/bin/"
    CC="${mingwbin}gcc.exe"
    LD="${mingwbin}ld.exe"
    NM="${mingwbin}nm.exe"
    RANLIB="${mingwbin}ranlib.exe"
    OBJDUMP="${mingwbin}objdump.exe"
    fp_prog_ar="${mingwbin}ar.exe"

    AC_PATH_PROG([Genlib],[genlib])
fi

# We don't want to bundle a MinGW-w64 toolchain
# So we have to find these individual tools.
if test "$EnableDistroToolchain" = "YES"
then
    # Ideally should use AC_CHECK_TARGET_TOOL but our triples
    # are screwed up. Configure doesn't think they're ever equal and
    # so never tried without the prefix.
    AC_PATH_PROG([CC],[gcc], [clang])
    AC_PATH_PROG([NM],[nm])
    AC_PATH_PROG([AR],[ar])
    AC_PATH_PROG([RANLIB],[ranlib])
    AC_PATH_PROG([OBJDUMP],[objdump])
    AC_PATH_PROG([DllWrap],[dllwrap])
    AC_PATH_PROG([Windres],[windres])
    AC_PATH_PROG([Genlib],[genlib])
else
    AC_CHECK_TARGET_TOOL([Windres],[windres])
    AC_CHECK_TARGET_TOOL([DllWrap],[dllwrap])
    AC_CHECK_TARGET_TOOL([OBJDUMP],[objdump])
fi

DllWrapCmd="$DllWrap"
WindresCmd="$Windres"

HAVE_GENLIB=False
if test "$HostOS" = "mingw32"
then
    if test "$Genlib" != ""; then
        GenlibCmd="$(cygpath -m $Genlib)"
        HAVE_GENLIB=True
    fi
fi

AC_SUBST([DllWrapCmd])
AC_SUBST([WindresCmd])
AC_SUBST([GenlibCmd])
AC_SUBST([HAVE_GENLIB])

FP_ICONV
FP_GMP
FP_CURSES

XCODE_VERSION()


dnl ** Building a cross compiler?
dnl --------------------------------------------------------------
CrossCompiling=NO
# If 'host' and 'target' differ, then this means we are building a cross-compiler.
if test "$TargetPlatform" != "$HostPlatform" ; then
    CrossCompiling=YES
    cross_compiling=yes   # This tells configure that it can accept just 'target',
                          # otherwise you get
                          #   configure: error: cannot run C compiled programs.
                          #   If you meant to cross compile, use `--host'.
fi
if test "$BuildPlatform" != "$HostPlatform" ; then
   AC_MSG_ERROR([
You've selected:

  BUILD:  $BuildPlatform   (the architecture we're building on)
  HOST:   $HostPlatform    (the architecture the compiler we're building will execute on)
  TARGET: $TargetPlatform  (the architecture the compiler we're building will produce code for)

BUILD must equal HOST; that is, we do not support building GHC itself
with a cross-compiler.  To cross-compile GHC itself, set TARGET: stage
1 will be a cross-compiler, and stage 2 will be the cross-compiled
GHC.
])
fi
# Despite its similarity in name to TargetPlatform, TargetPlatformFull is used
# in calls to subproject configure scripts and thus must be set to the autoconf
# triple, not the normalized GHC triple that TargetPlatform is set to.
#
# We use the non-canonicalized triple, target_alias, here since the subproject
# configure scripts will use this triple to construct the names of the toolchain
# executables. If we instead passed down the triple produced by
# AC_CANONICAL_TARGET then it may look for the target toolchain under the wrong
# name (this is a known problem in the case of the Android NDK, which has
# slightly odd triples).
#
# It may be better to just do away with the GHC triples altogether. This would
# all be taken care of for us if we configured the subprojects using
# AC_CONFIG_DIR, but unfortunately Cabal needs to be the one to do the
# configuration.
#
# We also use non-canonicalized triple when install stage1 crosscompiler
if test -z "${target_alias}"
then
  # --target wasn't given; use result from AC_CANONICAL_TARGET
  TargetPlatformFull="${target}"
else
  TargetPlatformFull="${target_alias}"
fi
if test "$CrossCompiling" = "YES"
then
  # Use value passed by user from --target=
  CrossCompilePrefix="${TargetPlatformFull}-"
else
  CrossCompilePrefix=""
fi
AC_SUBST(CrossCompiling)
AC_SUBST(CrossCompilePrefix)
AC_SUBST(TargetPlatformFull)
AC_SUBST(EnableDistroToolchain)

dnl ** Which gcc to use?
dnl --------------------------------------------------------------

AC_ARG_WITH([gcc],
        AS_HELP_STRING([--with-gcc=ARG], [Use ARG as the path to gcc (obsolete, use CC=ARG instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-gcc=$withval is obsolete (use './configure CC=$withval' or 'CC=$withval ./configure' instead)]))

AC_ARG_WITH([clang],
        AS_HELP_STRING([--with-clang=ARG], [Use ARG as the path to gcc (obsolete, use CC=ARG instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-clang=$withval is obsolete (use './configure CC=$withval' or 'CC=$withval ./configure' instead)]))

dnl detect compiler (prefer gcc over clang) and set $CC (unless CC already set),
dnl later CC is copied to CC_STAGE{1,2,3}
AC_PROG_CC([cc gcc clang])

MAYBE_OVERRIDE_STAGE0([ar],[AR_STAGE0])

dnl make extensions visible to allow feature-tests to detect them lateron
AC_USE_SYSTEM_EXTENSIONS

dnl ** figure out how to invoke the C preprocessor (i.e. `gcc -E`)
AC_PROG_CPP

# --with-hs-cpp/--with-hs-cpp-flags
FP_CPP_CMD_WITH_ARGS(HaskellCPPCmd, HaskellCPPArgs)
AC_SUBST([HaskellCPPCmd])
AC_SUBST([HaskellCPPArgs])

FP_SET_CFLAGS_C99([CC],[CFLAGS],[CPPFLAGS])
FP_SET_CFLAGS_C99([CC_STAGE0],[CONF_CC_OPTS_STAGE0],[CONF_CPP_OPTS_STAGE0])
FP_SET_CFLAGS_C99([CC],[CONF_CC_OPTS_STAGE1],[CONF_CPP_OPTS_STAGE1])
FP_SET_CFLAGS_C99([CC],[CONF_CC_OPTS_STAGE2],[CONF_CPP_OPTS_STAGE2])

dnl ** Which ld to use
dnl --------------------------------------------------------------
AC_ARG_VAR(LD,[Use as the path to ld. See also --disable-ld-override.])
FIND_LD([$target],[GccUseLdOpt])
CONF_GCC_LINKER_OPTS_STAGE1="$CONF_GCC_LINKER_OPTS_STAGE1 $GccUseLdOpt"
CONF_GCC_LINKER_OPTS_STAGE2="$CONF_GCC_LINKER_OPTS_STAGE2 $GccUseLdOpt"
LdCmd="$LD"
LdNoGoldCmd="$LD_NO_GOLD"
CFLAGS="$CFLAGS $GccUseLdOpt"
AC_SUBST([LdCmd])
AC_SUBST([LdNoGoldCmd])

FP_PROG_LD_IS_GNU
FP_PROG_LD_BUILD_ID
FP_PROG_LD_NO_COMPACT_UNWIND
FP_PROG_LD_FILELIST

dnl ** Which nm to use?
dnl --------------------------------------------------------------
if test "$HostOS" != "mingw32"; then
    AC_CHECK_TARGET_TOOL([NM], [nm])
    if test "$NM" = ":"; then
        AC_MSG_ERROR([cannot find nm in your PATH])
    fi
fi
NmCmd="$NM"
AC_SUBST([NmCmd])

if test "$TargetOS_CPP" = "darwin"
then
    AC_MSG_CHECKING(whether nm program is broken)
    # Some versions of XCode ship a broken version of `nm`. Detect and work
    # around this issue. See : https://gitlab.haskell.org/ghc/ghc/issues/11744
    nmver=$(${NM} --version | grep version | sed 's/ //g')
    case "$nmver" in
        LLVMversion7.3.0|LLVMversion7.3.1)
            AC_MSG_RESULT(yes)
            echo "The detected nm program is broken."
            echo
            echo "See: https://gitlab.haskell.org/ghc/ghc/issues/11744"
            echo
            echo "Try re-running configure with:"
            echo
            echo '   NM=$(xcrun --find nm-classic) ./configure'
            echo
            exit 1
            ;;
        *)
            AC_MSG_RESULT(no)
            ;;
        esac
fi

dnl ** Which objdump to use?
dnl --------------------------------------------------------------
dnl Note: we may not have objdump on OS X, and we only need it on
dnl Windows (for DLL checks), OpenBSD, and AIX
case $HostOS_CPP in
  cygwin32|mingw32|openbsd|aix)
    AC_CHECK_TARGET_TOOL([OBJDUMP], [objdump])
    ;;
esac

ObjdumpCmd="$OBJDUMP"
AC_SUBST([ObjdumpCmd])

dnl ** Which ranlib to use?
dnl --------------------------------------------------------------
AC_PROG_RANLIB
if test "$RANLIB" = ":"; then
    AC_MSG_ERROR([cannot find ranlib in your PATH])
fi
RanlibCmd="$RANLIB"
AC_SUBST([RanlibCmd])

dnl ** which strip to use?
dnl --------------------------------------------------------------
AC_CHECK_TARGET_TOOL([STRIP], [strip])
StripCmd="$STRIP"
AC_SUBST([StripCmd])


dnl ** which libtool to use?
dnl --------------------------------------------------------------
# The host normalization on Windows breaks autoconf, it no longer
# thinks that target == host so it never checks the unqualified
# tools for Windows. See #14274.
if test "$HostOS" = "mingw32"
then
    AC_PATH_PROG([LIBTOOL],[libtool])
    LibtoolCmd="$(cygpath -m $LIBTOOL)"
else
    AC_CHECK_TARGET_TOOL([LIBTOOL], [libtool])
    LibtoolCmd="$LIBTOOL"
fi
AC_SUBST([LibtoolCmd])

# Here is where we re-target which specific version of the LLVM
# tools we are looking for. In the past, GHC supported a number of
# versions of LLVM simultaneously, but that stopped working around
# 3.5/3.6 release of LLVM.
LlvmVersion=7
AC_SUBST([LlvmVersion])
sUPPORTED_LLVM_VERSION=$(echo \($LlvmVersion\) | sed 's/\./,/')
AC_DEFINE_UNQUOTED([sUPPORTED_LLVM_VERSION], ${sUPPORTED_LLVM_VERSION}, [The supported LLVM version number])

dnl ** Which LLVM clang to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(CLANG,[Use as the path to clang [default=autodetect]])
AC_CHECK_TARGET_TOOL([CLANG], [clang])
ClangCmd="$CLANG"
AC_SUBST([ClangCmd])

dnl ** Which LLVM llc to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(LLC,[Use as the path to LLVM's llc [default=autodetect]])
FIND_LLVM_PROG([LLC], [llc], [$LlvmVersion])
LlcCmd="$LLC"
AC_SUBST([LlcCmd])

dnl ** Which LLVM opt to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(OPT,[Use as the path to LLVM's opt [default=autodetect]])
FIND_LLVM_PROG([OPT], [opt], [$LlvmVersion])
OptCmd="$OPT"
AC_SUBST([OptCmd])

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------

dnl ** Copy the files from the "fs" utility into the right folders.
dnl --------------------------------------------------------------
AC_MSG_NOTICE([Creating links for in-tree file handling routines.])
ln -f utils/fs/fs.* utils/lndir/
ln -f utils/fs/fs.* utils/unlit/
ln -f utils/fs/fs.* rts/
ln -f utils/fs/fs.h libraries/base/include/
ln -f utils/fs/fs.c libraries/base/cbits/
AC_MSG_NOTICE([Routines in place. Packages can now be build normally.])

dnl --------------------------------------------------------------
dnl ** Can the unix package be built?
dnl --------------------------------------------------------------

dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** look for GCC and find out which version
dnl     Figure out which C compiler to use.  Gcc is preferred.
dnl     If gcc, make sure it's at least 3.0
dnl
FP_GCC_VERSION

dnl ** look to see if we have a C compiler using an llvm back end.
dnl
FP_CC_LLVM_BACKEND

FPTOOLS_SET_C_LD_FLAGS([target],[CFLAGS],[LDFLAGS],[IGNORE_LINKER_LD_FLAGS],[CPPFLAGS])
FPTOOLS_SET_C_LD_FLAGS([build],[CONF_CC_OPTS_STAGE0],[CONF_GCC_LINKER_OPTS_STAGE0],[CONF_LD_LINKER_OPTS_STAGE0],[CONF_CPP_OPTS_STAGE0])
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE1],[CONF_GCC_LINKER_OPTS_STAGE1],[CONF_LD_LINKER_OPTS_STAGE1],[CONF_CPP_OPTS_STAGE1])
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE2],[CONF_GCC_LINKER_OPTS_STAGE2],[CONF_LD_LINKER_OPTS_STAGE2],[CONF_CPP_OPTS_STAGE2])
# Stage 3 won't be supported by cross-compilation

# See rules/distdir-way-opts.mk for details.
# Flags passed to the C compiler
AC_SUBST(CONF_CC_OPTS_STAGE0)
AC_SUBST(CONF_CC_OPTS_STAGE1)
AC_SUBST(CONF_CC_OPTS_STAGE2)
# Flags passed to the C compiler when we ask it to link
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE0)
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE1)
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE2)
# Flags passed to the linker when we ask it to link
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE0)
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE1)
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE2)
# Flags passed to the C preprocessor
AC_SUBST(CONF_CPP_OPTS_STAGE0)
AC_SUBST(CONF_CPP_OPTS_STAGE1)
AC_SUBST(CONF_CPP_OPTS_STAGE2)
# Flags passed to the Haskell compiler
AC_SUBST(CONF_HC_OPTS_STAGE0)
AC_SUBST(CONF_HC_OPTS_STAGE1)
AC_SUBST(CONF_HC_OPTS_STAGE2)

dnl ** Set up the variables for the platform in the settings file.
dnl May need to use gcc to find platform details.
dnl --------------------------------------------------------------
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Build])

FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])
AC_SUBST(HaskellHostArch)
AC_SUBST(HaskellHostOs)

dnl ** figure out how to do context diffs
FP_PROG_CONTEXT_DIFF

dnl Let's make sure install-sh is executable here.  If we got it from
dnl a darcs repo, it might not be (see bug #978).
chmod +x install-sh
dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl ** how to invoke `ar' and `ranlib'
FP_PROG_AR_SUPPORTS_ATFILE
FP_PROG_AR_NEEDS_RANLIB

dnl ** Check to see whether ln -s works
AC_PROG_LN_S

FP_SETTINGS

dnl ** Find the path to sed
AC_PATH_PROGS(SedCmd,gsed sed,sed)


dnl ** check for time command
AC_PATH_PROG(TimeCmd,time)

dnl ** check for tar
dnl   if GNU tar is named gtar, look for it first.
AC_PATH_PROGS(TarCmd,gnutar gtar tar,tar)

dnl ** check for compressors
AC_PATH_PROGS(Bzip2Cmd,bzip2, bzip2)
AC_PATH_PROGS(GzipCmd,gzip, gzip)
AC_PATH_PROGS(XzCmd,pxz xz, xz)

dnl ** check for patch
dnl if GNU patch is named gpatch, look for it first
AC_PATH_PROGS(PatchCmd,gpatch patch, patch)

dnl ** check for autoreconf
AC_PATH_PROG(AutoreconfCmd, autoreconf, autoreconf)

dnl ** check for dtrace (currently only implemented for Mac OS X)
AC_ARG_ENABLE(dtrace,
    [AC_HELP_STRING([--enable-dtrace],
        [Enable DTrace])],
    EnableDtrace=$enableval,
    EnableDtrace=yes
)

HaveDtrace=NO

AC_PATH_PROG(DtraceCmd,dtrace)
if test "x$EnableDtrace" = "xyes"; then
  if test -n "$DtraceCmd"; then
    if test "x$TargetOS_CPP-$TargetVendor_CPP" = "xdarwin-apple" \
      -o "x$TargetOS_CPP-$TargetVendor_CPP" = "xfreebsd-portbld" \
      -o "x$TargetOS_CPP-$TargetVendor_CPP" = "xsolaris2-unknown"; then
      HaveDtrace=YES
    fi
  fi
fi
AC_SUBST(HaveDtrace)

AC_PATH_PROG(HSCOLOUR,HsColour)
# HsColour is passed to Cabal, so we need a native path
if test "$HostOS"      = "mingw32" && \
   test "${OSTYPE}"   != "msys"    && \
   test "${HSCOLOUR}" != ""
then
    # Canonicalise to <drive>:/path/to/gcc
    HSCOLOUR=`cygpath -m ${HSCOLOUR}`
fi

dnl ** check for Sphinx toolchain
AC_PATH_PROG(SPHINXBUILD,sphinx-build)
AC_CACHE_CHECK([for version of sphinx-build], fp_cv_sphinx_version,
changequote(, )dnl
[if test -n "$SPHINXBUILD"; then
  fp_cv_sphinx_version=`"$SPHINXBUILD" --version 2>&1 | sed 's/.* v\?\([0-9]\.[0-9]\.[0-9]\)/\1/' | head -n1`;
fi;
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fp_cv_sphinx_version],-lt,1.0.0,
    [AC_MSG_WARN([Sphinx version 1.0.0 or later is required to build documentation]); SPHINXBUILD=;])

dnl ** check for xelatex
AC_PATH_PROG(XELATEX,xelatex)

dnl ** check for makeinfo
AC_PATH_PROG(MAKEINFO,makeinfo)

dnl ** check for ghc-pkg command
FP_PROG_GHC_PKG

dnl ** check for installed happy binary + version

AC_ARG_VAR(HAPPY,[Use as the path to happy [default=autodetect]])
FPTOOLS_HAPPY

dnl ** check for installed alex binary + version

AC_ARG_VAR(ALEX,[Use as the path to alex [default=autodetect]])
FPTOOLS_ALEX

dnl --------------------------------------------------
dnl ### program checking section ends here ###
dnl --------------------------------------------------

dnl ** Documentation
dnl --------------------------------------------------------------
if test -n "$SPHINXBUILD"; then
    BUILD_MAN=YES
    BUILD_SPHINX_HTML=YES
    if test -n "$XELATEX"; then
        BUILD_SPHINX_PDF=YES
    else
        BUILD_SPHINX_PDF=NO
    fi
    if test -n "$MAKEINFO"; then
        BUILD_SPHINX_INFO=YES
    else
        BUILD_SPHINX_INFO=NO
    fi
else
    BUILD_MAN=NO
    BUILD_SPHINX_HTML=NO
    BUILD_SPHINX_PDF=NO
    BUILD_SPHINX_INFO=NO
fi
AC_SUBST(BUILD_MAN)
AC_SUBST(BUILD_SPHINX_HTML)
AC_SUBST(BUILD_SPHINX_PDF)

dnl ** Determine library versions
dnl The packages below should include all packages needed by
dnl doc/users_guide/ghc_config.py.in.
LIBRARY_VERSION(base)
LIBRARY_VERSION(Cabal, Cabal/Cabal)
LIBRARY_VERSION(template-haskell)
LIBRARY_VERSION(parallel)
LIBRARY_VERSION(array)
LIBRARY_VERSION(ghc-prim)
LIBRARY_VERSION(ghc-compact)
LIBRARY_ghc_VERSION="$ProjectVersion"
AC_SUBST(LIBRARY_ghc_VERSION)

if grep '	' compiler/ghc.cabal.in 2>&1 >/dev/null; then
   AC_MSG_ERROR([compiler/ghc.cabal.in contains tab characters; please remove them])
fi

# Create the configuration for the Hadrian build system if it is present
if test -e hadrian/cfg/system.config.in; then
    AC_CONFIG_FILES([hadrian/cfg/system.config])
fi

# We got caught by
#     http://savannah.gnu.org/bugs/index.php?1516
#     $(eval ...) inside conditionals causes errors
# with make 3.80, so warn the user if it looks like they're about to
# try to use it.
# We would use "grep -q" here, but Solaris's grep doesn't support it.
print_make_warning=""
checkMake380() {
    make_ver=`$1 --version 2>&1 | head -1`
    if echo "$make_ver" | grep 'GNU Make 3\.80' > /dev/null
    then
        print_make_warning="true"
    fi
    if echo "$make_ver" | grep 'GNU Make' > /dev/null
    then
        MakeCmd=$1
        AC_SUBST(MakeCmd)
    fi
}

checkMake380 make
checkMake380 gmake

AC_CONFIG_FILES([mk/config.mk mk/install.mk mk/project.mk rts/rts.cabal compiler/ghc.cabal ghc/ghc-bin.cabal utils/iserv/iserv.cabal utils/iserv-proxy/iserv-proxy.cabal utils/remote-iserv/remote-iserv.cabal utils/runghc/runghc.cabal utils/gen-dll/gen-dll.cabal libraries/ghc-boot/ghc-boot.cabal libraries/ghc-boot-th/ghc-boot-th.cabal libraries/ghci/ghci.cabal libraries/ghc-heap/ghc-heap.cabal libraries/libiserv/libiserv.cabal libraries/template-haskell/template-haskell.cabal docs/users_guide/ghc_config.py docs/index.html libraries/prologue.txt distrib/configure.ac])
AC_OUTPUT
[
if test "$print_make_warning" = "true"; then
   echo
   echo "WARNING: It looks like \"$MakeCmd\" is GNU make 3.80."
   echo "This version cannot be used to build GHC."
   echo "Please use GNU make >= 3.81."
fi

echo "
----------------------------------------------------------------------
Configure completed successfully.

   Building GHC version  : $ProjectVersion
          Git commit id  : $ProjectGitCommitId

   Build platform        : $BuildPlatform
   Host platform         : $HostPlatform
   Target platform       : $TargetPlatform
"

echo "\
   Bootstrapping using   : $WithGhc
      which is version   : $GhcVersion
"

if test "x$CC_LLVM_BACKEND" = "x1"; then
    CompilerName="clang        "
else
    CompilerName="gcc          "
fi

echo "\
   Using (for bootstrapping) : $CC_STAGE0
   Using $CompilerName       : $CC
      which is version       : $GccVersion
   Building a cross compiler : $CrossCompiling
   Unregisterised            : $Unregisterised
   TablesNextToCode          : $TablesNextToCode
   hs-cpp       : $HaskellCPPCmd
   hs-cpp-flags : $HaskellCPPArgs
   ar           : $ArCmd
   ld           : $LdCmd
   nm           : $NmCmd
   libtool      : $LibtoolCmd
   objdump      : $ObjdumpCmd
   ranlib       : $RanlibCmd
   windres      : $WindresCmd
   dllwrap      : $DllWrapCmd
   genlib       : $GenlibCmd
   Happy        : $HappyCmd ($HappyVersion)
   Alex         : $AlexCmd ($AlexVersion)
   sphinx-build : $SPHINXBUILD
   xelatex      : $XELATEX
   makeinfo     : $MAKEINFO

   Using LLVM tools
      clang : $ClangCmd
      llc   : $LlcCmd
      opt   : $OptCmd"

if test "$HSCOLOUR" = ""; then
echo "
   HsColour was not found; documentation will not contain source links
"
else
echo "\
   HsColour : $HSCOLOUR
"
fi

echo "\
   Tools to build Sphinx HTML documentation available: $BUILD_SPHINX_HTML
   Tools to build Sphinx PDF documentation available: $BUILD_SPHINX_PDF
   Tools to build Sphinx INFO documentation available: $BUILD_SPHINX_INFO"

echo "----------------------------------------------------------------------
"

echo "\
For a standard build of GHC (fully optimised with profiling), type (g)make.

To make changes to the default build configuration, copy the file
mk/build.mk.sample to mk/build.mk, and edit the settings in there.

For more information on how to configure your GHC build, see
   https://gitlab.haskell.org/ghc/ghc/wikis/building
"]
