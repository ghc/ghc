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

AC_INIT([The Glorious Glasgow Haskell Compilation System], [9.11], [glasgow-haskell-bugs@haskell.org], [ghc-AC_PACKAGE_VERSION])
    # Version on master must be X.Y (not X.Y.Z) for ProjectVersionMunged variable
    # to be useful (cf #19058). However, the version must have three components
    # (X.Y.Z) on stable branches (e.g. ghc-9.2) to ensure that pre-releases are
    # versioned correctly.

AC_CONFIG_MACRO_DIRS([m4])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=NO}

# The primary version (e.g. 7.5, 7.4.1) is set in the AC_INIT line
# above.  If this is not a released version, then we will append the
# date to the version number (e.g. 7.4.20111220).  The date is
# constructed by finding the date of the most recent patch in the
# git repository.  If this is a source distribution (not a git
# checkout), then we ship a file 'VERSION' containing the full version
# when the source distribution was created.

if test ! -f rts/ghcautoconf.h.autoconf.in; then
   echo "rts/ghcautoconf.h.autoconf.in doesn't exist: perhaps you haven't run 'python3 boot'?"
   exit 1
fi

dnl this makes sure `./configure --target=<cross-compile-target>`
dnl works as expected, since we're slightly modifying how Autoconf
dnl interprets build/host/target and how this interacts with $CC tests
test -n "$target_alias" && ac_tool_prefix=$target_alias-

dnl ----------------------------------------------------------
dnl ** Store USER specified environment variables to pass them on to
dnl ** ghc-toolchain (in m4/ghc-toolchain.m4)
USER_CFLAGS="$CFLAGS"
USER_LDFLAGS="$LDFLAGS"
USER_LIBS="$LIBS"
USER_CXXFLAGS="$CXXFLAGS"
dnl The lower-level/not user-facing environment variables that may still be set
dnl by developers such as in ghc-wasm-meta
USER_CONF_CC_OPTS_STAGE2="$CONF_CC_OPTS_STAGE2"
USER_CONF_CXX_OPTS_STAGE2="$CONF_CXX_OPTS_STAGE2"
USER_CONF_GCC_LINKER_OPTS_STAGE2="$CONF_GCC_LINKER_OPTS_STAGE2"

USER_LD="$LD"

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

AC_ARG_VAR(GHC,[Use as the bootstrap GHC. [default=autodetect]])
AC_CHECK_PROG([GHC], [ghc], [ghc])
AC_ARG_WITH([ghc],
        AS_HELP_STRING([--with-ghc=PATH], [Use PATH as the bootstrap ghc (obsolete, use GHC=PATH instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-ghc=$withval is obsolete (use './configure GHC=$withval' instead)]))
AC_SUBST(WithGhc,$GHC)

AC_ARG_ENABLE(bootstrap-with-devel-snapshot,
[AS_HELP_STRING([--enable-bootstrap-with-devel-snapshot],
                [Allow bootstrapping using a development snapshot of GHC.  This is not guaranteed to work.])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableBootstrapWithDevelSnaphost])],
  [EnableBootstrapWithDevelSnaphost=NO]
)

AC_ARG_ENABLE(tarballs-autodownload,
[AS_HELP_STRING([--enable-tarballs-autodownload],
                [Automatically download Windows distribution binaries if needed.])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [TarballsAutodownload])],
  [TarballsAutodownload=NO]
)

AC_ARG_ENABLE(distro-toolchain,
[AS_HELP_STRING([--enable-distro-toolchain],
                [Do not use bundled Windows toolchain binaries.])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableDistroToolchain])],
  [EnableDistroToolchain=NO]
)

if test "$EnableDistroToolchain" = "YES"; then
  TarballsAutodownload=NO
fi

AC_ARG_ENABLE(ghc-toolchain,
[AS_HELP_STRING([--enable-ghc-toolchain],
                [Whether to use the newer ghc-toolchain tool to configure ghc targets])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableGhcToolchain])],
  [EnableGhcToolchain=NO]
)
AC_SUBST([EnableGhcToolchain])

AC_ARG_ENABLE(strict-ghc-toolchain-check,
[AS_HELP_STRING([--enable-strict-ghc-toolchain-check],
                [Whether to raise an error if the output of ghc-toolchain differs from configure])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableStrictGhcToolchainCheck])],
  [EnableStrictGhcToolchainCheck=NO]
)
AC_SUBST([EnableStrictGhcToolchainCheck])

dnl CC_STAGE0, LD_STAGE0, AR_STAGE0 are like the "previous" variable
dnl CC, LD, AR (inherited by CC_STAGE[123], etc.)
dnl but instead used by stage0 for bootstrapping stage1
AC_ARG_VAR(CC_STAGE0, [C compiler command (bootstrap)])
AC_ARG_VAR(LD_STAGE0, [Linker command (bootstrap)])
AC_ARG_VAR(AR_STAGE0, [Archive command (bootstrap)])

dnl RTS ways supplied by the bootstrapping compiler.
AC_ARG_VAR(RTS_WAYS_STAGE0, [RTS ways])

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

  dnl infer {CC,LD,AR}_STAGE0 from `ghc --info` unless explicitly set by user
  if test -z "$CC_STAGE0"; then
    BOOTSTRAPPING_GHC_INFO_FIELD([CC_STAGE0],[C compiler command])
  fi

  if test -z "$LD_STAGE0"; then
    BOOTSTRAPPING_GHC_INFO_FIELD([LD_STAGE0],[ld command])
  fi
  if test -z "$AR_STAGE0"; then
    BOOTSTRAPPING_GHC_INFO_FIELD([AR_STAGE0],[ar command])
  fi
  BOOTSTRAPPING_GHC_INFO_FIELD([AR_OPTS_STAGE0],[ar flags])
  BOOTSTRAPPING_GHC_INFO_FIELD([ArSupportsAtFile_STAGE0],[ar supports at file])
  BOOTSTRAPPING_GHC_INFO_FIELD([ArSupportsDashL_STAGE0],[ar supports -L])
  BOOTSTRAPPING_GHC_INFO_FIELD([SUPPORT_SMP_STAGE0],[Support SMP])
  BOOTSTRAPPING_GHC_INFO_FIELD([RTS_WAYS_STAGE0],[RTS ways])

  dnl Check whether or not the bootstrapping GHC has a threaded RTS. This
  dnl determines whether or not we can have a threaded stage 1.
  dnl See Note [Linking ghc-bin against threaded stage0 RTS] in
  dnl hadrian/src/Settings/Packages.hs for details.
  dnl SMP support which implies a registerised stage0 is also required (see issue 18266)
  if echo ${RTS_WAYS_STAGE0} | tr ' ' '\n' | grep '^thr$' 2>&1 >/dev/null && \
     test "$SUPPORT_SMP_STAGE0" = "YES"
  then
      AC_SUBST(GhcThreadedRts, YES)
  else
      AC_SUBST(GhcThreadedRts, NO)
  fi
fi

dnl ** Must have GHC to build GHC
if test "$WithGhc" = ""
then
    AC_MSG_ERROR([GHC is required.])
fi
MinBootGhcVersion="9.6"
FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[$MinBootGhcVersion],
    [AC_MSG_ERROR([GHC version $MinBootGhcVersion or later is required to compile GHC.])])

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

FP_PROG_SH

# Verify that the installed (bootstrap) GHC is capable of generating
# code for the requested build platform.
if test "$BuildPlatform" != "$bootstrap_target"
then
    echo "This GHC (${WithGhc}) does not generate code for the build platform"
    echo "   GHC target platform    : $bootstrap_target"
    echo "   Desired build platform : $BuildPlatform"
    exit 1
fi

dnl ** Do an unregisterised build?
dnl --------------------------------------------------------------

GHC_UNREGISTERISED
AC_SUBST(Unregisterised)

dnl ** Do a build with tables next to code?
dnl --------------------------------------------------------------

GHC_TABLES_NEXT_TO_CODE
AC_SUBST(TablesNextToCode)

# Requires FPTOOLS_SET_PLATFORMS_VARS to be run first.
FP_FIND_ROOT

# Extract and configure the Windows toolchain
if test "$HostOS" = "mingw32" -a "$EnableDistroToolchain" = "NO"; then
    FP_INSTALL_WINDOWS_TOOLCHAIN
    FP_SETUP_WINDOWS_TOOLCHAIN([$hardtop/inplace/mingw], [$hardtop/inplace/mingw])
else
    AC_PATH_TOOL([CC],[gcc], [clang])
    AC_PATH_TOOL([CXX],[g++], [clang++])
    AC_PATH_TOOL([NM],[nm])
    # N.B. we don't probe for LD here but instead
    # do so in FIND_LD to avoid #21778.
    AC_PATH_TOOL([AR],[ar])
    AC_PATH_TOOL([RANLIB],[ranlib])
    AC_PATH_TOOL([OBJDUMP],[objdump])
    AC_PATH_TOOL([WindresCmd],[windres])
    AC_PATH_TOOL([Genlib],[genlib])

    if test "$HostOS" = "mingw32"; then
        AC_CHECK_TARGET_TOOL([WindresCmd],[windres])
        AC_CHECK_TARGET_TOOL([OBJDUMP],[objdump])

        if test "$Genlib" != ""; then
            GenlibCmd="$(cygpath -m $Genlib)"
        fi
    fi
fi

FP_ICONV
FP_GMP
FP_CURSES

dnl On Windows we force in-tree GMP build until we support dynamic linking
if test "$HostOS" = "mingw32"
then
  GMP_FORCE_INTREE="YES"
fi

dnl ** Building a cross compiler?
dnl --------------------------------------------------------------
CrossCompiling=NO
# If 'host' and 'target' differ, then this means we are building a cross-compiler.
if test "$target" != "$host" ; then
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
AC_SUBST(CrossCompiling)
AC_SUBST(TargetPlatformFull)

dnl ** Which gcc to use?
dnl --------------------------------------------------------------

AC_ARG_WITH([gcc],
        AS_HELP_STRING([--with-gcc=ARG], [Use ARG as the path to gcc (obsolete, use CC=ARG instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-gcc=$withval is obsolete (use './configure CC=$withval' or 'CC=$withval ./configure' instead)]))

AC_ARG_WITH([clang],
        AS_HELP_STRING([--with-clang=ARG], [Use ARG as the path to clang (obsolete, use CC=ARG instead) [default=autodetect]]),
        AC_MSG_ERROR([--with-clang=$withval is obsolete (use './configure CC=$withval' or 'CC=$withval ./configure' instead)]))

dnl detect compiler (prefer gcc over clang) and set $CC (unless CC already set),
dnl later CC is copied to CC_STAGE{1,2,3}
AC_PROG_CC([cc gcc clang])
AC_PROG_CXX([g++ clang++ c++])
# Work around #24324
MOVE_TO_FLAGS([CC],[CFLAGS])
MOVE_TO_FLAGS([CXX],[CXXFLAGS])

MAYBE_OVERRIDE_STAGE0([ar],[AR_STAGE0])

dnl make extensions visible to allow feature-tests to detect them lateron
AC_USE_SYSTEM_EXTENSIONS

# --with-hs-cpp/--with-hs-cpp-flags
FP_HSCPP_CMD_WITH_ARGS(HaskellCPPCmd, HaskellCPPArgs)
AC_SUBST([HaskellCPPCmd])
AC_SUBST([HaskellCPPArgs])

# --with-js-cpp/--with-js-cpp-flags
FP_JSCPP_CMD_WITH_ARGS(JavaScriptCPPCmd, JavaScriptCPPArgs)
AC_SUBST([JavaScriptCPPCmd])
AC_SUBST([JavaScriptCPPArgs])

# --with-cmm-cpp/--with-cmm-cpp-flags
FP_CMM_CPP_CMD_WITH_ARGS([$CC_STAGE0], [CmmCPPCmd_STAGE0], [CmmCPPArgs_STAGE0], [CmmCPPSupportsG0_STAGE0])
AC_SUBST([CmmCPPCmd_STAGE0])
AC_SUBST([CmmCPPArgs_STAGE0])
AC_SUBST([CmmCPPSupportsG0_STAGE0])
FP_CMM_CPP_CMD_WITH_ARGS([$CC], [CmmCPPCmd], [CmmCPPArgs], [CmmCPPSupportsG0])
AC_SUBST([CmmCPPCmd])
AC_SUBST([CmmCPPArgs])
AC_SUBST([CmmCPPSupportsG0])

FP_SET_CFLAGS_C99([CC],[CFLAGS],[CPPFLAGS])
FP_SET_CFLAGS_C99([CC_STAGE0],[CONF_CC_OPTS_STAGE0],[CONF_CPP_OPTS_STAGE0])
FP_SET_CFLAGS_C99([CC],[CONF_CC_OPTS_STAGE1],[CONF_CPP_OPTS_STAGE1])
FP_SET_CFLAGS_C99([CC],[CONF_CC_OPTS_STAGE2],[CONF_CPP_OPTS_STAGE2])

dnl ** Do we have a compatible emsdk version?
dnl --------------------------------------------------------------
EMSDK_VERSION("3.1.20", "", "")

dnl ** Which ld to use
dnl --------------------------------------------------------------
AC_ARG_VAR(LD,[Use as the path to ld. See also --disable-ld-override.])
FIND_LD([$target],[GccUseLdOpt])
FIND_MERGE_OBJECTS()
CONF_GCC_LINKER_OPTS_STAGE1="$CONF_GCC_LINKER_OPTS_STAGE1 $GccUseLdOpt"
CONF_GCC_LINKER_OPTS_STAGE2="$CONF_GCC_LINKER_OPTS_STAGE2 $GccUseLdOpt"
CFLAGS="$CFLAGS $GccUseLdOpt"

FP_PROG_LD_IS_GNU
FP_PROG_LD_NO_COMPACT_UNWIND
FP_PROG_LD_FILELIST
FP_PROG_LD_SINGLE_MODULE


dnl ** Which nm to use?
dnl --------------------------------------------------------------
FP_FIND_NM

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

dnl ** Which otool to use on macOS
dnl --------------------------------------------------------------
AC_CHECK_TARGET_TOOL([OTOOL], [otool])
OtoolCmd="$OTOOL"
AC_SUBST(OtoolCmd)

dnl ** Which install_name_tool to use on macOS
dnl --------------------------------------------------------------
AC_CHECK_TARGET_TOOL([INSTALL_NAME_TOOL], [install_name_tool])
InstallNameToolCmd="$INSTALL_NAME_TOOL"
AC_SUBST(InstallNameToolCmd)

# Here is where we re-target which specific version of the LLVM
# tools we are looking for. In the past, GHC supported a number of
# versions of LLVM simultaneously, but that stopped working around
# 3.5/3.6 release of LLVM.
LlvmMinVersion=13  # inclusive
LlvmMaxVersion=16 # not inclusive
AC_SUBST([LlvmMinVersion])
AC_SUBST([LlvmMaxVersion])

ConfiguredEmsdkVersion="${EmsdkVersion}"
AC_SUBST([ConfiguredEmsdkVersion])

dnl ** Which LLVM llc to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(LLC,[Use as the path to LLVM's llc [default=autodetect]])
FIND_LLVM_PROG([LLC], [llc], [$LlvmMinVersion], [$LlvmMaxVersion])
LlcCmd="$LLC"
AC_SUBST([LlcCmd])

dnl ** Which LLVM opt to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(OPT,[Use as the path to LLVM's opt [default=autodetect]])
FIND_LLVM_PROG([OPT], [opt], [$LlvmMinVersion], [$LlvmMaxVersion])
OptCmd="$OPT"
AC_SUBST([OptCmd])

dnl ** Which LLVM assembler to use?
dnl --------------------------------------------------------------
AC_ARG_VAR(LLVMAS,[Use as the path to LLVM's assembler (typically clang) [default=autodetect]])
FIND_LLVM_PROG([LLVMAS], [clang], [$LlvmMinVersion], [$LlvmMaxVersion])
LlvmAsCmd="$LLVMAS"
AC_SUBST([LlvmAsCmd])

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------

dnl ** Copy headers shared by the RTS and compiler
dnl --------------------------------------------------------------
dnl We can't commit symlinks without breaking Windows in the default
dnl configuration.
AC_MSG_NOTICE([Creating links for headers shared by the RTS and compiler])
ln -f rts/include/rts/Bytecodes.h compiler/
ln -f rts/include/rts/storage/ClosureTypes.h compiler/
ln -f rts/include/rts/storage/FunTypes.h compiler/
ln -f rts/include/stg/MachRegs.h compiler/
mkdir -p compiler/MachRegs
ln -f rts/include/stg/MachRegs/arm32.h compiler/MachRegs/arm32.h
ln -f rts/include/stg/MachRegs/arm64.h compiler/MachRegs/arm64.h
ln -f rts/include/stg/MachRegs/loongarch64.h compiler/MachRegs/loongarch64.h
ln -f rts/include/stg/MachRegs/ppc.h compiler/MachRegs/ppc.h
ln -f rts/include/stg/MachRegs/riscv64.h compiler/MachRegs/riscv64.h
ln -f rts/include/stg/MachRegs/s390x.h compiler/MachRegs/s390x.h
ln -f rts/include/stg/MachRegs/wasm32.h compiler/MachRegs/wasm32.h
ln -f rts/include/stg/MachRegs/x86.h compiler/MachRegs/x86.h
AC_MSG_NOTICE([done.])

dnl ** Copy the files from the "fs" utility into the right folders.
dnl --------------------------------------------------------------
AC_MSG_NOTICE([Creating links for in-tree file handling routines])
ln -f utils/fs/fs.* utils/unlit/
ln -f utils/fs/fs.* rts/
ln -f utils/fs/fs.h libraries/ghc-internal/include/
ln -f utils/fs/fs.c libraries/ghc-internal/cbits/
AC_MSG_NOTICE([Routines in place. Packages can now be build normally.])

dnl ** Copy files for ghci wrapper C utilities.
dnl --------------------------------------------------------------
dnl See Note [Hadrian's ghci-wrapper package] in hadrian/src/Packages.hs
AC_MSG_NOTICE([Creating links for ghci wrapper])
ln -f driver/utils/getLocation.c driver/ghci/
ln -f driver/utils/getLocation.h driver/ghci/
ln -f driver/utils/isMinTTY.c driver/ghci/
ln -f driver/utils/isMinTTY.h driver/ghci/
ln -f driver/utils/cwrapper.c driver/ghci/
ln -f driver/utils/cwrapper.h driver/ghci/
AC_MSG_NOTICE([done.])

dnl --------------------------------------------------------------
dnl ** Can the unix package be built?
dnl --------------------------------------------------------------

dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** look for GCC and find out which version
dnl     Figure out which C compiler to use.  Gcc is preferred.
dnl     If gcc, make sure it's at least 4.7
dnl
FP_GCC_VERSION


dnl ** Check support for the extra flags passed by GHC when compiling via C
FP_GCC_SUPPORTS_VIA_C_FLAGS

dnl ** Used to determine how to compile ghc-prim's atomics.c, used by
dnl    unregisterised, Sparc, and PPC backends. Also determines whether
dnl    linking to libatomic is required for atomic operations, e.g. on
dnl    RISCV64 GCC.
FP_CC_SUPPORTS__ATOMICS
if test "$need_latomic" = 1; then
    AC_SUBST([NeedLibatomic],[YES])
else
    AC_SUBST([NeedLibatomic],[NO])
fi

dnl ** look to see if we have a C compiler using an llvm back end.
dnl
FP_CC_LLVM_BACKEND
AC_SUBST(CcLlvmBackend)

FPTOOLS_SET_C_LD_FLAGS([target],[CFLAGS],[LDFLAGS],[IGNORE_LINKER_LD_FLAGS],[CPPFLAGS])
FPTOOLS_SET_C_LD_FLAGS([build],[CONF_CC_OPTS_STAGE0],[CONF_GCC_LINKER_OPTS_STAGE0],[CONF_LD_LINKER_OPTS_STAGE0],[CONF_CPP_OPTS_STAGE0])
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE1],[CONF_GCC_LINKER_OPTS_STAGE1],[CONF_LD_LINKER_OPTS_STAGE1],[CONF_CPP_OPTS_STAGE1])
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE2],[CONF_GCC_LINKER_OPTS_STAGE2],[CONF_LD_LINKER_OPTS_STAGE2],[CONF_CPP_OPTS_STAGE2])
# Stage 3 won't be supported by cross-compilation

#-no_fixup_chains
FP_LD_NO_FIXUP_CHAINS([target], [LDFLAGS])
FP_LD_NO_FIXUP_CHAINS([build], [CONF_GCC_LINKER_OPTS_STAGE0])
FP_LD_NO_FIXUP_CHAINS([target], [CONF_GCC_LINKER_OPTS_STAGE1])
FP_LD_NO_FIXUP_CHAINS([target], [CONF_GCC_LINKER_OPTS_STAGE2])

#-no_warn_duplicate_libraries
FP_LD_NO_WARN_DUPLICATE_LIBRARIES([build], [CONF_GCC_LINKER_OPTS_STAGE0])
FP_LD_NO_WARN_DUPLICATE_LIBRARIES([target], [CONF_GCC_LINKER_OPTS_STAGE1])
FP_LD_NO_WARN_DUPLICATE_LIBRARIES([target], [CONF_GCC_LINKER_OPTS_STAGE2])

FP_MERGE_OBJECTS_SUPPORTS_RESPONSE_FILES

GHC_LLVM_TARGET_SET_VAR
# The target is substituted into the distrib/configure.ac file
AC_SUBST(LlvmTarget)

dnl ** See whether cc supports --target=<triple> and set
dnl CONF_CC_OPTS_STAGE[012] accordingly.
FP_CC_SUPPORTS_TARGET([$CC_STAGE0], [CONF_CC_OPTS_STAGE0], [CONF_CXX_OPTS_STAGE0])
FP_CC_SUPPORTS_TARGET([$CC], [CONF_CC_OPTS_STAGE1], [CONF_CXX_OPTS_STAGE1])
FP_CC_SUPPORTS_TARGET([$CC], [CONF_CC_OPTS_STAGE2], [CONF_CXX_OPTS_STAGE2])

FP_PROG_CC_LINKER_TARGET([$CC_STAGE0], [CONF_CC_OPTS_STAGE0], [CONF_GCC_LINKER_OPTS_STAGE0])
FP_PROG_CC_LINKER_TARGET([$CC], [CONF_CC_OPTS_STAGE1], [CONF_GCC_LINKER_OPTS_STAGE1])
FP_PROG_CC_LINKER_TARGET([$CC], [CONF_CC_OPTS_STAGE2], [CONF_GCC_LINKER_OPTS_STAGE2])

dnl ** See whether cc used as a linker supports -no-pie
FP_GCC_SUPPORTS_NO_PIE

dnl Pass -Qunused-arguments or otherwise GHC will have very noisy invocations of Clang
dnl TODO: Do we need -Qunused-arguments in CXX and GCC linker too?
FP_CC_IGNORE_UNUSED_ARGS([$CC_STAGE0], [CONF_CC_OPTS_STAGE0])
FP_CC_IGNORE_UNUSED_ARGS([$CC], [CONF_CC_OPTS_STAGE1])
FP_CC_IGNORE_UNUSED_ARGS([$CC], [CONF_CC_OPTS_STAGE2])

# CPP, CPPFLAGS
# --with-cpp/-with-cpp-flags
dnl Note that we must do this after setting and using the C99 CPPFLAGS, or
dnl otherwise risk trying to configure the C99 and LD flags using -E as a CPPFLAG
FP_CPP_CMD_WITH_ARGS([$CC_STAGE0],[CPPCmd_STAGE0],[CONF_CPP_OPTS_STAGE0])
FP_CPP_CMD_WITH_ARGS([$CC],[CPPCmd],[CONF_CPP_OPTS_STAGE1])
FP_CPP_CMD_WITH_ARGS([$CC],[CPPCmd],[CONF_CPP_OPTS_STAGE2])
AC_SUBST([CPPCmd_STAGE0])
AC_SUBST([CPPCmd])

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

dnl Identify C++ standard library flavour and location only when _not_ compiling
dnl the JS backend. The JS backend uses emscripten to wrap c++ utilities which
dnl fails this check, so we avoid it when compiling to JS.
if test "$TargetOS" != "ghcjs"; then
   FP_FIND_CXX_STD_LIB
fi
AC_CONFIG_FILES([mk/system-cxx-std-lib-1.0.conf])

dnl ** Set up the variables for the platform in the settings file.
dnl May need to use gcc to find platform details.
dnl --------------------------------------------------------------
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Build])

FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])
AC_SUBST(HaskellHostArch)
AC_SUBST(HaskellHostOs)

FPTOOLS_SET_HASKELL_PLATFORM_VARS([Target])
AC_SUBST(HaskellTargetArch)
AC_SUBST(HaskellTargetOs)

GHC_SUBSECTIONS_VIA_SYMBOLS
AC_SUBST(TargetHasSubsectionsViaSymbols)

GHC_IDENT_DIRECTIVE
AC_SUBST(TargetHasIdentDirective)

GHC_GNU_NONEXEC_STACK
AC_SUBST(TargetHasGnuNonexecStack)

dnl Let's make sure install-sh is executable here.  If we got it from
dnl a darcs repo, it might not be (see bug #978).
chmod +x install-sh
dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl ** how to invoke `ar' and `ranlib'
FP_PROG_AR_SUPPORTS_ATFILE
FP_PROG_AR_SUPPORTS_DASH_L
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

dnl ** check for autoreconf
AC_PATH_PROG(AutoreconfCmd, autoreconf, autoreconf)

dnl ** check for dtrace (currently only implemented for Mac OS X)
AC_ARG_ENABLE(dtrace,
    [AS_HELP_STRING([--enable-dtrace],
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
  fp_cv_sphinx_version=`"$SPHINXBUILD" --version 2>&1 | sed -re 's/.* v?([0-9]\.[0-9]\.[0-9])/\1/' | head -n1`;
fi;
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fp_cv_sphinx_version],-lt,1.0.0,
    [AC_MSG_WARN([Sphinx version 1.0.0 or later is required to build documentation]); SPHINXBUILD=;])
if test -n "$SPHINXBUILD"; then
  if "$SPHINXBUILD" -b text utils/check-sphinx utils/check-sphinx/dist > /dev/null 2>&1; then true; else
    AC_MSG_WARN([Sphinx for python3 is required to build documentation.])
    SPHINXBUILD=;
  fi
fi

dnl ** check for xelatex
AC_PATH_PROG(XELATEX,xelatex)
AC_PATH_PROG(MAKEINDEX,makeindex)
AC_PATH_PROG(GIT,git)

dnl ** check for makeinfo
AC_PATH_PROG(MAKEINFO,makeinfo)

dnl ** check for cabal
AC_PATH_PROG(CABAL,cabal)


dnl ** check for Python for testsuite driver
FIND_PYTHON

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

dnl for use in settings file
AC_CHECK_SIZEOF([void *])
TargetWordSize=$ac_cv_sizeof_void_p
AC_SUBST(TargetWordSize)

AC_C_BIGENDIAN([TargetWordBigEndian=YES],[TargetWordBigEndian=NO])
AC_SUBST(TargetWordBigEndian)

dnl ** check for math library
dnl    Keep that check as early as possible.
dnl    as we need to know whether we need libm
dnl    for math functions or not
dnl    (see https://gitlab.haskell.org/ghc/ghc/issues/3730)
AC_CHECK_LIB(m, atan, UseLibm=YES, UseLibm=NO)
AC_SUBST([UseLibm])
TargetHasLibm=$UseLibm
AC_SUBST(TargetHasLibm)

FP_BFD_FLAG
AC_SUBST([UseLibbfd])

dnl ################################################################
dnl Check for libraries
dnl ################################################################

FP_FIND_LIBFFI
AC_SUBST(UseSystemLibFFI)
AC_SUBST(FFILibDir)
AC_SUBST(FFIIncludeDir)

dnl ** check whether we need -ldl to get dlopen()
AC_CHECK_LIB([dl], [dlopen], UseLibdl=YES, UseLibdl=NO)
AC_SUBST([UseLibdl])

dnl ** check for leading underscores in symbol names
FP_LEADING_UNDERSCORE
AC_SUBST([LeadingUnderscore], [`echo $fptools_cv_leading_underscore | sed 'y/yesno/YESNO/'`])

dnl ** check for librt
AC_CHECK_LIB([rt], [clock_gettime], UseLibrt=YES, UseLibrt=NO)
AC_SUBST([UseLibrt])

FP_CHECK_PTHREAD_LIB
AC_SUBST([UseLibpthread])

GHC_ADJUSTORS_METHOD([Target])
AC_SUBST([UseLibffiForAdjustors])

dnl ** IPE data compression
dnl --------------------------------------------------------------
FP_FIND_LIBZSTD
AC_SUBST(UseLibZstd)
AC_SUBST(UseStaticLibZstd)
AC_SUBST(LibZstdLibDir)
AC_SUBST(LibZstdIncludeDir)

dnl ** Other RTS features
dnl --------------------------------------------------------------
FP_FIND_LIBDW
AC_SUBST(UseLibdw)
AC_SUBST(LibdwLibDir)
AC_SUBST(LibdwIncludeDir)

FP_FIND_LIBNUMA
AC_SUBST(UseLibNuma)
AC_SUBST(LibNumaLibDir)
AC_SUBST(LibNumaIncludeDir)

dnl ** Documentation
dnl --------------------------------------------------------------
if test -n "$SPHINXBUILD"; then
    BUILD_MAN=YES
    BUILD_SPHINX_HTML=YES
    if test -n "$XELATEX" -a -n "$MAKEINDEX"; then
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

if grep '	' compiler/ghc.cabal.in 2>&1 >/dev/null; then
   AC_MSG_ERROR([compiler/ghc.cabal.in contains tab characters; please remove them])
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

# Toolchain target files
FIND_GHC_TOOLCHAIN_BIN([NO])
PREP_TARGET_FILE
FIND_GHC_TOOLCHAIN([hadrian/cfg])

AC_CONFIG_FILES(
[ hadrian/cfg/system.config
  hadrian/ghci-cabal
  hadrian/ghci-multi-cabal
  hadrian/ghci-stack
  hadrian/cfg/default.host.target
  hadrian/cfg/default.target
])

dnl Create the VERSION file, satisfying #22322.
printf "$ProjectVersion" > VERSION

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
      with threaded RTS? : $GhcThreadedRts
"

if test "x$CcLlvmBackend" = "xYES"; then
    CompilerName="clang        "
else
    CompilerName="gcc          "
fi

echo "\
   Using (for bootstrapping) : $CC_STAGE0
   Using $CompilerName       : $CC
      which is version       : $GccVersion
      linker options         : $GccUseLdOpt
   Building a cross compiler : $CrossCompiling
   Unregisterised            : $Unregisterised
   TablesNextToCode          : $TablesNextToCode
   Build GMP in tree         : $GMP_FORCE_INTREE
   cpp          : $CPPCmd
   cpp-flags    : $CONF_CPP_OPTS_STAGE2
   hs-cpp       : $HaskellCPPCmd
   hs-cpp-flags : $HaskellCPPArgs
   js-cpp       : $JavaScriptCPPCmd
   js-cpp-flags : $JavaScriptCPPArgs
   cmmcpp       : $CmmCPPCmd
   cmmcpp-flags : $CmmCPPArgs
   cmmcpp-g0    : $CmmCPPSupportsG0
   ar           : $ArCmd
   nm           : $NmCmd
   objdump      : $ObjdumpCmd
   ranlib       : $RanlibCmd
   otool        : $OtoolCmd
   install_name_tool : $InstallNameToolCmd
   windres      : $WindresCmd
   genlib       : $GenlibCmd
   Happy        : $HappyCmd ($HappyVersion)
   Alex         : $AlexCmd ($AlexVersion)
   sphinx-build : $SPHINXBUILD
   xelatex      : $XELATEX
   makeinfo     : $MAKEINFO
   git          : $GIT
   cabal-install : $CABAL
"

echo "\
   Using optional dependencies:
      libnuma : $UseLibNuma
      libzstd : $UseLibZstd
         statically linked? : ${UseStaticLibZstd:-N/A}
      libdw   : $UseLibdw

   Using LLVM tools
      llc   : $LlcCmd
      opt   : $OptCmd
      llvm-as : $LlvmAsCmd"

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
For a standard build of GHC (fully optimised with profiling), type
   ./hadrian/build

You can customise the build with flags such as
   ./hadrian/build -j --flavour=devel2 [--freeze1]

To make changes to the default build configuration, see the file
   hadrian/src/UserSettings.hs

For more information on how to configure your GHC build, see
   https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian
"]

# Currently we don't validate the /host/ GHC toolchain because configure
# doesn't configure flags and properties for most of the host toolchain
#
# In fact, most values in default.host.target are dummy values since they are
# never used (see default.host.target.in)
#
# When we move to configure toolchains by means of ghc-toolchain only, we'll
# have a correct complete /host/ toolchain rather than an incomplete one, which
# might further unlock things like canadian cross-compilation
#
# VALIDATE_GHC_TOOLCHAIN([default.host.target],[default.host.target.ghc-toolchain])

VALIDATE_GHC_TOOLCHAIN([hadrian/cfg/default.target],[hadrian/cfg/default.target.ghc-toolchain])

rm -Rf acargs acghc-toolchain actmp-ghc-toolchain
