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

AC_INIT([The Glorious Glasgow Haskell Compilation System], [7.8.1], [glasgow-haskell-bugs@haskell.org], [ghc])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=YES}

# The primary version (e.g. 7.5, 7.4.1) is set in the AC_INIT line
# above.  If this is not a released version, then we will append the
# date to the version number (e.g. 7.4.20111220).  The date is
# constructed by finding the date of the most recent patch in the
# git repository.  If this is a source distribution (not a git
# checkout), then we ship a file 'VERSION' containing the full version
# when the source distribution was created.

if test ! -f mk/config.h.in; then
   echo "mk/config.h.in doesn't exist: perhaps you haven't run 'perl boot'?"
   exit 1
fi

AC_SUBST([CONFIGURE_ARGS], [$ac_configure_args])

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

dnl * We require autoconf version 2.60
dnl We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
dnl Using autoconf 2.59 started to give nonsense like this
dnl  #define SIZEOF_CHAR 0
dnl recently.
AC_PREREQ([2.60])

# -------------------------------------------------------------------------
# Prepare to generate the following header files
#
#
AC_CONFIG_HEADER(mk/config.h)

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

AC_ARG_WITH([ghc],
[AC_HELP_STRING([--with-ghc=ARG],
  [Use ARG as the path to GHC  [default=autodetect]])],
  [WithGhc="$withval"],
  [if test "$GHC" = ""; then
    AC_PATH_PROG([GHC], [ghc])
  fi
  WithGhc="$GHC"])

dnl ** Tell the make system which OS we are using
dnl $OSTYPE is set by the operating system to "msys" or "cygwin" or something 
AC_SUBST(OSTYPE)

AC_ARG_ENABLE(bootstrap-with-devel-snapshot,
[AC_HELP_STRING([--enable-bootstrap-with-devel-snapshot],
                [Allow bootstrapping using a development snapshot of GHC.  This is not guaranteed to work.])],
        EnableBootstrapWithDevelSnaphost=YES,
        EnableBootstrapWithDevelSnaphost=NO
)

if test "$WithGhc" != ""; then
  FPTOOLS_GHC_VERSION([GhcVersion], [GhcMajVersion], [GhcMinVersion], [GhcPatchLevel])dnl

  if test "$GhcMajVersion" = "unknown" -o "$GhcMinVersion" = "unknown"; then
     AC_MSG_ERROR([Cannot determine the version of $WithGhc.  Is it really GHC?])
  fi

  AC_SUBST(GhcVersion)dnl
  AC_SUBST(GhcMajVersion)dnl
  AC_SUBST(GhcMinVersion)dnl
  AC_SUBST(GhcPatchLevel)dnl
  GhcMinVersion2=`echo "$GhcMinVersion" | sed 's/^\\(.\\)$/0\\1/'`
  GhcCanonVersion="$GhcMajVersion$GhcMinVersion2"

  BOOTSTRAPPING_GHC_INFO_FIELD([OS_STAGE0],[target os])
  BOOTSTRAPPING_GHC_INFO_FIELD([CC_STAGE0],[C compiler command])
  dnl ToDo, once "ld command" is reliably available.
  dnl Then, we can remove the LD_STAGE0 hack in mk/build-package-date.mk
  dnl BOOTSTRAPPING_GHC_INFO_FIELD([LD_STAGE0],[ld command])
  if test "x$OS_STAGE0" != "xOSDarwin"; then
    BOOTSTRAPPING_GHC_INFO_FIELD([AR_STAGE0],[ar command])
    BOOTSTRAPPING_GHC_INFO_FIELD([AR_OPTS_STAGE0],[ar flags])
    BOOTSTRAPPING_GHC_INFO_FIELD([ArSupportsAtFile_STAGE0],[ar supports at file])
  else
    AR_STAGE0='$(AR)'
    AR_OPTS_STAGE0='$(AR_OPTS)'
    ArSupportsAtFile_STAGE0='$(ArSupportsAtFile)'
  fi
fi

dnl ** Must have GHC to build GHC
if test "$WithGhc" = ""
then
    AC_MSG_ERROR([GHC is required.])
fi
FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[7.4],
    [AC_MSG_ERROR([GHC version 7.4 or later is required to compile GHC.])])dnl

if test `expr $GhcMinVersion % 2` = "1"
then
    if test "$EnableBootstrapWithDevelSnaphost" = "NO"
    then
        AC_MSG_ERROR([
     $WithGhc is a development snapshot of GHC, version $GhcVersion.
     Bootstrapping using this version of GHC is not supported, and may not
     work.  Use --enable-bootstrap-with-devel-snapshot to try it anyway,
     or --with-ghc to specify a different GHC to use.])
    fi
fi

FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[7.5],
                    GHC_PACKAGE_DB_FLAG=package-conf,
                    GHC_PACKAGE_DB_FLAG=package-db)
AC_SUBST(GHC_PACKAGE_DB_FLAG)

# GHC 7.7+ needs -fcmm-sink when compiling Parser.hs. See #8182
FP_COMPARE_VERSIONS([$GhcVersion],[-gt],[7.7],
                    CMM_SINK_BOOTSTRAP_IS_NEEDED=YES,
                    CMM_SINK_BOOTSTRAP_IS_NEEDED=NO)
AC_SUBST(CMM_SINK_BOOTSTRAP_IS_NEEDED)

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

FPTOOLS_SET_PLATFORM_VARS

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
case "$HostArch" in
    i386|x86_64|powerpc|arm)
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

AC_ARG_WITH(hc,
[AC_HELP_STRING([--with-hc=ARG],
        [Use ARG as the path to the compiler for compiling ordinary
         Haskell code  (default= value of --with-ghc)])],
[WithHc="$withval"],
[WithHc=$WithGhc]
)
AC_SUBST(WithHc)

# This uses GHC, so put it after the "GHC is required" check above:
FP_INTREE_GHC_PWD
FP_FIND_ROOT

if test "$HostOS" = "mingw32"
then
    test -d inplace || mkdir inplace

    if test "$HostArch" = "i386"
    then
        # NB. If you update the tarballs to a new version of gcc, don't
        # forget to tweak the paths in driver/gcc/gcc.c.
        if ! test -d inplace/mingw ||
             test inplace/mingw -ot ghc-tarballs/mingw/binutils*.tar.lzma  ||
             test inplace/mingw -ot ghc-tarballs/mingw/gcc-core*.tar.lzma  ||
             test inplace/mingw -ot ghc-tarballs/mingw/gcc-c++*.tar.lzma   ||
             test inplace/mingw -ot ghc-tarballs/mingw/libgcc*.tar.gz      ||
             test inplace/mingw -ot ghc-tarballs/mingw/libgmp*.tar.gz      ||
             test inplace/mingw -ot ghc-tarballs/mingw/libmpc*.tar.gz      ||
             test inplace/mingw -ot ghc-tarballs/mingw/libmpfr*.tar.gz     ||
             test inplace/mingw -ot ghc-tarballs/mingw/libstdc*.tar.lzma   ||
             test inplace/mingw -ot ghc-tarballs/mingw/mingwrt*-dev.tar.gz ||
             test inplace/mingw -ot ghc-tarballs/mingw/mingwrt*-dll.tar.gz ||
             test inplace/mingw -ot ghc-tarballs/mingw/w32api*.tar.lzma
        then
            AC_MSG_NOTICE([Making in-tree mingw tree])
            rm -rf inplace/mingw
            mkdir inplace/mingw
            (
                cd inplace/mingw &&
                tar --lzma -xf ../../ghc-tarballs/mingw/binutils*.tar.lzma  &&
                tar --lzma -xf ../../ghc-tarballs/mingw/gcc-core*.tar.lzma  &&
                tar --lzma -xf ../../ghc-tarballs/mingw/gcc-c++*.tar.lzma   &&
                tar --lzma -xf ../../ghc-tarballs/mingw/libgcc*.tar.lzma    &&
                tar --lzma -xf ../../ghc-tarballs/mingw/libgmp*.tar.lzma    &&
                tar --lzma -xf ../../ghc-tarballs/mingw/libmpc*.tar.lzma    &&
                tar --lzma -xf ../../ghc-tarballs/mingw/libmpfr*.tar.lzma   &&
                tar --lzma -xf ../../ghc-tarballs/mingw/libstdc*.tar.lzma   &&
                tar -z     -xf ../../ghc-tarballs/mingw/mingwrt*-dev.tar.gz &&
                tar -z     -xf ../../ghc-tarballs/mingw/mingwrt*-dll.tar.gz &&
                tar --lzma -xf ../../ghc-tarballs/mingw/w32api*.tar.lzma    &&
                mv bin/gcc.exe bin/realgcc.exe
            )
            PATH=`pwd`/inplace/mingw/bin:$PATH inplace/mingw/bin/realgcc.exe driver/gcc/gcc.c driver/utils/cwrapper.c driver/utils/getLocation.c -Idriver/utils -o inplace/mingw/bin/gcc.exe
            AC_MSG_NOTICE([In-tree mingw tree created])
        fi
    else
        # NB. If you update the tarballs to a new version of gcc, don't
        # forget to tweak the paths in driver/gcc/gcc.c.
        if ! test -d inplace/mingw ||
             test inplace/mingw -ot ghc-tarballs/mingw64/*.tar.bz2
        then
            AC_MSG_NOTICE([Making in-tree mingw tree])
            rm -rf inplace/mingw
            mkdir inplace/mingw
            (
                cd inplace/mingw &&
                tar -jxf ../../ghc-tarballs/mingw64/*.tar.bz2
            )
            AC_MSG_NOTICE([In-tree mingw tree created])
        fi
    fi

    mingwbin="$hardtop/inplace/mingw/bin/"
    CC="${mingwbin}gcc.exe"
    LD="${mingwbin}ld.exe"
    NM="${mingwbin}nm.exe"
    RANLIB="${mingwbin}ranlib.exe"
    OBJDUMP="${mingwbin}objdump.exe"
    fp_prog_ar="${mingwbin}ar.exe"

    if ! test -d inplace/perl ||
         test inplace/perl -ot ghc-tarballs/perl/ghc-perl*.tar.gz
    then
        AC_MSG_NOTICE([Making in-tree perl tree])
        rm -rf inplace/perl
        mkdir inplace/perl
        (
            cd inplace/perl &&
            tar -zxf ../../ghc-tarballs/perl/ghc-perl*.tar.gz
        )
        AC_MSG_NOTICE([In-tree perl tree created])
    fi
fi

# system libffi

AC_ARG_WITH([system-libffi],
[AC_HELP_STRING([--with-system-libffi],
  [Use system provided libffi for RTS [default=no]])
])

AS_IF([test "x$with_system_libffi" = "xyes"],
  [UseSystemLibFFI="YES"], [UseSystemLibFFI="NO"]
)


AC_SUBST(UseSystemLibFFI)

AC_ARG_WITH([ffi-includes],
[AC_HELP_STRING([--with-ffi-includes=ARG]
  [Find includes for libffi in ARG [default=system default]])
],
[
 if test "x$UseSystemLibFFI" != "xYES"; then
    AC_MSG_WARN([--with-ffi-includes will be ignored, --with-system-libffi not set])
 else
    FFIIncludeDir="$withval"
    LIBFFI_CFLAGS="-I$withval"
 fi
])

AC_SUBST(FFIIncludeDir)

AC_ARG_WITH([ffi-libraries],
[AC_HELP_STRING([--with-ffi-libraries=ARG]
  [Find libffi in ARG [default=system default]])
],
[
 if test "x$UseSystemLibFFI" != "xYES"; then
    AC_MSG_WARN([--with-ffi-libraries will be ignored, --with-system-libffi not set])
 else
    FFILibDir="$withval" LIBFFI_LDFLAGS="-L$withval"
 fi
])

AC_SUBST(FFILibDir)

AS_IF([test "$UseSystemLibFFI" = "YES"], [
 CFLAGS2="$CFLAGS"
 CFLAGS="$LIBFFI_CFLAGS $CFLAGS"
 LDFLAGS2="$LDFLAGS"
 LDFLAGS="$LIBFFI_LDFLAGS $LDFLAGS"
 AC_CHECK_LIB(ffi, ffi_call,
  [AC_CHECK_HEADERS([ffi.h], [break], [])
   AC_DEFINE([HAVE_LIBFFI], [1], [Define to 1 if you have libffi.])],
  [AC_MSG_ERROR([Cannot find system libffi])])
 CFLAGS="$CFLAGS2"
 LDFLAGS="$LDFLAGS2"
])

FP_ICONV
FP_GMP

XCODE_VERSION()

SplitObjsBroken=NO
if test "$TargetOS_CPP" = "darwin"
then
    # Split objects is broken (#4013) with XCode < 3.2
    if test "$XCodeVersion1" -lt 3
    then
        SplitObjsBroken=YES
    else
        if test "$XCodeVersion1" -eq 3
        then
            if test "$XCodeVersion2" -lt 2
            then
                SplitObjsBroken=YES
            fi
        fi
    fi
fi
AC_SUBST([SplitObjsBroken])

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
if test "$build" != "$host" ; then
   AC_MSG_ERROR([
You've selected:

  BUILD:  $build   (the architecture we're building on)
  HOST:   $host    (the architecture the compiler we're building will execute on)
  TARGET: $target  (the architecture the compiler we're building will produce code for)

BUILD must equal HOST; that is, we do not support building GHC itself
with a cross-compiler.  To cross-compile GHC itself, set TARGET: stage
1 will be a cross-compiler, and stage 2 will be the cross-compiled
GHC.
])
fi
if test "$CrossCompiling" = "YES"
then
  CrossCompilePrefix="${target}-"
else
  CrossCompilePrefix=""
fi
TargetPlatformFull="${target}"
AC_SUBST(CrossCompiling)
AC_SUBST(CrossCompilePrefix)
AC_SUBST(TargetPlatformFull)

dnl ** Which gcc to use?
dnl --------------------------------------------------------------
FIND_GCC([WhatGccIsCalled], [gcc], [gcc])
CC="$WhatGccIsCalled"
export CC

dnl ** Which ld to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([LD], [ld], [ld])
LdCmd="$LD"
AC_SUBST([LdCmd])

dnl ** Which nm to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([NM], [nm], [nm])
NmCmd="$NM"
AC_SUBST([NmCmd])

dnl ** Which ar to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([AR], [ar], [ar])
ArCmd="$AR"
fp_prog_ar="$AR"
AC_SUBST([ArCmd])

dnl ** Which ranlib to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([RANLIB], [ranlib], [ranlib])
RanlibCmd="$RANLIB"
RANLIB="$RanlibCmd"
AC_SUBST([RanlibCmd])


# Note: we may not have objdump on OS X, and we only need it on Windows (for DLL checks)
case $HostOS_CPP in
cygwin32|mingw32)
    dnl ** Which objdump to use?
    dnl --------------------------------------------------------------
    FP_ARG_WITH_PATH_GNU_PROG([OBJDUMP], [objdump], [objdump])
    ObjdumpCmd="$OBJDUMP"
    AC_SUBST([ObjdumpCmd])
    ;;
esac

dnl ** Which LLVM llc to use?
dnl --------------------------------------------------------------
FIND_LLVM_PROG([LLC], [llc], [llc])
LlcCmd="$LLC"
AC_SUBST([LlcCmd])

dnl ** Which LLVM opt to use?
dnl --------------------------------------------------------------
FIND_LLVM_PROG([OPT], [opt], [opt])
OptCmd="$OPT"
AC_SUBST([OptCmd])

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------


dnl --------------------------------------------------------------
dnl * General configuration checks
dnl --------------------------------------------------------------

dnl ** Can the unix package be built?
dnl --------------------------------------------------------------

if test "$TargetOS" = "mingw32"
then
    GhcLibsWithUnix=NO
else
    GhcLibsWithUnix=YES
fi
AC_SUBST([GhcLibsWithUnix])

dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** look for `perl'
case $HostOS_CPP in
cygwin32|mingw32)
    PerlCmd=$hardtop/inplace/perl/perl
    ;;
*)
    AC_PATH_PROG(PerlCmd,perl)
    if test -z "$PerlCmd"
    then
        echo "You must install perl before you can continue"
        echo "Perhaps it is already installed, but not in your PATH?"
        exit 1
    else
        FPTOOLS_CHECK_PERL_VERSION
    fi
    ;;
esac

dnl ** look for GCC and find out which version
dnl     Figure out which C compiler to use.  Gcc is preferred.
dnl     If gcc, make sure it's at least 3.0
dnl
FP_GCC_VERSION

dnl ** look to see if we have a C compiler using an llvm back end.
dnl
FP_CC_LLVM_BACKEND

FP_PROG_LD_IS_GNU
FP_PROG_LD_BUILD_ID
FP_PROG_LD_NO_COMPACT_UNWIND
FP_PROG_LD_FILELIST


FPTOOLS_SET_C_LD_FLAGS([target],[CFLAGS],[LDFLAGS],[IGNORE_LINKER_LD_FLAGS],[CPPFLAGS])
FPTOOLS_SET_C_LD_FLAGS([build],[CONF_CC_OPTS_STAGE0],[CONF_GCC_LINKER_OPTS_STAGE0],[CONF_LD_LINKER_OPTS_STAGE0],[CONF_CPP_OPTS_STAGE0])
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE1],[CONF_GCC_LINKER_OPTS_STAGE1],[CONF_LD_LINKER_OPTS_STAGE1],[CONF_CPP_OPTS_STAGE1])
# Stage 3 won't be supported by cross-compilation
FPTOOLS_SET_C_LD_FLAGS([target],[CONF_CC_OPTS_STAGE2],[CONF_GCC_LINKER_OPTS_STAGE2],[CONF_LD_LINKER_OPTS_STAGE2],[CONF_CPP_OPTS_STAGE2])

FP_GCC_EXTRA_FLAGS

dnl ** figure out how to invoke cpp directly (gcc -E is no good)
AC_PROG_CPP

AC_SUBST(CONF_CC_OPTS_STAGE0)
AC_SUBST(CONF_CC_OPTS_STAGE1)
AC_SUBST(CONF_CC_OPTS_STAGE2)
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE0)
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE1)
AC_SUBST(CONF_GCC_LINKER_OPTS_STAGE2)
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE0)
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE1)
AC_SUBST(CONF_LD_LINKER_OPTS_STAGE2)
AC_SUBST(CONF_CPP_OPTS_STAGE0)
AC_SUBST(CONF_CPP_OPTS_STAGE1)
AC_SUBST(CONF_CPP_OPTS_STAGE2)

dnl ** Set up the variables for the platform in the settings file.
dnl May need to use gcc to find platform details.
dnl --------------------------------------------------------------
FPTOOLS_SET_HASKELL_PLATFORM_VARS

dnl ** figure out how to do context diffs
FP_PROG_CONTEXT_DIFF

dnl Let's make sure install-sh is executable here.  If we got it from
dnl a darcs repo, it might not be (see bug #978).
chmod +x install-sh
dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl If you can run configure, you certainly have /bin/sh
AC_DEFINE([HAVE_BIN_SH], [1], [Define to 1 if you have /bin/sh.])

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

dnl ** check for patch
dnl if GNU patch is named gpatch, look for it first
AC_PATH_PROGS(PatchCmd,gpatch patch, patch)

dnl ** check for dtrace (currently only implemented for Mac OS X)
HaveDtrace=NO
AC_PATH_PROG(DtraceCmd,dtrace)
if test -n "$DtraceCmd"; then
  if test "x$TargetOS_CPP-$TargetVendor_CPP" = "xdarwin-apple" -o "x$TargetOS_CPP-$TargetVendor_CPP" = "xsolaris2-unknown"; then
    HaveDtrace=YES
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

dnl ** check for DocBook toolchain
FP_CHECK_DOCBOOK_DTD
FP_DOCBOOK_XSL
FP_PROG_DBLATEX

dnl ** check for ghc-pkg command
FP_PROG_GHC_PKG

dnl ** check for installed happy binary + version
FPTOOLS_HAPPY

dnl ** check for installed alex binary + version
FPTOOLS_ALEX

dnl --------------------------------------------------
dnl ### program checking section ends here ###
dnl --------------------------------------------------

dnl --------------------------------------------------
dnl * Platform header file and syscall feature tests
dnl ### checking the state of the local header files and syscalls ###

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/param.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h sched.h])

dnl sys/cpuset.h needs sys/param.h to be included first on FreeBSD 9.1; #7708
AC_CHECK_HEADERS([sys/cpuset.h], [], [],
[[#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
]])

dnl ** check if it is safe to include both <time.h> and <sys/time.h>
AC_HEADER_TIME

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** what are the sizes of various types
AC_CHECK_SIZEOF(char,               1)
AC_CHECK_SIZEOF(double,             8)
AC_CHECK_SIZEOF(float,              4)
AC_CHECK_SIZEOF(int,                4)
AC_CHECK_SIZEOF(long,               4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(long long,          8)
fi
AC_CHECK_SIZEOF(short,              2)
AC_CHECK_SIZEOF(unsigned char,      1)
AC_CHECK_SIZEOF(unsigned int,       4)
AC_CHECK_SIZEOF(unsigned long,      4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(unsigned long long, 8)
fi
AC_CHECK_SIZEOF(unsigned short,     2)
AC_CHECK_SIZEOF(void *,             4)

dnl for use in settings.in
WordSize=$ac_cv_sizeof_void_p
AC_SUBST(WordSize)

dnl ** what are alignment constraints on various types
FP_CHECK_ALIGNMENT(char)
FP_CHECK_ALIGNMENT(double)
FP_CHECK_ALIGNMENT(float)
FP_CHECK_ALIGNMENT(int)
FP_CHECK_ALIGNMENT(long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(long long)
fi
FP_CHECK_ALIGNMENT(short)
FP_CHECK_ALIGNMENT(unsigned char)
FP_CHECK_ALIGNMENT(unsigned int)
FP_CHECK_ALIGNMENT(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(unsigned long long)
fi
FP_CHECK_ALIGNMENT(unsigned short)
FP_CHECK_ALIGNMENT(void *)

FP_CHECK_FUNC([WinExec],
  [@%:@include <windows.h>], [WinExec("",0)])

FP_CHECK_FUNC([GetModuleFileName],
  [@%:@include <windows.h>], [GetModuleFileName((HMODULE)0,(LPTSTR)0,0)])

dnl ** check return type of signal handlers
dnl Foo: assumes we can use prototypes.
dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
dnl AC_CACHE_CHECK([type of signal handlers], ac_cv_type_signal_handler,
dnl [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
dnl #include <signal.h>
dnl #ifdef signal
dnl #undef signal
dnl #endif
dnl void (*signal (int, void (*)(int)))(int);
dnl ]],
dnl [[int i;]])],
dnl [ac_cv_type_signal_handler=void_int],
dnl [ac_cv_type_signal_handler=int_void])])
dnl if test "$ac_cv_type_signal_handler" = void_int; then
dnl AC_DEFINE(VOID_INT_SIGNALS)
dnl fi

dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
AC_TYPE_SIGNAL
if test "$ac_cv_type_signal" = void; then
  AC_DEFINE([VOID_INT_SIGNALS], [1], [Define to 1 if signal handlers have type void (*)(int). Otherwise, they're assumed to have type int (*)(void).])
fi

dnl ** check for more functions
dnl ** The following have been verified to be used in ghc/, but might be used somewhere else, too.
AC_CHECK_FUNCS([getclock getrusage gettimeofday setitimer siginterrupt sysconf times ctime_r sched_setaffinity setlocale])

dnl ** On OS X 10.4 (at least), time.h doesn't declare ctime_r if
dnl ** _POSIX_C_SOURCE is defined
AC_CHECK_DECLS([ctime_r], , ,
[#define _POSIX_SOURCE 1
#define _POSIX_C_SOURCE 199506L
#include <time.h>])

dnl ** check for mingwex library
AC_CHECK_LIB(mingwex, closedir, HaveLibMingwEx=YES, HaveLibMingwEx=NO)
AC_SUBST(HaveLibMingwEx)

if test $HaveLibMingwEx = YES ; then
  AC_DEFINE([HAVE_MINGWEX], [1], [Define to 1 if you have the mingwex library.])
fi

dnl ** check for math library
dnl    Keep that check as early as possible.
dnl    as we need to know whether we need libm
dnl    for math functions or not
dnl    (see http://ghc.haskell.org/trac/ghc/ticket/3730)
AC_CHECK_LIB(m, atan, HaveLibM=YES, HaveLibM=NO)
if test $HaveLibM = YES
then
  AC_DEFINE([HAVE_LIBM], [1], [Define to 1 if you need to link with libm])
fi

dnl ** check whether this machine has BFD and libiberty installed (used for debugging)
dnl    the order of these tests matters: bfd needs libiberty
AC_CHECK_LIB(iberty, xmalloc)
AC_CHECK_LIB(bfd,    bfd_uncompress_section_contents)

dnl ################################################################
dnl Check for libraries
dnl ################################################################

dnl ** check whether we need -ldl to get dlopen()

AC_CHECK_LIB(dl, dlopen,
    [HaveLibDL=YES
     AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
     LIBS="$LIBS -ldl"],
    [HaveLibDL=NO])
AC_SUBST(HaveLibDL)

dnl --------------------------------------------------
dnl * Miscellaneous feature tests
dnl --------------------------------------------------

dnl ** can we get alloca?
AC_FUNC_ALLOCA

dnl ** working vfork?
AC_FUNC_FORK

dnl ** determine whether or not const works
AC_C_CONST

dnl ** are we big endian?
AC_C_BIGENDIAN
FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN

dnl ** check for leading underscores in symbol names
FP_LEADING_UNDERSCORE

FP_VISIBILITY_HIDDEN

dnl ** check for librt
AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_FUNCS(clock_gettime timer_settime)
FP_CHECK_TIMER_CREATE

dnl ** check for Apple's "interesting" long double compatibility scheme
AC_MSG_CHECKING(for printf\$LDBLStub)
AC_TRY_LINK_FUNC(printf\$LDBLStub,
    [
        AC_MSG_RESULT(yes)
        AC_DEFINE([HAVE_PRINTF_LDBLSTUB],[1],
            [Define to 1 if we have printf$LDBLStub (Apple Mac OS >= 10.4, PPC).])
    ],
    [
        AC_MSG_RESULT(no)
        AC_DEFINE([HAVE_PRINTF_LDBLSTUB],[0],
            [Define to 1 if we have printf$LDBLStub (Apple Mac OS >= 10.4, PPC).])
    ])

dnl ** check for eventfd which is needed by the I/O manager
AC_CHECK_HEADERS([sys/eventfd.h])
AC_CHECK_FUNCS([eventfd])

dnl ** Check for __thread support in the compiler
AC_MSG_CHECKING(for __thread support)
AC_COMPILE_IFELSE(
  [ AC_LANG_SOURCE([[__thread int tester = 0;]]) ],
  [
   AC_MSG_RESULT(yes)
   AC_DEFINE([CC_SUPPORTS_TLS],[1],[Define to 1 if __thread is supported])
  ],
  [
   AC_MSG_RESULT(no)
   AC_DEFINE([CC_SUPPORTS_TLS],[0],[Define to 1 if __thread is supported])
  ])


dnl ** checking for PAPI
AC_CHECK_LIB(papi, PAPI_library_init, HavePapiLib=YES, HavePapiLib=NO)
AC_CHECK_HEADER([papi.h], [HavePapiHeader=YES], [HavePapiHeader=NO])
AC_SUBST(HavePapiLib)
AC_SUBST(HavePapiHeader)

AC_CHECK_FUNCS(__mingw_vfprintf)

if test "$HavePapiLib" = "YES" -a "$HavePapiHeader" = "YES"; then
   HavePapi=YES
else
   HavePapi=NO
fi
AC_SUBST(HavePapi)

if test "$HAVE_DOCBOOK_XSL" = "NO" ||
   test "$XsltprocCmd" = ""
then
    BUILD_DOCBOOK_HTML=NO
else
    BUILD_DOCBOOK_HTML=YES
fi
AC_SUBST(BUILD_DOCBOOK_HTML)

if test "$DblatexCmd" = ""
then
    BUILD_DOCBOOK_PS=NO
    BUILD_DOCBOOK_PDF=NO
else
    BUILD_DOCBOOK_PS=YES
    BUILD_DOCBOOK_PDF=YES
fi
AC_SUBST(BUILD_DOCBOOK_PS)
AC_SUBST(BUILD_DOCBOOK_PDF)

LIBRARY_VERSION(base)
LIBRARY_VERSION(Cabal, Cabal/Cabal)
LIBRARY_VERSION(ghc-prim)
LIBRARY_ghc_VERSION="$ProjectVersion"
AC_SUBST(LIBRARY_ghc_VERSION)

if grep '	' compiler/ghc.cabal.in 2>&1 >/dev/null; then
   AC_MSG_ERROR([compiler/ghc.cabal.in contains tab characters; please remove them])
fi

AC_CONFIG_FILES([mk/config.mk mk/install.mk mk/project.mk compiler/ghc.cabal ghc/ghc-bin.cabal utils/runghc/runghc.cabal settings docs/users_guide/ug-book.xml docs/users_guide/ug-ent.xml docs/index.html libraries/prologue.txt distrib/configure.ac])
AC_OUTPUT

# We got caught by
#     http://savannah.gnu.org/bugs/index.php?1516
#     $(eval ...) inside conditionals causes errors
# with make 3.80, so warn the user if it looks like they're about to
# try to use it.
# We would use "grep -q" here, but Solaris's grep doesn't support it.
checkMake380() {
    if $1 --version 2>&1 | head -1 | grep 'GNU Make 3\.80' > /dev/null
    then
        echo
        echo "WARNING: It looks like \"$1\" is GNU make 3.80."
        echo "This version cannot be used to build GHC."
        echo "Please use GNU make >= 3.81."
    fi
}

checkMake380 make
checkMake380 gmake

echo ["
----------------------------------------------------------------------
Configure completed successfully.

   Building GHC version  : $ProjectVersion

   Build platform        : $BuildPlatform
   Host platform         : $HostPlatform
   Target platform       : $TargetPlatform
"]

echo ["\
   Bootstrapping using   : $WithGhc
      which is version   : $GhcVersion
"]

if test "x$CC_LLVM_BACKEND" = "x1"; then
    if test "x$CC_CLANG_BACKEND" = "x1"; then
        CompilerName="clang        "
    else
        CompilerName="llvm-gcc     "
    fi
else
    CompilerName="gcc          "
fi

echo ["\
   Using $CompilerName       : $WhatGccIsCalled
      which is version       : $GccVersion
   Building a cross compiler : $CrossCompiling

   ld       : $LdCmd
   Happy    : $HappyCmd ($HappyVersion)
   Alex     : $AlexCmd ($AlexVersion)
   Perl     : $PerlCmd
   dblatex  : $DblatexCmd
   xsltproc : $XsltprocCmd

   Using LLVM tools
      llc   : $LlcCmd
      opt   : $OptCmd"]

if test "$HSCOLOUR" = ""; then
echo ["
   HsColour was not found; documentation will not contain source links
"]
else
echo ["\
   HsColour : $HSCOLOUR
"]
fi

echo ["\
   Building DocBook HTML documentation : $BUILD_DOCBOOK_HTML
   Building DocBook PS documentation   : $BUILD_DOCBOOK_PS
   Building DocBook PDF documentation  : $BUILD_DOCBOOK_PDF"]

echo ["----------------------------------------------------------------------
"]

echo "\
For a standard build of GHC (fully optimised with profiling), type (g)make.

To make changes to the default build configuration, copy the file
mk/build.mk.sample to mk/build.mk, and edit the settings in there.

For more information on how to configure your GHC build, see
   http://ghc.haskell.org/trac/ghc/wiki/Building
"
