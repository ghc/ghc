dnl == autoconf source for the Glasgow FP tools ==
dnl (run "grep '^dnl \*' configure.ac | sed -e 's/dnl / /g; s/\*\*/   +/g;'"
dnl  (or some such) to see the outline of this file)
dnl
#
# (c) The AQUA Project, Glasgow University, 1994-1998
#
# Configure script template for the Glasgow functional programming tools
#
# Process with 'autoconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

# First off, a distrib sanity check..
AC_INIT(mk/config.mk.in)

dnl * We require autoconf version 2.52
dnl We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
AC_PREREQ(2.52)

dnl * Declare subdirectories that have a private configure script
dnl
dnl After the toplevel configuration is complete, the script will recurse into
dnl these subdirectories if they exist. The use of a cache file makes repeated
dnl checks cheap.
AC_CONFIG_SUBDIRS([ghc libraries])

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

dnl--------------------------------------------------------------------
dnl * Choose host(/target/build) platform
dnl--------------------------------------------------------------------

dnl Guess host/target/build platform(s) if necessary.
AC_CANONICAL_SYSTEM

# "$host" defaults to "$target"
if test "x$host" = xNONE ; then
    host=$target
fi

dnl ** canonicalize platform names
BuildPlatform=`/bin/sh $srcdir/config.sub $build` || exit 1
HostPlatform=`/bin/sh $srcdir/config.sub $host` || exit 1
TargetPlatform=`/bin/sh $srcdir/config.sub $target` || exit 1

if test x"$TargetPlatform" != x"$HostPlatform" ; then
    echo "GHC configuration does not support differing host/target (i.e., cross-compiling)"
    exit 1
fi

exeext=''
#
# The following will be more difficult when we *are* cross-compiling.
# Suitable names to slam in *_CPP are in platform.h.in.
# We also record the architecture, vendor, and operating system (OS)
# separately.
case $HostPlatform in
alpha*-dec-osf[[12]]*)
	HostPlatform=alpha-dec-osf1   # canonicalise for our purposes
	TargetPlatform=alpha-dec-osf1 # this will work for now... (hack)
	BuildPlatform=alpha-dec-osf1  # hack
        HostPlatform_CPP='alpha_dec_osf1'
        HostArch_CPP='alpha'
        HostVendor_CPP='dec'
        HostOS_CPP='osf1'
        ;;
alpha*-dec-osf[[345]]*)
	HostPlatform=alpha-dec-osf3   # canonicalise for our purposes
	TargetPlatform=alpha-dec-osf3 # this will work for now... (hack)
	BuildPlatform=alpha-dec-osf3  # hack
        HostPlatform_CPP='alpha_dec_osf3'
        HostArch_CPP='alpha'
        HostVendor_CPP='dec'
        HostOS_CPP='osf3'
        ;;
alpha*-unknown-linux*)
	HostPlatform=alpha-unknown-linux
	TargetPlatform=alpha-unknown-linux
	BuildPlatform=alpha-unknown-linux
        HostPlatform_CPP='alpha_unknown_linux'
        HostArch_CPP='alpha'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
alpha*-unknown-freebsd*)
	HostPlatform=alpha-unknown-freebsd
	TargetPlatform=alpha-unknown-freebsd
	BuildPlatform=alpha-unknown-freebsd
        HostPlatform_CPP='alpha_unknown_freebsd'
        HostArch_CPP='alpha'
        HostVendor_CPP='unknown'
        HostOS_CPP='freebsd'
        ;;
arm*-linux*)
        HostPlatform=arm-unknown-linux # hack again
        TargetPlatform=arm-unknown-linux
        BuildPlatform=arm-unknown-linux
        HostPlatform_CPP='arm_unknown_linux'                 
        HostArch_CPP='arm'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
hppa*-*-linux*)
        HostPlatform=hppa-unknown-linux # hack again
        TargetPlatform=hppa-unknown-linux
        BuildPlatform=hppa-unknown-linux
        HostPlatform_CPP='hppa_unknown_linux'                
        HostArch_CPP='hppa'
        HostVendor_CPP='unknown'                             
        HostOS_CPP='linux'
        ;;
hppa1.1-hp-hpux*)
	HostPlatform=hppa1.1-hp-hpux  # canonicalise for our purposes (hack)
	TargetPlatform=hppa1.1-hp-hpux
	BuildPlatform=hppa1.1-hp-hpux
        HostPlatform_CPP='hppa1_1_hp_hpux'
        HostArch_CPP='hppa1_1'
        HostVendor_CPP='hp'
        HostOS_CPP='hpux'
        ;;
i[[3456]]86-*-linuxaout*)
	HostPlatform=i386-unknown-linuxaout   # hack again
	TargetPlatform=i386-unknown-linuxaout
	BuildPlatform=i386-unknown-linuxaout
        HostPlatform_CPP='i386_unknown_linuxaout'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='linuxaout'
        ;;
i[[3456]]86-*-linux*)
	HostPlatform=i386-unknown-linux # hack again
	TargetPlatform=i386-unknown-linux
	BuildPlatform=i386-unknown-linux
        HostPlatform_CPP='i386_unknown_linux'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
i[[3456]]86-*-freebsd[[3-9]]*) # FreeBSD 3.0+ uses ELF
	HostPlatform=i386-unknown-freebsd # hack again
	TargetPlatform=i386-unknown-freebsd
	BuildPlatform=i386-unknown-freebsd
        HostPlatform_CPP='i386_unknown_freebsd'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='freebsd'
        ;;
i[[3456]]86-*-freebsd2*) # Older FreeBSDs are a.out
	HostPlatform=i386-unknown-freebsd2 # hack again
	TargetPlatform=i386-unknown-freebsd2
	BuildPlatform=i386-unknown-freebsd2
        HostPlatform_CPP='i386_unknown_freebsd2'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='freebsd2'
        ;;
i[[3456]]86-*-netbsd*)
	HostPlatform=i386-unknown-netbsd # hack again
	TargetPlatform=i386-unknown-netbsd
	BuildPlatform=i386-unknown-netbsd
        HostPlatform_CPP='i386_unknown_netbsd'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='netbsd'
        ;;
i[[3456]]86-*-openbsd*)
	HostPlatform=i386-unknown-openbsd # hack again
	TargetPlatform=i386-unknown-openbsd
	BuildPlatform=i386-unknown-openbsd
        HostPlatform_CPP='i386_unknown_openbsd'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
i[[3456]]86-*-solaris2*)
	HostPlatform=i386-unknown-solaris2 # hack again
	TargetPlatform=i386-unknown-solaris2
	BuildPlatform=i386-unknown-solaris2
        HostPlatform_CPP='i386_unknown_solaris2'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='solaris2'
        ;;
i[[3456]]86-*-cygwin*)
	HostPlatform=i386-unknown-cygwin32 # hack again
	TargetPlatform=i386-unknown-cygwin32
	BuildPlatform=i386-unknown-cygwin32
        HostPlatform_CPP='i386_unknown_cygwin32'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='cygwin32'
	exeext='.exe'
        ;;
i[[3456]]86-*-mingw32*)
	HostPlatform=i386-unknown-mingw32 # hack again
	TargetPlatform=i386-unknown-mingw32
	BuildPlatform=i386-unknown-mingw32
        HostPlatform_CPP='i386_unknown_mingw32'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='mingw32'
	exeext='.exe'
        ;;
ia64-*-linux*)
        HostPlatform=ia64-unknown-linux # hack again
        TargetPlatform=ia64-unknown-linux
        BuildPlatform=ia64-unknown-linux
        HostPlatform_CPP='ia64_unknown_linux'
        HostArch_CPP='ia64'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
x86_64-*-linux*)
        HostPlatform=x86_64-unknown-linux
        TargetPlatform=x86_64-unknown-linux
        BuildPlatform=x86_64-unknown-linux
        HostPlatform_CPP='x86_64_unknown_linux'
        HostArch_CPP='x86_64'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
	;;
m68k-*-linux*)
        HostPlatform=m68k-unknown-linux # hack again
        TargetPlatform=m68k-unknown-linux                    
        BuildPlatform=m68k-unknown-linux
        HostPlatform_CPP='m68k_unknown_linux'
        HostArch_CPP='m68k'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
m68k-next-nextstep2)
        HostPlatform_CPP='m68k_next_nextstep2'
        HostArch_CPP='m68k'
        HostVendor_CPP='next'
        HostOS_CPP='nextstep2'
        ;;
m68k-next-nextstep3)
        HostPlatform_CPP='m68k_next_nextstep3'
        HostArch_CPP='m68k'
        HostVendor_CPP='next'
        HostOS_CPP='nextstep3'
        ;;
i[[3456]]86-next-nextstep3)
	HostPlatform=i386-next-nextstep3 # hack again
	TargetPlatform=i386-next-nextstep3
	BuildPlatform=i386-next-nextstep3
        HostPlatform_CPP='i386_next_nextstep3'
        HostArch_CPP='i386'
        HostVendor_CPP='next'
        HostOS_CPP='nextstep3'
        ;;
m68k-*-openbsd*)
	HostPlatform=m68k-unknown-openbsd
	TargetPlatform=m68k-unknown-openbsd
	BuildPlatform=m68k-unknown-openbsd
        HostPlatform_CPP='m68k_unknown_openbsd'
        HostArch_CPP='m68k'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
m68k-*-netbsd*)
	HostPlatform=m68k-unknown-netbsd
	TargetPlatform=m68k-unknown-netbsd
	BuildPlatform=m68k-unknown-netbsd
        HostPlatform_CPP='m68k_unknown_netbsd'
        HostArch_CPP='m68k'
        HostVendor_CPP='unknown'
        HostOS_CPP='netbsd'
        ;;
m68k-sun-sunos4*)
	HostPlatform=m68k-sun-sunos4
	TargetPlatform=m68k-sun-sunos4 #hack
	BuildPlatform=m68k-sun-sunos4 #hack
        HostPlatform_CPP='m68k_sun_sunos4'
        HostArch_CPP='m68k'
        HostVendor_CPP='sun'
        HostOS_CPP='sunos4'
        ;;
mips-*-linux*)
        HostPlatform=mips-unknown-linux # hack again
        TargetPlatform=mips-unknown-linux
        BuildPlatform=mips-unknown-linux
        HostPlatform_CPP='mips_unknown_linux'
        HostArch_CPP='mips'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
mips-dec-ultrix*)
        HostPlatform_CPP='mips_dec_ultrix'
        HostArch_CPP='mipsel'   # NB a little different
        HostVendor_CPP='dec'
        HostOS_CPP='ultrix'
        ;;
mips-sgi-irix*)
	HostPlatform=mips-sgi-irix
	TargetPlatform=mips-sgi-irix #hack
	BuildPlatform=mips-sgi-irix #hack
        HostPlatform_CPP='mips_sgi_irix'
        HostArch_CPP='mipseb'   # NB a little different
        HostVendor_CPP='sgi'
        HostOS_CPP='irix'
        ;;
rs6000-ibm-aix*)
	HostPlatform=rs6000-ibm-aix
	TargetPlatform=rs6000-ibm-aix #hack
	BuildPlatform=rs6000-ibm-aix #hack
        HostPlatform_CPP='rs6000_ibm_aix'
        HostArch_CPP='rs6000'
        HostVendor_CPP='ibm'
        HostOS_CPP='aix'
        ;;
powerpc-ibm-aix*)
	HostPlatform=powerpc-ibm-aix
	TargetPlatform=powerpc-ibm-aix #hack
	BuildPlatform=powerpc-ibm-aix #hack
        HostPlatform_CPP='powerpc_ibm_aix'
        HostArch_CPP='powerpc'
        HostVendor_CPP='ibm'
        HostOS_CPP='aix'
        ;;
powerpc-apple-darwin*)
	HostPlatform=powerpc-apple-darwin
	TargetPlatform=powerpc-apple-darwin #hack
	BuildPlatform=powerpc-apple-darwin #hack
	HostPlatform_CPP='powerpc_apple_darwin'
	HostArch_CPP='powerpc'
	HostVendor_CPP='apple'
	HostOS_CPP='darwin'
	;;
powerpc-unknown-linux*)
        HostPlatform=powerpc-unknown-linux
        TargetPlatform=powerpc-unknown-linux
        BuildPlatform=powerpc-unknown-linux
        HostPlatform_CPP='powerpc_unknown_linux'
        HostArch_CPP='powerpc'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
s390-ibm-linux*)
       HostPlatform=s390-ibm-linux
       TargetPlatform=s390-ibm-linux #hack
       BuildPlatform=s390-ibm-linux #hack
        HostPlatform_CPP='s390_ibm_linux'
        HostArch_CPP='s390'
        HostVendor_CPP='ibm'
        HostOS_CPP='linux'
        ;;
sparc-sun-sunos4*)
	HostPlatform=sparc-sun-sunos4
	TargetPlatform=sparc-sun-sunos4 #hack
	BuildPlatform=sparc-sun-sunos4 #hack
        HostPlatform_CPP='sparc_sun_sunos4'
        HostArch_CPP='sparc'
        HostVendor_CPP='sun'
        HostOS_CPP='sunos4'
        ;;
sparc-sun-solaris2*)
	HostPlatform=sparc-sun-solaris2
	TargetPlatform=sparc-sun-solaris2 #hack
	BuildPlatform=sparc-sun-solaris2 #hack
        HostPlatform_CPP='sparc_sun_solaris2'
        HostArch_CPP='sparc'
        HostVendor_CPP='sun'
        HostOS_CPP='solaris2'
        ;;
sparc*-linux*)
        HostPlatform=sparc-unknown-linux
        TargetPlatform=sparc-unknown-linux
        BuildPlatform=sparc-unknown-linux
        HostPlatform_CPP='sparc_unknown_linux'
        HostArch_CPP='sparc'
        HostVendor_CPP='unknown'
        HostOS_CPP='linux'
        ;;
sparc-*-openbsd*)
        HostPlatform=sparc-unknown-openbsd
        TargetPlatform=sparc-unknown-openbsd
        BuildPlatform=sparc-unknown-openbsd
        HostPlatform_CPP='sparc_unknown_openbsd'
        HostArch_CPP='sparc'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
*)
        echo "Unrecognised platform: $HostPlatform"
        exit 1
        ;;
esac
echo "Canonicalised to: $HostPlatform"
test  x"$HostPlatform" != x"$TargetPlatform" && echo "Target platform set to $TargetPlatform"
test  x"$BuildPlatform" != x"$HostPlatform"  && echo "Build platform set to $BuildPlatform"

BuildPlatform_CPP=$HostPlatform_CPP
TargetPlatform_CPP=$HostPlatform_CPP
BuildArch_CPP=$HostArch_CPP
TargetArch_CPP=$HostArch_CPP
BuildOS_CPP=$HostOS_CPP
HostOS_Full=$host_os
TargetOS_CPP=$HostOS_CPP
BuildVendor_CPP=$HostVendor_CPP
TargetVendor_CPP=$HostVendor_CPP

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
AC_SUBST(HostOS_Full)
AC_SUBST(BuildOS_CPP)
AC_SUBST(TargetOS_CPP)

AC_SUBST(HostVendor_CPP)
AC_SUBST(BuildVendor_CPP)
AC_SUBST(TargetVendor_CPP)

AC_SUBST(exeext)

dnl --------------------------------------------------------------
dnl * Calculate absolute path to build tree
dnl --------------------------------------------------------------

AC_MSG_CHECKING(for path to top of build tree)

hardtop=`pwd`

dnl Remove common automounter nonsense
dnl
hardtop=`echo $hardtop | sed 's|^/tmp_mnt.*\(/local/.*\)$|\1|' | sed 's|^/tmp_mnt/|/|' | sed 's|^//\(.\)/|\1:/|' `

dnl Find 'hardtop_plat', the native format for 'hardtop' (i.e., right kind of \dnl slashes on a Win32 box, but with b-slashes being escaped).
dnl
case $HostPlatform in
  i386-unknown-mingw32 | i386-unknown-cygwin32)
  	# convert $hardtop to a path that mingw will understand too
	cyghardtop=${hardtop}
	hardtop=`cygpath -w ${cyghardtop} | sed -e 's@\\\\@/@g'`
  	hardtop_plat=`cygpath -w ${cyghardtop} | sed -e 's@\\\\@\\\\\\\\@g'`
	;;
  *)
  	hardtop_plat=${hardtop}
	;;
esac
AC_SUBST(hardtop)
AC_SUBST(hardtop_plat)

AC_MSG_RESULT(${hardtop})

dnl --------------------------------------------------------------
dnl * Project specific configuration options
dnl --------------------------------------------------------------
dnl What follows is a bunch of options that can either be configured
dnl through command line options to the configure script or by
dnl supplying defns in the build tree's mk/build.mk. Having the option to
dnl use either is considered a Feature.

dnl ** What command to use to compile compiler sources ?
dnl --------------------------------------------------------------

AC_ARG_WITH(ghc,
[  --with-ghc=<haskell compiler>
        Use a command different from 'ghc' to compile GHC-specific Haskell code
	    (including GHC itself).
],
[ WithGhc="$withval" ],
[ 
  if test "$GHC" = ""; then
    AC_PATH_PROG(GHC,ghc)
  fi
  WithGhc=$GHC
]
)
AC_SUBST(WithGhc)

AC_ARG_WITH(hc,
[  --with-hc=<haskell compiler>
        Use a command different from 'ghc' to compile generic Haskell code.
],
[WithHc="$withval"],
[WithHc=$WithGhc]
)
AC_SUBST(WithHc)

if test "$WithGhc" != ""; then
  FPTOOLS_GHC_VERSION([GhcVersion], [GhcMajVersion], [GhcMinVersion], [GhcPatchLevel])dnl
  AC_SUBST(GhcVersion)dnl
  AC_SUBST(GhcMajVersion)dnl
  AC_SUBST(GhcMinVersion)dnl
  AC_SUBST(GhcPatchLevel)dnl
fi

AC_PATH_PROGS(NHC,nhc nhc98)
AC_PATH_PROG(HBC,hbc)

dnl ** Which gcc to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(gcc,
[  --with-gcc=<gcc command>
        Use a different command instead of 'gcc' for the GNU C compiler.
],
[WhatGccIsCalled="$withval"
 if test "x$HostPlatform" = "xi386-unknown-mingw32"; then
    # Canonicalise to <drive>:/path/to/gcc
    withval=`cygpath -w ${withval} | sed -e 's@\\\\@/@g' `
 fi;
 CC="$withval"],
[WhatGccIsCalled="gcc"]
)
AC_SUBST(WhatGccIsCalled)

dnl ** Booting from .hc files?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(hc-boot,
[  --enable-hc-boot
        Boot the Glasgow Haskell Compiler from intermediate .hc files.
        (This option is mostly of interest to porters.)
],
[BootingFromHc=YES],
[BootingFromHc=NO]
)
AC_SUBST(BootingFromHc)

dnl ** Booting from unregisterised .hc files?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(hc-boot-unregisterised,
[  --enable-hc-boot-unregisterised
        With --enable-hc-boot, treat the intermediate .hc files as
        unregisterised rather than registerised code.
        (This option is mostly of interest to porters.)
],
[BootingFromUnregisterisedHc=YES],
[BootingFromUnregisterisedHc=NO]
)
AC_SUBST(BootingFromUnregisterisedHc)

if test "$BootingFromHc" = "NO"; then
if test "$BootingFromUnregisterisedHc" = "YES"; then
AC_MSG_ERROR([--enable-hc-boot-unregisterised requires --enable-hc-boot.])
fi;
fi;

dnl ** Must have GHC to build GHC, unless --enable-hc-boot is on
if test "$BootingFromHc" = "NO" -a "$WithGhc" = "" -a -d "$srcdir/ghc"; then
AC_MSG_ERROR([GHC is required unless bootstrapping from .hc files.])
fi;

dnl ** Enable multi-thread friendly RTS?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(threaded-rts,
[  --enable-threaded-rts
        Support better interop with OS threads.
],
[ThreadedRts=YES],
[ThreadedRts=NO]
)
AC_SUBST(ThreadedRts)

dnl ** Enable the construction of Win32 DLLs?
dnl --------------------------------------------------------------
dnl
dnl [ The ability to build the RTS and libraries as separate DLLs used
dnl   to be supported, but isn't currently (updates to the RTS, compiler 
dnl   and build system would be required to bring it back again.)
dnl   Hence, exposing it as an option is false advertisement, so better
dnl   disable it to avoid confusion (but leave it commented-out rather
dnl   than removed in order to remind ourselves to bring back the 
dnl   feature at some stage.) ]
dnl
dnl AC_ARG_ENABLE(win32-dlls,
dnl [  --enable-win32-dlls
dnl         If on a Win32 platform running mingw32/cygwin, enable the
dnl 	construction of DLLs containing ghc-compiled code.
dnl ],
dnl [
dnl case $HostOS_CPP in
dnl cygwin32) ;;
dnl mingw32)  ;;
dnl *)    echo "Unrecognised win32 platform: $HostPlatform"
dnl       exit 1
dnl       ;;
dnl esac
dnl EnableWin32DLLs=YES
dnl ],
dnl [EnableWin32DLLs=NO]
dnl )
dnl AC_SUBST(EnableWin32DLLs)
dnl if test x"$EnableWin32DLLs" = "xYES" ; then
dnl  AC_DEFINE(HAVE_WIN32_DLL_SUPPORT)
dnl fi

dnl ** Enable the building of the ObjectIO?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(objectio,
[  --enable-objectio
        Build ObjectIO, a portable GUI library for Haskell.
],
[GhcLibsWithObjectIO=YES],
[GhcLibsWithObjectIO=NO]
)
AC_SUBST(GhcLibsWithObjectIO)

dnl ** Enable the building of the OpenGL/GLUT binding in hslibs?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(hopengl,
[  --enable-hopengl
        Build HOpenGL, a Haskell binding for OpenGL/GLUT.
        On Mac OS X, use --enable-hopengl=x11 to use X11 instead
        of the "native" libraries.
],
[GhcLibsWithHOpenGL=YES],
[GhcLibsWithHOpenGL=NO]
)
UseQuartzOpenGL=NO
if test x"$TargetOS_CPP" = x"darwin"; then
  if test x"$enable_hopengl" != x"x11"; then
    AC_DEFINE([USE_QUARTZ_OPENGL], [1],
              [Define to 1 if native OpenGL should be used on Mac OS X])
    UseQuartzOpenGL=YES
  fi
fi

AC_SUBST(GhcLibsWithHOpenGL)

dnl ** .NET interop support?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(dotnet,
[  --enable-dotnet
        Build .NET interop layer.
],
[DotnetSupport=YES; AC_DEFINE([WANT_DOTNET_SUPPORT], [1], [Define to 1 if you want to include .NET interop support.])],
[DotnetSupport=NO]
)
AC_SUBST(DotnetSupport)

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------


dnl --------------------------------------------------------------
dnl * General configuration checks
dnl --------------------------------------------------------------

dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** look for `perl', but only in /bin on Windows
case $HostOS_CPP in
cygwin32|mingw32)
      AC_CHECK_PROG(PerlCmd,perl,/bin/perl,,/bin)
      if test -z "$PerlCmd"; then
         echo "You must install the version of Perl shipped with GHC"
         echo "(or a compatible one) in /bin."
         exit 1
      fi
   ;;
*)
   AC_PATH_PROG(PerlCmd,perl)
   if test -z "$PerlCmd"; then
      echo "You must install perl before you can continue"
      echo "Perhaps it is already installed, but not in your PATH?"
      exit 1
   else
   FPTOOLS_CHECK_PERL_VERSION
   fi
   ;;
esac

dnl ** does #! path/to/perl work? (sometimes it's too long...)
FPTOOLS_SHEBANG_PERL

dnl ** check for Python
AC_PATH_PROG(PythonCmd,python)

dnl ** look for GCC and find out which version
dnl     Figure out which C compiler to use.  Gcc is preferred.
dnl     If gcc, make sure it's at least 2.1
dnl
AC_PROG_CC
FPTOOLS_HAVE_GCC
FPTOOLS_GCC_NEEDS_NO_OMIT_LFPTR

dnl ** figure out how to invoke cpp directly (gcc -E is no good)
AC_PROG_CPP

dnl ** Without optimization some INLINE trickery fails for GHCi
SRC_CC_OPTS="-O"

dnl ** Try to add -mno-cygwin to the C compiler options
FP_CHECK_FLAG([-mno-cygwin], [
SRC_CC_OPTS="-mno-cygwin $SRC_CC_OPTS"
CPPFLAGS="-mno-cygwin $CPPFLAGS"])
AC_SUBST(SRC_CC_OPTS)

dnl ** figure out how to do context diffs
FP_PROG_CONTEXT_DIFF

dnl ** Find find command (for Win32's benefit)
FPTOOLS_FIND_FIND

dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl If you can run configure, you certainly have /bin/sh
AC_DEFINE([HAVE_BIN_SH], [1], [Define to 1 if you have /bin/sh.])

dnl ** how to invoke `ar' and `ranlib'
FPTOOLS_PROG_AR_AND_RANLIB

dnl ** Check to see whether ln -s works
AC_PROG_LN_S


dnl ** Find the path to sed
AC_PATH_PROG(SedCmd,sed)


dnl ** check for time command
AC_PATH_PROG(TimeCmd,time)

dnl ** check for tar
dnl   if GNU tar is named gtar, look for it first.
AC_PATH_PROGS(TarCmd,gtar tar,tar)

dnl ** check for jade/openjade & determine a working catalog
AC_PATH_PROGS(JadeCmd,openjade jade,jade)
FPTOOLS_DOCBOOK_CATALOG(Catalog, $JadeCmd, docs/fptools-both.dsl, 
                        /etc/sgml/catalog /etc/sgml.catalog /usr/share/sgml/CATALOG.docbkdsl /usr/local/share/sgml/catalog glafp-utils/docbook/CATALOG*)
if test -z "$Catalog"; then
  AC_MSG_RESULT([Warning: You will not be able to build the documentation.])
fi
case $Catalog in
   yes) # assume it is provided by other means (e.g., SGML_CATALOG_FILES env var).
        Catalog=
   	;;
   glafp*) 
	case $HostOS_CPP in
	   mingw32)
	      Catalog=`cygpath -w $hardtop/$Catalog`
	      ;;
	   *) Catalog=$hardtop/$Catalog
	      ;;
	esac
   ;;
esac   
AC_SUBST(Catalog)


dnl ** check for ghc-pkg command
changequote(, )dnl
ghc_pkg_guess=`echo $WithGhc | sed 's@ghc\([^/\\]*\)$@ghc-pkg\1@'`
changequote([, ])dnl
if $ghc_pkg_guess -l >/dev/null 2>/dev/null; then
   GhcPkgCmd=$ghc_pkg_guess
   AC_MSG_NOTICE([using $ghc_pkg_guess for ghc-pkg])
else
   AC_PATH_PROG(GhcPkgCmd,ghc-pkg)
fi

AC_ARG_WITH(greencard,
[  --with-greencard=<greencard compiler>
        Use a command different from 'greencard' to compile GreenCard files
],
[
GreenCardCmd=$withval;
FPTOOLS_GREENCARD(3.00)
]
)

AC_ARG_ENABLE(src-tree-happy,
[  --enable-src-tree-happy
        Build and use source tree (fptools/happy) version of Happy.
],
[UseSrcTreeHappy=YES],
[UseSrcTreeHappy=NO]
)
dnl ** check for installed happy binary + version
dnl    (don't do it if we're booting from .hc files though.)
if test "$BootingFromHc" = "NO"; then
FPTOOLS_HAPPY
fi;

AC_ARG_ENABLE(src-tree-haddock,
[  --enable-src-tree-haddock
        Build and use source tree (fptools/haddock) version of Haddock.
],
[UseSrcTreeHaddock=YES],
[UseSrcTreeHaddock=NO]
)
dnl ** check for installed haddock
FPTOOLS_HADDOCK

AC_ARG_ENABLE(src-tree-alex,
[  --enable-src-tree-alex
        Build and use source tree (fptools/alex) version of Alex.
],
[UseSrcTreeAlex=YES],
[UseSrcTreeAlex=NO]
)
dnl ** check for installed alex binary + version
dnl    (don't do it if we're booting from .hc files though.)
if test "$BootingFromHc" = "NO"; then
FPTOOLS_ALEX
fi;

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
AC_CHECK_HEADERS(Files.h arpa/inet.h assert.h console.h ctype.h dirent.h errno.h fcntl.h float.h ftw.h grp.h ieee754.h inttypes.h limits.h locale.h malloc.h memory.h nlist.h pascal.h pwd.h sgtty.h siginfo.h signal.h stat.h stdint.h stdlib.h stddef.h stdarg.h string.h sys/fault.h sys/file.h sys/ioctl.h sys/limits.h sys/mman.h sys/param.h sys/procfs.h sys/resource.h sys/signal.h sys/socket.h netdb.h netinet/in.h netinet/tcp.h sys/stat.h sys/syscall.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/types.h sys/un.h sys/utsname.h sys/vadvise.h sys/wait.h termio.h termios.h time.h types.h unistd.h utime.h values.h bfd.h winsock.h pthread.h sys/uio.h)
AC_CHECK_HEADER(unistd.h,[AC_CHECK_FUNCS(lchown)])

AC_CHECK_HEADER(readline/readline.h, [HaveReadlineReadlineH=YES], [HaveReadlineReadlineH=NO])
AC_CHECK_HEADER(readline/history.h, [HaveReadlineHistoryH=YES], [HaveReadlineHistoryH=NO])

if test $HaveReadlineReadlineH = YES && test $HaveReadlineHistoryH = YES ; then
  HaveReadlineHeaders=YES
  AC_DEFINE([HAVE_READLINE_HEADERS], [1], [Define to 1 if readline/readline.h and readline/history.h exist.])
else
  HaveReadlineHeaders=NO
  AC_DEFINE([HAVE_READLINE_HEADERS], [0], [Define to 1 if readline/readline.h and readline/history.h exist.])
fi
AC_SUBST(HaveReadlineHeaders)

dnl ** check for DOS include files
AC_CHECK_HEADERS(dos.h conio.h io.h std.h) 

dnl ** check for Windows include files
AC_CHECK_HEADERS(windows.h)

dnl ** check for OpenGL include files
if test $UseQuartzOpenGL = YES ; then
  AC_CHECK_HEADERS(OpenGL/gl.h)
else
  AC_CHECK_HEADERS(GL/gl.h)
fi

dnl ** check if it is safe to include both <time.h> and <sys/time.h>
AC_HEADER_TIME

dnl dynamic loading include files
AC_CHECK_HEADERS(dlfcn.h dl.h) 

dnl ** check for farcalloc (in bcc)
AC_CHECK_HEADER(alloc.h,[AC_CHECK_FUNCS(farcalloc)])

dnl ** check for valloc (in sunos, solaris, mips, amiga, next, minix, ultrix)
AC_CHECK_HEADER(malloc.h,[AC_CHECK_FUNCS(valloc)])

dnl ** check for POSIX regex
HavePosixRegex=NO
AC_CHECK_HEADERS(regex.h,[AC_CHECK_FUNC(regcomp, [HavePosixRegex=YES])])
AC_SUBST(HavePosixRegex)

dnl ** how do we get a timezone name, and UTC offset ?
AC_STRUCT_TIMEZONE

dnl ** do we have altzone?
FP_DECL_ALTZONE

dnl ** does struct stat contain st_blksize?
AC_STRUCT_ST_BLKSIZE

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** check what fields struct msghdr contains
AC_CHECK_HEADERS([sys/types.h sys/socket.h sys/uio.h])
AC_CHECK_MEMBERS([struct msghdr.msg_control, struct msghdr.msg_accrights], [], [], [#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif])

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

dnl ** map standard C types and ISO types to Haskell types
FPTOOLS_CHECK_HTYPE(char)
FPTOOLS_CHECK_HTYPE(signed char)
FPTOOLS_CHECK_HTYPE(unsigned char)
FPTOOLS_CHECK_HTYPE(short)
FPTOOLS_CHECK_HTYPE(unsigned short)
FPTOOLS_CHECK_HTYPE(int)
FPTOOLS_CHECK_HTYPE(unsigned int)
FPTOOLS_CHECK_HTYPE(long)
FPTOOLS_CHECK_HTYPE(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FPTOOLS_CHECK_HTYPE(long long)
FPTOOLS_CHECK_HTYPE(unsigned long long)
fi
FPTOOLS_CHECK_HTYPE(float)
FPTOOLS_CHECK_HTYPE(double)
FPTOOLS_CHECK_HTYPE(ptrdiff_t)
FPTOOLS_CHECK_HTYPE(size_t)
FPTOOLS_CHECK_HTYPE(wchar_t)
dnl Int32 is a HACK for non-ISO C compilers
FPTOOLS_CHECK_HTYPE(sig_atomic_t, Int32)
FPTOOLS_CHECK_HTYPE(clock_t)
FPTOOLS_CHECK_HTYPE(time_t)
FPTOOLS_CHECK_HTYPE(dev_t, Word32)
FPTOOLS_CHECK_HTYPE(ino_t)
FPTOOLS_CHECK_HTYPE(mode_t)
FPTOOLS_CHECK_HTYPE(off_t)
FPTOOLS_CHECK_HTYPE(pid_t)
FPTOOLS_CHECK_HTYPE(gid_t)
FPTOOLS_CHECK_HTYPE(uid_t)
FPTOOLS_CHECK_HTYPE(cc_t)
FPTOOLS_CHECK_HTYPE(speed_t)
FPTOOLS_CHECK_HTYPE(tcflag_t)
FPTOOLS_CHECK_HTYPE(blkcnt_t)
FPTOOLS_CHECK_HTYPE(nlink_t)
FPTOOLS_CHECK_HTYPE(ssize_t)
FPTOOLS_CHECK_HTYPE(rlim_t)
FPTOOLS_CHECK_HTYPE(wint_t)

dnl ** Map OpenGL data types to Haskell types
if test $GhcLibsWithHOpenGL = YES ; then
FPTOOLS_CHECK_HTYPE(GLboolean)
FPTOOLS_CHECK_HTYPE(GLbyte)
FPTOOLS_CHECK_HTYPE(GLubyte)
FPTOOLS_CHECK_HTYPE(GLshort)
FPTOOLS_CHECK_HTYPE(GLushort)
FPTOOLS_CHECK_HTYPE(GLint)
FPTOOLS_CHECK_HTYPE(GLuint)
FPTOOLS_CHECK_HTYPE(GLsizei)
FPTOOLS_CHECK_HTYPE(GLenum)
FPTOOLS_CHECK_HTYPE(GLbitfield)
FPTOOLS_CHECK_HTYPE(GLfloat)
FPTOOLS_CHECK_HTYPE(GLclampf)
FPTOOLS_CHECK_HTYPE(GLdouble)
FPTOOLS_CHECK_HTYPE(GLclampd)
fi

FP_CHECK_CONSTS([E2BIG EACCES EADDRINUSE EADDRNOTAVAIL EADV EAFNOSUPPORT EAGAIN EALREADY EBADF EBADMSG EBADRPC EBUSY ECHILD ECOMM ECONNABORTED ECONNREFUSED ECONNRESET EDEADLK EDESTADDRREQ EDIRTY EDOM EDQUOT EEXIST EFAULT EFBIG EFTYPE EHOSTDOWN EHOSTUNREACH EIDRM EILSEQ EINPROGRESS EINTR EINVAL EIO EISCONN EISDIR ELOOP EMFILE EMLINK EMSGSIZE EMULTIHOP ENAMETOOLONG ENETDOWN ENETRESET ENETUNREACH ENFILE ENOBUFS ENODATA ENODEV ENOENT ENOEXEC ENOLCK ENOLINK ENOMEM ENOMSG ENONET ENOPROTOOPT ENOSPC ENOSR ENOSTR ENOSYS ENOTBLK ENOTCONN ENOTDIR ENOTEMPTY ENOTSOCK ENOTTY ENXIO EOPNOTSUPP EPERM EPFNOSUPPORT EPIPE EPROCLIM EPROCUNAVAIL EPROGMISMATCH EPROGUNAVAIL EPROTO EPROTONOSUPPORT EPROTOTYPE ERANGE EREMCHG EREMOTE EROFS ERPCMISMATCH ERREMOTE ESHUTDOWN ESOCKTNOSUPPORT ESPIPE ESRCH ESRMNT ESTALE ETIME ETIMEDOUT ETOOMANYREFS ETXTBSY EUSERS EWOULDBLOCK EXDEV], [#include <stdio.h>
#include <errno.h>])

dnl ** can we open files in binary mode?
FP_CHECK_CONST([O_BINARY], [#include <fcntl.h>], [0])

dnl ** check for GetModuleFileName and WinExec (windows only)
dnl Old: AC_CHECK_FUNCS(WinExec GetModuleFileName)       dnl Windows
dnl Doesn't work because the linker can't see the functions if
dnl you omit the #include <windows.h>.  (I've no idea why not...)

FPTOOLS_TRY_LINK_NOWARN(,[
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
main() { 
  WinExec("",0);
  exit(0);
}
],
[have_winexec=1],
[have_winexec=0])
if test "$have_winexec" = "1"; then
  AC_DEFINE([HAVE_WINEXEC], [1], [Define to 1 if you have the WinExec function.])
fi

FPTOOLS_TRY_LINK_NOWARN(,[
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
main() { 
  char* dir;
  GetModuleFileName((HMODULE)0,dir,0);
  exit(0);
}
],
[have_getmodulefilename=1],
[have_getmodulefilename=0])
if test "$have_getmodulefilename" = "1"; then
AC_DEFINE([HAVE_GETMODULEFILENAME], [1], [Define to 1 if you have the GetModuleFileName function.])
fi

dnl ** check return type of signal handlers
dnl Foo: assumes we can use prototypes.
dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
dnl AC_CACHE_CHECK([type of signal handlers], ac_cv_type_signal_handler,
dnl [AC_TRY_COMPILE([#include <sys/types.h>
dnl #include <signal.h>
dnl #ifdef signal
dnl #undef signal
dnl #endif
dnl void (*signal (int, void (*)(int)))(int);
dnl ],
dnl [int i;], 
dnl ac_cv_type_signal_handler=void_int,
dnl ac_cv_type_signal_handler=int_void)])
dnl if test "$ac_cv_type_signal_handler" = void_int; then
dnl AC_DEFINE(VOID_INT_SIGNALS)
dnl fi

dnl On BCC, signal handlers have type "int(void)", elsewhere its "void(int)".
AC_TYPE_SIGNAL
if test "$ac_cv_type_signal" = void; then
  AC_DEFINE([VOID_INT_SIGNALS], [1], [Define to 1 if signal handlers have type void (*)(int). Otherwise, they're assumed to have type int (*)(void).])
fi

dnl ** check for more functions
AC_CHECK_FUNCS(strcasecmp _stricmp stricmp strcmpi)
AC_CHECK_FUNCS(strcmp)
AC_CHECK_FUNCS(realpath _fullpath)  
AC_CHECK_FUNCS(PBHSetVolSync macsystem)  
AC_CHECK_FUNCS(fgetpos fsetpos fseek ftell)
AC_CHECK_FUNCS(vsnprintf _vsnprintf)
AC_CHECK_FUNCS(snprintf  _snprintf )  
AC_CHECK_FUNCS(popen     _popen )  
AC_CHECK_FUNCS(pclose    _pclose )  
AC_CHECK_FUNCS(setenv unsetenv)


dnl ** check for specific library functions that we are interested in
AC_CHECK_FUNCS(access ftime getclock getpagesize getrusage gettimeofday mktime mprotect readlink setitimer stat lstat siginterrupt symlink sysconf timelocal times vadvise localtime_r gmtime_r readdir_r getgrgid_r getgrnam_r getpwuid_r getpwnam_r)

dnl ** Solaris2 needs additionl flag for getpw*_r()
case "$TargetPlatform" in
  *-solaris2*)
    unix_SRC_HSC2HS_OPTS="-D_POSIX_PTHREAD_SEMANTICS"
    AC_SUBST(unix_SRC_HSC2HS_OPTS)
  ;;
esac

dnl ** check whether this machine has gmp3 installed
AC_CHECK_LIB(gmp,  __gmpz_fdiv_qr, HaveLibGmp=YES; LibGmp=gmp,
  AC_CHECK_LIB(gmp3, __gmpz_fdiv_qr,  HaveLibGmp=YES; LibGmp=gmp3,
    HaveLibGmp=NO; LibGmp=not-installed))
AC_SUBST(HaveLibGmp)
AC_SUBST(LibGmp)

dnl ** (Mac OS X only: check for HaskellSupport.framework)
HaveFrameworkHaskellSupport=NO
if test $HostPlatform = "powerpc-apple-darwin"; then
 AC_MSG_CHECKING([for HaskellSupport.framework])
 save_libs="$LIBS"
 LIBS="-framework HaskellSupport"
 AC_TRY_LINK_FUNC(__gmpz_fdiv_qr, HaveFrameworkHaskellSupport=YES,)
 if test $HaveFrameworkHaskellSupport = YES; then
  AC_DEFINE([HAVE_FRAMEWORK_HASKELLSUPPORT], [1], [Define to 1 if the HaskellSupport.framework is installed (Mac OS X only).])
 fi;
 LIBS="$save_libs"
 AC_MSG_RESULT([$HaveFrameworkHaskellSupport])
fi;
AC_SUBST(HaveFrameworkHaskellSupport)

dnl ** check for mingwex library
AC_CHECK_LIB(mingwex, closedir, HaveLibMingwEx=YES, HaveLibMingwEx=NO)
AC_SUBST(HaveLibMingwEx)

if test $HaveLibMingwEx = YES ; then
  AC_DEFINE([HAVE_MINGWEX], [1], [Define to 1 if you have the mingwex library.])
fi

if test "$HaveLibGmp" = "NO"; then
if test "$HostArch_CPP" = "ia64"; then
AC_MSG_ERROR([You need to install libgmp (the in-tree version does not work on IA64).])
fi;
fi;

dnl ** check whether this machine has BFD and liberty installed (used for debugging)
dnl    the order of these tests matters: bfd needs liberty
AC_CHECK_LIB(iberty, xmalloc)
AC_CHECK_LIB(bfd,    bfd_init)

dnl ** check for wide-char classifications
dnl FreeBSD has an emtpy wctype.h, so test one of the affected
dnl functions if it's really there.
AC_CHECK_HEADERS(wctype.h,
  [AC_CHECK_FUNCS(iswspace)]
)

dnl ** check for readline, for Hugs and hslibs' Readline
dnl ncurses supersedes termcap and curses, but for compatibility,
dnl we have to check for all...
AC_CHECK_LIB(ncurses, tputs, HaveLibTermcap=YES; LibTermcap=ncurses,
  AC_CHECK_LIB(termcap, tputs, HaveLibTermcap=YES; LibTermcap=termcap,
    AC_CHECK_LIB(curses, tputs, HaveLibTermcap=YES; LibTermcap=curses,
      HaveLibTermcap=NO; LibTermcap=not-installed)))

if test $HaveLibTermcap = YES ; then
  LIBS="-l$LibTermcap $LIBS"
  AC_CHECK_LIB(readline, readline, HaveLibReadline=YES, HaveLibReadline=NO)
fi

if test $HaveLibTermcap = YES && test x"$HaveLibReadline" = xYES ; then
  AC_DEFINE([HAVE_READLINE_LIBS], [1], [Define to 1 if readline plus any additional libs needed for it exist.])
  LibsReadline="readline $LibTermcap"
else
  AC_DEFINE([HAVE_READLINE_LIBS], [0], [Define to 1 if readline plus any additional libs needed for it exist.])
  LibsReadline=
fi
AC_SUBST(LibsReadline)

if test "$HaveLibReadline"; then
  AC_CHECK_LIB(readline, rl_erase_empty_line,
    [AC_DEFINE([HAVE_READLINE_4], [1], [Define to 1 if readline has version >= 4.0.])],
    [AC_DEFINE([HAVE_READLINE_4], [0], [Define to 1 if readline has version >= 4.0.])])
  AC_CHECK_LIB(readline, rl_free_undo_list,
    [AC_DEFINE([HAVE_READLINE_4_2], [1], [Define to 1 if readline has version >= 4.2.])],
    [AC_DEFINE([HAVE_READLINE_4_2], [0], [Define to 1 if readline has version >= 4.2.])])
else
  AC_DEFINE([HAVE_READLINE_4], [0], [Define to 1 if readline has version >= 4.0.])
  AC_DEFINE([HAVE_READLINE_4_2], [0], [Define to 1 if readline has version >= 4.2.])
fi

dnl ** check for math library
FPTOOLS_CHECK_LIBM()
AC_SUBST(LIBM)

dnl ** check for X Window System
AC_PATH_XTRA()

dnl ** check for OpenGL/GLUT/GLX include paths and libraries
if test $GhcLibsWithHOpenGL = YES ; then
  if test $UseQuartzOpenGL = YES ; then
    dnl Take a shortcut for Mac OS X native OpenGL:
    dnl It's in a non-standard location, but it's always there
    GL_CFLAGS=
	GL_LIBS=
	GLUT_LIBS=
	AC_SUBST(GL_CFLAGS)
	AC_SUBST(GL_LIBS)
	AC_SUBST(GLUT_LIBS)
  else
    FPTOOLS_HAVE_OPENGL
  fi
fi

dnl ################################################################
dnl Check for libraries
dnl ################################################################

dnl ** check for libdl & RTLD_NEXT

dnl (Mac OS X only) ... but don't check if we already have the
dnl HaskellSupport.framework

if test $HaveFrameworkHaskellSupport = YES; then
    HaveLibDL=NO
    HaveRtldNext=NO
    HaveRtldLocal=YES
    AC_DEFINE([HAVE_RTLDLOCAL], [1], [Define to 1 if RTLD_LOCAL is available.])
    HaveRtldGlobal=YES
    AC_DEFINE([HAVE_RTLDGLOBAL], [1], [Define to 1 if RTLD_GLOBAL is available.])
    HaveRtldNow=YES
    AC_DEFINE([HAVE_RTLDNOW], [1], [Define to 1 if we can see RTLD_NOW in dlfcn.h.])
    AC_SUBST(HaveLibDL)
    AC_SUBST(HaveRtldNext)
    AC_SUBST(HaveRtldLocal)
    AC_SUBST(HaveRtldGlobal)
    AC_SUBST(HaveRtldNow)
else
    AC_CHECK_LIB(dl, dlopen, 
    	[HaveLibDL=YES
    	 AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
    	 LIBS="$LIBS -ldl"], 
    	[HaveLibDL=NO])
    AC_CHECK_FUNCS(dlopen)
    AC_SUBST(HaveLibDL)

    dnl ** sometimes RTLD_NEXT is hidden in #ifdefs we really don't wan to set
    AC_MSG_CHECKING(for RTLD_NEXT from dlfcn.h)
    AC_EGREP_CPP(yes,
    [
     #include <dlfcn.h>
     #ifdef RTLD_NEXT
            yes
     #endif
    ], [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_RTLDNEXT], [1], [Define to 1 if we can see RTLD_NEXT in dlfcn.h.])
      HaveRtldNext=YES
    ], [
      AC_MSG_RESULT(no)
      HaveRtldNext=NO
      ])    
    AC_SUBST(HaveRtldNext)

    dnl ** RTLD_LOCAL isn't available on cygwin or openbsd
    AC_MSG_CHECKING(for RTLD_LOCAL from dlfcn.h)
    AC_EGREP_CPP(yes,
    [
     #include <dlfcn.h>
     #ifdef RTLD_LOCAL
            yes
     #endif
    ], [
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_RTLDLOCAL)
      HaveRtldLocal=YES
    ], [
      AC_MSG_RESULT(no)
      HaveRtldLocal=NO
      ])    
    AC_SUBST(HaveRtldLocal)

    dnl ** RTLD_GLOBAL isn't available on openbsd
    AC_MSG_CHECKING(for RTLD_GLOBAL from dlfcn.h)
    AC_EGREP_CPP(yes,
    [
     #include <dlfcn.h>
     #ifdef RTLD_GLOBAL
            yes
     #endif
    ], [
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_RTLDGLOBAL)
      HaveRtldGlobal=YES
    ], [
      AC_MSG_RESULT(no)
      HaveRtldGlobal=NO
      ])    
    AC_SUBST(HaveRtldGlobal)

    dnl ** RTLD_NOW isn't available on openbsd
    AC_MSG_CHECKING(for RTLD_NOW from dlfcn.h)
    AC_EGREP_CPP(yes,
    [
     #include <dlfcn.h>
     #ifdef RTLD_NOW
            yes
     #endif
    ], [
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_RTLDNOW)
      HaveRtldNow=YES
    ], [
      AC_MSG_RESULT(no)
      HaveRtldNow=NO
      ])    
    AC_SUBST(HaveRtldNow)
fi

FPTOOLS_CHECK_LIB_NOWARN(m, atan)

dnl ---------- usleep ----------
dnl --- stolen from guile configure ---
dnl --- FIXME: /usr/include/unistd.h can't be right?

### On some systems usleep has no return value.  If it does have one,
### we'd like to return it; otherwise, we'll fake it.
AC_CACHE_CHECK([return type of usleep], cv_func_usleep_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+usleep>changequote([, ]),
                   /usr/include/unistd.h,
                   [cv_func_usleep_return_type=void],
                   [cv_func_usleep_return_type=int])])
case "$cv_func_usleep_return_type" in
  "void" )
    AC_DEFINE([USLEEP_RETURNS_VOID], [1], [Define if the system headers declare usleep to return void.])
  ;;
esac

dnl --------------------------------------------------
dnl * test for in_addr_t
dnl --------------------------------------------------
AC_MSG_CHECKING(for in_addr_t in netinet/in.h)
AC_EGREP_HEADER(in_addr_t, netinet/in.h,
 [ AC_DEFINE([HAVE_IN_ADDR_T], [1], [Define to 1 if in_addr_t is available.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

dnl --------------------------------------------------
dnl * test for Linux sendfile(2)
dnl --------------------------------------------------
AC_MSG_CHECKING(for sendfile in sys/sendfile.h)
AC_EGREP_HEADER(sendfile, sys/sendfile.h,
 [ AC_DEFINE([HAVE_LINUX_SENDFILE], [1], [Define to 1 if you have a Linux sendfile(2) implementation.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

dnl --------------------------------------------------
dnl * test for BSD sendfile(2)
dnl --------------------------------------------------
AC_MSG_CHECKING(for sendfile in sys/socket.h)
AC_EGREP_HEADER(sendfile, sys/socket.h,
 [ AC_DEFINE([HAVE_BSD_SENDFILE], [1], [Define to 1 if you have a BSDish sendfile(2) implementation.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

dnl --------------------------------------------------
dnl * test for GTK+
dnl --------------------------------------------------

AC_PATH_PROGS(GTK_CONFIG, gtk-config gtk12-config)
if test "$GTK_CONFIG" != ""; then
   AC_CACHE_CHECK([for version of GTK+], fptools_cv_gtk_version, [
	fptools_cv_gtk_version=`$GTK_CONFIG --version`
    ])
    GTK_VERSION=$fptools_cv_gtk_version
else
    GTK_VERSION=
fi

case $fptools_cv_gtk_version in
  1.[[23]].*) ;;
  *) AC_MSG_WARN([GTK+ not usable; need at least version 1.2])
     GTK_CONFIG=
     ;;
esac

AC_SUBST(GTK_CONFIG)
AC_SUBST(GTK_VERSION)

dnl --------------------------------------------------
dnl * Miscellaneous feature tests
dnl --------------------------------------------------

dnl ** can we get alloca?
AC_FUNC_ALLOCA

dnl ** Working vfork?
AC_FUNC_VFORK

dnl ** determine whether or not const works
AC_C_CONST

dnl ** are we big endian?
AC_C_BIGENDIAN

dnl ** check for leading underscores in symbol names
FPTOOLS_UNDERSCORE

dnl ** check for ld, and whether ld has -x option
AC_PATH_PROG(LdCmdRaw, ld)
case $HostOS_CPP in
 mingw32) LdCmd=`cygpath -w ${LdCmdRaw} | sed -e 's@\\\\@/@g' `
 	   ;;
 *) LdCmd=${LdCmdRaw}
    ;;
esac
AC_SUBST(LdCmd)
FPTOOLS_LD_X

AC_MSG_CHECKING([for SIGPOLL])
AC_EGREP_CPP(we_have_sigpoll,
[#include <signal.h>
#ifdef SIGPOLL
we_have_sigpoll
#endif
], AC_DEFINE([HAVE_SIGPOLL], [1], [Define to 1 if you have the sigpoll() function.]) haveSIGPOLL=yes, haveSIGPOLL=no)
AC_MSG_RESULT([$haveSIGPOLL])

AC_MSG_CHECKING([for _SC_GETGR_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETGR_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETGR_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETGR_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for _SC_GETPW_R_SIZE_MAX])
AC_EGREP_CPP(we_have_that_sysconf_thing,
[
#include <unistd.h>
#ifdef _SC_GETPW_R_SIZE_MAX
we_have_that_sysconf_thing
#endif
],
[AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_SC_GETPW_R_SIZE_MAX], [1], [Define to 1 if <unistd.h> defines _SC_GETPW_R_SIZE_MAX.])],
[AC_MSG_RESULT([no])])

AC_OUTPUT(mk/config.mk, echo timestamp > mk/stamp-h )
