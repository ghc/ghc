dnl == autoconf source for the Glasgow FP tools ==
dnl (run "grep '^dnl \*' configure.ac | sed -e 's/dnl / /g; s/\*\*/   +/g;'"
dnl  (or some such) to see the outline of this file)
dnl
#
# (c) The AQUA Project, Glasgow University, 1994-2004
#
# Configure script template for the Glasgow functional programming tools
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([The Glorious Glasgow Haskell Compilation System], [6.5], [glasgow-haskell-bugs@haskell.org], [ghc])

if test ! -f mk/config.h.in; then
   echo "mk/config.h.in doesn't exist: perhaps you haven't run 'autoreconf'?"
   exit 1
fi

FP_SETUP_PROJECT_VERSION

# Hmmm, we fix the RPM release number to 1 here... Is this convenient?
AC_SUBST([release], [1])

# First off, a distrib sanity check..
AC_CONFIG_SRCDIR([mk/config.mk.in])

dnl * We require autoconf version 2.52
dnl We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
AC_PREREQ([2.52])

dnl * Declare subdirectories that have a private configure script
dnl
dnl After the toplevel configuration is complete, the script will recurse into
dnl these subdirectories if they exist. The use of a cache file makes repeated
dnl checks cheap.
AC_CONFIG_SUBDIRS([libraries])

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
AC_CANONICAL_TARGET

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
alpha*-unknown-openbsd*)
        HostPlatform=alpha-unknown-openbsd
        TargetPlatform=alpha-unknown-openbsd
        BuildPlatform=alpha-unknown-openbsd
        HostPlatform_CPP='alpha_unknown_openbsd'
        HostArch_CPP='alpha'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
amd64-*-openbsd*|x86_64-*-openbsd*)
        HostPlatform=x86_64-unknown-openbsd
        TargetPlatform=x86_64-unknown-openbsd
        BuildPlatform=x86_64-unknown-openbsd
        HostPlatform_CPP='x86_64_unknown_openbsd'
        HostArch_CPP='x86_64'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
amd64-*-freebsd*)
        HostPlatform=x86_64-unknown-freebsd
        TargetPlatform=x86_64-unknown-freebsd
        BuildPlatform=x86_64-unknown-freebsd
        HostPlatform_CPP='x86_64_unknown_freebsd'
        HostArch_CPP='x86_64'
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
arm*-openbsd*)
        HostPlatform=arm-unknown-openbsd
        TargetPlatform=arm-unknown-openbsd
        BuildPlatform=arm-unknown-openbsd
        HostPlatform_CPP='arm_unknown_openbsd'
        HostArch_CPP='arm'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
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
hppa*-*-openbsd*)
        HostPlatform=hppa-unknown-openbsd # hack again
        TargetPlatform=hppa-unknown-openbsd
        BuildPlatform=hppa-unknown-openbsd
        HostPlatform_CPP='hppa_unknown_openbsd'
        HostArch_CPP='hppa'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
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
i[[3456]]86-*-kfreebsd*-gnu)
        HostPlatform=i386-unknown-kfreebsdgnu # hack again
        TargetPlatform=i386-unknown-kfreebsdgnu
        BuildPlatform=i386-unknown-kfreebsdgnu
        HostPlatform_CPP='i386_unknown_kfreebsdgnu'
        HostArch_CPP='i386'
        HostVendor_CPP='unknown'
        HostOS_CPP='kfreebsdgnu'
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
i[[3456]]86-apple-darwin*)
        HostPlatform=i386-apple-darwin
        TargetPlatform=i386-apple-darwin #hack
        BuildPlatform=i386-apple-darwin #hack
        HostPlatform_CPP='i386_apple_darwin'
        HostArch_CPP='i386'
        HostVendor_CPP='apple'
        HostOS_CPP='darwin'
        ;;
i[[3456]]86-*-gnu*)
        HostPlatform=i386-unknown-gnu
        TargetPlatform=i386-unknown-gnu
        BuildPlatform=i386-unknown-gnu
        HostPlatform_CPP=i386_unknown_gnu
        HostArch_CPP=i386
        HostVendor_CPP=unknown
        HostOS_CPP=gnu
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
m88k-*-openbsd*)
        HostPlatform=m88k-unknown-openbsd
        TargetPlatform=m88k-unknown-openbsd
        BuildPlatform=m88k-unknown-openbsd
        HostPlatform_CPP='m88k_unknown_openbsd'
        HostArch_CPP='m88k'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
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
mipsel-*-linux*)
        HostPlatform=mipsel-unknown-linux # hack again
        TargetPlatform=mipsel-unknown-linux
        BuildPlatform=mipsel-unknown-linux
        HostPlatform_CPP='mipsel_unknown_linux'
        HostArch_CPP='mipsel'
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
powerpc-unknown-openbsd*)
        HostPlatform=powerpc-unknown-openbsd
        TargetPlatform=powerpc-unknown-openbsd
        BuildPlatform=powerpc-unknown-openbsd
        HostPlatform_CPP='powerpc_unknown_openbsd'
        HostArch_CPP='powerpc'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
powerpc64-unknown-linux*)
        HostPlatform=powerpc64-unknown-linux
        TargetPlatform=powerpc64-unknown-linux
        BuildPlatform=powerpc64-unknown-linux
        HostPlatform_CPP='powerpc64_unknown_linux'
        HostArch_CPP='powerpc64'
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
sparc64-*-openbsd*)
        HostPlatform=sparc64-unknown-openbsd
        TargetPlatform=sparc64-unknown-openbsd
        BuildPlatform=sparc64-unknown-openbsd
        HostPlatform_CPP='sparc64_unknown_openbsd'
        HostArch_CPP='sparc64'
        HostVendor_CPP='unknown'
        HostOS_CPP='openbsd'
        ;;
vax-*-openbsd*)
        HostPlatform=vax-unknown-openbsd
        TargetPlatform=vax-unknown-openbsd
        BuildPlatform=vax-unknown-openbsd
        HostPlatform_CPP='vax_unknown_openbsd'
        HostArch_CPP='vax'
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

dnl Find 'hardtop_plat', the native format for 'hardtop'
dnl (i.e., right kind of \dnl slashes on a Win32 box, but with b-slashes
dnl being escaped).
dnl
dnl Note OSTYPE: On Cygwin we need to use 'cygpath' to convert
dnl              /cygdrive/c/foo to c:/foo but we must not do that if we
dnl              aren't building using Cygwin (notably msys), because
dnl              cygpath doesn't exist.  It seems that 'bash' sets
dnl              OSTYPE to 'cygwin' or 'msys' respectively, but cygwin's
dnl              'sh' does not.  So we hackily assume that if the shell
dnl              hasn't set it to 'msys' then we must be in Cygwin.
dnl              Sigh.
dnl
dnl              The Right Thing is probably to test $BuildPlatform
dnl              instead, but we are sloppy about setting that correctly
dnl              at the moment, so we just work around for now.
dnl
dnl              The quotes round "$(OSTYPE)" are essential, for the
dnl              Cygwin-sh case where OSTYPE is not set.
case $HostPlatform in
  i386-unknown-mingw32 | i386-unknown-cygwin32)
        if test "${OSTYPE}" != "msys"
          then
            # convert $hardtop to a path that mingw will understand too
            cyghardtop=${hardtop}
            hardtop=`cygpath -w ${cyghardtop} | sed -e 's@\\\\@/@g'`
            hardtop_plat=`cygpath -w ${cyghardtop} | sed -e 's@\\\\@\\\\\\\\@g'`
          else
dnl OK, so we're in the MSYS case.  hardtop looks like /c/....
dnl We want to make hardtop_plat into c:/...
dnl Stop using [] for quotes temporarily, so we can use [] in the sed regexp
changequote(, )dnl
               hardtop_plat=`echo ${hardtop} | sed -e 's@^/\\([a-zA-Z]\\)/@\\1:/@g'`
changequote([, ])dnl
        fi
        ;;
  *)
        hardtop_plat=${hardtop}
        ;;
esac
AC_SUBST(hardtop)
AC_SUBST(hardtop_plat)

AC_MSG_RESULT(${hardtop})

# We don't support building in directories with spaces.
case "$hardtop" in
  *' '*) AC_MSG_ERROR([
   The build system does not support building in a directory containing
   space characters.  Suggestion: move the build tree somewhere else.])
 ;;
esac

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
AC_SUBST([WithGhc])

AC_ARG_WITH(hc,
[AC_HELP_STRING([--with-hc=ARG],
        [Use ARG as the path to the compiler for compiling ordinary
         Haskell code  (default= value of --with-ghc)])],
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

# Check whether this GHC has readline installed
FP_GHC_HAS_READLINE

AC_PATH_PROGS(NHC,nhc nhc98)
AC_PATH_PROG(HBC,hbc)

dnl ** Which gcc to use?
dnl --------------------------------------------------------------
AC_ARG_WITH(gcc,
[AC_HELP_STRING([--with-gcc=ARG],
        [Use ARG as the path to GCC  [default=autodetect]])],
[WhatGccIsCalled="$withval"
 if test "x$HostPlatform" = "xi386-unknown-mingw32"
    then
       if test "${OSTYPE}" != "msys"
         then
         # Canonicalise to <drive>:/path/to/gcc
         withval=`cygpath -w ${withval} | sed -e 's@\\\\@/@g' `
       fi
 fi;
 CC="$withval"
 export CC
 ],
[WhatGccIsCalled="gcc"]
)
AC_SUBST(WhatGccIsCalled)

dnl ** Booting from .hc files?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(hc-boot,
[AC_HELP_STRING([--enable-hc-boot],
[Boot the Glasgow Haskell Compiler from intermediate .hc files.
 (This option is mostly of interest to porters.) [default=no]])],
[ if test x"$enableval" = x"yes"; then
        BootingFromHc=YES
  else
        BootingFromHc=NO
  fi
],
[BootingFromHc=NO]
)
AC_SUBST(BootingFromHc)

dnl ** Booting from unregisterised .hc files?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(hc-boot-unregisterised,
[AC_HELP_STRING([--enable-hc-boot-unregisterised],
[ With --enable-hc-boot, treat the intermediate .hc files as
 unregisterised rather than registerised code.
 (This option is mostly of interest to porters.) [default=no]])],
[ if test x"$enableval" = x"yes"; then
        BootingFromUnregisterisedHc=YES
  else
        BootingFromUnregisterisedHc=NO
  fi
],
[BootingFromUnregisterisedHc=NO]
)
AC_SUBST(BootingFromUnregisterisedHc)

if test "$BootingFromHc" = "NO"; then
if test "$BootingFromUnregisterisedHc" = "YES"; then
AC_MSG_ERROR([--enable-hc-boot-unregisterised requires --enable-hc-boot.])
fi;
fi;

dnl ** Must have GHC to build GHC, unless --enable-hc-boot is on
if test "$BootingFromHc" = "NO" -a "$WithGhc" = "" -a -d "$srcdir/compiler"; then
AC_MSG_ERROR([GHC is required unless bootstrapping from .hc files.])
fi;

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
dnl         construction of DLLs containing ghc-compiled code.
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
[AC_HELP_STRING([--enable-objectio],
[Build ObjectIO, a portable GUI library for Haskell. [default=no]])],
[ if test x"$enableval" = x"yes"; then
        GhcLibsWithObjectIO=YES
  else
        GhcLibsWithObjectIO=NO
  fi
],
[GhcLibsWithObjectIO=NO]
)
AC_SUBST(GhcLibsWithObjectIO)

dnl ** .NET interop support?
dnl --------------------------------------------------------------
AC_ARG_ENABLE(dotnet,
[AC_HELP_STRING([--enable-dotnet],
[Build .NET interop layer. [default=no]])],
[ if test x"$enableval" = x"yes"; then
        DotnetSupport=YES; AC_DEFINE([WANT_DOTNET_SUPPORT], [1], [Define to 1 if you want to include .NET interop support.])
  else
        DotnetSupport=NO
  fi],
[DotnetSupport=NO]
)
AC_SUBST(DotnetSupport)

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------


dnl --------------------------------------------------------------
dnl * General configuration checks
dnl --------------------------------------------------------------

dnl ** Can the unix package be built?
dnl --------------------------------------------------------------

if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
   GhcLibsWithUnix=NO
else
   GhcLibsWithUnix=YES
fi
AC_SUBST([GhcLibsWithUnix])

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
FP_HAVE_GCC
FP_MINGW_GCC
FP_GCC_NEEDS_NO_OMIT_LFPTR
FP_GCC_HAS_NO_UNIT_AT_A_TIME

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
FP_PROG_FIND

dnl ** Find sort command (for the benefit of Win32 environs)
FP_PROG_SORT

dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl If you can run configure, you certainly have /bin/sh
AC_DEFINE([HAVE_BIN_SH], [1], [Define to 1 if you have /bin/sh.])

dnl ** how to invoke `ar' and `ranlib'
FP_PROG_AR_NEEDS_RANLIB
FP_PROG_AR_SUPPORTS_INPUT

dnl ** Check to see whether ln -s works
AC_PROG_LN_S


dnl ** Find the path to sed
AC_PATH_PROG(SedCmd,sed)


dnl ** check for time command
AC_PATH_PROG(TimeCmd,time)

dnl ** check for tar
dnl   if GNU tar is named gtar, look for it first.
AC_PATH_PROGS(TarCmd,gtar tar,tar)

dnl ** check for DocBook toolchain
FP_CHECK_DOCBOOK_DTD
FP_DIR_DOCBOOK_XSL([/usr/share/xml/docbook/stylesheet/nwalsh/current /usr/share/xml/docbook/stylesheet/nwalsh /usr/share/sgml/docbook/docbook-xsl-stylesheets* /usr/share/sgml/docbook/xsl-stylesheets* /opt/kde?/share/apps/ksgmltools2/docbook/xsl /usr/share/docbook-xsl /usr/share/sgml/docbkxsl /usr/local/share/xsl/docbook /sw/share/xml/xsl/docbook-xsl])
FP_PROG_FO_PROCESSOR

dnl ** check for ghc-pkg command
FP_PROG_GHC_PKG

AC_ARG_WITH(greencard,
[AC_HELP_STRING([--with-greencard=ARG],
        [Use ARG as the path to greencard  [default=autodetct]])],
[
GreenCardCmd=$withval;
FPTOOLS_GREENCARD(3.00)
]
)

dnl ** check for installed happy binary + version
dnl    (don't do it if we're booting from .hc files though.)
if test "$BootingFromHc" = "NO"; then
FPTOOLS_HAPPY
fi;

dnl ** check for installed haddock
FPTOOLS_HADDOCK

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
AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/mman.h sys/resource.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h])

AC_CHECK_HEADER([readline/readline.h], [HaveReadlineReadlineH=YES], [HaveReadlineReadlineH=NO])
AC_CHECK_HEADER([readline/history.h], [HaveReadlineHistoryH=YES], [HaveReadlineHistoryH=NO])

if test $HaveReadlineReadlineH = YES && test $HaveReadlineHistoryH = YES ; then
  GhcLibsWithReadline=YES
  AC_DEFINE([HAVE_READLINE_HEADERS], [1], [Define to 1 if readline/readline.h and readline/history.h exist.])
else
  GhcLibsWithReadline=NO
  AC_DEFINE([HAVE_READLINE_HEADERS], [0], [Define to 1 if readline/readline.h and readline/history.h exist.])
fi
AC_SUBST(GhcLibsWithReadline)

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
AC_CHECK_FUNCS([getclock getrusage gettimeofday setitimer siginterrupt sysconf times ctime_r])
dnl ** For ghc/rts/gmp:
AC_CHECK_FUNCS([getpagesize])

dnl ** check whether this machine has gmp3 installed
AC_CHECK_LIB(gmp,  __gmpz_fdiv_qr, HaveLibGmp=YES; LibGmp=gmp,
  AC_CHECK_LIB(gmp3, __gmpz_fdiv_qr,  HaveLibGmp=YES; LibGmp=gmp3,
    HaveLibGmp=NO; LibGmp=not-installed))
AC_SUBST(HaveLibGmp)
AC_SUBST(LibGmp)

dnl ** (Mac OS X only: check for GMP.framework)
HaveFrameworkGMP=NO
case $HostPlatform in
*-apple-darwin)
    AC_MSG_CHECKING([for GMP.framework])
    save_libs="$LIBS"
    LIBS="-framework GMP"
    AC_TRY_LINK_FUNC(__gmpz_fdiv_qr, HaveFrameworkGMP=YES,)
    if test $HaveFrameworkGMP = YES; then
      AC_DEFINE([HAVE_FRAMEWORK_GMP], [1], [Define to 1 if GMP.framework is installed (Mac OS X only).])
    fi;
    LIBS="$save_libs"
    AC_MSG_RESULT([$HaveFrameworkGMP])
    ;;
esac
AC_SUBST(HaveFrameworkGMP)

dnl ** check for mingwex library
AC_CHECK_LIB(mingwex, closedir, HaveLibMingwEx=YES, HaveLibMingwEx=NO)
AC_SUBST(HaveLibMingwEx)

if test $HaveLibMingwEx = YES ; then
  AC_DEFINE([HAVE_MINGWEX], [1], [Define to 1 if you have the mingwex library.])
fi

if test "$HaveLibGmp" = "NO"; then
if test "$HostArch_CPP" = "ia64" -o "$HostArch_CPP" = "mipseb" ; then
AC_MSG_ERROR([You need to install libgmp (the in-tree version does not work on IA64 or mips64).])
fi;
fi;

dnl ** check whether this machine has BFD and liberty installed (used for debugging)
dnl    the order of these tests matters: bfd needs liberty
AC_CHECK_LIB(iberty, xmalloc)
AC_CHECK_LIB(bfd,    bfd_init)

dnl ** check for math library
AC_CHECK_FUNC(atan,[fp_libm_not_needed=yes;LIBM=],[fp_libm_not_needed=dunno])
if test x"$fp_libm_not_needed" = xdunno; then
   AC_CHECK_LIB([m], [atan], [LIBS="-lm $LIBS"; LIBM="-lm"],[LIBM=])
fi
AC_SUBST([LIBM])

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

dnl ** Working vfork?
AC_FUNC_FORK

dnl ** determine whether or not const works
AC_C_CONST

dnl ** are we big endian?
AC_C_BIGENDIAN

dnl ** check for leading underscores in symbol names
FP_LEADING_UNDERSCORE

dnl ** check for ld, whether it has an -x option, and if it is GNU ld
FP_PROG_LD_X
FP_PROG_LD_IS_GNU

dnl ** check for Apple-style dead-stripping support
dnl    (.subsections-via-symbols assembler directive)


AC_MSG_CHECKING(for .subsections_via_symbols)
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([], [__asm__ (".subsections_via_symbols");])],
    [AC_MSG_RESULT(yes)
     AC_DEFINE([HAVE_SUBSECTIONS_VIA_SYMBOLS],[1],
               [Define to 1 if Apple-style dead-stripping is supported.])
    ],
    [AC_MSG_RESULT(no)])

dnl *** check for GNU non-executable stack note support (ELF only)
dnl     (.section .note.GNU-stack,"",@progbits)

AC_MSG_CHECKING(for GNU non-executable stack support)
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([__asm__ (".section .note.GNU-stack,\"\",@progbits");], [0])],
    [AC_MSG_RESULT(yes)
     AC_DEFINE([HAVE_GNU_NONEXEC_STACK],[1],
               [Define to 1 if GNU non-executable stack notes are supported.])
    ],
    [AC_MSG_RESULT(no)])

dnl ** check for librt
AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_FUNCS(clock_gettime)

dnl ** check for Apple's "interesting" long double compatibility scheme
AC_MSG_CHECKING(for printf$LDBLStub)
AC_TRY_LINK_FUNC(printf$LSBLStub,
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

# test for GTK+
AC_PATH_PROGS([GTK_CONFIG], [pkg-config])
if test -n "$GTK_CONFIG"; then
  if $GTK_CONFIG gtk+-2.0 --atleast-version=2.0; then
    GTK_CONFIG="$GTK_CONFIG gtk+-2.0"
  else
    AC_MSG_WARN([GTK+ not usable, need at least version 2.0])
    GTK_CONFIG=
  fi
fi
AC_SUBST([GTK_CONFIG])

AC_CONFIG_FILES([mk/config.mk ghc.spec docs/users_guide/ug-book.xml])
AC_CONFIG_COMMANDS([mk/stamp-h],[echo timestamp > mk/stamp-h])
AC_OUTPUT
