/* -----------------------------------------------------------------------------

   (c) The University of Glasgow, 1994-2004

   Native-code generator header file - just useful macros for now.

   -------------------------------------------------------------------------- */

#ifndef NCG_H
#define NCG_H

#include "ghc_boot_platform.h"

#define COMMA ,

-- - - - - - - - - - - - - - - - - - - - - - 
#if alpha_TARGET_ARCH
# define IF_ARCH_alpha(x,y) x
#else
# define IF_ARCH_alpha(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if i386_TARGET_ARCH
# define IF_ARCH_i386(x,y) x
#else
# define IF_ARCH_i386(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if freebsd_TARGET_OS
# define IF_OS_freebsd(x,y) x
#else
# define IF_OS_freebsd(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if netbsd_TARGET_OS
# define IF_OS_netbsd(x,y) x
#else
# define IF_OS_netbsd(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if openbsd_TARGET_OS
# define IF_OS_openbsd(x,y) x
#else
# define IF_OS_openbsd(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if linux_TARGET_OS
# define IF_OS_linux(x,y) x
#else
# define IF_OS_linux(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if linuxaout_TARGET_OS
# define IF_OS_linuxaout(x,y) x
#else
# define IF_OS_linuxaout(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if bsdi_TARGET_OS
# define IF_OS_bsdi(x,y) x
#else
# define IF_OS_bsdi(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if cygwin32_TARGET_OS
# define IF_OS_cygwin32(x,y) x
#else
# define IF_OS_cygwin32(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if sparc_TARGET_ARCH
# define IF_ARCH_sparc(x,y) x
#else
# define IF_ARCH_sparc(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if sunos4_TARGET_OS
# define IF_OS_sunos4(x,y) x
#else
# define IF_OS_sunos4(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
-- NB: this will catch i386-*-solaris2, too
#if solaris2_TARGET_OS
# define IF_OS_solaris2(x,y) x
#else
# define IF_OS_solaris2(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if powerpc_TARGET_ARCH
# define IF_ARCH_powerpc(x,y) x
#else
# define IF_ARCH_powerpc(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if darwin_TARGET_OS
# define IF_OS_darwin(x,y) x
#else
# define IF_OS_darwin(x,y) y
#endif
---------------------------------------------
#endif
