#define COMMA ,

#ifndef NCG_H
#define NCG_H
#if 0
 COMPILING_NCG is used to control the visibility of 
 relevant information from the GHC header files when
 compiling the native code generator modules.
#endif

#ifndef COMPILING_NCG
#define COMPILING_NCG
#endif

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

** Convenience macros for writing the native-code generator **

#endif

#include "../includes/config.h"

#if 0
{-testing only-}
#undef sparc_TARGET_ARCH
#undef sunos4_TARGET_OS
#undef i386_TARGET_ARCH
#define i386_TARGET_ARCH 1
#undef linuxaout_TARGET_OS
#define linuxaout_TARGET_OS 1
#endif
#if 0
{-testing only-}
#undef sparc_TARGET_ARCH
#undef sunos4_TARGET_OS
#undef alpha_TARGET_ARCH
#define alpha_TARGET_ARCH 1
#endif

#if i386_TARGET_ARCH
# define STOLEN_X86_REGS 4
-- HACK: go for the max
#endif

#include "../includes/MachRegs.h"
#include "../includes/NativeDefs.h"

#if alpha_TARGET_ARCH
# define BYTES_PER_WORD 8
# define BYTES_PER_WORD_STR "8"
#endif

#if i386_TARGET_ARCH
# define BYTES_PER_WORD 4
# define BYTES_PER_WORD_STR "4"
#endif

#if sparc_TARGET_ARCH
# define BYTES_PER_WORD 4
# define BYTES_PER_WORD_STR "4"
#endif

#if powerpc_TARGET_ARCH
# define BYTES_PER_WORD 4
# define BYTES_PER_WORD_STR "4"
#endif

---------------------------------------------

#if alpha_TARGET_ARCH
# define IF_ARCH_alpha(x,y) x
#else
# define IF_ARCH_alpha(x,y) y
#endif

---------------------------------------------

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
---------------------------------------------
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
---------------------------------------------
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
