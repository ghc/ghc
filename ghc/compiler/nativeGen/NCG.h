#ifndef NCG_H
#define NCG_H

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

** Convenience macros for writing the native-code generator **

#endif

#define FAST_REG_NO FAST_INT

#include "../../includes/config.h"

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
#include "../../includes/MachRegs.h"

#if alpha_TARGET_ARCH
# define BYTES_PER_WORD 8
# define BYTES_PER_WORD_STR "8"

# include "../../includes/alpha-dec-osf1.h"
#endif

#if i386_TARGET_ARCH
# define BYTES_PER_WORD 4
# define BYTES_PER_WORD_STR "4"

# if linuxaout_TARGET_OS
#  include "../../includes/i386-unknown-linuxaout.h"
# endif
# if linux_TARGET_OS
#  include "../../includes/i386-unknown-linux.h"
# endif
# if freebsd_TARGET_OS
#  include "../../includes/i386-unknown-freebsd.h"
# endif
# if netbsd_TARGET_OS
#  include "../../includes/i386-unknown-netbsd.h"
# endif
# if bsdi_TARGET_OS
#  include "../../includes/i386-unknown-bsdi.h"
# endif
# if cygwin32_TARGET_OS
#  include "../../includes/i386-unknown-cygwin32.h"
# endif
# if solaris2_TARGET_OS
#  include "../../includes/i386-unknown-solaris2.h"
# endif
#endif

#if sparc_TARGET_ARCH
# define BYTES_PER_WORD 4
# define BYTES_PER_WORD_STR "4"

# if sunos4_TARGET_OS
#  include "../../includes/sparc-sun-sunos4.h"
# endif
# if solaris2_TARGET_OS
#  include "../../includes/sparc-sun-solaris2.h"
# endif
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
#endif
