/* -----------------------------------------------------------------------------

   (c) The University of Glasgow, 1994-2004

   Native-code generator header file - just useful macros for now.

   -------------------------------------------------------------------------- */

#ifndef NCG_H
#define NCG_H

#include "ghc_boot_platform.h"

#define COMMA ,

-- - - - - - - - - - - - - - - - - - - - - - 
#if i386_TARGET_ARCH
# define IF_ARCH_i386(x,y) x
#else
# define IF_ARCH_i386(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if linux_TARGET_OS
# define IF_OS_linux(x,y) x
#else
# define IF_OS_linux(x,y) y
#endif
-- - - - - - - - - - - - - - - - - - - - - - 
#if darwin_TARGET_OS
# define IF_OS_darwin(x,y) x
#else
# define IF_OS_darwin(x,y) y
#endif
#endif
