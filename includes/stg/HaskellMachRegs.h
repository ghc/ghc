/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2011
 *
 * Registers used in STG code.  Might or might not correspond to
 * actual machine registers.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef HASKELLMACHREGS_H
#define HASKELLMACHREGS_H

/*
 * Defining NO_REGS causes no global registers to be used.  NO_REGS is
 * typically defined by GHC, via a command-line option passed to gcc,
 * when the -funregisterised flag is given.
 *
 * NB. When NO_REGS is on, calling & return conventions may be
 * different.  For example, all function arguments will be passed on
 * the stack, and components of an unboxed tuple will be returned on
 * the stack rather than in registers.
 */
#ifdef NO_REGS

#define MACHREGS_NO_REGS 1

#else

#define MACHREGS_NO_REGS 0

#define MACHREGS_i386     i386_TARGET_ARCH
#define MACHREGS_x86_64   x86_64_TARGET_ARCH
#define MACHREGS_powerpc  (powerpc_TARGET_ARCH || powerpc64_TARGET_ARCH || rs6000_TARGET_ARCH)
#define MACHREGS_sparc    sparc_TARGET_ARCH
#define MACHREGS_arm      arm_TARGET_ARCH
#define MACHREGS_darwin   darwin_TARGET_OS

#endif

#include "MachRegs.h"

#endif /* HASKELLMACHREGS_H */
