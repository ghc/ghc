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
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(UnregisterisedCompiler)
#if !defined(NO_REGS)
#define NO_REGS
#endif
#endif

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
#if defined(NO_REGS)

#define MACHREGS_NO_REGS 1

#else

#define MACHREGS_NO_REGS 0

#if defined(i386_HOST_ARCH)
#define MACHREGS_i386     1
#endif

#if defined(x86_64_HOST_ARCH)
#define MACHREGS_x86_64   1
#endif

#if defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
        || defined(powerpc64le_HOST_ARCH) || defined(rs6000_HOST_ARCH)
#define MACHREGS_powerpc  1
#endif

#if defined(sparc_HOST_ARCH)
#define MACHREGS_sparc    1
#endif

#if defined(arm_HOST_ARCH)
#define MACHREGS_arm      1
#endif

#if defined(aarch64_HOST_ARCH)
#define MACHREGS_aarch64  1
#endif

#if defined(darwin_HOST_OS)
#define MACHREGS_darwin   1
#endif

#endif

#include "MachRegs.h"
