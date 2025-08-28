/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2011
 *
 * This header includes MachRegs.h "selecting" regs for the current host
 * platform.
 *
 * Don't #include this in the RTS directly, instead include "RTS.h".
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(UnregisterisedCompiler) || defined(javascript_HOST_ARCH)
#if !defined(NO_REGS)
#define NO_REGS
#endif
#endif

/*
 * Defining NO_REGS causes no global registers to be used.  NO_REGS is
 * typically defined by GHC, via a command-line option passed to gcc,
 * when the -funregisterised flag is given.
 *
 * It is also enabled for target architectures that really lack registers, such
 * as JavaScript.
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

#if defined(arm_HOST_ARCH)
#define MACHREGS_arm      1
#endif

#if defined(aarch64_HOST_ARCH)
#define MACHREGS_aarch64  1
#endif

#if defined(darwin_HOST_OS)
#define MACHREGS_darwin   1
#endif

#if defined(s390x_HOST_ARCH)
#define MACHREGS_s390x    1
#endif

#if defined(riscv64_HOST_ARCH)
#define MACHREGS_riscv64  1
#endif

#if defined(wasm32_HOST_ARCH)
#define MACHREGS_wasm32   1
#endif

#if defined(loongarch64_HOST_ARCH)
#define MACHREGS_loongarch64  1
#endif

#endif

#include "stg/MachRegs.h"
