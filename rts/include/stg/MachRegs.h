/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2014
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

/* This file is #included into Haskell code in the compiler: #defines
 * only in here please.
 */

/*
 * Undefine these as a precaution: some of them were found to be
 * defined by system headers on ARM/Linux.
 */
#undef REG_R1
#undef REG_R2
#undef REG_R3
#undef REG_R4
#undef REG_R5
#undef REG_R6
#undef REG_R7
#undef REG_R8
#undef REG_R9
#undef REG_R10

/*
 * Defining MACHREGS_NO_REGS to 1 causes no global registers to be used.
 * MACHREGS_NO_REGS is typically controlled by NO_REGS, which is
 * typically defined by GHC, via a command-line option passed to gcc,
 * when the -funregisterised flag is given.
 *
 * NB. When MACHREGS_NO_REGS to 1, calling & return conventions may be
 * different.  For example, all function arguments will be passed on
 * the stack, and components of an unboxed tuple will be returned on
 * the stack rather than in registers.
 */
#if MACHREGS_NO_REGS == 1

/* Nothing */

#elif MACHREGS_NO_REGS == 0

/* ----------------------------------------------------------------------------
   Note [Caller saves and callee-saves regs.]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Caller-saves regs have to be saved around C-calls made from STG
   land, so this file defines CALLER_SAVES_<reg> for each <reg> that
   is designated caller-saves in that machine's C calling convention.
   NB: Caller-saved registers not mapped to a STG register don't
       require a CALLER_SAVES_ define.

   As it stands, the only registers that are ever marked caller saves
   are the RX, FX, DX, XMM and USER registers; as a result, if you
   decide to caller save a system register (e.g. SP, HP, etc), note that
   this code path is completely untested! -- EZY

   See Note [Register parameter passing] for details.
   -------------------------------------------------------------------------- */

/* Define STG <-> machine register mappings. */
#if defined(MACHREGS_i386) || defined(MACHREGS_x86_64)

#include "MachRegs/x86.h"

#elif defined(MACHREGS_powerpc)

#include "MachRegs/ppc.h"

#elif defined(MACHREGS_arm)

#include "MachRegs/arm32.h"

#elif defined(MACHREGS_aarch64)

#include "MachRegs/arm64.h"

#elif defined(MACHREGS_s390x)

#include "MachRegs/s390x.h"

#elif defined(MACHREGS_riscv64)

#include "MachRegs/riscv64.h"

#elif defined(MACHREGS_wasm32)

#include "MachRegs/wasm32.h"

#elif defined(MACHREGS_loongarch64)

#include "MachRegs/loongarch64.h"

#else

#error Cannot find platform to give register info for

#endif

#else

#error Bad MACHREGS_NO_REGS value

#endif

/* -----------------------------------------------------------------------------
 * These constants define how many stg registers will be used for
 * passing arguments (and results, in the case of an unboxed-tuple
 * return).
 *
 * We usually set MAX_REAL_VANILLA_REG and co. to be the number of the
 * highest STG register to occupy a real machine register, otherwise
 * the calling conventions will needlessly shuffle data between the
 * stack and memory-resident STG registers.  We might occasionally
 * set these macros to other values for testing, though.
 *
 * Registers above these values might still be used, for instance to
 * communicate with PrimOps and RTS functions.
 */

#if !defined(MAX_REAL_VANILLA_REG)
#  if   defined(REG_R10)
#  define MAX_REAL_VANILLA_REG 10
#  elif   defined(REG_R9)
#  define MAX_REAL_VANILLA_REG 9
#  elif   defined(REG_R8)
#  define MAX_REAL_VANILLA_REG 8
#  elif defined(REG_R7)
#  define MAX_REAL_VANILLA_REG 7
#  elif defined(REG_R6)
#  define MAX_REAL_VANILLA_REG 6
#  elif defined(REG_R5)
#  define MAX_REAL_VANILLA_REG 5
#  elif defined(REG_R4)
#  define MAX_REAL_VANILLA_REG 4
#  elif defined(REG_R3)
#  define MAX_REAL_VANILLA_REG 3
#  elif defined(REG_R2)
#  define MAX_REAL_VANILLA_REG 2
#  elif defined(REG_R1)
#  define MAX_REAL_VANILLA_REG 1
#  else
#  define MAX_REAL_VANILLA_REG 0
#  endif
#endif

#if !defined(MAX_REAL_FLOAT_REG)
#  if   defined(REG_F7)
#  error Please manually define MAX_REAL_FLOAT_REG for this architecture
#  elif defined(REG_F6)
#  define MAX_REAL_FLOAT_REG 6
#  elif defined(REG_F5)
#  define MAX_REAL_FLOAT_REG 5
#  elif defined(REG_F4)
#  define MAX_REAL_FLOAT_REG 4
#  elif defined(REG_F3)
#  define MAX_REAL_FLOAT_REG 3
#  elif defined(REG_F2)
#  define MAX_REAL_FLOAT_REG 2
#  elif defined(REG_F1)
#  define MAX_REAL_FLOAT_REG 1
#  else
#  define MAX_REAL_FLOAT_REG 0
#  endif
#endif

#if !defined(MAX_REAL_DOUBLE_REG)
#  if   defined(REG_D7)
#  error Please manually define MAX_REAL_DOUBLE_REG for this architecture
#  elif defined(REG_D6)
#  define MAX_REAL_DOUBLE_REG 6
#  elif defined(REG_D5)
#  define MAX_REAL_DOUBLE_REG 5
#  elif defined(REG_D4)
#  define MAX_REAL_DOUBLE_REG 4
#  elif defined(REG_D3)
#  define MAX_REAL_DOUBLE_REG 3
#  elif defined(REG_D2)
#  define MAX_REAL_DOUBLE_REG 2
#  elif defined(REG_D1)
#  define MAX_REAL_DOUBLE_REG 1
#  else
#  define MAX_REAL_DOUBLE_REG 0
#  endif
#endif

#if !defined(MAX_REAL_LONG_REG)
#  if   defined(REG_L1)
#  define MAX_REAL_LONG_REG 1
#  else
#  define MAX_REAL_LONG_REG 0
#  endif
#endif

#if !defined(MAX_REAL_XMM_REG)
#  if   defined(REG_XMM6)
#  define MAX_REAL_XMM_REG 6
#  elif defined(REG_XMM5)
#  define MAX_REAL_XMM_REG 5
#  elif defined(REG_XMM4)
#  define MAX_REAL_XMM_REG 4
#  elif defined(REG_XMM3)
#  define MAX_REAL_XMM_REG 3
#  elif defined(REG_XMM2)
#  define MAX_REAL_XMM_REG 2
#  elif defined(REG_XMM1)
#  define MAX_REAL_XMM_REG 1
#  else
#  define MAX_REAL_XMM_REG 0
#  endif
#endif

/* define NO_ARG_REGS if we have no argument registers at all (we can
 * optimise certain code paths using this predicate).
 */
#if MAX_REAL_VANILLA_REG < 2
#define NO_ARG_REGS
#else
#undef NO_ARG_REGS
#endif
