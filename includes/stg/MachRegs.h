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

#ifndef MACHREGS_H
#define MACHREGS_H

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
   Caller saves and callee-saves regs.

   Caller-saves regs have to be saved around C-calls made from STG
   land, so this file defines CALLER_SAVES_<reg> for each <reg> that
   is designated caller-saves in that machine's C calling convention.

   As it stands, the only registers that are ever marked caller saves
   are the RX, FX, DX and USER registers; as a result, if you
   decide to caller save a system register (e.g. SP, HP, etc), note that
   this code path is completely untested! -- EZY
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
   The x86 register mapping

   Ok, we've only got 6 general purpose registers, a frame pointer and a
   stack pointer.  \tr{%eax} and \tr{%edx} are return values from C functions,
   hence they get trashed across ccalls and are caller saves. \tr{%ebx},
   \tr{%esi}, \tr{%edi}, \tr{%ebp} are all callee-saves.

   Reg     STG-Reg
   ---------------
   ebx     Base
   ebp     Sp
   esi     R1
   edi     Hp

   Leaving SpLim out of the picture.
   -------------------------------------------------------------------------- */

#if MACHREGS_i386

#define REG(x) __asm__("%" #x)

#ifndef not_doing_dynamic_linking
#define REG_Base    ebx
#endif
#define REG_Sp      ebp

#ifndef STOLEN_X86_REGS
#define STOLEN_X86_REGS 4
#endif

#if STOLEN_X86_REGS >= 3
# define REG_R1     esi
#endif

#if STOLEN_X86_REGS >= 4
# define REG_Hp     edi
#endif

#define REG_XMM1    xmm0
#define REG_XMM2    xmm1
#define REG_XMM3    xmm2
#define REG_XMM4    xmm3

#define MAX_REAL_VANILLA_REG 1  /* always, since it defines the entry conv */
#define MAX_REAL_FLOAT_REG   0
#define MAX_REAL_DOUBLE_REG  0
#define MAX_REAL_LONG_REG    0
#define MAX_REAL_XMM_REG     4

/* -----------------------------------------------------------------------------
  The x86-64 register mapping

  %rax          caller-saves, don't steal this one
  %rbx          YES
  %rcx          arg reg, caller-saves
  %rdx          arg reg, caller-saves
  %rsi          arg reg, caller-saves
  %rdi          arg reg, caller-saves
  %rbp          YES (our *prime* register)
  %rsp          (unavailable - stack pointer)
  %r8           arg reg, caller-saves
  %r9           arg reg, caller-saves
  %r10          caller-saves
  %r11          caller-saves
  %r12          YES
  %r13          YES
  %r14          YES
  %r15          YES

  %xmm0-7       arg regs, caller-saves
  %xmm8-15      caller-saves

  Use the caller-saves regs for Rn, because we don't always have to
  save those (as opposed to Sp/Hp/SpLim etc. which always have to be
  saved).

  --------------------------------------------------------------------------- */

#elif MACHREGS_x86_64

#define REG(x) __asm__("%" #x)

#define REG_Base  r13
#define REG_Sp    rbp
#define REG_Hp    r12
#define REG_R1    rbx
#define REG_R2    r14
#define REG_R3    rsi
#define REG_R4    rdi
#define REG_R5    r8
#define REG_R6    r9
#define REG_SpLim r15

#define REG_F1    xmm1
#define REG_F2    xmm2
#define REG_F3    xmm3
#define REG_F4    xmm4
#define REG_F5    xmm5
#define REG_F6    xmm6

#define REG_D1    xmm1
#define REG_D2    xmm2
#define REG_D3    xmm3
#define REG_D4    xmm4
#define REG_D5    xmm5
#define REG_D6    xmm6

#define REG_XMM1    xmm1
#define REG_XMM2    xmm2
#define REG_XMM3    xmm3
#define REG_XMM4    xmm4
#define REG_XMM5    xmm5
#define REG_XMM6    xmm6

#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_R3
#define CALLER_SAVES_R4
#endif
#define CALLER_SAVES_R5
#define CALLER_SAVES_R6

#define CALLER_SAVES_F1
#define CALLER_SAVES_F2
#define CALLER_SAVES_F3
#define CALLER_SAVES_F4
#define CALLER_SAVES_F5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_F6
#endif

#define CALLER_SAVES_D1
#define CALLER_SAVES_D2
#define CALLER_SAVES_D3
#define CALLER_SAVES_D4
#define CALLER_SAVES_D5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_D6
#endif

#define CALLER_SAVES_XMM1
#define CALLER_SAVES_XMM2
#define CALLER_SAVES_XMM3
#define CALLER_SAVES_XMM4
#define CALLER_SAVES_XMM5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_XMM6
#endif

#define MAX_REAL_VANILLA_REG 6
#define MAX_REAL_FLOAT_REG   6
#define MAX_REAL_DOUBLE_REG  6
#define MAX_REAL_LONG_REG    0
#define MAX_REAL_XMM_REG     6

/* -----------------------------------------------------------------------------
   The PowerPC register mapping

   0            system glue?    (caller-save, volatile)
   1            SP              (callee-save, non-volatile)
   2            AIX, powerpc64-linux:
                    RTOC        (a strange special case)
                darwin:
                                (caller-save, volatile)
                powerpc32-linux:
                                reserved for use by system

   3-10         args/return     (caller-save, volatile)
   11,12        system glue?    (caller-save, volatile)
   13           on 64-bit:      reserved for thread state pointer
                on 32-bit:      (callee-save, non-volatile)
   14-31                        (callee-save, non-volatile)

   f0                           (caller-save, volatile)
   f1-f13       args/return     (caller-save, volatile)
   f14-f31                      (callee-save, non-volatile)

   \tr{14}--\tr{31} are wonderful callee-save registers on all ppc OSes.
   \tr{0}--\tr{12} are caller-save registers.

   \tr{%f14}--\tr{%f31} are callee-save floating-point registers.

   We can do the Whole Business with callee-save registers only!
   -------------------------------------------------------------------------- */

#elif MACHREGS_powerpc

#define REG(x) __asm__(#x)

#define REG_R1          r14
#define REG_R2          r15
#define REG_R3          r16
#define REG_R4          r17
#define REG_R5          r18
#define REG_R6          r19
#define REG_R7          r20
#define REG_R8          r21

#if MACHREGS_darwin

#define REG_F1          f14
#define REG_F2          f15
#define REG_F3          f16
#define REG_F4          f17

#define REG_D1          f18
#define REG_D2          f19

#else

#define REG_F1          fr14
#define REG_F2          fr15
#define REG_F3          fr16
#define REG_F4          fr17

#define REG_D1          fr18
#define REG_D2          fr19

#endif

#define REG_Sp          r22
#define REG_SpLim       r24

#define REG_Hp          r25

#define REG_Base        r27

/* -----------------------------------------------------------------------------
   The Sun SPARC register mapping

   !! IMPORTANT: if you change this register mapping you must also update
                 compiler/nativeGen/SPARC/Regs.hs. That file handles the
                 mapping for the NCG. This one only affects via-c code.

   The SPARC register (window) story: Remember, within the Haskell
   Threaded World, we essentially ``shut down'' the register-window
   mechanism---the window doesn't move at all while in this World.  It
   *does* move, of course, if we call out to arbitrary~C...

   The %i, %l, and %o registers (8 each) are the input, local, and
   output registers visible in one register window.  The 8 %g (global)
   registers are visible all the time.

      zero: always zero
   scratch: volatile across C-fn calls. used by linker.
       app: usable by application
    system: reserved for system

     alloc: allocated to in the register allocator, intra-closure only

                GHC usage     v8 ABI        v9 ABI
   Global
     %g0        zero        zero          zero
     %g1        alloc       scratch       scrach
     %g2        alloc       app           app
     %g3        alloc       app           app
     %g4        alloc       app           scratch
     %g5                    system        scratch
     %g6                    system        system
     %g7                    system        system

   Output: can be zapped by callee
     %o0-o5     alloc       caller saves
     %o6                    C stack ptr
     %o7                    C ret addr

   Local: maintained by register windowing mechanism
     %l0        alloc
     %l1        R1
     %l2        R2
     %l3        R3
     %l4        R4
     %l5        R5
     %l6        alloc
     %l7        alloc

   Input
     %i0        Sp
     %i1        Base
     %i2        SpLim
     %i3        Hp
     %i4        alloc
     %i5        R6
     %i6                    C frame ptr
     %i7                    C ret addr

   The paired nature of the floating point registers causes complications for
   the native code generator.  For convenience, we pretend that the first 22
   fp regs %f0 .. %f21 are actually 11 double regs, and the remaining 10 are
   float (single) regs.  The NCG acts accordingly.  That means that the
   following FP assignment is rather fragile, and should only be changed
   with extreme care.  The current scheme is:

      %f0 /%f1    FP return from C
      %f2 /%f3    D1
      %f4 /%f5    D2
      %f6 /%f7    ncg double spill tmp #1
      %f8 /%f9    ncg double spill tmp #2
      %f10/%f11   allocatable
      %f12/%f13   allocatable
      %f14/%f15   allocatable
      %f16/%f17   allocatable
      %f18/%f19   allocatable
      %f20/%f21   allocatable

      %f22        F1
      %f23        F2
      %f24        F3
      %f25        F4
      %f26        ncg single spill tmp #1
      %f27        ncg single spill tmp #2
      %f28        allocatable
      %f29        allocatable
      %f30        allocatable
      %f31        allocatable

   -------------------------------------------------------------------------- */

#elif MACHREGS_sparc

#define REG(x) __asm__("%" #x)

#define CALLER_SAVES_USER

#define CALLER_SAVES_F1
#define CALLER_SAVES_F2
#define CALLER_SAVES_F3
#define CALLER_SAVES_F4
#define CALLER_SAVES_D1
#define CALLER_SAVES_D2

#define REG_R1          l1
#define REG_R2          l2
#define REG_R3          l3
#define REG_R4          l4
#define REG_R5          l5
#define REG_R6          i5

#define REG_F1          f22
#define REG_F2          f23
#define REG_F3          f24
#define REG_F4          f25

/* for each of the double arg regs,
   Dn_2 is the high half. */

#define REG_D1          f2
#define REG_D1_2        f3

#define REG_D2          f4
#define REG_D2_2        f5

#define REG_Sp          i0
#define REG_SpLim       i2

#define REG_Hp          i3

#define REG_Base        i1

#define NCG_FirstFloatReg f22

/* -----------------------------------------------------------------------------
   The ARM EABI register mapping

   Here we consider ARM mode (i.e. 32bit isns)
   and also CPU with full VFPv3 implementation

   ARM registers (see Chapter 5.1 in ARM IHI 0042D)

   r15  PC         The Program Counter.
   r14  LR         The Link Register.
   r13  SP         The Stack Pointer.
   r12  IP         The Intra-Procedure-call scratch register.
   r11  v8         Variable-register 8.
   r10  v7         Variable-register 7.
   r9   v6/SB/TR   Platform register. The meaning of this register is
                   defined by the platform standard.
   r8   v5         Variable-register 5.
   r7   v4         Variable register 4.
   r6   v3         Variable register 3.
   r5   v2         Variable register 2.
   r4   v1         Variable register 1.
   r3   a4         Argument / scratch register 4.
   r2   a3         Argument / scratch register 3.
   r1   a2         Argument / result / scratch register 2.
   r0   a1         Argument / result / scratch register 1.

   VFPv2/VFPv3/NEON registers
   s0-s15/d0-d7/q0-q3    Argument / result/ scratch registers
   s16-s31/d8-d15/q4-q7  callee-saved registers (must be preserved across
                         subrutine calls)

   VFPv3/NEON registers (added to the VFPv2 registers set)
   d16-d31/q8-q15        Argument / result/ scratch registers
   ----------------------------------------------------------------------------- */

#elif MACHREGS_arm

#define REG(x) __asm__(#x)

#define REG_Base        r4
#define REG_Sp          r5
#define REG_Hp          r6
#define REG_R1          r7
#define REG_R2          r8
#define REG_R3          r9
#define REG_R4          r10
#define REG_SpLim       r11

#if !defined(arm_HOST_ARCH_PRE_ARMv6)
/* d8 */
#define REG_F1    s16
#define REG_F2    s17
/* d9 */
#define REG_F3    s18
#define REG_F4    s19

#define REG_D1    d10
#define REG_D2    d11
#endif

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

#ifndef MAX_REAL_VANILLA_REG
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

#ifndef MAX_REAL_FLOAT_REG
#  if   defined(REG_F4)
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

#ifndef MAX_REAL_DOUBLE_REG
#  if   defined(REG_D2)
#  define MAX_REAL_DOUBLE_REG 2
#  elif defined(REG_D1)
#  define MAX_REAL_DOUBLE_REG 1
#  else
#  define MAX_REAL_DOUBLE_REG 0
#  endif
#endif

#ifndef MAX_REAL_LONG_REG
#  if   defined(REG_L1)
#  define MAX_REAL_LONG_REG 1
#  else
#  define MAX_REAL_LONG_REG 0
#  endif
#endif

#ifndef MAX_REAL_XMM_REG
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

#endif /* MACHREGS_H */
