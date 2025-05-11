#pragma once

/* -----------------------------------------------------------------------------
   The ARM EABI register mapping

   Here we consider ARM mode (i.e. 32bit isns)
   and also CPU with full VFPv3 implementation

   ARM registers (see Chapter 5.1 in ARM IHI 0042D and
   Section 9.2.2 in ARM Software Development Toolkit Reference Guide)

   r15  PC         The Program Counter.
   r14  LR         The Link Register.
   r13  SP         The Stack Pointer.
   r12  IP         The Intra-Procedure-call scratch register.
   r11  v8/fp      Variable-register 8.
   r10  v7/sl      Variable-register 7.
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
                         subroutine calls)

   VFPv3/NEON registers (added to the VFPv2 registers set)
   d16-d31/q8-q15        Argument / result/ scratch registers
   ----------------------------------------------------------------------------- */

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