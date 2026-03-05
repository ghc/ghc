#pragma once


/* -----------------------------------------------------------------------------
   The ARMv8/AArch64 ABI register mapping

   The AArch64 provides 31 64-bit general purpose registers
   and 32 128-bit SIMD/floating point registers.

   General purpose registers (see Chapter 5.1.1 in ARM IHI 0055B)

   Register | Special | Role in the procedure call standard
   ---------+---------+------------------------------------
     SP     |         | The Stack Pointer
     r30    |  LR     | The Link Register
     r29    |  FP     | The Frame Pointer
   r19-r28  |         | Callee-saved registers
     r18    |         | The Platform Register, if needed;
            |         | or temporary register
     r17    |  IP1    | The second intra-procedure-call temporary register
     r16    |  IP0    | The first intra-procedure-call scratch register
    r9-r15  |         | Temporary registers
     r8     |         | Indirect result location register
    r0-r7   |         | Parameter/result registers


   FPU/SIMD registers

   s/d/q/v0-v7    Argument / result/ scratch registers
   s/d/q/v8-v15   callee-saved registers (must be preserved across subroutine calls,
                  but only bottom 64-bit value needs to be preserved)
   s/d/q/v16-v31  temporary registers

   ----------------------------------------------------------------------------- */

#define REG(x) __asm__(#x)

#define REG_Base        r19
#define REG_Sp          r20
#define REG_Hp          r21
#define REG_R1          r22
#define REG_R2          r23
#define REG_R3          r24
#define REG_R4          r25
#define REG_R5          r26
#define REG_R6          r27
#define REG_SpLim       r28

#define REG_F1          s8
#define REG_F2          s9
#define REG_F3          s10
#define REG_F4          s11

#define REG_D1          d12
#define REG_D2          d13
#define REG_D3          d14
#define REG_D4          d15

#define REG_XMM1        q4
#define REG_XMM2        q5

#define CALLER_SAVES_XMM1
#define CALLER_SAVES_XMM2

