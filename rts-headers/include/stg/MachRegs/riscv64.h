#pragma once

/* -----------------------------------------------------------------------------
   The riscv64 register mapping

   Register    | Role(s)                                 | Call effect
   ------------+-----------------------------------------+-------------
   zero        | Hard-wired zero                         | -
   ra          | Return address                          | caller-saved
   sp          | Stack pointer                           | callee-saved
   gp          | Global pointer                          | callee-saved
   tp          | Thread pointer                          | callee-saved
   t0,t1,t2    | -                                       | caller-saved
   s0          | Frame pointer                           | callee-saved
   s1          | -                                       | callee-saved
   a0,a1       | Arguments / return values               | caller-saved
   a2..a7      | Arguments                               | caller-saved
   s2..s11     | -                                       | callee-saved
   t3..t6      | -                                       | caller-saved
   ft0..ft7    | -                                       | caller-saved
   fs0,fs1     | -                                       | callee-saved
   fa0,fa1     | Arguments / return values               | caller-saved
   fa2..fa7    | Arguments                               | caller-saved
   fs2..fs11   | -                                       | callee-saved
   ft8..ft11   | -                                       | caller-saved

   Each general purpose register as well as each floating-point
   register is 64 bits wide.

   -------------------------------------------------------------------------- */

#define REG(x) __asm__(#x)

#define REG_Base        s1
#define REG_Sp          s2
#define REG_Hp          s3
#define REG_R1          s4
#define REG_R2          s5
#define REG_R3          s6
#define REG_R4          s7
#define REG_R5          s8
#define REG_R6          s9
#define REG_R7          s10
#define REG_SpLim       s11

#define REG_F1          fs0
#define REG_F2          fs1
#define REG_F3          fs2
#define REG_F4          fs3
#define REG_F5          fs4
#define REG_F6          fs5

#define REG_D1          fs6
#define REG_D2          fs7
#define REG_D3          fs8
#define REG_D4          fs9
#define REG_D5          fs10
#define REG_D6          fs11
