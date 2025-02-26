#pragma once

/* -----------------------------------------------------------------------------
   The loongarch64 register mapping

   Register    | Role(s)                                 | Call effect
   ------------+-----------------------------------------+-------------
   zero        | Hard-wired zero                         | -
   ra          | Return address                          | caller-saved
   tp          | Thread pointer                          | -
   sp          | Stack pointer                           | callee-saved
   a0,a1       | Arguments / return values               | caller-saved
   a2..a7      | Arguments                               | caller-saved
   t0..t8      | -                                       | caller-saved
   u0          | Reserve                                 | -
   fp          | Frame pointer                           | callee-saved
   s0..s8      | -                                       | callee-saved
   fa0,fa1     | Arguments / return values               | caller-saved
   fa2..fa7    | Arguments                               | caller-saved
   ft0..ft15   | -                                       | caller-saved
   fs0..fs7    | -                                       | callee-saved

   Each general purpose register as well as each floating-point
   register is 64 bits wide, also, the u0 register is called r21 in some cases.

   -------------------------------------------------------------------------- */

#define REG(x) __asm__("$" #x)

#define REG_Base        s0
#define REG_Sp          s1
#define REG_Hp          s2
#define REG_R1          s3
#define REG_R2          s4
#define REG_R3          s5
#define REG_R4          s6
#define REG_R5          s7
#define REG_SpLim       s8

#define REG_F1          fs0
#define REG_F2          fs1
#define REG_F3          fs2
#define REG_F4          fs3

#define REG_D1          fs4
#define REG_D2          fs5
#define REG_D3          fs6
#define REG_D4          fs7
