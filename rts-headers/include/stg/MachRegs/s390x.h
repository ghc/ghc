#pragma once

/* -----------------------------------------------------------------------------
   The s390x register mapping

   Register    | Role(s)                                 | Call effect
   ------------+-------------------------------------+-----------------
   r0,r1       | -                                       | caller-saved
   r2          | Argument / return value                 | caller-saved
   r3,r4,r5    | Arguments                               | caller-saved
   r6          | Argument                                | callee-saved
   r7...r11    | -                                       | callee-saved
   r12         | (Commonly used as GOT pointer)          | callee-saved
   r13         | (Commonly used as literal pool pointer) | callee-saved
   r14         | Return address                          | caller-saved
   r15         | Stack pointer                           | callee-saved
   f0          | Argument / return value                 | caller-saved
   f2,f4,f6    | Arguments                               | caller-saved
   f1,f3,f5,f7 | -                                       | caller-saved
   f8...f15    | -                                       | callee-saved
   v0...v31    | -                                       | caller-saved

   Each general purpose register r0 through r15 as well as each floating-point
   register f0 through f15 is 64 bits wide. Each vector register v0 through v31
   is 128 bits wide.

   Note, the vector registers v0 through v15 overlap with the floating-point
   registers f0 through f15.

   -------------------------------------------------------------------------- */


#define REG(x) __asm__("%" #x)

#define REG_Base        r7
#define REG_Sp          r8
#define REG_Hp          r10
#define REG_R1          r11
#define REG_R2          r12
#define REG_R3          r13
#define REG_R4          r6
#define REG_R5          r2
#define REG_R6          r3
#define REG_R7          r4
#define REG_R8          r5
#define REG_SpLim       r9
#define REG_MachSp      r15

#define REG_F1          f8
#define REG_F2          f9
#define REG_F3          f10
#define REG_F4          f11
#define REG_F5          f0
#define REG_F6          f1

#define REG_D1          f12
#define REG_D2          f13
#define REG_D3          f14
#define REG_D4          f15
#define REG_D5          f2
#define REG_D6          f3

#define CALLER_SAVES_R5
#define CALLER_SAVES_R6
#define CALLER_SAVES_R7
#define CALLER_SAVES_R8

#define CALLER_SAVES_F5
#define CALLER_SAVES_F6

#define CALLER_SAVES_D5
#define CALLER_SAVES_D6