#pragma once

/* -----------------------------------------------------------------------------
   The PowerPC register mapping

   0            system glue?    (caller-save, volatile)
   1            SP              (callee-save, non-volatile)
   2            AIX, powerpc64-linux:
                    RTOC        (a strange special case)
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


#define REG(x) __asm__(#x)

#define REG_R1          r14
#define REG_R2          r15
#define REG_R3          r16
#define REG_R4          r17
#define REG_R5          r18
#define REG_R6          r19
#define REG_R7          r20
#define REG_R8          r21
#define REG_R9          r22
#define REG_R10         r23

#define REG_F1          fr14
#define REG_F2          fr15
#define REG_F3          fr16
#define REG_F4          fr17
#define REG_F5          fr18
#define REG_F6          fr19

#define REG_D1          fr20
#define REG_D2          fr21
#define REG_D3          fr22
#define REG_D4          fr23
#define REG_D5          fr24
#define REG_D6          fr25

#define REG_Sp          r24
#define REG_SpLim       r25
#define REG_Hp          r26
#define REG_Base        r27
