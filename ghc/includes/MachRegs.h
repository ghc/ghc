/* -----------------------------------------------------------------------------
 * $Id: MachRegs.h,v 1.2 1998/12/02 13:21:13 simonm Exp $
 *
 * Registers used in STG code.  Might or might not correspond to
 * actual machine registers.
 *
 * ---------------------------------------------------------------------------*/

#ifndef MACHREGS_H
#define MACHREGS_H

/* This file is #included into Haskell code in the compiler: #defines
 * only in here please.
 */

/* define NO_REGS to omit register declarations - used in RTS C code
 * that needs all the STG definitions but not the global register 
 * settings.
 */
#ifndef NO_REGS

/* ----------------------------------------------------------------------------
   Caller saves and callee-saves regs.
   
   Caller-saves regs have to be saved around C-calls made from STG
   land, so this file defines CALLER_SAVES_<reg> for each <reg> that
   is designated caller-saves in that machine's C calling convention.

   Additionally, the following macros should be defined when

     CALLER_SAVES_USER		one or more of R<n>, F, D
 			        are caller-saves.

     CALLER_SAVES_SYSTEM        one or more of Sp, Su, SpLim, Hp, HpLim
     				are caller-saves.

   This is so that the callWrapper mechanism knows which kind of
   wrapper to generate for certain types of C call.
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
   The DEC Alpha register mapping

   Alpha registers
   \tr{$9}--\tr{$14} are our ``prize'' callee-save registers.  
   \tr{$15} is the frame pointer, and \tr{$16}--\tr{$21} are argument
   registers.  (These are off-limits.)  We can steal some of the \tr{$22}-and-up 
   caller-save registers provided we do the appropriate save/restore stuff.
   
   \tr{$f2}--\tr{$f9} are some callee-save floating-point registers.
   
   We cannot use \tr{$23} (aka t9), \tr{$24} (aka t10), \tr{$25} (aka
   t11), \tr{$27} (aka pv), or \tr{$28} (aka at), because they are
   occasionally required by the assembler to handle non-primitive
   instructions (e.g. ldb, remq).  Sigh!
   
   Cheat sheet for GDB:
   
   GDB	here	Main map
   ===	====	========
   s5	$14	R1
   t1	$2	R2
   t2	$3	R3
   t3	$4	R4
   t4	$5	R5
   t5	$6	R6
   t6	$7	R7
   t7	$8	R8
   s0	$9	Sp
   s1	$10	Su
   s2	$11	SpLim
   s3	$12	Hp   
   s4	$13	HpLim
   t8	$22	NCG_reserved
   t12	$27	NCG_reserved
   -------------------------------------------------------------------------- */

#if defined(alpha_TARGET_ARCH)
# define REG(x) __asm__("$" #x)

#  define CALLER_SAVES_R2
#  define CALLER_SAVES_R3
#  define CALLER_SAVES_R4
#  define CALLER_SAVES_R5
#  define CALLER_SAVES_R6
#  define CALLER_SAVES_R7
#  define CALLER_SAVES_R8
  
#  define CALLER_SAVES_USER
  
#  define REG_R1	14
#  define REG_R2    	2
#  define REG_R3    	3
#  define REG_R4    	4
#  define REG_R5    	5
#  define REG_R6    	6
#  define REG_R7    	7
#  define REG_R8    	8
  
#  define REG_F1	f2
#  define REG_F2	f3
#  define REG_F3	f4
#  define REG_F4	f5
  
#  define REG_D1	f6
#  define REG_D2	f7
  
#  define REG_Sp    	9
#  define REG_Su    	10
#  define REG_SpLim     11

#  define REG_Hp	12
#  define REG_HpLim	13
  
#  define NCG_Reserved_I1 22
#  define NCG_Reserved_I2 27
#  define NCG_Reserved_F1 f29
#  define NCG_Reserved_F2 f30

#endif /* alpha_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   The HP-PA register mapping

   We cater for HP-PA 1.1.
   
   \tr{%r0}--\tr{%r1} are special.
   \tr{%r2} is the return pointer.
   \tr{%r3} is the frame pointer.
   \tr{%r4}--\tr{%r18} are callee-save registers.
   \tr{%r19} is a linkage table register for HPUX 8.0 shared libraries.
   \tr{%r20}--\tr{%r22} are caller-save registers.
   \tr{%r23}--\tr{%r26} are parameter registers.
   \tr{%r27} is a global data pointer.
   \tr{%r28}--\tr{%r29} are temporaries.
   \tr{%r30} is the stack pointer.
   \tr{%r31} is a temporary.
   
   \tr{%fr12}--\tr{%fr15} are some callee-save floating-point registers.
   \tr{%fr8}--\tr{%fr11} are some available caller-save fl-pt registers.
   -------------------------------------------------------------------------- */

#if hppa1_1_TARGET_ARCH

#define REG(x) __asm__("%" #x)

#define REG_R1		r11
#define REG_R2    	r12
#define REG_R3    	r13
#define REG_R4    	r14
#define REG_R5    	r15
#define REG_R6    	r16
#define REG_R7    	r17
#define REG_R8    	r18

#define REG_F1		fr12
#define REG_F2		fr12R
#define REG_F3		fr13
#define REG_F4		fr13R

#define REG_D1		fr20	/* L & R */
#define REG_D2		fr21	/* L & R */

#define REG_Sp    	r4
#define REG_Su    	r5
#define REG_SpLim    	r6

#define REG_Hp	    	r7
#define REG_HpLim	r8

#define NCG_Reserved_I1 r28
#define NCG_Reserved_I2	r29
#define NCG_Reserved_F1	fr8
#define NCG_Reserved_F2	fr8R
#define NCG_Reserved_D1	fr10
#define NCG_Reserved_D2	fr11

#endif /* hppa */

/* -----------------------------------------------------------------------------
   The Intel iX86 register mapping

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

   Leaving Su, SpLim, and HpLim out of the picture.
   -------------------------------------------------------------------------- */


#if i386_TARGET_ARCH

#define REG(x) __asm__("%" #x)

#define REG_Base    ebx
#define REG_Sp	    ebp

#if STOLEN_X86_REGS >= 3
# define REG_R1	    esi
#endif

#if STOLEN_X86_REGS >= 4
# define REG_Hp     edi
#endif

#define MAX_REAL_VANILLA_REG 1	/* always, since it defines the entry conv */
#define MAX_REAL_FLOAT_REG   0
#define MAX_REAL_DOUBLE_REG  0

#endif /* iX86 */

/* -----------------------------------------------------------------------------
   The Motorola 680x0 register mapping

   A Sun3 (mc680x0) has eight address registers, \tr{a0} to \tr{a7}, and
   eight data registers, \tr{d0} to \tr{d7}.  Address operations have to
   be done through address registers; data registers are used for
   comparison values and data.
   
   Here's the register-usage picture for m68k boxes with GCC.

   \begin{tabular}{ll}
   a0 & used directly by GCC \\
   a1 & used directly by GCC \\
   \\
   a2..a5 & callee-saved: available for STG registers \\
   & (a5 may be special, ``global'' register for PIC?) \\
   \\
   a6 & C-stack frame pointer \\
   a7 & C-stack pointer \\
   \\
   d0 & used directly by GCC \\
   d1 & used directly by GCC \\
   d2 & really needed for local optimisation by GCC \\
   \\
   d3..d7 & callee-saved: available for STG registers
   \\
   fp0 & call-clobbered \\
   fp1 & call-clobbered \\
   fp2..fp7 & callee-saved: available for STG registers
   \end{tabular}
   -------------------------------------------------------------------------- */

#if m68k_TARGET_ARCH

#define REG(x) __asm__(#x)

#define REG_Base	a2

#define REG_Sp		a3
#define REG_Su     	a4
#define REG_SpLim	d3

#define REG_Hp	    	d4
#define REG_HpLim       d5

#define REG_R1    	a5
#define REG_R2    	d6
#define MAX_REAL_VANILLA_REG 2

#define REG_Ret		d7

#define REG_F1		fp2
#define REG_F2		fp3
#define REG_F3		fp4
#define REG_F4		fp5

#define REG_D1		fp6
#define REG_D2		fp7

#endif /* m68k */

/* -----------------------------------------------------------------------------
   The DECstation (MIPS) register mapping

   Here's at least some simple stuff about registers on a MIPS.
   
   \tr{s0}--\tr{s7} are callee-save integer registers; they are our
   ``prize'' stolen registers.  There is also a wad of callee-save
   floating-point registers, \tr{$f20}--\tr{$f31}; we'll use some of
   those.
   
   \tr{t0}--\tr{t9} are caller-save (``temporary?'') integer registers.
   We can steal some, but we might have to save/restore around ccalls.
   -------------------------------------------------------------------------- */

#if mipsel_TARGET_ARCH || mipseb_TARGET_ARCH

#define REG(x) __asm__("$" #x)

#define CALLER_SAVES_R1
#define CALLER_SAVES_R2
#define CALLER_SAVES_R3
#define CALLER_SAVES_R4
#define CALLER_SAVES_R5
#define CALLER_SAVES_R6
#define CALLER_SAVES_R7
#define CALLER_SAVES_R8

#define CALLER_SAVES_USER

#define REG_R1		9
#define REG_R2    	10
#define REG_R3    	11
#define REG_R4    	12
#define REG_R5    	13
#define REG_R6    	14
#define REG_R7    	15
#define REG_R8    	24

#define REG_F1		f20
#define REG_F2		f22
#define REG_F3		f24
#define REG_F4		f26

#define REG_D1		f28
#define REG_D2		f30

#define REG_Sp    	16
#define REG_Su    	17
#define REG_SpLim    	18

#define REG_Hp	    	19
#define REG_HpLim	20

#endif /* mipse[lb] */

/* -----------------------------------------------------------------------------
   The PowerPC register mapping

   0		system glue?	(caller-save, volatile)
   1		SP		(callee-save, non-volatile)
   2		RTOC		(callee-save, non-volatile)
   3-10		args/return	(caller-save, volatile)
   11,12	system glue?	(caller-save, volatile)
   13-31			(callee-save, non-volatile)
   
   f0				(caller-save, volatile)
   f1-f13	args/return	(caller-save, volatile)
   f14-f31			(callee-save, non-volatile)
   
   \tr{13}--\tr{31} are wonderful callee-save registers.
   \tr{0}--\tr{12} are caller-save registers.
   
   \tr{%f14}--\tr{%f31} are callee-save floating-point registers.
   
   I think we can do the Whole Business with callee-save registers only!
   -------------------------------------------------------------------------- */

#if powerpc_TARGET_ARCH || rs6000_TARGET_ARCH

#define REG(x) __asm__(#x)

#define REG_R1		r14
#define REG_R2    	r15
#define REG_R3    	r16
#define REG_R4    	r17
#define REG_R5    	r18
#define REG_R6    	r19
#define REG_R7    	r20
#define REG_R8    	r21

#define REG_F1		fr14
#define REG_F2		fr15
#define REG_F3		fr16
#define REG_F4		fr17

#define REG_D1		fr18
#define REG_D2		fr19

#define REG_Sp    	r22
#define REG_Su    	r23
#define REG_SpLim    	r24

#define REG_Hp	    	r25
#define REG_HpLim	r26

#endif /* powerpc */

/* -----------------------------------------------------------------------------
   The Sun SPARC register mapping

   The SPARC register (window) story: Remember, within the Haskell
   Threaded World, we essentially ``shut down'' the register-window
   mechanism---the window doesn't move at all while in this World.  It
   *does* move, of course, if we call out to arbitrary~C...
   
   The %i, %l, and %o registers (8 each) are the input, local, and
   output registers visible in one register window.  The 8 %g (global)
   registers are visible all the time.
   
   %o0..%o7 	       	not available; can be zapped by callee
   			  (%o6 is C-stack ptr; %o7 hold ret addrs)
   %i0..%i7            	available (except %i6 is used as frame ptr)
   			  (and %i7 tends to have ret-addr-ish things)
   %l0..%l7	       	available
   %g0..%g4 		not available; prone to stomping by division, etc.
   %g5..%g7 		not available; reserved for the OS

   Note: %g3 is *definitely* clobbered in the builtin divide code (and
   our save/restore machinery is NOT GOOD ENOUGH for that); discretion
   being the better part of valor, we also don't take %g4.
   -------------------------------------------------------------------------- */

#if sparc_TARGET_ARCH

#define REG(x) __asm__("%" #x)

#define CALLER_SAVES_USER

#define CALLER_SAVES_F1
#define CALLER_SAVES_F2
#define CALLER_SAVES_F3
#define CALLER_SAVES_F4
#define CALLER_SAVES_D1
#define CALLER_SAVES_D2

#define REG_R1	    	l1
#define REG_R2    	l2
#define REG_R3    	l3
#define REG_R4    	l4
#define REG_R5    	l5
#define REG_R6    	l6
#define REG_R7    	l7
#define REG_R8		i5

#define REG_F1	    	f2
#define REG_F2	    	f3
#define REG_F3	    	f4
#define REG_F4	    	f5
#define REG_D1	    	f6
#define REG_D2	    	f8

#define REG_Sp    	i0
#define REG_Su    	i1
#define REG_SpLim    	i2

#define REG_Hp	    	i3
#define REG_HpLim	i4

#define NCG_Reserved_I1	g1
#define NCG_Reserved_I2	g2
#define NCG_Reserved_F1	f14
#define NCG_Reserved_F2 f15
#define NCG_Reserved_D1	f16
#define NCG_Reserved_D2	f18

#endif /* sparc */

/* These constants define how many stg registers are *actually* in
 * registers: the code generator uses this information to avoid
 * generating code to load/store registers which are really offsets
 * from BaseReg.
 *
 * Registers above these values might still be used, for instance to
 * communicate with PrimOps and RTS functions.
 */

#ifndef MAX_REAL_VANILLA_REG
#define MAX_REAL_VANILLA_REG 8
#endif

#ifndef MAX_REAL_FLOAT_REG
#define MAX_REAL_FLOAT_REG 4
#endif

#ifndef MAX_REAL_DOUBLE_REG
#define MAX_REAL_DOUBLE_REG 2
#endif

#endif /* NO_REGS */

#endif /* MACHREGS_H */
