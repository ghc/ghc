/* -----------------------------------------------------------------------------
 * $Id: StgCRun.c,v 1.27 2002/01/07 22:35:55 ken Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * STG-to-C glue.
 *
 * To run an STG function from C land, call
 *
 *		rv = StgRun(f,BaseReg);
 *
 * where "f" is the STG function to call, and BaseReg is the address of the
 * RegTable for this run (we might have separate RegTables if we're running
 * multiple threads on an SMP machine).
 *
 * In the end, "f" must JMP to StgReturn (defined below),
 * passing the return-value "rv" in R1,
 * to return to the caller of StgRun returning "rv" in
 * the whatever way C returns a value.
 *
 * NOTE: StgRun/StgReturn do *NOT* load or store Hp or any
 * other registers (other than saving the C callee-saves
 * registers).  Instead, the called function "f" must do that
 * in STG land.
 *
 * GCC will have assumed that pushing/popping of C-stack frames is
 * going on when it generated its code, and used stack space
 * accordingly.  However, we actually {\em post-process away} all
 * such stack-framery (see \tr{ghc/driver/ghc-asm.lprl}). Things will
 * be OK however, if we initially make sure there are
 * @RESERVED_C_STACK_BYTES@ on the C-stack to begin with, for local
 * variables.
 *
 * -------------------------------------------------------------------------- */

#include "PosixSource.h"


/*
 * We define the following (unused) global register variables, because for
 * some reason gcc generates sub-optimal code for StgRun() on the Alpha
 * (unnecessarily saving extra registers on the stack) if we don't.
 *
 * Why do it at the top of this file, rather than near StgRun() below?  Because
 * gcc doesn't let us define global register variables after any function
 * definition has been read.  Any point after #include "Stg.h" would be too
 * late.
 *
 * We define alpha_EXTRA_CAREFUL here to save $s6, $f8 and $f9 -- registers
 * that we don't use but which are callee-save registers.  The __divq() routine
 * in libc.a clobbers $s6.
 */
#include "config.h"
#ifdef alpha_TARGET_ARCH
#define alpha_EXTRA_CAREFUL
register long   fake_ra __asm__("$26");
#ifdef alpha_EXTRA_CAREFUL
register long   fake_s6 __asm__("$15");
register double fake_f8 __asm__("$f8");
register double fake_f9 __asm__("$f9");
#endif
#endif

/* include Stg.h first because we want real machine regs in here: we
 * have to get the value of R1 back from Stg land to C land intact.
 */
#include "Stg.h"
#include "Rts.h"
#include "StgRun.h"

#ifdef DEBUG
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Printer.h"
#endif

#ifdef USE_MINIINTERPRETER

/* -----------------------------------------------------------------------------
   any architecture (using miniinterpreter)
   -------------------------------------------------------------------------- */

extern StgThreadReturnCode StgRun(StgFunPtr f, StgRegTable *basereg)
{
   while (f) {
      IF_DEBUG(evaluator,
	       fprintf(stderr,"Jumping to ");
	       printPtr((P_)f);
	       fprintf(stderr,"\n");
	      );
      f = (StgFunPtr) (f)();
   }
   return (StgThreadReturnCode)R1.i;
}

EXTFUN(StgReturn)
{
   return 0;
}

#else /* !USE_MINIINTERPRETER */

#ifdef LEADING_UNDERSCORE
#define STG_RETURN "_StgReturn"
#else
#define STG_RETURN "StgReturn"
#endif

/* -----------------------------------------------------------------------------
   x86 architecture
   -------------------------------------------------------------------------- */

#ifdef i386_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) {

    unsigned char space[ RESERVED_C_STACK_BYTES + 4*sizeof(void *) ];
    StgThreadReturnCode r;

    __asm__ volatile (
	/*
	 * save callee-saves registers on behalf of the STG code.
	 */
	"movl %%esp, %%eax\n\t"
	"addl %4, %%eax\n\t"
        "movl %%ebx,0(%%eax)\n\t"
        "movl %%esi,4(%%eax)\n\t"
        "movl %%edi,8(%%eax)\n\t"
        "movl %%ebp,12(%%eax)\n\t"
	/*
	 * Set BaseReg
	 */
	"movl %3,%%ebx\n\t"
	/*
	 * grab the function argument from the stack, and jump to it.
	 */
        "movl %2,%%eax\n\t"
        "jmp *%%eax\n\t"

	".global " STG_RETURN "\n"
       	STG_RETURN ":\n\t"

	"movl %%esi, %%eax\n\t"   /* Return value in R1  */

	/*
	 * restore callee-saves registers.  (Don't stomp on %%eax!)
	 */
	"movl %%esp, %%edx\n\t"
	"addl %4, %%edx\n\t"
        "movl 0(%%edx),%%ebx\n\t"	/* restore the registers saved above */
        "movl 4(%%edx),%%esi\n\t"
        "movl 8(%%edx),%%edi\n\t"
        "movl 12(%%edx),%%ebp\n\t"

      : "=&a" (r), "=m" (space)
      : "m" (f), "m" (basereg), "i" (RESERVED_C_STACK_BYTES)
      : "edx" /* stomps on %edx */
    );

    return r;
}

#endif

/* -----------------------------------------------------------------------------
   Sparc architecture

   --
   OLD COMMENT from GHC-3.02:

   We want tailjumps to be calls, because `call xxx' is the only Sparc
   branch that allows an arbitrary label as a target.  (Gcc's ``goto
   *target'' construct ends up loading the label into a register and
   then jumping, at the cost of two extra instructions for the 32-bit
   load.)

   When entering the threaded world, we stash our return address in a
   known location so that \tr{%i7} is available as an extra
   callee-saves register.  Of course, we have to restore this when
   coming out of the threaded world.

   I hate this god-forsaken architecture.  Since the top of the
   reserved stack space is used for globals and the bottom is reserved
   for outgoing arguments, we have to stick our return address
   somewhere in the middle.  Currently, I'm allowing 100 extra
   outgoing arguments beyond the first 6.  --JSM

   Updated info (GHC 4.06): we don't appear to use %i7 any more, so
   I'm not sure whether we still need to save it.  Incedentally, what
   does the last paragraph above mean when it says "the top of the
   stack is used for globals"?  What globals?  --SDM

   Updated info (GHC 4.08.2): not saving %i7 any more (see below).
   -------------------------------------------------------------------------- */

#ifdef sparc_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg) {

    unsigned char space[RESERVED_C_STACK_BYTES];
#if 0
    register void *i7 __asm__("%i7");
    ((void **)(space))[100] = i7;
#endif
    f();
    __asm__ volatile (
	    ".align 4\n"
            ".global " STG_RETURN "\n"
       	    STG_RETURN ":"
	    : : : "l0","l1","l2","l3","l4","l5","l6","l7");
    /* we tell the C compiler that l0-l7 are clobbered on return to
     * StgReturn, otherwise it tries to use these to save eg. the
     * address of space[100] across the call.  The correct thing
     * to do would be to save all the callee-saves regs, but we
     * can't be bothered to do that.
     *
     * The code that gcc generates for this little fragment is now
     * terrible.  We could do much better by coding it directly in
     * assembler.
     */
#if 0
    /* updated 4.08.2: we don't save %i7 in the middle of the reserved
     * space any more, since gcc tries to save its address across the
     * call to f(), this gets clobbered in STG land and we end up
     * dereferencing a bogus pointer in StgReturn.
     */
    __asm__ volatile ("ld %1,%0"
		      : "=r" (i7) : "m" (((void **)(space))[100]));
#endif
    return (StgThreadReturnCode)R1.i;
}

#endif

/* -----------------------------------------------------------------------------
   alpha architecture

   "The stack pointer (SP) must at all times denote an address that has octaword
    alignment. (This restriction has the side effect that the in-memory portion
    of the argument list, if any, will start on an octaword boundary.) Note that
    the stack grows toward lower addresses. During a procedure invocation, SP
    can never be set to a value that is higher than the value of SP at entry to
    that procedure invocation.

   "The contents of the stack, located above the portion of the argument list
    (if any) that is passed in memory, belong to the calling procedure. Because
    they are part of the calling procedure, they should not be read or written
    by the called procedure, except as specified by indirect arguments or
    language-controlled up-level references.

   "The SP value might be used by the hardware when raising exceptions and
    asynchronous interrupts. It must be assumed that the contents of the stack
    below the current SP value and within the stack for the current thread are
    continually and unpredictably modified, as specified in the _Alpha
    Architecture Reference Manual_, and as a result of asynchronous software
    actions."

   -- Compaq Computer Corporation, Houston. Tru64 UNIX Calling Standard for
      Alpha Systems, 5.1 edition, August 2000, section 3.2.1.  http://www.
      tru64unix.compaq.com/docs/base_doc/DOCUMENTATION/V51_PDF/ARH9MBTE.PDF
   -------------------------------------------------------------------------- */

#ifdef alpha_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg)
{
    register long   real_ra __asm__("$26"); volatile long   save_ra;

    register long   real_s0 __asm__("$9" ); volatile long   save_s0;
    register long   real_s1 __asm__("$10"); volatile long   save_s1;
    register long   real_s2 __asm__("$11"); volatile long   save_s2;
    register long   real_s3 __asm__("$12"); volatile long   save_s3;
    register long   real_s4 __asm__("$13"); volatile long   save_s4;
    register long   real_s5 __asm__("$14"); volatile long   save_s5;
#ifdef alpha_EXTRA_CAREFUL
    register long   real_s6 __asm__("$15"); volatile long   save_s6;
#endif

    register double real_f2 __asm__("$f2"); volatile double save_f2;
    register double real_f3 __asm__("$f3"); volatile double save_f3;
    register double real_f4 __asm__("$f4"); volatile double save_f4;
    register double real_f5 __asm__("$f5"); volatile double save_f5;
    register double real_f6 __asm__("$f6"); volatile double save_f6;
    register double real_f7 __asm__("$f7"); volatile double save_f7;
#ifdef alpha_EXTRA_CAREFUL
    register double real_f8 __asm__("$f8"); volatile double save_f8;
    register double real_f9 __asm__("$f9"); volatile double save_f9;
#endif

    register StgFunPtr real_pv __asm__("$27");

    StgThreadReturnCode ret;

    save_ra = real_ra;

    save_s0 = real_s0;
    save_s1 = real_s1;
    save_s2 = real_s2;
    save_s3 = real_s3;
    save_s4 = real_s4;
    save_s5 = real_s5;
#ifdef alpha_EXTRA_CAREFUL
    save_s6 = real_s6;
#endif

    save_f2 = real_f2;
    save_f3 = real_f3;
    save_f4 = real_f4;
    save_f5 = real_f5;
    save_f6 = real_f6;
    save_f7 = real_f7;
#ifdef alpha_EXTRA_CAREFUL
    save_f8 = real_f8;
    save_f9 = real_f9;
#endif

    real_pv = f;

    __asm__ volatile(	"lda $30,-%0($30)"	"\n"
		"\t"	"jmp ($27)"		"\n"
		"\t"	".align 3"		"\n"
		".globl " STG_RETURN		"\n"
		STG_RETURN ":"			"\n"
		"\t"	"lda $30,%0($30)"	"\n"
		: : "K" (RESERVED_C_STACK_BYTES));

    ret = real_s5;

    real_s0 = save_s0;
    real_s1 = save_s1;
    real_s2 = save_s2;
    real_s3 = save_s3;
    real_s4 = save_s4;
    real_s5 = save_s5;
#ifdef alpha_EXTRA_CAREFUL
    real_s6 = save_s6;
#endif

    real_f2 = save_f2;
    real_f3 = save_f3;
    real_f4 = save_f4;
    real_f5 = save_f5;
    real_f6 = save_f6;
    real_f7 = save_f7;
#ifdef alpha_EXTRA_CAREFUL
    real_f8 = save_f8;
    real_f9 = save_f9;
#endif

    real_ra = save_ra;

    return ret;
}

#endif /* alpha_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   HP-PA architecture
   -------------------------------------------------------------------------- */

#ifdef hppa1_1_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f, StgRegTable *basereg)
{
    StgChar space[RESERVED_C_STACK_BYTES+16*sizeof(long)+10*sizeof(double)];
    StgThreadReturnCode ret;

    __asm__ volatile ("ldo %0(%%r30),%%r19\n"
		      "\tstw %%r3, 0(0,%%r19)\n"
                      "\tstw %%r4, 4(0,%%r19)\n"
                      "\tstw %%r5, 8(0,%%r19)\n"
                      "\tstw %%r6,12(0,%%r19)\n"
                      "\tstw %%r7,16(0,%%r19)\n"
                      "\tstw %%r8,20(0,%%r19)\n"
                      "\tstw %%r9,24(0,%%r19)\n"
		      "\tstw %%r10,28(0,%%r19)\n"
                      "\tstw %%r11,32(0,%%r19)\n"
                      "\tstw %%r12,36(0,%%r19)\n"
                      "\tstw %%r13,40(0,%%r19)\n"
                      "\tstw %%r14,44(0,%%r19)\n"
                      "\tstw %%r15,48(0,%%r19)\n"
                      "\tstw %%r16,52(0,%%r19)\n"
                      "\tstw %%r17,56(0,%%r19)\n"
                      "\tstw %%r18,60(0,%%r19)\n"
		      "\tldo 80(%%r19),%%r19\n"
		      "\tfstds %%fr12,-16(0,%%r19)\n"
		      "\tfstds %%fr13, -8(0,%%r19)\n"
		      "\tfstds %%fr14,  0(0,%%r19)\n"
		      "\tfstds %%fr15,  8(0,%%r19)\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfstds %%fr16,-16(0,%%r19)\n"
		      "\tfstds %%fr17, -8(0,%%r19)\n"
		      "\tfstds %%fr18,  0(0,%%r19)\n"
		      "\tfstds %%fr19,  8(0,%%r19)\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfstds %%fr20,-16(0,%%r19)\n"
		      "\tfstds %%fr21, -8(0,%%r19)\n" : :
                      "n" (-(116 * sizeof(long) + 10 * sizeof(double))) : "%r19"
		      );

    f();

    __asm__ volatile (".align 4\n"
               	      "\t.EXPORT " STG_RETURN ",CODE\n"
		      "\t.EXPORT " STG_RETURN ",ENTRY,PRIV_LEV=3\n"
                      STG_RETURN "\n"
                      /* "\tldo %0(%%r3),%%r19\n" */
                      "\tldo %1(%%r30),%%r19\n"
                      "\tcopy %%r11, %0\n"  /* save R1 */
		      "\tldw  0(0,%%r19),%%r3\n"
                      "\tldw  4(0,%%r19),%%r4\n"
                      "\tldw  8(0,%%r19),%%r5\n"
                      "\tldw 12(0,%%r19),%%r6\n"
                      "\tldw 16(0,%%r19),%%r7\n"
                      "\tldw 20(0,%%r19),%%r8\n"
                      "\tldw 24(0,%%r19),%%r9\n"
		      "\tldw 28(0,%%r19),%%r10\n"
                      "\tldw 32(0,%%r19),%%r11\n"
                      "\tldw 36(0,%%r19),%%r12\n"
                      "\tldw 40(0,%%r19),%%r13\n"
                      "\tldw 44(0,%%r19),%%r14\n"
                      "\tldw 48(0,%%r19),%%r15\n"
                      "\tldw 52(0,%%r19),%%r16\n"
                      "\tldw 56(0,%%r19),%%r17\n"
                      "\tldw 60(0,%%r19),%%r18\n"
		      "\tldo 80(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr12\n"
		      "\tfldds  -8(0,%%r19),%%fr13\n"
		      "\tfldds   0(0,%%r19),%%fr14\n"
		      "\tfldds   8(0,%%r19),%%fr15\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr16\n"
		      "\tfldds  -8(0,%%r19),%%fr17\n"
		      "\tfldds   0(0,%%r19),%%fr18\n"
		      "\tfldds   8(0,%%r19),%%fr19\n"
		      "\tldo 32(%%r19),%%r19\n"
		      "\tfldds -16(0,%%r19),%%fr20\n"
		      "\tfldds  -8(0,%%r19),%%fr21\n"
		         : "=r" (ret)
		         : "n" (-(116 * sizeof(long) + 10 * sizeof(double)))
		         : "%r19"
		      );

    return ret;
}

#endif /* hppa1_1_TARGET_ARCH */

#endif /* !USE_MINIINTERPRETER */
