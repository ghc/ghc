/* -----------------------------------------------------------------------------
 * $Id: StgCRun.c,v 1.6 1999/07/06 16:40:27 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * STG-to-C glue.  Some architectures have this code written in
 * straight assembler (see StgRun.S), some in C.
 *
 * -------------------------------------------------------------------------- */

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
	
/* The static @jmp_environment@ variable allows @miniInterpret@ to
 * communicate with @StgReturn@.
 * 
 * Because @StgRun@ may be used recursively, we carefully
 * save and restore the whole of @jmp_environment@.
 */
#include <setjmp.h>
#include <string.h> /* for memcpy */

static jmp_buf jmp_environment;

#if 0

extern StgThreadReturnCode StgRun(StgFunPtr f)
{
    jmp_buf save_buf;
    /* Save jmp_environment for previous call to miniInterpret  */
    memcpy((void *) jmp_environment, (void *) save_buf, sizeof(jmp_buf));
    if (setjmp(jmp_environment) == 0) {
	while ( 1 ) {
            IF_DEBUG(evaluator,
		     fprintf(stderr,"Jumping to ");
		     printPtr((P_)f);
		     fprintf(stderr,"\n");
		     );
	    f = (StgFunPtr) (f)();
	}
    }
    /* Restore jmp_environment for previous call */
    memcpy((void*) save_buf, (void*) jmp_environment, sizeof(jmp_buf));

    return (StgThreadReturnCode)R1.i;
}

EXTFUN(StgReturn)
{
    longjmp(jmp_environment, 1);
}

#else

extern StgThreadReturnCode StgRun(StgFunPtr f)
{
    char* nm;
    while ( f ) {

#if 0
      //IF_DEBUG(evaluator,
                fprintf(stderr,"Jumping to ");
                nm = nameOfObjSym ( f );
                if (nm)
                   fprintf(stderr, "%s (%p)", nm, f); else
                   printPtr((P_)f);
                fprintf(stderr,"\n");
		//         );
if (0&& MainRegTable.rSp) {
   int i;
   StgWord* p = MainRegTable.rSp;
fprintf(stderr, "SP = %p\n", p);
   p += (8-1);
   for (i = 0; i < 8; i++, p--)
      fprintf (stderr, "-- %p: %p\n", p, *p );
}    
#endif    

       f = (StgFunPtr) (f)();
    }

    return (StgThreadReturnCode)R1.i;
}

EXTFUN(StgReturn)
{
   return 0;
}
#endif



#else /* !USE_MINIINTERPRETER */

#ifdef LEADING_UNDERSCORE
#define STG_RETURN "_StgReturn"
#else
#define STG_RETURN "StgReturn"
#endif

/* -----------------------------------------------------------------------------
   sparc architecture
   -------------------------------------------------------------------------- */
	
#ifdef sparc_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f) {

    StgChar space[RESERVED_C_STACK_BYTES+sizeof(void *)];
    register void *i7 __asm__("%i7");
    ((void **)(space))[100] = i7;
    f();
    __asm__ volatile (".align 4\n"		
            ".global " STG_RETURN "\n"
       	    STG_RETURN ":\n"
    	    "\tld %1,%0" : "=r" (i7) : "m" (((void **)(space))[100]));
    return (StgThreadReturnCode)R1.i;
}

#endif

/* -----------------------------------------------------------------------------
   alpha architecture
   -------------------------------------------------------------------------- */

#ifdef alpha_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f) 
{
    __asm__ volatile ("stq $9,-8($30)\n\t"
                      "stq $10,-16($30)\n\t"
                      "stq $11,-24($30)\n\t"
                      "stq $12,-32($30)\n\t"
                      "stq $13,-40($30)\n\t"
                      "stq $14,-48($30)\n\t"
                      "stq $15,-56($30)\n\t"
                      "stt $f2,-64($30)\n\t"
                      "stt $f3,-72($30)\n\t"
                      "stt $f4,-80($30)\n\t"
                      "stt $f5,-88($30)\n\t"
                      "stt $f6,-96($30)\n\t"
                      "stt $f7,-104($30)\n\t"
                      "stt $f8,-112($30)\n\t"
                      "stt $f9,-120($30)\n\t"
    	    	      "lda $30,-%0($30)" : :
                      "K" (RESERVED_C_STACK_BYTES+
			   8*sizeof(double)+8*sizeof(long)));

    f();

    __asm__ volatile (".align 3\n"
               	      ".globl " STG_RETURN "\n"
                      STG_RETURN ":\n\t"
               	      "lda $30,%0($30)\n\t"
               	      "ldq $9,-8($30)\n\t"
               	      "ldq $10,-16($30)\n\t"
               	      "ldq $11,-24($30)\n\t"
               	      "ldq $12,-32($30)\n\t"
               	      "ldq $13,-40($30)\n\t"
               	      "ldq $14,-48($30)\n\t"
               	      "ldq $15,-56($30)\n\t"
               	      "ldt $f2,-64($30)\n\t"
               	      "ldt $f3,-72($30)\n\t"
               	      "ldt $f4,-80($30)\n\t"
               	      "ldt $f5,-88($30)\n\t"
               	      "ldt $f6,-96($30)\n\t"
    	    	      "ldt $f7,-104($30)\n\t"
    	    	      "ldt $f8,-112($30)\n\t" 
    	    	      "ldt $f9,-120($30)" : :
                      "K" (RESERVED_C_STACK_BYTES+
			   8*sizeof(double)+8*sizeof(long)));

    return (StgThreadReturnCode)R1.i;
}

#endif /* alpha_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   HP-PA architecture
   -------------------------------------------------------------------------- */

#ifdef hppa1_1_TARGET_ARCH

StgThreadReturnCode
StgRun(StgFunPtr f) 
{
    StgChar space[RESERVED_C_STACK_BYTES+16*sizeof(long)+10*sizeof(double)];
    StgThredReturnCode ret;

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
		      : "=r" (ret) /* result */
                      : "n" (-(116 * sizeof(long) + 10 * sizeof(double))) 
		      : "%r19"
		      );

    return ret;
}

#endif /* hppa1_1_TARGET_ARCH */

#endif /* !USE_MINIINTERPRETER */
