/* -----------------------------------------------------------------------------
 * $Id: StgCRun.c,v 1.3 1999/02/05 16:02:57 simonm Exp $
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

#endif /* sparc_TARGET_ARCH */

#endif /* !USE_MINIINTERPRETER */
