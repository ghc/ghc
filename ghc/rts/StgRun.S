/* -----------------------------------------------------------------------------
 * $Id: StgRun.S,v 1.4 1999/12/01 14:21:02 simonmar Exp $
 *
 * Tiny assembler 'layer' between the C and STG worlds.
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
 * -------------------------------------------------------------------------- */

#include "config.h"
#include "Constants.h"	

#ifndef USE_MINIINTERPRETER
	
/* 
 * GCC will have assumed that pushing/popping of C-stack frames is
 * going on when it generated its code, and used stack space
 * accordingly.  However, we actually {\em post-process away} all
 * such stack-framery (see \tr{ghc/driver/ghc-asm.lprl}). Things will
 * be OK however, if we initially make sure there are
 * @RESERVED_C_STACK_BYTES@ on the C-stack to begin with, for local
 * variables.  
 */

/* -----------------------------------------------------------------------------
   x86 architecture
   -------------------------------------------------------------------------- */
	
#if i386_TARGET_ARCH	

.text
        .align 2

#ifdef LEADING_UNDERSCORE	
.globl _StgRun
_StgRun:
#else	
.globl StgRun
StgRun:
#endif
	pushl %ebp		/* standard frame-pointer stuff */
        movl %esp,%ebp

	/*
	 * leave a giant chunk of C-stack for temporaries in the STG world.
	 */
	subl $RESERVED_C_STACK_BYTES + 4*SIZEOF_LONG,%esp

	/* 
	 * save callee-saves registers on behalf of the STG code.
	 */
	leal RESERVED_C_STACK_BYTES(%esp),%eax
        movl %ebx,0(%eax)
        movl %esi,4(%eax)
        movl %edi,8(%eax)
        movl %ebp,12(%eax)

	/*
	 * Set BaseReg
	 */
	movl 12(%ebp),%ebx
	
	/*
	 * grab the function argument from the stack, and jump to it.
	 */
        movl 8(%ebp),%eax
        jmp *%eax

        .align 2

#ifdef LEADING_UNDERSCORE
.globl _StgReturn
_StgReturn:
#else	
.globl StgReturn
StgReturn:
#endif	
	movl %esi,%eax   /* Return value in R1  */

	/*
	 * restore callee-saves registers.  (Don't stomp on %eax!)
	 */
	leal RESERVED_C_STACK_BYTES(%esp),%edx
        movl 0(%edx),%ebx	/* restore the registers saved above */
        movl 4(%edx),%esi
        movl 8(%edx),%edi
        movl 12(%edx),%ebp

	movl %ebp, %esp		/* restore the C stack state */
	popl %ebp
        ret

#endif /* i386_TARGET_ARCH */

#endif /* !USE_MINIINTERPRETER */
