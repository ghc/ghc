/* --------------------------------------------------------------------------
 * Assembly code to call C and Haskell functions 
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: callfun.S,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/15 13:08:03 $
 * ------------------------------------------------------------------------*/
	
#include "config.h"
#include "options.h"	
	
#ifdef INTERPRETER
	.file "callfun.S"

/* No longer needed - I finally figured out how to use __builtin_apply	*/
#if 0 && i386_TARGET_ARCH

#if 0	
  void ccall( CFunDescriptor* d, void* fun )
  {
      void *rs=alloca(d->result_size);
      void *as=alloca(d->arg_size);
      unmarshall(d->arg_tys,as);
      rs = fun(as)		; 
      marshall(d->result_tys,rs);
  }

  On entry, we have:	
    ret   =  0(%esp)
    d     =  4(%esp)
    fun   =  8(%esp)	
  	  
  We assume that %ebp is a callee saves register
  and that %ecx is not used to return the result.
  If %ecx is a callee saves register (I think it is), the code
  can be optimised slightly - but I doubt its worth it.	
#endif
.globl ccall
ccall:
	pushl %ebp           /* Save stack frame pointer       */
	pushl %ecx           /* Save callee-saves register     */
		
	leal  8(%esp), %ebp  /* ebp = frame pointer            */
	movl  4(%ebp), %ecx  /* ecx = d;                       */
	subl 12(%ecx), %esp  /* rs  = alloca(d->result_size);  */
	subl  4(%ecx), %esp  /* as  = alloca(d->arg_size);     */

	/* Marshall arguments off STG stack */	
	pushl %esp
	pushl 0(%ecx)
	call  unmarshall
	addl  $8,%esp        /* unmarshall(d->arg_tys,as);     */

	/* Call function */
	movl  8(%ebp), %ecx 
	call  *%ecx          /* rs = fun(as);                  */

	movl  4(%ebp), %ecx  /* ecx = d;                       */
	addl  4(%ecx), %esp  /* free(as)                       */


	/* Save result in rs - assume one or zero results for now */
	movl  8(%ecx), %ecx  /* ecx = d->result_tys            */

	cmpl  $0,(%ecx)      /* '\0' = no result               */
	je    .args_saved

	cmpl  $70,(%ecx)     /* 'F' = float result             */
	jne   .not_float
	flds  (%esp)         /* *rs = (float)f1                */
	jmp   .args_saved

.not_float:
	cmpl  $68,(%ecx)     /* 'D' = double result            */
	jne   .not_double
	fldl  (%esp)         /* *rs = (double)f1               */
	jmp   .args_saved

.not_double:	
	movl  %eax,(%esp)    /* *rs = eax                      */
	/* fall through to .args_saved */

	/* Marshall results back onto STG stack */
.args_saved:
	pushl %esp					       
	movl  4(%ebp), %ecx  /* ecx = d;                       */
	pushl 8(%ecx)					       
	call  marshall					       
	addl  $8,%esp        /* marshall(d->result_tys,rs);    */

	
	movl  4(%ebp), %ecx  /* ecx = d;                       */
	addl  12(%ecx), %esp /* free(rs)                       */

	popl %ecx            /* Restore callee-saves register  */
	popl %ebp            /* restore stack frame pointer    */
	ret                  
	
#if 0
/* When we call a Fun, we push the arguments on the stack, push a return
 * address and execute the instruction "call callFun_entry" which brings us
 * here with a return address on top of the stack, a pointer to
 * the FunDescriptor under that and the arguments under that.
 * We swap the top arguments so that when we jmp to callFunDesc, the stack
 * will look as though we executed "callFunDesc(fDescriptor,arg1,arg2,...)"
 */
	
	/* function call/return - standard entry point
	 * we'll have one of these for each calling convention
	 * all of which jump to callFunDesc when done
	 */	
	.globl callFun_entry
	.type  callFun_entry,@function
callFun_entry:	
	popl  %eax   /* FunDescriptor  */
	popl  %edx   /* Return address */
	pushl %eax
	pushl %edx
	jmp   callFunDesc

	/* generic function call/return */
callFunDesc:
	subl  $8,%esp        /* int/double res1;  */
	pushl %esp           /* &res1             */
	leal  20(%esp),%ecx  /* &arg1             */
	pushl %ecx 
	pushl 20(%esp)       /* fun               */
	call  call_H         /* returns result type in %eax */
	addl  $20,%esp

	testl %eax,%eax      /* '\0' = no result */
	jne   .L1
	ret
.L1:
	cmpl  $70,%eax       /* 'F' = float result */
	jne   .L2
	flds  -8(%esp)
	ret
.L2:
	cmpl  $68,%eax       /* 'D' = double result */
	jne   .L3
	fldl  -8(%esp)
	ret
.L3:
	movl  -8(%esp),%eax  /* return r          */
	ret


/* Some useful instructions - for later use:		
 *	fstpl (%ebx)  store a double
 *	fstps (%ebx)  store a float
 *
 *	fldl (%esi)   load a double (ready for return)
 *	flds (%esi)   load a float (ready for return)
 */
#endif /* 0 */
	
#endif /* i386_TARGET_ARCH */
	
#endif /* INTERPRETER */