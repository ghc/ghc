
/* --------------------------------------------------------------------------
 * Assembly code to call C and Haskell functions 
 *
 * Copyright (c) 1994-1999.
 *
 * $RCSfile: universal_call_c.S,v $
 * $Revision: 1.7 $
 * $Date: 1999/10/27 15:29:37 $
 * ------------------------------------------------------------------------*/
	
#include "config.h"
#include "options.h"	
	
#ifdef INTERPRETER
	.file "universal_call_c.S"

#if 0
   Implement this.  See comment in rts/ForeignCall.c for details.

   void universal_call_c_ARCHNAME
	                 ( int   n_args,
                           void* args, 
                           char* argstr, 
                           void* fun )

   You can get a crude approximation to the assembly you need by
   compiling the following:

      extern void pingi64 ( unsigned long long int );
      extern void pingi32 ( unsigned int );
      extern void pingf32 ( float f );
      extern void pingf64 ( double d );
      
      void universal_call_c_ARCHNAME ( int   n_args,
                                       void* args, 
                                       char* argstr, 
                                       void* fun )
      {
         int i;
         for (i = 1; i <= n_args; i++) {
           if (argstr[i] == 'i') {
              unsigned int u1 = ((unsigned int*)args)[2*i];
              pingi32(u1);
           } else
           if (argstr[i] == 'I') {
              unsigned long long int uu1 = ((unsigned long long int*)args)[i];
              pingi64(uu1);
           } else
           if (argstr[i] == 'f') {
              float u1 = ((float*)args)[2*i];
              pingf32(u1);
           } else
           if (argstr[i] == 'F') {
              double u1 = ((double*)args)[i];
              pingf64(u1);
           }
         }
      
         if (argstr[0] == 'f' || argstr[0] == 'F') {
            pingi32(987654321);
         } else {
            pingi32(123456789);
         }
      }
#endif
	
#if i386_TARGET_ARCH

/*
 * Tricky!  Calls the specified function using ccall convention,
 * *and* assumes that I myself was called using ccall.
 */

.globl universal_call_c_x86_ccall
universal_call_c_x86_ccall:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%esi
	movl 16(%ebp),%edi
	movl 8(%ebp),%ebx
	testl %ebx,%ebx
	jle .Lcdocall
	
.Lclooptop:
	cmpb $105,(%ebx,%edi)	# 'i'
	jne .Lc6
	pushl (%esi,%ebx,8)
	jmp .Lclooptest
.Lc6:
	cmpb $73,(%ebx,%edi)	# 'I'
	jne .Lc8
	pushl 4(%esi,%ebx,8)
	pushl (%esi,%ebx,8)
	jmp .Lclooptest
.Lc8:
	cmpb $102,(%ebx,%edi)	# 'f'
	jne .Lc10
	movl (%esi,%ebx,8),%eax
	pushl %eax
	jmp .Lclooptest
.Lc10:
	cmpb $70,(%ebx,%edi)	# 'F'
	jne .Lclooptest
	movl 4(%esi,%ebx,8),%eax
	movl (%esi,%ebx,8),%edx
	pushl %eax
	pushl %edx
.Lclooptest:
	decl %ebx
        testl %ebx,%ebx
	jg .Lclooptop

.Lcdocall:	
	call *20(%ebp)
	
	cmpb $102,(%edi)	# 'f'
	je .Lcfloat32
	cmpb $70,(%edi)		# 'F'
	je .Lcfloat64
.LciorI:
	movl %eax,0(%esi)
	movl %edx,4(%esi)
	jmp .Lcbye
.Lcfloat32:
	fstps 0(%esi)
	jmp .Lcbye
.Lcfloat64:
	fstpl 0(%esi)
	jmp .Lcbye	
.Lcbye:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret


	
# Almost identical to the above piece of code
# see comments near end for differences 

# Even more tricky!  Calls the specified function using 
# stdcall convention, *but* assumes that I myself was called 
# using ccall.
	
.globl universal_call_c_x86_stdcall
universal_call_c_x86_stdcall:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%esi
	movl 16(%ebp),%edi
	movl 8(%ebp),%ebx
	testl %ebx,%ebx
	jle .Lsdocall
	
.Lslooptop:
	cmpb $105,(%ebx,%edi)	# 'i'
	jne .Ls6
	pushl (%esi,%ebx,8)
	jmp .Lslooptest
.Ls6:
	cmpb $73,(%ebx,%edi)	# 'I'
	jne .Ls8
	pushl 4(%esi,%ebx,8)
	pushl (%esi,%ebx,8)
	jmp .Lslooptest
.Ls8:
	cmpb $102,(%ebx,%edi)	# 'f'
	jne .Ls10
	movl (%esi,%ebx,8),%eax
	pushl %eax
	jmp .Lslooptest
.Ls10:
	cmpb $70,(%ebx,%edi)	# 'F'
	jne .Lslooptest
	movl 4(%esi,%ebx,8),%eax
	movl (%esi,%ebx,8),%edx
	pushl %eax
	pushl %edx
.Lslooptest:
	decl %ebx
        testl %ebx,%ebx
	jg .Lslooptop

.Lsdocall:	
	call *20(%ebp)
	
	cmpb $102,(%edi)	# 'f'
	je .Lsfloat32
	cmpb $70,(%edi)		# 'F'
	je .Lsfloat64
.LsiorI:
	movl %eax,0(%esi)
	movl %edx,4(%esi)
	jmp .Lsbye
.Lsfloat32:
	fstps 0(%esi)
	jmp .Lsbye
.Lsfloat64:
	fstpl 0(%esi)
	jmp .Lsbye	
.Lsbye:
	/* don_t clear the args -- the callee does it */
	/* leal -12(%ebp),%esp */
	popl %ebx
	popl %esi
	popl %edi
	leave
	/* ret $16     # but we have to clear our own! (no! we were ccall_d) */
	ret

#endif /* i386_TARGET_ARCH */
	
#endif /* INTERPRETER */
