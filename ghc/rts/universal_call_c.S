
/* --------------------------------------------------------------------------
 * Assembly code to call C and Haskell functions 
 *
 * Copyright (c) 1994-1999.
 *
 * $RCSfile: universal_call_c.S,v $
 * $Revision: 1.1 $
 * $Date: 1999/10/19 11:03:39 $
 * ------------------------------------------------------------------------*/
	
#include "config.h"
#include "options.h"	
	
#ifdef INTERPRETER
	.file "callfun.S"

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
.globl universal_call_c_x86_linux
universal_call_c_x86_linux:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%esi
	movl 16(%ebp),%edi
	movl 8(%ebp),%ebx
	testl %ebx,%ebx
	jle docall
	
looptop:
	cmpb $105,(%ebx,%edi)	# 'i'
	jne .L6
	pushl (%esi,%ebx,8)
	jmp looptest
.L6:
	cmpb $73,(%ebx,%edi)	# 'I'
	jne .L8
	pushl 4(%esi,%ebx,8)
	pushl (%esi,%ebx,8)
	jmp looptest
.L8:
	cmpb $102,(%ebx,%edi)	# 'f'
	jne .L10
	movl (%esi,%ebx,8),%eax
	pushl %eax
	jmp looptest
.L10:
	cmpb $70,(%ebx,%edi)	# 'F'
	jne looptest
	movl 4(%esi,%ebx,8),%eax
	movl (%esi,%ebx,8),%edx
	pushl %eax
	pushl %edx
looptest:
	decl %ebx
        testl %ebx,%ebx
	jg looptop

docall:	
	call *20(%ebp)
	
	cmpb $102,(%edi)	# 'f'
	je float32
	cmpb $70,(%edi)		# 'F'
	je float64
iorI:
	movl %eax,0(%esi)
	movl %edx,4(%esi)
	jmp bye
float32:
	fstps 0(%esi)
	jmp bye
float64:
	fstpl 0(%esi)
	jmp bye	
bye:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
#endif /* i386_TARGET_ARCH */
	
#endif /* INTERPRETER */