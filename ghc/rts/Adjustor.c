/* -----------------------------------------------------------------------------
 * Foreign export adjustor thunks
 *
 * Copyright (c) 1998.
 *
 * ---------------------------------------------------------------------------*/

/* A little bit of background...

An adjustor thunk is a dynamically allocated code snippet that allows
Haskell closures to be viewed as C function pointers. 

Stable pointers provide a way for the outside world to get access to,
and evaluate, Haskell heap objects, with the RTS providing a small
range of ops for doing so. So, assuming we've got a stable pointer in
our hand in C, we can jump into the Haskell world and evaluate a callback
procedure, say. This works OK in some cases where callbacks are used, but
does require the external code to know about stable pointers and how to deal
with them. We'd like to hide the Haskell-nature of a callback and have it
be invoked just like any other C function pointer. 

Enter adjustor thunks. An adjustor thunk is a little piece of code
that's generated on-the-fly (one per Haskell closure being exported)
that, when entered using some 'universal' calling convention (e.g., the
C calling convention on platform X), pushes an implicit stable pointer
(to the Haskell callback) before calling another (static) C function stub
which takes care of entering the Haskell code via its stable pointer.

An adjustor thunk is allocated on the C heap, and is called from within
Haskell just before handing out the function pointer to the Haskell (IO)
action. User code should never have to invoke it explicitly.

An adjustor thunk differs from a C function pointer in one respect: when
the code is through with it, it has to be freed in order to release Haskell
and C resources. Failure to do so result in memory leaks on both the C and
Haskell side.

*/
#include "Rts.h"
#include "RtsUtils.h"

/* Heavily arch-specific, I'm afraid.. */
#if defined(i386_TARGET_ARCH)
char*
createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr)
{
  void *adjustor,*adj;
  unsigned char* adj_code;
  int i;
  size_t sizeof_adjustor;

  if (cconv == 0) { /* the adjustor will be _stdcall'ed */

    /* Magic constant computed by inspecting the code length of
       the following assembly language snippet
       (offset and machine code prefixed):

     <0>:	58	          popl   %eax              # temp. remove ret addr..
     <1>:	68 63 fd fc fe fa pushl  0xfafefcfd  	   # constant is large enough to
        			   	           	   # hold a StgStablePtr
     <6>:	50	          pushl  %eax		   # put back ret. addr
     <7>:	b8 fa ef ff 00	  movl   $0x00ffeffa, %eax # load up wptr
     <c>: 	ff e0             jmp    %eax        	   # and jump to it.
		# the callee cleans up the it will then clean up the stack
    */
    sizeof_adjustor = 15*sizeof(char);

    if ((adjustor = stgMallocBytes(sizeof_adjustor,"createAdjustor")) == NULL) {
        return NULL;
    }

    adj_code    = (unsigned char*)adjustor;
    adj_code[0] = (unsigned char)0x58;  /* popl %eax  */
    adj_code[1] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */

    adj = (StgStablePtr*)(adj_code+2);
    *((StgStablePtr*)adj) = (StgStablePtr)hptr;

    i = 2 + sizeof(StgStablePtr);
    adj_code[i]   = (unsigned char)0x50; /* pushl %eax */
    adj_code[i+1] = (unsigned char)0xb8; /* movl  $wptr, %eax */
    adj = (char*)(adj_code+i+2);
    *((StgFunPtr*)adj) = (StgFunPtr)wptr;

    i = i+2+sizeof(StgFunPtr);
    adj_code[i]   = (unsigned char)0xff;  /* jmp %eax */
    adj_code[i+1] = (unsigned char)0xe0;

  } else { /* the adjustor will be _ccall'ed */

  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed):

   <0>:	58	          popl   %eax              # temp. remove ret addr..
   <1>:	68 63 fd fc fe fa pushl  0xfafefcfd  	   # constant is large enough to
        			   	           # hold a StgStablePtr
   <6>:	50	          pushl  %eax		   # put back ret. addr
   <7>:	b8 fa ef ff 00	  movl   $0x00ffeffa, %eax # load up wptr
   <c>: ff d0             call   %eax        	   # and call it.
   <e>:	58	          popl   %eax		   # store away return address.
   <f>:	83 c4 04	  addl   $0x4,%esp	   # remove stable pointer
  <12>:	50	          pushl  %eax		   # put back return address.
  <13>:	c3	          ret			   # return to where you came from.

  */
    sizeof_adjustor = 20*sizeof(char);

    if ((adjustor = stgMallocBytes(sizeof_adjustor,"createAdjustor")) == NULL) {
        return NULL;
    }

    adj_code    = (unsigned char*)adjustor;
    adj_code[0] = (unsigned char)0x58;  /* popl %eax  */
    adj_code[1] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */

    adj = (StgStablePtr*)(adj_code+2);
    *((StgStablePtr*)adj) = (StgStablePtr)hptr;

    i = 2 + sizeof(StgStablePtr);
    adj_code[i]   = (unsigned char)0x50; /* pushl %eax */
    adj_code[i+1] = (unsigned char)0xb8; /* movl  $wptr, %eax */
    adj = (char*)(adj_code+i+2);
    *((StgFunPtr*)adj) = (StgFunPtr)wptr;

    i = i+2+sizeof(StgFunPtr);
    adj_code[i]   = (unsigned char)0xff;  /* call %eax */
    adj_code[i+1] = (unsigned char)0xd0;
    adj_code[i+2] = (unsigned char)0x58;  /* popl %eax */
    adj_code[i+3] = (unsigned char)0x83;  /* addl $0x4, %esp */
    adj_code[i+4] = (unsigned char)0xc4;
    adj_code[i+5] = (unsigned char)0x04;
    adj_code[i+6] = (unsigned char)0x50; /* pushl %eax */
    adj_code[i+7] = (unsigned char)0xc3; /* ret */
  }

  /* Have fun! */
  return (adjustor);
}

void
freeHaskellFunctionPtr(void* ptr)
{
 char* tmp;
 
 /* Free the stable pointer first..*/
 tmp=(char*)ptr+2;
 freeStablePointer(*((StgStablePtr*)tmp));

 free(ptr);
}

#endif /* i386_TARGET_ARCH */

