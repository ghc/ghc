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
#include "RtsFlags.h"

/* Heavily arch-specific, I'm afraid.. */
#if defined(i386_TARGET_ARCH)
void*
createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr)
{
  void *adjustor;
  unsigned char* adj_code;
  size_t sizeof_adjustor;

  if (cconv == 0) { /* the adjustor will be _stdcall'ed */

    /* Magic constant computed by inspecting the code length of
       the following assembly language snippet
       (offset and machine code prefixed):

     <0>:	58	          popl   %eax              # temp. remove ret addr..
     <1>:	68 fd fc fe fa    pushl  0xfafefcfd  	   # constant is large enough to
        			   	           	   # hold a StgStablePtr
     <6>:	50	          pushl  %eax		   # put back ret. addr
     <7>:	b8 fa ef ff 00	  movl   $0x00ffeffa, %eax # load up wptr
     <c>: 	ff e0             jmp    %eax        	   # and jump to it.
		# the callee cleans up the stack
    */
    sizeof_adjustor = 14*sizeof(char);

    if ((adjustor = stgMallocBytes(sizeof_adjustor,"createAdjustor")) == NULL) {
        return NULL;
    }

    adj_code       = (unsigned char*)adjustor;
    adj_code[0x00] = (unsigned char)0x58;  /* popl %eax  */

    adj_code[0x01] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
    *((StgStablePtr*)(adj_code + 0x02)) = (StgStablePtr)hptr;

    adj_code[0x06] = (unsigned char)0x50; /* pushl %eax */

    adj_code[0x07] = (unsigned char)0xb8; /* movl  $wptr, %eax */
    *((StgFunPtr*)(adj_code + 0x08)) = (StgFunPtr)wptr;

    adj_code[0x0c] = (unsigned char)0xff; /* jmp %eax */
    adj_code[0x0d] = (unsigned char)0xe0;


  } else { /* the adjustor will be _ccall'ed */

  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed):

  <00>: 68 ef be ad de    pushl  $0xdeadbeef  	   # constant is large enough to
        			   	           # hold a StgStablePtr
  <05>:	b8 fa ef ff 00	  movl   $0x00ffeffa, %eax # load up wptr
  <0a>: ff d0             call   %eax        	   # and call it.
  <0c>:	83 c4 04	  addl   $0x4,%esp	   # remove stable pointer.
  <0f>:	c3	          ret			   # return to where you came from.

    The ccall'ing version is a tad different, passing in the return
    address of the caller to the auto-generated C stub (which enters
    via the stable pointer.) (The auto-generated C stub is in on this
    game, don't worry :-)

    The adjustor makes the assumption that any return value
    coming back from the C stub is not stored on the stack.
    That's (thankfully) the case here with the restricted set of 
    return types that we support.
  */
    sizeof_adjustor = 16*sizeof(char);

    if ((adjustor = stgMallocBytes(sizeof_adjustor,"createAdjustor")) == NULL) {
        return NULL;
    }

    adj_code       = (unsigned char*)adjustor;

    adj_code[0x00] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
    *((StgStablePtr*)(adj_code+0x01)) = (StgStablePtr)hptr;

    adj_code[0x05] = (unsigned char)0xb8;  /* movl  $wptr, %eax */
    *((StgFunPtr*)(adj_code + 0x06)) = (StgFunPtr)wptr;
    
    adj_code[0x0a] = (unsigned char)0xff; /* call %eax */
    adj_code[0x0b] = (unsigned char)0xd0; 
    
    adj_code[0x0c] = (unsigned char)0x83; /* addl $0x4, %esp */
    adj_code[0x0d] = (unsigned char)0xc4; 
    adj_code[0x0e] = (unsigned char)0x04; 

    adj_code[0x0f] = (unsigned char)0xc3; /* ret */

  }

  /* Have fun! */
  return ((void*)adjustor);
}

void
freeHaskellFunctionPtr(void* ptr)
{
 if ( *(unsigned char*)ptr != 0x68 &&
      *(unsigned char*)ptr != 0x58 ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 if (*(unsigned char*)ptr == 0x68) { /* Aha, a ccall adjustor! */
    freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x01)));
 } else {
    freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x02)));
 }    
 *((unsigned char*)ptr) = '\0';

 free(ptr);
}

#endif /* i386_TARGET_ARCH */

