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
#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"

/* Heavily arch-specific, I'm afraid.. */
#if defined(i386_TARGET_ARCH) || defined(sparc_TARGET_ARCH) || defined(alpha_TARGET_ARCH)

#if defined(i386_TARGET_ARCH)
/* Now here's something obscure for you:

   When generating an adjustor thunk that uses the C calling
   convention, we have to make sure that the thunk kicks off
   the process of jumping into Haskell with a tail jump. Why?
   Because as a result of jumping in into Haskell we may end
   up freeing the very adjustor thunk we came from using
   freeHaskellFunctionPtr(). Hence, we better not return to
   the adjustor code on our way  out, since it could by then
   point to junk.

   The fix is readily at hand, just include the opcodes
   for the C stack fixup code that we need to perform when
   returning in some static piece of memory and arrange
   to return to it before tail jumping from the adjustor thunk.

   For this to work we make the assumption that bytes in .data
   are considered executable.
*/
static unsigned char __obscure_ccall_ret_code [] = 
  { 0x83, 0xc4, 0x04 /* addl $0x4, %esp */
  , 0xc3             /* ret */
  };
#endif

#if defined(alpha_TARGET_ARCH)
/* To get the definition of PAL_imb: */
#include <machine/pal.h>
#endif

void*
createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr)
{
  void *adjustor = NULL;

  switch (cconv)
  {
  case 0: /* _stdcall */
#if defined(i386_TARGET_ARCH)
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
    if ((adjustor = stgMallocBytes(14, "createAdjustor")) != NULL) {
	unsigned char *const adj_code = (unsigned char *)adjustor;
	adj_code[0x00] = (unsigned char)0x58;  /* popl %eax  */

	adj_code[0x01] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
	*((StgStablePtr*)(adj_code + 0x02)) = (StgStablePtr)hptr;

	adj_code[0x06] = (unsigned char)0x50; /* pushl %eax */

	adj_code[0x07] = (unsigned char)0xb8; /* movl  $wptr, %eax */
	*((StgFunPtr*)(adj_code + 0x08)) = (StgFunPtr)wptr;

	adj_code[0x0c] = (unsigned char)0xff; /* jmp %eax */
	adj_code[0x0d] = (unsigned char)0xe0;
    }
#endif
    break;

  case 1: /* _ccall */
#if defined(i386_TARGET_ARCH)
  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed):

  <00>: 68 ef be ad de     pushl  $0xdeadbeef  	   # constant is large enough to
        			   	           # hold a StgStablePtr
  <05>:	b8 fa ef ff 00	   movl   $0x00ffeffa, %eax # load up wptr
  <0a>: 68 ef be ad de     pushl  $__obscure_ccall_ret_code # push the return address
  <0f>: ff e0              jmp    *%eax            # jump to wptr

    The ccall'ing version is a tad different, passing in the return
    address of the caller to the auto-generated C stub (which enters
    via the stable pointer.) (The auto-generated C stub is in on this
    game, don't worry :-)

    See the comment next to __obscure_ccall_ret_code why we need to
    perform a tail jump instead of a call, followed by some C stack
    fixup.

    Note: The adjustor makes the assumption that any return value
    coming back from the C stub is not stored on the stack.
    That's (thankfully) the case here with the restricted set of 
    return types that we support.
  */
    if ((adjustor = stgMallocBytes(17, "createAdjustor")) != NULL) {
	unsigned char *const adj_code = (unsigned char *)adjustor;

	adj_code[0x00] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
	*((StgStablePtr*)(adj_code+0x01)) = (StgStablePtr)hptr;

	adj_code[0x05] = (unsigned char)0xb8;  /* movl  $wptr, %eax */
	*((StgFunPtr*)(adj_code + 0x06)) = (StgFunPtr)wptr;

	adj_code[0x0a] = (unsigned char)0x68;  /* pushl __obscure_ccall_ret_code */
	*((StgFunPtr*)(adj_code + 0x0b)) = (StgFunPtr)__obscure_ccall_ret_code;

	adj_code[0x0f] = (unsigned char)0xff; /* jmp *%eax */
	adj_code[0x10] = (unsigned char)0xe0; 
    }
#elif defined(sparc_TARGET_ARCH)
  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed):

  <00>: BA 10 00 1B     mov    %i3, %i5
  <04>: B8 10 00 1A     mov    %i2, %i4
  <08>: B6 10 00 19     mov    %i1, %i3
  <0c>: B4 10 00 18     mov    %i0, %i2
  <10>: 13 00 3f fb     sethi  %hi(0x00ffeffa), %o1 # load up wptr (1 of 2)
  <14>: 11 37 ab 6f     sethi  %hi(0xdeadbeef), %o0 # load up hptr (1 of 2)
  <18>: 81 c2 63 fa     jmp    %o1+%lo(0x00ffeffa)  # jump to wptr (load 2 of 2)
  <1c>: 90 12 22 ef     or     %o0, %lo(0xdeadbeef), %o0 # load up hptr (2 of 2)
                                                         # [in delay slot]
  <20>: de ad be ef     # Place the value of the StgStablePtr somewhere readable

    ccall'ing on a SPARC leaves little to be performed by the caller.
    The callee shifts the window on entry and restores it on exit.
    Input paramters and results are passed via registers. (%o0 in the
    code above contains the input paramter to wptr.) The return address
    is stored in %o7/%i7. Since we don't shift the window in this code,
    the return address is preserved and wptr will return to our caller.

    JRS, 21 Aug 01: the above para is a fiction.  The caller passes
    args in %i0 .. %i5 and then the rest at [%sp+92].  We want to
    tailjump to wptr, passing hptr as the new first arg, and a dummy
    second arg, which would be where the return address is on x86.
    That means we have to shuffle the original caller's args along by
    two.

    We do a half-correct solution which works only if the original
    caller passed 4 or fewer arg words.  Move %i0 .. %i3 into %i3
    .. %i6, so we can park hptr in %i0 and a bogus arg in %i1.  The
    fully correct solution would be to subtract 8 from %sp and then
    place %i4 and %i5 at [%sp+92] and [%sp+96] respectively.  This
    machinery should then work in all cases.  (Or would it?  Perhaps
    it would trash parts of the caller's frame.  Dunno).  
  */
    if ((adjustor = stgMallocBytes(4*(8+1), "createAdjustor")) != NULL) {
	unsigned long *const adj_code = (unsigned long *)adjustor;

	/* mov	%o3, %o5 */
	adj_code[0] = (unsigned long)0x9A10000B;
	/* mov	%o2, %o4 */
	adj_code[1] = (unsigned long)0x9810000A;
	/* mov	%o1, %o3 */
	adj_code[2] = (unsigned long)0x96100009;
	/* mov	%o0, %o2 */
	adj_code[3] = (unsigned long)0x94100008;

	/* sethi %hi(wptr), %o1 */
	adj_code[4] = (unsigned long)0x13000000;
	adj_code[4] |= ((unsigned long)wptr) >> 10;

	/* sethi %hi(hptr), %o0 */
	adj_code[5] = (unsigned long)0x11000000;
	adj_code[5] |= ((unsigned long)hptr) >> 10;

	/* jmp %o1+%lo(wptr) */
	adj_code[6] = (unsigned long)0x81c26000;
	adj_code[6] |= ((unsigned long)wptr) & 0x000003ff;

	/* or %o0, %lo(hptr), %o0 */
	adj_code[7] = (unsigned long)0x90122000;
	adj_code[7] |= ((unsigned long)hptr) & 0x000003ff;

	adj_code[8] = (StgStablePtr)hptr;
    }
#elif defined(alpha_TARGET_ARCH)
  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed; note that the machine code
     shown is longwords stored in little-endian order):

  <00>: 46520414	mov	a2, a4
  <04>: 46100412	mov	a0, a2
  <08>: a61b0020	ldq     a0, 0x20(pv)	# load up hptr
  <0c>: 46730415	mov	a3, a5
  <10>: a77b0028	ldq     t12, 0x28(pv)	# load up wptr
  <14>: 46310413	mov	a1, a3
  <18>: 6bfb----	jmp     (pv), <hint>	# jump to wptr (with hint)
  <1c>: 00000000				# padding for alignment
  <20>: [8 bytes for hptr quadword]
  <28>: [8 bytes for wptr quadword]

     The "computed" jump at <08> above is really a jump to a fixed
     location.  Accordingly, we place an always-correct hint in the
     jump instruction, namely the address offset from <0c> to wptr,
     divided by 4, taking the lowest 14 bits.

     We only support passing 4 or fewer argument words, for the same
     reason described under sparc_TARGET_ARCH above by JRS, 21 Aug 01.
     On the Alpha the first 6 integer arguments are in a0 through a5,
     and the rest on the stack.  Hence we want to shuffle the original
     caller's arguments by two.

     On the Alpha the calling convention is so complex and dependent
     on the callee's signature -- for example, the stack pointer has
     to be a multiple of 16 -- that it seems impossible to me [ccshan]
     to handle the general case correctly without changing how the
     adjustor is called from C.  For now, our solution of shuffling
     registers only and ignoring the stack only works if the original
     caller passed 4 or fewer argument words.

TODO: Depending on how much allocation overhead stgMallocBytes uses for
      header information (more precisely, if the overhead is no more than
      4 bytes), we should move the first three instructions above down by
      4 bytes (getting rid of the nop), hence saving memory. [ccshan]
  */
    ASSERT(((StgWord64)wptr & 3) == 0);
    if ((adjustor = stgMallocBytes(48, "createAdjustor")) != NULL) {
	StgWord64 *const code = (StgWord64 *)adjustor;

	code[0] = 0x4610041246520414L;
	code[1] = 0x46730415a61b0020L;
	code[2] = 0x46310413a77b0028L;
	code[3] = 0x000000006bfb0000L
		| (((StgWord32*)(wptr) - (StgWord32*)(code) - 3) & 0x3fff);

	code[4] = (StgWord64)hptr;
	code[5] = (StgWord64)wptr;

	/* Ensure that instruction cache is consistent with our new code */
	__asm__ volatile("call_pal %0" : : "i" (PAL_imb));
    }
#else
#error Adjustor creation is not supported on this platform.
#endif
    break;
  
  default:
    ASSERT(0);
    break;
  }

  /* Have fun! */
  return adjustor;
}

#endif

void
freeHaskellFunctionPtr(void* ptr)
{
#if defined(i386_TARGET_ARCH)
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
#elif defined(sparc_TARGET_ARCH)
 if ( *(unsigned char*)ptr != 0x13 ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x10)));
#elif defined(sparc_TARGET_ARCH)
 if ( *(StgWord64*)ptr != 0xa77b0018a61b0010L ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x10)));
#else
 ASSERT(0);
#endif
 *((unsigned char*)ptr) = '\0';

 free(ptr);
}

