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

#include <stdlib.h>

/* Heavily arch-specific, I'm afraid.. */

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
# if defined(linux_TARGET_OS)
#  include <asm/pal.h>
# else
#  include <machine/pal.h>
# endif
#endif

#if defined(ia64_TARGET_ARCH)
#include "Storage.h"

/* Layout of a function descriptor */
typedef struct _IA64FunDesc {
    StgWord64 ip;
    StgWord64 gp;
} IA64FunDesc;

static void *
stgAllocStable(size_t size_in_bytes, StgStablePtr *stable)
{
  StgArrWords* arr;
  nat data_size_in_words, total_size_in_words;
  
  /* round up to a whole number of words */
  data_size_in_words  = (size_in_bytes + sizeof(W_) + 1) / sizeof(W_);
  total_size_in_words = sizeofW(StgArrWords) + data_size_in_words;
  
  /* allocate and fill it in */
  arr = (StgArrWords *)allocate(total_size_in_words);
  SET_ARR_HDR(arr, &stg_ARR_WORDS_info, CCCS, data_size_in_words);
 
  /* obtain a stable ptr */
  *stable = getStablePtr((StgPtr)arr);

  /* and return a ptr to the goods inside the array */
  return(BYTE_ARR_CTS(arr));
}
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
  /* Magic constant computed by inspecting the code length of the following
     assembly language snippet (offset and machine code prefixed):

     <00>: 9C23A008   sub   %sp, 8, %sp         ! make room for %o4/%o5 in caller's frame
     <04>: DA23A060   st    %o5, [%sp + 96]     ! shift registers by 2 positions
     <08>: D823A05C   st    %o4, [%sp + 92]
     <0C>: 9A10000B   mov   %o3, %o5
     <10>: 9810000A   mov   %o2, %o4
     <14>: 96100009   mov   %o1, %o3
     <18>: 94100008   mov   %o0, %o2
     <1C>: 13000000   sethi %hi(wptr), %o1      ! load up wptr (1 of 2)
     <20>: 11000000   sethi %hi(hptr), %o0      ! load up hptr (1 of 2)
     <24>: 81C26000   jmp   %o1 + %lo(wptr)     ! jump to wptr (load 2 of 2)
     <28>: 90122000   or    %o0, %lo(hptr), %o0 ! load up hptr (2 of 2, delay slot)
     <2C>  00000000                             ! place for getting hptr back easily

     ccall'ing on SPARC is easy, because we are quite lucky to push a
     multiple of 8 bytes (1 word hptr + 1 word dummy arg) in front of the
     existing arguments (note that %sp must stay double-word aligned at
     all times, see ABI spec at http://www.sparc.org/standards/psABI3rd.pdf).
     To do this, we extend the *caller's* stack frame by 2 words and shift
     the output registers used for argument passing (%o0 - %o5, we are a *leaf*
     procedure because of the tail-jump) by 2 positions. This makes room in
     %o0 and %o1 for the additinal arguments, namely  hptr and a dummy (used
     for destination addr of jump on SPARC, return address on x86, ...). This
     shouldn't cause any problems for a C-like caller: alloca is implemented
     similarly, and local variables should be accessed via %fp, not %sp. In a
     nutshell: This should work! (Famous last words! :-)
  */
    if ((adjustor = stgMallocBytes(4*(11+1), "createAdjustor")) != NULL) {
        unsigned long *const adj_code = (unsigned long *)adjustor;

        adj_code[ 0]  = 0x9C23A008UL;   /* sub   %sp, 8, %sp         */
        adj_code[ 1]  = 0xDA23A060UL;   /* st    %o5, [%sp + 96]     */
        adj_code[ 2]  = 0xD823A05CUL;   /* st    %o4, [%sp + 92]     */
        adj_code[ 3]  = 0x9A10000BUL;   /* mov   %o3, %o5            */
        adj_code[ 4]  = 0x9810000AUL;   /* mov   %o2, %o4            */
        adj_code[ 5]  = 0x96100009UL;   /* mov   %o1, %o3            */
        adj_code[ 6]  = 0x94100008UL;   /* mov   %o0, %o2            */
        adj_code[ 7]  = 0x13000000UL;   /* sethi %hi(wptr), %o1      */
        adj_code[ 7] |= ((unsigned long)wptr) >> 10;
        adj_code[ 8]  = 0x11000000UL;   /* sethi %hi(hptr), %o0      */
        adj_code[ 8] |= ((unsigned long)hptr) >> 10;
        adj_code[ 9]  = 0x81C26000UL;   /* jmp   %o1 + %lo(wptr)     */
        adj_code[ 9] |= ((unsigned long)wptr) & 0x000003FFUL;
        adj_code[10]  = 0x90122000UL;   /* or    %o0, %lo(hptr), %o0 */
        adj_code[10] |= ((unsigned long)hptr) & 0x000003FFUL;

        adj_code[11]  = (unsigned long)hptr;

        /* flush cache */
        asm("flush %0" : : "r" (adj_code     ));
        asm("flush %0" : : "r" (adj_code +  2));
        asm("flush %0" : : "r" (adj_code +  4));
        asm("flush %0" : : "r" (adj_code +  6));
        asm("flush %0" : : "r" (adj_code + 10));

        /* max. 5 instructions latency, and we need at >= 1 for returning */
        asm("nop");
        asm("nop");
        asm("nop");
        asm("nop");
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
  <10>: a77b0028	ldq     pv, 0x28(pv)	# load up wptr
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
#elif defined(powerpc_TARGET_ARCH)
/*
	For PowerPC, the following code is used:

	mr r10,r8
	mr r9,r7
	mr r8,r6
	mr r7,r5
	mr r6,r4
	mr r5,r3
	lis r0,0xDEAD ;hi(wptr)
	lis r3,0xDEAF ;hi(hptr)
	ori r0,r0,0xBEEF ; lo(wptr)
	ori r3,r3,0xFACE ; lo(hptr)
	mtctr r0
	bctr

	The arguments (passed in registers r3 - r10) are shuffled along by two to
	make room for hptr and a dummy argument. As r9 and r10 are overwritten by
	this code, it only works for up to 6 arguments (when floating point arguments
	are involved, this may be more or less, depending on the exact situation).
*/
	if ((adjustor = stgMallocBytes(4*13, "createAdjustor")) != NULL) {
		unsigned long *const adj_code = (unsigned long *)adjustor;

		// make room for extra arguments
		adj_code[0] = 0x7d0a4378;	//mr r10,r8
		adj_code[1] = 0x7ce93b78;	//mr r9,r7
		adj_code[2] = 0x7cc83378;	//mr r8,r6
		adj_code[3] = 0x7ca72b78;	//mr r7,r5
		adj_code[4] = 0x7c862378;	//mr r6,r4
		adj_code[5] = 0x7c651b78;	//mr r5,r3
		
		adj_code[6] = 0x3c000000;	//lis r0,hi(wptr)
		adj_code[6] |= ((unsigned long)wptr) >> 16;
		
		adj_code[7] = 0x3c600000;	//lis r3,hi(hptr)
		adj_code[7] |= ((unsigned long)hptr) >> 16;
		
		adj_code[8] = 0x60000000;	//ori r0,r0,lo(wptr)
		adj_code[8] |= ((unsigned long)wptr) & 0xFFFF; 
		
		adj_code[9] = 0x60630000;	//ori r3,r3,lo(hptr)
		adj_code[9] |= ((unsigned long)hptr) & 0xFFFF;
		
		adj_code[10] = 0x7c0903a6;	//mtctr r0
		adj_code[11] = 0x4e800420;	//bctr
		adj_code[12] = (unsigned long)hptr;
		
		// Flush the Instruction cache:
		//	MakeDataExecutable(adjustor,4*13);
			/* This would require us to link with CoreServices.framework */
		{		/* this should do the same: */
			int n = 13;
			unsigned long *p = adj_code;
			while(n--)
			{
				__asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
						    : : "r" (p));
				p++;
			}
			__asm__ volatile ("sync\n\tisync");
		}
	}
#elif defined(ia64_TARGET_ARCH)
/*
    Up to 8 inputs are passed in registers.  We flush the last two inputs to
    the stack, initially into the 16-byte scratch region left by the caller.
    We then shuffle the others along by 4 (taking 2 registers for ourselves
    to save return address and previous function state - we need to come back
    here on the way out to restore the stack, so this is a real function
    rather than just a trampoline).
    
    The function descriptor we create contains the gp of the target function
    so gp is already loaded correctly.

	[MLX]       alloc r16=ar.pfs,10,2,0
		    movl r17=wptr
	[MII]       st8.spill [r12]=r38,8		// spill in6 (out4)
		    mov r41=r37				// out7 = in5 (out3)
		    mov r40=r36;;			// out6 = in4 (out2)
	[MII]       st8.spill [r12]=r39			// spill in7 (out5)
		    mov.sptk b6=r17,50
		    mov r38=r34;;			// out4 = in2 (out0)
	[MII]       mov r39=r35				// out5 = in3 (out1)
		    mov r37=r33				// out3 = in1 (loc1)
		    mov r36=r32				// out2 = in0 (loc0)
	[MLX]       adds r12=-24,r12			// update sp
		    movl r34=hptr;;			// out0 = hptr
	[MIB]       mov r33=r16				// loc1 = ar.pfs
		    mov r32=b0				// loc0 = retaddr
		    br.call.sptk.many b0=b6;;

	[MII]       adds r12=-16,r12
		    mov b0=r32
		    mov.i ar.pfs=r33
	[MFB]       nop.m 0x0
		    nop.f 0x0
		    br.ret.sptk.many b0;;
*/

/* These macros distribute a long constant into the two words of an MLX bundle */
#define BITS(val,start,count)	(((val) >> (start)) & ((1 << (count))-1))
#define MOVL_LOWORD(val)	(BITS(val,22,18) << 46)
#define MOVL_HIWORD(val)	(BITS(val,40,23) | (BITS(val,0,7) << 36) | (BITS(val,7,9) << 50) \
				| (BITS(val,16,5) << 55) | (BITS(val,21,1) << 44) | BITS(val,63,1) << 59)

    {
	StgStablePtr stable;
	IA64FunDesc *wdesc = (IA64FunDesc *)wptr;
	StgWord64 wcode = wdesc->ip;
	IA64FunDesc *fdesc;
	StgWord64 *code;

	/* we allocate on the Haskell heap since malloc'd memory isn't executable - argh */
	adjustor = stgAllocStable(sizeof(IA64FunDesc)+18*8, &stable);

	fdesc = (IA64FunDesc *)adjustor;
	code = (StgWord64 *)(fdesc + 1);
	fdesc->ip = (StgWord64)code;
	fdesc->gp = wdesc->gp;

	code[0]  = 0x0000058004288004 | MOVL_LOWORD(wcode);
	code[1]  = 0x6000000220000000 | MOVL_HIWORD(wcode);
	code[2]  = 0x029015d818984001;
	code[3]  = 0x8401200500420094;
	code[4]  = 0x886011d8189c0001;
	code[5]  = 0x84011004c00380c0;
	code[6]  = 0x0250210046013800;
	code[7]  = 0x8401000480420084;
	code[8]  = 0x0000233f19a06005 | MOVL_LOWORD((StgWord64)hptr);
	code[9]  = 0x6000000440000000 | MOVL_HIWORD((StgWord64)hptr);
	code[10] = 0x0200210020010811;
	code[11] = 0x1080006800006200;
	code[12] = 0x0000210018406000;
	code[13] = 0x00aa021000038005;
	code[14] = 0x000000010000001d;
	code[15] = 0x0084000880000200;

	/* save stable pointers in convenient form */
	code[16] = (StgWord64)hptr;
	code[17] = (StgWord64)stable;
    }
#else
    barf("adjustor creation not supported on this platform");
#endif
    break;
  
  default:
    ASSERT(0);
    break;
  }

  /* Have fun! */
  return adjustor;
}


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
 if ( *(unsigned long*)ptr != 0x9C23A008UL ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned long*)ptr + 11)));
#elif defined(alpha_TARGET_ARCH)
 if ( *(StgWord64*)ptr != 0xa77b0018a61b0010L ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x10)));
#elif defined(powerpc_TARGET_ARCH)
 if ( *(StgWord*)ptr != 0x7d0a4378 ) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }
 freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 4*12)));
#elif defined(ia64_TARGET_ARCH)
 IA64FunDesc *fdesc = (IA64FunDesc *)ptr;
 StgWord64 *code = (StgWord64 *)(fdesc+1);

 if (fdesc->ip != (StgWord64)code) {
   fprintf(stderr, "freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }
 freeStablePtr((StgStablePtr)code[16]);
 freeStablePtr((StgStablePtr)code[17]);
 return;
#else
 ASSERT(0);
#endif
 *((unsigned char*)ptr) = '\0';

 stgFree(ptr);
}

