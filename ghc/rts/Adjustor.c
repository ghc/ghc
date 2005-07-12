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
#include "RtsExternal.h"
#include "RtsUtils.h"
#include <stdlib.h>

#if defined(_WIN32)
#include <windows.h>
#endif

#if defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS)
#include <string.h>
#endif

#ifdef LEADING_UNDERSCORE
#define UNDERSCORE "_"
#else 
#define UNDERSCORE ""
#endif
#if defined(i386_HOST_ARCH)
/* 
  Now here's something obscure for you:

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
*/
static void  GNUC3_ATTRIBUTE(used) obscure_ccall_wrapper(void)
{
  __asm__ (
     ".globl " UNDERSCORE "obscure_ccall_ret_code\n"
     UNDERSCORE "obscure_ccall_ret_code:\n\t"
     "addl $0x4, %esp\n\t"
     "ret"
   );
}
extern void obscure_ccall_ret_code(void);

#if defined(openbsd_HOST_OS)
static unsigned char *obscure_ccall_ret_code_dyn;
#endif

#endif

#if defined(x86_64_HOST_ARCH)
static void GNUC3_ATTRIBUTE(used) obscure_ccall_wrapper(void)
{
  __asm__ (
   ".globl " UNDERSCORE "obscure_ccall_ret_code\n"
   UNDERSCORE "obscure_ccall_ret_code:\n\t"
   "addq $0x8, %rsp\n\t"
   "ret"
  );
}
extern void obscure_ccall_ret_code(void);
#endif

#if defined(alpha_HOST_ARCH)
/* To get the definition of PAL_imb: */
# if defined(linux_HOST_OS)
#  include <asm/pal.h>
# else
#  include <machine/pal.h>
# endif
#endif

#if defined(ia64_HOST_ARCH)
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
  return(&(arr->payload));
}
#endif

#if defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS)
__asm__("obscure_ccall_ret_code:\n\t"
        "lwz 1,0(1)\n\t"
        "lwz 0,4(1)\n\t"
        "mtlr 0\n\t"
        "blr");
extern void obscure_ccall_ret_code(void);
#endif

#if defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH)
#if !(defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS))

/* !!! !!! WARNING: !!! !!!
 * This structure is accessed from AdjustorAsm.s
 * Any changes here have to be mirrored in the offsets there.
 */

typedef struct AdjustorStub {
#if defined(powerpc_HOST_ARCH) && defined(darwin_HOST_OS)
    unsigned        lis;
    unsigned        ori;
    unsigned        lwz;
    unsigned        mtctr;
    unsigned        bctr;
    StgFunPtr       code;
#elif defined(powerpc64_HOST_ARCH) && defined(darwin_HOST_OS)
        /* powerpc64-darwin: just guessing that it won't use fundescs. */
    unsigned        lis;
    unsigned        ori;
    unsigned        rldimi;
    unsigned        oris;
    unsigned        ori2;
    unsigned        lwz;
    unsigned        mtctr;
    unsigned        bctr;
    StgFunPtr       code;
#else
        /* fundesc-based ABIs */
#define         FUNDESCS
    StgFunPtr       code;
    struct AdjustorStub
                    *toc;
    void            *env;
#endif
    StgStablePtr    hptr;
    StgFunPtr       wptr;
    StgInt          negative_framesize;
    StgInt          extrawords_plus_one;
} AdjustorStub;

#endif
#endif

void*
createAdjustor(int cconv, StgStablePtr hptr,
	       StgFunPtr wptr,
	       char *typeString
#if !defined(powerpc_HOST_ARCH) && !defined(powerpc64_HOST_ARCH) && !defined(x86_64_HOST_ARCH)
	          STG_UNUSED
#endif
              )
{
  void *adjustor = NULL;

  switch (cconv)
  {
  case 0: /* _stdcall */
#if defined(i386_HOST_ARCH)
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
    adjustor = stgMallocBytesRWX(14);
    {
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
#if defined(i386_HOST_ARCH)
  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed):

  <00>: 68 ef be ad de     pushl  $0xdeadbeef  	   # constant is large enough to
        			   	           # hold a StgStablePtr
  <05>:	b8 fa ef ff 00	   movl   $0x00ffeffa, %eax # load up wptr
  <0a>: 68 ef be ad de     pushl  $obscure_ccall_ret_code # push the return address
  <0f>: ff e0              jmp    *%eax            # jump to wptr

    The ccall'ing version is a tad different, passing in the return
    address of the caller to the auto-generated C stub (which enters
    via the stable pointer.) (The auto-generated C stub is in on this
    game, don't worry :-)

    See the comment next to obscure_ccall_ret_code why we need to
    perform a tail jump instead of a call, followed by some C stack
    fixup.

    Note: The adjustor makes the assumption that any return value
    coming back from the C stub is not stored on the stack.
    That's (thankfully) the case here with the restricted set of 
    return types that we support.
  */
    adjustor = stgMallocBytesRWX(17);
    {
	unsigned char *const adj_code = (unsigned char *)adjustor;

	adj_code[0x00] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
	*((StgStablePtr*)(adj_code+0x01)) = (StgStablePtr)hptr;

	adj_code[0x05] = (unsigned char)0xb8;  /* movl  $wptr, %eax */
	*((StgFunPtr*)(adj_code + 0x06)) = (StgFunPtr)wptr;

	adj_code[0x0a] = (unsigned char)0x68;  /* pushl obscure_ccall_ret_code */
	*((StgFunPtr*)(adj_code + 0x0b)) = 
#if !defined(openbsd_HOST_OS)
			(StgFunPtr)obscure_ccall_ret_code;
#else
			(StgFunPtr)obscure_ccall_ret_code_dyn;
#endif

	adj_code[0x0f] = (unsigned char)0xff; /* jmp *%eax */
	adj_code[0x10] = (unsigned char)0xe0; 
    }
#elif defined(x86_64_HOST_ARCH)
    /*
      stack at call:
               argn
	       ...
	       arg7
               return address
	       %rdi,%rsi,%rdx,%rcx,%r8,%r9 = arg0..arg6

      if there are <6 integer args, then we can just push the
      StablePtr into %edi and shuffle the other args up.

      If there are >=6 integer args, then we have to flush one arg
      to the stack, and arrange to adjust the stack ptr on return.
      The stack will be rearranged to this:

             argn
	     ...
	     arg7
	     return address  *** <-- dummy arg in stub fn.
	     arg6
	     obscure_ccall_ret_code

      This unfortunately means that the type of the stub function
      must have a dummy argument for the original return address
      pointer inserted just after the 6th integer argument.

      Code for the simple case:

   0:   4d 89 c1                mov    %r8,%r9
   3:   49 89 c8                mov    %rcx,%r8
   6:   48 89 d1                mov    %rdx,%rcx
   9:   48 89 f2                mov    %rsi,%rdx
   c:   48 89 fe                mov    %rdi,%rsi
   f:   48 8b 3d 0a 00 00 00    mov    10(%rip),%rdi
  16:   e9 00 00 00 00          jmpq   stub_function
  ... 
  20: .quad 0  # aligned on 8-byte boundary


  And the version for >=6 integer arguments:

   0:   41 51                   push   %r9
   2:   68 00 00 00 00          pushq  $obscure_ccall_ret_code
   7:   4d 89 c1                mov    %r8,%r9
   a:   49 89 c8                mov    %rcx,%r8
   d:   48 89 d1                mov    %rdx,%rcx
  10:   48 89 f2                mov    %rsi,%rdx
  13:   48 89 fe                mov    %rdi,%rsi
  16:   48 8b 3d 0b 00 00 00    mov    11(%rip),%rdi
  1d:   e9 00 00 00 00          jmpq   stub_function
  ...
  28: .quad 0  # aligned on 8-byte boundary
    */

    /* we assume the small code model (gcc -mcmmodel=small) where
     * all symbols are <2^32, so hence wptr should fit into 32 bits.
     */
    ASSERT(((long)wptr >> 32) == 0);

    {  
	int i = 0;
	char *c;

	// determine whether we have 6 or more integer arguments,
	// and therefore need to flush one to the stack.
	for (c = typeString; *c != '\0'; c++) {
	    if (*c == 'i' || *c == 'l') i++;
	    if (i == 6) break;
	}

	if (i < 6) {
	    adjustor = stgMallocBytesRWX(40);

	    *(StgInt32 *)adjustor      = 0x49c1894d;
	    *(StgInt32 *)(adjustor+4)  = 0x8948c889;
	    *(StgInt32 *)(adjustor+8)  = 0xf28948d1;
	    *(StgInt32 *)(adjustor+12) = 0x48fe8948;
	    *(StgInt32 *)(adjustor+16) = 0x000a3d8b;
	    *(StgInt32 *)(adjustor+20) = 0x00e90000;
	    
	    *(StgInt32 *)(adjustor+23) = 
		(StgInt32)((StgInt64)wptr - (StgInt64)adjustor - 27);
	    *(StgInt64 *)(adjustor+32) = (StgInt64)hptr;
	}
	else
	{
	    adjustor = stgMallocBytesRWX(48);

	    *(StgInt32 *)adjustor      = 0x00685141;
	    *(StgInt32 *)(adjustor+4)  = 0x4d000000;
	    *(StgInt32 *)(adjustor+8)  = 0x8949c189;
	    *(StgInt32 *)(adjustor+12) = 0xd18948c8;
	    *(StgInt32 *)(adjustor+16) = 0x48f28948;
	    *(StgInt32 *)(adjustor+20) = 0x8b48fe89;
	    *(StgInt32 *)(adjustor+24) = 0x00000b3d;
	    *(StgInt32 *)(adjustor+28) = 0x0000e900;
	    
	    *(StgInt32 *)(adjustor+3) = 
		(StgInt32)(StgInt64)obscure_ccall_ret_code;
	    *(StgInt32 *)(adjustor+30) = 
		(StgInt32)((StgInt64)wptr - (StgInt64)adjustor - 34);
	    *(StgInt64 *)(adjustor+40) = (StgInt64)hptr;
	}
    }
#elif defined(sparc_HOST_ARCH)
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
    adjustor = stgMallocBytesRWX(4*(11+1));
    {
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
#elif defined(alpha_HOST_ARCH)
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
     reason described under sparc_HOST_ARCH above by JRS, 21 Aug 01.
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
    adjustor = stgMallocBytesRWX(48);
    {
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
#elif defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS)

#define OP_LO(op,lo)  ((((unsigned)(op)) << 16) | (((unsigned)(lo)) & 0xFFFF))
#define OP_HI(op,hi)  ((((unsigned)(op)) << 16) | (((unsigned)(hi)) >> 16))
    {
        /* The PowerPC Linux (32-bit) calling convention is annoyingly complex.
           We need to calculate all the details of the stack frame layout,
           taking into account the types of all the arguments, and then
           generate code on the fly. */
    
        int src_gpr = 3, dst_gpr = 5;
        int fpr = 3;
        int src_offset = 0, dst_offset = 0;
        int n = strlen(typeString),i;
        int src_locs[n], dst_locs[n];
        int frameSize;
        unsigned *code;
      
            /* Step 1:
               Calculate where the arguments should go.
               src_locs[] will contain the locations of the arguments in the
               original stack frame passed to the adjustor.
               dst_locs[] will contain the locations of the arguments after the
               adjustor runs, on entry to the wrapper proc pointed to by wptr.

               This algorithm is based on the one described on page 3-19 of the
               System V ABI PowerPC Processor Supplement.
            */
        for(i=0;typeString[i];i++)
        {
            char t = typeString[i];
            if((t == 'f' || t == 'd') && fpr <= 8)
                src_locs[i] = dst_locs[i] = -32-(fpr++);
            else
            {
                if(t == 'l' && src_gpr <= 9)
                {
                    if((src_gpr & 1) == 0)
                        src_gpr++;
                    src_locs[i] = -src_gpr;
                    src_gpr += 2;
                }
                else if(t == 'i' && src_gpr <= 10)
                {
                    src_locs[i] = -(src_gpr++);
                }
                else
                {
                    if(t == 'l' || t == 'd')
                    {
                        if(src_offset % 8)
                            src_offset += 4;
                    }
                    src_locs[i] = src_offset;
                    src_offset += (t == 'l' || t == 'd') ? 8 : 4;
                }

                if(t == 'l' && dst_gpr <= 9)
                {
                    if((dst_gpr & 1) == 0)
                        dst_gpr++;
                    dst_locs[i] = -dst_gpr;
                    dst_gpr += 2;
                }
                else if(t == 'i' && dst_gpr <= 10)
                {
                    dst_locs[i] = -(dst_gpr++);
                }
                else
                {
                    if(t == 'l' || t == 'd')
                    {
                        if(dst_offset % 8)
                            dst_offset += 4;
                    }
                    dst_locs[i] = dst_offset;
                    dst_offset += (t == 'l' || t == 'd') ? 8 : 4;
                }
            }
        }

        frameSize = dst_offset + 8;
        frameSize = (frameSize+15) & ~0xF;

            /* Step 2:
               Build the adjustor.
            */
                    // allocate space for at most 4 insns per parameter
                    // plus 14 more instructions.
        adjustor = stgMallocBytesRWX(4 * (4*n + 14));
        code = (unsigned*)adjustor;
        
        *code++ = 0x48000008; // b *+8
            // * Put the hptr in a place where freeHaskellFunctionPtr
            //   can get at it.
        *code++ = (unsigned) hptr;

            // * save the link register
        *code++ = 0x7c0802a6; // mflr r0;
        *code++ = 0x90010004; // stw r0, 4(r1);
            // * and build a new stack frame
        *code++ = OP_LO(0x9421, -frameSize); // stwu r1, -frameSize(r1)

            // * now generate instructions to copy arguments
            //   from the old stack frame into the new stack frame.
        for(i=n-1;i>=0;i--)
        {
            if(src_locs[i] < -32)
                ASSERT(dst_locs[i] == src_locs[i]);
            else if(src_locs[i] < 0)
            {
                // source in GPR.
                ASSERT(typeString[i] != 'f' && typeString[i] != 'd');
                if(dst_locs[i] < 0)
                {
                    ASSERT(dst_locs[i] > -32);
                        // dst is in GPR, too.

                    if(typeString[i] == 'l')
                    {
                            // mr dst+1, src+1
                        *code++ = 0x7c000378
                                | ((-dst_locs[i]+1) << 16)
                                | ((-src_locs[i]+1) << 11)
                                | ((-src_locs[i]+1) << 21);
                    }
                    // mr dst, src
                    *code++ = 0x7c000378
                            | ((-dst_locs[i]) << 16)
                            | ((-src_locs[i]) << 11)
                            | ((-src_locs[i]) << 21);
                }
                else
                {
                    if(typeString[i] == 'l')
                    {
                            // stw src+1, dst_offset+4(r1)
                        *code++ = 0x90010000
                                | ((-src_locs[i]+1) << 21)
                                | (dst_locs[i] + 4);
                    }
                    
                        // stw src, dst_offset(r1)
                    *code++ = 0x90010000
                            | ((-src_locs[i]) << 21)
                            | (dst_locs[i] + 8);
                }
            }
            else
            {
                ASSERT(dst_locs[i] >= 0);
                ASSERT(typeString[i] != 'f' && typeString[i] != 'd');

                if(typeString[i] == 'l')
                {
                    // lwz r0, src_offset(r1)
                        *code++ = 0x80010000
                                | (src_locs[i] + frameSize + 8 + 4);
                    // stw r0, dst_offset(r1)
                        *code++ = 0x90010000
                                | (dst_locs[i] + 8 + 4);
                    }
                // lwz r0, src_offset(r1)
                    *code++ = 0x80010000
                            | (src_locs[i] + frameSize + 8);
                // stw r0, dst_offset(r1)
                    *code++ = 0x90010000
                            | (dst_locs[i] + 8);
           }
        }

            // * hptr will be the new first argument.
            // lis r3, hi(hptr)
        *code++ = OP_HI(0x3c60, hptr);
            // ori r3,r3,lo(hptr)
        *code++ = OP_LO(0x6063, hptr);

            // * we need to return to a piece of code
            //   which will tear down the stack frame.
            // lis r11,hi(obscure_ccall_ret_code)
        *code++ = OP_HI(0x3d60, obscure_ccall_ret_code);
            // ori r11,r11,lo(obscure_ccall_ret_code)
        *code++ = OP_LO(0x616b, obscure_ccall_ret_code);
            // mtlr r11
        *code++ = 0x7d6803a6;

            // * jump to wptr
            // lis r11,hi(wptr)
        *code++ = OP_HI(0x3d60, wptr);
            // ori r11,r11,lo(wptr)
        *code++ = OP_LO(0x616b, wptr);
            // mtctr r11
        *code++ = 0x7d6903a6;
            // bctr
        *code++ = 0x4e800420;

        // Flush the Instruction cache:
        {
            unsigned *p = adjustor;
            while(p < code)
            {
                __asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
                                 : : "r" (p));
                p++;
            }
            __asm__ volatile ("sync\n\tisync");
        }
    }

#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH)
        
#define OP_LO(op,lo)  ((((unsigned)(op)) << 16) | (((unsigned)(lo)) & 0xFFFF))
#define OP_HI(op,hi)  ((((unsigned)(op)) << 16) | (((unsigned)(hi)) >> 16))
    {
        /* The following code applies to all PowerPC and PowerPC64 platforms
           whose stack layout is based on the AIX ABI.

           Besides (obviously) AIX, this includes
            Mac OS 9 and BeOS/PPC (may they rest in peace),
                which use the 32-bit AIX ABI
            powerpc64-linux,
                which uses the 64-bit AIX ABI
            and Darwin (Mac OS X),
                which uses the same stack layout as AIX,
                but no function descriptors.

           The actual stack-frame shuffling is implemented out-of-line
           in the function adjustorCode, in AdjustorAsm.S.
           Here, we set up an AdjustorStub structure, which
           is a function descriptor (on platforms that have function
           descriptors) or a short piece of stub code (on Darwin) to call
           adjustorCode with a pointer to the AdjustorStub struct loaded
           into register r2.

           One nice thing about this is that there is _no_ code generated at
           runtime on the platforms that have function descriptors.
        */
        AdjustorStub *adjustorStub;
        int sz = 0, extra_sz, total_sz;

            // from AdjustorAsm.s
            // not declared as a function so that AIX-style
            // fundescs can never get in the way.
        extern void *adjustorCode;
        
#ifdef FUNDESCS
        adjustorStub = stgMallocBytes(sizeof(AdjustorStub), "createAdjustor");
#else
        adjustorStub = stgMallocBytesRWX(sizeof(AdjustorStub));
#endif
        adjustor = adjustorStub;
            
        adjustorStub->code = (void*) &adjustorCode;

#ifdef FUNDESCS
            // function descriptors are a cool idea.
            // We don't need to generate any code at runtime.
        adjustorStub->toc = adjustorStub;
#else

            // no function descriptors :-(
            // We need to do things "by hand".
#if defined(powerpc_HOST_ARCH)
            // lis  r2, hi(adjustorStub)
        adjustorStub->lis = OP_HI(0x3c40, adjustorStub);
            // ori  r2, r2, lo(adjustorStub)
        adjustorStub->ori = OP_LO(0x6042, adjustorStub);
            // lwz r0, code(r2)
        adjustorStub->lwz = OP_LO(0x8002, (char*)(&adjustorStub->code)
                                        - (char*)adjustorStub);
            // mtctr r0
        adjustorStub->mtctr = 0x7c0903a6;
            // bctr
        adjustorStub->bctr = 0x4e800420;
#else
        barf("adjustor creation not supported on this platform");
#endif

        // Flush the Instruction cache:
        {
            int n = sizeof(AdjustorStub)/sizeof(unsigned);
            unsigned *p = (unsigned*)adjustor;
            while(n--)
            {
                __asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
                                    : : "r" (p));
                p++;
            }
            __asm__ volatile ("sync\n\tisync");
        }
#endif

            // Calculate the size of the stack frame, in words.
        while(*typeString)
        {
            char t = *typeString++;

            switch(t)
            {
#if defined(powerpc_HOST_ARCH)
                    // on 32-bit platforms, Double and Int64 occupy two words.
                case 'd':
                case 'l':
                    sz += 2;
                    break;
#endif
                    // everything else is one word.
                default:
                    sz += 1;
            }
        }
            // The first eight words of the parameter area
            // are just "backing store" for the parameters passed in
            // the GPRs. extra_sz is the number of words beyond those first
            // 8 words.
        extra_sz = sz - 8;
        if(extra_sz < 0)
            extra_sz = 0;

            // Calculate the total size of the stack frame.
        total_sz = (6 /* linkage area */
                  + 8 /* minimum parameter area */
                  + 2 /* two extra arguments */
                  + extra_sz)*sizeof(StgWord);
       
            // align to 16 bytes.
            // AIX only requires 8 bytes, but who cares?
        total_sz = (total_sz+15) & ~0xF;
       
            // Fill in the information that adjustorCode in AdjustorAsm.S
            // will use to create a new stack frame with the additional args.
        adjustorStub->hptr = hptr;
        adjustorStub->wptr = wptr;
        adjustorStub->negative_framesize = -total_sz;
        adjustorStub->extrawords_plus_one = extra_sz + 1;
    }

#elif defined(ia64_HOST_ARCH)
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
#if defined(i386_HOST_ARCH)
 if ( *(unsigned char*)ptr != 0x68 &&
      *(unsigned char*)ptr != 0x58 ) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 if (*(unsigned char*)ptr == 0x68) { /* Aha, a ccall adjustor! */
    freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x01)));
 } else {
    freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x02)));
 }
#elif defined(x86_64_HOST_ARCH)
 if ( *(StgWord16 *)ptr == 0x894d ) {
     freeStablePtr(*(StgStablePtr*)(ptr+32));
 } else if ( *(StgWord16 *)ptr == 0x5141 ) {
     freeStablePtr(*(StgStablePtr*)(ptr+40));
 } else {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }
#elif defined(sparc_HOST_ARCH)
 if ( *(unsigned long*)ptr != 0x9C23A008UL ) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned long*)ptr + 11)));
#elif defined(alpha_HOST_ARCH)
 if ( *(StgWord64*)ptr != 0xa77b0018a61b0010L ) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }

 /* Free the stable pointer first..*/
 freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x10)));
#elif defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS)
 if ( *(StgWord*)ptr != 0x48000008 ) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }
 freeStablePtr(((StgStablePtr*)ptr)[1]);
#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH)
 extern void* adjustorCode;
 if ( ((AdjustorStub*)ptr)->code != (StgFunPtr) &adjustorCode ) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
   return;
 }
 freeStablePtr(((AdjustorStub*)ptr)->hptr);
#elif defined(ia64_HOST_ARCH)
 IA64FunDesc *fdesc = (IA64FunDesc *)ptr;
 StgWord64 *code = (StgWord64 *)(fdesc+1);

 if (fdesc->ip != (StgWord64)code) {
   errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
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


/*
 * Function: initAdjustor()
 *
 * Perform initialisation of adjustor thunk layer (if needed.)
 */
void
initAdjustor(void)
{
#if defined(i386_HOST_ARCH) && defined(openbsd_HOST_OS)
    obscure_ccall_ret_code_dyn = stgMallocBytesRWX(4);
    obscure_ccall_ret_code_dyn[0] = ((unsigned char *)obscure_ccall_ret_code)[0];
    obscure_ccall_ret_code_dyn[1] = ((unsigned char *)obscure_ccall_ret_code)[1];
    obscure_ccall_ret_code_dyn[2] = ((unsigned char *)obscure_ccall_ret_code)[2];
    obscure_ccall_ret_code_dyn[3] = ((unsigned char *)obscure_ccall_ret_code)[3];
#endif
}
