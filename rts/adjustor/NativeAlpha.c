/* -----------------------------------------------------------------------------
 * Alpha architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"

/* To get the definition of PAL_imb: */
#if defined(linux_HOST_OS)
# include <asm/pal.h>
#else
# include <machine/pal.h>
#endif

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString STG_UNUSED
    )
{
    switch (cconv)
    {
    case 1: /* _ccall */
  /* Magic constant computed by inspecting the code length of
     the following assembly language snippet
     (offset and machine code prefixed; note that the machine code
     shown is longwords stored in little-endian order):

  <00>: 46520414        mov     a2, a4
  <04>: 46100412        mov     a0, a2
  <08>: a61b0020        ldq     a0, 0x20(pv)    # load up hptr
  <0c>: 46730415        mov     a3, a5
  <10>: a77b0028        ldq     pv, 0x28(pv)    # load up wptr
  <14>: 46310413        mov     a1, a3
  <18>: 6bfb----        jmp     (pv), <hint>    # jump to wptr (with hint)
  <1c>: 00000000                                # padding for alignment
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
    {
        ASSERT(((StgWord64)wptr & 3) == 0);
        ExecPage *page = allocateExecPage();
        StgWord64 *const code = (StgWord64 *) page;

        code[0] = 0x4610041246520414L;
        code[1] = 0x46730415a61b0020L;
        code[2] = 0x46310413a77b0028L;
        code[3] = 0x000000006bfb0000L
                | (((StgWord32*)(wptr) - (StgWord32*)(code) - 3) & 0x3fff);

        code[4] = (StgWord64)hptr;
        code[5] = (StgWord64)wptr;

        freezeExecPage(page);
        /* Ensure that instruction cache is consistent with our new code */
        __asm__ volatile("call_pal %0" : : "i" (PAL_imb));
        return code;
    }

    default:
        barf("createAdjustor: Unsupported calling convention");
    }
}

void
freeHaskellFunctionPtr(void* ptr)
{
    if ( *(StgWord64*)ptr != 0xa77b0018a61b0010L ) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }

    /* Free the stable pointer first..*/
    freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x10)));

    freeExecPage((ExecPage *) ptr);
}
