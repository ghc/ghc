/* -----------------------------------------------------------------------------
 * SPARC architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString STG_UNUSED
    )
{
    switch (cconv)
    {
    case 1: /* _ccall */
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
     %o0 and %o1 for the additional arguments, namely  hptr and a dummy (used
     for destination addr of jump on SPARC, return address on x86, ...). This
     shouldn't cause any problems for a C-like caller: alloca is implemented
     similarly, and local variables should be accessed via %fp, not %sp. In a
     nutshell: This should work! (Famous last words! :-)
  */
    {
        ExecPage *page = allocateExecPage();
        unsigned long *const adj_code = (unsigned long *) page;

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

        freezeExecPage(page);

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

        return page;
    }

    default:
        barf("createAdjustor: Unsupported calling convention");
    }
}

void
freeHaskellFunctionPtr(void* ptr)
{
    if ( *(unsigned long*)ptr != 0x9C23A008UL ) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }

    /* Free the stable pointer first..*/
    freeStablePtr(*((StgStablePtr*)((unsigned long*)ptr + 11)));

    freeExecPage(ptr);
}
