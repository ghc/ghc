/* -----------------------------------------------------------------------------
 * i386 architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"
#include "Adjustor.h"

#if defined(_WIN32)
#include <windows.h>
#endif

extern void adjustorCode(void);

/* !!! !!! WARNING: !!! !!!
 * This structure is accessed from AdjustorAsm.s
 * Any changes here have to be mirrored in the offsets there.
 */

typedef struct AdjustorStub {
    unsigned char   call[8];
    StgStablePtr    hptr;
    StgFunPtr       wptr;
    StgInt          frame_size;
    StgInt          argument_size;
} AdjustorStub;

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString STG_UNUSED
    )
{
    void *adjustor = NULL;
    void *code = NULL;

    switch (cconv)
    {
    case 0: /* _stdcall */
#if !defined(darwin_HOST_OS)
    /* Magic constant computed by inspecting the code length of
       the following assembly language snippet
       (offset and machine code prefixed):

     <0>:       58                popl   %eax              # temp. remove ret addr..
     <1>:       68 fd fc fe fa    pushl  0xfafefcfd        # constant is large enough to
                                                           # hold a StgStablePtr
     <6>:       50                pushl  %eax              # put back ret. addr
     <7>:       b8 fa ef ff 00    movl   $0x00ffeffa, %eax # load up wptr
     <c>:       ff e0             jmp    %eax              # and jump to it.
                # the callee cleans up the stack
    */

    {
        unsigned char adj_code[14];
        adj_code[0x00] = (unsigned char)0x58;  /* popl %eax  */

        adj_code[0x01] = (unsigned char)0x68;  /* pushl hptr (which is a dword immediate ) */
        *((StgStablePtr*)(adj_code + 0x02)) = (StgStablePtr)hptr;

        adj_code[0x06] = (unsigned char)0x50; /* pushl %eax */

        adj_code[0x07] = (unsigned char)0xb8; /* movl  $wptr, %eax */
        *((StgFunPtr*)(adj_code + 0x08)) = (StgFunPtr)wptr;

        adj_code[0x0c] = (unsigned char)0xff; /* jmp %eax */
        adj_code[0x0d] = (unsigned char)0xe0;
        adjustor = allocateExec(14, &adj_code);
    }
#endif /* !defined(darwin_HOST_OS) */

    case 1: /* _ccall */
    {
        /*
          Most of the trickiness here is due to the need to keep the
          stack pointer 16-byte aligned (see #5250).  That means we
          can't just push another argument on the stack and call the
          wrapper, we may have to shuffle the whole argument block.

          We offload most of the work to AdjustorAsm.S.
        */
        AdjustorStub *adjustorStub = allocateExec(sizeof(AdjustorStub),&code);
        adjustor = adjustorStub;

        int sz = totalArgumentSize(typeString);

        adjustorStub->call[0] = 0xe8;
        *(long*)&adjustorStub->call[1] = ((char*)&adjustorCode) - ((char*)code + 5);
        adjustorStub->hptr = hptr;
        adjustorStub->wptr = wptr;

            // The adjustor puts the following things on the stack:
            // 1.) %ebp link
            // 2.) padding and (a copy of) the arguments
            // 3.) a dummy argument
            // 4.) hptr
            // 5.) return address (for returning to the adjustor)
            // All these have to add up to a multiple of 16.

            // first, include everything in frame_size
        adjustorStub->frame_size = sz * 4 + 16;
            // align to 16 bytes
        adjustorStub->frame_size = (adjustorStub->frame_size + 15) & ~15;
            // only count 2.) and 3.) as part of frame_size
        adjustorStub->frame_size -= 12;
        adjustorStub->argument_size = sz;
    }

    default:
        barf("createAdjustor: Unsupported calling convention");
    }

    return code;
}

void
freeHaskellFunctionPtr(void* ptr)
{
    if ( *(unsigned char*)ptr != 0xe8 &&
         *(unsigned char*)ptr != 0x58 ) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }
    if (*(unsigned char*)ptr == 0xe8) { /* Aha, a ccall adjustor! */
        freeStablePtr(((AdjustorStub*)ptr)->hptr);
    } else {
        freeStablePtr(*((StgStablePtr*)((unsigned char*)ptr + 0x02)));
    }

    // Can't write to this memory, it is only executable:
    // *((unsigned char*)ptr) = '\0';

    freeExec(ptr);
}
