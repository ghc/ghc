/* -----------------------------------------------------------------------------
 * PowerPC architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"
#include "Adjustor.h"

/* Adjustor logic for PowerPC and PowerPC64 */

#if defined(linux_HOST_OS)
#include <string.h>
#endif

// from AdjustorAsm.s
// not declared as a function so that AIX-style
// fundescs can never get in the way.
extern void *adjustorCode;

#if defined(linux_HOST_OS)
__asm__("obscure_ccall_ret_code:\n\t"
        "lwz 1,0(1)\n\t"
        "lwz 0,4(1)\n\t"
        "mtlr 0\n\t"
        "blr");
extern void obscure_ccall_ret_code(void);
#endif /* defined(linux_HOST_OS) */

#if defined(powerpc_HOST_ARCH) && defined(aix_HOST_OS) || defined(powerpc64_HOST_ARCH) && defined(__ELF__) && (!defined(_CALL_ELF) || _CALL_ELF == 1)

/* !!! !!! WARNING: !!! !!!
 * This structure is accessed from AdjustorAsm.s
 * Any changes here have to be mirrored in the offsets there.
 */

typedef struct AdjustorStub {
    /* fundesc-based ABIs */
#define         FUNDESCS
    StgFunPtr       code;
    struct AdjustorStub
    *toc;
    void            *env;
    StgStablePtr    hptr;
    StgFunPtr       wptr;
    StgInt          negative_framesize;
    StgInt          extrawords_plus_one;
} AdjustorStub;

#endif

void initAdjustors(void) { }

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString
    )
{
    switch (cconv)
    {
    case 1: /* _ccall */
#if defined(linux_HOST_OS)

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
                if((t == 'l' || t == 'L') && src_gpr <= 9)
                {
                    if((src_gpr & 1) == 0)
                        src_gpr++;
                    src_locs[i] = -src_gpr;
                    src_gpr += 2;
                }
                else if((t == 'w' || t == 'W') && src_gpr <= 10)
                {
                    src_locs[i] = -(src_gpr++);
                }
                else
                {
                    if(t == 'l' || t == 'L' || t == 'd')
                    {
                        if(src_offset % 8)
                            src_offset += 4;
                    }
                    src_locs[i] = src_offset;
                    src_offset += (t == 'l' || t == 'L' || t == 'd') ? 8 : 4;
                }

                    if((t == 'l' || t == 'L') && dst_gpr <= 9)
                {
                    if((dst_gpr & 1) == 0)
                        dst_gpr++;
                    dst_locs[i] = -dst_gpr;
                    dst_gpr += 2;
                }
                else if((t == 'w' || t == 'W') && dst_gpr <= 10)
                {
                    dst_locs[i] = -(dst_gpr++);
                }
                else
                {
                    if(t == 'l' || t == 'L' || t == 'd')
                    {
                        if(dst_offset % 8)
                            dst_offset += 4;
                    }
                    dst_locs[i] = dst_offset;
                    dst_offset += (t == 'l' || t == 'L' || t == 'd') ? 8 : 4;
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
        ExecPage *page = allocateExecPage();
        if (page == NULL) {
            barf("createAdjustor: failed to allocate executable page\n");
        }
        unsigned *code = adjustor;

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

                    if(typeString[i] == 'l' || typeString[i] == 'L')
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
                    if(typeString[i] == 'l' || typeString[i] == 'L')
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

                if(typeString[i] == 'l' || typeString[i] == 'L')
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

        freezeExecPage(page);

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

#else

#define OP_LO(op,lo)  ((((unsigned)(op)) << 16) | (((unsigned)(lo)) & 0xFFFF))
#define OP_HI(op,hi)  ((((unsigned)(op)) << 16) | (((unsigned)(hi)) >> 16))
    {
        /* The following code applies to all PowerPC and PowerPC64 platforms
           whose stack layout is based on the AIX ABI.

           Besides (obviously) AIX, this includes
            Mac OS 9 and BeOS/PPC and Mac OS X PPC (may they rest in peace),
                which use the 32-bit AIX ABI
            powerpc64-linux,
                which uses the 64-bit AIX ABI.

           The actual stack-frame shuffling is implemented out-of-line
           in the function adjustorCode, in AdjustorAsm.S.
           Here, we set up an AdjustorStub structure, which
           is a function descriptor with a pointer to the AdjustorStub
           struct in the position of the TOC that is loaded
           into register r2.

           One nice thing about this is that there is _no_ code generated at
           runtime on the platforms that have function descriptors.
        */
        AdjustorStub *adjustorStub;
        int sz = 0, extra_sz, total_sz;

#if defined(FUNDESCS)
        adjustorStub = stgMallocBytes(sizeof(AdjustorStub), "createAdjustor");
#else
        ExecPage *page = allocateExecPage();
        if (page == NULL) {
            barf("createAdjustor: failed to allocate executable page\n");
        }
        adjustorStub = (AdjustorStub *) page;
#endif /* defined(FUNDESCS) */
        adjustor = adjustorStub;

        adjustorStub->code = (void*) &adjustorCode;

#if defined(FUNDESCS)
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

        freezeExecPage(page);
#else
        barf("adjustor creation not supported on this platform");
#endif /* defined(powerpc_HOST_ARCH) */

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
#endif /* defined(FUNDESCS) */

            // Calculate the size of the stack frame, in words.
        sz = totalArgumentSize(typeString);

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

        return code;
    }

    default:
        barf("createAdjustor: Unsupported calling convention");
    }
}

void
freeHaskellFunctionPtr(void* ptr)
{
#if defined(linux_HOST_OS)
    if ( *(StgWord*)ptr != 0x48000008 ) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }
    freeStablePtr(((StgStablePtr*)ptr)[1]);
#else
    if ( ((AdjustorStub*)ptr)->code != (StgFunPtr) &adjustorCode ) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }
    freeStablePtr(((AdjustorStub*)ptr)->hptr);
#endif

    freeExecPage(ptr);
}
