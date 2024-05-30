/* -----------------------------------------------------------------------------
 * IA64 architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"

/* Layout of a function descriptor */
typedef struct _IA64FunDesc {
    StgWord64 ip;
    StgWord64 gp;
} IA64FunDesc;

static void *
stgAllocStable(size_t size_in_bytes, StgStablePtr *stable)
{
    StgArrBytes* arr;
    uint32_t data_size_in_words, total_size_in_words;

    /* round up to a whole number of words */
    data_size_in_words  = ROUNDUP_BYTES_TO_WDS(size_in_bytes);
    total_size_in_words = sizeofW(StgArrBytes) + data_size_in_words;

    /* allocate and fill it in */
    arr = (StgArrBytes *)allocate(total_size_in_words);
    SET_ARR_HDR(arr, &stg_ARR_WORDS_info, CCCS, size_in_bytes);

    /* obtain a stable ptr */
    *stable = getStablePtr((StgPtr)arr);

    /* and return a ptr to the goods inside the array */
    return(&(arr->payload));
}

void initAdjustors(void) { }

void*
createAdjustor(StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString
#if !defined(powerpc_HOST_ARCH) && !defined(powerpc64_HOST_ARCH) && !defined(x86_64_HOST_ARCH)
               STG_UNUSED
#endif
    )
{
    void *adjustor = NULL;
    void *code = NULL;

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
        [MII]       st8.spill [r12]=r38,8               // spill in6 (out4)
                    mov r41=r37                         // out7 = in5 (out3)
                    mov r40=r36;;                       // out6 = in4 (out2)
        [MII]       st8.spill [r12]=r39                 // spill in7 (out5)
                    mov.sptk b6=r17,50
                    mov r38=r34;;                       // out4 = in2 (out0)
        [MII]       mov r39=r35                         // out5 = in3 (out1)
                    mov r37=r33                         // out3 = in1 (loc1)
                    mov r36=r32                         // out2 = in0 (loc0)
        [MLX]       adds r12=-24,r12                    // update sp
                    movl r34=hptr;;                     // out0 = hptr
        [MIB]       mov r33=r16                         // loc1 = ar.pfs
                    mov r32=b0                          // loc0 = retaddr
                    br.call.sptk.many b0=b6;;

        [MII]       adds r12=-16,r12
                    mov b0=r32
                    mov.i ar.pfs=r33
        [MFB]       nop.m 0x0
                    nop.f 0x0
                    br.ret.sptk.many b0;;
*/

/* These macros distribute a long constant into the two words of an MLX bundle */
#define BITS(val,start,count)   (((val) >> (start)) & ((1 << (count))-1))
#define MOVL_LOWORD(val)        (BITS(val,22,18) << 46)
#define MOVL_HIWORD(val)        ( (BITS(val,0,7)    << 36)      \
                                | (BITS(val,7,9)    << 50)      \
                                | (BITS(val,16,5)   << 45)      \
                                | (BITS(val,21,1)   << 44)      \
                                | (BITS(val,40,23))             \
                                | (BITS(val,63,1)    << 59))

        StgStablePtr stable;
        IA64FunDesc *wdesc = (IA64FunDesc *)wptr;
        StgWord64 wcode = wdesc->ip;
        IA64FunDesc *fdesc;
        StgWord64 *code;

        /* we allocate on the Haskell heap since malloc'd memory isn't
         * executable - argh */
        /* Allocated memory is word-aligned (8 bytes) but functions on ia64
         * must be aligned to 16 bytes.  We allocate an extra 8 bytes of
         * wiggle room so that we can put the code on a 16 byte boundary. */
        adjustor = stgAllocStable(sizeof(IA64FunDesc)+18*8+8, &stable);

        fdesc = (IA64FunDesc *)adjustor;
        code = (StgWord64 *)(fdesc + 1);
        /* add 8 bytes to code if needed to align to a 16-byte boundary */
        if ((StgWord64)code & 15) code++;
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

    return code;
}

void
freeHaskellFunctionPtr(void* ptr)
{
    IA64FunDesc *fdesc = (IA64FunDesc *)ptr;
    StgWord64 *code = (StgWord64 *)(fdesc+1);

    if (fdesc->ip != (StgWord64)code) {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }
    freeStablePtr((StgStablePtr)code[16]);
    freeStablePtr((StgStablePtr)code[17]);
}
