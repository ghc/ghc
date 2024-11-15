/* -----------------------------------------------------------------------------
 * i386 architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"
#include "Adjustor.h"
#include "AdjustorPool.h"

#if defined(_WIN32)
#include <windows.h>
#endif

// Defined in Nativei386Asm.S
extern void ccall_adjustor(void);

/***************************************
 * ccall adjustor
 ***************************************/

// Matches constants in Nativei386Asm.S
struct CCallContext {
    StgStablePtr    hptr;
    StgFunPtr       wptr;
    StgInt          frame_size;
    StgInt          argument_size;
};

#define CCALL_CONTEXT_LEN sizeof(struct CCallContext)
#define CCALL_ADJUSTOR_LEN 10

static void mk_ccall_adjustor(uint8_t *code, const void *context, void *user_data STG_UNUSED)
{
    /*
      Most of the trickiness here is due to the need to keep the
      stack pointer 16-byte aligned (see #5250).  That means we
      can't just push another argument on the stack and call the
      wrapper, we may have to shuffle the whole argument block.
    */

    // MOVL context, %eax
    code[0] = 0xb8;
    *(const void **) &code[1] = context;

    // JMP ccall_adjustor
    int32_t jmp_off = (uint8_t *) &ccall_adjustor - &code[10];
    code[5] = 0xe9;
    *(int32_t *) &code[6] = jmp_off;
}

/* adjustors to handle ccalls */
static struct AdjustorPool *ccall_pool;

void initAdjustors(void) {
    ccall_pool = new_adjustor_pool(sizeof(struct CCallContext), CCALL_ADJUSTOR_LEN, mk_ccall_adjustor, NULL);
}

void*
createAdjustor(StgStablePtr hptr, StgFunPtr wptr,
               char *typeString STG_UNUSED)
{
    // The adjustor puts the following things on the stack:
    // 1. %ebp link
    // 2. padding and (a copy of) the arguments
    // 3. a dummy argument
    // 4. hptr
    // 5. return address (for returning to the adjustor)
    // All these have to add up to a multiple of 16.

    int sz = totalArgumentSize(typeString);
        // first, include everything in frame_size
    StgInt frame_size = sz * 4 + 16;
        // align to 16 bytes
    frame_size = (frame_size + 15) & ~15;
        // only count 2.) and 3.) as part of frame_size
    frame_size -= 12;

    struct CCallContext context = {
        .hptr = hptr,
        .wptr = wptr,
        .frame_size = frame_size,
        .argument_size = sz,
    };
    return alloc_adjustor(ccall_pool, &context);
}

void
freeHaskellFunctionPtr(void* ptr)
{
    struct AdjustorContext context;
    free_adjustor(ptr, &context);
    freeStablePtr(context.hptr);
}
