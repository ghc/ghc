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

/***************************************
 * stdcall adjustor
 ***************************************/

#if !defined(darwin_HOST_OS)
#define STDCALL_ADJUSTOR_LEN 0x0c

static void mk_stdcall_adjustor(uint8_t *code, const void *context, void *user_data STG_UNUSED)
{
    /* Magic constant computed by inspecting the code length of
       the following assembly language snippet
       (offset and machine code prefixed):

     <0>:       58                popl   %eax              # temp. remove return addr.
     <1>:       b9 fd fc fe fa    movl   0xfafefcfd, %ecx  # constant is addr. of AdjustorContext
     <6>:       ff 31             pushl  (%ecx)            # push hptr
     <8>:       50                pushl  %eax              # put back return addr.
     <9>:       ff 61 04          jmp    *4(%ecx)          # and jump to wptr
                # the callee cleans up the stack
    */
    code[0x00] = 0x58;  /* popl %eax  */

    code[0x01] = 0xb9;  /* movl context (which is a dword immediate), %ecx */
    *((const void **) &(code[0x02])) = context;

    code[0x06] = 0xff; /* pushl (%ecx) */
    code[0x07] = 0x31;

    code[0x08] = 0x50; /* pushl %eax */

    code[0x09] = 0xff; /* jmp *4(%ecx) */
    code[0x0a] = 0x61;
    code[0x0b] = 0x04;
}

static struct AdjustorPool *stdcall_pool;
#endif

void initAdjustors() {
    ccall_pool = new_adjustor_pool(sizeof(struct CCallContext), CCALL_ADJUSTOR_LEN, mk_ccall_adjustor, NULL);
#if !defined(darwin_HOST_OS)
    stdcall_pool = new_adjustor_pool(sizeof(struct AdjustorContext), STDCALL_ADJUSTOR_LEN, mk_stdcall_adjustor, NULL);
#endif
}

void*
createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr,
               char *typeString STG_UNUSED
    )
{

    switch (cconv)
    {
    case 0: { /* _stdcall */
#if defined(darwin_HOST_OS)
        barf("stdcall is not supported on Darwin")
#else
        struct AdjustorContext context = {
            .hptr = hptr,
            .wptr = wptr,
        };
        return alloc_adjustor(stdcall_pool, &context);
#endif /* !defined(darwin_HOST_OS) */
    }

    case 1: /* _ccall */
    {
            // The adjustor puts the following things on the stack:
            // 1.) %ebp link
            // 2.) padding and (a copy of) the arguments
            // 3.) a dummy argument
            // 4.) hptr
            // 5.) return address (for returning to the adjustor)
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

    default:
        barf("createAdjustor: Unsupported calling convention");
    }
}

void
freeHaskellFunctionPtr(void* ptr)
{
    struct AdjustorContext context;
    free_adjustor(ptr, &context);
    freeStablePtr(context.hptr);
}
