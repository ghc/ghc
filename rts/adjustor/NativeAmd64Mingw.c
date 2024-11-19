/* -----------------------------------------------------------------------------
 * AMD64/Windows architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"
#include "Adjustor.h"
#include "AdjustorPool.h"

#define DECLARE_ADJUSTOR_TEMPLATE(NAME) \
    extern uint8_t NAME ## _adjustor; \
    extern uint8_t NAME ## _adjustor_context; \
    extern uint8_t NAME ## _adjustor_end; \
    const struct AdjustorTemplate NAME ## _adjustor_template = { \
        .code_start = (uint8_t *) &NAME ## _adjustor, \
        .code_end = (uint8_t *) &NAME ## _adjustor_end, \
        .context_ptr = (const struct AdjustorContext **) &NAME ## _adjustor_context, \
    };

/* adjustors to handle calls with less than 4 integer arguments */
DECLARE_ADJUSTOR_TEMPLATE(simple_ccall);
static struct AdjustorPool *simple_ccall_pool;

/* adjustors to handle calls with 4 or more integer arguments where the fourth
 * argument is a float */
DECLARE_ADJUSTOR_TEMPLATE(complex_float_ccall);
static struct AdjustorPool *complex_float_ccall_pool;

/* adjustors to handle calls with 4 or more integer arguments where the fourth
 * argument is not a float */
DECLARE_ADJUSTOR_TEMPLATE(complex_nofloat_ccall);
static struct AdjustorPool *complex_nofloat_ccall_pool;

void initAdjustors(void)
{
    simple_ccall_pool = new_adjustor_pool_from_template(&simple_ccall_adjustor_template);
    complex_float_ccall_pool = new_adjustor_pool_from_template(&complex_float_ccall_adjustor_template);
    complex_nofloat_ccall_pool = new_adjustor_pool_from_template(&complex_nofloat_ccall_adjustor_template);
}

void*
createAdjustor(StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString
    )
{
    struct AdjustorContext context = {
        .hptr = hptr,
        .wptr = wptr,
    };

    atomic_inc(&n_allocd_adjustors, 1);

    /*
      stack at call:
               argn
               ...
               arg5
               return address
               %rcx,%rdx,%r8,%r9 = arg1..arg4

      if there are <4 integer args, then we can just push the
      StablePtr into %rcx and shuffle the other args up.

      If there are >=4 integer args, then we have to flush one arg
      to the stack, and arrange to adjust the stack ptr on return.
      The stack will be rearranged to this:

             argn
             ...
             arg5
             return address  *** <-- dummy arg in stub fn.
             arg4
             obscure_ccall_ret_code

      This unfortunately means that the type of the stub function
      must have a dummy argument for the original return address
      pointer inserted just after the 4th integer argument.

      See NativeAmd64MingwAsm.S.
    */

    // determine whether we have 4 or more integer arguments,
    // and therefore need to flush one to the stack.
    if ((typeString[0] == '\0') ||
        (typeString[1] == '\0') ||
        (typeString[2] == '\0') ||
        (typeString[3] == '\0'))
    {
        return alloc_adjustor(simple_ccall_pool, &context);
    }
    else
    {
        bool fourthFloating = (typeString[3] == 'f' || typeString[3] == 'd');
        if (fourthFloating) {
            return alloc_adjustor(complex_float_ccall_pool, &context);
        } else {
            return alloc_adjustor(complex_nofloat_ccall_pool, &context);
        }
    }
}

void freeHaskellFunctionPtr(void* ptr)
{
    struct AdjustorContext context;
    free_adjustor(ptr, &context);
    freeStablePtr(context.hptr);
    atomic_dec(&n_allocd_adjustors, 1);
}
