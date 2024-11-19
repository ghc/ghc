/* -----------------------------------------------------------------------------
 * AMD64 architecture adjustor thunk logic.
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

/* adjustors to handle calls with less than 6 integer arguments */
DECLARE_ADJUSTOR_TEMPLATE(simple_ccall);
static struct AdjustorPool *simple_ccall_pool;

/* adjustors to handle calls with 6 or more integer arguments */
DECLARE_ADJUSTOR_TEMPLATE(complex_ccall);
static struct AdjustorPool *complex_ccall_pool;

void initAdjustors(void)
{
    simple_ccall_pool = new_adjustor_pool_from_template(&simple_ccall_adjustor_template);
    complex_ccall_pool = new_adjustor_pool_from_template(&complex_ccall_adjustor_template);
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

    /*
      stack at call:
               argn
               ...
               arg7
               return address
               %rdi,%rsi,%rdx,%rcx,%r8,%r9 = arg1..arg6

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
    */

    int n_int_args = 0;

    // determine whether we have 6 or more integer arguments,
    // and therefore need to flush one to the stack.
    for (char *c = typeString; *c != '\0'; c++) {
        if (*c != 'f' && *c != 'd') {
            n_int_args++;
        }
        if (n_int_args == 6) {
            break;
        }
    }

    atomic_inc(&n_allocd_adjustors, 1);

    if (n_int_args < 6) {
        return alloc_adjustor(simple_ccall_pool, &context);
    } else {
        return alloc_adjustor(complex_ccall_pool, &context);
    }
}

void freeHaskellFunctionPtr(void* ptr)
{
    struct AdjustorContext context;
    free_adjustor(ptr, &context);
    freeStablePtr(context.hptr);
    atomic_dec(&n_allocd_adjustors, 1);
}
