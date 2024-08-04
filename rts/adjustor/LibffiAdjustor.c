/* -----------------------------------------------------------------------------
 * libffi-based adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"
#include "sm/Storage.h"
#include "Hash.h"
#include "Adjustor.h"

#include "rts/ghc_ffi.h"
#include <stdint.h>
#include <string.h>

// Note that ffi_alloc_prep_closure is a non-standard libffi closure
// API that is only provided by libffi-wasm32, not upstream libffi.
#if !defined(wasm32_HOST_ARCH)

static ffi_status ffi_alloc_prep_closure(ffi_closure **pclosure, ffi_cif *cif,
                                  void (*fun)(ffi_cif *cif, void *ret,
                                              void **args, void *user_data),
                                  void *user_data, void **code) {
  *pclosure = ffi_closure_alloc(sizeof(ffi_closure), code);
  return ffi_prep_closure_loc(*pclosure, cif, fun, user_data, *code);
}

#endif

/* Maps AdjustorExecutable* to AdjustorWritable*. */
static HashTable* allocatedExecs;

void initAdjustors(void) {
    allocatedExecs = allocHashTable();
}

static AdjustorWritable allocate_adjustor(AdjustorExecutable *exec_ret, ffi_cif *cif, void *wptr, void *hptr)
{
    AdjustorWritable writ;

    ffi_status r = ffi_alloc_prep_closure((ffi_closure **) &writ, cif, wptr, hptr, exec_ret);
    if (r != FFI_OK)
        barf("ffi_alloc_prep_closure failed: %d", r);

    if (*exec_ret != NULL) {
        ACQUIRE_SM_LOCK;
        insertHashTable(allocatedExecs, (StgWord)*exec_ret, writ);
        RELEASE_SM_LOCK;
    }

    return writ;
}

static AdjustorWritable exec_to_writable(AdjustorExecutable exec)
{
    AdjustorWritable writ;
    ACQUIRE_SM_LOCK;
    if ((writ = lookupHashTable(allocatedExecs, (StgWord)exec)) == NULL) {
        RELEASE_SM_LOCK;
        barf("exec_to_writable: not found");
    }
    RELEASE_SM_LOCK;
    return writ;
}

static void free_adjustor(AdjustorExecutable exec)
{
    AdjustorWritable writ;
    ffi_closure* cl;
    cl = writ = exec_to_writable(exec);
    ACQUIRE_SM_LOCK;
    removeHashTable(allocatedExecs, (StgWord)exec, writ);
    ffi_closure_free(cl);
    RELEASE_SM_LOCK;
}


/* Note [Freeing libffi adjustors]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * HOW ADJUSTORS/CLOSURES WORK ON LIBFFI:
 * libffi's ffi_closure_alloc() function gives you two pointers to a closure,
 * 1. the writable pointer, and 2. the executable pointer. You write the
 * closure into the writable pointer (and ffi_prep_closure_loc() will do this
 * for you) and you execute it at the executable pointer.
 *
 * THE PROBLEM:
 * The RTS deals only with the executable pointer, but when it comes time to
 * free the closure, libffi wants the writable pointer back that it gave you
 * when you allocated it.
 *
 * On Linux we used to solve this problem by storing the address of the writable
 * mapping into itself, then returning both writable and executable pointers
 * plus 1 machine word for preparing the closure for use by the RTS (see the
 * Linux version of allocateExec() in rts/sm/Storage.c). When we want to recover
 * the writable address, we subtract 1 word from the executable address and
 * fetch. This works because Linux kernel magic gives us two pointers with
 * different addresses that refer to the same memory. Whatever you write into
 * the writable address can be read back at the executable address. This method
 * is very efficient.
 *
 * On iOS this breaks for two reasons: 1. the two pointers do not refer to
 * the same memory (so we can't retrieve anything stored into the writable
 * pointer if we only have the exec pointer), and 2. libffi's
 * ffi_closure_alloc() assumes the pointer it has returned you is a
 * ffi_closure structure and treats it as such: It uses that memory to
 * communicate with ffi_prep_closure_loc(). On Linux by contrast
 * ffi_closure_alloc() is viewed simply as a memory allocation, and only
 * ffi_prep_closure_loc() deals in ffi_closure structures. Each of these
 * differences is enough make the efficient way used on Linux not work on iOS.
 * Instead on iOS we use hash tables to recover the writable address from the
 * executable one. This method is conservative and would almost certainly work
 * on any platform. This is what we now do everywhere.
 */
void
freeHaskellFunctionPtr(void* ptr)
{
    ffi_closure *cl;

    cl = exec_to_writable(ptr);
    freeStablePtr(cl->user_data);
    stgFree(cl->cif->arg_types);
    stgFree(cl->cif);
    free_adjustor(ptr);
}

static ffi_type * char_to_ffi_type(char c)
{
    switch (c) {
    case 'v':  return &ffi_type_void;
    case 'f':  return &ffi_type_float;
    case 'd':  return &ffi_type_double;
    case 'L':  return &ffi_type_sint64;
    case 'l':  return &ffi_type_uint64;
    case 'W':  return &ffi_type_sint32;
    case 'w':  return &ffi_type_uint32;
    case 'S':  return &ffi_type_sint16;
    case 's':  return &ffi_type_uint16;
    case 'B':  return &ffi_type_sint8;
    case 'b':  return &ffi_type_uint8;
    case 'p':  return &ffi_type_pointer;
    default:   barf("char_to_ffi_type: unknown type '%c'", c);
    }
}

void*
createAdjustor (StgStablePtr hptr,
                StgFunPtr wptr,
                char *typeString)
{
    ffi_cif *cif;
    ffi_type **arg_types;
    uint32_t n_args, i;
    ffi_type *result_type;
    ffi_closure *cl;
    int r;
    void *code;

    n_args = strlen(typeString) - 1;
    cif = stgMallocBytes(sizeof(ffi_cif), "createAdjustor");
    arg_types = stgMallocBytes(n_args * sizeof(ffi_type*), "createAdjustor");

    result_type = char_to_ffi_type(typeString[0]);
    for (i=0; i < n_args; i++) {
        arg_types[i] = char_to_ffi_type(typeString[i+1]);
    }

    r = ffi_prep_cif(cif, FFI_DEFAULT_ABI, n_args, result_type, arg_types);
    if (r != FFI_OK) barf("ffi_prep_cif failed: %d", r);

    cl = allocate_adjustor(&code, cif, wptr, hptr);
    if (cl == NULL) {
        barf("createAdjustor: failed to allocate memory");
    }

#if defined(riscv64_HOST_ARCH)
    // Synchronize the memory and instruction cache to prevent illegal
    // instruction exceptions.

    // We expect two instructions for address loading, one for the jump.
    int instrCount = 3;
    // On Linux the parameters of __builtin___clear_cache are currently unused.
    // Add them anyways for future compatibility. (I.e. the parameters couldn't
    // be checked during development.)
    // TODO: Check the upper boundary e.g. with a debugger.
    __builtin___clear_cache((void *)code,
                            (void *)((uint64_t *) code + instrCount));
    // Memory barrier to ensure nothing circumvents the fence.i / cache flush.
    SEQ_CST_FENCE();
#endif

    return (void *)code;
}
