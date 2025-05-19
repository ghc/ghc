/* -----------------------------------------------------------------------------
 * Bytecode interpreter
 *
 * Copyright (c) The GHC Team, 1994-2002.
 * ---------------------------------------------------------------------------*/

/*
Note [CBV Functions and the interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the byte code interpreter loads a reference to a value it often
ends up as a non-tagged pointers *especially* if we already know a value
is a certain constructor and therefore don't perform an eval on the reference.
This causes friction with CBV functions which assume
their value arguments are properly tagged by the caller.

In order to ensure CBV functions still get passed tagged functions we have
three options:
a)  Special case the interpreter behaviour into the tag inference analysis.
    If we assume the interpreter can't properly tag value references the STG passes
    would then wrap such calls in appropriate evals which are executed at runtime.
    This would ensure tags by doing additional evals at runtime.
b)  When the interpreter pushes references for known constructors instead of
    pushing the objects address add the tag to the value pushed. This is what
    the NCG backends do.
c)  When the interpreter pushes a reference inspect the closure of the object
    and apply the appropriate tag at runtime.

For now we use approach c). Mostly because it's easiest to implement. We also don't
tag functions as tag inference currently doesn't rely on those being properly tagged.


Note [Subwords in the interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To retain our sanity while dealing with little and big endian we strive to:
* Store subwords as zero extended words to the stack in host byte order.
* Read full words from the stack and truncated to actual width after.
This ensures we generally don't have to worry about litte vs big endian.

There are two exceptions:
* When allocating a constructor we use PUSH_UBX8/PUSH8 and their variants to store
  subwords exactly at SP. This allows us to allocate a constructor with packed values
  via memcpy from the stack to the heap after setting up the stack appropriately.
  on the heap.
* When we use PUSH_UBX<width> to push literals to the stack we add instructions
  to add padding such that it will result in a full words in host byte order on the stack.

See also Note [Width of parameters] for some more motivation.
*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "rts/Bytecodes.h"

// internal headers
#include "sm/Storage.h"
#include "sm/Sanity.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Updates.h"
#include "Prelude.h"
#include "StablePtr.h"
#include "Printer.h"
#include "Profiling.h"
#include "Disassembler.h"
#include "Interpreter.h"
#include "ThreadPaused.h"
#include "Threads.h"

#include <string.h>     /* for memcpy */
#if defined(HAVE_ERRNO_H)
#include <errno.h>
#endif

// When building the RTS in the non-dyn way on Windows, we don't
//      want declspec(__dllimport__) on the front of function prototypes
//      from libffi.
#if defined(mingw32_HOST_OS)
# define LIBFFI_NOT_DLL
#endif

#include "rts/ghc_ffi.h"

/* --------------------------------------------------------------------------
 * The bytecode interpreter
 * ------------------------------------------------------------------------*/

/* Gather stats about entry, opcode, opcode-pair frequencies.  For
   tuning the interpreter. */

/* #define INTERP_STATS */


/* Sp points to the lowest live word on the stack. */

#define BCO_NEXT         instrs[bciPtr++]
#define BCO_NEXT_32      (bciPtr += 2)
#define BCO_READ_NEXT_32 (BCO_NEXT_32, (((StgWord) instrs[bciPtr-2]) << 16) \
                                     + ( (StgWord) instrs[bciPtr-1]))
#define BCO_NEXT_64      (bciPtr += 4)
#define BCO_READ_NEXT_64 (BCO_NEXT_64, (((StgWord) instrs[bciPtr-4]) << 48) \
                                     + (((StgWord) instrs[bciPtr-3]) << 32) \
                                     + (((StgWord) instrs[bciPtr-2]) << 16) \
                                     + ( (StgWord) instrs[bciPtr-1]))
#if WORD_SIZE_IN_BITS == 32
#define BCO_NEXT_WORD BCO_NEXT_32
#define BCO_READ_NEXT_WORD BCO_READ_NEXT_32
#elif WORD_SIZE_IN_BITS == 64
#define BCO_NEXT_WORD BCO_NEXT_64
#define BCO_READ_NEXT_WORD BCO_READ_NEXT_64
#else
#error Cannot cope with WORD_SIZE_IN_BITS being nether 32 nor 64
#endif
#define BCO_GET_LARGE_ARG ((bci & bci_FLAG_LARGE_ARGS) ? BCO_READ_NEXT_WORD : BCO_NEXT)

#define BCO_PTR(n)    (W_)ptrs[n]
#define BCO_LIT(n)    literals[n]
#define BCO_LITW64(n) (*(StgWord64*)(literals+n))
#define BCO_LITI64(n) (*(StgInt64*)(literals+n))

#define LOAD_STACK_POINTERS                                     \
    Sp = cap->r.rCurrentTSO->stackobj->sp;                      \
    /* We don't change this ... */                              \
    SpLim = tso_SpLim(cap->r.rCurrentTSO);

#define SAVE_STACK_POINTERS                     \
    cap->r.rCurrentTSO->stackobj->sp = Sp;

#if defined(PROFILING)
#define LOAD_THREAD_STATE()                     \
    LOAD_STACK_POINTERS                         \
    cap->r.rCCCS = cap->r.rCurrentTSO->prof.cccs;
#else
#define LOAD_THREAD_STATE()                     \
    LOAD_STACK_POINTERS
#endif

#if defined(PROFILING)
#define SAVE_THREAD_STATE()                     \
    SAVE_STACK_POINTERS                         \
    cap->r.rCurrentTSO->prof.cccs = cap->r.rCCCS;
#else
#define SAVE_THREAD_STATE()                     \
    SAVE_STACK_POINTERS
#endif

// Note [Not true: ASSERT(Sp > SpLim)]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SpLim has some headroom (RESERVED_STACK_WORDS) to allow for saving
// any necessary state on the stack when returning to the scheduler
// when a stack check fails..  The upshot of this is that Sp could be
// less than SpLim both when leaving to return to the scheduler.

#define RETURN_TO_SCHEDULER(todo,retcode)       \
   SAVE_THREAD_STATE();                         \
   cap->r.rCurrentTSO->what_next = (todo);      \
   threadPaused(cap,cap->r.rCurrentTSO);        \
   cap->r.rRet = (retcode);                     \
   return cap;

// Note [avoiding threadPaused]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Switching between the interpreter to compiled code can happen very
// frequently, so we don't want to call threadPaused(), which is
// expensive.  BUT we must be careful not to violate the invariant
// that threadPaused() has been called on all threads before we GC
// (see Note [upd-black-hole].  So the scheduler must ensure that when
// we return in this way that we definitely immediately run the thread
// again and don't GC or do something else.
//
#define RETURN_TO_SCHEDULER_NO_PAUSE(todo,retcode)      \
   SAVE_THREAD_STATE();                                 \
   cap->r.rCurrentTSO->what_next = (todo);              \
   cap->r.rRet = (retcode);                             \
   return cap;

#define Sp_plusB(n)  ((void *)((StgWord8*)Sp + (ptrdiff_t)(n)))
#define Sp_minusB(n) ((void *)((StgWord8*)Sp - (ptrdiff_t)(n)))

#define Sp_plusW(n)    ((void*)Sp_plusB((ptrdiff_t)(n) * (ptrdiff_t)sizeof(W_)))
#define Sp_plusW64(n)  ((void*)Sp_plusB((ptrdiff_t)(n) * (ptrdiff_t)sizeof(StgWord64)))
#define Sp_minusW(n)   ((void*)Sp_minusB((ptrdiff_t)(n) * (ptrdiff_t)sizeof(W_)))

#define Sp_addB(n)   (Sp = Sp_plusB(n))
#define Sp_subB(n)   (Sp = Sp_minusB(n))
#define Sp_addW(n)   (Sp = Sp_plusW(n))
#define Sp_addW64(n) (Sp = Sp_plusW64(n))
#define Sp_subW(n)   (Sp = Sp_minusW(n))

// Assumes stack location is within stack chunk bounds
#define SpW(n)      (*(StgWord*)(Sp_plusW(n)))
#define SpW64(n)    (*(StgWord*)(Sp_plusW64(n)))

#define WITHIN_CAP_CHUNK_BOUNDS_W(n)  WITHIN_CHUNK_BOUNDS_W(n, cap->r.rCurrentTSO->stackobj)

#define WITHIN_CHUNK_BOUNDS_W(n, s)  \
    (RTS_LIKELY(((StgWord*) Sp_plusW(n)) < ((s)->stack + (s)->stack_size - sizeofW(StgUnderflowFrame))))


#define W64_TO_WDS(n) ((n * sizeof(StgWord64) / sizeof(StgWord)))

// Always safe to use - Return the value at the address
#define ReadSpW(n)       (*((StgWord*)   SafeSpWP(n)))
//Argument is offset in multiples of word64
#define ReadSpW64(n)     (*((StgWord64*) SafeSpWP(W64_TO_WDS(n))))
// Perhaps confusingly this still reads a full word, merely the offset is in bytes.
#define ReadSpB(n)       (*((StgWord*)   SafeSpBP(n)))

/* Note [PUSH_L underflow]
   ~~~~~~~~~~~~~~~~~~~~~~~
BCOs can be nested, resulting in nested BCO stack frames where the inner most
stack frame can refer to variables stored on earlier stack frames via the
PUSH_L instruction.

|---------|
|  BCO_1  | -<-┐
|---------|
 .........     |
|---------|    | PUSH_L <n>
|  BCO_N  | ->-┘
|---------|

Here BCO_N is syntactically nested within the code for BCO_1 and will result
in code that references the prior stack frame of BCO_1 for some of it's local
variables. If a stack overflow happens between the creation of the stack frame
for BCO_1 and BCO_N the RTS might move BCO_N to a new stack chunk while leaving
BCO_1 in place, invalidating a simple offset based reference to the outer stack
frames.
Therefore `SafeSpW` first performs a bounds check to ensure that accesses onto
the stack will succeed. If the target address would not be a valid location for
the current stack chunk then `slow_sp` function is called, which dereferences
the underflow frame to adjust the offset before performing the lookup.

               ┌->--x   |  CHK_1  |
|  CHK_2  |    |    |   |---------|
|---------|    |    └-> |  BCO_1  |
| UD_FLOW | -- x        |---------|
|---------|    |
| ......  |    |
|---------|    | PUSH_L <n>
|  BCO_ N | ->-┘
|---------|

To keep things simpler all accesses to the stack which might go beyond the stack
chunk go through one of the ReadSP* or SafeSP* macros.
When writing to the stack there is no need for checks, we ensured we have space
in the current chunk ahead of time. So there we use SpW and it's variants which
omit the stack bounds check.

See ticket #25750

*/

// Returns a pointer to the stack location.
#define SafeSpWP(n)      \
  ( ((WITHIN_CAP_CHUNK_BOUNDS_W(n)) ? Sp_plusW(n) : slow_spw(Sp, cap->r.rCurrentTSO->stackobj, n)))
#define SafeSpBP(off_w)      \
  ( (WITHIN_CAP_CHUNK_BOUNDS_W((1+(off_w))/sizeof(StgWord))) ? \
        Sp_plusB(off_w) : \
        (void*)((ptrdiff_t)((ptrdiff_t)(off_w) % (ptrdiff_t)sizeof(StgWord)) + (StgWord8*)slow_spw(Sp, cap->r.rCurrentTSO->stackobj, (off_w)/sizeof(StgWord))) \
    )



/* Note [Interpreter subword primops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general the interpreter stack is host-platform word aligned.
We keep with this convention when evaluating primops for simplicity.

This means:

* All arguments are pushed extended to word size.
* Results are written to the stack extended to word size.

The only exception are constructor allocations where we push unaligned subwords
on the stack which are cleaned up by the PACK instruction afterwards.

*/

STATIC_INLINE StgPtr
allocate_NONUPD (Capability *cap, int n_words)
{
    return allocate(cap, stg_max(sizeofW(StgHeader)+MIN_PAYLOAD_SIZE, n_words));
}

int rts_stop_on_exception = 0;

/* ---------------------------------------------------------------------------
 * Enabling and disabling global single step mode
 * ------------------------------------------------------------------------ */

/* A global toggle for single-step mode.
 * Unlike `TSO_STOP_NEXT_BREAKPOINT`, which sets single-step mode per-thread,
 * `rts_stop_next_breakpoint` globally enables single-step mode. If enabled, we
 * will stop at the immediate next breakpoint regardless of what thread it is in. */
int rts_stop_next_breakpoint = 0;

void rts_enableStopNextBreakpointAll(void)
{
  rts_stop_next_breakpoint = 1;
}

void rts_disableStopNextBreakpointAll(void)
{
  rts_stop_next_breakpoint = 0;
}

/* ---------------------------------------------------------------------------
 * Enabling and disabling per-thread single step mode
 * ------------------------------------------------------------------------ */

void rts_enableStopNextBreakpoint(StgPtr tso)
{
    ((StgTSO *)tso)->flags |= TSO_STOP_NEXT_BREAKPOINT;
}

void rts_disableStopNextBreakpoint(StgPtr tso)
{
    ((StgTSO *)tso)->flags &= ~TSO_STOP_NEXT_BREAKPOINT;
}

/* -------------------------------------------------------------------------- */

#if defined(INTERP_STATS)

#define N_CODES 128

/* Hacky stats, for tuning the interpreter ... */
unsigned long it_unknown_entries[N_CLOSURE_TYPES];
unsigned long it_total_unknown_entries;

unsigned long it_total_entries;
unsigned long it_retto_BCO;
unsigned long it_retto_UPDATE;
unsigned long it_retto_other;

unsigned long it_underflow_lookups;

unsigned long it_slides;
unsigned long it_insns;
unsigned long it_BCO_entries;

unsigned long it_ofreq[N_CODES];
unsigned long it_oofreq[N_CODES][N_CODES];
unsigned long it_lastopc;


#define INTERP_TICK(n) (n)++

void interp_startup ( void )
{
   int i, j;
   it_retto_BCO = it_retto_UPDATE = it_retto_other = 0;
   it_total_entries = it_total_unknown_entries = 0;
   it_underflow_lookups = 0;
   for (i = 0; i < N_CLOSURE_TYPES; i++)
      it_unknown_entries[i] = 0;
   it_slides = it_insns = it_BCO_entries = 0;
   for (i = 0; i < N_CODES; i++) it_ofreq[i] = 0;
   for (i = 0; i < N_CODES; i++)
     for (j = 0; j < N_CODES; j++)
        it_oofreq[i][j] = 0;
   it_lastopc = 0;
}

void interp_shutdown ( void )
{
   int i, j, k, i_max, j_max;
   long unsigned o_max;
   unsigned long copy_freq[N_CODES][N_CODES];
   debugBelch("%lu constrs entered -> (%lu BCO, %lu UPD, %lu ??? )\n",
                   it_retto_BCO + it_retto_UPDATE + it_retto_other,
                   it_retto_BCO, it_retto_UPDATE, it_retto_other );
   debugBelch("%lu total entries, %lu unknown entries \n",
                   it_total_entries, it_total_unknown_entries);
   debugBelch("%lu lookups past the end of the stack frame\n", it_underflow_lookups);
   for (i = 0; i < N_CLOSURE_TYPES; i++) {
     if (it_unknown_entries[i] == 0) continue;
     debugBelch("   type %2d: unknown entries (%4.1f%%) == %lu\n",
             i, 100.0 * ((double)it_unknown_entries[i]) /
                        ((double)it_total_unknown_entries),
             it_unknown_entries[i]);
   }
   debugBelch("%lu insns, %lu slides, %lu BCO_entries\n",
                   it_insns, it_slides, it_BCO_entries);
   for (i = 0; i < N_CODES; i++)
      debugBelch("opcode %2d got %lu\n", i, it_ofreq[i] );

   for (i = 0; i < N_CODES; i++)
     for (j = 0; j < N_CODES; j++)
        copy_freq[i][j] = it_oofreq[i][j];

   for (k = 1; k < 20; k++) {
      o_max = 0;
      i_max = j_max = 0;
      for (i = 0; i < N_CODES; i++) {
         for (j = 0; j < N_CODES; j++) {
            if (copy_freq[i][j] > o_max) {
               o_max = copy_freq[i][j];
               i_max = i; j_max = j;
            }
         }
      }

      debugBelch("%d:  count (%4.1f%%) %lu   is %d then %d\n",
                k, ((double)o_max) * 100.0 / ((double)it_insns), o_max,
                   i_max, j_max );
      copy_freq[i_max][j_max] = 0;

   }
}

#else // !INTERP_STATS

void interp_startup( void ){
}

void interp_shutdown( void ){
}

#define INTERP_TICK(n) /* nothing */

#endif

#if defined(PROFILING)

//
// Build a zero-argument PAP with the current CCS
// See Note [Evaluating functions with profiling] in Apply.cmm
//
STATIC_INLINE
StgClosure * newEmptyPAP (Capability *cap,
                          StgClosure *tagged_obj, // a FUN or a BCO
                          uint32_t arity)
{
    StgPAP *pap = (StgPAP *)allocate(cap, sizeofW(StgPAP));
    SET_HDR(pap, &stg_PAP_info, cap->r.rCCCS);
    pap->arity = arity;
    pap->n_args = 0;
    pap->fun = tagged_obj;
    return (StgClosure *)pap;
}

//
// Make an exact copy of a PAP, except that we combine the current CCS with the
// CCS in the PAP.  See Note [Evaluating functions with profiling] in Apply.cmm
//
STATIC_INLINE
StgClosure * copyPAP  (Capability *cap, StgPAP *oldpap)
{
    uint32_t size = PAP_sizeW(oldpap->n_args);
    StgPAP *pap = (StgPAP *)allocate(cap, size);
    enterFunCCS(&cap->r, oldpap->header.prof.ccs);
    pap->arity = oldpap->arity;
    pap->n_args = oldpap->n_args;
    pap->fun = oldpap->fun;
    uint32_t i;
    for (i = 0; i < ((StgPAP *)pap)->n_args; i++) {
        pap->payload[i] = oldpap->payload[i];
    }
    // No write barrier is needed here as this is a new allocation
    SET_HDR(pap, &stg_PAP_info, cap->r.rCCCS);
    return (StgClosure *)pap;
}

#endif

// See Note [PUSH_L underflow] for in which situations this
// slow lookup is needed
// Returns a pointer to the stack location.
static void*
slow_spw(void *Sp, StgStack *cur_stack, StgWord offset_words){
  // 1. If in range, simply return ptr+offset_words pointing into the current stack chunk
  if (WITHIN_CHUNK_BOUNDS_W(offset_words, cur_stack)) {
    return Sp_plusW(offset_words);
  }
  // 2. Not in this stack chunk, so access the underflow frame.
  else {
    StgWord stackWords;
    StgUnderflowFrame *frame;
    StgStack *new_stack;

    frame = (StgUnderflowFrame*)(cur_stack->stack + cur_stack->stack_size
               - sizeofW(StgUnderflowFrame));

    // 2a. Check it is an underflow frame (the top stack chunk won't have one).
    if( frame->info == &stg_stack_underflow_frame_d_info
       || frame->info == &stg_stack_underflow_frame_v16_info
       || frame->info == &stg_stack_underflow_frame_v32_info
       || frame->info == &stg_stack_underflow_frame_v64_info )
    {

      INTERP_TICK(it_underflow_lookups);

      new_stack = (StgStack*)frame->next_chunk;

      // How many words were on the stack
      stackWords = (StgWord *)frame - (StgWord *) Sp;
      ASSERT(offset_words > stackWords);

      // Recursive, in the very unlikely case we have to traverse two
      // stack chunks.
      return slow_spw(new_stack->sp, new_stack, offset_words-stackWords);
    }
    // 2b. Access the element if there is no underflow frame, it must be right
    // at the top of the stack.
    else {
        // Not actually in the underflow case
        return Sp_plusW(offset_words);
    }
  }
}

// Compute the pointer tag for the constructor and tag the pointer;
// see Note [Data constructor dynamic tags] in GHC.StgToCmm.Closure.
//
// Note: we need to update this if we change the tagging strategy.
STATIC_INLINE StgClosure *tagConstr(StgClosure *con) {
    return TAG_CLOSURE(stg_min(TAG_MASK, 1 + GET_TAG(con)), con);
}

static StgWord app_ptrs_itbl[] = {
    (W_)&stg_ap_p_info,
    (W_)&stg_ap_pp_info,
    (W_)&stg_ap_ppp_info,
    (W_)&stg_ap_pppp_info,
    (W_)&stg_ap_ppppp_info,
    (W_)&stg_ap_pppppp_info,
};

HsStablePtr rts_breakpoint_io_action; // points to the IO action which is executed on a breakpoint
                                      // it is set in ghci/GHCi/Run.hs:withBreakAction

Capability *
interpretBCO (Capability* cap)
{
    // Use of register here is primarily to make it clear to compilers
    // that these entities are non-aliasable.
    register void *Sp;     // local state -- stack pointer
    register void *SpLim;  // local state -- stack lim pointer
    register StgClosure *tagged_obj = 0, *obj = NULL;
    uint32_t n, m;

    LOAD_THREAD_STATE();

    // N.B. HpLim is the context-switch flag; when it
    // goes to zero we must return to the scheduler.
    RELAXED_STORE_ALWAYS(&cap->r.rHpLim, (P_)1);

    IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Entering the interpreter, Sp = %p\n", Sp);
#if defined(PROFILING)
             fprintCCS(stderr, cap->r.rCCCS);
             debugBelch("\n");
#endif
             debugBelch("\n");
             printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             debugBelch("\n\n");
            );

    // ------------------------------------------------------------------------
    // Case 1:
    //
    //       We have a closure to evaluate.  Stack looks like:
    //
    //          |   XXXX_info   |
    //          +---------------+
    //       Sp |      -------------------> closure
    //          +---------------+
    //          |   stg_enter   |
    //          +---------------+
    //
    if (SpW(0) == (W_)&stg_enter_info) {
       Sp_addW(1);
       goto eval;
    }

    // ------------------------------------------------------------------------
    // Case 2:
    //
    //       We have a BCO application to perform.  Stack looks like:
    //
    //          |     ....      |
    //          +---------------+
    //          |     arg1      |
    //          +---------------+
    //          |     BCO       |
    //          +---------------+
    //       Sp |   RET_BCO     |
    //          +---------------+
    //
    else if (SpW(0) == (W_)&stg_apply_interp_info) {
        obj = UNTAG_CLOSURE((StgClosure *)ReadSpW(1));
        Sp_addW(2);
        goto run_BCO_fun;
    }

    // ------------------------------------------------------------------------
    // Case 3:
    //
    //       We have a pointer to return.  See comment before
    //       do_return_pointer, below.
    //
    else if (SpW(0) == (W_)&stg_ret_p_info) {
      tagged_obj = (StgClosure *)ReadSpW(1);
      Sp_addW(2);
      goto do_return_pointer;
    }

    // ------------------------------------------------------------------------
    // Case 4:
    //
    //       We have a nonpointer to return.
    //
    else {
        goto do_return_nonpointer;
    }

    // Evaluate the object on top of the stack.
eval:
    tagged_obj = (StgClosure*)ReadSpW(0); Sp_addW(1);

eval_obj:
    obj = UNTAG_CLOSURE(tagged_obj);
    INTERP_TICK(it_total_entries);

    IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Evaluating: "); printObj(obj);
             debugBelch("Sp = %p\n", Sp);
#if defined(PROFILING)
             fprintCCS(stderr, cap->r.rCCCS);
             debugBelch("\n");
#endif
             debugBelch("\n" );

             printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             debugBelch("\n\n");
            );

//    IF_DEBUG(sanity,checkStackChunk(Sp, cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size));
    IF_DEBUG(sanity,checkStackFrame(Sp));

    switch ( get_itbl(obj)->type ) {

    case IND:
    case IND_STATIC:
    {
        tagged_obj = ACQUIRE_LOAD(&((StgInd*)obj)->indirectee);
        goto eval_obj;
    }

    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_NOCAF:
        // The value is already evaluated, so we can just return it. However,
        // before we do, we MUST ensure that the pointer is tagged, because we
        // might return to a native `case` expression, which assumes the returned
        // pointer is tagged so it can use the tag to select an alternative.
        tagged_obj = tagConstr(obj);
        break;

    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_STATIC:
#if defined(PROFILING)
        if (cap->r.rCCCS != obj->header.prof.ccs) {
            int arity = get_fun_itbl(obj)->f.arity;
            // Tag the function correctly.  We guarantee that pap->fun
            // is correctly tagged (this is checked by
            // Sanity.c:checkPAP()), but we don't guarantee that every
            // pointer to a FUN is tagged on the stack or elsewhere,
            // so we fix the tag here. (#13767)
            // For full details of the invariants on tagging, see
            // https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/haskell-execution/pointer-tagging
            tagged_obj =
                newEmptyPAP(cap,
                            arity <= TAG_MASK
                              ? (StgClosure *) ((intptr_t) obj + arity)
                              : obj,
                            arity);
        }
#endif
        break;

    case PAP:
#if defined(PROFILING)
        if (cap->r.rCCCS != obj->header.prof.ccs) {
            tagged_obj = copyPAP(cap, (StgPAP *)obj);
        }
#endif
        break;

    case BCO:
        ASSERT(((StgBCO *)obj)->arity > 0);
#if defined(PROFILING)
        if (cap->r.rCCCS != obj->header.prof.ccs) {
            tagged_obj = newEmptyPAP(cap, obj, ((StgBCO *)obj)->arity);
        }
#endif
        break;

    case AP:    /* Copied from stg_AP_entry. */
    {
        uint32_t i, words;
        StgAP *ap;

        ap = (StgAP*)obj;
        words = ap->n_args;

        // Stack check
        if (Sp_minusW(words+sizeofW(StgUpdateFrame)+2) < SpLim) {
            Sp_subW(2);
            SpW(1) = (W_)tagged_obj;
            SpW(0) = (W_)&stg_enter_info;
            RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
        }

#if defined(PROFILING)
        // restore the CCCS after evaluating the AP
        Sp_subW(2);
        SpW(1) = (W_)cap->r.rCCCS;
        SpW(0) = (W_)&stg_restore_cccs_eval_info;
#endif

        Sp_subW(sizeofW(StgUpdateFrame));
        {
            StgUpdateFrame *__frame;
            __frame = (StgUpdateFrame *)Sp;
            SET_INFO((StgClosure *)__frame, (StgInfoTable *)&stg_upd_frame_info);
            __frame->updatee = (StgClosure *)(ap);
        }

        ENTER_CCS_THUNK(cap,ap);

        /* Reload the stack */
        Sp_subW(words);
        for (i=0; i < words; i++) {
            SpW(i) = (W_)ap->payload[i];
        }

        obj = UNTAG_CLOSURE((StgClosure*)ap->fun);
        ASSERT(get_itbl(obj)->type == BCO);
        goto run_BCO_fun;
    }

    default:
#if defined(INTERP_STATS)
    {
        int j;

        j = get_itbl(obj)->type;
        ASSERT(j >= 0 && j < N_CLOSURE_TYPES);
        it_unknown_entries[j]++;
        it_total_unknown_entries++;
    }
#endif
    {
        // Can't handle this object; yield to scheduler
        IF_DEBUG(interpreter,
                 debugBelch("evaluating unknown closure -- yielding to sched\n");
                 printObj(obj);
            );
#if defined(PROFILING)
        // restore the CCCS after evaluating the closure
        Sp_subW(2);
        SpW(1) = (W_)cap->r.rCCCS;
        SpW(0) = (W_)&stg_restore_cccs_eval_info;
#endif
        Sp_subW(2);
        SpW(1) = (W_)tagged_obj;
        SpW(0) = (W_)&stg_enter_info;
        RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
    }
    }

    // ------------------------------------------------------------------------
    // We now have a pointer to return (tagged_obj).  The next thing to
    // do is return it to the stack frame on top of the stack.
do_return_pointer:
    obj = UNTAG_CLOSURE(tagged_obj);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(obj));

    IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Returning closure: "); printObj(obj);
             debugBelch("Sp = %p\n", Sp);
#if defined(PROFILING)
             fprintCCS(stderr, cap->r.rCCCS);
             debugBelch("\n");
#endif
             debugBelch("\n");
             printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             debugBelch("\n\n");
            );

    IF_DEBUG(sanity,checkStackChunk(Sp, cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size));

    switch (get_itbl((StgClosure *)Sp)->type) {

    case RET_SMALL: {
        const StgInfoTable *info;

        // NOTE: not using get_itbl().
        info = ((StgClosure *)Sp)->header.info;

        if (info == (StgInfoTable *)&stg_restore_cccs_d_info ||
            info == (StgInfoTable *)&stg_restore_cccs_v16_info ||
            info == (StgInfoTable *)&stg_restore_cccs_v32_info ||
            info == (StgInfoTable *)&stg_restore_cccs_v64_info ||
            info == (StgInfoTable *)&stg_restore_cccs_eval_info) {
            cap->r.rCCCS = (CostCentreStack*)ReadSpW(1);
            Sp_addW(2);
            goto do_return_pointer;
        }

        if (info == (StgInfoTable *)&stg_ap_v_info) {
            n = 1; m = 0; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_f_info) {
            n = 1; m = 1; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_d_info) {
            n = 1; m = sizeofW(StgDouble); goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_l_info) {
            n = 1; m = sizeofW(StgInt64); goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_n_info) {
            n = 1; m = 1; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_p_info) {
            n = 1; m = 1; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_pp_info) {
            n = 2; m = 2; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_ppp_info) {
            n = 3; m = 3; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_pppp_info) {
            n = 4; m = 4; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_ppppp_info) {
            n = 5; m = 5; goto do_apply;
        }
        if (info == (StgInfoTable *)&stg_ap_pppppp_info) {
            n = 6; m = 6; goto do_apply;
        }
        goto do_return_unrecognised;
    }

    case UPDATE_FRAME:
        // Returning to an update frame: do the update, pop the update
        // frame, and continue with the next stack frame.
        //
        // NB. we must update with the *tagged* pointer.  Some tags
        // are not optional, and if we omit the tag bits when updating
        // then bad things can happen (albeit very rarely).  See #1925.
        // What happened was an indirection was created with an
        // untagged pointer, and this untagged pointer was propagated
        // to a PAP by the GC, violating the invariant that PAPs
        // always contain a tagged pointer to the function.
        INTERP_TICK(it_retto_UPDATE);
        updateThunk(cap, cap->r.rCurrentTSO,
                    ((StgUpdateFrame *)Sp)->updatee, tagged_obj);
        Sp_addW(sizeofW(StgUpdateFrame));
        goto do_return_pointer;

    case RET_BCO:
        // Returning to an interpreted continuation: put the object on
        // the stack, and start executing the BCO.
        INTERP_TICK(it_retto_BCO);
        Sp_subW(1);
        SpW(0) = (W_)tagged_obj;
        obj = (StgClosure*)ReadSpW(2);
        ASSERT(get_itbl(obj)->type == BCO);
        goto run_BCO_return_pointer;

    default:
    do_return_unrecognised:
    {
        // Can't handle this return address; yield to scheduler
        INTERP_TICK(it_retto_other);
        IF_DEBUG(interpreter,
                 debugBelch("returning to unknown frame -- yielding to sched\n");
                 printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
            );
        Sp_subW(2);
        SpW(1) = (W_)tagged_obj;
        SpW(0) = (W_)&stg_ret_p_info;
        RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
    }
    }

    // -------------------------------------------------------------------------
    // Returning an unlifted value.  The stack looks like this:
    //
    //    |     ....      |
    //    +---------------+
    //    |     fv2       |
    //    +---------------+
    //    |     fv1       |
    //    +---------------+
    //    |     BCO       |
    //    +---------------+
    //    | stg_ctoi_ret_ |
    //    +---------------+
    //    |    retval     |
    //    +---------------+
    //    |   XXXX_info   |
    //    +---------------+
    //
    // where XXXX_info is one of the stg_ret_*_info family.
    //
    // We're only interested in the case when the real return address
    // is a BCO; otherwise we'll return to the scheduler.

do_return_nonpointer:
    {
        int offset;

        ASSERT(    ReadSpW(0) == (W_)&stg_ret_v_info
                || ReadSpW(0) == (W_)&stg_ret_n_info
                || ReadSpW(0) == (W_)&stg_ret_f_info
                || ReadSpW(0) == (W_)&stg_ret_d_info
                || ReadSpW(0) == (W_)&stg_ret_l_info
                || ReadSpW(0) == (W_)&stg_ret_t_info
            );

        IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Returning nonpointer\n");
             debugBelch("Sp = %p\n", Sp);
#if defined(PROFILING)
             fprintCCS(stderr, cap->r.rCCCS);
             debugBelch("\n");
#endif
             debugBelch("\n");
             printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             debugBelch("\n\n");
            );

        // get the offset of the header of the next stack frame
        offset = stack_frame_sizeW((StgClosure *)Sp);

        switch (get_itbl((StgClosure*)(SafeSpWP(offset)))->type) {

        case RET_BCO:
            // Returning to an interpreted continuation: pop the return frame
            // so the returned value is at the top of the stack, and start
            // executing the BCO.
            INTERP_TICK(it_retto_BCO);
            obj = (StgClosure*)ReadSpW(offset+1);
            ASSERT(get_itbl(obj)->type == BCO);
            goto run_BCO_return_nonpointer;

        default:
        {
            // Can't handle this return address; yield to scheduler
            INTERP_TICK(it_retto_other);
            IF_DEBUG(interpreter,
                     debugBelch("returning to unknown frame -- yielding to sched\n");
                     printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
                );
            RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
        }
        }
    }
    // not reached.


    // -------------------------------------------------------------------------
    // Application...

do_apply:
    ASSERT(obj == UNTAG_CLOSURE(tagged_obj));
    // we have a function to apply (obj), and n arguments taking up m
    // words on the stack.  The info table (stg_ap_pp_info or whatever)
    // is on top of the arguments on the stack.
    {
        switch (get_itbl(obj)->type) {

        case PAP: {
            StgPAP *pap;
            uint32_t i, arity;

            pap = (StgPAP *)obj;

            // we only cope with PAPs whose function is a BCO
            if (get_itbl(UNTAG_CLOSURE(pap->fun))->type != BCO) {
                goto defer_apply_to_sched;
            }

            // Stack check: we're about to unpack the PAP onto the
            // stack.  The (+1) is for the (arity < n) case, where we
            // also need space for an extra info pointer.
            if (Sp_minusW(pap->n_args + 1) < SpLim) {
                Sp_subW(2);
                SpW(1) = (W_)tagged_obj;
                SpW(0) = (W_)&stg_enter_info;
                RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
            }

            Sp_addW(1);
            arity = pap->arity;
            ASSERT(arity > 0);
            if (arity < n) {
                // n must be greater than 1, and the only kinds of
                // application we support with more than one argument
                // are all pointers...
                //
                // Shuffle the args for this function down, and put
                // the appropriate info table in the gap.
                for (i = 0; i < arity; i++) {
                    SpW((int)i-1) = ReadSpW(i);
                    // ^^^^^ careful, i-1 might be negative, but i is unsigned
                }
                SpW(arity-1) = app_ptrs_itbl[n-arity-1];
                Sp_subW(1);
                // unpack the PAP's arguments onto the stack
                Sp_subW(pap->n_args);
                for (i = 0; i < pap->n_args; i++) {
                    SpW(i) = (W_)pap->payload[i];
                }
                obj = UNTAG_CLOSURE(pap->fun);

#if defined(PROFILING)
                enterFunCCS(&cap->r, pap->header.prof.ccs);
#endif
                goto run_BCO_fun;
            }
            else if (arity == n) {
                Sp_subW(pap->n_args);
                for (i = 0; i < pap->n_args; i++) {
                    SpW(i) = (W_)pap->payload[i];
                }
                obj = UNTAG_CLOSURE(pap->fun);
#if defined(PROFILING)
                enterFunCCS(&cap->r, pap->header.prof.ccs);
#endif
                goto run_BCO_fun;
            }
            else /* arity > n */ {
                // build a new PAP and return it.
                StgPAP *new_pap;
                new_pap = (StgPAP *)allocate(cap, PAP_sizeW(pap->n_args + m));
                new_pap->arity = pap->arity - n;
                new_pap->n_args = pap->n_args + m;
                new_pap->fun = pap->fun;
                for (i = 0; i < pap->n_args; i++) {
                    new_pap->payload[i] = pap->payload[i];
                }
                for (i = 0; i < m; i++) {
                    new_pap->payload[pap->n_args + i] = (StgClosure *)ReadSpW(i);
                }
                // No write barrier is needed here as this is a new allocation
                SET_HDR(new_pap,&stg_PAP_info,cap->r.rCCCS);
                tagged_obj = (StgClosure *)new_pap;
                Sp_addW(m);
                goto do_return_pointer;
            }
        }

        case BCO: {
            uint32_t arity, i;

            Sp_addW(1);
            arity = ((StgBCO *)obj)->arity;
            ASSERT(arity > 0);
            if (arity < n) {
                // n must be greater than 1, and the only kinds of
                // application we support with more than one argument
                // are all pointers...
                //
                // Shuffle the args for this function down, and put
                // the appropriate info table in the gap.
                for (i = 0; i < arity; i++) {
                    SpW((int)i-1) = ReadSpW(i);
                    // ^^^^^ careful, i-1 might be negative, but i is unsigned
                }
                SpW(arity-1) = app_ptrs_itbl[n-arity-1];
                Sp_subW(1);
                goto run_BCO_fun;
            }
            else if (arity == n) {
                goto run_BCO_fun;
            }
            else /* arity > n */ {
                // build a PAP and return it.
                StgPAP *pap;
                uint32_t i;
                pap = (StgPAP *)allocate(cap, PAP_sizeW(m));
                pap->arity = arity - n;
                pap->fun = obj;
                pap->n_args = m;
                for (i = 0; i < m; i++) {
                    pap->payload[i] = (StgClosure *)ReadSpW(i);
                }
                // No write barrier is needed here as this is a new allocation
                SET_HDR(pap, &stg_PAP_info,cap->r.rCCCS);
                tagged_obj = (StgClosure *)pap;
                Sp_addW(m);
                goto do_return_pointer;
            }
        }

        // No point in us applying machine-code functions
        default:
        defer_apply_to_sched:
            IF_DEBUG(interpreter,
                     debugBelch("Cannot apply compiled function; yielding to scheduler\n"));
            Sp_subW(2);
            SpW(1) = (W_)tagged_obj;
            SpW(0) = (W_)&stg_enter_info;
            RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
    }

    // ------------------------------------------------------------------------
    // Ok, we now have a bco (obj), and its arguments are all on the
    // stack.  We can start executing the byte codes.
    //
    // The stack is in one of two states.  First, if this BCO is a
    // function:
    //
    //    |     ....      |
    //    +---------------+
    //    |     arg2      |
    //    +---------------+
    //    |     arg1      |
    //    +---------------+
    //
    // Second, if this BCO is a continuation:
    //
    //    |     ....      |
    //    +---------------+
    //    |     fv2       |
    //    +---------------+
    //    |     fv1       |
    //    +---------------+
    //    |     BCO       |
    //    +---------------+
    //    | stg_ctoi_ret_ |
    //    +---------------+
    //    |    retval     |
    //    +---------------+
    //
    // where retval is the value being returned to this continuation.
    // In the event of a stack check, heap check, or context switch,
    // we need to leave the stack in a sane state so the garbage
    // collector can find all the pointers.
    //
    //  (1) BCO is a function:  the BCO's bitmap describes the
    //      pointerhood of the arguments.
    //
    //  (2) BCO is a continuation: BCO's bitmap describes the
    //      pointerhood of the free variables.
    //
    // Sadly we have three different kinds of stack/heap/cswitch check
    // to do:


run_BCO_return_pointer:
    // Heap check
    if (doYouWantToGC(cap)) {
        Sp_subW(1); SpW(0) = (W_)&stg_ret_p_info;
        RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.

    goto run_BCO;

run_BCO_return_nonpointer:
    // Heap check
    if (doYouWantToGC(cap)) {
        RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.

#if defined(PROFILING)
    /*
       Restore the current cost centre stack if a tuple is being returned.

       When a "simple" unlifted value is returned, the cccs is restored with
       an stg_restore_cccs frame on the stack, for example:

           ...
           stg_ctoi_D1
           <CCCS>
           stg_restore_cccs

       But stg_restore_cccs cannot deal with tuples, which may have more
       things on the stack. Therefore we store the CCCS inside the
       stg_ctoi_t frame.

       If we have a tuple being returned, the stack looks like this:

           ...
           <CCCS>           <- to restore, Sp offset <next frame + 4 words>
           tuple_BCO
           tuple_info
           cont_BCO
           stg_ctoi_t       <- next frame
           tuple_data_1
           ...
           tuple_data_n
           tuple_info
           tuple_BCO
           stg_ret_t        <- Sp
     */

    if(SpW(0) == (W_)&stg_ret_t_info) {
        cap->r.rCCCS = (CostCentreStack*)ReadSpW(stack_frame_sizeW((StgClosure *)Sp) + 4);
    }
#endif

    if (SpW(0) != (W_)&stg_ret_t_info) {
      Sp_addW(1);
    }
    goto run_BCO;

run_BCO_fun:
    IF_DEBUG(sanity,
             Sp_subW(2);
             SpW(1) = (W_)obj;
             SpW(0) = (W_)&stg_apply_interp_info;
             checkStackChunk(Sp, cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             Sp_addW(2);
        );

    // Heap check
    if (doYouWantToGC(cap)) {
        Sp_subW(2);
        SpW(1) = (W_)obj;
        SpW(0) = (W_)&stg_apply_interp_info; // placeholder, really
        RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }

    // Stack check
    if (Sp_minusW(INTERP_STACK_CHECK_THRESH) < SpLim) {
        Sp_subW(2);
        SpW(1) = (W_)obj;
        SpW(0) = (W_)&stg_apply_interp_info; // placeholder, really
        RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
    }

    goto run_BCO;

    // Now, actually interpret the BCO... (no returning to the
    // scheduler again until the stack is in an orderly state).
run_BCO:
    INTERP_TICK(it_BCO_entries);
    {
        register int       bciPtr = 0; /* instruction pointer */
        register StgWord16 bci;
        register StgBCO*   bco        = (StgBCO*)obj;
        register StgWord16* instrs    = (StgWord16*)(bco->instrs->payload);
        register StgWord*  literals   = (StgWord*)(&bco->literals->payload[0]);
        register StgPtr*   ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);
        int bcoSize = bco->instrs->bytes / sizeof(StgWord16);
        IF_DEBUG(interpreter,debugBelch("bcoSize = %d\n", bcoSize));

#if defined(INTERP_STATS)
        it_lastopc = 0; /* no opcode */
#endif

    nextInsn:
        ASSERT(bciPtr < bcoSize);
        IF_DEBUG(interpreter,
                 //if (do_print_stack) {
                 //debugBelch("\n-- BEGIN stack\n");
                 //printStack(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size,iSu);
                 //debugBelch("-- END stack\n\n");
                 //}
                 debugBelch("Sp = %p   pc = %-4d ", Sp, bciPtr);
                 disInstr(bco,bciPtr);
                 if (0) { int i;
                 debugBelch("\n");
                 for (i = 8; i >= 0; i--) {
                     debugBelch("%d  %p\n", i, (void *) ReadSpW(i));
                 }
                 debugBelch("\n");
                 }
                 //if (do_print_stack) checkStack(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size,iSu);
            );


        INTERP_TICK(it_insns);

#if defined(INTERP_STATS)
        ASSERT( (int)instrs[bciPtr] >= 0 && (int)instrs[bciPtr] < N_CODES );
        it_ofreq[ (int)instrs[bciPtr] ] ++;
        it_oofreq[ it_lastopc ][ (int)instrs[bciPtr] ] ++;
        it_lastopc = (int)instrs[bciPtr];
#endif

        bci = BCO_NEXT;
    /* We use the high 8 bits for flags. The highest of which is
     * currently allocated to LARGE_ARGS */
    ASSERT((bci & 0xFF00) == (bci & ( bci_FLAG_LARGE_ARGS )));

    switch (bci & 0xFF) {

        /* check for a breakpoint on the beginning of a let binding */
        case bci_BRK_FUN:
        {
            int arg1_brk_array, arg2_tick_mod, arg3_info_mod, arg4_tick_mod_id, arg5_info_mod_id, arg6_tick_index, arg7_info_index;
#if defined(PROFILING)
            int arg8_cc;
#endif
            StgArrBytes *breakPoints;
            int returning_from_break, stop_next_breakpoint;

            // the io action to run at a breakpoint
            StgClosure *ioAction;

            // a closure to save the top stack frame on the heap
            StgAP_STACK *new_aps;

            int i;
            int size_words;

            arg1_brk_array      = BCO_GET_LARGE_ARG;
            arg2_tick_mod       = BCO_GET_LARGE_ARG;
            arg3_info_mod       = BCO_GET_LARGE_ARG;
            arg4_tick_mod_id    = BCO_GET_LARGE_ARG;
            arg5_info_mod_id    = BCO_GET_LARGE_ARG;
            arg6_tick_index     = BCO_NEXT;
            arg7_info_index     = BCO_NEXT;
#if defined(PROFILING)
            arg8_cc             = BCO_GET_LARGE_ARG;
#else
            BCO_GET_LARGE_ARG;
#endif

            // check if we are returning from a breakpoint - this info
            // is stored in the flags field of the current TSO. If true,
            // then don't break this time around.
            returning_from_break =
                cap->r.rCurrentTSO->flags & TSO_STOPPED_ON_BREAKPOINT;

            // check whether this thread is set to stop at the immediate next
            // breakpoint -- either by the global `rts_stop_next_breakpoint`
            // flag, or by the local `TSO_STOP_NEXT_BREAKPOINT`
            stop_next_breakpoint =
              rts_stop_next_breakpoint ||
                cap->r.rCurrentTSO->flags & TSO_STOP_NEXT_BREAKPOINT;

#if defined(PROFILING)
            cap->r.rCCCS = pushCostCentre(cap->r.rCCCS,
                                          (CostCentre*)BCO_LIT(arg8_cc));
#endif

            // if we are returning from a break then skip this section
            // and continue executing
            if (!returning_from_break)
            {
               breakPoints = (StgArrBytes *) BCO_PTR(arg1_brk_array);

               // stop the current thread if either `stop_next_breakpoint` is
               // true OR if the ignore count for this particular breakpoint is zero
               StgInt ignore_count = ((StgInt*)breakPoints->payload)[arg6_tick_index];
               if (stop_next_breakpoint == false && ignore_count > 0)
               {
                  // decrement and write back ignore count
                  ((StgInt*)breakPoints->payload)[arg6_tick_index] = --ignore_count;
               }
               else if (stop_next_breakpoint == true || ignore_count == 0)
               {
                  // make sure we don't automatically stop at the
                  // next breakpoint
                  rts_stop_next_breakpoint = 0;
                  cap->r.rCurrentTSO->flags &= ~TSO_STOP_NEXT_BREAKPOINT;

                  // allocate memory for a new AP_STACK, enough to
                  // store the top stack frame plus an
                  // stg_apply_interp_info pointer and a pointer to
                  // the BCO
                  size_words = BCO_BITMAP_SIZE(obj) + 2;
                  new_aps = (StgAP_STACK *) allocate(cap, AP_STACK_sizeW(size_words));
                  new_aps->size = size_words;
                  new_aps->fun = &stg_dummy_ret_closure;

                  // fill in the payload of the AP_STACK
                  new_aps->payload[0] = (StgClosure *)&stg_apply_interp_info;
                  new_aps->payload[1] = (StgClosure *)obj;

                  // copy the contents of the top stack frame into the AP_STACK
                  for (i = 2; i < size_words; i++)
                  {
                     new_aps->payload[i] = (StgClosure *)ReadSpW(i-2);
                  }

                  // No write barrier is needed here as this is a new allocation
                  SET_HDR(new_aps,&stg_AP_STACK_info,cap->r.rCCCS);

                  // Arrange the stack to call the breakpoint IO action, and
                  // continue execution of this BCO when the IO action returns.
                  //
                  // ioAction :: Addr#       -- the breakpoint tick module
                  //          -> Addr#       -- the breakpoint tick module unit id
                  //          -> Int#        -- the breakpoint tick index
                  //          -> Addr#       -- the breakpoint info module
                  //          -> Addr#       -- the breakpoint info module unit id
                  //          -> Int#        -- the breakpoint info index
                  //          -> Bool        -- exception?
                  //          -> HValue      -- the AP_STACK, or exception
                  //          -> IO ()
                  //
                  ioAction = (StgClosure *) deRefStablePtr (
                      rts_breakpoint_io_action);

                  Sp_subW(19);
                  SpW(18) = (W_)obj;
                  SpW(17) = (W_)&stg_apply_interp_info;
                  SpW(16) = (W_)new_aps;
                  SpW(15) = (W_)False_closure;         // True <=> an exception
                  SpW(14) = (W_)&stg_ap_ppv_info;
                  SpW(13)  = (W_)arg7_info_index;
                  SpW(12)  = (W_)&stg_ap_n_info;
                  SpW(11)  = (W_)BCO_LIT(arg5_info_mod_id);
                  SpW(10)  = (W_)&stg_ap_n_info;
                  SpW(9)  = (W_)BCO_LIT(arg3_info_mod);
                  SpW(8)  = (W_)&stg_ap_n_info;
                  SpW(7)  = (W_)arg6_tick_index;
                  SpW(6)  = (W_)&stg_ap_n_info;
                  SpW(5)  = (W_)BCO_LIT(arg4_tick_mod_id);
                  SpW(4)  = (W_)&stg_ap_n_info;
                  SpW(3)  = (W_)BCO_LIT(arg2_tick_mod);
                  SpW(2)  = (W_)&stg_ap_n_info;
                  SpW(1)  = (W_)ioAction;
                  SpW(0)  = (W_)&stg_enter_info;

                  // set the flag in the TSO to say that we are now
                  // stopping at a breakpoint so that when we resume
                  // we don't stop on the same breakpoint that we
                  // already stopped at just now
                  cap->r.rCurrentTSO->flags |= TSO_STOPPED_ON_BREAKPOINT;

                  // stop this thread and return to the scheduler -
                  // eventually we will come back and the IO action on
                  // the top of the stack will be executed
                  RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
               }
            }
            // record that this thread is not stopped at a breakpoint anymore
            cap->r.rCurrentTSO->flags &= ~TSO_STOPPED_ON_BREAKPOINT;

            // continue normal execution of the byte code instructions
            goto nextInsn;
        }

        case bci_STKCHECK: {
            // Explicit stack check at the beginning of a function
            // *only* (stack checks in case alternatives are
            // propagated to the enclosing function).
            StgWord stk_words_reqd = BCO_GET_LARGE_ARG + 1;
            if (Sp_minusW(stk_words_reqd) < SpLim) {
                Sp_subW(2);
                SpW(1) = (W_)obj;
                SpW(0) = (W_)&stg_apply_interp_info;
                RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
            } else {
                goto nextInsn;
            }
        }

        case bci_PUSH_L: {
            W_ o1 = BCO_GET_LARGE_ARG;
            SpW(-1) = ReadSpW(o1);
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_LL: {
            W_ o1 = BCO_GET_LARGE_ARG;
            W_ o2 = BCO_GET_LARGE_ARG;
            SpW(-1) = ReadSpW(o1);
            SpW(-2) = ReadSpW(o2);
            Sp_subW(2);
            goto nextInsn;
        }

        case bci_PUSH_LLL: {
            W_ o1 = BCO_GET_LARGE_ARG;
            W_ o2 = BCO_GET_LARGE_ARG;
            W_ o3 = BCO_GET_LARGE_ARG;
            SpW(-1) = ReadSpW(o1);
            SpW(-2) = ReadSpW(o2);
            SpW(-3) = ReadSpW(o3);
            Sp_subW(3);
            goto nextInsn;
        }

        case bci_PUSH8: {
            W_ off = BCO_GET_LARGE_ARG;
            Sp_subB(1);
            *(StgWord8*)Sp = (StgWord8) (ReadSpB(off+1));
            goto nextInsn;
        }

        case bci_PUSH16: {
            W_ off = BCO_GET_LARGE_ARG;
            Sp_subB(2);
            *(StgWord16*)Sp = (StgWord16) (ReadSpB(off+2));
            goto nextInsn;
        }

        case bci_PUSH32: {
            W_ off = BCO_GET_LARGE_ARG;
            Sp_subB(4);
            *(StgWord32*)Sp = (StgWord32) (ReadSpB(off+4));
            goto nextInsn;
        }

        case bci_PUSH8_W: {
            W_ off = BCO_GET_LARGE_ARG;
            *(StgWord*)(Sp_minusW(1)) = (StgWord) ((StgWord8) (ReadSpB(off)));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH16_W: {
            W_ off = BCO_GET_LARGE_ARG;
            *(StgWord*)(Sp_minusW(1)) = (StgWord) ((StgWord16) (ReadSpB(off)));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH32_W: {
            W_ off = BCO_GET_LARGE_ARG;
            *(StgWord*)(Sp_minusW(1)) = (StgWord) ((StgWord32) (ReadSpB(off)));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_G: {
            W_ o1 = BCO_GET_LARGE_ARG;
            StgClosure *tagged_obj = (StgClosure*) BCO_PTR(o1);

            tag_push_g:
            ASSERT(LOOKS_LIKE_CLOSURE_PTR((StgClosure*) tagged_obj));
            // Here we make sure references we push are tagged.
            // See Note [CBV Functions and the interpreter] in Info.hs

            //Safe some memory reads if we already have a tag.
            if(GET_CLOSURE_TAG(tagged_obj) == 0) {
                StgClosure *obj = UNTAG_CLOSURE(tagged_obj);
                switch ( get_itbl(obj)->type ) {
                    case IND:
                    case IND_STATIC:
                    {
                        tagged_obj = ACQUIRE_LOAD(&((StgInd*)obj)->indirectee);
                        goto tag_push_g;
                    }
                    case CONSTR:
                    case CONSTR_1_0:
                    case CONSTR_0_1:
                    case CONSTR_2_0:
                    case CONSTR_1_1:
                    case CONSTR_0_2:
                    case CONSTR_NOCAF:
                        // The value is already evaluated, so we can just return it. However,
                        // before we do, we MUST ensure that the pointer is tagged, because we
                        // might return to a native `case` expression, which assumes the returned
                        // pointer is tagged so it can use the tag to select an alternative.
                        tagged_obj = tagConstr(obj);
                        break;
                    default:
                        break;
                }
            }

            SpW(-1) = (W_) tagged_obj;
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_ALTS_P: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            Sp_subW(2);
            SpW(1) = BCO_PTR(o_bco);
            SpW(0) = (W_)&stg_ctoi_R1p_info;
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_N: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_R1n_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_F: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_F1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_D: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_D1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_L: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_L1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_V: {
            W_ o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_V_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_d_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_T: {
            W_ o_bco = BCO_GET_LARGE_ARG;
            W_ tuple_info = (W_)BCO_LIT(BCO_GET_LARGE_ARG);
            W_ o_tuple_bco = BCO_GET_LARGE_ARG;

#if defined(PROFILING)
            SpW(-1) = (W_)cap->r.rCCCS;
            Sp_subW(1);
#endif

            SpW(-1) = BCO_PTR(o_tuple_bco);
            SpW(-2) = tuple_info;
            SpW(-3) = BCO_PTR(o_bco);
            W_ ctoi_t_offset;
            int tuple_stack_words = (tuple_info >> 24) & 0xff;
            switch(tuple_stack_words) {
                case 0:  ctoi_t_offset = (W_)&stg_ctoi_t0_info;  break;
                case 1:  ctoi_t_offset = (W_)&stg_ctoi_t1_info;  break;
                case 2:  ctoi_t_offset = (W_)&stg_ctoi_t2_info;  break;
                case 3:  ctoi_t_offset = (W_)&stg_ctoi_t3_info;  break;
                case 4:  ctoi_t_offset = (W_)&stg_ctoi_t4_info;  break;
                case 5:  ctoi_t_offset = (W_)&stg_ctoi_t5_info;  break;
                case 6:  ctoi_t_offset = (W_)&stg_ctoi_t6_info;  break;
                case 7:  ctoi_t_offset = (W_)&stg_ctoi_t7_info;  break;
                case 8:  ctoi_t_offset = (W_)&stg_ctoi_t8_info;  break;
                case 9:  ctoi_t_offset = (W_)&stg_ctoi_t9_info;  break;

                case 10: ctoi_t_offset = (W_)&stg_ctoi_t10_info; break;
                case 11: ctoi_t_offset = (W_)&stg_ctoi_t11_info; break;
                case 12: ctoi_t_offset = (W_)&stg_ctoi_t12_info; break;
                case 13: ctoi_t_offset = (W_)&stg_ctoi_t13_info; break;
                case 14: ctoi_t_offset = (W_)&stg_ctoi_t14_info; break;
                case 15: ctoi_t_offset = (W_)&stg_ctoi_t15_info; break;
                case 16: ctoi_t_offset = (W_)&stg_ctoi_t16_info; break;
                case 17: ctoi_t_offset = (W_)&stg_ctoi_t17_info; break;
                case 18: ctoi_t_offset = (W_)&stg_ctoi_t18_info; break;
                case 19: ctoi_t_offset = (W_)&stg_ctoi_t19_info; break;

                case 20: ctoi_t_offset = (W_)&stg_ctoi_t20_info; break;
                case 21: ctoi_t_offset = (W_)&stg_ctoi_t21_info; break;
                case 22: ctoi_t_offset = (W_)&stg_ctoi_t22_info; break;
                case 23: ctoi_t_offset = (W_)&stg_ctoi_t23_info; break;
                case 24: ctoi_t_offset = (W_)&stg_ctoi_t24_info; break;
                case 25: ctoi_t_offset = (W_)&stg_ctoi_t25_info; break;
                case 26: ctoi_t_offset = (W_)&stg_ctoi_t26_info; break;
                case 27: ctoi_t_offset = (W_)&stg_ctoi_t27_info; break;
                case 28: ctoi_t_offset = (W_)&stg_ctoi_t28_info; break;
                case 29: ctoi_t_offset = (W_)&stg_ctoi_t29_info; break;

                case 30: ctoi_t_offset = (W_)&stg_ctoi_t30_info; break;
                case 31: ctoi_t_offset = (W_)&stg_ctoi_t31_info; break;
                case 32: ctoi_t_offset = (W_)&stg_ctoi_t32_info; break;
                case 33: ctoi_t_offset = (W_)&stg_ctoi_t33_info; break;
                case 34: ctoi_t_offset = (W_)&stg_ctoi_t34_info; break;
                case 35: ctoi_t_offset = (W_)&stg_ctoi_t35_info; break;
                case 36: ctoi_t_offset = (W_)&stg_ctoi_t36_info; break;
                case 37: ctoi_t_offset = (W_)&stg_ctoi_t37_info; break;
                case 38: ctoi_t_offset = (W_)&stg_ctoi_t38_info; break;
                case 39: ctoi_t_offset = (W_)&stg_ctoi_t39_info; break;

                case 40: ctoi_t_offset = (W_)&stg_ctoi_t40_info; break;
                case 41: ctoi_t_offset = (W_)&stg_ctoi_t41_info; break;
                case 42: ctoi_t_offset = (W_)&stg_ctoi_t42_info; break;
                case 43: ctoi_t_offset = (W_)&stg_ctoi_t43_info; break;
                case 44: ctoi_t_offset = (W_)&stg_ctoi_t44_info; break;
                case 45: ctoi_t_offset = (W_)&stg_ctoi_t45_info; break;
                case 46: ctoi_t_offset = (W_)&stg_ctoi_t46_info; break;
                case 47: ctoi_t_offset = (W_)&stg_ctoi_t47_info; break;
                case 48: ctoi_t_offset = (W_)&stg_ctoi_t48_info; break;
                case 49: ctoi_t_offset = (W_)&stg_ctoi_t49_info; break;

                case 50: ctoi_t_offset = (W_)&stg_ctoi_t50_info; break;
                case 51: ctoi_t_offset = (W_)&stg_ctoi_t51_info; break;
                case 52: ctoi_t_offset = (W_)&stg_ctoi_t52_info; break;
                case 53: ctoi_t_offset = (W_)&stg_ctoi_t53_info; break;
                case 54: ctoi_t_offset = (W_)&stg_ctoi_t54_info; break;
                case 55: ctoi_t_offset = (W_)&stg_ctoi_t55_info; break;
                case 56: ctoi_t_offset = (W_)&stg_ctoi_t56_info; break;
                case 57: ctoi_t_offset = (W_)&stg_ctoi_t57_info; break;
                case 58: ctoi_t_offset = (W_)&stg_ctoi_t58_info; break;
                case 59: ctoi_t_offset = (W_)&stg_ctoi_t59_info; break;

                case 60: ctoi_t_offset = (W_)&stg_ctoi_t60_info; break;
                case 61: ctoi_t_offset = (W_)&stg_ctoi_t61_info; break;
                case 62: ctoi_t_offset = (W_)&stg_ctoi_t62_info; break;

                default: barf("unsupported tuple size %d", tuple_stack_words);
            }

            SpW(-4) = ctoi_t_offset;
            Sp_subW(4);
            goto nextInsn;
        }

        case bci_PUSH_APPLY_N:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_n_info;
            goto nextInsn;
        case bci_PUSH_APPLY_V:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_v_info;
            goto nextInsn;
        case bci_PUSH_APPLY_F:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_f_info;
            goto nextInsn;
        case bci_PUSH_APPLY_D:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_d_info;
            goto nextInsn;
        case bci_PUSH_APPLY_L:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_l_info;
            goto nextInsn;
        case bci_PUSH_APPLY_P:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_p_info;
            goto nextInsn;
        case bci_PUSH_APPLY_PP:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_pp_info;
            goto nextInsn;
        case bci_PUSH_APPLY_PPP:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_ppp_info;
            goto nextInsn;
        case bci_PUSH_APPLY_PPPP:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_pppp_info;
            goto nextInsn;
        case bci_PUSH_APPLY_PPPPP:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_ppppp_info;
            goto nextInsn;
        case bci_PUSH_APPLY_PPPPPP:
            Sp_subW(1); SpW(0) = (W_)&stg_ap_pppppp_info;
            goto nextInsn;

        case bci_PUSH_PAD8: {
            Sp_subB(1);
            *(StgWord8*)Sp = 0;
            goto nextInsn;
        }

        case bci_PUSH_PAD16: {
            Sp_subB(2);
            *(StgWord16*)Sp = 0;
            goto nextInsn;
        }

        case bci_PUSH_PAD32: {
            Sp_subB(4);
            *(StgWord32*)Sp = 0;
            goto nextInsn;
        }

        case bci_PUSH_UBX8: {
            W_ o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(1);
            *(StgWord8*)Sp = (StgWord8) BCO_LIT(o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX16: {
            W_ o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(2);
            *(StgWord16*)Sp = (StgWord16) BCO_LIT(o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX32: {
            W_ o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(4);
            *(StgWord32*)Sp = (StgWord32) BCO_LIT(o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX: {
            W_ i;
            W_ o_lits = BCO_GET_LARGE_ARG;
            W_ n_words = BCO_GET_LARGE_ARG;
            Sp_subW(n_words);
            for (i = 0; i < n_words; i++) {
                SpW(i) = (W_)BCO_LIT(o_lits+i);
            }
            goto nextInsn;
        }

        case bci_SLIDE: {
            W_ n  = BCO_GET_LARGE_ARG;
            W_ by = BCO_GET_LARGE_ARG;
            /*
             * a_1 ... a_n, b_1 ... b_by, k
             *           =>
             * a_1 ... a_n, k
             */
            while(n-- > 0) {
                SpW(n+by) = ReadSpW(n);
            }
            Sp_addW(by);
            INTERP_TICK(it_slides);
            goto nextInsn;
        }

        case bci_ALLOC_AP: {
            StgHalfWord n_payload = BCO_GET_LARGE_ARG;
            StgAP *ap = (StgAP*)allocate(cap, AP_sizeW(n_payload));
            SpW(-1) = (W_)ap;
            ap->n_args = n_payload;
            ap->arity = 0;
            // No write barrier is needed here as this is a new allocation
            // visible only from our stack
            SET_HDR(ap, &stg_AP_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_ALLOC_AP_NOUPD: {
            StgHalfWord n_payload = BCO_GET_LARGE_ARG;
            StgAP *ap = (StgAP*)allocate(cap, AP_sizeW(n_payload));
            SpW(-1) = (W_)ap;
            ap->n_args = n_payload;
            ap->arity = 0;
            // No write barrier is needed here as this is a new allocation
            // visible only from our stack
            SET_HDR(ap, &stg_AP_NOUPD_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_ALLOC_PAP: {
            StgPAP* pap;
            StgHalfWord arity = BCO_GET_LARGE_ARG;
            StgHalfWord n_payload = BCO_GET_LARGE_ARG;
            pap = (StgPAP*)allocate(cap, PAP_sizeW(n_payload));
            SpW(-1) = (W_)pap;
            pap->n_args = n_payload;
            pap->arity = arity;
            // No write barrier is needed here as this is a new allocation
            // visible only from our stack
            SET_HDR(pap, &stg_PAP_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_MKAP: {
            StgHalfWord i;
            W_ stkoff = BCO_GET_LARGE_ARG;
            StgHalfWord n_payload = BCO_GET_LARGE_ARG;
            StgAP* ap = (StgAP*)ReadSpW(stkoff);
            ASSERT(ap->n_args == n_payload);
            ap->fun = (StgClosure*)ReadSpW(0);

            // The function should be a BCO, and its bitmap should
            // cover the payload of the AP correctly.
            ASSERT(get_itbl(ap->fun)->type == BCO
                   && BCO_BITMAP_SIZE(ap->fun) == ap->n_args);

            for (i = 0; i < n_payload; i++) {
                ap->payload[i] = (StgClosure*)ReadSpW(i+1);
            }
            Sp_addW(n_payload+1);
            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)ap);
                );
            goto nextInsn;
        }

        case bci_MKPAP: {
            StgHalfWord i;
            W_ stkoff = BCO_GET_LARGE_ARG;
            StgHalfWord n_payload = BCO_GET_LARGE_ARG;
            StgPAP* pap = (StgPAP*)ReadSpW(stkoff);
            ASSERT(pap->n_args == n_payload);
            pap->fun = (StgClosure*)ReadSpW(0);

            // The function should be a BCO
            if (get_itbl(pap->fun)->type != BCO) {
#if defined(DEBUG)
                printClosure(pap->fun);
#endif
                barf("bci_MKPAP");
            }

            for (i = 0; i < n_payload; i++) {
                pap->payload[i] = (StgClosure*)ReadSpW(i+1);
            }
            Sp_addW(n_payload+1);
            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)pap);
                );
            goto nextInsn;
        }

        case bci_UNPACK: {
            /* Unpack N ptr words from t.o.s constructor */
            W_ i;
            W_ n_words = BCO_GET_LARGE_ARG;
            StgClosure* con = UNTAG_CLOSURE((StgClosure*)ReadSpW(0));
            Sp_subW(n_words);
            for (i = 0; i < n_words; i++) {
                SpW(i) = (W_)con->payload[i];
            }
            goto nextInsn;
        }

        case bci_PACK: {
            W_ o_itbl         = BCO_GET_LARGE_ARG;
            W_ n_words        = BCO_GET_LARGE_ARG;
            StgConInfoTable* itbl = CON_INFO_PTR_TO_STRUCT((StgInfoTable *)BCO_LIT(o_itbl));
            W_ n_ptrs         = itbl->i.layout.payload.ptrs;
            W_ n_nptrs        = itbl->i.layout.payload.nptrs;
            W_ request        = CONSTR_sizeW( n_ptrs, n_nptrs );
            StgClosure* con = (StgClosure*)allocate_NONUPD(cap,request);
            ASSERT(ip_HNF(&itbl->i)); // We don't have a CON flag, HNF is a good approximation
                                      // N.
            // N.B. we may have a nullary datacon with padding, in which case
            // n_nptrs=1, n_ptrs=0.
            ASSERT(n_ptrs + n_nptrs == n_words || (n_nptrs == 1 && n_ptrs == 0));
            ASSERT(n_ptrs + n_nptrs > 0);
            //ASSERT(n_words > 0); // We shouldn't ever need to allocate nullary constructors
            for (W_ i = 0; i < n_words; i++) {
                con->payload[i] = (StgClosure*)ReadSpW(i);
            }
            Sp_addW(n_words);
            Sp_subW(1);
            // No write barrier is needed here as this is a new allocation
            // visible only from our stack
            StgInfoTable *con_ptr = (StgInfoTable*) BCO_LIT(o_itbl);
            SET_HDR(con, con_ptr, cap->r.rCCCS);

            StgClosure* tagged_con = tagConstr(con);
            SpW(0) = (W_)tagged_con;

            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)tagged_con);
                );
            goto nextInsn;
        }

        case bci_TESTLT_P: {
            unsigned int discr  = BCO_NEXT;
            int failto = BCO_GET_LARGE_ARG;
            StgClosure* con = UNTAG_CLOSURE((StgClosure*)ReadSpW(0));
            if (GET_TAG(con) >= discr) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_P: {
            unsigned int discr  = BCO_NEXT;
            int failto = BCO_GET_LARGE_ARG;
            StgClosure* con = UNTAG_CLOSURE((StgClosure*)ReadSpW(0));
            if (GET_TAG(con) != discr) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_I: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            I_ stackInt = (I_)ReadSpW(0);
            if (stackInt >= (I_)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_I64: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt64 stackInt = ReadSpW64(0);
            if (stackInt >= BCO_LITI64(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_I32: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt32 stackInt = (StgInt32) ReadSpW(0);
            if (stackInt >= (StgInt32)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_I16: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt16 stackInt = (StgInt16) ReadSpW(0);
            if (stackInt >= (StgInt16)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_I8: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt8 stackInt = (StgInt8) ReadSpW(0);
            if (stackInt >= (StgInt8)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTEQ_I: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            I_ stackInt = (I_)ReadSpW(0);
            if (stackInt != (I_)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_I64: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt64 stackInt = ReadSpW64(0);
            if (stackInt != BCO_LITI64(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_I32: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt32 stackInt = (StgInt32) ReadSpW(0);
            if (stackInt != (StgInt32)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_I16: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt16 stackInt = (StgInt16) ReadSpW(0);
            if (stackInt != (StgInt16)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_I8: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgInt8 stackInt = (StgInt8) ReadSpW(0);
            if (stackInt != (StgInt8)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_W: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            W_ stackWord = (W_)ReadSpW(0);
            if (stackWord >= (W_)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_W64: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord64 stackWord = ReadSpW64(0);
            if (stackWord >= BCO_LITW64(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_W32: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord32 stackWord = (StgWord32) ReadSpW(0);
            if (stackWord >= (StgWord32)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_W16: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord16 stackWord = (StgInt16) ReadSpW(0);
            if (stackWord >= (StgWord16)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTLT_W8: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord8 stackWord = (StgInt8) ReadSpW(0);
            if (stackWord >= (StgWord8)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTEQ_W: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            W_ stackWord = (W_)ReadSpW(0);
            if (stackWord != (W_)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_W64: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord64 stackWord = ReadSpW64(0);
            if (stackWord != BCO_LITW64(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_W32: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord32 stackWord = (StgWord32) ReadSpW(0);
            if (stackWord != (StgWord32)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_W16: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord16 stackWord = (StgWord16) ReadSpW(0);
            if (stackWord != (StgWord16)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_W8: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgWord8 stackWord = (StgWord8) ReadSpW(0);
            if (stackWord != (StgWord8)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_D: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgDouble stackDbl, discrDbl;
            stackDbl = PK_DBL( & SpW(0) );
            discrDbl = PK_DBL( & BCO_LIT(discr) );
            if (stackDbl >= discrDbl) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_D: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgDouble stackDbl, discrDbl;
            stackDbl = PK_DBL( & SpW(0) );
            discrDbl = PK_DBL( & BCO_LIT(discr) );
            if (stackDbl != discrDbl) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_F: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgFloat stackFlt, discrFlt;
            stackFlt = PK_FLT( & SpW(0) );
            discrFlt = PK_FLT( & BCO_LIT(discr) );
            if (stackFlt >= discrFlt) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_F: {
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgFloat stackFlt, discrFlt;
            stackFlt = PK_FLT( & SpW(0) );
            discrFlt = PK_FLT( & BCO_LIT(discr) );
            if (stackFlt != discrFlt) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        // Control-flow ish things
        case bci_ENTER:
            // Context-switch check.  We put it here to ensure that
            // the interpreter has done at least *some* work before
            // context switching: sometimes the scheduler can invoke
            // the interpreter with context_switch == 1, particularly
            // if the -C0 flag has been given on the cmd line.
            if (RELAXED_LOAD(&cap->r.rHpLim) == NULL) {
                Sp_subW(1); SpW(0) = (W_)&stg_enter_info;
                RETURN_TO_SCHEDULER(ThreadInterpret, ThreadYielding);
            }
            goto eval;

        case bci_RETURN_P:
            tagged_obj = (StgClosure *)ReadSpW(0);
            Sp_addW(1);
            goto do_return_pointer;

        case bci_RETURN_N:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_n_info;
            goto do_return_nonpointer;
        case bci_RETURN_F:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_f_info;
            goto do_return_nonpointer;
        case bci_RETURN_D:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_d_info;
            goto do_return_nonpointer;
        case bci_RETURN_L:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_l_info;
            goto do_return_nonpointer;
        case bci_RETURN_V:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_v_info;
            goto do_return_nonpointer;
        case bci_RETURN_T: {
            /* tuple_info and tuple_bco must already be on the stack */
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_t_info;
            goto do_return_nonpointer;
        }

        case bci_BCO_NAME:
            bciPtr++;
            goto nextInsn;

        case bci_SWIZZLE: {
            W_ stkoff = BCO_GET_LARGE_ARG;
            StgInt n = BCO_GET_LARGE_ARG;
            (*(StgInt*)(SafeSpWP(stkoff))) += n;
            goto nextInsn;
        }

        case bci_PRIMCALL: {
            Sp_subW(1);
            SpW(0) = (W_)&stg_primcall_info;
            RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
        }

// op :: ty -> ty
#define UN_SIZED_OP(op,ty)                                          \
    {                                                               \
        if(sizeof(ty) == 8) {                                       \
            ty r = op ((ty) ReadSpW64(0));                        \
            SpW64(0) = (StgWord64) r;                               \
        } else {                                                    \
            ty r = op ((ty) ReadSpW(0));                          \
            SpW(0) = (StgWord) r;                                   \
        }                                                           \
        goto nextInsn;                                              \
    }

// op :: ty -> ty -> ty
#define SIZED_BIN_OP(op,ty)                                                     \
        {                                                                       \
            if(sizeof(ty) == 8) {                                               \
                ty r = ((ty) ReadSpW64(0)) op ((ty) ReadSpW64(1));                  \
                Sp_addW64(1);                                                   \
                SpW64(0) = (StgWord64) r;                                       \
            } else {                                                            \
                ty r = ((ty) ReadSpW(0)) op ((ty) ReadSpW(1));                  \
                Sp_addW(1);                                                     \
                SpW(0) = (StgWord) r;                                           \
            };                                                                  \
            goto nextInsn;                                                      \
        }

// op :: ty -> Int -> ty
#define SIZED_BIN_OP_TY_INT(op,ty)                                      \
{                                                                       \
    if(sizeof(ty) > sizeof(StgWord)) {                                  \
        ty r = ((ty) ReadSpW64(0)) op ((ty) ReadSpW(2));                \
        Sp_addW(1);                                                     \
        SpW64(0) = (StgWord64) r;                                       \
    } else {                                                            \
        ty r = ((ty) ReadSpW(0)) op ((ty) ReadSpW(1));                  \
        Sp_addW(1);                                                     \
        SpW(0) = (StgWord) r;                                           \
    };                                                                  \
    goto nextInsn;                                                      \
}

// op :: ty -> ty -> Int
#define SIZED_BIN_OP_TY_TY_INT(op,ty)                                   \
{                                                                       \
    if(sizeof(ty) > sizeof(StgWord)) {                                  \
        ty r = ((ty) ReadSpW64(0)) op ((ty) ReadSpW64(1));              \
        Sp_addW(3);                                                     \
        SpW(0) = (StgWord) r;                                       \
    } else {                                                            \
        ty r = ((ty) ReadSpW(0)) op ((ty) ReadSpW(1));                  \
        Sp_addW(1);                                                     \
        SpW(0) = (StgWord) r;                                           \
    };                                                                  \
    goto nextInsn;                                                      \
}

        case bci_OP_ADD_64: SIZED_BIN_OP(+, StgInt64)
        case bci_OP_SUB_64: SIZED_BIN_OP(-, StgInt64)
        case bci_OP_AND_64: SIZED_BIN_OP(&, StgInt64)
        case bci_OP_XOR_64: SIZED_BIN_OP(^, StgInt64)
        case bci_OP_OR_64:  SIZED_BIN_OP(|, StgInt64)
        case bci_OP_MUL_64: SIZED_BIN_OP(*, StgInt64)
        case bci_OP_SHL_64: SIZED_BIN_OP_TY_INT(<<, StgWord64)
        case bci_OP_LSR_64: SIZED_BIN_OP_TY_INT(>>, StgWord64)
        case bci_OP_ASR_64: SIZED_BIN_OP_TY_INT(>>, StgInt64)

        case bci_OP_NEQ_64:  SIZED_BIN_OP_TY_TY_INT(!=, StgWord64)
        case bci_OP_EQ_64:   SIZED_BIN_OP_TY_TY_INT(==, StgWord64)
        case bci_OP_U_GT_64: SIZED_BIN_OP_TY_TY_INT(>, StgWord64)
        case bci_OP_U_GE_64: SIZED_BIN_OP_TY_TY_INT(>=, StgWord64)
        case bci_OP_U_LT_64: SIZED_BIN_OP_TY_TY_INT(<, StgWord64)
        case bci_OP_U_LE_64: SIZED_BIN_OP_TY_TY_INT(<=, StgWord64)

        case bci_OP_S_GT_64: SIZED_BIN_OP_TY_TY_INT(>, StgInt64)
        case bci_OP_S_GE_64: SIZED_BIN_OP_TY_TY_INT(>=, StgInt64)
        case bci_OP_S_LT_64: SIZED_BIN_OP_TY_TY_INT(<, StgInt64)
        case bci_OP_S_LE_64: SIZED_BIN_OP_TY_TY_INT(<=, StgInt64)

        case bci_OP_NOT_64: UN_SIZED_OP(~, StgWord64)
        case bci_OP_NEG_64: UN_SIZED_OP(-, StgInt64)


        case bci_OP_ADD_32: SIZED_BIN_OP(+, StgInt32)
        case bci_OP_SUB_32: SIZED_BIN_OP(-, StgInt32)
        case bci_OP_AND_32: SIZED_BIN_OP(&, StgInt32)
        case bci_OP_XOR_32: SIZED_BIN_OP(^, StgInt32)
        case bci_OP_OR_32:  SIZED_BIN_OP(|, StgInt32)
        case bci_OP_MUL_32: SIZED_BIN_OP(*, StgInt32)
        case bci_OP_SHL_32: SIZED_BIN_OP_TY_INT(<<, StgWord32)
        case bci_OP_LSR_32: SIZED_BIN_OP_TY_INT(>>, StgWord32)
        case bci_OP_ASR_32: SIZED_BIN_OP_TY_INT(>>, StgInt32)

        case bci_OP_NEQ_32:  SIZED_BIN_OP_TY_TY_INT(!=, StgWord32)
        case bci_OP_EQ_32:   SIZED_BIN_OP_TY_TY_INT(==, StgWord32)
        case bci_OP_U_GT_32: SIZED_BIN_OP_TY_TY_INT(>, StgWord32)
        case bci_OP_U_GE_32: SIZED_BIN_OP_TY_TY_INT(>=, StgWord32)
        case bci_OP_U_LT_32: SIZED_BIN_OP_TY_TY_INT(<, StgWord32)
        case bci_OP_U_LE_32: SIZED_BIN_OP_TY_TY_INT(<=, StgWord32)

        case bci_OP_S_GT_32: SIZED_BIN_OP_TY_TY_INT(>, StgInt32)
        case bci_OP_S_GE_32: SIZED_BIN_OP_TY_TY_INT(>=, StgInt32)
        case bci_OP_S_LT_32: SIZED_BIN_OP_TY_TY_INT(<, StgInt32)
        case bci_OP_S_LE_32: SIZED_BIN_OP_TY_TY_INT(<=, StgInt32)

        case bci_OP_NOT_32: UN_SIZED_OP(~, StgWord32)
        case bci_OP_NEG_32: UN_SIZED_OP(-, StgInt32)


        case bci_OP_ADD_16: SIZED_BIN_OP(+, StgInt16)
        case bci_OP_SUB_16: SIZED_BIN_OP(-, StgInt16)
        case bci_OP_AND_16: SIZED_BIN_OP(&, StgInt16)
        case bci_OP_XOR_16: SIZED_BIN_OP(^, StgInt16)
        case bci_OP_OR_16:  SIZED_BIN_OP(|, StgInt16)
        case bci_OP_MUL_16: SIZED_BIN_OP(*, StgInt16)
        case bci_OP_SHL_16: SIZED_BIN_OP_TY_INT(<<, StgWord16)
        case bci_OP_LSR_16: SIZED_BIN_OP_TY_INT(>>, StgWord16)
        case bci_OP_ASR_16: SIZED_BIN_OP_TY_INT(>>, StgInt16)

        case bci_OP_NEQ_16:  SIZED_BIN_OP_TY_TY_INT(!=, StgWord16)
        case bci_OP_EQ_16:   SIZED_BIN_OP_TY_TY_INT(==, StgWord16)
        case bci_OP_U_GT_16: SIZED_BIN_OP_TY_TY_INT(>, StgWord16)
        case bci_OP_U_GE_16: SIZED_BIN_OP_TY_TY_INT(>=, StgWord16)
        case bci_OP_U_LT_16: SIZED_BIN_OP_TY_TY_INT(<, StgWord16)
        case bci_OP_U_LE_16: SIZED_BIN_OP_TY_TY_INT(<=, StgWord16)

        case bci_OP_S_GT_16: SIZED_BIN_OP(>, StgInt16)
        case bci_OP_S_GE_16: SIZED_BIN_OP(>=, StgInt16)
        case bci_OP_S_LT_16: SIZED_BIN_OP(<, StgInt16)
        case bci_OP_S_LE_16: SIZED_BIN_OP(<=, StgInt16)

        case bci_OP_NOT_16: UN_SIZED_OP(~, StgWord16)
        case bci_OP_NEG_16: UN_SIZED_OP(-, StgInt16)


        case bci_OP_ADD_08: SIZED_BIN_OP(+, StgInt8)
        case bci_OP_SUB_08: SIZED_BIN_OP(-, StgInt8)
        case bci_OP_AND_08: SIZED_BIN_OP(&, StgInt8)
        case bci_OP_XOR_08: SIZED_BIN_OP(^, StgInt8)
        case bci_OP_OR_08:  SIZED_BIN_OP(|, StgInt8)
        case bci_OP_MUL_08: SIZED_BIN_OP(*, StgInt8)
        case bci_OP_SHL_08: SIZED_BIN_OP_TY_INT(<<, StgWord8)
        case bci_OP_LSR_08: SIZED_BIN_OP_TY_INT(>>, StgWord8)
        case bci_OP_ASR_08: SIZED_BIN_OP_TY_INT(>>, StgInt8)

        case bci_OP_NEQ_08:  SIZED_BIN_OP_TY_TY_INT(!=, StgWord8)
        case bci_OP_EQ_08:   SIZED_BIN_OP_TY_TY_INT(==, StgWord8)
        case bci_OP_U_GT_08: SIZED_BIN_OP_TY_TY_INT(>, StgWord8)
        case bci_OP_U_GE_08: SIZED_BIN_OP_TY_TY_INT(>=, StgWord8)
        case bci_OP_U_LT_08: SIZED_BIN_OP_TY_TY_INT(<, StgWord8)
        case bci_OP_U_LE_08: SIZED_BIN_OP_TY_TY_INT(<=, StgWord8)

        case bci_OP_S_GT_08: SIZED_BIN_OP_TY_TY_INT(>, StgInt8)
        case bci_OP_S_GE_08: SIZED_BIN_OP_TY_TY_INT(>=, StgInt8)
        case bci_OP_S_LT_08: SIZED_BIN_OP_TY_TY_INT(<, StgInt8)
        case bci_OP_S_LE_08: SIZED_BIN_OP_TY_TY_INT(<=, StgInt8)

        case bci_OP_NOT_08: UN_SIZED_OP(~, StgWord8)
        case bci_OP_NEG_08: UN_SIZED_OP(-, StgInt8)

        case bci_OP_INDEX_ADDR_64:
        {
            StgWord64* addr = (StgWord64*) SpW(0);
            StgInt offset = (StgInt) SpW(1);
            if(sizeof(StgPtr) == sizeof(StgWord64)) {
                Sp_addW(1);
            }
            SpW64(0) = *(addr+offset);
            goto nextInsn;
        }

        case bci_OP_INDEX_ADDR_32:
        {
            StgWord32* addr = (StgWord32*) SpW(0);
            StgInt offset = (StgInt) SpW(1);
            Sp_addW(1);
            SpW(0) = (StgWord) *(addr+offset);
            goto nextInsn;
        }
        case bci_OP_INDEX_ADDR_16:
        {
            StgWord16* addr = (StgWord16*) SpW(0);
            StgInt offset = (StgInt) SpW(1);
            Sp_addW(1);
            SpW(0) = (StgWord) *(addr+offset);
            goto nextInsn;
        }
        case bci_OP_INDEX_ADDR_08:
        {
            StgWord8* addr = (StgWord8*) SpW(0);
            StgInt offset = (StgInt) SpW(1);
            Sp_addW(1);
            SpW(0) = (StgWord) *(addr+offset);
            goto nextInsn;
        }

        case bci_CCALL: {
            void *tok;
            W_ stk_offset             = BCO_GET_LARGE_ARG;
            int o_itbl                = BCO_GET_LARGE_ARG;
            int flags                 = BCO_NEXT;
            bool interruptible        = flags & 0x1;
            bool unsafe_call          = flags & 0x2;
            void(*marshal_fn)(void*) = (void (*)(void*))BCO_LIT(o_itbl);

            /* the stack looks like this:

               |             |  <- Sp + stk_offset
               +-------------+
               |             |
               |    args     |
               |             |  <- Sp + ret_size + 1
               +-------------+
               |    C fun    |  <- Sp + ret_size
               +-------------+
               |     ret     |  <- Sp
               +-------------+

               ret is a placeholder for the return address, and may be
               up to 2 words.

               We need to copy the args out of the TSO, because when
               we call suspendThread() we no longer own the TSO stack,
               and it may move at any time - indeed suspendThread()
               itself may do stack squeezing and move our args.
               So we make a copy of the argument block.
            */

#define ROUND_UP_WDS(p)  ((((StgWord)(p)) + sizeof(W_)-1)/sizeof(W_))

            ffi_cif *cif = (ffi_cif *)marshal_fn;
            uint32_t nargs = cif->nargs;
            uint32_t ret_size;
            uint32_t i;
            W_ j;
            StgPtr p;
            W_ ret[2];                  // max needed
            W_ *arguments[stk_offset];  // max needed
            void *argptrs[nargs];
            void (*fn)(void);

            if (cif->rtype == &ffi_type_void) {
                // necessary because cif->rtype->size == 1 for void,
                // but the bytecode generator has not pushed a
                // placeholder in this case.
                ret_size = 0;
            } else {
                ret_size = ROUND_UP_WDS(cif->rtype->size);
            }

            memcpy(arguments, Sp_plusW(ret_size+1),
                   sizeof(W_) * (stk_offset-1-ret_size));

            // libffi expects the args as an array of pointers to
            // values, so we have to construct this array before making
            // the call.
            p = (StgPtr)arguments;
            for (i = 0; i < nargs; i++) {
#if defined(WORDS_BIGENDIAN)
                // Arguments passed to the interpreter are extended to whole
                // words.  More precisely subwords are passed in the low bytes
                // of a word.  This means p must be adjusted in order to point
                // to the proper subword.  In all other cases the size of the
                // argument type is a multiple of word size as e.g. for type
                // double on 32bit machines and p must not be adjusted.
                argptrs[i] = (void *)((StgWord8 *)p + (sizeof(W_) > cif->arg_types[i]->size
                                                       ? sizeof(W_) - cif->arg_types[i]->size : 0));
#else
                argptrs[i] = (void *)p;
#endif
                // get the size from the cif
                p += ROUND_UP_WDS(cif->arg_types[i]->size);
            }

            // this is the function we're going to call
            fn = (void(*)(void))ReadSpW(ret_size);

            // Restore the Haskell thread's current value of errno
            errno = cap->r.rCurrentTSO->saved_errno;

            // There are a bunch of non-ptr words on the stack (the
            // ccall args, the ccall fun address and space for the
            // result), which we need to cover with an info table
            // since we might GC during this call.
            //
            // We know how many (non-ptr) words there are before the
            // next valid stack frame: it is the stk_offset arg to the
            // CCALL instruction.   So we overwrite this area of the
            // stack with empty stack frames (stg_ret_v_info);
            //
            for (j = 0; j < stk_offset; j++) {
                SpW(j) = (W_)&stg_ret_v_info; /* an empty stack frame */
            }

            // save obj (pointer to the current BCO), since this
            // might move during the call.  We push an stg_ret_p frame
            // for this.
            Sp_subW(2);
            SpW(1) = (W_)obj;
            SpW(0) = (W_)&stg_ret_p_info;

            if (!unsafe_call) {
                SAVE_THREAD_STATE();
                tok = suspendThread(&cap->r, interruptible);
            }

            // We already made a copy of the arguments above.
            ffi_call(cif, fn, ret, argptrs);

            // And restart the thread again, popping the stg_ret_p frame.
            if (!unsafe_call) {
                cap = (Capability *)((void *)((unsigned char*)resumeThread(tok) - STG_FIELD_OFFSET(Capability,r)));
                LOAD_THREAD_STATE();
            }

            if (SpW(0) != (W_)&stg_ret_p_info) {
                // the stack is not how we left it.  This probably
                // means that an exception got raised on exit from the
                // foreign call, so we should just continue with
                // whatever is on top of the stack now.
                RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
            }

            // Re-load the pointer to the BCO from the stg_ret_p frame,
            // it might have moved during the call.  Also reload the
            // pointers to the components of the BCO.
            obj        = (StgClosure*)ReadSpW(1);
              // N.B. this is a BCO and therefore is by definition not tagged
            bco        = (StgBCO*)obj;
            instrs     = (StgWord16*)(bco->instrs->payload);
            literals   = (StgWord*)(&bco->literals->payload[0]);
            ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);

            Sp_addW(2); // pop the stg_ret_p frame

            // Save the Haskell thread's current value of errno
            cap->r.rCurrentTSO->saved_errno = errno;

            // Copy the return value back to the TSO stack.  It is at
            // most 2 words large.
#if defined(WORDS_BIGENDIAN)
            if (sizeof(W_) >= cif->rtype->size) {
                // In contrast to function arguments where subwords are passed
                // in the low bytes of a word, the return value is expected to
                // reside in the high bytes of a word.
                SpW(0) = (*(StgPtr)ret) << ((sizeof(W_) - cif->rtype->size) * 8);
            } else {
                memcpy(Sp, ret, sizeof(W_) * ret_size);
            }
#else
            memcpy(Sp, ret, sizeof(W_) * ret_size);
#endif

            goto nextInsn;
        }

        case bci_JMP: {
            /* BCO_NEXT modifies bciPtr, so be conservative. */
            int nextpc = BCO_GET_LARGE_ARG;
            bciPtr     = nextpc;
            goto nextInsn;
        }

        case bci_CASEFAIL:
            barf("interpretBCO: hit a CASEFAIL");

            // Errors
        default:
            barf("interpretBCO: unknown or unimplemented opcode %d",
                 (int)(bci & 0xFF));

        } /* switch on opcode */
    }
    }

    barf("interpretBCO: fell off end of the interpreter");
}
