/* -----------------------------------------------------------------------------
 * Bytecode interpreter
 *
 * Copyright (c) The GHC Team, 1994-2002.
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Bytecodes.h"

// internal headers
#include "sm/Storage.h"
#include "sm/Sanity.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Updates.h"
#include "Prelude.h"
#include "Stable.h"
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
#if (defined(i386_HOST_ARCH) && !defined(__PIC__)) || defined(x86_64_HOST_ARCH)
# define LIBFFI_NOT_DLL
#endif
#endif

#include "ffi.h"

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
//
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
//
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

#define Sp_plusB(n)  ((void *)(((StgWord8*)Sp) + (n)))
#define Sp_minusB(n) ((void *)(((StgWord8*)Sp) - (n)))

#define Sp_plusW(n)  (Sp_plusB((n) * sizeof(W_)))
#define Sp_minusW(n) (Sp_minusB((n) * sizeof(W_)))

#define Sp_addB(n)   (Sp = Sp_plusB(n))
#define Sp_subB(n)   (Sp = Sp_minusB(n))
#define Sp_addW(n)   (Sp = Sp_plusW(n))
#define Sp_subW(n)   (Sp = Sp_minusW(n))

#define SpW(n)       (*(StgWord*)(Sp_plusW(n)))
#define SpB(n)       (*(StgWord*)(Sp_plusB(n)))

STATIC_INLINE StgPtr
allocate_NONUPD (Capability *cap, int n_words)
{
    return allocate(cap, stg_max(sizeofW(StgHeader)+MIN_PAYLOAD_SIZE, n_words));
}

int rts_stop_next_breakpoint = 0;
int rts_stop_on_exception = 0;

#if defined(INTERP_STATS)

/* Hacky stats, for tuning the interpreter ... */
int it_unknown_entries[N_CLOSURE_TYPES];
int it_total_unknown_entries;
int it_total_entries;

int it_retto_BCO;
int it_retto_UPDATE;
int it_retto_other;

int it_slides;
int it_insns;
int it_BCO_entries;

int it_ofreq[27];
int it_oofreq[27][27];
int it_lastopc;


#define INTERP_TICK(n) (n)++

void interp_startup ( void )
{
   int i, j;
   it_retto_BCO = it_retto_UPDATE = it_retto_other = 0;
   it_total_entries = it_total_unknown_entries = 0;
   for (i = 0; i < N_CLOSURE_TYPES; i++)
      it_unknown_entries[i] = 0;
   it_slides = it_insns = it_BCO_entries = 0;
   for (i = 0; i < 27; i++) it_ofreq[i] = 0;
   for (i = 0; i < 27; i++)
     for (j = 0; j < 27; j++)
        it_oofreq[i][j] = 0;
   it_lastopc = 0;
}

void interp_shutdown ( void )
{
   int i, j, k, o_max, i_max, j_max;
   debugBelch("%d constrs entered -> (%d BCO, %d UPD, %d ??? )\n",
                   it_retto_BCO + it_retto_UPDATE + it_retto_other,
                   it_retto_BCO, it_retto_UPDATE, it_retto_other );
   debugBelch("%d total entries, %d unknown entries \n",
                   it_total_entries, it_total_unknown_entries);
   for (i = 0; i < N_CLOSURE_TYPES; i++) {
     if (it_unknown_entries[i] == 0) continue;
     debugBelch("   type %2d: unknown entries (%4.1f%%) == %d\n",
             i, 100.0 * ((double)it_unknown_entries[i]) /
                        ((double)it_total_unknown_entries),
             it_unknown_entries[i]);
   }
   debugBelch("%d insns, %d slides, %d BCO_entries\n",
                   it_insns, it_slides, it_BCO_entries);
   for (i = 0; i < 27; i++)
      debugBelch("opcode %2d got %d\n", i, it_ofreq[i] );

   for (k = 1; k < 20; k++) {
      o_max = 0;
      i_max = j_max = 0;
      for (i = 0; i < 27; i++) {
         for (j = 0; j < 27; j++) {
            if (it_oofreq[i][j] > o_max) {
               o_max = it_oofreq[i][j];
               i_max = i; j_max = j;
            }
         }
      }

      debugBelch("%d:  count (%4.1f%%) %6d   is %d then %d\n",
                k, ((double)o_max) * 100.0 / ((double)it_insns), o_max,
                   i_max, j_max );
      it_oofreq[i_max][j_max] = 0;

   }
}

#else // !INTERP_STATS

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
    SET_HDR(pap, &stg_PAP_info, cap->r.rCCCS);
    pap->arity = oldpap->arity;
    pap->n_args = oldpap->n_args;
    pap->fun = oldpap->fun;
    uint32_t i;
    for (i = 0; i < ((StgPAP *)pap)->n_args; i++) {
        pap->payload[i] = oldpap->payload[i];
    }
    return (StgClosure *)pap;
}

#endif

static StgWord app_ptrs_itbl[] = {
    (W_)&stg_ap_p_info,
    (W_)&stg_ap_pp_info,
    (W_)&stg_ap_ppp_info,
    (W_)&stg_ap_pppp_info,
    (W_)&stg_ap_ppppp_info,
    (W_)&stg_ap_pppppp_info,
};

HsStablePtr rts_breakpoint_io_action; // points to the IO action which is executed on a breakpoint
                                // it is set in main/GHC.hs:runStmt

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

    cap->r.rHpLim = (P_)1; // HpLim is the context-switch flag; when it
                           // goes to zero we must return to the scheduler.

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
        obj = UNTAG_CLOSURE((StgClosure *)SpW(1));
        Sp_addW(2);
        goto run_BCO_fun;
    }

    // ------------------------------------------------------------------------
    // Case 3:
    //
    //       We have an unboxed value to return.  See comment before
    //       do_return_unboxed, below.
    //
    else {
        goto do_return_unboxed;
    }

    // Evaluate the object on top of the stack.
eval:
    tagged_obj = (StgClosure*)SpW(0); Sp_addW(1);

eval_obj:
    obj = UNTAG_CLOSURE(tagged_obj);
    INTERP_TICK(it_total_evals);

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
        tagged_obj = ((StgInd*)obj)->indirectee;
        goto eval_obj;
    }

    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_NOCAF:
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
            // https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/PointerTagging
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
    // We now have an evaluated object (tagged_obj).  The next thing to
    // do is return it to the stack frame on top of the stack.
do_return:
    obj = UNTAG_CLOSURE(tagged_obj);
    ASSERT(closure_HNF(obj));

    IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Returning: "); printObj(obj);
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

        if (info == (StgInfoTable *)&stg_restore_cccs_info ||
            info == (StgInfoTable *)&stg_restore_cccs_eval_info) {
            cap->r.rCCCS = (CostCentreStack*)SpW(1);
            Sp_addW(2);
            goto do_return;
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
        goto do_return;

    case RET_BCO:
        // Returning to an interpreted continuation: put the object on
        // the stack, and start executing the BCO.
        INTERP_TICK(it_retto_BCO);
        Sp_subW(1);
        SpW(0) = (W_)obj;
        // NB. return the untagged object; the bytecode expects it to
        // be untagged.  XXX this doesn't seem right.
        obj = (StgClosure*)SpW(2);
        ASSERT(get_itbl(obj)->type == BCO);
        goto run_BCO_return;

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
        SpW(0) = (W_)&stg_enter_info;
        RETURN_TO_SCHEDULER_NO_PAUSE(ThreadRunGHC, ThreadYielding);
    }
    }

    // -------------------------------------------------------------------------
    // Returning an unboxed value.  The stack looks like this:
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

do_return_unboxed:
    {
        int offset;

        ASSERT(    SpW(0) == (W_)&stg_ret_v_info
                || SpW(0) == (W_)&stg_ret_p_info
                || SpW(0) == (W_)&stg_ret_n_info
                || SpW(0) == (W_)&stg_ret_f_info
                || SpW(0) == (W_)&stg_ret_d_info
                || SpW(0) == (W_)&stg_ret_l_info
            );

        IF_DEBUG(interpreter,
             debugBelch(
             "\n---------------------------------------------------------------\n");
             debugBelch("Returning: "); printObj(obj);
             debugBelch("Sp = %p\n", Sp);
#if defined(PROFILING)
             fprintCCS(stderr, cap->r.rCCCS);
             debugBelch("\n");
#endif
             debugBelch("\n");
             printStackChunk(Sp,cap->r.rCurrentTSO->stackobj->stack+cap->r.rCurrentTSO->stackobj->stack_size);
             debugBelch("\n\n");
            );

        // get the offset of the stg_ctoi_ret_XXX itbl
        offset = stack_frame_sizeW((StgClosure *)Sp);

        switch (get_itbl((StgClosure*)(Sp_plusW(offset)))->type) {

        case RET_BCO:
            // Returning to an interpreted continuation: put the object on
            // the stack, and start executing the BCO.
            INTERP_TICK(it_retto_BCO);
            obj = (StgClosure*)SpW(offset+1);
            ASSERT(get_itbl(obj)->type == BCO);
            goto run_BCO_return_unboxed;

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
                    SpW((int)i-1) = SpW(i);
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
                SET_HDR(new_pap,&stg_PAP_info,cap->r.rCCCS);
                new_pap->arity = pap->arity - n;
                new_pap->n_args = pap->n_args + m;
                new_pap->fun = pap->fun;
                for (i = 0; i < pap->n_args; i++) {
                    new_pap->payload[i] = pap->payload[i];
                }
                for (i = 0; i < m; i++) {
                    new_pap->payload[pap->n_args + i] = (StgClosure *)SpW(i);
                }
                tagged_obj = (StgClosure *)new_pap;
                Sp_addW(m);
                goto do_return;
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
                    SpW((int)i-1) = SpW(i);
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
                SET_HDR(pap, &stg_PAP_info,cap->r.rCCCS);
                pap->arity = arity - n;
                pap->fun = obj;
                pap->n_args = m;
                for (i = 0; i < m; i++) {
                    pap->payload[i] = (StgClosure *)SpW(i);
                }
                tagged_obj = (StgClosure *)pap;
                Sp_addW(m);
                goto do_return;
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


run_BCO_return:
    // Heap check
    if (doYouWantToGC(cap)) {
        Sp_subW(1); SpW(0) = (W_)&stg_enter_info;
        RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.

    goto run_BCO;

run_BCO_return_unboxed:
    // Heap check
    if (doYouWantToGC(cap)) {
        RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.

    goto run_BCO;

run_BCO_fun:
    IF_DEBUG(sanity,
             Sp_subW(2);
             SpW(1) = (W_)obj;
             SpW(0) = (W_)&stg_apply_interp_info;
             checkStackChunk(Sp,SpLim);
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
#if defined(DEBUG)
        int bcoSize;
        bcoSize = bco->instrs->bytes / sizeof(StgWord16);
#endif
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
                     debugBelch("%d  %p\n", i, (void *) SpW(i));
                 }
                 debugBelch("\n");
                 }
                 //if (do_print_stack) checkStack(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size,iSu);
            );


        INTERP_TICK(it_insns);

#if defined(INTERP_STATS)
        ASSERT( (int)instrs[bciPtr] >= 0 && (int)instrs[bciPtr] < 27 );
        it_ofreq[ (int)instrs[bciPtr] ] ++;
        it_oofreq[ it_lastopc ][ (int)instrs[bciPtr] ] ++;
        it_lastopc = (int)instrs[bciPtr];
#endif

        bci = BCO_NEXT;
    /* We use the high 8 bits for flags, only the highest of which is
     * currently allocated */
    ASSERT((bci & 0xFF00) == (bci & 0x8000));

    switch (bci & 0xFF) {

        /* check for a breakpoint on the beginning of a let binding */
        case bci_BRK_FUN:
        {
            int arg1_brk_array, arg2_array_index, arg3_module_uniq;
#if defined(PROFILING)
            int arg4_cc;
#endif
            StgArrBytes *breakPoints;
            int returning_from_break;

            // the io action to run at a breakpoint
            StgClosure *ioAction;

            // a closure to save the top stack frame on the heap
            StgAP_STACK *new_aps;

            int i;
            int size_words;

            arg1_brk_array      = BCO_GET_LARGE_ARG;
            arg2_array_index    = BCO_NEXT;
            arg3_module_uniq    = BCO_GET_LARGE_ARG;
#if defined(PROFILING)
            arg4_cc             = BCO_GET_LARGE_ARG;
#else
            BCO_GET_LARGE_ARG;
#endif

            // check if we are returning from a breakpoint - this info
            // is stored in the flags field of the current TSO. If true,
            // then don't break this time around.
            returning_from_break =
                cap->r.rCurrentTSO->flags & TSO_STOPPED_ON_BREAKPOINT;

#if defined(PROFILING)
            cap->r.rCCCS = pushCostCentre(cap->r.rCCCS,
                                          (CostCentre*)BCO_LIT(arg4_cc));
#endif

            // if we are returning from a break then skip this section
            // and continue executing
            if (!returning_from_break)
            {
               breakPoints = (StgArrBytes *) BCO_PTR(arg1_brk_array);

               // stop the current thread if either the
               // "rts_stop_next_breakpoint" flag is true OR if the
               // breakpoint flag for this particular expression is
               // true
               if (rts_stop_next_breakpoint == true ||
                   ((StgWord8*)breakPoints->payload)[arg2_array_index]
                     == true)
               {
                  // make sure we don't automatically stop at the
                  // next breakpoint
                  rts_stop_next_breakpoint = false;

                  // allocate memory for a new AP_STACK, enough to
                  // store the top stack frame plus an
                  // stg_apply_interp_info pointer and a pointer to
                  // the BCO
                  size_words = BCO_BITMAP_SIZE(obj) + 2;
                  new_aps = (StgAP_STACK *) allocate(cap, AP_STACK_sizeW(size_words));
                  SET_HDR(new_aps,&stg_AP_STACK_info,cap->r.rCCCS);
                  new_aps->size = size_words;
                  new_aps->fun = &stg_dummy_ret_closure;

                  // fill in the payload of the AP_STACK
                  new_aps->payload[0] = (StgClosure *)&stg_apply_interp_info;
                  new_aps->payload[1] = (StgClosure *)obj;

                  // copy the contents of the top stack frame into the AP_STACK
                  for (i = 2; i < size_words; i++)
                  {
                     new_aps->payload[i] = (StgClosure *)SpW(i-2);
                  }

                  // Arrange the stack to call the breakpoint IO action, and
                  // continue execution of this BCO when the IO action returns.
                  //
                  // ioAction :: Bool        -- exception?
                  //          -> HValue      -- the AP_STACK, or exception
                  //          -> Int         -- the breakpoint index (arg2)
                  //          -> Int         -- the module uniq (arg3)
                  //          -> IO ()
                  //
                  ioAction = (StgClosure *) deRefStablePtr (
                      rts_breakpoint_io_action);

                  Sp_subW(11);
                  SpW(10) = (W_)obj;
                  SpW(9)  = (W_)&stg_apply_interp_info;
                  SpW(8)  = (W_)new_aps;
                  SpW(7)  = (W_)False_closure;         // True <=> a breakpoint
                  SpW(6)  = (W_)&stg_ap_ppv_info;
                  SpW(5)  = (W_)BCO_LIT(arg3_module_uniq);
                  SpW(4)  = (W_)&stg_ap_n_info;
                  SpW(3)  = (W_)arg2_array_index;
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
            int o1 = BCO_NEXT;
            SpW(-1) = SpW(o1);
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_LL: {
            int o1 = BCO_NEXT;
            int o2 = BCO_NEXT;
            SpW(-1) = SpW(o1);
            SpW(-2) = SpW(o2);
            Sp_subW(2);
            goto nextInsn;
        }

        case bci_PUSH_LLL: {
            int o1 = BCO_NEXT;
            int o2 = BCO_NEXT;
            int o3 = BCO_NEXT;
            SpW(-1) = SpW(o1);
            SpW(-2) = SpW(o2);
            SpW(-3) = SpW(o3);
            Sp_subW(3);
            goto nextInsn;
        }

        case bci_PUSH8: {
            int off = BCO_NEXT;
            Sp_subB(1);
            *(StgWord8*)Sp = *(StgWord8*)(Sp_plusB(off+1));
            goto nextInsn;
        }

        case bci_PUSH16: {
            int off = BCO_NEXT;
            Sp_subB(2);
            *(StgWord16*)Sp = *(StgWord16*)(Sp_plusB(off+2));
            goto nextInsn;
        }

        case bci_PUSH32: {
            int off = BCO_NEXT;
            Sp_subB(4);
            *(StgWord32*)Sp = *(StgWord32*)(Sp_plusB(off+4));
            goto nextInsn;
        }

        case bci_PUSH8_W: {
            int off = BCO_NEXT;
            *(StgWord*)(Sp_minusW(1)) = *(StgWord8*)(Sp_plusB(off));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH16_W: {
            int off = BCO_NEXT;
            *(StgWord*)(Sp_minusW(1)) = *(StgWord16*)(Sp_plusB(off));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH32_W: {
            int off = BCO_NEXT;
            *(StgWord*)(Sp_minusW(1)) = *(StgWord32*)(Sp_plusB(off));
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_G: {
            int o1 = BCO_GET_LARGE_ARG;
            SpW(-1) = BCO_PTR(o1);
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_PUSH_ALTS: {
            int o_bco  = BCO_GET_LARGE_ARG;
            Sp_subW(2);
            SpW(1) = BCO_PTR(o_bco);
            SpW(0) = (W_)&stg_ctoi_R1p_info;
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_P: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_R1unpt_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_N: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_R1n_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_F: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_F1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_D: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_D1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_L: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_L1_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
            goto nextInsn;
        }

        case bci_PUSH_ALTS_V: {
            int o_bco  = BCO_GET_LARGE_ARG;
            SpW(-2) = (W_)&stg_ctoi_V_info;
            SpW(-1) = BCO_PTR(o_bco);
            Sp_subW(2);
#if defined(PROFILING)
            Sp_subW(2);
            SpW(1) = (W_)cap->r.rCCCS;
            SpW(0) = (W_)&stg_restore_cccs_info;
#endif
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
            int o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(1);
            *(StgWord8*)Sp = *(StgWord8*)(literals+o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX16: {
            int o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(2);
            *(StgWord16*)Sp = *(StgWord16*)(literals+o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX32: {
            int o_lit = BCO_GET_LARGE_ARG;
            Sp_subB(4);
            *(StgWord32*)Sp = *(StgWord32*)(literals+o_lit);
            goto nextInsn;
        }

        case bci_PUSH_UBX: {
            int i;
            int o_lits = BCO_GET_LARGE_ARG;
            int n_words = BCO_NEXT;
            Sp_subW(n_words);
            for (i = 0; i < n_words; i++) {
                SpW(i) = (W_)BCO_LIT(o_lits+i);
            }
            goto nextInsn;
        }

        case bci_SLIDE: {
            int n  = BCO_NEXT;
            int by = BCO_NEXT;
            /* a_1, .. a_n, b_1, .. b_by, s => a_1, .. a_n, s */
            while(--n >= 0) {
                SpW(n+by) = SpW(n);
            }
            Sp_addW(by);
            INTERP_TICK(it_slides);
            goto nextInsn;
        }

        case bci_ALLOC_AP: {
            StgAP* ap;
            int n_payload = BCO_NEXT;
            ap = (StgAP*)allocate(cap, AP_sizeW(n_payload));
            SpW(-1) = (W_)ap;
            ap->n_args = n_payload;
            SET_HDR(ap, &stg_AP_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_ALLOC_AP_NOUPD: {
            StgAP* ap;
            int n_payload = BCO_NEXT;
            ap = (StgAP*)allocate(cap, AP_sizeW(n_payload));
            SpW(-1) = (W_)ap;
            ap->n_args = n_payload;
            SET_HDR(ap, &stg_AP_NOUPD_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_ALLOC_PAP: {
            StgPAP* pap;
            int arity = BCO_NEXT;
            int n_payload = BCO_NEXT;
            pap = (StgPAP*)allocate(cap, PAP_sizeW(n_payload));
            SpW(-1) = (W_)pap;
            pap->n_args = n_payload;
            pap->arity = arity;
            SET_HDR(pap, &stg_PAP_info, cap->r.rCCCS)
            Sp_subW(1);
            goto nextInsn;
        }

        case bci_MKAP: {
            int i;
            int stkoff = BCO_NEXT;
            int n_payload = BCO_NEXT;
            StgAP* ap = (StgAP*)SpW(stkoff);
            ASSERT((int)ap->n_args == n_payload);
            ap->fun = (StgClosure*)SpW(0);

            // The function should be a BCO, and its bitmap should
            // cover the payload of the AP correctly.
            ASSERT(get_itbl(ap->fun)->type == BCO
                   && BCO_BITMAP_SIZE(ap->fun) == ap->n_args);

            for (i = 0; i < n_payload; i++)
                ap->payload[i] = (StgClosure*)SpW(i+1);
            Sp_addW(n_payload+1);
            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)ap);
                );
            goto nextInsn;
        }

        case bci_MKPAP: {
            int i;
            int stkoff = BCO_NEXT;
            int n_payload = BCO_NEXT;
            StgPAP* pap = (StgPAP*)SpW(stkoff);
            ASSERT((int)pap->n_args == n_payload);
            pap->fun = (StgClosure*)SpW(0);

            // The function should be a BCO
            if (get_itbl(pap->fun)->type != BCO) {
#if defined(DEBUG)
                printClosure(pap->fun);
#endif
                barf("bci_MKPAP");
            }

            for (i = 0; i < n_payload; i++)
                pap->payload[i] = (StgClosure*)SpW(i+1);
            Sp_addW(n_payload+1);
            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)pap);
                );
            goto nextInsn;
        }

        case bci_UNPACK: {
            /* Unpack N ptr words from t.o.s constructor */
            int i;
            int n_words = BCO_NEXT;
            StgClosure* con = (StgClosure*)SpW(0);
            Sp_subW(n_words);
            for (i = 0; i < n_words; i++) {
                SpW(i) = (W_)con->payload[i];
            }
            goto nextInsn;
        }

        case bci_PACK: {
            int i;
            int o_itbl         = BCO_GET_LARGE_ARG;
            int n_words        = BCO_NEXT;
            StgInfoTable* itbl = INFO_PTR_TO_STRUCT((StgInfoTable *)BCO_LIT(o_itbl));
            int request        = CONSTR_sizeW( itbl->layout.payload.ptrs,
                                               itbl->layout.payload.nptrs );
            StgClosure* con = (StgClosure*)allocate_NONUPD(cap,request);
            ASSERT( itbl->layout.payload.ptrs + itbl->layout.payload.nptrs > 0);
            SET_HDR(con, (StgInfoTable*)BCO_LIT(o_itbl), cap->r.rCCCS);
            for (i = 0; i < n_words; i++) {
                con->payload[i] = (StgClosure*)SpW(i);
            }
            Sp_addW(n_words);
            Sp_subW(1);
            SpW(0) = (W_)con;
            IF_DEBUG(interpreter,
                     debugBelch("\tBuilt ");
                     printObj((StgClosure*)con);
                );
            goto nextInsn;
        }

        case bci_TESTLT_P: {
            unsigned int discr  = BCO_NEXT;
            int failto = BCO_GET_LARGE_ARG;
            StgClosure* con = (StgClosure*)SpW(0);
            if (GET_TAG(con) >= discr) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_P: {
            unsigned int discr  = BCO_NEXT;
            int failto = BCO_GET_LARGE_ARG;
            StgClosure* con = (StgClosure*)SpW(0);
            if (GET_TAG(con) != discr) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_I: {
            // There should be an Int at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            I_ stackInt = (I_)SpW(1);
            if (stackInt >= (I_)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTEQ_I: {
            // There should be an Int at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            I_ stackInt = (I_)SpW(1);
            if (stackInt != (I_)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_W: {
            // There should be an Int at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            W_ stackWord = (W_)SpW(1);
            if (stackWord >= (W_)BCO_LIT(discr))
                bciPtr = failto;
            goto nextInsn;
        }

        case bci_TESTEQ_W: {
            // There should be an Int at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            W_ stackWord = (W_)SpW(1);
            if (stackWord != (W_)BCO_LIT(discr)) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_D: {
            // There should be a Double at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgDouble stackDbl, discrDbl;
            stackDbl = PK_DBL( & SpW(1) );
            discrDbl = PK_DBL( & BCO_LIT(discr) );
            if (stackDbl >= discrDbl) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_D: {
            // There should be a Double at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgDouble stackDbl, discrDbl;
            stackDbl = PK_DBL( & SpW(1) );
            discrDbl = PK_DBL( & BCO_LIT(discr) );
            if (stackDbl != discrDbl) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTLT_F: {
            // There should be a Float at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgFloat stackFlt, discrFlt;
            stackFlt = PK_FLT( & SpW(1) );
            discrFlt = PK_FLT( & BCO_LIT(discr) );
            if (stackFlt >= discrFlt) {
                bciPtr = failto;
            }
            goto nextInsn;
        }

        case bci_TESTEQ_F: {
            // There should be a Float at SpW(1), and an info table at SpW(0).
            int discr   = BCO_GET_LARGE_ARG;
            int failto  = BCO_GET_LARGE_ARG;
            StgFloat stackFlt, discrFlt;
            stackFlt = PK_FLT( & SpW(1) );
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
            if (cap->r.rHpLim == NULL) {
                Sp_subW(1); SpW(0) = (W_)&stg_enter_info;
                RETURN_TO_SCHEDULER(ThreadInterpret, ThreadYielding);
            }
            goto eval;

        case bci_RETURN:
            tagged_obj = (StgClosure *)SpW(0);
            Sp_addW(1);
            goto do_return;

        case bci_RETURN_P:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_p_info;
            goto do_return_unboxed;
        case bci_RETURN_N:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_n_info;
            goto do_return_unboxed;
        case bci_RETURN_F:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_f_info;
            goto do_return_unboxed;
        case bci_RETURN_D:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_d_info;
            goto do_return_unboxed;
        case bci_RETURN_L:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_l_info;
            goto do_return_unboxed;
        case bci_RETURN_V:
            Sp_subW(1);
            SpW(0) = (W_)&stg_ret_v_info;
            goto do_return_unboxed;

        case bci_SWIZZLE: {
            int stkoff = BCO_NEXT;
            signed short n = (signed short)(BCO_NEXT);
            SpW(stkoff) += (W_)n;
            goto nextInsn;
        }

        case bci_CCALL: {
            void *tok;
            int stk_offset            = BCO_NEXT;
            int o_itbl                = BCO_GET_LARGE_ARG;
            int flags                 = BCO_NEXT;
            bool interruptible        = flags & 0x1;
            bool unsafe_call          = flags & 0x2;
            void(*marshall_fn)(void*) = (void (*)(void*))BCO_LIT(o_itbl);

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

            ffi_cif *cif = (ffi_cif *)marshall_fn;
            uint32_t nargs = cif->nargs;
            uint32_t ret_size;
            uint32_t i;
            int j;
            StgPtr p;
            W_ ret[2];                  // max needed
            W_ *arguments[stk_offset];  // max needed
            void *argptrs[nargs];
            void (*fn)(void);

            if (cif->rtype->type == FFI_TYPE_VOID) {
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
                argptrs[i] = (void *)p;
                // get the size from the cif
                p += ROUND_UP_WDS(cif->arg_types[i]->size);
            }

            // this is the function we're going to call
            fn = (void(*)(void))SpW(ret_size);

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
            obj        = (StgClosure*)SpW(1);
            bco        = (StgBCO*)obj;
            instrs     = (StgWord16*)(bco->instrs->payload);
            literals   = (StgWord*)(&bco->literals->payload[0]);
            ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);

            Sp_addW(2); // pop the stg_ret_p frame

            // Save the Haskell thread's current value of errno
            cap->r.rCurrentTSO->saved_errno = errno;

            // Copy the return value back to the TSO stack.  It is at
            // most 2 words large, and resides at arguments[0].
            memcpy(Sp, ret, sizeof(W_) * stg_min(stk_offset,ret_size));

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
