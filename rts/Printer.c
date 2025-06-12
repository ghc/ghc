/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1994-2000.
 *
 * Heap printer: This is used for debugging within GDB or for emitting debug
 * prints.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "ghcconfig.h"

#include "Rts.h"
#include "rts/Bytecodes.h"  /* for InstrPtr */

#include "sm/Storage.h"
#include "sm/GCThread.h"
#include "Hash.h"
#include "Printer.h"
#include "RtsUtils.h"

#if defined(PROFILING)
#include "Profiling.h"
#endif

#include <string.h>

#if defined(DEBUG)

#include "Disassembler.h"
#include "Apply.h"

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

static void    printStdObjPayload( const StgClosure *obj );

/* --------------------------------------------------------------------------
 * Printer
 * ------------------------------------------------------------------------*/

void printPtr( StgPtr p )
{
    const char *raw;
    raw = lookupGHCName(p);
    if (raw != NULL) {
        debugBelch("<%s>", raw);
        debugBelch("[%p]", p);
    } else {
        debugBelch("%p", p);
    }
}

void printObj( StgClosure *obj )
{
    debugBelch("Object "); printPtr((StgPtr)obj); debugBelch(" = ");
    printClosure(obj);
}

STATIC_INLINE void
printStdObjHdr( const StgClosure *obj, char* tag )
{
    debugBelch("%s(",tag);
    printPtr((StgPtr)obj->header.info);
#if defined(PROFILING)
    debugBelch(", %s", obj->header.prof.ccs->cc->label);
#endif
}

static void
printStdObjPayload( const StgClosure *obj )
{
    StgWord i, j;
    const StgInfoTable* info;

    info = get_itbl(obj);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        debugBelch(", ");
        printPtr((StgPtr)obj->payload[i]);
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        debugBelch(", %pd#",obj->payload[i+j]);
    }
    debugBelch(")\n");
}

static void
printThunkPayload( StgThunk *obj )
{
    StgWord i, j;
    const StgInfoTable* info;

    info = get_itbl((StgClosure *)obj);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        debugBelch(", ");
        printPtr((StgPtr)obj->payload[i]);
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        debugBelch(", %pd#",obj->payload[i+j]);
    }
    debugBelch(")\n");
}

static void
printThunkObject( StgThunk *obj, char* tag )
{
    printStdObjHdr( (StgClosure *)obj, tag );
    printThunkPayload( obj );
}

void
printClosure( const StgClosure *obj )
{
    debugBelch("%p: ", obj);
    obj = UNTAG_CONST_CLOSURE(obj);
    const StgInfoTable* info = get_itbl(obj);

    while (IS_FORWARDING_PTR(info)) {
        obj = (StgClosure*)UN_FORWARDING_PTR(info);
        debugBelch("(forwarding to %p) ", (void*)obj);
        info = get_itbl(obj);
    }

    switch ( info->type ) {
    case INVALID_OBJECT:
            barf("Invalid object");

    case CONSTR:
    case CONSTR_1_0: case CONSTR_0_1:
    case CONSTR_1_1: case CONSTR_0_2: case CONSTR_2_0:
    case CONSTR_NOCAF:
        {
            StgWord i, j;
            const StgConInfoTable *con_info = get_con_itbl (obj);

            debugBelch("%s(", GET_CON_DESC(con_info));
            for (i = 0; i < info->layout.payload.ptrs; ++i) {
                if (i != 0) debugBelch(", ");
                printPtr((StgPtr)obj->payload[i]);
            }
            for (j = 0; j < info->layout.payload.nptrs; ++j) {
                if (i != 0 || j != 0) debugBelch(", ");
                debugBelch("%p#", obj->payload[i+j]);
            }
            debugBelch(")\n");
            break;
        }

    case FUN:
    case FUN_1_0: case FUN_0_1:
    case FUN_1_1: case FUN_0_2: case FUN_2_0:
    case FUN_STATIC:
        {
            debugBelch("FUN/%d(",(int)itbl_to_fun_itbl(info)->f.arity);
            printPtr((StgPtr)obj->header.info);

            InfoProvEnt ipe;
            if (lookupIPE(obj->header.info, &ipe)) {
                debugBelch(", %s", ipe.prov.table_name);
            }
#if defined(PROFILING)
            debugBelch(", %s", obj->header.prof.ccs->cc->label);
#endif
            printStdObjPayload(obj);
            break;
        }

    case PRIM:
        debugBelch("PRIM(");
        printPtr((StgPtr)obj->header.info);
        printStdObjPayload(obj);
        break;

    case MUT_PRIM:
        debugBelch("MUT_PRIM(");
        printPtr((StgPtr)obj->header.info);
        printStdObjPayload(obj);
        break;

    case THUNK:
    case THUNK_1_0: case THUNK_0_1:
    case THUNK_1_1: case THUNK_0_2: case THUNK_2_0:
    case THUNK_STATIC:
        {
            /* ToDo: will this work for THUNK_STATIC too? */
#if defined(PROFILING)
            printThunkObject((StgThunk *)obj,GET_PROF_DESC(info));
#else
            printThunkObject((StgThunk *)obj,"THUNK");
            InfoProvEnt ipe;
            if (lookupIPE(obj->header.info, &ipe)) {
                debugBelch(", %s", ipe.prov.table_name);
            }
#endif
            break;
        }

    case THUNK_SELECTOR:
        printStdObjHdr(obj, "THUNK_SELECTOR");
        debugBelch(", %p)\n", ((StgSelector *)obj)->selectee);
        break;

    case BCO:
            disassemble( (StgBCO*)obj );
            break;

    case AP:
        {
            StgAP* ap = (StgAP*)obj;
            StgWord i;
            debugBelch("AP("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->n_args; ++i) {
                debugBelch(", ");
                printPtr((P_)ap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case PAP:
        {
            StgPAP* pap = (StgPAP*)obj;
            StgWord i;
            debugBelch("PAP/%d(",(int)pap->arity);
            printPtr((StgPtr)pap->fun);
            for (i = 0; i < pap->n_args; ++i) {
                debugBelch(", ");
                printPtr((StgPtr)pap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case AP_STACK:
        {
            StgAP_STACK* ap = (StgAP_STACK*)obj;
            StgWord i;
            debugBelch("AP_STACK("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->size; ++i) {
                debugBelch(", ");
                printPtr((P_)ap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case IND:
            debugBelch("IND(");
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n");
            break;

    case IND_STATIC:
            debugBelch("IND_STATIC(");
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n");
            break;

    case BLACKHOLE:
            debugBelch("BLACKHOLE(");
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n");
            break;

    /* Cannot happen -- use default case.
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case RET_FUN:
    */

    case UPDATE_FRAME:
        {
            StgUpdateFrame* frame = (StgUpdateFrame*)obj;
            debugBelch("%s(", info_update_frame(obj));
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(",");
            printPtr((StgPtr)frame->updatee);
            debugBelch(")\n");
            break;
        }

    case CATCH_FRAME:
        {
            StgCatchFrame* frame = (StgCatchFrame*)obj;
            debugBelch("CATCH_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(",");
            printPtr((StgPtr)frame->handler);
            debugBelch(")\n");
            break;
        }

    case UNDERFLOW_FRAME:
        {
            StgUnderflowFrame* frame = (StgUnderflowFrame*)obj;
            debugBelch("UNDERFLOW_FRAME(");
            printPtr((StgPtr)frame->next_chunk);
            debugBelch(")\n");
            break;
        }

    case STOP_FRAME:
        {
            StgStopFrame* frame = (StgStopFrame*)obj;
            debugBelch("STOP_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(")\n");
            break;
        }

    case ATOMICALLY_FRAME:
        {
            StgAtomicallyFrame* frame = (StgAtomicallyFrame*)obj;
            debugBelch("ATOMICALLY_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(",");
            printPtr((StgPtr)frame->code);
            debugBelch(",");
            printPtr((StgPtr)frame->result);
            debugBelch(")\n");
            break;
        }

    case CATCH_RETRY_FRAME:
        {
            StgCatchRetryFrame* frame = (StgCatchRetryFrame*)obj;
            debugBelch("CATCH_RETRY_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(",");
            printPtr((StgPtr)frame->first_code);
            debugBelch(",");
            printPtr((StgPtr)frame->alt_code);
            debugBelch(")\n");
            break;
        }

    case CATCH_STM_FRAME:
        {
            StgCatchSTMFrame* frame = (StgCatchSTMFrame*)obj;
            debugBelch("CATCH_STM_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)frame));
            debugBelch(",");
            printPtr((StgPtr)frame->code);
            debugBelch(",");
            printPtr((StgPtr)frame->handler);
            debugBelch(")\n");
            break;
        }

    case ARR_WORDS:
        {
            StgWord i;
            debugBelch("ARR_WORDS(\"");
            for (i=0; i<arr_words_words((StgArrBytes *)obj); i++)
              debugBelch("%" FMT_Word, (W_)((StgArrBytes *)obj)->payload[i]);
            debugBelch("\")\n");
            break;
        }

    case MUT_ARR_PTRS_CLEAN:
        debugBelch("MUT_ARR_PTRS_CLEAN(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
        break;

    case MUT_ARR_PTRS_DIRTY:
        debugBelch("MUT_ARR_PTRS_DIRTY(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
        break;

    case MUT_ARR_PTRS_FROZEN_CLEAN:
        debugBelch("MUT_ARR_PTRS_FROZEN_CLEAN(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
        break;

    case MUT_ARR_PTRS_FROZEN_DIRTY:
        debugBelch("MUT_ARR_PTRS_FROZEN_DIRTY(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
        break;

    case SMALL_MUT_ARR_PTRS_CLEAN:
        debugBelch("SMALL_MUT_ARR_PTRS_CLEAN(size=%" FMT_Word ")\n",
                   (W_)((StgSmallMutArrPtrs *)obj)->ptrs);
        break;

    case SMALL_MUT_ARR_PTRS_DIRTY:
        debugBelch("SMALL_MUT_ARR_PTRS_DIRTY(size=%" FMT_Word ")\n",
                   (W_)((StgSmallMutArrPtrs *)obj)->ptrs);
        break;

    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
        debugBelch("SMALL_MUT_ARR_PTRS_FROZEN_CLEAN(size=%" FMT_Word ")\n",
                   (W_)((StgSmallMutArrPtrs *)obj)->ptrs);
        break;

    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        debugBelch("SMALL_MUT_ARR_PTRS_FROZEN_DIRTY(size=%" FMT_Word ")\n",
                   (W_)((StgSmallMutArrPtrs *)obj)->ptrs);
        break;

    case MVAR_CLEAN:
    case MVAR_DIRTY:
        {
          StgMVar* mv = (StgMVar*)obj;

          debugBelch("MVAR(head=");
          if ((StgClosure*)mv->head == &stg_END_TSO_QUEUE_closure) {
              debugBelch("END_TSO_QUEUE");
          } else {
              debugBelch("%p", mv->head);
          }

          debugBelch(", tail=");
          if ((StgClosure*)mv->tail == &stg_END_TSO_QUEUE_closure) {
              debugBelch("END_TSO_QUEUE");
          } else {
              debugBelch("%p", mv->tail);
          }

          debugBelch(", value=");
          if ((StgClosure*)mv->value == &stg_END_TSO_QUEUE_closure) {
              debugBelch("END_TSO_QUEUE");
          } else {
              debugBelch("%p", mv->value);
          }
          debugBelch(")\n");

          break;
        }

    case TVAR:
        {
          StgTVar* tv = (StgTVar*)obj;
          debugBelch("TVAR(value=%p, wq=%p, num_updates=%" FMT_Word ")\n", tv->current_value, tv->first_watch_queue_entry, tv->num_updates);
          break;
        }

    case MUT_VAR_CLEAN:
        {
          StgMutVar* mv = (StgMutVar*)obj;
          debugBelch("MUT_VAR_CLEAN(var=%p)\n", mv->var);
          break;
        }

    case MUT_VAR_DIRTY:
        {
          StgMutVar* mv = (StgMutVar*)obj;
          debugBelch("MUT_VAR_DIRTY(var=%p)\n", mv->var);
          break;
        }

    case WEAK:
            debugBelch("WEAK(");
            debugBelch("key=%p value=%p finalizer=%p",
                    (StgPtr)(((StgWeak*)obj)->key),
                    (StgPtr)(((StgWeak*)obj)->value),
                    (StgPtr)(((StgWeak*)obj)->finalizer));
            debugBelch(")\n");
            /* ToDo: chase 'link' ? */
            break;

    case TSO:
      debugBelch("TSO(");
      debugBelch("%lu (%p)",(unsigned long)(((StgTSO*)obj)->id), (StgTSO*)obj);
      debugBelch(")\n");
      break;

    case STACK:
      debugBelch("STACK\n");
      break;

#if 0
      /* Symptomatic of a problem elsewhere, have it fall-through & fail */
    case EVACUATED:
      debugBelch("EVACUATED(");
      printClosure((StgEvacuated*)obj->evacuee);
      debugBelch(")\n");
      break;
#endif

    case COMPACT_NFDATA:
        debugBelch("COMPACT_NFDATA(size=%" FMT_Word ")\n",
                   (W_)((StgCompactNFData *)obj)->totalW * (W_)sizeof(W_));
        break;

    case TREC_CHUNK:
        debugBelch("TREC_CHUNK\n");
        break;

    case CONTINUATION:
    {
        StgContinuation *u = (StgContinuation *)obj;
        debugBelch("CONTINUATION(apply_mask_frame=");
        printPtr((StgPtr)u->apply_mask_frame);
        debugBelch(",stack_size=%" FMT_Word ")\n", u->stack_size);
        break;
    }

    default:
            //barf("printClosure %d",get_itbl(obj)->type);
            debugBelch("*** printClosure: unknown type %d ****\n",
                    (int)get_itbl(obj)->type );
            barf("printClosure %d",get_itbl(obj)->type);
            return;
    }
}

void
printMutableList(bdescr *bd)
{
    StgPtr p;

    debugBelch("mutable list %p: ", bd);

    for (; bd != NULL; bd = bd->link) {
        for (p = bd->start; p < bd->free; p++) {
            debugBelch("%p (%s), ", (void *)*p, info_type((StgClosure *)*p));
        }
    }
    debugBelch("\n");
}

// If you know you have an UPDATE_FRAME, but want to know exactly which.
const char *info_update_frame(const StgClosure *closure)
{
    // Note: We intentionally don't take the info table pointer as
    // an argument. As it will be confusing whether one should pass
    // it pointing to the code or struct members when compiling with
    // TABLES_NEXT_TO_CODE.
    const StgInfoTable *info = closure->header.info;
    if (info == &stg_upd_frame_info) {
        return "NORMAL_UPDATE_FRAME";
    } else if (info == &stg_bh_upd_frame_info) {
        return "BH_UPDATE_FRAME";
    } else if (info == &stg_marked_upd_frame_info) {
        return "MARKED_UPDATE_FRAME";
    } else {
        return "ERROR: Not an update frame!!!";
    }
}

static void
printSmallBitmap( StgPtr spBottom, StgPtr payload, StgWord bitmap,
                    uint32_t size )
{
    uint32_t i;

    for(i = 0; i < size; i++, bitmap >>= 1 ) {
        debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-(payload+i)), payload+i);
        if ((bitmap & 1) == 0) {
            printPtr((P_)payload[i]);
            debugBelch(" -- ");
            printObj((StgClosure*) payload[i]);
        } else {
            debugBelch("Word# %" FMT_Word "\n", (W_)payload[i]);
        }
    }
}

static void
printLargeBitmap( StgPtr spBottom, StgPtr payload, StgLargeBitmap* large_bitmap,
                    uint32_t size )
{
    StgWord bmp;
    uint32_t i, j;

    i = 0;
    for (bmp=0; i < size; bmp++) {
        StgWord bitmap = large_bitmap->bitmap[bmp];
        j = 0;
        for(; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1 ) {
            debugBelch("   stk[%" FMT_Word "] (%p) = ", (W_)(spBottom-(payload+i)), payload+i);
            if ((bitmap & 1) == 0) {
                printPtr((P_)payload[i]);
                debugBelch(" -- ");
                printObj((StgClosure*) payload[i]);
            } else {
                debugBelch("Word# %" FMT_Word "\n", (W_)payload[i]);
            }
        }
    }
}

void
printStackChunk( StgPtr sp, StgPtr spBottom )
{
    const StgInfoTable *info;

    ASSERT(sp <= spBottom);
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {

        info = get_itbl((StgClosure *)sp);

        switch (info->type) {

        case UPDATE_FRAME:
        case CATCH_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case ATOMICALLY_FRAME:
        case CATCH_RETRY_FRAME:
        case CATCH_STM_FRAME:
            printClosure((StgClosure*)sp);
            continue;

        case RET_SMALL: {
            StgWord c = *sp;
            if (c == (StgWord)&stg_ap_v_info) {
                debugBelch("stg_ap_v_info\n" );
            } else if (c == (StgWord)&stg_ap_f_info) {
                debugBelch("stg_ap_f_info\n" );
            } else if (c == (StgWord)&stg_ap_d_info) {
                debugBelch("stg_ap_d_info\n" );
            } else if (c == (StgWord)&stg_ap_l_info) {
                debugBelch("stg_ap_l_info\n" );
            } else if (c == (StgWord)&stg_ap_n_info) {
                debugBelch("stg_ap_n_info\n" );
            } else if (c == (StgWord)&stg_ap_p_info) {
                debugBelch("stg_ap_p_info\n" );
            } else if (c == (StgWord)&stg_ap_pp_info) {
                debugBelch("stg_ap_pp_info\n" );
            } else if (c == (StgWord)&stg_ap_ppp_info) {
                debugBelch("stg_ap_ppp_info\n" );
            } else if (c == (StgWord)&stg_ap_pppp_info) {
                debugBelch("stg_ap_pppp_info\n" );
            } else if (c == (StgWord)&stg_ap_ppppp_info) {
                debugBelch("stg_ap_ppppp_info\n" );
            } else if (c == (StgWord)&stg_ap_pppppp_info) {
                debugBelch("stg_ap_pppppp_info\n" );
            } else if (c == (StgWord)&stg_ret_v_info) {
                debugBelch("stg_ret_v_info\n" );
            } else if (c == (StgWord)&stg_ret_p_info) {
                debugBelch("stg_ret_p_info\n" );
            } else if (c == (StgWord)&stg_ret_n_info) {
                debugBelch("stg_ret_n_info\n" );
            } else if (c == (StgWord)&stg_ret_f_info) {
                debugBelch("stg_ret_f_info\n" );
            } else if (c == (StgWord)&stg_ret_d_info) {
                debugBelch("stg_ret_d_info\n" );
            } else if (c == (StgWord)&stg_ret_l_info) {
                debugBelch("stg_ret_l_info\n" );
            } else if (c == (StgWord)&stg_prompt_frame_info) {
                debugBelch("stg_prompt_frame_info\n");
#if defined(PROFILING)
            } else if (c == (StgWord)&stg_restore_cccs_d_info) {
                debugBelch("stg_restore_cccs_d_info\n" );
                fprintCCS(stderr, (CostCentreStack*)sp[1]);
                debugBelch("\n" );
                continue;
            } else if (c == (StgWord)&stg_restore_cccs_v16_info) {
                debugBelch("stg_restore_cccs_v16_info\n" );
                fprintCCS(stderr, (CostCentreStack*)sp[1]);
                debugBelch("\n" );
                continue;
            } else if (c == (StgWord)&stg_restore_cccs_v32_info) {
                debugBelch("stg_restore_cccs_v32_info\n" );
                fprintCCS(stderr, (CostCentreStack*)sp[1]);
                debugBelch("\n" );
                continue;
            } else if (c == (StgWord)&stg_restore_cccs_v64_info) {
                debugBelch("stg_restore_cccs_v64_info\n" );
                fprintCCS(stderr, (CostCentreStack*)sp[1]);
                debugBelch("\n" );
                continue;
            } else if (c == (StgWord)&stg_restore_cccs_eval_info) {
                debugBelch("stg_restore_cccs_eval_info\n" );
                fprintCCS(stderr, (CostCentreStack*)sp[1]);
                debugBelch("\n" );
                continue;
#endif
            } else {
                debugBelch("RET_SMALL (%p)\n", info);
            }
            StgWord bitmap = info->layout.bitmap;
            printSmallBitmap(spBottom, sp+1,
                             BITMAP_BITS(bitmap), BITMAP_SIZE(bitmap));
            continue;
        }

        case RET_BCO: {
            StgWord c = *sp;
            StgBCO *bco = ((StgBCO *)sp[1]);

            if (c == (StgWord)&stg_ctoi_R1p_info) {
                debugBelch("stg_ctoi_R1p_info" );
            } else if (c == (StgWord)&stg_ctoi_R1n_info) {
                debugBelch("stg_ctoi_R1n_info" );
            } else if (c == (StgWord)&stg_ctoi_F1_info) {
                debugBelch("stg_ctoi_F1_info" );
            } else if (c == (StgWord)&stg_ctoi_D1_info) {
                debugBelch("stg_ctoi_D1_info" );
            } else if (c == (StgWord)&stg_ctoi_V_info) {
                debugBelch("stg_ctoi_V_info" );
            } else if (c == (StgWord)&stg_BCO_info) {
                debugBelch("stg_BCO_info" );
            } else if (c == (StgWord)&stg_CASE_CONT_BCO_info) {
                debugBelch("stg_CASE_CONT_BCO_info" );
            } else if (c == (StgWord)&stg_apply_interp_info) {
                debugBelch("stg_apply_interp_info" );
            } else if (c == (StgWord)&stg_ret_t_info) {
                debugBelch("stg_ret_t_info" );
            } else if (c == (StgWord)&stg_ctoi_t0_info) {
                debugBelch("stg_ctoi_t0_info" );
            } else if (c == (StgWord)&stg_ctoi_t1_info) {
                debugBelch("stg_ctoi_t1_info" );
            } else if (c == (StgWord)&stg_ctoi_t2_info) {
                debugBelch("stg_ctoi_t2_info" );
            } else if (c == (StgWord)&stg_ctoi_t3_info) {
                debugBelch("stg_ctoi_t3_info" );
            } else if (c == (StgWord)&stg_ctoi_t4_info) {
                debugBelch("stg_ctoi_t4_info" );
            } else if (c == (StgWord)&stg_ctoi_t5_info) {
                debugBelch("stg_ctoi_t5_info" );
            } else if (c == (StgWord)&stg_ctoi_t6_info) {
                debugBelch("stg_ctoi_t6_info" );
            } else if (c == (StgWord)&stg_ctoi_t7_info) {
                debugBelch("stg_ctoi_t7_info" );
            } else if (c == (StgWord)&stg_ctoi_t8_info) {
                debugBelch("stg_ctoi_t8_info" );
            /* there are more stg_ctoi_tN_info frames,
               but we don't print them all */
            } else {
                debugBelch("RET_BCO");
            }
            debugBelch(" (%p)\n", sp);
            printLargeBitmap(spBottom, sp+2,
                             BCO_BITMAP(bco), BCO_BITMAP_SIZE(bco));
            continue;
        }

        case RET_BIG:
            debugBelch("RET_BIG (%p)\n", sp);
            StgLargeBitmap* bitmap = GET_LARGE_BITMAP(info);
            printLargeBitmap(spBottom,
                            (StgPtr)((StgClosure *) sp)->payload,
                            bitmap,
                            bitmap->size);
            continue;
        case RET_FUN:
        {
            const StgFunInfoTable *fun_info;
            StgRetFun *ret_fun;

            ret_fun = (StgRetFun *)sp;
            fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            debugBelch("RET_FUN (%p) (type=%d)\n", ret_fun->fun, (int)fun_info->f.fun_type);
            switch (fun_info->f.fun_type) {
            case ARG_GEN:
                printSmallBitmap(spBottom, (StgPtr) &ret_fun->payload,
                                 BITMAP_BITS(fun_info->f.b.bitmap),
                                 BITMAP_SIZE(fun_info->f.b.bitmap));
                break;
            case ARG_GEN_BIG:
                printLargeBitmap(spBottom, (StgPtr) &ret_fun->payload,
                                 GET_FUN_LARGE_BITMAP(fun_info),
                                 GET_FUN_LARGE_BITMAP(fun_info)->size);
                break;
            default:
                printSmallBitmap(spBottom, (StgPtr) &ret_fun->payload,
                                 BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
                                 BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]));
                break;
            }
            continue;
        }

        default:
            debugBelch("unknown object %d\n", (int)info->type);
            barf("printStackChunk");
        }
    }
}

void printStack( StgStack *stack )
{
    printStackChunk( stack->sp, stack->stack + stack->stack_size );
}

void printTSO( StgTSO *tso )
{
    printStack( tso->stackobj );
}

void printStaticObjects( StgClosure *p )
{
    while (p != END_OF_STATIC_OBJECT_LIST) {
        p = UNTAG_STATIC_LIST_PTR(p);
        printClosure(p);

        const StgInfoTable *info = get_itbl(p);
        p = *STATIC_LINK(info, p);
    }
}

void printWeakLists(void)
{
    debugBelch("======= WEAK LISTS =======\n");

    for (uint32_t cap_idx = 0; cap_idx < getNumCapabilities(); ++cap_idx) {
        debugBelch("Capability %d:\n", cap_idx);
        Capability *cap = getCapability(cap_idx);
        for (StgWeak *weak = cap->weak_ptr_list_hd; weak; weak = weak->link) {
            printClosure((StgClosure*)weak);
        }
    }

    for (uint32_t gen_idx = 0; gen_idx <= oldest_gen->no; ++gen_idx) {
        generation *gen = &generations[gen_idx];
        debugBelch("Generation %d current weaks:\n", gen_idx);
        for (StgWeak *weak = gen->weak_ptr_list; weak; weak = weak->link) {
            printClosure((StgClosure*)weak);
        }
        debugBelch("Generation %d old weaks:\n", gen_idx);
        for (StgWeak *weak = gen->old_weak_ptr_list; weak; weak = weak->link) {
            printClosure((StgClosure*)weak);
        }
    }

    debugBelch("=========================\n");
}

void printLargeAndPinnedObjects(void)
{
    debugBelch("====== PINNED OBJECTS ======\n");

    for (uint32_t cap_idx = 0; cap_idx < getNumCapabilities(); ++cap_idx) {
        Capability *cap = getCapability(cap_idx);

        debugBelch("Capability %d: Current pinned object block: %p\n",
                   cap_idx, (void*)cap->pinned_object_block);
        for (bdescr *bd = cap->pinned_object_blocks; bd; bd = bd->link) {
            debugBelch("%p\n", (void*)bd);
        }
    }

    debugBelch("====== LARGE OBJECTS =======\n");
    for (uint32_t gen_idx = 0; gen_idx <= oldest_gen->no; ++gen_idx) {
        generation *gen = &generations[gen_idx];
        debugBelch("Generation %d current large objects:\n", gen_idx);
        for (bdescr *bd = gen->large_objects; bd; bd = bd->link) {
            debugBelch("%p: ", (void*)bd);
            printClosure((StgClosure*)bd->start);
        }

        debugBelch("Generation %d scavenged large objects:\n", gen_idx);
        for (bdescr *bd = gen->scavenged_large_objects; bd; bd = bd->link) {
            debugBelch("%p: ", (void*)bd);
            printClosure((StgClosure*)bd->start);
        }
    }

    debugBelch("============================\n");
}

/* --------------------------------------------------------------------------
 * Address printing code
 *
 * Uses symbol table in (unstripped executable)
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Simple lookup table
 * address -> function name
 * ------------------------------------------------------------------------*/

static HashTable * add_to_fname_table = NULL;

const char *lookupGHCName( void *addr )
{
    if (add_to_fname_table == NULL)
        return NULL;

    return lookupHashTable(add_to_fname_table, (StgWord)addr);
}

/* --------------------------------------------------------------------------
 * Symbol table loading
 * ------------------------------------------------------------------------*/

/* Causing linking trouble on Win32 plats, so I'm
   disabling this for now.
*/
#if defined(USING_LIBBFD)
#    define PACKAGE 1
#    define PACKAGE_VERSION 1
/* Those PACKAGE_* defines are workarounds for bfd:
 *     https://sourceware.org/bugzilla/show_bug.cgi?id=14243
 * ghc's build system filter PACKAGE_* values out specifically to avoid clashes
 * with user's autoconf-based Cabal packages.
 * It's a shame <bfd.h> checks for unrelated fields instead of actually used
 * macros.
 */
#    include <bfd.h>

/* Fairly ad-hoc piece of code that seems to filter out a lot of
 * rubbish like the obj-splitting symbols
 */

static bool isReal( flagword flags STG_UNUSED, const char *name )
{
#if 0
    /* ToDo: make this work on BFD */
    int tp = type & N_TYPE;
    if (tp == N_TEXT || tp == N_DATA) {
        return (name[0] == '_' && name[1] != '_');
    } else {
        return false;
    }
#else
    if (*name == '\0'  ||
        (name[0] == 'g' && name[1] == 'c' && name[2] == 'c') ||
        (name[0] == 'c' && name[1] == 'c' && name[2] == '.')) {
        return false;
    }
    return true;
#endif
}

extern void DEBUG_LoadSymbols( const char *name )
{
    bfd* abfd;
    char **matching;

    bfd_init();
    abfd = bfd_openr(name, "default");
    if (abfd == NULL) {
        barf("can't open executable %s to get symbol table", name);
    }
    if (!bfd_check_format_matches (abfd, bfd_object, &matching)) {
        barf("mismatch");
    }

    {
        long storage_needed;
        asymbol **symbol_table;
        long number_of_symbols;
        long num_real_syms = 0;
        long i;

        storage_needed = bfd_get_symtab_upper_bound (abfd);

        if (storage_needed < 0) {
            barf("can't read symbol table");
        }
        symbol_table = (asymbol **) stgMallocBytes(storage_needed,"DEBUG_LoadSymbols");

        number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);

        if (number_of_symbols < 0) {
            barf("can't canonicalise symbol table");
        }

        if (add_to_fname_table == NULL)
            add_to_fname_table = allocHashTable();

        for( i = 0; i != number_of_symbols; ++i ) {
            symbol_info info;
            bfd_get_symbol_info(abfd,symbol_table[i],&info);
            if (isReal(info.type, info.name)) {
                insertHashTable(add_to_fname_table,
                                info.value, (void*)info.name);
                num_real_syms += 1;
            }
        }

        IF_DEBUG(interpreter,
                 debugBelch("Loaded %ld symbols. Of which %ld are real symbols\n",
                         number_of_symbols, num_real_syms)
                 );

        stgFree(symbol_table);
    }
}

#else /* USING_LIBBFD */

extern void DEBUG_LoadSymbols( const char *name STG_UNUSED )
{
  /* nothing, yet */
}

#endif /* USING_LIBBFD */

void findPtr(P_ p, int);                /* keep gcc -Wall happy */

int searched = 0;

static int
findPtrBlocks (StgPtr p, bdescr *bd, StgPtr arr[], int arr_size, int i)
{
    StgPtr q, r, end;
    for (; bd; bd = bd->link) {
        searched++;
        for (q = bd->start; q < bd->free; q++) {
            if (UNTAG_CONST_CLOSURE((StgClosure*)*q) == (const StgClosure *)p) {
                if (i < arr_size) {
                    for (r = bd->start; r < bd->free; r = end) {
                        // skip over zeroed-out slop
                        while (*r == 0) r++;
                        if (!LOOKS_LIKE_CLOSURE_PTR(r)) {
                            debugBelch("%p found at %p, no closure at %p\n",
                                       p, q, r);
                            break;
                        }
                        end = r + closure_sizeW((StgClosure*)r);
                        if (q < end) {
                            debugBelch("%p = ", r);
                            printClosure((StgClosure *)r);
                            arr[i++] = r;
                            break;
                        }
                    }
                    if (r >= bd->free) {
                        debugBelch("%p found at %p, closure?", p, q);
                    }
                } else {
                    return i;
                }
            }
        }
    }
    return i;
}

void
findPtr(P_ p, int follow)
{
  uint32_t g, n;
  bdescr *bd;
  const int arr_size = 1024;
  StgPtr arr[arr_size];
  int i = 0;
  searched = 0;

#if 0
  // We can't search the nursery, because we don't know which blocks contain
  // valid data, because the bd->free pointers in the nursery are only reset
  // just before a block is used.
  for (n = 0; n < getNumCapabilities(); n++) {
      bd = nurseries[i].blocks;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      if (i >= arr_size) return;
  }
#endif

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      bd = generations[g].blocks;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      bd = generations[g].large_objects;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      if (i >= arr_size) return;
      for (n = 0; n < getNumCapabilities(); n++) {
          i = findPtrBlocks(p, gc_threads[n]->gens[g].part_list,
                            arr, arr_size, i);
          i = findPtrBlocks(p, gc_threads[n]->gens[g].todo_bd,
                            arr, arr_size, i);
      }
      if (i >= arr_size) return;
  }
  if (follow && i == 1) {
      debugBelch("-->\n");
      findPtr(arr[0], 1);
  }
}

const char *what_next_strs[] = {
  [0]               = "(unknown)",
  [ThreadRunGHC]    = "ThreadRunGHC",
  [ThreadInterpret] = "ThreadInterpret",
  [ThreadKilled]    = "ThreadKilled",
  [ThreadComplete]  = "ThreadComplete"
};

#else /* DEBUG */
void printPtr( StgPtr p )
{
    debugBelch("ptr 0x%p (enable -DDEBUG for more info) " , p );
}

void printObj( StgClosure *obj )
{
    debugBelch("obj 0x%p (enable -DDEBUG for more info) " , obj );
}


#endif /* DEBUG */

/* -----------------------------------------------------------------------------
   Closure types

   NOTE: must be kept in sync with the closure types in
   rts/include/rts/storage/ClosureTypes.h
   -------------------------------------------------------------------------- */

const char *closure_type_names[] = {
 [INVALID_OBJECT]        = "INVALID_OBJECT",
 [CONSTR]                = "CONSTR",
 [CONSTR_1_0]            = "CONSTR_1_0",
 [CONSTR_0_1]            = "CONSTR_0_1",
 [CONSTR_2_0]            = "CONSTR_2_0",
 [CONSTR_1_1]            = "CONSTR_1_1",
 [CONSTR_0_2]            = "CONSTR_0_2",
 [CONSTR_NOCAF]          = "CONSTR_NOCAF",
 [FUN]                   = "FUN",
 [FUN_1_0]               = "FUN_1_0",
 [FUN_0_1]               = "FUN_0_1",
 [FUN_2_0]               = "FUN_2_0",
 [FUN_1_1]               = "FUN_1_1",
 [FUN_0_2]               = "FUN_0_2",
 [FUN_STATIC]            = "FUN_STATIC",
 [THUNK]                 = "THUNK",
 [THUNK_1_0]             = "THUNK_1_0",
 [THUNK_0_1]             = "THUNK_0_1",
 [THUNK_2_0]             = "THUNK_2_0",
 [THUNK_1_1]             = "THUNK_1_1",
 [THUNK_0_2]             = "THUNK_0_2",
 [THUNK_STATIC]          = "THUNK_STATIC",
 [THUNK_SELECTOR]        = "THUNK_SELECTOR",
 [BCO]                   = "BCO",
 [AP]                    = "AP",
 [PAP]                   = "PAP",
 [AP_STACK]              = "AP_STACK",
 [IND]                   = "IND",
 [IND_STATIC]            = "IND_STATIC",
 [RET_BCO]               = "RET_BCO",
 [RET_SMALL]             = "RET_SMALL",
 [RET_BIG]               = "RET_BIG",
 [RET_FUN]               = "RET_FUN",
 [UPDATE_FRAME]          = "UPDATE_FRAME",
 [CATCH_FRAME]           = "CATCH_FRAME",
 [UNDERFLOW_FRAME]       = "UNDERFLOW_FRAME",
 [STOP_FRAME]            = "STOP_FRAME",
 [BLOCKING_QUEUE]        = "BLOCKING_QUEUE",
 [BLACKHOLE]             = "BLACKHOLE",
 [MVAR_CLEAN]            = "MVAR_CLEAN",
 [MVAR_DIRTY]            = "MVAR_DIRTY",
 [TVAR]                  = "TVAR",
 [ARR_WORDS]             = "ARR_WORDS",
 [MUT_ARR_PTRS_CLEAN]    = "MUT_ARR_PTRS_CLEAN",
 [MUT_ARR_PTRS_DIRTY]    = "MUT_ARR_PTRS_DIRTY",
 [MUT_ARR_PTRS_FROZEN_DIRTY]  = "MUT_ARR_PTRS_FROZEN_DIRTY",
 [MUT_ARR_PTRS_FROZEN_CLEAN]   = "MUT_ARR_PTRS_FROZEN_CLEAN",
 [MUT_VAR_CLEAN]         = "MUT_VAR_CLEAN",
 [MUT_VAR_DIRTY]         = "MUT_VAR_DIRTY",
 [WEAK]                  = "WEAK",
 [PRIM]                  = "PRIM",
 [MUT_PRIM]              = "MUT_PRIM",
 [TSO]                   = "TSO",
 [STACK]                 = "STACK",
 [TREC_CHUNK]            = "TREC_CHUNK",
 [ATOMICALLY_FRAME]      = "ATOMICALLY_FRAME",
 [CATCH_RETRY_FRAME]     = "CATCH_RETRY_FRAME",
 [CATCH_STM_FRAME]       = "CATCH_STM_FRAME",
 [WHITEHOLE]             = "WHITEHOLE",
 [SMALL_MUT_ARR_PTRS_CLEAN] = "SMALL_MUT_ARR_PTRS_CLEAN",
 [SMALL_MUT_ARR_PTRS_DIRTY] = "SMALL_MUT_ARR_PTRS_DIRTY",
 [SMALL_MUT_ARR_PTRS_FROZEN_DIRTY] = "SMALL_MUT_ARR_PTRS_FROZEN_DIRTY",
 [SMALL_MUT_ARR_PTRS_FROZEN_CLEAN] = "SMALL_MUT_ARR_PTRS_FROZEN_CLEAN",
 [COMPACT_NFDATA]        = "COMPACT_NFDATA",
 [CONTINUATION]          = "CONTINUATION",
};

#if N_CLOSURE_TYPES != 65
#error Closure types changed: update Printer.c!
#endif

const char *
info_type(const StgClosure *closure){
  return closure_type_names[get_itbl(closure)->type];
}

const char *
info_type_by_ip(const StgInfoTable *ip){
  return closure_type_names[ip->type];
}

void
info_hdr_type(const StgClosure *closure, char *res){
  strcpy(res,closure_type_names[get_itbl(closure)->type]);
}
