
/* -----------------------------------------------------------------------------
 * Bytecode evaluator
 *
 * Copyright (c) 1994-2000.
 *
 * $RCSfile: Interpreter.c,v $
 * $Revision: 1.23 $
 * $Date: 2001/05/25 18:32:51 $
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "Closures.h"
#include "TSO.h"
#include "Schedule.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "Updates.h"

#include "Bytecodes.h"
#include "Printer.h"
#include "Disassembler.h"
#include "Interpreter.h"


/* --------------------------------------------------------------------------
 * The new bytecode interpreter
 * ------------------------------------------------------------------------*/

/* The interpreter can be compiled so it just interprets BCOs and
   hands literally everything else to the scheduler.  This gives a
   "reference interpreter" which is correct but slow -- useful for
   debugging.  By default, we handle certain closures specially so as
   to dramatically cut down on the number of deferrals to the
   scheduler.  Ie normally you don't want REFERENCE_INTERPRETER to be
   defined. */

/* #define REFERENCE_INTERPRETER */

/* Gather stats about entry, opcode, opcode-pair frequencies.  For
   tuning the interpreter. */

/* #define INTERP_STATS */



/* iSp points to the lowest live word on the stack. */

#define StackWord(n)  iSp[n]
#define BCO_NEXT      instrs[bciPtr++]
#define BCO_PTR(n)    (W_)ptrs[n]
#define BCO_LIT(n)    (W_)literals[n]
#define BCO_ITBL(n)   itbls[n]

#define LOAD_STACK_POINTERS \
    iSp = cap->rCurrentTSO->sp; iSu = cap->rCurrentTSO->su;

#define SAVE_STACK_POINTERS \
    cap->rCurrentTSO->sp = iSp; cap->rCurrentTSO->su = iSu;

#define RETURN(retcode) \
   SAVE_STACK_POINTERS; return retcode;


static __inline__ StgPtr allocate_UPD ( int n_words )
{
   if (n_words - sizeofW(StgHeader) < MIN_UPD_SIZE)
      n_words = MIN_UPD_SIZE + sizeofW(StgHeader);
   return allocate(n_words);
}

static __inline__ StgPtr allocate_NONUPD ( int n_words )
{
   if (n_words - sizeofW(StgHeader) < MIN_NONUPD_SIZE)
      n_words = MIN_NONUPD_SIZE + sizeofW(StgHeader);
   return allocate(n_words);
}


#ifdef INTERP_STATS
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
   fprintf(stderr, "%d constrs entered -> (%d BCO, %d UPD, %d ??)\n",
                   it_retto_BCO + it_retto_UPDATE + it_retto_other,
                   it_retto_BCO, it_retto_UPDATE, it_retto_other );
   fprintf(stderr, "%d total entries, %d unknown entries \n", 
                   it_total_entries, it_total_unknown_entries);
   for (i = 0; i < N_CLOSURE_TYPES; i++) {
     if (it_unknown_entries[i] == 0) continue;
     fprintf(stderr, "   type %2d: unknown entries (%4.1f%%) == %d\n",
	     i, 100.0 * ((double)it_unknown_entries[i]) / 
                        ((double)it_total_unknown_entries),
             it_unknown_entries[i]);
   }
   fprintf(stderr, "%d insns, %d slides, %d BCO_entries\n", 
                   it_insns, it_slides, it_BCO_entries);
   for (i = 0; i < 27; i++) 
      fprintf(stderr, "opcode %2d got %d\n", i, it_ofreq[i] );

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
      
      fprintf ( stderr, "%d:  count (%4.1f%%) %6d   is %d then %d\n",
                k, ((double)o_max) * 100.0 / ((double)it_insns), o_max,
                   i_max, j_max );
      it_oofreq[i_max][j_max] = 0;

   }
}
#endif


StgThreadReturnCode interpretBCO ( Capability* cap )
{
   /* On entry, the closure to interpret is on the top of the
      stack. */
 
   /* Use of register here is primarily to make it clear to compilers
      that these entities are non-aliasable.
   */
    register W_*              iSp;    /* local state -- stack pointer */
    register StgUpdateFrame*  iSu;    /* local state -- frame pointer */
    register StgPtr           iSpLim; /* local state -- stack lim pointer */
    register StgClosure*      obj;

    LOAD_STACK_POINTERS;

    /* We don't change this ... */
    iSpLim = cap->rCurrentTSO->stack + RESERVED_STACK_WORDS;

    /* Main object-entering loop.  Object to be entered is on top of
       stack. */
    nextEnter:

    obj = (StgClosure*)StackWord(0); iSp++;

    nextEnter_obj:

#   ifdef INTERP_STATS
    it_total_entries++;
#   endif

    IF_DEBUG(evaluator,
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"Entering: "); printObj(obj);
             fprintf(stderr,"iSp = %p\tiSu = %p\n", iSp, iSu);
             fprintf(stderr, "\n" );

	     //	     checkSanity(1);
	     //             iSp--; StackWord(0) = obj;
	     //             checkStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
	     //             iSp++;

             printStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
             fprintf(stderr, "\n\n");
            );



    switch ( get_itbl(obj)->type ) {

       case INVALID_OBJECT:
               barf("Invalid object %p",(StgPtr)obj);

#      ifndef REFERENCE_INTERPRETER

       case IND:
       case IND_OLDGEN:
       case IND_PERM:
       case IND_OLDGEN_PERM:
       case IND_STATIC:
       { 
          obj = ((StgInd*)obj)->indirectee;
          goto nextEnter_obj;
       }

       case CONSTR:
       case CONSTR_1_0:
       case CONSTR_0_1:
       case CONSTR_2_0:
       case CONSTR_1_1:
       case CONSTR_0_2:
       case CONSTR_INTLIKE:
       case CONSTR_CHARLIKE:
       case CONSTR_STATIC:
       case CONSTR_NOCAF_STATIC:
       nextEnter_obj_CONSTR:
       {
          StgInfoTable* ret_itbl = (StgInfoTable*)StackWord(0);
          if (ret_itbl == (StgInfoTable*)&stg_ctoi_ret_R1p_info) {
#            ifdef INTERP_STATS
             it_retto_BCO++;
#            endif
             /* Returning this constr to a BCO.  Push the constr on
                the stack and enter the return continuation BCO, which
                is immediately underneath ret_itbl. */
             StackWord(-1) = (W_)obj;
             obj = (StgClosure*)StackWord(1);
             iSp --;
	     if (get_itbl(obj)->type == BCO) 
                goto nextEnter_obj_BCO; /* fast-track common case */
             else
                goto nextEnter_obj; /* a safe fallback */
	  } else
	  if (ret_itbl == (StgInfoTable*)&stg_upd_frame_info) {
#            ifdef INTERP_STATS
	     it_retto_UPDATE++;
#            endif
             /* Returning this constr to an update frame.  Do the
                update and re-enter the constr. */
             ASSERT((W_*)iSu == iSp);
             UPD_IND(iSu->updatee, obj); 
             iSu = iSu->link;
             iSp += sizeofW(StgUpdateFrame);
             goto nextEnter_obj_CONSTR;
          }
#         ifdef INTERP_STATS
          else it_retto_other++;
#         endif
          goto defer_to_sched;
       }

       case AP_UPD:
       /* Copied from stg_AP_UPD_entry. */
       { 
          nat i, words;
          StgAP_UPD *ap = (StgAP_UPD*)obj;
          words = ap->n_args;

	  /* Stack check.  If a stack overflow might occur, don't enter
             the closure; let the scheduler handle it instead. */
          if (iSp - (words+sizeofW(StgUpdateFrame)) < iSpLim)
             goto defer_to_sched;

	  /* Ok; we're safe.  Party on.  Push an update frame. */
          iSp -= sizeofW(StgUpdateFrame);
          {
              StgUpdateFrame *__frame;
              __frame = (StgUpdateFrame *)iSp;
              SET_INFO(__frame, (StgInfoTable *)&stg_upd_frame_info);
              __frame->link = iSu;
              __frame->updatee = (StgClosure *)(ap);
              iSu = __frame;
          }

          /* Reload the stack */
          iSp -= words;
          for (i=0; i < words; i++) StackWord(i) = (W_)ap->payload[i];

          obj = (StgClosure*)ap->fun;
          goto nextEnter_obj;
       }

       case PAP:
       /* Copied from stg_PAP_entry. */
       {
          nat     words, i;
          StgPAP* pap = (StgPAP *)obj;
  
          /*
           * remove any update frames on the top of the stack, by just
           * performing the update here.
           */
          while ((W_)iSu - (W_)iSp == 0) {

             switch (get_itbl(iSu)->type) {

             case UPDATE_FRAME:
                /* We're sitting on top of an update frame, so let's
                   do the business. */
                UPD_IND(iSu->updatee, pap);
                iSu = iSu->link;
                iSp += sizeofW(StgUpdateFrame);
                continue;

             case SEQ_FRAME:
                /* Too complicated ... adopt the Usual Solution. */
                /* fprintf(stderr, "!!! SEQ frame in PAP update\n"); */
                goto defer_to_sched;

             case CATCH_FRAME:
                /* can't happen, see stg_update_PAP */
                barf("interpretBCO: PAP_entry: CATCH_FRAME");

             default:
                barf("interpretBCO: PAP_entry: strange activation record");
             }
          }

          words = pap->n_args;

	  /* Stack check.  If a stack overflow might occur, don't enter
             the closure; let the scheduler handle it instead. */
          if (iSp - words < iSpLim)
             goto defer_to_sched;

          /* Ok; safe. */         
          iSp -= words;
          for (i=0; i < words; i++) StackWord(i) = (W_)pap->payload[i];

          obj = (StgClosure*)pap->fun;
          goto nextEnter_obj;
       }

#      endif /* ndef REFERENCE_INTERPRETER */

       case BCO:
       /* ---------------------------------------------------- */
       /* Start of the bytecode interpreter                    */
       /* ---------------------------------------------------- */
       nextEnter_obj_BCO:
#      ifdef INTERP_STATS
       it_BCO_entries++;
#      endif
       {
          int do_print_stack = 1;
          register int       bciPtr     = 1; /* instruction pointer */
          register StgBCO*   bco        = (StgBCO*)obj;
          register UShort*   instrs     = (UShort*)(&bco->instrs->payload[0]);
          register StgWord*  literals   = (StgWord*)(&bco->literals->payload[0]);
          register StgPtr*   ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);
          register StgInfoTable** itbls = (StgInfoTable**)
                                             (&bco->itbls->payload[0]);

          /* Heap check */
          if (doYouWantToGC()) {
	     iSp--; StackWord(0) = (W_)bco;
             cap->rCurrentTSO->what_next = ThreadEnterInterp;
             RETURN(HeapOverflow);
          }

          /* "Standard" stack check */
          if (iSp - (INTERP_STACK_CHECK_THRESH+1) < iSpLim) {
             iSp--;
             StackWord(0) = (W_)obj;
             cap->rCurrentTSO->what_next = ThreadEnterInterp;
             RETURN(StackOverflow);
          }

          /* Context-switch check */
          if (context_switch) {
             iSp--;
             StackWord(0) = (W_)obj;
             cap->rCurrentTSO->what_next = ThreadEnterInterp;
             RETURN(ThreadYielding);
	  }
 

#         ifdef INTERP_STATS
          it_lastopc = 0; /* no opcode */
#         endif

          nextInsn:

          ASSERT(bciPtr <= instrs[0]);
          IF_DEBUG(evaluator,
		   //if (do_print_stack) {
		   //fprintf(stderr, "\n-- BEGIN stack\n");
		   //printStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
		   //fprintf(stderr, "-- END stack\n\n");
		   //}
                   do_print_stack = 1;
		   fprintf(stderr,"iSp = %p   iSu = %p   pc = %d      ", iSp, iSu, bciPtr);
                   disInstr(bco,bciPtr);
                    if (0) { int i;
                             fprintf(stderr,"\n");
                             for (i = 8; i >= 0; i--) 
                                fprintf(stderr, "%d  %p\n", i, (StgPtr)(*(iSp+i)));
                             fprintf(stderr,"\n");
                           }
		    //if (do_print_stack) checkStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
                  );

#         ifdef INTERP_STATS
          it_insns++;
          ASSERT( (int)instrs[bciPtr] >= 0 && (int)instrs[bciPtr] < 27 );
          it_ofreq[ (int)instrs[bciPtr] ] ++;
          it_oofreq[ it_lastopc ][ (int)instrs[bciPtr] ] ++;
          it_lastopc = (int)instrs[bciPtr];
#         endif

          switch (BCO_NEXT) {

              case bci_STKCHECK: {
		/* An explicit stack check; we hope these will be
                   rare. */
                int stk_words_reqd = BCO_NEXT + 1;
                if (iSp - stk_words_reqd < iSpLim) {
                   iSp--;
                   StackWord(0) = (W_)obj;
                   cap->rCurrentTSO->what_next = ThreadEnterInterp;
                   RETURN(StackOverflow);
                }
                goto nextInsn;
              }
              case bci_ARGCHECK: {
                 int i;
                 StgPAP* pap;
                 int arg_words_reqd = BCO_NEXT;
                 int arg_words_avail = ((W_*)iSu) - ((W_*)iSp);
                 if (arg_words_avail >= arg_words_reqd) goto nextInsn;

#                ifndef REFERENCE_INTERPRETER

                 /* Optimisation: if there are no args avail and the
                    t-o-s is an update frame, do the update, and
                    re-enter the object. */
                 if (arg_words_avail == 0 
                    && get_itbl(iSu)->type == UPDATE_FRAME) {
                    UPD_IND(iSu->updatee, obj); 
                    iSu = iSu->link;
                    iSp += sizeofW(StgUpdateFrame);
                    goto nextEnter_obj_BCO;
		 }

#                endif /* ndef REFERENCE_INTERPRETER */

                 /* Handle arg check failure.  General case: copy the
                    spare args into a PAP frame. */
                 pap = (StgPAP*)allocate_UPD(PAP_sizeW(arg_words_avail));
                 SET_HDR(pap,&stg_PAP_info,CCS_SYSTEM/*ToDo*/);
                 pap->n_args = arg_words_avail;
                 pap->fun = obj;
                 for (i = 0; i < arg_words_avail; i++)
                    pap->payload[i] = (StgClosure*)StackWord(i);

                 /* Push on the stack and defer to the scheduler. */
                 iSp = (StgPtr)iSu;
                 iSp --;
                 StackWord(0) = (W_)pap;
		 IF_DEBUG(evaluator,
                          fprintf(stderr,"\tBuilt "); 
                          printObj((StgClosure*)pap);
		         );
                 cap->rCurrentTSO->what_next = ThreadEnterGHC;
                 RETURN(ThreadYielding);
              }
              case bci_PUSH_L: {
                 int o1 = BCO_NEXT;
                 ASSERT((W_*)iSp+o1 < (W_*)iSu);
                 StackWord(-1) = StackWord(o1);
                 iSp--;
                 do_print_stack = 0;
                 goto nextInsn;
              }
              case bci_PUSH_LL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 ASSERT((W_*)iSp+o1 < (W_*)iSu);
                 ASSERT((W_*)iSp+o2 < (W_*)iSu);
                 StackWord(-1) = StackWord(o1);
                 StackWord(-2) = StackWord(o2);
                 iSp -= 2;
                 goto nextInsn;
              }
              case bci_PUSH_LLL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 int o3 = BCO_NEXT;
                 ASSERT((W_*)iSp+o1 < (W_*)iSu);
                 ASSERT((W_*)iSp+o2 < (W_*)iSu);
                 ASSERT((W_*)iSp+o3 < (W_*)iSu);
                 StackWord(-1) = StackWord(o1);
                 StackWord(-2) = StackWord(o2);
                 StackWord(-3) = StackWord(o3);
                 iSp -= 3;
                 goto nextInsn;
              }
              case bci_PUSH_G: {
                 int o1 = BCO_NEXT;
                 StackWord(-1) = BCO_PTR(o1);
                 iSp -= 1;
                 goto nextInsn;
              }
              case bci_PUSH_AS: {
                 int o_bco  = BCO_NEXT;
                 int o_itbl = BCO_NEXT;
                 StackWord(-2) = BCO_LIT(o_itbl);
                 StackWord(-1) = BCO_PTR(o_bco);
                 iSp -= 2;
                 goto nextInsn;
              }
              case bci_PUSH_UBX: {
                 int i;
                 int o_lits = BCO_NEXT;
                 int n_words = BCO_NEXT;
                 iSp -= n_words;
                 for (i = 0; i < n_words; i++)
                    StackWord(i) = BCO_LIT(o_lits+i);
                 do_print_stack = 0;
                 goto nextInsn;
              }
              case bci_PUSH_TAG: {
                 W_ tag = (W_)(BCO_NEXT);
                 StackWord(-1) = tag;
                 iSp --;
                 goto nextInsn;
              }
              case bci_SLIDE: {
                 int n  = BCO_NEXT;
                 int by = BCO_NEXT;
                 ASSERT((W_*)iSp+n+by <= (W_*)iSu);
                 /* a_1, .. a_n, b_1, .. b_by, s => a_1, .. a_n, s */
                 while(--n >= 0) {
                    StackWord(n+by) = StackWord(n);
                 }
                 iSp += by;
#                ifdef INTERP_STATS
                 it_slides++;
#                endif
                 goto nextInsn;
              }
              case bci_ALLOC: {
                 StgAP_UPD* ap; 
                 int n_payload = BCO_NEXT - 1;
                 int request   = AP_sizeW(n_payload);
                 ap = (StgAP_UPD*)allocate_UPD(request);
                 StackWord(-1) = (W_)ap;
                 ap->n_args = n_payload;
                 SET_HDR(ap, &stg_AP_UPD_info, CCS_SYSTEM/*ToDo*/)
                 iSp --;
                 goto nextInsn;
              }
              case bci_MKAP: {
                 int i;
                 int stkoff = BCO_NEXT;
                 int n_payload = BCO_NEXT - 1;
                 StgAP_UPD* ap = (StgAP_UPD*)StackWord(stkoff);
                 ASSERT((int)ap->n_args == n_payload);
                 ap->fun = (StgClosure*)StackWord(0);
                 for (i = 0; i < n_payload; i++)
                    ap->payload[i] = (StgClosure*)StackWord(i+1);
                 iSp += n_payload+1;
		 IF_DEBUG(evaluator,
                          fprintf(stderr,"\tBuilt "); 
                          printObj((StgClosure*)ap);
		         );
                 goto nextInsn;
              }
              case bci_UNPACK: {
                 /* Unpack N ptr words from t.o.s constructor */
                 /* The common case ! */
                 int i;
                 int n_words = BCO_NEXT;
                 StgClosure* con = (StgClosure*)StackWord(0);
                 iSp -= n_words;
                 for (i = 0; i < n_words; i++)
                    StackWord(i) = (W_)con->payload[i];
                 goto nextInsn;
              }
              case bci_UPK_TAG: {
                 /* Unpack N (non-ptr) words from offset M in the
                    constructor K words down the stack, and then push
                    N as a tag, on top of it.  Slow but general; we
                    hope it will be the rare case. */
                 int i;                
                 int n_words = BCO_NEXT;
                 int con_off = BCO_NEXT;
                 int stk_off = BCO_NEXT;
                 StgClosure* con = (StgClosure*)StackWord(stk_off);
                 iSp -= n_words;
                 for (i = 0; i < n_words; i++) 
                    StackWord(i) = (W_)con->payload[con_off + i];
                 iSp --;
                 StackWord(0) = n_words;
                 goto nextInsn;
              }
              case bci_PACK: {
                 int i;
                 int o_itbl         = BCO_NEXT;
                 int n_words        = BCO_NEXT;
                 StgInfoTable* itbl = INFO_PTR_TO_STRUCT(BCO_ITBL(o_itbl));
                 int request        = CONSTR_sizeW( itbl->layout.payload.ptrs, 
                                                    itbl->layout.payload.nptrs );
                 StgClosure* con = (StgClosure*)allocate_NONUPD(request);
                 ASSERT( itbl->layout.payload.ptrs + itbl->layout.payload.nptrs > 0);
                 SET_HDR(con, BCO_ITBL(o_itbl), CCS_SYSTEM/*ToDo*/);
                 for (i = 0; i < n_words; i++)
                    con->payload[i] = (StgClosure*)StackWord(i);
                 iSp += n_words;
                 iSp --;
                 StackWord(0) = (W_)con;
		 IF_DEBUG(evaluator,
                          fprintf(stderr,"\tBuilt "); 
                          printObj((StgClosure*)con);
		         );
                 goto nextInsn;
              }
              case bci_TESTLT_P: {
                 int discr  = BCO_NEXT;
                 int failto = BCO_NEXT;
                 StgClosure* con = (StgClosure*)StackWord(0);
                 if (constrTag(con) >= discr)
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTEQ_P: {
                 int discr  = BCO_NEXT;
                 int failto = BCO_NEXT;
                 StgClosure* con = (StgClosure*)StackWord(0);
                 if (constrTag(con) != discr)
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTLT_I: {
                 /* The top thing on the stack should be a tagged int. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 I_ stackInt = (I_)StackWord(1);
                 ASSERT(1 == StackWord(0));
                 if (stackInt >= (I_)BCO_LIT(discr))
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTEQ_I: {
                 /* The top thing on the stack should be a tagged int. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 I_ stackInt = (I_)StackWord(1);
                 ASSERT(1 == StackWord(0));
                 if (stackInt != (I_)BCO_LIT(discr))
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTLT_D: {
                 /* The top thing on the stack should be a tagged double. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 StgDouble stackDbl, discrDbl;
                 ASSERT(sizeofW(StgDouble) == StackWord(0));
                 stackDbl = PK_DBL( & StackWord(1) );
                 discrDbl = PK_DBL( & BCO_LIT(discr) );
                 if (stackDbl >= discrDbl)
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTEQ_D: {
                 /* The top thing on the stack should be a tagged double. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 StgDouble stackDbl, discrDbl;
                 ASSERT(sizeofW(StgDouble) == StackWord(0));
                 stackDbl = PK_DBL( & StackWord(1) );
                 discrDbl = PK_DBL( & BCO_LIT(discr) );
                 if (stackDbl != discrDbl)
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTLT_F: {
                 /* The top thing on the stack should be a tagged float. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 StgFloat stackFlt, discrFlt;
                 ASSERT(sizeofW(StgFloat) == StackWord(0));
                 stackFlt = PK_FLT( & StackWord(1) );
                 discrFlt = PK_FLT( & BCO_LIT(discr) );
                 if (stackFlt >= discrFlt)
                    bciPtr = failto;
                 goto nextInsn;
              }
              case bci_TESTEQ_F: {
                 /* The top thing on the stack should be a tagged float. */
                 int discr   = BCO_NEXT;
                 int failto  = BCO_NEXT;
                 StgFloat stackFlt, discrFlt;
                 ASSERT(sizeofW(StgFloat) == StackWord(0));
                 stackFlt = PK_FLT( & StackWord(1) );
                 discrFlt = PK_FLT( & BCO_LIT(discr) );
                 if (stackFlt != discrFlt)
                    bciPtr = failto;
                 goto nextInsn;
              }

              /* Control-flow ish things */
              case bci_ENTER: {
                 goto nextEnter;
              }
              case bci_RETURN: {
                 /* Figure out whether returning to interpreted or
                    compiled code. */
                 int           o_itoc_itbl = BCO_NEXT;
                 int           tag         = StackWord(0);
                 StgInfoTable* ret_itbl    = (StgInfoTable*)StackWord(tag +1);
                 ASSERT(tag <= 2); /* say ... */
                 if (ret_itbl == (StgInfoTable*)&stg_ctoi_ret_R1p_info
                     || ret_itbl == (StgInfoTable*)&stg_ctoi_ret_R1n_info
                     || ret_itbl == (StgInfoTable*)&stg_ctoi_ret_F1_info
                     || ret_itbl == (StgInfoTable*)&stg_ctoi_ret_D1_info
                     || ret_itbl == (StgInfoTable*)&stg_ctoi_ret_V_info) {
                     /* Returning to interpreted code.  Interpret the BCO 
                        immediately underneath the itbl. */
                     StgBCO* ret_bco = (StgBCO*)StackWord(tag +1+1);
                     iSp --;
                     StackWord(0) = (W_)ret_bco;
                     goto nextEnter;
                 } else {
                     /* Returning (unboxed value) to compiled code.
                        Replace tag with a suitable itbl and ask the
                        scheduler to run it.  The itbl code will copy
                        the TOS value into R1/F1/D1 and do a standard
                        compiled-code return. */
                     StgInfoTable* magic_itbl = BCO_ITBL(o_itoc_itbl);
                     if (magic_itbl != NULL) {
                        StackWord(0) = (W_)magic_itbl;
                        cap->rCurrentTSO->what_next = ThreadRunGHC;
                        RETURN(ThreadYielding);
                     } else {
                        /* Special case -- returning a VoidRep to
                           compiled code.  T.O.S is the VoidRep tag,
                           and underneath is the return itbl.  Zap the
                           tag and enter the itbl. */
		        ASSERT(StackWord(0) == (W_)NULL);
		        iSp ++;
                        cap->rCurrentTSO->what_next = ThreadRunGHC;
                        RETURN(ThreadYielding);
                     }
                 }
              }
        
              case bci_JMP: {
                 /* BCO_NEXT modifies bciPtr, so be conservative. */
                 int nextpc = BCO_NEXT;
                 bciPtr     = nextpc;
                 goto nextInsn;
              }
              case bci_CASEFAIL:
                 barf("interpretBCO: hit a CASEFAIL");

              /* Errors */
              default: 
                 barf("interpretBCO: unknown or unimplemented opcode");

          } /* switch on opcode */

	  barf("interpretBCO: fell off end of insn loop");

       }
       /* ---------------------------------------------------- */
       /* End of the bytecode interpreter                      */
       /* ---------------------------------------------------- */

       defer_to_sched:
       default: {
#         ifdef INTERP_STATS
          { int j = get_itbl(obj)->type;
            ASSERT(j >= 0 && j < N_CLOSURE_TYPES);
            it_unknown_entries[j]++;
            it_total_unknown_entries++;
          }
#         endif

          /* Can't handle this object; yield to sched. */
          IF_DEBUG(evaluator,
                   fprintf(stderr, "entering unknown closure -- yielding to sched\n"); 
                   printObj(obj);
                  );
          iSp--; StackWord(0) = (W_)obj;
          cap->rCurrentTSO->what_next = ThreadEnterGHC;
          RETURN(ThreadYielding);
       }
    } /* switch on object kind */

    barf("fallen off end of object-type switch in interpretBCO()");
}
