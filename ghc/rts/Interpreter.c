
/* -----------------------------------------------------------------------------
 * Bytecode evaluator
 *
 * Copyright (c) 1994-2000.
 *
 * $RCSfile: Interpreter.c,v $
 * $Revision: 1.8 $
 * $Date: 2001/01/05 15:24:28 $
 * ---------------------------------------------------------------------------*/

#ifdef GHCI

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

/* Sp points to the lowest live word on the stack. */

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

    iSpLim = cap->rCurrentTSO->stack + RESERVED_STACK_WORDS;

    /* Main object-entering loop.  Object to be entered is on top of
       stack. */
    nextEnter:

    obj = (StgClosure*)StackWord(0); iSp++;

    IF_DEBUG(evaluator,
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"Entering: "); printObj(obj);
             fprintf(stderr,"iSp = %p\tiSu = %p\n", iSp, iSu);
             fprintf(stderr, "\n" );
             printStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
             fprintf(stderr, "\n\n");
            );

    switch ( get_itbl(obj)->type ) {
       case INVALID_OBJECT:
               barf("Invalid object %p",(StgPtr)obj);

#if 0
       case AP_UPD:
        { nat Words;
          nat i;
          StgAP_UPD *ap = (StgAP_UPD*)obj;
          Words = ap->n_args;

          iSp -= sizeofW(StgUpdateFrame);

          {
              StgUpdateFrame *__frame;
              __frame = (StgUpdateFrame *)iSp;
              SET_INFO(__frame, (StgInfoTable *)&stg_upd_frame_info);
              __frame->link = iSu;
              __frame->updatee = (StgClosure *)(ap);
              iSu = __frame;
          }

	  /* WARNING: do a stack overflow check here ! */
          iSp -= Words;

          /* Reload the stack */
          for (i=0; i<Words; i++) StackWord(i) = (W_)ap->payload[i];

          iSp--; StackWord(0) = (W_)ap->fun;
          goto nextEnter;
        }
#endif

       case BCO:

       /* ---------------------------------------------------- */
       /* Start of the bytecode interpreter                    */
       /* ---------------------------------------------------- */
       {
          register int       bciPtr     = 1; /* instruction pointer */
          register StgBCO*   bco        = (StgBCO*)obj;
          register UShort*   instrs     = (UShort*)(&bco->instrs->payload[0]);
          register StgWord*  literals   = (StgWord*)(&bco->literals->payload[0]);
          register StgPtr*   ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);
          register StgInfoTable** itbls = (StgInfoTable**)
                                             (&bco->itbls->payload[0]);

          if (doYouWantToGC()) {
	     iSp--; StackWord(0) = (W_)bco;
             RETURN(HeapOverflow);
          }

          nextInsn:

          ASSERT(bciPtr <= instrs[0]);
          IF_DEBUG(evaluator,
		   //fprintf(stderr, "\n-- BEGIN stack\n");
		   //printStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
		   //fprintf(stderr, "-- END stack\n\n");
		   fprintf(stderr,"iSp = %p   iSu = %p   pc = %d      ", iSp, iSu, bciPtr);
                  disInstr(bco,bciPtr);
                  if (0) { int i;
                           fprintf(stderr,"\n");
                           for (i = 8; i >= 0; i--) 
                              fprintf(stderr, "%d  %p\n", i, (StgPtr)(*(iSp+i)));
                           fprintf(stderr,"\n");
                         }
                 );

          switch (BCO_NEXT) {

              case bci_ARGCHECK: {
                 int i;
                 StgPAP* pap;
                 int arg_words_reqd = BCO_NEXT;
                 int arg_words_avail = ((W_*)iSu) - ((W_*)iSp);
                 if (arg_words_avail >= arg_words_reqd) goto nextInsn;
                 /* Handle arg check failure.  Copy the spare args
                    into a PAP frame. */
		 /* fprintf(stderr, "arg check fail %d %d\n", arg_words_reqd, arg_words_avail ); */
                 pap = (StgPAP*)allocate(PAP_sizeW(arg_words_avail));
                 SET_HDR(pap,&stg_PAP_info,CCS_SYSTEM/*ToDo*/);
                 pap->n_args = arg_words_avail;
                 pap->fun = obj;
                 for (i = 0; i < arg_words_avail; i++)
                    pap->payload[i] = (StgClosure*)StackWord(i);
                 /* Push on the stack and defer to the scheduler. */
                 iSp = (StgPtr)iSu;
                 iSp --;
                 StackWord(0) = (W_)pap;
                 RETURN(ThreadEnterGHC);
              }
              case bci_PUSH_L: {
                 int o1 = BCO_NEXT;
                 ASSERT((W_*)iSp+o1 < (W_*)iSu);
                 StackWord(-1) = StackWord(o1);
                 iSp--;
                 goto nextInsn;
              }
              case bci_PUSH_LL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 StackWord(-1) = StackWord(o1);
                 StackWord(-2) = StackWord(o2);
                 iSp -= 2;
                 goto nextInsn;
              }
              case bci_PUSH_LLL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 int o3 = BCO_NEXT;
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
                 int o_lits = BCO_NEXT;
                 int n_words = BCO_NEXT;
                 for (; n_words > 0; n_words--) {
                    iSp --;
                    StackWord(0) = BCO_LIT(o_lits);
                    o_lits++;
                 }
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
                 goto nextInsn;
              }
              case bci_ALLOC: {
                 int n_payload = BCO_NEXT - 1;
                 StgAP_UPD* ap = (StgAP_UPD*)allocate(AP_sizeW(n_payload));
                 StackWord(-1) = (W_)ap;
                 ap->n_args = n_payload;
                 SET_HDR(ap, &stg_AP_UPD_info, ??)
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
                 StgInfoTable* itbl = BCO_ITBL(o_itbl);
                 /* A bit of a kludge since n_words = n_p + n_np */
                 int request        = CONSTR_sizeW( n_words, 0 );
                 StgClosure* con = (StgClosure*)allocate(request);
                 SET_HDR(con, itbl, CCS_SYSTEM/*ToDo*/);
                 for (i = 0; i < n_words; i++)
                    con->payload[i] = (StgClosure*)StackWord(i);
                 iSp += n_words;
                 iSp --;
                 StackWord(0) = (W_)con;
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

              /* Control-flow ish things */
              case bci_ENTER: {
                 goto nextEnter;
              }
              case bci_RETURN: {
                 /* Figure out whether returning to interpreted or
                    compiled code. */
                 int           o_itoc_itbl = BCO_NEXT;
                 int           tag         = StackWord(0);
                 StgInfoTable* ret_itbl    = (StgInfoTable*)StackWord(tag+1 +1);
                 ASSERT(tag <= 2); /* say ... */
                 if (ret_itbl == (StgInfoTable*)&stg_ctoi_ret_R1_info
                     /* || ret_itbl == stg_ctoi_ret_F1_info
                        || ret_itbl == stg_ctoi_ret_D1_info */) {
                     /* Returning to interpreted code.  Interpret the BCO 
                        immediately underneath the itbl. */
                     StgBCO* ret_bco = (StgBCO*)StackWord(tag+1 +1+1);
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
                     StackWord(0) = (W_)magic_itbl;
                     RETURN(ThreadRunGHC);
                 }
              }
        
              case bci_CASEFAIL:
                 barf("interpretBCO: hit a CASEFAIL");

              /* As yet unimplemented */
              case bci_TESTLT_I:
              case bci_TESTEQ_I:
              case bci_TESTLT_F:
              case bci_TESTEQ_F:
              case bci_TESTLT_D:
              case bci_TESTEQ_D:

              /* Errors */
              default: 
                 barf("interpretBCO: unknown or unimplemented opcode");

          } /* switch on opcode */

	  barf("interpretBCO: fell off end of insn loop");

       }
       /* ---------------------------------------------------- */
       /* End of the bytecode interpreter                      */
       /* ---------------------------------------------------- */

       default: {
          /* Can't handle this object; yield to sched. */
          IF_DEBUG(evaluator,
                   fprintf(stderr, "entering unknown closure -- yielding to sched\n"); 
                   printObj(obj);
                  )
          cap->rCurrentTSO->what_next = ThreadEnterGHC;
          iSp--; StackWord(0) = (W_)obj;
          RETURN(ThreadYielding);
       }
    } /* switch on object kind */

    barf("fallen off end of object-type switch in interpretBCO()");
}

#endif /* GHCI */
