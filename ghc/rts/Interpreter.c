#if 0
/* -----------------------------------------------------------------------------
 * Bytecode evaluator
 *
 * Copyright (c) 1994-2000.
 *
 * $RCSfile: Interpreter.c,v $
 * $Revision: 1.2 $
 * $Date: 2000/12/11 17:59:01 $
 * ---------------------------------------------------------------------------*/

#include "Rts.h"



#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Updates.h"
#include "Storage.h"
#include "SchedAPI.h" /* for createGenThread */
#include "Schedule.h" /* for context_switch  */
#include "Bytecodes.h"
#include "Assembler.h" /* for CFun stuff */
#include "ForeignCall.h"
#include "PrimOps.h"   /* for __{encode,decode}{Float,Double} */
#include "Prelude.h"
#include "Itimer.h"
#include "Evaluator.h"
#include "sainteger.h"

#ifdef DEBUG
#include "Printer.h"
#include "Disassembler.h"
#include "Sanity.h"
#include "StgRun.h"
#endif

#include <math.h>    /* These are for primops */
#include <limits.h>  /* These are for primops */
#include <float.h>   /* These are for primops */
#ifdef HAVE_IEEE754_H
#include <ieee754.h> /* These are for primops */
#endif

#endif /* 0 */

#if 0
/* --------------------------------------------------------------------------
 * The new bytecode interpreter
 * ------------------------------------------------------------------------*/

/* Sp points to the lowest live word on the stack. */

#define StackWord(n)  ((W_*)iSp)[n]
#define BCO_NEXT      bco_instrs[bciPtr++]
#define BCO_PTR(n)    bco_ptrs[n]


StgThreadReturnCode enter ( Capability* cap )
{
   /* On entry, the closure to interpret is on the top of the
      stack. */
 
   /* Use of register here is primarily to make it clear to compilers
      that these entities are non-aliasable.
   */
    register StgPtr           iSp;    /* local state -- stack pointer */
    register StgUpdateFrame*  iSu;    /* local state -- frame pointer */
    register StgPtr           iSpLim; /* local state -- stack lim pointer */
    register StgClosure*      obj;

    iSp    = cap->rCurrentTSO->sp;
    iSu    = cap->rCurrentTSO->su;
    iSpLim = cap->rCurrentTSO->stack + RESERVED_STACK_WORDS;

    IF_DEBUG(evaluator,
             enterCountI++;
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"Entering: ",); printObj(obj);
             fprintf(stderr,"xSp = %p\txSu = %p\n", xSp, xSu);
             fprintf(stderr, "\n" );
             printStack(iSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,iSu);
             fprintf(stderr, "\n\n");
            );

    /* Main object-entering loop.  Object to be entered is on top of
       stack. */
    nextEnter:

    obj = StackWord(0); iSp++;

    switch ( get_itbl(obj)->type ) {
       case INVALID_OBJECT:
               barf("Invalid object %p",obj);

       case BCO: bco_entry:

       /* ---------------------------------------------------- */
       /* Start of the bytecode interpreter                    */
       /* ---------------------------------------------------- */
       {
          register StgWord8* bciPtr; /* instruction pointer */
          register StgBCO*   bco = (StgBCO*)obj;
          if (doYouWantToGC()) {
	     iSp--; StackWord(0) = bco;
             return HeapOverflow;
          }

          nextInsn:

          ASSERT((StgWord)(PC) < bco->n_instrs);
          IF_DEBUG(evaluator,
          fprintf(stderr,"Sp = %p\tSu = %p\tpc = %d\t", xSp, xSu, PC);
                  disInstr(bco,PC);
                  if (0) { int i;
                           fprintf(stderr,"\n");
                           for (i = 8; i >= 0; i--) 
                              fprintf(stderr, "%d  %p\n", i, (StgPtr)(*(gSp+i)));
                         }
                  fprintf(stderr,"\n");
                 );

          switch (BCO_NEXT) {

              case bci_PUSH_L: {
                 int o1 = BCO_NEXT;
                 StackWord(-1) = StackWord(o1);
                 Sp--;
                 goto nextInsn;
              }
              case bci_PUSH_LL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 StackWord(-1) = StackWord(o1);
                 StackWord(-2) = StackWord(o2);
                 Sp -= 2;
                 goto nextInsn;
              }
              case bci_PUSH_LLL: {
                 int o1 = BCO_NEXT;
                 int o2 = BCO_NEXT;
                 int o3 = BCO_NEXT;
                 StackWord(-1) = StackWord(o1);
                 StackWord(-2) = StackWord(o2);
                 StackWord(-3) = StackWord(o3);
                 Sp -= 3;
                 goto nextInsn;
              }
              case bci_PUSH_G: {
                 int o1 = BCO_NEXT;
                 StackWord(-1) = BCO_PTR(o1);
                 Sp -= 3;
                 goto nextInsn;
              }
              case bci_PUSH_AS: {
                 int o_bco  = BCO_NEXT;
                 int o_itbl = BCO_NEXT;
                 StackWord(-1) = BCO_LITW(o_itbl);
                 StackWord(-2) = BCO_PTR(o_bco);
                 Sp -= 2;
                 goto nextInsn;
              }
              case bci_PUSH_LIT:{
                 int o = BCO_NEXT;
                 StackWord(-1) = BCO_LIT(o);
                 Sp --;
                 goto nextInsn;
              }
              case bci_PUSH_TAG: {
                 W_ tag = (W_)(BCO_NEXT);
                 StackWord(-1) = tag;
                 Sp --;
                 goto nextInsn;
              }
              case bci_SLIDE: {
                 int n  = BCO_NEXT;
                 int by = BCO_NEXT;
                 ASSERT(Sp+n+by <= (StgPtr)xSu);
                 /* a_1, .. a_n, b_1, .. b_by, s => a_1, .. a_n, s */
                 while(--n >= 0) {
                    StackWord(n+by) = StackWord(n);
                 }
                 Sp += by;
                 goto nextInsn;
              }
              case bci_ALLOC: {
                 int n_payload = BCO_NEXT;
                 P_ p = allocate(AP_sizeW(n_payload));
                 StackWord(-1) = p;
                 Sp --;
                 goto nextInsn;
              }
              case bci_MKAP:        {
                 int off = BCO_NEXT;
                 int n_payload = BCO_NEXT - 1;
                 StgAP_UPD* ap = StackWord(off);
                 ap->n_args = n_payload;
                 ap->fun = (StgClosure*)StackWord(0);
                 for (i = 0; i < n_payload; i++)
                    ap->payload[i] = StackWord(i+1);
                 Sp += n_payload+1;
                 goto nextInsn;
              }
              case bci_UNPACK: {
                 /* Unpack N ptr words from t.o.s constructor */
                 /* The common case ! */
                 int n_words = BCO_NEXT;
                 StgClosure* con = StackWord(0);
                 Sp -= n_words;
                 for (i = 0; i < n_words; i++)
                    StackWord(i) = con->payload[i];
                 goto nextInsn;
              }
              case bci_UNPACK_BX: {
                 /* Unpack N (non-ptr) words from offset M in the
                    constructor K words down the stack, and then push
                    N as a tag, on top of it.  Slow but general; we
                    hope it will be the rare case. */
                 int n_words = BCO_NEXT;
                 int con_off = BCO_NEXT;
                 int stk_off = BCO_NEXT;
                 StgClosure* con = StackWord(stk_off);
                 Sp -= n_words;
                 for (i = 0; i < n_words; i++) 
                    StackWord(i) = con->payload[con_off + i];
                 Sp --;
                 StackWord(0) = n_words;
                 goto nextInsn;
              }
              case bci_PACK:
              case bci_TESTLT_I:
              case bci_TESTEQ_I:
              case bci_TESTLT_F:
              case bci_TESTEQ_F:
              case bci_TESTLT_D:
              case bci_TESTEQ_D:
              case bci_TESTLT_P:
              case bci_TESTEQ_P:
              case bci_CASEFAIL:
   
              /* Control-flow ish things */
              case bci_ARGCHECK:
              case bci_ENTER:
              case bci_RETURN:
        
              /* Errors */
              case bci_LABEL:
              default: barf

          } /* switch on opcode */
	  goto nextEnter;

       }
       /* ---------------------------------------------------- */
       /* End of the bytecode interpreter                      */
       /* ---------------------------------------------------- */

       default: {
          /* Can't handle this object; yield to sched. */
          fprintf(stderr, "entering unknown closure -- yielding to sched\n"); 
          printObj(obj);
          cap->rCurrentTSO->what_next = ThreadEnterGHC;
          iSp--; StackWord(0) = obj;
          return ThreadYielding;
       }
    } /* switch on object kind */

    barf("fallen off end of switch in enter()");
}


#endif /* 0 */
