// -----------------------------------------------------------------------------
// Apply.hc
//
// (c) The University of Glasgow 2002
//
// Application-related bits.
//
// -----------------------------------------------------------------------------

#include "Stg.h"
#include "Rts.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "Printer.h"
#include "Sanity.h"
#include "Apply.h"

#include <stdio.h>

// ----------------------------------------------------------------------------
// Evaluate a closure and return it.
//
//      stg_ap_0_info   <--- Sp
//
// NOTE: this needs to be a polymorphic return point, because we can't
// be sure that the thing being evaluated is not a function.

// These names are just to keep VEC_POLY_INFO_TABLE() happy - all the
// entry points in the polymorphic info table point to the same code.
#define stg_ap_0_0_ret stg_ap_0_ret
#define stg_ap_0_1_ret stg_ap_0_ret
#define stg_ap_0_2_ret stg_ap_0_ret
#define stg_ap_0_3_ret stg_ap_0_ret
#define stg_ap_0_4_ret stg_ap_0_ret
#define stg_ap_0_5_ret stg_ap_0_ret
#define stg_ap_0_6_ret stg_ap_0_ret
#define stg_ap_0_7_ret stg_ap_0_ret

VEC_POLY_INFO_TABLE(stg_ap_0,
	       MK_SMALL_BITMAP(0/*framsize*/, 0/*bitmap*/),
	       0,0,0,RET_SMALL,,EF_);
F_
stg_ap_0_ret(void)
{ 
    // fn is in R1, no args on the stack
    StgInfoTable *info;
    nat arity;
    FB_;

    IF_DEBUG(apply,fprintf(stderr, "stg_ap_0_ret... "); printClosure(R1.cl));
    IF_DEBUG(sanity,checkStackChunk(Sp+1,CurrentTSO->stack + CurrentTSO->stack_size));

    Sp++;
    ENTER();
    FE_
}

/* -----------------------------------------------------------------------------
   Entry Code for a PAP.

   This entry code is *only* called by one of the stg_ap functions.
   On entry: Sp points to the remaining arguments on the stack.  If
   the stack check fails, we can just push the PAP on the stack and
   return to the scheduler.

   On entry: R1 points to the PAP.  The rest of the function's arguments
   (*all* of 'em) are on the stack, starting at Sp[0].

   The idea is to copy the chunk of stack from the PAP object onto the
   stack / into registers, and enter the function.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_PAP_info,stg_PAP_entry,/*special layout*/0,0,PAP,,EF_,"PAP","PAP");
STGFUN(stg_PAP_entry)
{
  nat Words;
  StgPtr p;
  nat i;
  StgPAP *pap;
  FB_
    
  pap = (StgPAP *) R1.p;

  Words = pap->n_args;

  // Check for stack overflow and bump the stack pointer.
  // We have a hand-rolled stack check fragment here, because none of
  // the canned ones suit this situation.
  if ((Sp - Words) < SpLim) {
      DEBUG_ONLY(fprintf(stderr,"PAP STACK CHECK!\n"));
      // there is a return address on the stack in the event of a
      // stack check failure.  The various stg_apply functions arrange
      // this before calling stg_PAP_entry.
      JMP_(stg_gc_unpt_r1);
  }
  // Sp is already pointing one word below the arguments...
  Sp -= Words-1;

  // profiling
  TICK_ENT_PAP(pap);
  LDV_ENTER(pap);
  // Enter PAP cost centre -- lexical scoping only
  ENTER_CCS_PAP_CL(pap);

  R1.cl = pap->fun;
  p = (P_)(pap->payload);

  // Reload the stack
  for (i=0; i<Words; i++) {
      Sp[i] = (W_) *p++;
  }

  // Off we go!
  TICK_ENT_VIA_NODE();

#ifdef NO_ARG_REGS
  JMP_(GET_ENTRY(R1.cl));
#else
  {
      StgFunInfoTable *info;
      info = get_fun_itbl(R1.cl);
      if (info->fun_type == ARG_GEN || info->fun_type == ARG_GEN_BIG) {
	  JMP_(info->slow_apply);
      } else {
	  JMP_(stg_ap_stack_entries[info->fun_type]);
      }
  }
#endif
  FE_
}
