/* -----------------------------------------------------------------------------
 * $Id: Updates.hc,v 1.24 1999/12/01 14:34:39 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Code to perform updates.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "HeapStackCheck.h"
#include "Storage.h"
#include "ProfRts.h"

/*
  The update frame return address must be *polymorphic*, that means
  we have to cope with both vectored and non-vectored returns.  This
  is done by putting the return vector right before the info table, and
  having a standard direct return address after the info table (pointed
  to by the return address itself, as usual).

  Each entry in the vector table points to a specialised entry code fragment
  that knows how to return after doing the update.  It would be possible to
  use a single generic piece of code that simply entered the return value
  to return, but it's quicker this way.  The direct return code of course
  just does another direct return when it's finished.

  Why is there necessarily an activation underneath us on the stack?
  Because if we're returning, that means we've got a constructor in
  our hands.  If there were any arguments to be applied to it, that
  would be a type error.  We don't ever return a PAP to an update frame,
  the update is handled manually by stg_update_PAP.
*/

/* on entry to the update code
   (1) R1 points to the closure being returned
   (2) R2 contains the tag (if we returned directly, non-vectored)
   (3) Sp points to the update frame
   */

/* Why updatee is placed in a temporary variable here: this helps
   gcc's aliasing by indicating that the location of the updatee
   doesn't change across assignments.  Saves one instruction in the
   update code. 
   */

#if defined(REG_Su)
#define UPD_FRAME_ENTRY_TEMPLATE(label,ret)				\
        STGFUN(label);							\
	STGFUN(label)							\
	{								\
	  FB_								\
									\
          Su = (StgUpdateFrame *)((StgUpdateFrame *)Sp)->updatee;	\
									\
	  /* Tick - it must be a con, all the paps are handled		\
	   * in stg_upd_PAP and PAP_entry below				\
	   */								\
	  TICK_UPD_CON_IN_NEW(sizeW_fromITBL(get_itbl(Su)));		\
									\
 	  UPD_IND(Su,R1.p);						\
									\
	  /* reset Su to the next update frame */			\
	  Su = ((StgUpdateFrame *)Sp)->link;				\
									\
	  /* remove the update frame from the stack */			\
	  Sp += sizeofW(StgUpdateFrame);				\
									\
	  JMP_(ret);							\
	  FE_								\
	}
#else

#define UPD_FRAME_ENTRY_TEMPLATE(label,ret)				\
        STGFUN(label);							\
	STGFUN(label)							\
	{								\
          StgClosure *updatee;						\
	  FB_								\
									\
          updatee = ((StgUpdateFrame *)Sp)->updatee;			\
									\
	  /* Tick - it must be a con, all the paps are handled		\
	   * in stg_upd_PAP and PAP_entry below				\
	   */								\
	  TICK_UPD_CON_IN_NEW(sizeW_fromITBL(get_itbl(updatee)));	\
									\
	  UPD_IND(updatee, R1.cl);					\
									\
	  /* reset Su to the next update frame */			\
	  Su = ((StgUpdateFrame *)Sp)->link;				\
									\
	  /* remove the update frame from the stack */			\
	  Sp += sizeofW(StgUpdateFrame);				\
									\
	  JMP_(ret);							\
	  FE_								\
	}
#endif

UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_entry,ENTRY_CODE(Sp[0]));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_0_entry,RET_VEC(Sp[0],0));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_1_entry,RET_VEC(Sp[0],1));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_2_entry,RET_VEC(Sp[0],2));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_3_entry,RET_VEC(Sp[0],3));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_4_entry,RET_VEC(Sp[0],4));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_5_entry,RET_VEC(Sp[0],5));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_6_entry,RET_VEC(Sp[0],6));
UPD_FRAME_ENTRY_TEMPLATE(Upd_frame_7_entry,RET_VEC(Sp[0],7));

/*
  Make sure this table is big enough to handle the maximum vectored
  return size!
  */

#ifdef PROFILING
#define UPD_FRAME_BITMAP 3
#else
#define UPD_FRAME_BITMAP 1
#endif

/* this bitmap indicates that the first word of an update frame is a
 * non-pointer - this is the update frame link.  (for profiling,
 * there's a cost-centre-stack in there too).
 */

VEC_POLY_INFO_TABLE(Upd_frame,UPD_FRAME_BITMAP, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, UPDATE_FRAME,, EF_);

/* -----------------------------------------------------------------------------
   Entry Code for a PAP.

   The idea is to copy the chunk of stack from the PAP object and then
   re-enter the function closure that failed it's args check in the
   first place.

   In fact, we do a little optimisation too, by performing the updates
   for any update frames sitting on top of the stack. (ToDo: is this
   really an optimisation? --SDM)
   -------------------------------------------------------------------------- */

INFO_TABLE(PAP_info,PAP_entry,/*special layout*/0,0,PAP,,EF_,0,0);
STGFUN(PAP_entry)
{
  nat Words;
  P_ p;
  nat i;
  StgPAP *pap;

  FB_
    
  pap = (StgPAP *) R1.p;
  
  /*
   * remove any update frames on the top of the stack, by just
   * performing the update here.
   */
  while ((W_)Su - (W_)Sp == 0) {

    switch (get_itbl(Su)->type) {

    case UPDATE_FRAME:
      /* We're sitting on top of an update frame, so let's do the business */
      UPD_IND(Su->updatee, pap);

#if defined(PROFILING)
      /* 
       * Restore the Cost Centre too (if required); again see Sansom
       * thesis p 183.  Take the CC out of the update frame if a
       * CAF/DICT.
       */
      
      CCCS = Su->header.prof.ccs;
#endif /* PROFILING */
      
      Su = Su->link;
      Sp += sizeofW(StgUpdateFrame);
      continue;

    case SEQ_FRAME:
      /* Just pop the seq frame and return to the activation record
       * underneath us - R1 already contains the address of the PAP.
       */
      Su = ((StgSeqFrame *)Su)->link;
      Sp += sizeofW(StgSeqFrame);
      JMP_(ENTRY_CODE(*Sp));

    case CATCH_FRAME:
      /* can't happen, see stg_update_PAP */
      barf("PAP_entry: CATCH_FRAME");

    default:
      barf("PAP_entry: strange activation record");
    }

  }

  Words = pap->n_args;

  /* 
   * Check for stack overflow.
   */
  STK_CHK_NP(Words,1,);
  Sp -= Words;

  TICK_ENT_PAP(pap);

  /* Enter PAP cost centre -- lexical scoping only */
  ENTER_CCS_PAP_CL(pap);

  R1.cl = pap->fun;
  p = (P_)(pap->payload);

  /* Reload the stack */
  for (i=0; i<Words; i++) Sp[i] = (W_) *p++;

  /* Off we go! */
  TICK_ENT_VIA_NODE();
  JMP_(GET_ENTRY(R1.cl));
  FE_
}

/* -----------------------------------------------------------------------------
   stg_update_PAP: Update the current closure with a partial application.

   This function is called whenever an argument satisfaction check fails.
   -------------------------------------------------------------------------- */

EXTFUN(stg_update_PAP)
{
  nat Words, PapSize;
#ifdef PROFILING
  CostCentreStack *CCS_pap;
#endif
  StgPAP* PapClosure;
  StgClosure *Fun, *Updatee;
  P_ p;
  I_ i;
  
  FB_

    /* Save the pointer to the function closure that just failed the
     * argument satisfaction check
     */
    Fun = R1.cl;

#if defined(GRAN_COUNT)
#error Fixme.
      ++nPAPs;
#endif

    /* Just copy the whole block of stack between the stack pointer
     * and the update frame pointer.
     */
    Words    = (P_)Su - (P_)Sp;
    ASSERT((int)Words >= 0);

#if defined(PROFILING)
    /* pretend we just entered the function closure */
    ENTER_CCS_FCL(Fun);
    CCS_pap = CCCS;
#endif

    if (Words == 0) { 

        /* 
         * No arguments, only Node.  Skip building the PAP and
         * just plan to update with an indirection.
         */

	PapClosure = (StgPAP *)Fun;

    } else {
           /* Build the PAP */

	PapSize = Words + sizeofW(StgPAP);
    
	/*
	 * First we need to do a heap check, which involves saving
	 * everything on the stack.  We only have one live pointer:
	 * Fun, the function closure that was passed to us.  If the
	 * heap check fails, we push the function closure on the stack
	 * and instruct the scheduler to try entering it again when
	 * the garbage collector has run.
	 *
	 * It's done this way because there's a possibility that the
	 * garbage collector might have messed around with the stack,
	 * such as removing the update frame.
	 */
	if ((Hp += PapSize) > HpLim) {
	  Sp -= 1;
	  Sp[0] = (W_)Fun;	    
	  JMP_(stg_gc_entertop);
	}

	TICK_ALLOC_UPD_PAP(1/*fun*/ + Words, 0);
#ifdef PROFILING
	CCS_ALLOC(CCS_pap, PapSize);
#endif

	PapClosure = (StgPAP *)(Hp + 1 - PapSize); /* The new PapClosure */

	SET_HDR(PapClosure,&PAP_info,CCS_pap);
	PapClosure->n_args = Words;
	PapClosure->fun = Fun;

	/* Now fill in the closure fields */

	p = Hp;
        for (i = Words-1; i >= 0; i--) {
	   *p-- = (W_) Sp[i];
	}
    }

    /* 
     * Finished constructing PAP closure; now update the updatee. 
     */

    /* ToDo: we'd like to just jump to the code for PAP_entry here,
     * which deals with a stack of update frames in one go.  What to
     * do about the special ticky and profiling stuff here?
     */

    switch (get_itbl(Su)->type) {

    case SEQ_FRAME:
      /* Set Sp to just above the SEQ frame (should be an activation rec.)*/
      Sp = (P_)Su + sizeofW(StgSeqFrame);

      /* restore Su */
      Su = ((StgSeqFrame *)Su)->link;
	
      /* return to the activation record, with the address of the PAP in R1 */
      R1.p = (P_)PapClosure;
      JMP_(ENTRY_CODE(*Sp));
      
    case CATCH_FRAME:
      /* Set Sp to just above the CATCH frame (should be an activation rec.)*/
      Sp = (P_)Su + sizeofW(StgCatchFrame);

      /* restore Su */
      Su = ((StgCatchFrame *)Su)->link;
	
      /* restart by entering the PAP */
      R1.p = (P_)PapClosure;
      JMP_(GET_ENTRY(R1.cl));
      
    case UPDATE_FRAME:
      /* 
       * Now we have a standard update frame, so we update the updatee with 
       * either the new PAP or Node.
       */
      
      Updatee = Su->updatee; 

#if defined(PROFILING)
      if (Words != 0) {
        UPD_IND(Updatee,PapClosure);
	TICK_UPD_PAP_IN_NEW(Words+1);
      } else {
	/* Lexical scoping requires a *permanent* indirection, and we
	 * also have to set the cost centre for the indirection.
	 */
	UPD_PERM_IND(Updatee,PapClosure);
	TICK_UPD_PAP_IN_PLACE();
	Updatee->header.prof.ccs = CCS_pap;
      }
#else
      UPD_IND(Updatee,PapClosure);
      if (Words != 0) {
	TICK_UPD_PAP_IN_NEW(Words+1);
      } else {
	TICK_UPD_PAP_IN_PLACE();
      }
#endif	

#if defined(PROFILING)
      CCCS = Su->header.prof.ccs;
      ENTER_CCS_PAP(CCS_pap);
#endif /* PROFILING */
      
      /* Restore Su */
      Su = Su->link;
      
      /* 
       * Squeeze out update frame from stack.
       */
      for (i = Words-1; i >= 0; i--) {
	Sp[i+(sizeofW(StgUpdateFrame))] = Sp[i];
      }
      Sp += sizeofW(StgUpdateFrame);
      break;
      
    default:
      barf("stg_update_PAP: strange activation record");
    }	

    /* 
     * All done!  Restart by re-entering Node
     * Don't count this entry for ticky-ticky profiling. 
     */
    JMP_(GET_ENTRY(R1.cl));
    FE_
}


/* -----------------------------------------------------------------------------
   Entry Code for an AP_UPD.

   The idea is to copy the chunk of stack from the AP object and then
   enter the function closure.

   (This code is a simplified copy of the PAP code - with all the 
    update frame code stripped out.)
   -------------------------------------------------------------------------- */


INFO_TABLE(AP_UPD_info,AP_UPD_entry,/*special layout*/0,0,AP_UPD,,EF_,0,0);
STGFUN(AP_UPD_entry)
{
  nat Words;
  P_ p;
  nat i;
  StgAP_UPD *ap;

  FB_
    
  ap = (StgAP_UPD *) R1.p;
  
  Words = ap->n_args;

  /* 
   * Check for stack overflow.
   */
  STK_CHK(Words+sizeofW(StgUpdateFrame),AP_UPD_entry,R2.p,1,);

  PUSH_UPD_FRAME(R1.p, 0);
  Sp -= sizeofW(StgUpdateFrame) + Words;

  TICK_ENT_AP_UPD(ap);

  /* Enter PAP cost centre -- lexical scoping only */
  ENTER_CCS_PAP_CL(ap);   /* ToDo: ENTER_CC_AP_UPD_CL */

  R1.cl = ap->fun;
  p = (P_)(ap->payload);

  /* Reload the stack */
  for (i=0; i<Words; i++) Sp[i] = (W_) *p++;

  /* Off we go! */
  TICK_ENT_VIA_NODE();
  JMP_(GET_ENTRY(R1.cl));
  FE_
}


/*-----------------------------------------------------------------------------
  Seq frames 

  We don't have a primitive seq# operator: it is just a 'case'
  expression whose scrutinee has either a polymorphic or function type
  (constructor types can be handled by normal 'case' expressions).

  To handle a polymorphic/function typed seq, we push a SEQ_FRAME on
  the stack.  This is a polymorphic activation record that just pops
  itself and returns when entered.  The purpose of the SEQ_FRAME is to
  act as a barrier in case the scrutinee is a partial application - in
  this way it is just like an update frame, except that it doesn't
  update anything.
  -------------------------------------------------------------------------- */

#define SEQ_FRAME_ENTRY_TEMPLATE(label,ret) 	\
   IFN_(label)					\
   {						\
      FB_					\
      Su = ((StgSeqFrame *)Sp)->link;	\
      Sp += sizeofW(StgSeqFrame);		\
      JMP_(ret);				\
      FE_					\
   }

SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_entry,  ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_0_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_1_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_2_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_3_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_4_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_5_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_6_entry,ENTRY_CODE(Sp[0]));
SEQ_FRAME_ENTRY_TEMPLATE(seq_frame_7_entry,ENTRY_CODE(Sp[0]));

VEC_POLY_INFO_TABLE(seq_frame, UPD_FRAME_BITMAP, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, SEQ_FRAME,, EF_);

/* -----------------------------------------------------------------------------
 * The seq infotable
 *
 * This closure takes one argument, which it evaluates and returns the
 * result with a direct return (never a vectored return!) in R1.  It
 * does this by pushing a SEQ_FRAME on the stack and
 * entering its argument.
 *
 * It is used in deleteThread when reverting blackholes.
 * -------------------------------------------------------------------------- */

INFO_TABLE(seq_info,seq_entry,1,0,FUN,,EF_,0,0);
STGFUN(seq_entry)
{
  FB_
  STK_CHK_GEN(sizeofW(StgSeqFrame), NO_PTRS, seq_entry, );
  Sp -= sizeofW(StgSeqFrame);
  PUSH_SEQ_FRAME(Sp);
  R1.cl = R1.cl->payload[0];
  JMP_(ENTRY_CODE(*R1.p));         
  FE_
}
