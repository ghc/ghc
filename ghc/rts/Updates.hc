/* -----------------------------------------------------------------------------
 * $Id: Updates.hc,v 1.3 1999/01/13 17:25:49 simonm Exp $
 *
 * Code to perform updates.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "HeapStackCheck.h"
#include "Storage.h"

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

#define UPD_FRAME_ENTRY_TEMPLATE(label,ret)				\
        STGFUN(label);							\
	STGFUN(label)							\
	{								\
          StgClosure *updatee;						\
	  FB_								\
	  /* tick - ToDo: check this is right */			\
	  TICK_UPD_EXISTING();						\
									\
          updatee = ((StgUpdateFrame *)Sp)->updatee;			\
						\
	  /* update the updatee with an indirection to the return value */\
	  UPD_IND(updatee,R1.p);					\
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

VEC_POLY_INFO_TABLE(Upd_frame,UPD_FRAME_BITMAP, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, UPDATE_FRAME);

/* -----------------------------------------------------------------------------
   Entry Code for a PAP.

   The idea is to copy the chunk of stack from the PAP object and then
   re-enter the function closure that failed it's args check in the
   first place.

   In fact, we do a little optimisation too, by performing the updates
   for any update frames sitting on top of the stack. (ToDo: is this
   really an optimisation? --SDM)
   -------------------------------------------------------------------------- */

INFO_TABLE(PAP_info,PAP_entry,/*special layout*/0,0,PAP,const,EF_,0,0);
STGFUN(PAP_entry)
{
  nat Words;
#ifdef PROFILING
  CostCentreStack *CCS_pap;
#endif
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
      
      CCS_pap = pap->header.prof.ccs;
      CCCS = (IS_CAF_OR_DICT_OR_SUB_CCS(CCS_pap)) 
		? Su->header.prof.ccs 
		: CCS_pap;
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
  STK_CHK(Words,PAP_entry,R2.p,1,);
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
       argument satisfaction check
       */
    Fun = R1.cl;

#if defined(GRAN_COUNT)
#error Fixme.
      ++nPAPs;
#endif

    /* Just copy the whole block of stack between the stack pointer
     * and the update frame pointer for now.  This might include some
     * tagging, which the garbage collector will have to pay attention
     * to, but it's much easier than sorting the words into pointers
     * and non-pointers.
     */

    Words    = (P_)Su - (P_)Sp;
    ASSERT((int)Words >= 0);

#if defined(PROFILING)
    /* set "CC_pap" to go in the updatee (see Sansom thesis, p 183) */

    CCS_pap = (CostCentreStack *) Fun->header.prof.ccs;
    if (IS_CAF_OR_DICT_OR_SUB_CCS(CCS_pap)) {
	CCS_pap = CCCS;
    }
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

	TICK_ALLOC_UPD_PAP(DYN_HS, NArgWords, 0, PapSize);
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
      Sp = stgCast(StgPtr,Su) + sizeofW(StgSeqFrame);

      /* restore Su */
      Su = stgCast(StgSeqFrame*,Su)->link;
	
      /* return to the activation record, with the address of the PAP in R1 */
      R1.p = (P_)PapClosure;
      JMP_(ENTRY_CODE(*Sp));
      
    case CATCH_FRAME:
      /* Set Sp to just above the CATCH frame (should be an activation rec.)*/
      Sp = stgCast(StgPtr,Su) + sizeofW(StgCatchFrame);

      /* restore Su */
      Su = stgCast(StgCatchFrame*,Su)->link;
	
      /* restart by entering the PAP */
      R1.p = (P_)PapClosure;
      JMP_(GET_ENTRY(R1.cl));
      
    case UPDATE_FRAME:
      /* 
       * Now we have a standard update frame, so we update the updatee with 
       * either the new PAP or Node.
       */
      
      Updatee = Su->updatee; 
      UPD_IND(Updatee,PapClosure);
      
      if (Words != 0) {
	TICK_UPD_PAP_IN_NEW(NArgWords);
	
      } else {
	TICK_UPD_PAP_IN_PLACE();
	
#if defined(PROFILING)
	/* 
	 * Lexical scoping requires a *permanent* indirection, and we
	 * also have to set the cost centre for the indirection.
	 */
	SET_INFO(Updatee, &IND_PERM_info);
	Updatee->header.prof.ccs = CCS_pap;
#endif /* PROFILING */
      }
      
#if defined(PROFILING)
      /* 
       * Restore the Cost Centre too (if required); again see Sansom
       * thesis p 183.  Take the CC out of the update frame if a CAF/DICT.
       */
      CCCS = IS_CAF_OR_DICT_OR_SUB_CCS(CCS_pap)
		? Su->header.prof.ccs 
		: CCS_pap;
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


INFO_TABLE(AP_UPD_info,AP_UPD_entry,/*special layout*/0,0,AP_UPD,const,EF_,0,0);
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

  TICK_ENT_PAP(ap);  /* ToDo: TICK_ENT_AP_UPD */

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
      Su = stgCast(StgSeqFrame*,Sp)->link;	\
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

VEC_POLY_INFO_TABLE(seq_frame,1, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, SEQ_FRAME);

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

INFO_TABLE(seq_info,seq_entry,1,0,FUN,const,EF_,0,0);
STGFUN(seq_entry)
{
  FB_
  STK_CHK_GEN(sizeofW(StgSeqFrame), NO_PTRS, seq_entry, );
  Sp -= sizeof(StgSeqFrame);
  PUSH_SEQ_FRAME(Sp);
  R1.cl = R1.cl->payload[0];
  JMP_(ENTRY_CODE(*R1.p));         
  FE_
}


/* -----------------------------------------------------------------------------
   Exception Primitives
   -------------------------------------------------------------------------- */

FN_(catchZh_fast);
FN_(raiseZh_fast);

#define CATCH_FRAME_ENTRY_TEMPLATE(label,ret) 	\
   FN_(label);					\
   FN_(label)					\
   {						\
      FB_					\
      Su = ((StgCatchFrame *)Sp)->link;		\
      Sp += sizeofW(StgCatchFrame);		\
      JMP_(ret);				\
      FE_					\
   }

CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_entry,ENTRY_CODE(Sp[0]));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_0_entry,RET_VEC(Sp[0],0));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_1_entry,RET_VEC(Sp[0],1));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_2_entry,RET_VEC(Sp[0],2));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_3_entry,RET_VEC(Sp[0],3));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_4_entry,RET_VEC(Sp[0],4));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_5_entry,RET_VEC(Sp[0],5));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_6_entry,RET_VEC(Sp[0],6));
CATCH_FRAME_ENTRY_TEMPLATE(catch_frame_7_entry,RET_VEC(Sp[0],7));

#ifdef PROFILING
#define CATCH_FRAME_BITMAP 3
#else
#define CATCH_FRAME_BITMAP 1
#endif

/* Catch frames are very similar to update frames, but when entering
 * one we just pop the frame off the stack and perform the correct
 * kind of return to the activation record underneath us on the stack.
 */

VEC_POLY_INFO_TABLE(catch_frame, CATCH_FRAME_BITMAP, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, CATCH_FRAME);

/* -----------------------------------------------------------------------------
 * The catch infotable
 *
 * This should be exactly the same as would be generated by this STG code
 *
 * catch = {x,h} \n {} -> catch#{x,h}
 *
 * It is used in deleteThread when reverting blackholes.
 * -------------------------------------------------------------------------- */

INFO_TABLE(catch_info,catch_entry,2,0,FUN,const,EF_,0,0);
STGFUN(catch_entry)
{
  FB_
  R2.cl = payloadCPtr(R1.cl,1); /* h */
  R1.cl = payloadCPtr(R1.cl,0); /* x */
  JMP_(catchZh_fast);
  FE_
}

FN_(catchZh_fast)
{
  StgCatchFrame *fp;
  FB_

    /* args: R1 = m, R2 = k */
    STK_CHK_GEN(sizeofW(StgCatchFrame), R1_PTR | R2_PTR, catchZh_fast, );
    Sp -= sizeofW(StgCatchFrame);
    fp = stgCast(StgCatchFrame*,Sp);
    SET_HDR(fp,(StgInfoTable *)&catch_frame_info,CCCS);
    fp -> handler = R2.cl;
    fp -> link = Su;
    Su = stgCast(StgUpdateFrame*,fp);
    TICK_ENT_VIA_NODE();
    JMP_(ENTRY_CODE(*R1.p));         
    
  FE_
}      

/* -----------------------------------------------------------------------------
 * The raise infotable
 * 
 * This should be exactly the same as would be generated by this STG code
 *
 *   raise = {err} \n {} -> raise#{err}
 *
 * It is used in raiseZh_fast to update thunks on the update list
 * -------------------------------------------------------------------------- */

INFO_TABLE(raise_info,raise_entry,1,0,FUN,const,EF_,0,0);
STGFUN(raise_entry)
{
  FB_
  R1.cl = payloadCPtr(R1.cl,0);
  JMP_(raiseZh_fast);
  FE_
}

FN_(raiseZh_fast)
{
  StgClosure *handler;
  StgUpdateFrame *p;
  FB_
    /* args : R1 = error */

    p = Su;

    while (1) {

      switch (get_itbl(p)->type) {

      case UPDATE_FRAME:
	UPD_INPLACE1(p->updatee,&raise_info,R1.cl);
	p = p->link;
	continue;

      case SEQ_FRAME:
	p = stgCast(StgSeqFrame*,p)->link;
	continue;

      case CATCH_FRAME:
	/* found it! */
	break;

      case STOP_FRAME:
	barf("raiseZh_fast: STOP_FRAME");

      default:
	barf("raiseZh_fast: weird activation record");
      }
      
      break;

    }
    
    /* Ok, p points to the enclosing CATCH_FRAME.  Pop everything down to
     * and including this frame, update Su, push R1, and enter the handler.
     */
    Su = ((StgCatchFrame *)p)->link; 
    handler = ((StgCatchFrame *)p)->handler;
    
    Sp = stgCast(StgPtr,p) + sizeofW(StgCatchFrame) - 1;
    *Sp = R1.w;

    TICK_ENT_VIA_NODE();
    R1.cl = handler;
    JMP_(ENTRY_CODE(handler->header.info));
    
  FE_
}

