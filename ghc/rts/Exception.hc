/* -----------------------------------------------------------------------------
 * $Id: Exception.hc,v 1.4 2000/01/14 11:45:21 hwloidl Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Exception.h"
#include "Schedule.h"
#include "StgRun.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#if defined(PAR)
# include "FetchMe.h"
#endif

/* -----------------------------------------------------------------------------
   Exception Primitives

   A thread can request that asynchronous exceptions not be delivered
   ("blocked") for the duration of an I/O computation.  The primitive
   
	blockAsyncExceptions# :: IO a -> IO a

   is used for this purpose.  During a blocked section, asynchronous
   exceptions may be unblocked again temporarily:

	unblockAsyncExceptions# :: IO a -> IO a

   Furthermore, asynchronous exceptions are blocked automatically during
   the execution of an exception handler.  Both of these primitives
   leave a continuation on the stack which reverts to the previous
   state (blocked or unblocked) on exit.

   A thread which wants to raise an exception in another thread (using
   killThread#) must block until the target thread is ready to receive
   it.  The action of unblocking exceptions in a thread will release all
   the threads waiting to deliver exceptions to that thread.

   -------------------------------------------------------------------------- */

FN_(blockAsyncExceptionszh_fast)
{
  FB_
    /* Args: R1 :: IO a */
    STK_CHK_GEN( 2/* worst case */, R1_PTR, blockAsyncExceptionszh_fast, );

    if (CurrentTSO->blocked_exceptions == NULL) {
      CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
      Sp--;
      Sp[0] = (W_)&unblockAsyncExceptionszh_ret_info;
    }
    Sp--;
    Sp[0] = ARG_TAG(0);
    JMP_(GET_ENTRY(R1.cl));
  FE_
}

INFO_TABLE_SRT_BITMAP(unblockAsyncExceptionszh_ret_info, unblockAsyncExceptionszh_ret_entry, 0, 0, 0, 0, RET_SMALL, , EF_, 0, 0);
FN_(unblockAsyncExceptionszh_ret_entry)
{
  FB_
    ASSERT(CurrentTSO->blocked_exceptions != NULL);
#if defined(GRAN)
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#elif defined(PAR)
      // is CurrentTSO->block_info.closure always set to the node
      // holding the blocking queue !? -- HWL
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#else
    awakenBlockedQueue(CurrentTSO->blocked_exceptions);
#endif
    CurrentTSO->blocked_exceptions = NULL;
    Sp++;
    JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(unblockAsyncExceptionszh_fast)
{
  FB_
    /* Args: R1 :: IO a */
    STK_CHK_GEN(2, R1_PTR, unblockAsyncExceptionszh_fast, );

    if (CurrentTSO->blocked_exceptions != NULL) {
#if defined(GRAN)
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#elif defined(PAR)
      // is CurrentTSO->block_info.closure always set to the node
      // holding the blocking queue !? -- HWL
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#else
      awakenBlockedQueue(CurrentTSO->blocked_exceptions);
#endif
      CurrentTSO->blocked_exceptions = NULL;
      Sp--;
      Sp[0] = (W_)&blockAsyncExceptionszh_ret_info;
    }
    Sp--;
    Sp[0] = ARG_TAG(0);
    JMP_(GET_ENTRY(R1.cl));
  FE_
}

INFO_TABLE_SRT_BITMAP(blockAsyncExceptionszh_ret_info, blockAsyncExceptionszh_ret_entry, 0, 0, 0, 0, RET_SMALL, , EF_, 0, 0);
FN_(blockAsyncExceptionszh_ret_entry)
{
  FB_
    ASSERT(CurrentTSO->blocked_exceptions == NULL);
    CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
    Sp++;
    JMP_(ENTRY_CODE(Sp[0]));
  FE_
}


FN_(killThreadzh_fast)
{
  FB_
  /* args: R1.p = TSO to kill, R2.p = Exception */

  /* If the target thread is currently blocking async exceptions,
   * we'll have to block until it's ready to accept them.
   */
  if (R1.t->blocked_exceptions != NULL) {

	/* ToDo (SMP): locking if destination thread is currently
	 * running...
	 */
	CurrentTSO->link = R1.t->blocked_exceptions;
	R1.t->blocked_exceptions = CurrentTSO;

        CurrentTSO->why_blocked = BlockedOnException;
        CurrentTSO->block_info.tso = R1.t;

        BLOCK( R1_PTR | R2_PTR, killThreadzh_fast );
  }

  /* Killed threads turn into zombies, which might be garbage
   * collected at a later date.  That's why we don't have to
   * explicitly remove them from any queues they might be on.
   */

  /* We might have killed ourselves.  In which case, better be *very*
   * careful.  If the exception killed us, then return to the scheduler.
   * If the exception went to a catch frame, we'll just continue from
   * the handler.
   */
  if (R1.t == CurrentTSO) {
	SaveThreadState();	/* inline! */
	STGCALL2(raiseAsync, R1.t, R2.cl);
	if (CurrentTSO->whatNext == ThreadKilled) {
		R1.w = ThreadYielding;
		JMP_(StgReturn);
	}
	LoadThreadState();
	if (CurrentTSO->whatNext == ThreadEnterGHC) {
		R1.w = Sp[0];
		Sp++;
		JMP_(GET_ENTRY(R1.cl));
	} else {
	  	barf("killThreadzh_fast");
	}
  } else {
	STGCALL2(raiseAsync, R1.t, R2.cl);
  }

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

/* -----------------------------------------------------------------------------
   Catch frames
   -------------------------------------------------------------------------- */

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
#define CATCH_FRAME_BITMAP 7
#else
#define CATCH_FRAME_BITMAP 3
#endif

/* Catch frames are very similar to update frames, but when entering
 * one we just pop the frame off the stack and perform the correct
 * kind of return to the activation record underneath us on the stack.
 */

VEC_POLY_INFO_TABLE(catch_frame, CATCH_FRAME_BITMAP, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, CATCH_FRAME,, EF_);

/* -----------------------------------------------------------------------------
 * The catch infotable
 *
 * This should be exactly the same as would be generated by this STG code
 *
 * catch = {x,h} \n {} -> catch#{x,h}
 *
 * It is used in deleteThread when reverting blackholes.
 * -------------------------------------------------------------------------- */

INFO_TABLE(catch_info,catch_entry,2,0,FUN,,EF_,0,0);
STGFUN(catch_entry)
{
  FB_
  R2.cl = payloadCPtr(R1.cl,1); /* h */
  R1.cl = payloadCPtr(R1.cl,0); /* x */
  JMP_(catchzh_fast);
  FE_
}

FN_(catchzh_fast)
{
  StgCatchFrame *fp;
  FB_

    /* args: R1 = m, R2 = handler */
    STK_CHK_GEN(sizeofW(StgCatchFrame), R1_PTR | R2_PTR, catchzh_fast, );
    Sp -= sizeofW(StgCatchFrame);
    fp = (StgCatchFrame *)Sp;
    SET_HDR(fp,(StgInfoTable *)&catch_frame_info,CCCS);
    fp -> handler = R2.cl;
    fp -> exceptions_blocked = (CurrentTSO->blocked_exceptions != NULL);
    fp -> link = Su;
    Su = (StgUpdateFrame *)fp;
    TICK_CATCHF_PUSHED();
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    
  FE_
}      

/* -----------------------------------------------------------------------------
 * The raise infotable
 * 
 * This should be exactly the same as would be generated by this STG code
 *
 *   raise = {err} \n {} -> raise#{err}
 *
 * It is used in raisezh_fast to update thunks on the update list
 * -------------------------------------------------------------------------- */

INFO_TABLE(raise_info,raise_entry,1,0,FUN,,EF_,0,0);
STGFUN(raise_entry)
{
  FB_
  R1.cl = R1.cl->payload[0];
  JMP_(raisezh_fast);
  FE_
}

FN_(raisezh_fast)
{
  StgClosure *handler;
  StgUpdateFrame *p;
  StgClosure *raise_closure;
  FB_
    /* args : R1 = error */


#if defined(PROFILING)

    /* Debugging tool: on raising an  exception, show where we are. */

    /* ToDo: currently this is a hack.  Would be much better if
     * the info was only displayed for an *uncaught* exception.
     */
    if (RtsFlags.ProfFlags.showCCSOnException) {
      STGCALL2(print_ccs,stderr,CCCS);
    }

#endif

    p = Su;

    /* This closure represents the expression 'raise# E' where E
     * is the exception raise.  It is used to overwrite all the
     * thunks which are currently under evaluataion.
     */
    raise_closure = (StgClosure *)RET_STGCALL1(P_,allocate,
					       sizeofW(StgClosure)+1);
    raise_closure->header.info = &raise_info;
    raise_closure->payload[0] = R1.cl;

    while (1) {

      switch (get_itbl(p)->type) {

      case UPDATE_FRAME:
	UPD_IND(p->updatee,raise_closure);
	p = p->link;
	continue;

      case SEQ_FRAME:
	p = ((StgSeqFrame *)p)->link;
	continue;

      case CATCH_FRAME:
	/* found it! */
	break;

      case STOP_FRAME:
	barf("raisezh_fast: STOP_FRAME");

      default:
	barf("raisezh_fast: weird activation record");
      }
      
      break;

    }
    
    /* Ok, p points to the enclosing CATCH_FRAME.  Pop everything down to
     * and including this frame, update Su, push R1, and enter the handler.
     */
    Su = ((StgCatchFrame *)p)->link; 
    handler = ((StgCatchFrame *)p)->handler;
    
    Sp = (P_)p + sizeofW(StgCatchFrame) - 1;

    /* Restore the blocked/unblocked state for asynchronous exceptions
     * at the CATCH_FRAME.  
     *
     * If exceptions were unblocked, arrange that they are unblocked
     * again after executing the handler by pushing an
     * unblockAsyncExceptions_ret stack frame.
     */
    if (! ((StgCatchFrame *)p)->exceptions_blocked) {
      *(Sp--) = (W_)&unblockAsyncExceptionszh_ret_info;
    }

    /* Ensure that async excpetions are blocked when running the handler.
    */
    if (CurrentTSO->blocked_exceptions == NULL) {
      CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
    }

    /* Enter the handler, passing the exception value as an argument.
     */
    *Sp = R1.w;
    TICK_ENT_VIA_NODE();
    R1.cl = handler;
    JMP_(GET_ENTRY(R1.cl));

  FE_
}
