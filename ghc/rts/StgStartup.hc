/* -----------------------------------------------------------------------------
 * $Id: StgStartup.hc,v 1.3 1999/02/05 16:03:00 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Code for starting, stopping and restarting threads.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "StgRun.h" /* StgReturn */
#include "StgStartup.h"

/*
 * This module contains the two entry points and the final exit point
 * to/from the Haskell world.  We can enter either by:
 *
 *   a) returning to the address on the top of the stack, or
 *   b) entering the closure on the top of the stack
 *
 * the function stg_stop_thread_entry is the final exit for a
 * thread: it is the last return address on the stack.  It returns
 * to the scheduler marking the thread as finished.
 */

#define CHECK_SENSIBLE_REGS() \
    ASSERT(Hp != (P_)0);			\
    ASSERT(Sp != (P_)0);			\
    ASSERT(Su != (StgUpdateFrame *)0);		\
    ASSERT(SpLim != (P_)0);			\
    ASSERT(HpLim != (P_)0);			\
    ASSERT(Sp <= (P_)Su);			\
    ASSERT(SpLim - RESERVED_STACK_WORDS <= Sp); \
    ASSERT(HpLim >= Hp);

/* -----------------------------------------------------------------------------
   Returning from the STG world.

   This is a polymorphic return address, meaning that any old constructor
   can be returned, we don't care (actually, it's probably going to be
   an IOok constructor, which will indirect through the vector table
   slot 0).
   -------------------------------------------------------------------------- */

EXTFUN(stg_stop_thread_entry);

#ifdef PROFILING
#define STOP_THREAD_BITMAP 1
#else
#define STOP_THREAD_BITMAP 0
#endif

/* VEC_POLY_INFO expects to see these names - but they should all be the same. */
#define stg_stop_thread_0_entry stg_stop_thread_entry 
#define stg_stop_thread_1_entry stg_stop_thread_entry 
#define stg_stop_thread_2_entry stg_stop_thread_entry 
#define stg_stop_thread_3_entry stg_stop_thread_entry 
#define stg_stop_thread_4_entry stg_stop_thread_entry 
#define stg_stop_thread_5_entry stg_stop_thread_entry 
#define stg_stop_thread_6_entry stg_stop_thread_entry 
#define stg_stop_thread_7_entry stg_stop_thread_entry 

VEC_POLY_INFO_TABLE(stg_stop_thread,STOP_THREAD_BITMAP,0,0,0,STOP_FRAME);

STGFUN(stg_stop_thread_entry)
{
    FB_

    /* 
     * The final exit.
     *
     * The top-top-level closures (e.g., "main") are of type "IO a".
     * When entered, they perform an IO action and return an 'a' in R1.
     *
     * We save R1 on top of the stack where the scheduler can find it,
     * tidy up the registers and return to the scheduler.
    */

    /* Move Su just off the end of the stack, we're about to spam the
     * STOP_FRAME with the return value.
     */
    Su = stgCast(StgUpdateFrame*,Sp+1);  
    *stgCast(StgClosure**,Sp) = R1.cl;

    SaveThreadState();	/* inline! */

    /* R1 contains the return value of the thread */
    R1.p = (P_)ThreadFinished;

    JMP_(StgReturn);
    FE_
}

/* -----------------------------------------------------------------------------
   Start a thread from the scheduler by returning to the address on
   the top of the stack  (and popping the address).  This is used for
   returning to the slow entry point of a function after a garbage collection
   or re-schedule.  The slow entry point expects the stack to contain the
   pending arguments only.
   -------------------------------------------------------------------------- */

STGFUN(stg_returnToStackTop)
{
  FB_
  LoadThreadState();
  CHECK_SENSIBLE_REGS();
  Sp++;
  JMP_(Sp[-1]);
  FE_
}

/* -----------------------------------------------------------------------------
   Start a thread from the scheduler by entering the closure pointed
   to by the word on the top of the stack.
   -------------------------------------------------------------------------- */

STGFUN(stg_enterStackTop)
{
  FB_
  LoadThreadState();
  CHECK_SENSIBLE_REGS();
  /* don't count this enter for ticky-ticky profiling */
  R1.p = (P_)Sp[0];
  Sp++;
  JMP_(GET_ENTRY(R1.cl));
  FE_
}

  
/* -----------------------------------------------------------------------------
   Special STG entry points for module registration.
   -------------------------------------------------------------------------- */

#ifdef PROFILING

STGFUN(stg_register_ret)
{
  FB_
  JMP_(StgReturn);
  FE_
}

STGFUN(stg_register)
{
  EF_(_regMain);
  EF_(_regPrelude);
  FB_
  PUSH_REGISTER_STACK(stg_register_ret);
  PUSH_REGISTER_STACK(_regPrelude);
  JMP_(_regMain);
  FE_
}

/* PrelGHC doesn't really exist... */

START_REGISTER_CCS(_regPrelGHC);
END_REGISTER_CCS();

#endif
