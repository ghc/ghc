/* -----------------------------------------------------------------------------
 * $Id: StgStartup.hc,v 1.20 2002/12/11 15:36:54 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Code for starting, stopping and restarting threads.
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
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
    ASSERT(SpLim != (P_)0);			\
    ASSERT(HpLim != (P_)0);			\
    ASSERT(SpLim - RESERVED_STACK_WORDS <= Sp); \
    ASSERT(HpLim >= Hp);

/* -----------------------------------------------------------------------------
   Returning from the STG world.

   This is a polymorphic return address, meaning that any old constructor
   can be returned, we don't care (actually, it's probably going to be
   an IOok constructor, which will indirect through the vector table
   slot 0).
   -------------------------------------------------------------------------- */

EXTFUN(stg_stop_thread_ret);

#if defined(PROFILING)
#define STOP_THREAD_BITMAP 3
#define STOP_THREAD_WORDS  2
#else
#define STOP_THREAD_BITMAP 0
#define STOP_THREAD_WORDS  0
#endif

/* VEC_POLY_INFO expects to see these names - but they should all be the same. */
#define stg_stop_thread_0_ret stg_stop_thread_ret 
#define stg_stop_thread_1_ret stg_stop_thread_ret 
#define stg_stop_thread_2_ret stg_stop_thread_ret 
#define stg_stop_thread_3_ret stg_stop_thread_ret 
#define stg_stop_thread_4_ret stg_stop_thread_ret 
#define stg_stop_thread_5_ret stg_stop_thread_ret 
#define stg_stop_thread_6_ret stg_stop_thread_ret 
#define stg_stop_thread_7_ret stg_stop_thread_ret 

VEC_POLY_INFO_TABLE( stg_stop_thread,
		     MK_SMALL_BITMAP(STOP_THREAD_WORDS, STOP_THREAD_BITMAP),
		     0,0,0,STOP_FRAME,,EF_);

STGFUN(stg_stop_thread_ret)
{
    FB_
    // 
    // The final exit.
    //
    // The top-top-level closures (e.g., "main") are of type "IO a".
    // When entered, they perform an IO action and return an 'a' in R1.
    //
    // We save R1 on top of the stack where the scheduler can find it,
    // tidy up the registers and return to the scheduler.
    //
    // We Leave the stack looking like this:
    //
    //      	+----------------+
    //          |      -------------------> return value
    //      	+----------------+
    //      	| stg_enter_info |
    //      	+----------------+
    //
    // The stg_enter_info is just a dummy info table so that the
    // garbage collector can understand the stack (there must always
    // be an info table on top of the stack).
    //

    Sp += sizeofW(StgStopFrame) - 2;
    Sp[1] = R1.w;
    Sp[0] = (W_)&stg_enter_info;

    CurrentTSO->what_next = ThreadComplete;

    SaveThreadState();	// inline!

    // R1 contains the return value of the thread
    R1.i = ThreadFinished;

    JMP_(StgReturn);
    FE_
}

/* -----------------------------------------------------------------------------
   Start a thread from the scheduler by returning to the address on
   the top of the stack.  This is used for all entries to STG code
   from C land.
   -------------------------------------------------------------------------- */

STGFUN(stg_returnToStackTop)
{
  FB_
  LoadThreadState();
  CHECK_SENSIBLE_REGS();
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

/* -----------------------------------------------------------------------------
    Strict IO application - performing an IO action and entering its result.
    
    rts_evalIO() lets you perform Haskell IO actions from outside of
    Haskell-land, returning back to you their result. Want this result
    to be evaluated to WHNF by that time, so that we can easily get at
    the int/char/whatever using the various get{Ty} functions provided
    by the RTS API.

    forceIO takes care of this, performing the IO action and entering the
    results that comes back.
    ------------------------------------------------------------------------- */

INFO_TABLE_RET( stg_forceIO_info,stg_forceIO_ret,
		MK_SMALL_BITMAP(0/*size*/, 0/*BITMAP*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

#ifdef REG_R1
STGFUN(stg_forceIO_ret)
{
  FB_
  Sp++;
  ENTER();
  FE_
}
#else
STGFUN(stg_forceIO_ret)
{
  FB_
  R1.w = Sp[0];
  Sp += 2;
  ENTER();
  FE_
}
#endif

/* -----------------------------------------------------------------------------
    Non-strict IO application.

    This stack frame works like stg_forceIO_info except that it
    doesn't evaluate the return value.  We need the layer because the
    return convention for an IO action differs depending on whether R1
    is a register or not.
    ------------------------------------------------------------------------- */

INFO_TABLE_RET( stg_noforceIO_info,stg_noforceIO_ret,
		MK_SMALL_BITMAP(0/*size*/, 0/*BITMAP*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

#ifdef REG_R1
STGFUN(stg_noforceIO_ret)
{
  FB_
  Sp++;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}
#else
STGFUN(stg_noforceIO_ret)
{
  FB_
  R1.w = Sp[0];
  Sp += 2;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}
#endif

/* -----------------------------------------------------------------------------
   Special STG entry points for module registration.
   -------------------------------------------------------------------------- */

extern F_ *init_stack;

STGFUN(stg_init_ret)
{
  FB_
  JMP_(StgReturn);
  FE_
}

/* On entry to stg_init:
 *    init_stack[0] = &stg_init_ret;
 *    init_stack[1] = __stginit_Something;
 */
STGFUN(stg_init)
{
  FB_
  Sp = BaseReg->rSp;
  JMP_(POP_INIT_STACK());
  FE_
}
