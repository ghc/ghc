/* -----------------------------------------------------------------------------
 * $Id: HeapStackCheck.hc,v 1.11 2000/01/13 14:34:03 hwloidl Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Canned Heap-Check and Stack-Check sequences.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"   	/* for CurrentTSO */
#include "StgRun.h"	/* for StgReturn and register saving */
#include "Schedule.h"   /* for context_switch */
#include "HeapStackCheck.h"

/* Stack/Heap Check Failure
 * ------------------------
 *
 * On discovering that a stack or heap check has failed, we do the following:
 *
 *    - If the context_switch flag is set, indicating that there are more
 *      threads waiting to run, we yield to the scheduler 
 *	(return ThreadYeilding).
 *
 *    - If Hp > HpLim, we've had a heap check failure.  This means we've
 *	come to the end of the current heap block, so we try to chain
 *	another block on with ExtendNursery().  
 *
 *	     - If this succeeds, we carry on without returning to the 
 *	       scheduler.  
 *
 *	     - If it fails, we return to the scheduler claiming HeapOverflow
 *	       so that a garbage collection can be performed.
 *
 *    - If Hp <= HpLim, it must have been a stack check that failed.  In
 *	which case, we return to the scheduler claiming StackOverflow, the
 *	scheduler will either increase the size of our stack, or flag
 *	an error if the stack is already too big.
 *
 * The effect of checking for context switch only in the heap/stack check
 * failure code is that we'll switch threads after the current thread has
 * reached the end of its heap block.  If a thread isn't allocating
 * at all, it won't yield.  Hopefully this won't be a problem in practice.
 */
 
/* Remember that the return address is *removed* when returning to a
 * ThreadRunGHC thread.
 */


#define GC_GENERIC					\
  if (Hp > HpLim) {					\
    if (ExtendNursery(Hp,HpLim)) {			\
	if (context_switch) {				\
	    R1.i = ThreadYielding;			\
	} else {					\
 	   Sp++;					\
	   JMP_(ENTRY_CODE(Sp[-1]));			\
	}						\
    } else {						\
      R1.i = HeapOverflow;				\
    }							\
  } else {						\
    R1.i = StackOverflow;				\
  }							\
  SaveThreadState();					\
  CurrentTSO->whatNext = ThreadRunGHC;			\
  JMP_(StgReturn);

#define GC_ENTER					\
  if (Hp > HpLim) {					\
    if (ExtendNursery(Hp,HpLim)) {			\
	if (context_switch) {				\
	    R1.i = ThreadYielding;			\
	} else {					\
 	   R1.w = *Sp;					\
  	   Sp++;					\
	   JMP_(ENTRY_CODE(*R1.p));			\
	}						\
    } else {						\
      R1.i = HeapOverflow;				\
    }							\
  } else {						\
    R1.i = StackOverflow;				\
  }							\
  SaveThreadState();					\
  CurrentTSO->whatNext = ThreadEnterGHC;		\
  JMP_(StgReturn);

#define HP_GENERIC			\
  SaveThreadState();			\
  CurrentTSO->whatNext = ThreadRunGHC;	\
  R1.i = HeapOverflow;			\
  JMP_(StgReturn);

#define STK_GENERIC 			\
  SaveThreadState();			\
  CurrentTSO->whatNext = ThreadRunGHC;	\
  R1.i = StackOverflow;			\
  JMP_(StgReturn);

#define YIELD_GENERIC			\
  SaveThreadState();			\
  CurrentTSO->whatNext = ThreadRunGHC;	\
  R1.i = ThreadYielding;		\
  JMP_(StgReturn);

#define YIELD_TO_HUGS			  \
  SaveThreadState();			  \
  CurrentTSO->whatNext = ThreadEnterHugs; \
  R1.i = ThreadYielding;		  \
  JMP_(StgReturn);

#define BLOCK_GENERIC			\
  SaveThreadState();			\
  CurrentTSO->whatNext = ThreadRunGHC;	\
  R1.i = ThreadBlocked;			\
  JMP_(StgReturn);

#define BLOCK_ENTER			\
  SaveThreadState();			\
  CurrentTSO->whatNext = ThreadEnterGHC;\
  R1.i = ThreadBlocked;			\
  JMP_(StgReturn);

/* -----------------------------------------------------------------------------
   Heap Checks
   -------------------------------------------------------------------------- */

/*
 * This one is used when we want to *enter* the top thing on the stack
 * when we return, instead of the just returning to an address.  See
 * UpdatePAP for an example.
 */

EXTFUN(stg_gc_entertop)
{
  FB_
  GC_ENTER
  FE_
}

/* -----------------------------------------------------------------------------
   Heap checks in non-top-level thunks/functions.

   In these cases, node always points to the function closure.  This gives
   us an easy way to return to the function: just leave R1 on the top of
   the stack, and have the scheduler enter it to return.

   There are canned sequences for 'n' pointer values in registers.
   -------------------------------------------------------------------------- */

EXTFUN(stg_gc_enter_1)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  GC_ENTER
  FE_
}

EXTFUN(stg_gc_enter_1_hponly)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  R1.i = HeapOverflow;
  SaveThreadState();
  CurrentTSO->whatNext = ThreadEnterGHC;
  JMP_(StgReturn);
  FE_
}

/*- 2 Regs--------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_2)
{
  FB_
  Sp -= 2;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 3 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_3)
{
  FB_
  Sp -= 3;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 4 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_4)
{
  FB_
  Sp -= 4;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 5 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_5)
{
  FB_
  Sp -= 5;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 6 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_6)
{
  FB_
  Sp -= 6;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 7 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_7)
{
  FB_
  Sp -= 7;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

/*- 8 Regs -------------------------------------------------------------------*/

EXTFUN(stg_gc_enter_8)
{
  FB_
  Sp -= 8;
  Sp[7] = R8.w;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  GC_ENTER;
  FE_
}

#if defined(GRAN)
/*
  ToDo: merge the block and yield macros, calling something like BLOCK(N)
        at the end;
*/

/* 
   Should we actually ever do a yield in such a case?? -- HWL
*/
EXTFUN(gran_yield_0)
{
  FB_
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

EXTFUN(gran_yield_1)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 2 Regs--------------------------------------------------------------------*/

EXTFUN(gran_yield_2)
{
  FB_
  Sp -= 2;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 3 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_3)
{
  FB_
  Sp -= 3;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 4 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_4)
{
  FB_
  Sp -= 4;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 5 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_5)
{
  FB_
  Sp -= 5;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 6 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_6)
{
  FB_
  Sp -= 6;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 7 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_7)
{
  FB_
  Sp -= 7;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

/*- 8 Regs -------------------------------------------------------------------*/

EXTFUN(gran_yield_8)
{
  FB_
  Sp -= 8;
  Sp[7] = R8.w;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadYielding;
  JMP_(StgReturn);
  FE_
}

// the same routines but with a block rather than a yield

EXTFUN(gran_block_1)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 2 Regs--------------------------------------------------------------------*/

EXTFUN(gran_block_2)
{
  FB_
  Sp -= 2;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 3 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_3)
{
  FB_
  Sp -= 3;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 4 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_4)
{
  FB_
  Sp -= 4;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 5 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_5)
{
  FB_
  Sp -= 5;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 6 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_6)
{
  FB_
  Sp -= 6;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 7 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_7)
{
  FB_
  Sp -= 7;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

/*- 8 Regs -------------------------------------------------------------------*/

EXTFUN(gran_block_8)
{
  FB_
  Sp -= 8;
  Sp[7] = R8.w;
  Sp[6] = R7.w;
  Sp[5] = R6.w;
  Sp[4] = R5.w;
  Sp[3] = R4.w;
  Sp[2] = R3.w;
  Sp[1] = R2.w;
  Sp[0] = R1.w;
  SaveThreadState();					
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

#endif

#if 0 && defined(PAR)

/*
  Similar to stg_block_1 (called via StgMacro BLOCK_NP) but separates the
  saving of the thread state from the actual jump via an StgReturn.
  We need this separation because we call RTS routines in blocking entry codes
  before jumping back into the RTS (see parallel/FetchMe.hc).
*/

EXTFUN(par_block_1_no_jump)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  SaveThreadState();					
  FE_
}

EXTFUN(par_jump)
{
  FB_
  CurrentTSO->whatNext = ThreadEnterGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

#endif

/* -----------------------------------------------------------------------------
   For a case expression on a polymorphic or function-typed object, if
   the default branch (there can only be one branch) of the case fails
   a heap-check, instead of using stg_gc_enter_1 as normal, we must
   push a new SEQ frame on the stack, followed by the object returned.  

   Otherwise, if the object is a function, it won't return to the
   correct activation record on returning from garbage collection.  It will
   assume it has some arguments and apply itself.
   -------------------------------------------------------------------------- */

EXTFUN(stg_gc_seq_1)
{
  FB_
  Sp -= 1 + sizeofW(StgSeqFrame);
  PUSH_SEQ_FRAME(Sp+1);
  *Sp = R1.w;
  GC_ENTER;
  FE_
}

/* -----------------------------------------------------------------------------
   Heap checks in Primitive case alternatives

   A primitive case alternative is entered with a value either in 
   R1, FloatReg1 or D1 depending on the return convention.  All the
   cases are covered below.
   -------------------------------------------------------------------------- */

/*-- No regsiters live (probably a void return) ----------------------------- */

/* If we change the policy for thread startup to *not* remove the
 * return address from the stack, we can get rid of this little
 * function/info table...  
 */
INFO_TABLE_SRT_BITMAP(stg_gc_noregs_ret_info, stg_gc_noregs_ret, 0/*BITMAP*/, 
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_noregs_ret)
{
  FB_
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_noregs)
{
  FB_
  Sp -= 1;
  Sp[0] = (W_)&stg_gc_noregs_ret_info;
  GC_GENERIC
  FE_
}

/*-- R1 is boxed/unpointed -------------------------------------------------- */

INFO_TABLE_SRT_BITMAP(stg_gc_unpt_r1_info, stg_gc_unpt_r1_entry, 0/*BITMAP*/, 
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_unpt_r1_entry)
{
  FB_
  R1.w = Sp[0];
  Sp += 1;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_unpt_r1)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_gc_unpt_r1_info;
  GC_GENERIC
  FE_
}

/*-- R1 is unboxed -------------------------------------------------- */

INFO_TABLE_SRT_BITMAP(stg_gc_unbx_r1_info, stg_gc_unbx_r1_entry, 1/*BITMAP*/,
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);
/* the 1 is a bitmap - i.e. 1 non-pointer word on the stack. */

EXTFUN(stg_gc_unbx_r1_entry)
{
  FB_
  R1.w = Sp[0];
  Sp += 1;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_unbx_r1)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_gc_unbx_r1_info;
  GC_GENERIC
  FE_
}

/*-- F1 contains a float ------------------------------------------------- */

INFO_TABLE_SRT_BITMAP(stg_gc_f1_info, stg_gc_f1_entry, 1/*BITMAP*/,
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_f1_entry)
{
  FB_
  F1 = PK_FLT(Sp);
  Sp += 1;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_f1)
{
  FB_
  Sp -= 2;
  ASSIGN_FLT(Sp+1, F1);
  Sp[0] = (W_)&stg_gc_f1_info;
  GC_GENERIC
  FE_
}

/*-- D1 contains a double ------------------------------------------------- */

/* we support doubles of either 1 or 2 words in size */

#if SIZEOF_DOUBLE == SIZEOF_VOID_P
#  define DBL_BITMAP 1
#else
#  define DBL_BITMAP 3
#endif 

INFO_TABLE_SRT_BITMAP(stg_gc_d1_info, stg_gc_d1_entry, DBL_BITMAP,
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_d1_entry)
{
  FB_
  D1 = PK_DBL(Sp);
  Sp += sizeofW(StgDouble);
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_d1)
{
  FB_
  Sp -= 1 + sizeofW(StgDouble);
  ASSIGN_DBL(Sp+1,D1);
  Sp[0] = (W_)&stg_gc_d1_info;
  GC_GENERIC
  FE_
}

/* -----------------------------------------------------------------------------
   Heap checks for unboxed tuple case alternatives

   The story is: 

      - for an unboxed tuple with n components, we rearrange the components
	with pointers first followed by non-pointers. (NB: not done yet)
 
      - The first k components are allocated registers, where k is the
        number of components that will fit in real registers.

      - The rest are placed on the stack, with space left for tagging
        of the non-pointer block if necessary.

      - On failure of a heap check:
		- the tag is filled in if necessary,
		- we load Ri with the address of the continuation,
		  where i is the lowest unused vanilla register.
		- jump to 'stg_gc_ut_x_y' where x is the number of pointer
		  registers and y the number of non-pointers.
		- if the required canned sequence isn't available, it will
		  have to be generated at compile-time by the code
		  generator (this will probably happen if there are
		  floating-point values, for instance).
  
   For now, just deal with R1, hence R2 contains the sequel address.
   -------------------------------------------------------------------------- */

/*---- R1 contains a pointer: ------ */

INFO_TABLE_SRT_BITMAP(stg_gc_ut_1_0_info, stg_gc_ut_1_0_entry, 1/*BITMAP*/, 
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_ut_1_0_entry)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
  JMP_(ENTRY_CODE(Sp[-2]));
  FE_
}

EXTFUN(stg_gc_ut_1_0)
{
  FB_
  Sp -= 3;
  Sp[2] = R1.w;
  Sp[1] = R2.w;
  Sp[0] = (W_)&stg_gc_ut_1_0_info;
  GC_GENERIC
  FE_
}

/*---- R1 contains a non-pointer: ------ */

INFO_TABLE_SRT_BITMAP(stg_gc_ut_0_1_info, stg_gc_ut_0_1_entry, 3/*BITMAP*/, 
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		      RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_ut_0_1_entry)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
  JMP_(ENTRY_CODE(Sp[-2]));
  FE_
}

EXTFUN(stg_gc_ut_0_1)
{
  FB_
  Sp -= 3;
  Sp[0] = (W_)&stg_gc_ut_0_1_info;
  Sp[1] = R2.w;
  Sp[2] = R1.w;
  GC_GENERIC
  FE_
}

/* -----------------------------------------------------------------------------
   Standard top-level fast-entry heap checks.

   - we want to make the stack look like it should at the slow entry
     point for the function.  That way we can just push the slow
     entry point on the stack and return using ThreadRunGHC.

   - The compiler will generate code to fill in any tags on the stack,
     in case we arrived directly at the fast entry point and these tags
     aren't present.

   - The rest is hopefully handled by jumping to a canned sequence.
     We currently have canned sequences for 0-8 pointer registers.  If
     any registers contain non-pointers, we must reduce to an all-pointers
     situation by pushing as many registers on the stack as necessary.

     eg. if R1, R2 contain pointers and R3 contains a word, the heap check
         failure sequence looks like this:

		Sp[-1] = R3.w;
	 	Sp[-2] = WORD_TAG;
		Sp -= 2;
		JMP_(stg_chk_2)

	  after pushing R3, we have pointers in R1 and R2 which corresponds
	  to the 2-pointer canned sequence.

  -------------------------------------------------------------------------- */

/*- 0 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_0)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  GC_GENERIC;
  FE_
}

/*- 1 Reg --------------------------------------------------------------------*/

EXTFUN(stg_chk_1)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = R2.w;
  GC_GENERIC;
  FE_
}

/*- 1 Reg (non-ptr) ----------------------------------------------------------*/

EXTFUN(stg_chk_1n)
{
  FB_
  Sp -= 3;
  Sp[2] = R1.w;
  Sp[1] = WORD_TAG; /* ToDo: or maybe its an int? */
  Sp[0] = R2.w;
  GC_GENERIC;
  FE_
}

/*- 2 Regs--------------------------------------------------------------------*/

EXTFUN(stg_chk_2)
{
  FB_
  Sp -= 3;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R3.w;
  GC_GENERIC;
  FE_
}

/*- 3 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_3)
{
  FB_
  Sp -= 4;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R4.w;
  GC_GENERIC;
  FE_
}

/*- 4 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_4)
{
  FB_
  Sp -= 5;
  Sp[4] = R4.w;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R5.w;
  GC_GENERIC;
  FE_
}

/*- 5 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_5)
{
  FB_
  Sp -= 6;
  Sp[5] = R5.w;
  Sp[4] = R4.w;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R6.w;
  GC_GENERIC;
  FE_
}

/*- 6 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_6)
{
  FB_
  Sp -= 7;
  Sp[6] = R6.w;
  Sp[5] = R5.w;
  Sp[4] = R4.w;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R7.w;
  GC_GENERIC;
  FE_
}

/*- 7 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_7)
{
  FB_
  Sp -= 8;
  Sp[7] = R7.w;
  Sp[6] = R6.w;
  Sp[5] = R5.w;
  Sp[4] = R4.w;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R8.w;
  GC_GENERIC;
  FE_
}

/*- 8 Regs -------------------------------------------------------------------*/

EXTFUN(stg_chk_8)
{
  FB_
  Sp -= 9;
  Sp[8] = R8.w;
  Sp[7] = R7.w;
  Sp[6] = R6.w;
  Sp[5] = R5.w;
  Sp[4] = R4.w;
  Sp[3] = R3.w;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = R9.w;
  GC_GENERIC;
  FE_
}

/* -----------------------------------------------------------------------------
   Generic Heap Check Code.

   Called with Liveness mask in R9,  Return address in R10.
   Stack must be consistent (tagged, and containing all necessary info pointers
   to relevant SRTs).

   We also define an stg_gen_yield here, because it's very similar.
   -------------------------------------------------------------------------- */

#if SIZEOF_DOUBLE > SIZEOF_VOID_P

#define RESTORE_EVERYTHING			\
    D2   = PK_DBL(Sp+16);			\
    D1   = PK_DBL(Sp+14);			\
    F4   = PK_FLT(Sp+13);			\
    F3   = PK_FLT(Sp+12);			\
    F2   = PK_FLT(Sp+11);			\
    F1   = PK_FLT(Sp+10);			\
    R8.w = Sp[9];				\
    R7.w = Sp[8];				\
    R6.w = Sp[7];				\
    R5.w = Sp[6];				\
    R4.w = Sp[5];				\
    R3.w = Sp[4];				\
    R2.w = Sp[3];				\
    R1.w = Sp[2];				\
    Sp += 18;

#define RET_OFFSET (-17)

#define SAVE_EVERYTHING				\
    ASSIGN_DBL(Sp-2,D2);			\
    ASSIGN_DBL(Sp-4,D1);			\
    ASSIGN_FLT(Sp-5,F4);			\
    ASSIGN_FLT(Sp-6,F3);			\
    ASSIGN_FLT(Sp-7,F2);			\
    ASSIGN_FLT(Sp-8,F1);			\
    Sp[-9]  = R8.w;				\
    Sp[-10] = R7.w;				\
    Sp[-11] = R6.w;				\
    Sp[-12] = R5.w;				\
    Sp[-13] = R4.w;				\
    Sp[-14] = R3.w;				\
    Sp[-15] = R2.w;				\
    Sp[-16] = R1.w;				\
    Sp[-17] = R10.w;    /* return address */	\
    Sp[-18] = R9.w;     /* liveness mask  */	\
    Sp[-19] = (W_)&stg_gen_chk_info;		\
    Sp -= 19;

#else

#define RESTORE_EVERYTHING			\
    D2   = PK_DBL(Sp+15);			\
    D1   = PK_DBL(Sp+14);			\
    F4   = PK_FLT(Sp+13);			\
    F3   = PK_FLT(Sp+12);			\
    F2   = PK_FLT(Sp+11);			\
    F1   = PK_FLT(Sp+10);			\
    R8.w = Sp[9];				\
    R7.w = Sp[8];				\
    R6.w = Sp[7];				\
    R5.w = Sp[6];				\
    R4.w = Sp[5];				\
    R3.w = Sp[4];				\
    R2.w = Sp[3];				\
    R1.w = Sp[2];				\
    Sp += 16;

#define RET_OFFSET (-15)

#define SAVE_EVERYTHING				\
    ASSIGN_DBL(Sp-1,D2);			\
    ASSIGN_DBL(Sp-2,D1);			\
    ASSIGN_FLT(Sp-3,F4);			\
    ASSIGN_FLT(Sp-4,F3);			\
    ASSIGN_FLT(Sp-5,F2);			\
    ASSIGN_FLT(Sp-6,F1);			\
    Sp[-7]  = R8.w;				\
    Sp[-8]  = R7.w;				\
    Sp[-9]  = R6.w;				\
    Sp[-10] = R5.w;				\
    Sp[-11] = R4.w;				\
    Sp[-12] = R3.w;				\
    Sp[-13] = R2.w;				\
    Sp[-14] = R1.w;				\
    Sp[-15] = R10.w;    /* return address */	\
    Sp[-16] = R9.w;     /* liveness mask  */	\
    Sp[-17] = (W_)&stg_gen_chk_info;		\
    Sp -= 17;

#endif

INFO_TABLE_SRT_BITMAP(stg_gen_chk_info, stg_gen_chk_ret, 0,
		      0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
	              RET_DYN,, EF_, 0, 0);

/* bitmap in the above info table is unused, the real one is on the stack. 
 */

FN_(stg_gen_chk_ret)
{
  FB_
  RESTORE_EVERYTHING;
  JMP_(Sp[RET_OFFSET]); /* NO ENTRY_CODE() - this is a direct ret address */
  FE_
}

FN_(stg_gen_chk)
{
  FB_
  SAVE_EVERYTHING;
  GC_GENERIC
  FE_
}	  

/*
 * stg_gen_hp is used by MAYBE_GC, where we can't use GC_GENERIC
 * because we've just failed doYouWantToGC(), not a standard heap
 * check.  GC_GENERIC would end up returning StackOverflow.
 */
FN_(stg_gen_hp)
{
  FB_
  SAVE_EVERYTHING;
  HP_GENERIC
  FE_
}	  

/* -----------------------------------------------------------------------------
   Yields
   -------------------------------------------------------------------------- */

FN_(stg_gen_yield)
{
  FB_
  SAVE_EVERYTHING;
  YIELD_GENERIC
  FE_
}

FN_(stg_yield_noregs)
{
  FB_
  Sp--;
  Sp[0] = (W_)&stg_gc_noregs_ret_info;
  YIELD_GENERIC;
  FE_
}

FN_(stg_yield_to_Hugs)
{
  FB_
  /* No need to save everything - no live registers */
  YIELD_TO_HUGS
  FE_
}

/* -----------------------------------------------------------------------------
   Blocks
   -------------------------------------------------------------------------- */

FN_(stg_gen_block)
{
  FB_
  SAVE_EVERYTHING;
  BLOCK_GENERIC
  FE_
}

FN_(stg_block_noregs)
{
  FB_
  Sp--;
  Sp[0] = (W_)&stg_gc_noregs_ret_info;
  BLOCK_GENERIC;
  FE_
}

FN_(stg_block_1)
{
  FB_
  Sp--;
  Sp[0] = R1.w;
  BLOCK_ENTER;
  FE_
}
