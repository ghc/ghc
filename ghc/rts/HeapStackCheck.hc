/* -----------------------------------------------------------------------------
 * $Id: HeapStackCheck.hc,v 1.30 2003/04/22 16:25:10 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Canned Heap-Check and Stack-Check sequences.
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"
#include "Storage.h"   	/* for CurrentTSO */
#include "StgRun.h"	/* for StgReturn and register saving */
#include "Schedule.h"   /* for context_switch */
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Apply.h"

#include <stdio.h>

#ifdef mingw32_TARGET_OS
#include <stdlib.h>
#endif

/* Stack/Heap Check Failure
 * ------------------------
 *
 * On discovering that a stack or heap check has failed, we do the following:
 *
 *    - If the context_switch flag is set, indicating that there are more
 *      threads waiting to run, we yield to the scheduler 
 *	(return ThreadYielding).
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
  DEBUG_ONLY(heapCheckFail());				\
  if (Hp > HpLim) {					\
    Hp -= HpAlloc;					\
    if (HpAlloc <= BLOCK_SIZE_W && ExtendNursery(Hp,HpLim)) {\
	if (context_switch) {				\
	    R1.i = ThreadYielding;			\
	} else {					\
	   JMP_(ENTRY_CODE(Sp[0]));			\
	}						\
    } else {						\
      R1.i = HeapOverflow;				\
    }							\
  } else {						\
    R1.i = StackOverflow;				\
  }							\
  SaveThreadState();					\
  CurrentTSO->what_next = ThreadRunGHC;			\
  JMP_(StgReturn);

#define HP_GENERIC				\
  SaveThreadState();				\
  CurrentTSO->what_next = ThreadRunGHC;		\
  R1.i = HeapOverflow;				\
  JMP_(StgReturn);

#define YIELD_GENERIC				\
  SaveThreadState();				\
  CurrentTSO->what_next = ThreadRunGHC;		\
  R1.i = ThreadYielding;			\
  JMP_(StgReturn);

#define YIELD_TO_INTERPRETER			\
  SaveThreadState();				\
  CurrentTSO->what_next = ThreadInterpret;	\
  R1.i = ThreadYielding;			\
  JMP_(StgReturn);

#define BLOCK_GENERIC				\
  SaveThreadState();				\
  CurrentTSO->what_next = ThreadRunGHC;		\
  R1.i = ThreadBlocked;				\
  JMP_(StgReturn);

/* -----------------------------------------------------------------------------
   Heap checks in thunks/functions.

   In these cases, node always points to the function closure.  This gives
   us an easy way to return to the function: just leave R1 on the top of
   the stack, and have the scheduler enter it to return.

   There are canned sequences for 'n' pointer values in registers.
   -------------------------------------------------------------------------- */

INFO_TABLE_RET( stg_enter_info, stg_enter_ret, 
	       	MK_SMALL_BITMAP(1/*framesize*/, 0/*bitmap*/),
	       	0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);
EXTFUN(stg_enter_ret)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
  ENTER();
  FE_
}

EXTFUN(__stg_gc_enter_1)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_enter_info;
  GC_GENERIC
  FE_
}

#ifdef SMP
EXTFUN(stg_gc_enter_1_hponly)
{
  FB_
  Sp -= 1;
  Sp[0] = R1.w;
  R1.i = HeapOverflow;
  SaveThreadState();
  CurrentTSO->what_next = ThreadRunGHC;
  JMP_(StgReturn);
  FE_
}
#endif

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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
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
  CurrentTSO->what_next = ThreadRunGHC;		
  R1.i = ThreadBlocked;
  JMP_(StgReturn);
  FE_
}

#endif

/* -----------------------------------------------------------------------------
   Heap checks in Primitive case alternatives

   A primitive case alternative is entered with a value either in 
   R1, FloatReg1 or D1 depending on the return convention.  All the
   cases are covered below.
   -------------------------------------------------------------------------- */

/*-- No Registers live ------------------------------------------------------ */

EXTFUN(stg_gc_noregs)
{
  FB_
  GC_GENERIC
  FE_
}

/*-- void return ------------------------------------------------------------ */

INFO_TABLE_RET( stg_gc_void_info, stg_gc_void_ret, 
	       	MK_SMALL_BITMAP(0/*framesize*/, 0/*bitmap*/),
	       	0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_void_ret)
{
  FB_
  Sp += 1;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

/*-- R1 is boxed/unpointed -------------------------------------------------- */

INFO_TABLE_RET( stg_gc_unpt_r1_info, stg_gc_unpt_r1_ret, 
	       	MK_SMALL_BITMAP(1/*framesize*/, 0/*bitmap*/),
	       	0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_unpt_r1_ret)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
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

INFO_TABLE_RET(	stg_gc_unbx_r1_info, stg_gc_unbx_r1_ret, 
	       	MK_SMALL_BITMAP(1/*framesize*/, 1/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

/* the 1 is a bitmap - i.e. 1 non-pointer word on the stack. */

EXTFUN(stg_gc_unbx_r1_ret)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
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

INFO_TABLE_RET(	stg_gc_f1_info, stg_gc_f1_ret, 
	       	MK_SMALL_BITMAP(1/*framesize*/, 1/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_f1_ret)
{
  FB_
  F1 = PK_FLT(Sp+1);
  Sp += 2;
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
#  define DBL_WORDS  1
#else
#  define DBL_BITMAP 3
#  define DBL_WORDS  2
#endif 

INFO_TABLE_RET(	stg_gc_d1_info, stg_gc_d1_ret, 
	       	MK_SMALL_BITMAP(DBL_WORDS/*framesize*/, DBL_BITMAP/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_d1_ret)
{
  FB_
  D1 = PK_DBL(Sp+1);
  Sp += 1 + sizeofW(StgDouble);
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


/*-- L1 contains an int64 ------------------------------------------------- */

/* we support int64s of either 1 or 2 words in size */

#if SIZEOF_VOID_P == 8
#  define LLI_BITMAP 1
#  define LLI_WORDS  1
#else
#  define LLI_BITMAP 3
#  define LLI_WORDS  2
#endif 

INFO_TABLE_RET( stg_gc_l1_info, stg_gc_l1_ret, 
	       	MK_SMALL_BITMAP(LLI_WORDS/*framesize*/, LLI_BITMAP/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_gc_l1_ret)
{
  FB_
  L1 = PK_Int64(Sp+1);
  Sp += 1 + sizeofW(StgWord64);
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

EXTFUN(stg_gc_l1)
{
  FB_
  Sp -= 1 + sizeofW(StgWord64);
  ASSIGN_Int64(Sp+1,L1);
  Sp[0] = (W_)&stg_gc_l1_info;
  GC_GENERIC
  FE_
}

/*-- Unboxed tuple return, one pointer (unregisterised build only) ---------- */

INFO_TABLE_RET( stg_ut_1_0_unreg_info, stg_ut_1_0_unreg_ret, 
		MK_SMALL_BITMAP(1/*size*/, 0/*BITMAP*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, EF_, 0, 0);

EXTFUN(stg_ut_1_0_unreg_ret)
{
  FB_
  Sp++;
  /* one ptr is on the stack (Sp[0]) */
  JMP_(ENTRY_CODE(Sp[1]));
  FE_
}

/* -----------------------------------------------------------------------------
   Generic function entry heap check code.

   At a function entry point, the arguments are as per the calling convention,
   i.e. some in regs and some on the stack.  There may or may not be 
   a pointer to the function closure in R1 - if there isn't, then the heap
   check failure code in the function will arrange to load it.

   The function's argument types are described in its info table, so we
   can just jump to this bit of generic code to save away all the
   registers and return to the scheduler.

   This code arranges the stack like this:
	 
         |        ....         |
         |        args         |
	 +---------------------+
         |      f_closure      |
	 +---------------------+
         |        size         |
	 +---------------------+
         |   stg_gc_fun_info   |
	 +---------------------+

   The size is the number of words of arguments on the stack, and is cached
   in the frame in order to simplify stack walking: otherwise the size of
   this stack frame would have to be calculated by looking at f's info table.

   -------------------------------------------------------------------------- */

EXTFUN(__stg_gc_fun)
{
    StgWord size;
    StgFunInfoTable *info;
    FB_

    info = get_fun_itbl(R1.cl);

    // cache the size
    if (info->fun_type == ARG_GEN) {
	size = BITMAP_SIZE(info->bitmap);
    } else if (info->fun_type == ARG_GEN_BIG) {
	size = ((StgLargeBitmap *)info->bitmap)->size;
    } else {
	size = BITMAP_SIZE(stg_arg_bitmaps[info->fun_type]);
    }
    
#ifdef NO_ARG_REGS
    // we don't have to save any registers away
    Sp -= 3;
    Sp[2] = R1.w;
    Sp[1] = size;
    Sp[0] = (W_)&stg_gc_fun_info;
    GC_GENERIC
#else
    if (info->fun_type == ARG_GEN || info->fun_type == ARG_GEN_BIG) {
        // regs already saved by the heap check code
        Sp -= 3;
        Sp[2] = R1.w;
        Sp[1] = size;
        Sp[0] = (W_)&stg_gc_fun_info;
        DEBUG_ONLY(fprintf(stderr, "stg_fun_gc_gen(ARG_GEN)"););
        GC_GENERIC
    } else {
        JMP_(stg_stack_save_entries[info->fun_type]);
        // jumps to stg_gc_noregs after saving stuff
    }
#endif // !NO_ARG_REGS

    FE_
}   

/* -----------------------------------------------------------------------------
   Generic Apply (return point)

   The dual to stg_fun_gc_gen (above): this fragment returns to the
   function, passing arguments in the stack and in registers
   appropriately.  The stack layout is given above.
   -------------------------------------------------------------------------- */

INFO_TABLE_RET( stg_gc_fun_info,stg_gc_fun_ret,
	       	MK_SMALL_BITMAP(0/*framesize*/, 0/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_FUN,, EF_, 0, 0);

EXTFUN(stg_gc_fun_ret)
{
  FB_
  R1.w = Sp[2];
  Sp += 3;
#ifdef NO_ARG_REGS
  // there are no argument registers to load up, so we can just jump
  // straight to the function's entry point.
  JMP_(GET_ENTRY(R1.cl));
#else
  {
      StgFunInfoTable *info;

      info = get_fun_itbl(R1.cl);
      if (info->fun_type == ARG_GEN || info->fun_type == ARG_GEN_BIG) {
	  // regs already saved by the heap check code
	  DEBUG_ONLY(fprintf(stderr, "stg_gc_fun_ret(ARG_GEN)\n"););
	  JMP_(info->slow_apply);
      } else if (info->fun_type == ARG_BCO) {
	  // cover this case just to be on the safe side
	  Sp -= 2;
	  Sp[1] = R1.cl;
	  Sp[0] = (W_)&stg_apply_interp_info;
	  JMP_(stg_yield_to_interpreter);
      } else {
	  JMP_(stg_ap_stack_entries[info->fun_type]);
      }
  }
#endif
  FE_
}

/* -----------------------------------------------------------------------------
   Generic Heap Check Code.

   Called with Liveness mask in R9,  Return address in R10.
   Stack must be consistent (containing all necessary info pointers
   to relevant SRTs).

   See StgMacros.h for a description of the RET_DYN stack frame.

   We also define an stg_gen_yield here, because it's very similar.
   -------------------------------------------------------------------------- */

// For simplicity, we assume that SIZEOF_DOUBLE == 2*SIZEOF_VOID_P
// on a 64-bit machine, we'll end up wasting a couple of words, but
// it's not a big deal.

#define RESTORE_EVERYTHING			\
    L1   = PK_Word64(Sp+19);			\
    D2   = PK_DBL(Sp+17);			\
    D1   = PK_DBL(Sp+15);			\
    F4   = PK_FLT(Sp+14);			\
    F3   = PK_FLT(Sp+13);			\
    F2   = PK_FLT(Sp+12);			\
    F1   = PK_FLT(Sp+11);			\
    R8.w = Sp[10];				\
    R7.w = Sp[9];				\
    R6.w = Sp[8];				\
    R5.w = Sp[7];				\
    R4.w = Sp[6];				\
    R3.w = Sp[5];				\
    R2.w = Sp[4];				\
    R1.w = Sp[3];				\
    Sp += 21;

#define RET_OFFSET (-19)

#define SAVE_EVERYTHING				\
    Sp -= 21;					\
    ASSIGN_Word64(Sp+19,L1);			\
    ASSIGN_DBL(Sp+17,D2);			\
    ASSIGN_DBL(Sp+15,D1);			\
    ASSIGN_FLT(Sp+14,F4);			\
    ASSIGN_FLT(Sp+13,F3);			\
    ASSIGN_FLT(Sp+12,F2);			\
    ASSIGN_FLT(Sp+11,F1);			\
    Sp[10] = R8.w;				\
    Sp[9] = R7.w;				\
    Sp[8] = R6.w;				\
    Sp[7] = R5.w;				\
    Sp[6] = R4.w;				\
    Sp[5] = R3.w;				\
    Sp[4] = R2.w;				\
    Sp[3] = R1.w;				\
    Sp[2] = R10.w;    /* return address */	\
    Sp[1] = R9.w;     /* liveness mask  */	\
    Sp[0] = (W_)&stg_gc_gen_info;		\

INFO_TABLE_RET( stg_gc_gen_info, stg_gc_gen_ret, 
		0/*bitmap*/,
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_DYN,, EF_, 0, 0);

/* bitmap in the above info table is unused, the real one is on the stack. 
 */

FN_(stg_gc_gen_ret)
{
  FB_
  RESTORE_EVERYTHING;
  JMP_(Sp[RET_OFFSET]); /* No ENTRY_CODE() - this is an actual code ptr */
  FE_
}

FN_(stg_gc_gen)
{
  FB_
  SAVE_EVERYTHING;
  GC_GENERIC
  FE_
}	  

// A heap check at an unboxed tuple return point.  The return address
// is on the stack, and we can find it by using the offsets given
// to us in the liveness mask.
FN_(stg_gc_ut)
{
  FB_
  R10.w = (W_)ENTRY_CODE(Sp[GET_NONPTRS(R9.w) + GET_PTRS(R9.w)]);
  SAVE_EVERYTHING;
  GC_GENERIC
  FE_
}

/*
 * stg_gen_hp is used by MAYBE_GC, where we can't use GC_GENERIC
 * because we've just failed doYouWantToGC(), not a standard heap
 * check.  GC_GENERIC would end up returning StackOverflow.
 */
FN_(stg_gc_gen_hp)
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
  YIELD_GENERIC;
  FE_
}

/* -----------------------------------------------------------------------------
   Yielding to the interpreter... top of stack says what to do next.
   -------------------------------------------------------------------------- */

FN_(stg_yield_to_interpreter)
{
  FB_
  YIELD_TO_INTERPRETER;
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
  BLOCK_GENERIC;
  FE_
}

FN_(stg_block_1)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_enter_info;
  BLOCK_GENERIC;
  FE_
}

/* -----------------------------------------------------------------------------
 * takeMVar/putMVar-specific blocks
 *
 * Stack layout for a thread blocked in takeMVar:
 *      
 *       ret. addr
 *       ptr to MVar   (R1)
 *       stg_block_takemvar_info
 *
 * Stack layout for a thread blocked in putMVar:
 *      
 *       ret. addr
 *       ptr to Value  (R2)
 *       ptr to MVar   (R1)
 *       stg_block_putmvar_info
 *
 * See PrimOps.hc for a description of the workings of take/putMVar.
 * 
 * -------------------------------------------------------------------------- */

INFO_TABLE_RET( stg_block_takemvar_info,  stg_block_takemvar_ret,
	       	MK_SMALL_BITMAP(1/*framesize*/, 0/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, IF_, 0, 0);

IF_(stg_block_takemvar_ret)
{
  FB_
  R1.w = Sp[1];
  Sp += 2;
  JMP_(takeMVarzh_fast);
  FE_
}

FN_(stg_block_takemvar)
{
  FB_
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_block_takemvar_info;
  BLOCK_GENERIC;
  FE_
}

INFO_TABLE_RET( stg_block_putmvar_info,  stg_block_putmvar_ret,
	       	MK_SMALL_BITMAP(2/*framesize*/, 0/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, IF_, 0, 0);

IF_(stg_block_putmvar_ret)
{
  FB_
  R2.w = Sp[2];
  R1.w = Sp[1];
  Sp += 3;
  JMP_(putMVarzh_fast);
  FE_
}

FN_(stg_block_putmvar)
{
  FB_
  Sp -= 3;
  Sp[2] = R2.w;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_block_putmvar_info;
  BLOCK_GENERIC;
  FE_
}

#ifdef mingw32_TARGET_OS
INFO_TABLE_RET( stg_block_async_info,  stg_block_async_ret,
	       	MK_SMALL_BITMAP(0/*framesize*/, 0/*bitmap*/),
		0/*SRT*/, 0/*SRT_OFF*/, 0/*SRT_LEN*/, 
		RET_SMALL,, IF_, 0, 0);

IF_(stg_block_async_ret)
{
  StgAsyncIOResult* ares;
  int len,errC;
  FB_
  ares = CurrentTSO->block_info.async_result;
  len  = ares->len;
  errC = ares->errCode;
  CurrentTSO->block_info.async_result = NULL;
  STGCALL1(free,ares);
  R1.w = len;
  *Sp = (W_)errC;
  JMP_(ENTRY_CODE(Sp[1]));
  FE_
}

FN_(stg_block_async)
{
  FB_
  Sp -= 1;
  Sp[0] = (W_)&stg_block_async_info;
  BLOCK_GENERIC;
  FE_
}

#endif
