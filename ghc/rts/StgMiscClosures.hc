/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.hc,v 1.26 1999/07/06 16:40:27 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Entry code for various built-in closure types.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "StgMiscClosures.h"
#include "HeapStackCheck.h"   /* for stg_gen_yield */
#include "Storage.h"
#include "StoragePriv.h"
#include "ProfRts.h"

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

/* ToDo: make the printing of panics more Win32-friendly, i.e.,
 *       pop up some lovely message boxes (as well).
 */
#define DUMP_ERRMSG(msg) STGCALL1(fflush,stdout); STGCALL2(fprintf,stderr,msg)

/* -----------------------------------------------------------------------------
   Entry code for an indirection.

   This code assumes R1 is in a register for now.
   -------------------------------------------------------------------------- */

INFO_TABLE(IND_info,IND_entry,1,0,IND,,EF_,0,0);
STGFUN(IND_entry)
{
    FB_
    TICK_ENT_IND(Node);	/* tick */

    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(ENTRY_CODE(*R1.p));
    FE_
}

INFO_TABLE(IND_STATIC_info,IND_STATIC_entry,1,0,IND_STATIC,,EF_,0,0);
STGFUN(IND_STATIC_entry)
{
    FB_
    TICK_ENT_IND(Node);	/* tick */
  
    R1.p = (P_) ((StgIndStatic*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(ENTRY_CODE(*R1.p));
    FE_
}

INFO_TABLE(IND_PERM_info,IND_PERM_entry,1,1,IND_PERM,,EF_,0,0);
STGFUN(IND_PERM_entry)
{
    FB_
    /* Don't add INDs to granularity cost */
    /* Dont: TICK_ENT_IND(Node); for ticky-ticky; this ind is here only to help profiling */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    /* TICKY_TICKY && !PROFILING means PERM_IND *replaces* an IND, rather than being extra  */
    TICK_ENT_PERM_IND(R1.p); /* tick */
#endif

    /* Enter PAP cost centre -- lexical scoping only */
    ENTER_CCS_PAP_CL(R1.cl);

    /* For ticky-ticky, change the perm_ind to a normal ind on first
     * entry, so the number of ent_perm_inds is the number of *thunks*
     * entered again, not the number of subsequent entries.
     *
     * Since this screws up cost centres, we die if profiling and
     * ticky_ticky are on at the same time.  KSW 1999-01.
     */

#ifdef TICKY_TICKY
#  ifdef PROFILING
#    error Profiling and ticky-ticky do not mix at present!
#  endif  /* PROFILING */
    SET_INFO((StgInd*)R1.p,&IND_info);
#endif /* TICKY_TICKY */

    R1.p = (P_) ((StgInd*)R1.p)->indirectee;

    /* Dont: TICK_ENT_VIA_NODE(); for ticky-ticky; as above */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    TICK_ENT_VIA_NODE();
#endif

    JMP_(ENTRY_CODE(*R1.p));
    FE_
}  

INFO_TABLE(IND_OLDGEN_info,IND_OLDGEN_entry,1,1,IND_OLDGEN,,EF_,0,0);
STGFUN(IND_OLDGEN_entry)
{
    FB_
    TICK_ENT_IND(Node);	/* tick */
  
    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(ENTRY_CODE(*R1.p));
    FE_
}

INFO_TABLE(IND_OLDGEN_PERM_info,IND_OLDGEN_PERM_entry,1,1,IND_OLDGEN_PERM,,EF_,0,0);
STGFUN(IND_OLDGEN_PERM_entry)
{
    FB_
    /* Dont: TICK_ENT_IND(Node); for ticky-ticky; this ind is here only to help profiling */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    /* TICKY_TICKY && !PROFILING means PERM_IND *replaces* an IND, rather than being extra  */
    TICK_ENT_PERM_IND(R1.p); /* tick */
#endif
  
    /* Enter PAP cost centre -- lexical scoping only */
    ENTER_CCS_PAP_CL(R1.cl);

    /* see comment in IND_PERM */
#ifdef TICKY_TICKY
#  ifdef PROFILING
#    error Profiling and ticky-ticky do not mix at present!
#  endif  /* PROFILING */
    SET_INFO((StgInd*)R1.p,&IND_OLDGEN_info);
#endif /* TICKY_TICKY */

    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(ENTRY_CODE(*R1.p));
    FE_
}

/* -----------------------------------------------------------------------------
   Entry code for CAFs

   This code assumes R1 is in a register for now.
   -------------------------------------------------------------------------- */

INFO_TABLE(CAF_UNENTERED_info,CAF_UNENTERED_entry,1,3,CAF_UNENTERED,,EF_,0,0);
STGFUN(CAF_UNENTERED_entry)
{
    FB_
    /* ToDo: implement directly in GHC */
    Sp -= 1;
    Sp[0] = R1.w;
    JMP_(stg_yield_to_Hugs);
    FE_
}

/* 0,4 is entirely bogus; _do not_ rely on this info */
INFO_TABLE(CAF_ENTERED_info,CAF_ENTERED_entry,0,4,CAF_ENTERED,,EF_,0,0);
STGFUN(CAF_ENTERED_entry)
{
    FB_
    R1.p = (P_) ((StgCAF*)R1.p)->value; /* just a fancy indirection */
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    FE_
}

/* -----------------------------------------------------------------------------
   Entry code for a black hole.

   Entering a black hole normally causes a cyclic data dependency, but
   in the concurrent world, black holes are synchronization points,
   and they are turned into blocking queues when there are threads
   waiting for the evaluation of the closure to finish.
   -------------------------------------------------------------------------- */

/* Note: a black hole must be big enough to be overwritten with an
 * indirection/evacuee/catch.  Thus we claim it has 1 non-pointer word of
 * payload (in addition to the pointer word for the blocking queue), which 
 * should be big enough for an old-generation indirection.  
 */

INFO_TABLE(BLACKHOLE_info, BLACKHOLE_entry,0,2,BLACKHOLE,,EF_,0,0);
STGFUN(BLACKHOLE_entry)
{
  FB_
    TICK_ENT_BH();

    /* Change the BLACKHOLE into a BLACKHOLE_BQ */
    ((StgBlockingQueue *)R1.p)->header.info = &BLACKHOLE_BQ_info;
    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->link = (StgTSO *)&END_TSO_QUEUE_closure;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
    CurrentTSO->blocked_on = R1.cl;
    recordMutable((StgMutClosure *)R1.cl);

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
  FE_
}

INFO_TABLE(BLACKHOLE_BQ_info, BLACKHOLE_BQ_entry,1,1,BLACKHOLE_BQ,,EF_,0,0);
STGFUN(BLACKHOLE_BQ_entry)
{
  FB_
    TICK_ENT_BH();

    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->blocked_on = R1.cl;
    CurrentTSO->link = ((StgBlockingQueue *)R1.p)->blocking_queue;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
  FE_
}

/* identical to BLACKHOLEs except for the infotag */
INFO_TABLE(CAF_BLACKHOLE_info, CAF_BLACKHOLE_entry,0,2,CAF_BLACKHOLE,,EF_,0,0);
STGFUN(CAF_BLACKHOLE_entry)
{
  FB_
    TICK_ENT_BH();

    /* Change the BLACKHOLE into a BLACKHOLE_BQ */
    ((StgBlockingQueue *)R1.p)->header.info = &BLACKHOLE_BQ_info;
    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->link = (StgTSO *)&END_TSO_QUEUE_closure;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
    CurrentTSO->blocked_on = R1.cl;
    recordMutable((StgMutClosure *)R1.cl);

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
  FE_
}

#ifdef TICKY_TICKY
INFO_TABLE(SE_BLACKHOLE_info, SE_BLACKHOLE_entry,0,2,SE_BLACKHOLE,,EF_,0,0);
STGFUN(SE_BLACKHOLE_entry)
{
  FB_
    STGCALL1(fflush,stdout);						
    STGCALL3(fprintf,stderr,"SE_BLACKHOLE at %p entered!\n",R1.p);
    STGCALL1(raiseError, errorHandler);
    stg_exit(EXIT_FAILURE); /* not executed */
  FE_
}

INFO_TABLE(SE_CAF_BLACKHOLE_info, SE_CAF_BLACKHOLE_entry,0,2,SE_CAF_BLACKHOLE,,EF_,0,0);
STGFUN(SE_CAF_BLACKHOLE_entry)
{
  FB_
    STGCALL1(fflush,stdout);						
    STGCALL3(fprintf,stderr,"SE_CAF_BLACKHOLE at %p entered!\n",R1.p);
    STGCALL1(raiseError, errorHandler);
    stg_exit(EXIT_FAILURE); /* not executed */
  FE_
}
#endif

/* -----------------------------------------------------------------------------
   The code for a BCO returns to the scheduler
   -------------------------------------------------------------------------- */
INFO_TABLE(BCO_info,BCO_entry,0,0,BCO,,EF_,0,0);
EF_(BCO_entry) {				
  FB_	
    Sp -= 1;
    Sp[0] = R1.w;
    JMP_(stg_yield_to_Hugs);
  FE_								
}

/* -----------------------------------------------------------------------------
   Some static info tables for things that don't get entered, and
   therefore don't need entry code (i.e. boxed but unpointed objects)
   -------------------------------------------------------------------------- */

#define NON_ENTERABLE_ENTRY_CODE(type)					\
STGFUN(type##_entry)							\
{									\
  FB_									\
    DUMP_ERRMSG(#type " object entered!\n");                            \
    STGCALL1(raiseError, errorHandler);					\
    stg_exit(EXIT_FAILURE); /* not executed */				\
  FE_									\
}

INFO_TABLE(TSO_info, TSO_entry, 0,0,TSO,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(TSO);

/* -----------------------------------------------------------------------------
   Evacuees are left behind by the garbage collector.  Any attempt to enter
   one is a real bug.
   -------------------------------------------------------------------------- */

INFO_TABLE(EVACUATED_info,EVACUATED_entry,1,0,EVACUATED,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(EVACUATED);

/* -----------------------------------------------------------------------------
   Weak pointers

   Live weak pointers have a special closure type.  Dead ones are just
   nullary constructors (although they live on the heap - we overwrite
   live weak pointers with dead ones).
   -------------------------------------------------------------------------- */

INFO_TABLE(WEAK_info,WEAK_entry,0,4,WEAK,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(WEAK);

INFO_TABLE_CONSTR(DEAD_WEAK_info,DEAD_WEAK_entry,0,1,0,CONSTR,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(DEAD_WEAK);

/* -----------------------------------------------------------------------------
   NO_FINALIZER

   This is a static nullary constructor (like []) that we use to mark an empty
   finalizer in a weak pointer object.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(NO_FINALIZER_info,NO_FINALIZER_entry,0,0,0,CONSTR_NOCAF_STATIC,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(NO_FINALIZER);

SET_STATIC_HDR(NO_FINALIZER_closure,NO_FINALIZER_info,0/*CC*/,,EI_)
};

/* -----------------------------------------------------------------------------
   Foreign Objects are unlifted and therefore never entered.
   -------------------------------------------------------------------------- */

INFO_TABLE(FOREIGN_info,FOREIGN_entry,0,1,FOREIGN,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(FOREIGN);

/* -----------------------------------------------------------------------------
   Stable Names are unlifted too.
   -------------------------------------------------------------------------- */

INFO_TABLE(STABLE_NAME_info,STABLE_NAME_entry,0,1,STABLE_NAME,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(STABLE_NAME);

/* -----------------------------------------------------------------------------
   MVars

   There are two kinds of these: full and empty.  We need an info table
   and entry code for each type.
   -------------------------------------------------------------------------- */

INFO_TABLE(FULL_MVAR_info,FULL_MVAR_entry,4,0,MVAR,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(FULL_MVAR);

INFO_TABLE(EMPTY_MVAR_info,EMPTY_MVAR_entry,4,0,MVAR,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(EMPTY_MVAR);

/* -----------------------------------------------------------------------------
   END_TSO_QUEUE

   This is a static nullary constructor (like []) that we use to mark the
   end of a linked TSO queue.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(END_TSO_QUEUE_info,END_TSO_QUEUE_entry,0,0,0,CONSTR_NOCAF_STATIC,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(END_TSO_QUEUE);

SET_STATIC_HDR(END_TSO_QUEUE_closure,END_TSO_QUEUE_info,0/*CC*/,,EI_)
};

/* -----------------------------------------------------------------------------
   Mutable lists

   Mutable lists (used by the garbage collector) consist of a chain of
   StgMutClosures connected through their mut_link fields, ending in
   an END_MUT_LIST closure.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(END_MUT_LIST_info,END_MUT_LIST_entry,0,0,0,CONSTR_NOCAF_STATIC,,EF_,0,0);
NON_ENTERABLE_ENTRY_CODE(END_MUT_LIST);

SET_STATIC_HDR(END_MUT_LIST_closure,END_MUT_LIST_info,0/*CC*/,,EI_)
};

INFO_TABLE(MUT_CONS_info, MUT_CONS_entry, 1, 1, MUT_VAR, , EF_, 0, 0);
NON_ENTERABLE_ENTRY_CODE(MUT_CONS);

/* -----------------------------------------------------------------------------
   Arrays

   These come in two basic flavours: arrays of data (StgArrWords) and arrays of
   pointers (StgArrPtrs).  They all have a similar layout:

	___________________________
	| Info | No. of | data....
        |  Ptr | Words  |
	---------------------------

   These are *unpointed* objects: i.e. they cannot be entered.

   -------------------------------------------------------------------------- */

#define ArrayInfo(type)					\
INFO_TABLE(type##_info, type##_entry, 0, 0, type, , EF_,0,0);

ArrayInfo(ARR_WORDS);
NON_ENTERABLE_ENTRY_CODE(ARR_WORDS);
ArrayInfo(MUT_ARR_PTRS);
NON_ENTERABLE_ENTRY_CODE(MUT_ARR_PTRS);
ArrayInfo(MUT_ARR_PTRS_FROZEN);
NON_ENTERABLE_ENTRY_CODE(MUT_ARR_PTRS_FROZEN);

#undef ArrayInfo

/* -----------------------------------------------------------------------------
   Mutable Variables
   -------------------------------------------------------------------------- */

INFO_TABLE(MUT_VAR_info, MUT_VAR_entry, 1, 1, MUT_VAR, , EF_, 0, 0);
NON_ENTERABLE_ENTRY_CODE(MUT_VAR);

/* -----------------------------------------------------------------------------
   Standard Error Entry.

   This is used for filling in vector-table entries that can never happen,
   for instance.
   -------------------------------------------------------------------------- */

STGFUN(stg_error_entry)							\
{									\
  FB_									\
    DUMP_ERRMSG("fatal: stg_error_entry");                              \
    STGCALL1(raiseError, errorHandler);					\
    exit(EXIT_FAILURE); /* not executed */				\
  FE_									\
}

/* -----------------------------------------------------------------------------
   Dummy return closure
 
   Entering this closure will just return to the address on the top of the
   stack.  Useful for getting a thread in a canonical form where we can
   just enter the top stack word to start the thread.  (see deleteThread)
 * -------------------------------------------------------------------------- */

INFO_TABLE(dummy_ret_info, dummy_ret_entry, 0, 0, CONSTR_NOCAF_STATIC, , EF_, 0, 0);
FN_(dummy_ret_entry)
{
  W_ ret_addr;
  FB_
  ret_addr = Sp[0];
  Sp++;
  JMP_(ENTRY_CODE(ret_addr));
  FE_
}
SET_STATIC_HDR(dummy_ret_closure,dummy_ret_info,CCS_DONTZuCARE,,EI_)
};

/* -----------------------------------------------------------------------------
    Strict IO application - performing an IO action and entering its result.
    
    rts_evalIO() lets you perform Haskell IO actions from outside of Haskell-land,
    returning back to you their result. Want this result to be evaluated to WHNF
    by that time, so that we can easily get at the int/char/whatever using the
    various get{Ty} functions provided by the RTS API.

    forceIO takes care of this, performing the IO action and entering the
    results that comes back.

 * -------------------------------------------------------------------------- */

INFO_TABLE_SRT_BITMAP(forceIO_ret_info,forceIO_ret_entry,0,0,0,0,RET_SMALL,,EF_,0,0);
FN_(forceIO_ret_entry)
{
  FB_
  Sp++;
  Sp -= sizeofW(StgSeqFrame);
  PUSH_SEQ_FRAME(Sp);
  JMP_(GET_ENTRY(R1.cl));
}


INFO_TABLE(forceIO_info,forceIO_entry,1,0,FUN,,EF_,0,0);
FN_(forceIO_entry)
{
  FB_
  /* Sp[0] contains the IO action we want to perform */
  R1.p  = (P_)Sp[0];
  /* Replace it with the return continuation that enters the result. */
  Sp[0] = (W_)&forceIO_ret_info;
  Sp--;
  /* Push the RealWorld# tag and enter */
  Sp[0] =(W_)REALWORLD_TAG;
  JMP_(GET_ENTRY(R1.cl));
  FE_
}
SET_STATIC_HDR(forceIO_closure,forceIO_info,CCS_DONTZuCARE,,EI_)
};


/* -----------------------------------------------------------------------------
   Standard Infotables (for use in interpreter)
   -------------------------------------------------------------------------- */

#ifdef INTERPRETER

STGFUN(Hugs_CONSTR_entry)
{
    /* R1 points at the constructor */
    JMP_(ENTRY_CODE(((StgPtr*)Sp)[0]));
}

#define RET_BCO_ENTRY_TEMPLATE(label) 	\
   IFN_(label)			        \
   {                                    \
      FB_				\
      Sp -= 1;				\
      ((StgPtr*)Sp)[0] = R1.p;		\
      JMP_(stg_yield_to_Hugs);          \
      FE_                               \
   }

RET_BCO_ENTRY_TEMPLATE(ret_bco_entry  );
RET_BCO_ENTRY_TEMPLATE(ret_bco_0_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_1_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_2_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_3_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_4_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_5_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_6_entry);
RET_BCO_ENTRY_TEMPLATE(ret_bco_7_entry);

VEC_POLY_INFO_TABLE(ret_bco,0, NULL/*srt*/, 0/*srt_off*/, 0/*srt_len*/, RET_BCO,, EF_);

#endif /* INTERPRETER */

#ifndef COMPILER

INFO_TABLE_CONSTR(Czh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgChar),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(Izh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgInt),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(I64zh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgInt64),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(Fzh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgFloat),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(Dzh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgDouble),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(Azh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgAddr),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(Wzh_con_info,Hugs_CONSTR_entry,0,sizeofW(StgWord),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(StablePtr_con_info,Hugs_CONSTR_entry,0,sizeofW(StgStablePtr),0,CONSTR,,EF_,0,0);

/* These might seem redundant but {I,C}zh_static_info are used in
 * {INT,CHAR}LIKE and the rest are used in RtsAPI.c
 */
INFO_TABLE_CONSTR(Czh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgChar),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(Izh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgInt),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(I64zh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgInt64),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(Fzh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgFloat),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(Dzh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgDouble),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(Azh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgAddr),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(Wzh_static_info,Hugs_CONSTR_entry,0,sizeofW(StgWord),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(StablePtr_static_info,Hugs_CONSTR_entry,0,sizeofW(StgStablePtr),0,CONSTR_NOCAF_STATIC,,EF_,0,0);

#endif /* !defined(COMPILER) */

/* -----------------------------------------------------------------------------
   CHARLIKE and INTLIKE closures.  

   These are static representations of Chars and small Ints, so that
   we can remove dynamic Chars and Ints during garbage collection and
   replace them with references to the static objects.
   -------------------------------------------------------------------------- */

#ifdef ENABLE_WIN32_DLL_SUPPORT
/*
 * When sticking the RTS in a DLL, we delay populating the
 * Charlike and Intlike tables until load-time, which is only
 * when we've got the real addresses to the C# and I# closures.
 *
 */
static INFO_TBL_CONST StgInfoTable czh_static_info;
static INFO_TBL_CONST StgInfoTable izh_static_info;
#define Char_hash_static_info czh_static_info
#define Int_hash_static_info izh_static_info
#else
#define Char_hash_static_info Czh_static_info
#define Int_hash_static_info Izh_static_info
#endif

#define CHARLIKE_HDR(n)						\
	{							\
	  STATIC_HDR(Char_hash_static_info, /* C# */   		\
			 CCS_DONTZuCARE),			\
          data : n						\
	}
					     
#define INTLIKE_HDR(n)						\
	{							\
	  STATIC_HDR(Int_hash_static_info,  /* I# */  		\
			 CCS_DONTZuCARE),			\
          data : n						\
	}

/* put these in the *data* section, since the garbage collector relies
 * on the fact that static closures live in the data section.
 */

/* end the name with _closure, to convince the mangler this is a closure */

StgIntCharlikeClosure CHARLIKE_closure[] = {
    CHARLIKE_HDR(0),
    CHARLIKE_HDR(1),
    CHARLIKE_HDR(2),
    CHARLIKE_HDR(3),
    CHARLIKE_HDR(4),
    CHARLIKE_HDR(5),
    CHARLIKE_HDR(6),
    CHARLIKE_HDR(7),
    CHARLIKE_HDR(8),
    CHARLIKE_HDR(9),
    CHARLIKE_HDR(10),
    CHARLIKE_HDR(11),
    CHARLIKE_HDR(12),
    CHARLIKE_HDR(13),
    CHARLIKE_HDR(14),
    CHARLIKE_HDR(15),
    CHARLIKE_HDR(16),
    CHARLIKE_HDR(17),
    CHARLIKE_HDR(18),
    CHARLIKE_HDR(19),
    CHARLIKE_HDR(20),
    CHARLIKE_HDR(21),
    CHARLIKE_HDR(22),
    CHARLIKE_HDR(23),
    CHARLIKE_HDR(24),
    CHARLIKE_HDR(25),
    CHARLIKE_HDR(26),
    CHARLIKE_HDR(27),
    CHARLIKE_HDR(28),
    CHARLIKE_HDR(29),
    CHARLIKE_HDR(30),
    CHARLIKE_HDR(31),
    CHARLIKE_HDR(32),
    CHARLIKE_HDR(33),
    CHARLIKE_HDR(34),
    CHARLIKE_HDR(35),
    CHARLIKE_HDR(36),
    CHARLIKE_HDR(37),
    CHARLIKE_HDR(38),
    CHARLIKE_HDR(39),
    CHARLIKE_HDR(40),
    CHARLIKE_HDR(41),
    CHARLIKE_HDR(42),
    CHARLIKE_HDR(43),
    CHARLIKE_HDR(44),
    CHARLIKE_HDR(45),
    CHARLIKE_HDR(46),
    CHARLIKE_HDR(47),
    CHARLIKE_HDR(48),
    CHARLIKE_HDR(49),
    CHARLIKE_HDR(50),
    CHARLIKE_HDR(51),
    CHARLIKE_HDR(52),
    CHARLIKE_HDR(53),
    CHARLIKE_HDR(54),
    CHARLIKE_HDR(55),
    CHARLIKE_HDR(56),
    CHARLIKE_HDR(57),
    CHARLIKE_HDR(58),
    CHARLIKE_HDR(59),
    CHARLIKE_HDR(60),
    CHARLIKE_HDR(61),
    CHARLIKE_HDR(62),
    CHARLIKE_HDR(63),
    CHARLIKE_HDR(64),
    CHARLIKE_HDR(65),
    CHARLIKE_HDR(66),
    CHARLIKE_HDR(67),
    CHARLIKE_HDR(68),
    CHARLIKE_HDR(69),
    CHARLIKE_HDR(70),
    CHARLIKE_HDR(71),
    CHARLIKE_HDR(72),
    CHARLIKE_HDR(73),
    CHARLIKE_HDR(74),
    CHARLIKE_HDR(75),
    CHARLIKE_HDR(76),
    CHARLIKE_HDR(77),
    CHARLIKE_HDR(78),
    CHARLIKE_HDR(79),
    CHARLIKE_HDR(80),
    CHARLIKE_HDR(81),
    CHARLIKE_HDR(82),
    CHARLIKE_HDR(83),
    CHARLIKE_HDR(84),
    CHARLIKE_HDR(85),
    CHARLIKE_HDR(86),
    CHARLIKE_HDR(87),
    CHARLIKE_HDR(88),
    CHARLIKE_HDR(89),
    CHARLIKE_HDR(90),
    CHARLIKE_HDR(91),
    CHARLIKE_HDR(92),
    CHARLIKE_HDR(93),
    CHARLIKE_HDR(94),
    CHARLIKE_HDR(95),
    CHARLIKE_HDR(96),
    CHARLIKE_HDR(97),
    CHARLIKE_HDR(98),
    CHARLIKE_HDR(99),
    CHARLIKE_HDR(100),
    CHARLIKE_HDR(101),
    CHARLIKE_HDR(102),
    CHARLIKE_HDR(103),
    CHARLIKE_HDR(104),
    CHARLIKE_HDR(105),
    CHARLIKE_HDR(106),
    CHARLIKE_HDR(107),
    CHARLIKE_HDR(108),
    CHARLIKE_HDR(109),
    CHARLIKE_HDR(110),
    CHARLIKE_HDR(111),
    CHARLIKE_HDR(112),
    CHARLIKE_HDR(113),
    CHARLIKE_HDR(114),
    CHARLIKE_HDR(115),
    CHARLIKE_HDR(116),
    CHARLIKE_HDR(117),
    CHARLIKE_HDR(118),
    CHARLIKE_HDR(119),
    CHARLIKE_HDR(120),
    CHARLIKE_HDR(121),
    CHARLIKE_HDR(122),
    CHARLIKE_HDR(123),
    CHARLIKE_HDR(124),
    CHARLIKE_HDR(125),
    CHARLIKE_HDR(126),
    CHARLIKE_HDR(127),
    CHARLIKE_HDR(128),
    CHARLIKE_HDR(129),
    CHARLIKE_HDR(130),
    CHARLIKE_HDR(131),
    CHARLIKE_HDR(132),
    CHARLIKE_HDR(133),
    CHARLIKE_HDR(134),
    CHARLIKE_HDR(135),
    CHARLIKE_HDR(136),
    CHARLIKE_HDR(137),
    CHARLIKE_HDR(138),
    CHARLIKE_HDR(139),
    CHARLIKE_HDR(140),
    CHARLIKE_HDR(141),
    CHARLIKE_HDR(142),
    CHARLIKE_HDR(143),
    CHARLIKE_HDR(144),
    CHARLIKE_HDR(145),
    CHARLIKE_HDR(146),
    CHARLIKE_HDR(147),
    CHARLIKE_HDR(148),
    CHARLIKE_HDR(149),
    CHARLIKE_HDR(150),
    CHARLIKE_HDR(151),
    CHARLIKE_HDR(152),
    CHARLIKE_HDR(153),
    CHARLIKE_HDR(154),
    CHARLIKE_HDR(155),
    CHARLIKE_HDR(156),
    CHARLIKE_HDR(157),
    CHARLIKE_HDR(158),
    CHARLIKE_HDR(159),
    CHARLIKE_HDR(160),
    CHARLIKE_HDR(161),
    CHARLIKE_HDR(162),
    CHARLIKE_HDR(163),
    CHARLIKE_HDR(164),
    CHARLIKE_HDR(165),
    CHARLIKE_HDR(166),
    CHARLIKE_HDR(167),
    CHARLIKE_HDR(168),
    CHARLIKE_HDR(169),
    CHARLIKE_HDR(170),
    CHARLIKE_HDR(171),
    CHARLIKE_HDR(172),
    CHARLIKE_HDR(173),
    CHARLIKE_HDR(174),
    CHARLIKE_HDR(175),
    CHARLIKE_HDR(176),
    CHARLIKE_HDR(177),
    CHARLIKE_HDR(178),
    CHARLIKE_HDR(179),
    CHARLIKE_HDR(180),
    CHARLIKE_HDR(181),
    CHARLIKE_HDR(182),
    CHARLIKE_HDR(183),
    CHARLIKE_HDR(184),
    CHARLIKE_HDR(185),
    CHARLIKE_HDR(186),
    CHARLIKE_HDR(187),
    CHARLIKE_HDR(188),
    CHARLIKE_HDR(189),
    CHARLIKE_HDR(190),
    CHARLIKE_HDR(191),
    CHARLIKE_HDR(192),
    CHARLIKE_HDR(193),
    CHARLIKE_HDR(194),
    CHARLIKE_HDR(195),
    CHARLIKE_HDR(196),
    CHARLIKE_HDR(197),
    CHARLIKE_HDR(198),
    CHARLIKE_HDR(199),
    CHARLIKE_HDR(200),
    CHARLIKE_HDR(201),
    CHARLIKE_HDR(202),
    CHARLIKE_HDR(203),
    CHARLIKE_HDR(204),
    CHARLIKE_HDR(205),
    CHARLIKE_HDR(206),
    CHARLIKE_HDR(207),
    CHARLIKE_HDR(208),
    CHARLIKE_HDR(209),
    CHARLIKE_HDR(210),
    CHARLIKE_HDR(211),
    CHARLIKE_HDR(212),
    CHARLIKE_HDR(213),
    CHARLIKE_HDR(214),
    CHARLIKE_HDR(215),
    CHARLIKE_HDR(216),
    CHARLIKE_HDR(217),
    CHARLIKE_HDR(218),
    CHARLIKE_HDR(219),
    CHARLIKE_HDR(220),
    CHARLIKE_HDR(221),
    CHARLIKE_HDR(222),
    CHARLIKE_HDR(223),
    CHARLIKE_HDR(224),
    CHARLIKE_HDR(225),
    CHARLIKE_HDR(226),
    CHARLIKE_HDR(227),
    CHARLIKE_HDR(228),
    CHARLIKE_HDR(229),
    CHARLIKE_HDR(230),
    CHARLIKE_HDR(231),
    CHARLIKE_HDR(232),
    CHARLIKE_HDR(233),
    CHARLIKE_HDR(234),
    CHARLIKE_HDR(235),
    CHARLIKE_HDR(236),
    CHARLIKE_HDR(237),
    CHARLIKE_HDR(238),
    CHARLIKE_HDR(239),
    CHARLIKE_HDR(240),
    CHARLIKE_HDR(241),
    CHARLIKE_HDR(242),
    CHARLIKE_HDR(243),
    CHARLIKE_HDR(244),
    CHARLIKE_HDR(245),
    CHARLIKE_HDR(246),
    CHARLIKE_HDR(247),
    CHARLIKE_HDR(248),
    CHARLIKE_HDR(249),
    CHARLIKE_HDR(250),
    CHARLIKE_HDR(251),
    CHARLIKE_HDR(252),
    CHARLIKE_HDR(253),
    CHARLIKE_HDR(254),
    CHARLIKE_HDR(255)
};

StgIntCharlikeClosure INTLIKE_closure[] = {
    INTLIKE_HDR(-16),	/* MIN_INTLIKE == -16 */
    INTLIKE_HDR(-15),
    INTLIKE_HDR(-14),
    INTLIKE_HDR(-13),
    INTLIKE_HDR(-12),
    INTLIKE_HDR(-11),
    INTLIKE_HDR(-10),
    INTLIKE_HDR(-9),
    INTLIKE_HDR(-8),
    INTLIKE_HDR(-7),
    INTLIKE_HDR(-6),
    INTLIKE_HDR(-5),
    INTLIKE_HDR(-4),
    INTLIKE_HDR(-3),
    INTLIKE_HDR(-2),
    INTLIKE_HDR(-1),
    INTLIKE_HDR(0),
    INTLIKE_HDR(1),
    INTLIKE_HDR(2),
    INTLIKE_HDR(3),
    INTLIKE_HDR(4),
    INTLIKE_HDR(5),
    INTLIKE_HDR(6),
    INTLIKE_HDR(7),
    INTLIKE_HDR(8),
    INTLIKE_HDR(9),
    INTLIKE_HDR(10),
    INTLIKE_HDR(11),
    INTLIKE_HDR(12),
    INTLIKE_HDR(13),
    INTLIKE_HDR(14),
    INTLIKE_HDR(15),
    INTLIKE_HDR(16)	/* MAX_INTLIKE == 16 */
};
