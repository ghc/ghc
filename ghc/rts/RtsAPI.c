/* ----------------------------------------------------------------------------
 * $Id: RtsAPI.c,v 1.30 2001/10/23 11:30:07 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Storage.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj
rts_mkChar (HsChar c)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, Czh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgChar)c;
  return p;
}

HaskellObj
rts_mkInt (HsInt i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, Izh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt8 (HsInt8 i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, I8zh_con_info, CCS_SYSTEM);
  /* Make sure we mask out the bits above the lowest 8 */
  p->payload[0]  = (StgClosure *)(StgInt)((unsigned)i & 0xff);
  return p;
}

HaskellObj
rts_mkInt16 (HsInt16 i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, I16zh_con_info, CCS_SYSTEM);
  /* Make sure we mask out the relevant bits */
  p->payload[0]  = (StgClosure *)(StgInt)((unsigned)i & 0xffff);
  return p;
}

HaskellObj
rts_mkInt32 (HsInt32 i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, I32zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgInt)((unsigned)i & 0xffffffff);
  return p;
}

HaskellObj
rts_mkInt64 (HsInt64 i)
{
  long long *tmp;
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  SET_HDR(p, I64zh_con_info, CCS_SYSTEM);
  tmp  = (long long*)&(p->payload[0]);
  *tmp = (StgInt64)i;
  return p;
}

HaskellObj
rts_mkWord (HsWord i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, Wzh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)i;
  return p;
}

HaskellObj
rts_mkWord8 (HsWord8 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, W8zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xff);
  return p;
}

HaskellObj
rts_mkWord16 (HsWord16 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, W16zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xffff);
  return p;
}

HaskellObj
rts_mkWord32 (HsWord32 w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, W32zh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xffffffff);
  return p;
}

HaskellObj
rts_mkWord64 (HsWord64 w)
{
  unsigned long long *tmp;

  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  /* see mk_Int8 comment */
  SET_HDR(p, W64zh_con_info, CCS_SYSTEM);
  tmp  = (unsigned long long*)&(p->payload[0]);
  *tmp = (StgWord64)w;
  return p;
}

HaskellObj
rts_mkFloat (HsFloat f)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, Fzh_con_info, CCS_SYSTEM);
  ASSIGN_FLT((P_)p->payload, (StgFloat)f);
  return p;
}

HaskellObj
rts_mkDouble (HsDouble d)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,sizeofW(StgDouble)));
  SET_HDR(p, Dzh_con_info, CCS_SYSTEM);
  ASSIGN_DBL((P_)p->payload, (StgDouble)d);
  return p;
}

HaskellObj
rts_mkStablePtr (HsStablePtr s)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  SET_HDR(p, StablePtr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)s;
  return p;
}

HaskellObj
rts_mkPtr (HsPtr a)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  SET_HDR(p, Ptr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)a;
  return p;
}

#ifdef COMPILER /* GHC has em, Hugs doesn't */
HaskellObj
rts_mkBool (HsBool b)
{
  if (b) {
    return (StgClosure *)True_closure;
  } else {
    return (StgClosure *)False_closure;
  }
}

HaskellObj
rts_mkString (char *s)
{
  return rts_apply((StgClosure *)unpackCString_closure, rts_mkPtr(s));
}
#endif /* COMPILER */

HaskellObj
rts_apply (HaskellObj f, HaskellObj arg)
{
  StgAP_UPD *ap = (StgAP_UPD *)allocate(AP_sizeW(1));
  SET_HDR(ap, &stg_AP_UPD_info, CCS_SYSTEM);
  ap->n_args = 1;
  ap->fun    = f;
  ap->payload[0] = arg;
  return (StgClosure *)ap;
}

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects
   ------------------------------------------------------------------------- */

HsChar
rts_getChar (HaskellObj p)
{
  if ( p->header.info == Czh_con_info || 
       p->header.info == Czh_static_info) {
    return (StgChar)(StgWord)(p->payload[0]);
  } else {
    barf("rts_getChar: not a Char");
  }
}

HsInt
rts_getInt (HaskellObj p)
{
  if ( 1 ||
       p->header.info == Izh_con_info || 
       p->header.info == Izh_static_info ) {
    return (HsInt)(p->payload[0]);
  } else {
    barf("rts_getInt: not an Int");
  }
}

HsInt8
rts_getInt8 (HaskellObj p)
{
  if ( 1 ||
       p->header.info == I8zh_con_info || 
       p->header.info == I8zh_static_info ) {
    return (HsInt8)(HsInt)(p->payload[0]);
  } else {
    barf("rts_getInt8: not an Int8");
  }
}

HsInt16
rts_getInt16 (HaskellObj p)
{
  if ( 1 ||
       p->header.info == I16zh_con_info || 
       p->header.info == I16zh_static_info ) {
    return (HsInt16)(HsInt)(p->payload[0]);
  } else {
    barf("rts_getInt16: not an Int16");
  }
}

HsInt32
rts_getInt32 (HaskellObj p)
{
  if ( 1 ||
       p->header.info == I32zh_con_info || 
       p->header.info == I32zh_static_info ) {
    return (HsInt32)(p->payload[0]);
  } else {
    barf("rts_getInt32: not an Int32");
  }
}

HsInt64
rts_getInt64 (HaskellObj p)
{
  HsInt64* tmp;
  if ( 1 ||
       p->header.info == I64zh_con_info || 
       p->header.info == I64zh_static_info ) {
    tmp = (HsInt64*)&(p->payload[0]);
    return *tmp;
  } else {
    barf("rts_getInt64: not an Int64");
  }
}
HsWord
rts_getWord (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == Wzh_con_info ||
       p->header.info == Wzh_static_info ) {
    return (HsWord)(p->payload[0]);
  } else {
    barf("rts_getWord: not a Word");
  }
}

HsWord8
rts_getWord8 (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == W8zh_con_info ||
       p->header.info == W8zh_static_info ) {
    return (HsWord8)(HsWord)(p->payload[0]);
  } else {
    barf("rts_getWord8: not a Word8");
  }
}

HsWord16
rts_getWord16 (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == W16zh_con_info ||
       p->header.info == W16zh_static_info ) {
    return (HsWord16)(HsWord)(p->payload[0]);
  } else {
    barf("rts_getWord16: not a Word16");
  }
}

HsWord32
rts_getWord32 (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == W32zh_con_info ||
       p->header.info == W32zh_static_info ) {
    return (unsigned int)(p->payload[0]);
  } else {
    barf("rts_getWord: not a Word");
  }
}


HsWord64
rts_getWord64 (HaskellObj p)
{
  HsWord64* tmp;
  if ( 1 || /* see above comment */
       p->header.info == W64zh_con_info ||
       p->header.info == W64zh_static_info ) {
    tmp = (HsWord64*)&(p->payload[0]);
    return *tmp;
  } else {
    barf("rts_getWord64: not a Word64");
  }
}

HsFloat
rts_getFloat (HaskellObj p)
{
  if ( p->header.info == Fzh_con_info || 
       p->header.info == Fzh_static_info ) {
    return (float)(PK_FLT((P_)p->payload));
  } else {
    barf("rts_getFloat: not a Float");
  }
}

HsDouble
rts_getDouble (HaskellObj p)
{
  if ( p->header.info == Dzh_con_info || 
       p->header.info == Dzh_static_info ) {
    return (double)(PK_DBL((P_)p->payload));
  } else {
    barf("rts_getDouble: not a Double");
  }
}

HsStablePtr
rts_getStablePtr (HaskellObj p)
{
  if ( p->header.info == StablePtr_con_info || 
       p->header.info == StablePtr_static_info ) {
    return (StgStablePtr)(p->payload[0]);
  } else {
    barf("rts_getStablePtr: not a StablePtr");
  }
}

HsPtr
rts_getPtr (HaskellObj p)
{
  if ( p->header.info == Ptr_con_info || 
       p->header.info == Ptr_static_info ) {
    return (void *)(p->payload[0]);
  } else {
    barf("rts_getPtr: not an Ptr");
  }
}

#ifdef COMPILER /* GHC has em, Hugs doesn't */
HsBool
rts_getBool (HaskellObj p)
{
  if (p == True_closure) {
    return 1;
  } else if (p == False_closure) {
    return 0;
  } else {
    barf("rts_getBool: not a Bool");
  }
}
#endif /* COMPILER */

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions
   ------------------------------------------------------------------------- */
SchedulerStatus
rts_eval (HaskellObj p, /*out*/HaskellObj *ret)
{
    StgTSO *tso;

    tso = createGenThread(RtsFlags.GcFlags.initialStkSize, p);
    scheduleThread(tso);
    return waitThread(tso, ret);
}

SchedulerStatus
rts_eval_ (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
    StgTSO *tso;
    
    tso = createGenThread(stack_size, p);
    scheduleThread(tso);
    return waitThread(tso, ret);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
SchedulerStatus
rts_evalIO (HaskellObj p, /*out*/HaskellObj *ret)
{
    StgTSO* tso; 
    
    tso = createStrictIOThread(RtsFlags.GcFlags.initialStkSize, p);
    scheduleThread(tso);
    return waitThread(tso, ret);
}

/*
 * rts_evalStableIO() is suitable for calling from Haskell.  It
 * evaluates a value of the form (StablePtr (IO a)), forcing the
 * action's result to WHNF before returning.  The result is returned
 * in a StablePtr.
 */
SchedulerStatus
rts_evalStableIO (HsStablePtr s, /*out*/HsStablePtr *ret)
{
    StgTSO* tso;
    StgClosure *p, *r;
    SchedulerStatus stat;
    
    p = (StgClosure *)deRefStablePtr(s);
    tso = createStrictIOThread(RtsFlags.GcFlags.initialStkSize, p);
    scheduleThread(tso);
    stat = waitThread(tso, &r);

    if (stat == Success) {
	ASSERT(r != NULL);
	*ret = getStablePtr((StgPtr)r);
    }

    return stat;
}

/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
SchedulerStatus
rts_evalLazyIO (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
    StgTSO *tso;

    tso = createIOThread(stack_size, p);
    scheduleThread(tso);
    return waitThread(tso, ret);
}

/* Convenience function for decoding the returned status. */

void
rts_checkSchedStatus ( char* site, SchedulerStatus rc )
{
    switch (rc) {
    case Success:
	return;
    case Killed:
	barf("%s: uncaught exception",site);
    case Interrupted:
	barf("%s: interrupted", site);
    case Deadlock:
	barf("%s: no threads to run:  infinite loop or deadlock?", site);
    default:
	barf("%s: Return code (%d) not ok",(site),(rc));	
    }
}
