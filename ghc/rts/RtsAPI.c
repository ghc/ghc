/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2001
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "OSThreads.h"
#include "Storage.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include "Schedule.h"
#include "Capability.h"

#include <stdlib.h>

static Capability *rtsApiCapability = NULL;

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj
rts_mkChar (HsChar c)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  SET_HDR(p, Czh_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)(StgWord)(StgChar)c;
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
  llong *tmp;
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  SET_HDR(p, I64zh_con_info, CCS_SYSTEM);
  tmp  = (llong*)&(p->payload[0]);
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
  ullong *tmp;

  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  /* see mk_Int8 comment */
  SET_HDR(p, W64zh_con_info, CCS_SYSTEM);
  tmp  = (ullong*)&(p->payload[0]);
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

HaskellObj
rts_mkFunPtr (HsFunPtr a)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  SET_HDR(p, FunPtr_con_info, CCS_SYSTEM);
  p->payload[0]  = (StgClosure *)a;
  return p;
}

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

HaskellObj
rts_apply (HaskellObj f, HaskellObj arg)
{
    StgThunk *ap;

    ap = (StgThunk *)allocate(sizeofW(StgThunk) + 2);
    SET_HDR(ap, (StgInfoTable *)&stg_ap_2_upd_info, CCS_SYSTEM);
    ap->payload[0] = f;
    ap->payload[1] = arg;
    return (StgClosure *)ap;
}

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects

   We would like to assert that we have the right kind of object in
   each case, but this is problematic because in GHCi the info table
   for the D# constructor (say) might be dynamically loaded.  Hence we
   omit these assertions for now.
   ------------------------------------------------------------------------- */

HsChar
rts_getChar (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Czh_con_info ||
    //        p->header.info == Czh_static_info);
    return (StgChar)(StgWord)(p->payload[0]);
}

HsInt
rts_getInt (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Izh_con_info ||
    //        p->header.info == Izh_static_info);
    return (HsInt)(p->payload[0]);
}

HsInt8
rts_getInt8 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I8zh_con_info ||
    //        p->header.info == I8zh_static_info);
    return (HsInt8)(HsInt)(p->payload[0]);
}

HsInt16
rts_getInt16 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I16zh_con_info ||
    //        p->header.info == I16zh_static_info);
    return (HsInt16)(HsInt)(p->payload[0]);
}

HsInt32
rts_getInt32 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == I32zh_con_info ||
    //        p->header.info == I32zh_static_info);
    return (HsInt32)(HsInt)(p->payload[0]);
}

HsInt64
rts_getInt64 (HaskellObj p)
{
    HsInt64* tmp;
    // See comment above:
    // ASSERT(p->header.info == I64zh_con_info ||
    //        p->header.info == I64zh_static_info);
    tmp = (HsInt64*)&(p->payload[0]);
    return *tmp;
}
HsWord
rts_getWord (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Wzh_con_info ||
    //        p->header.info == Wzh_static_info);
    return (HsWord)(p->payload[0]);
}

HsWord8
rts_getWord8 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W8zh_con_info ||
    //        p->header.info == W8zh_static_info);
    return (HsWord8)(HsWord)(p->payload[0]);
}

HsWord16
rts_getWord16 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W16zh_con_info ||
    //        p->header.info == W16zh_static_info);
    return (HsWord16)(HsWord)(p->payload[0]);
}

HsWord32
rts_getWord32 (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == W32zh_con_info ||
    //        p->header.info == W32zh_static_info);
    return (HsWord32)(HsWord)(p->payload[0]);
}


HsWord64
rts_getWord64 (HaskellObj p)
{
    HsWord64* tmp;
    // See comment above:
    // ASSERT(p->header.info == W64zh_con_info ||
    //        p->header.info == W64zh_static_info);
    tmp = (HsWord64*)&(p->payload[0]);
    return *tmp;
}

HsFloat
rts_getFloat (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Fzh_con_info ||
    //        p->header.info == Fzh_static_info);
    return (float)(PK_FLT((P_)p->payload));
}

HsDouble
rts_getDouble (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Dzh_con_info ||
    //        p->header.info == Dzh_static_info);
    return (double)(PK_DBL((P_)p->payload));
}

HsStablePtr
rts_getStablePtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == StablePtr_con_info ||
    //        p->header.info == StablePtr_static_info);
    return (StgStablePtr)(p->payload[0]);
}

HsPtr
rts_getPtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == Ptr_con_info ||
    //        p->header.info == Ptr_static_info);
    return (void *)(p->payload[0]);
}

HsFunPtr
rts_getFunPtr (HaskellObj p)
{
    // See comment above:
    // ASSERT(p->header.info == FunPtr_con_info ||
    //        p->header.info == FunPtr_static_info);
    return (void *)(p->payload[0]);
}

HsBool
rts_getBool (HaskellObj p)
{
    StgInfoTable *info;

    info = get_itbl((StgClosure *)p);
    if (info->srt_bitmap == 0) { // srt_bitmap is the constructor tag
	return 0;
    } else {
	return 1;
    }
}

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions
   ------------------------------------------------------------------------- */
SchedulerStatus
rts_eval (HaskellObj p, /*out*/HaskellObj *ret)
{
    StgTSO *tso;
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;

    tso = createGenThread(RtsFlags.GcFlags.initialStkSize, p);
    return scheduleWaitThread(tso,ret,cap);
}

SchedulerStatus
rts_eval_ (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
    StgTSO *tso;
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;
    
    tso = createGenThread(stack_size, p);
    return scheduleWaitThread(tso,ret,cap);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
SchedulerStatus
rts_evalIO (HaskellObj p, /*out*/HaskellObj *ret)
{
    StgTSO* tso; 
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;
    
    tso = createStrictIOThread(RtsFlags.GcFlags.initialStkSize, p);
    return scheduleWaitThread(tso,ret,cap);
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
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;
    
    p = (StgClosure *)deRefStablePtr(s);
    tso = createStrictIOThread(RtsFlags.GcFlags.initialStkSize, p);
    stat = scheduleWaitThread(tso,&r,cap);

    if (stat == Success && ret != NULL) {
	ASSERT(r != NULL);
	*ret = getStablePtr((StgPtr)r);
    }

    return stat;
}

/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
SchedulerStatus
rts_evalLazyIO (HaskellObj p, /*out*/HaskellObj *ret)
{
    StgTSO *tso;
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;

    tso = createIOThread(RtsFlags.GcFlags.initialStkSize, p);
    return scheduleWaitThread(tso,ret,cap);
}

SchedulerStatus
rts_evalLazyIO_ (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
    StgTSO *tso;
    Capability *cap = rtsApiCapability;
    rtsApiCapability = NULL;

    tso = createIOThread(stack_size, p);
    return scheduleWaitThread(tso,ret,cap);
}

/* Convenience function for decoding the returned status. */

void
rts_checkSchedStatus ( char* site, SchedulerStatus rc )
{
    switch (rc) {
    case Success:
	return;
    case Killed:
	errorBelch("%s: uncaught exception",site);
	stg_exit(EXIT_FAILURE);
    case Interrupted:
	errorBelch("%s: interrupted", site);
	stg_exit(EXIT_FAILURE);
    default:
	errorBelch("%s: Return code (%d) not ok",(site),(rc));	
	stg_exit(EXIT_FAILURE);
    }
}

void
rts_lock()
{
#ifdef RTS_SUPPORTS_THREADS
    ACQUIRE_LOCK(&sched_mutex);
	
    // we request to get the capability immediately, in order to
    // a) stop other threads from using allocate()
    // b) wake the current worker thread from awaitEvent()
    //       (so that a thread started by rts_eval* will start immediately)
    waitForReturnCapability(&sched_mutex,&rtsApiCapability);
#else
    grabCapability(&rtsApiCapability);
#endif
}

void
rts_unlock()
{
#ifdef RTS_SUPPORTS_THREADS
    if (rtsApiCapability) {
	releaseCapability(rtsApiCapability);
    }
    rtsApiCapability = NULL;
    RELEASE_LOCK(&sched_mutex);
#endif
}
