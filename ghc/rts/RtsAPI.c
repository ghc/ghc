/* ----------------------------------------------------------------------------
 * $Id: RtsAPI.c,v 1.7 1999/05/21 14:46:19 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj
rts_mkChar (char c)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Czh_con_info;
  p->payload[0]  = (StgClosure *)((StgInt)c);
  return p;
}

HaskellObj
rts_mkInt (int i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Izh_con_info;
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt8 (int i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  /* This is a 'cheat', using the static info table for Ints,
     instead of the one for Int8, but the types have identical
     representation.
  */
  p->header.info = (const StgInfoTable*)&Izh_con_info;
  /* Make sure we mask out the bits above the lowest 8 */
  p->payload[0]  = (StgClosure *)(StgInt)((unsigned)i & 0xff);
  return p;
}

HaskellObj
rts_mkInt16 (int i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  /* This is a 'cheat', using the static info table for Ints,
     instead of the one for Int8, but the types have identical
     representation.
  */
  p->header.info = (const StgInfoTable*)&Izh_con_info;
  /* Make sure we mask out the relevant bits */
  p->payload[0]  = (StgClosure *)(StgInt)((unsigned)i & 0xffff);
  return p;
}

HaskellObj
rts_mkInt32 (int i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  /* see mk_Int8 comment */
  p->header.info = (const StgInfoTable*)&Izh_con_info;
  p->payload[0]  = (StgClosure *)(StgInt)i;
  return p;
}

HaskellObj
rts_mkInt64 (long long int i)
{
  long long *tmp;
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  /* see mk_Int8 comment */
  p->header.info = (const StgInfoTable*)&I64zh_con_info;
  tmp  = (long long*)&(p->payload[0]);
  *tmp = (StgInt64)i;
  return p;
}

HaskellObj
rts_mkWord (unsigned int i)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Wzh_con_info;
  p->payload[0]  = (StgClosure *)(StgWord)i;
  return p;
}

HaskellObj
rts_mkWord8 (unsigned int w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Wzh_con_info;
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xff);
  return p;
}

HaskellObj
rts_mkWord16 (unsigned int w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Wzh_con_info;
  p->payload[0]  = (StgClosure *)(StgWord)(w & 0xffff);
  return p;
}

HaskellObj
rts_mkWord32 (unsigned int w)
{
  /* see rts_mkInt* comments */
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Wzh_con_info;
  p->payload[0]  = (StgClosure *)(StgWord)w;
  return p;
}

HaskellObj
rts_mkWord64 (unsigned long long w)
{
  unsigned long long *tmp;

  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,2));
  /* see mk_Int8 comment */
  p->header.info = (const StgInfoTable*)&W64zh_con_info;
  tmp  = (unsigned long long*)&(p->payload[0]);
  *tmp = (StgWord64)w;
  return p;
}

HaskellObj
rts_mkFloat (float f)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,1));
  p->header.info = (const StgInfoTable*)&Fzh_con_info;
  ASSIGN_FLT((P_)p->payload, (StgFloat)f);
  return p;
}

HaskellObj
rts_mkDouble (double d)
{
  StgClosure *p = (StgClosure *)allocate(CONSTR_sizeW(0,sizeofW(StgDouble)));
  p->header.info = (const StgInfoTable*)&Dzh_con_info;
  ASSIGN_DBL((P_)p->payload, (StgDouble)d);
  return p;
}

HaskellObj
rts_mkStablePtr (StgStablePtr s)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  p->header.info = (const StgInfoTable*)&StablePtr_con_info;
  p->payload[0]  = (StgClosure *)s;
  return p;
}

HaskellObj
rts_mkAddr (void *a)
{
  StgClosure *p = (StgClosure *)allocate(sizeofW(StgHeader)+1);
  p->header.info = (const StgInfoTable*)&Azh_con_info;
  p->payload[0]  = (StgClosure *)a;
  return p;
}

#ifdef COMPILER /* GHC has em, Hugs doesn't */
HaskellObj
rts_mkBool (int b)
{
  if (b) {
    return (StgClosure *)&True_closure;
  } else {
    return (StgClosure *)&False_closure;
  }
}

HaskellObj
rts_mkString (char *s)
{
  return rts_apply((StgClosure *)&unpackCString_closure, rts_mkAddr(s));
}

HaskellObj
rts_apply (HaskellObj f, HaskellObj arg)
{
  StgAP_UPD *ap = (StgAP_UPD *)allocate(AP_sizeW(1));
  ap->header.info = &AP_UPD_info;
  ap->n_args = 1;
  ap->fun    = f;
  ap->payload[0] = (P_)arg;
  return (StgClosure *)ap;
}
#endif /* COMPILER */

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects
   ------------------------------------------------------------------------- */

char
rts_getChar (HaskellObj p)
{
  if ( p->header.info == (const StgInfoTable*)&Czh_con_info || 
       p->header.info == (const StgInfoTable*)&Czh_static_info) {
    return (char)(StgWord)(p->payload[0]);
  } else {
    barf("getChar: not a Char");
  }
}

int
rts_getInt (HaskellObj p)
{
  if ( 1 ||
       p->header.info == (const StgInfoTable*)&Izh_con_info || 
       p->header.info == (const StgInfoTable*)&Izh_static_info ) {
    return (int)(p->payload[0]);
  } else {
    barf("getInt: not an Int");
  }
}

int
rts_getInt32 (HaskellObj p)
{
  if ( 1 ||
       p->header.info == (const StgInfoTable*)&Izh_con_info || 
       p->header.info == (const StgInfoTable*)&Izh_static_info ) {
    return (int)(p->payload[0]);
  } else {
    barf("getInt: not an Int");
  }
}

unsigned int
rts_getWord (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == (const StgInfoTable*)&Wzh_con_info ||
       p->header.info == (const StgInfoTable*)&Wzh_static_info ) {
    return (unsigned int)(p->payload[0]);
  } else {
    barf("getWord: not a Word");
  }
}

unsigned int
rts_getWord32 (HaskellObj p)
{
  if ( 1 || /* see above comment */
       p->header.info == (const StgInfoTable*)&Wzh_con_info ||
       p->header.info == (const StgInfoTable*)&Wzh_static_info ) {
    return (unsigned int)(p->payload[0]);
  } else {
    barf("getWord: not a Word");
  }
}

float
rts_getFloat (HaskellObj p)
{
  if ( p->header.info == (const StgInfoTable*)&Fzh_con_info || 
       p->header.info == (const StgInfoTable*)&Fzh_static_info ) {
    return (float)(PK_FLT((P_)p->payload));
  } else {
    barf("getFloat: not a Float");
  }
}

double
rts_getDouble (HaskellObj p)
{
  if ( p->header.info == (const StgInfoTable*)&Dzh_con_info || 
       p->header.info == (const StgInfoTable*)&Dzh_static_info ) {
    return (double)(PK_DBL((P_)p->payload));
  } else {
    barf("getDouble: not a Double");
  }
}

StgStablePtr
rts_getStablePtr (HaskellObj p)
{
  if ( p->header.info == (const StgInfoTable*)&StablePtr_con_info || 
       p->header.info == (const StgInfoTable*)&StablePtr_static_info ) {
    return (StgStablePtr)(p->payload[0]);
  } else {
    barf("getStablePtr: not a StablePtr");
  }
}

void *
rts_getAddr (HaskellObj p)
{
  if ( p->header.info == (const StgInfoTable*)&Azh_con_info || 
       p->header.info == (const StgInfoTable*)&Azh_static_info ) {
  
    return (void *)(p->payload[0]);
  } else {
    barf("getAddr: not an Addr");
  }
}

#ifdef COMPILER /* GHC has em, Hugs doesn't */
int
rts_getBool (HaskellObj p)
{
  if (p == &True_closure) {
    return 1;
  } else if (p == &False_closure) {
    return 0;
  } else {
    barf("getBool: not a Bool");
  }
}
#endif /* COMPILER */

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions
   ------------------------------------------------------------------------- */
SchedulerStatus
rts_eval (HaskellObj p, /*out*/HaskellObj *ret)
{
  StgTSO *tso = createGenThread(RtsFlags.GcFlags.initialStkSize, p);
  return schedule(tso, ret);
}

SchedulerStatus
rts_eval_ (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
  StgTSO *tso = createGenThread(stack_size, p);
  return schedule(tso, ret);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
SchedulerStatus
rts_evalIO (HaskellObj p, /*out*/HaskellObj *ret)
{
  StgTSO* tso = createStrictIOThread(RtsFlags.GcFlags.initialStkSize, p);
  return schedule(tso, ret);
}

/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
SchedulerStatus
rts_evalLazyIO (HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret)
{
  StgTSO *tso = createIOThread(stack_size, p);
  return schedule(tso, ret);
}

/* Convenience function for decoding the returned status. */

void rts_checkSchedStatus ( char* site, SchedulerStatus rc )
{
  if ( rc == Success ) {
     return;
  } else {
     barf("%s: Return code (%d) not ok",(site),(rc));
  }
}
