/* -----------------------------------------------------------------------------
 * $Id: RtsExternal.h,v 1.3 2004/08/21 12:47:17 panne Exp $
 *
 * (c) The GHC Team, 1998-2004
 *
 * Things visible externally to the RTS
 *
 * -------------------------------------------------------------------------- */

#ifndef RTSEXTERNAL_H
#define RTSEXTERNAL_H

/* The RTS public interface. */
#include "RtsAPI.h"

/* The standard FFI interface */
#include "HsFFI.h"

/* -----------------------------------------------------------------------------
   Functions exported by the RTS for use in Stg code
   -------------------------------------------------------------------------- */

#if IN_STG_CODE
extern void newCAF(void*);
#else
extern void newCAF(StgClosure*);
#endif

/* ToDo: remove? */
extern I_ genSymZh(void);
extern I_ resetGenSymZh(void);

/* Concurrency/Exception PrimOps. */
extern int cmp_thread(StgPtr tso1, StgPtr tso2);
extern int rts_getThreadId(StgPtr tso);
extern int forkOS_createThread ( HsStablePtr entry );

/* grimy low-level support functions defined in StgPrimFloat.c */
extern StgDouble __encodeDouble (I_ size, StgByteArray arr, I_ e);
extern StgDouble __int_encodeDouble (I_ j, I_ e);
extern StgFloat  __encodeFloat (I_ size, StgByteArray arr, I_ e);
extern StgFloat  __int_encodeFloat (I_ j, I_ e);
extern StgInt    isDoubleNaN(StgDouble d);
extern StgInt    isDoubleInfinite(StgDouble d);
extern StgInt    isDoubleDenormalized(StgDouble d);
extern StgInt    isDoubleNegativeZero(StgDouble d);
extern StgInt    isFloatNaN(StgFloat f);
extern StgInt    isFloatInfinite(StgFloat f);
extern StgInt    isFloatDenormalized(StgFloat f);
extern StgInt    isFloatNegativeZero(StgFloat f);

/* Suspending/resuming threads around foreign calls */
extern StgInt        suspendThread ( StgRegTable * );
extern StgRegTable * resumeThread  ( StgInt );

/* Creating and destroying an adjustor thunk */
extern void*  createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr);
extern void   freeHaskellFunctionPtr(void* ptr);

/* -----------------------------------------------------------------------------
   Storage manager stuff exported
   -------------------------------------------------------------------------- */

/* Prototype for an evacuate-like function */
typedef void (*evac_fn)(StgClosure **);

extern void performGC(void);
extern void performMajorGC(void);
extern void performGCWithRoots(void (*get_roots)(evac_fn));

#endif /*  RTSEXTERNAL_H */
