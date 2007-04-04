/* -----------------------------------------------------------------------------
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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* -----------------------------------------------------------------------------
   Functions exported by the RTS for use in Stg code
   -------------------------------------------------------------------------- */

#if IN_STG_CODE
extern void newCAF(void*);
#else
extern void newCAF(StgClosure*);
#endif

/* ToDo: remove? */
extern HsInt genSymZh(void);
extern HsInt resetGenSymZh(void);

/* Alternate to raise(3) for threaded rts, for OpenBSD */
extern int genericRaise(int sig);

/* Concurrency/Exception PrimOps. */
extern int cmp_thread(StgPtr tso1, StgPtr tso2);
extern int rts_getThreadId(StgPtr tso);
extern int forkOS_createThread ( HsStablePtr entry );
extern pid_t forkProcess(HsStablePtr *entry);
extern HsBool rtsSupportsBoundThreads(void);
extern StgInt newSpark (StgRegTable *reg, StgClosure *p);

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
extern void *        suspendThread ( StgRegTable * );
extern StgRegTable * resumeThread  ( void * );

/* scheduler stuff */
extern void stg_scheduleThread (StgRegTable *reg, struct StgTSO_ *tso);

/* Creating and destroying an adjustor thunk */
extern void*  createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr,
                             char *typeString);
extern void   freeHaskellFunctionPtr(void* ptr);

/* Hpc stuff */
extern int hs_hpc_module(char *modName,int modCount,StgWord64 *tixArr);
extern void hs_hpc_tick(int globIx,struct StgTSO_ *current_tso);
extern void hs_hpc_raise_event(struct StgTSO_ *current_tso);	
extern void hs_hpc_thread_finished_event(struct StgTSO_ *current_tso);

#if defined(mingw32_HOST_OS)
extern int  rts_InstallConsoleEvent ( int action, StgStablePtr *handler );
extern void rts_ConsoleHandlerDone  ( int ev );
#else
extern int stg_sig_install (int, int, StgStablePtr *, void *);
#endif

#if defined(mingw32_HOST_OS)
extern StgInt console_handler;
#else
extern StgInt *signal_handlers;
#endif

#if defined(mingw32_HOST_OS)
HANDLE getIOManagerEvent (void);
HsWord32 readIOManagerEvent (void);
void sendIOManagerEvent (HsWord32 event);
#else
extern void setIOManagerPipe (int fd);
#endif

extern void* allocateExec(unsigned int len);

/* -----------------------------------------------------------------------------
   Storage manager stuff exported
   -------------------------------------------------------------------------- */

extern void performGC(void);
extern void performMajorGC(void);
extern HsInt64 getAllocations( void );
extern void revertCAFs( void );
extern void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);

#endif /*  RTSEXTERNAL_H */
