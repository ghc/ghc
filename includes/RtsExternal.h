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
extern void stopTimer(void);
extern unsigned int n_capabilities;

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
extern int hs_hpc_module(char *modName,int modCount,int modHashNo,StgWord64 *tixArr);
// Simple linked list of modules
typedef struct _HpcModuleInfo {
  char *modName;		// name of module
  int tickCount;		// number of ticks
  int tickOffset;		// offset into a single large .tix Array
  int hashNo;			// Hash number for this module's mix info
  StgWord64 *tixArr;		// tix Array; local for this module
  struct _HpcModuleInfo *next;
} HpcModuleInfo;

extern HpcModuleInfo *hs_hpc_rootModule(void);


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
void *getIOManagerEvent (void);
HsWord32 readIOManagerEvent (void);
void sendIOManagerEvent (HsWord32 event);
#else
extern void setIOManagerPipe (int fd);
#endif

extern void* allocateExec(unsigned int len);

// Breakpoint stuff
extern int rts_stop_next_breakpoint;
extern int rts_stop_on_exception;
extern HsStablePtr rts_breakpoint_io_action;

/* -----------------------------------------------------------------------------
   Storage manager stuff exported
   -------------------------------------------------------------------------- */

extern void performGC(void);
extern void performMajorGC(void);
extern HsInt64 getAllocations( void );
extern void revertCAFs( void );
extern void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);

#endif /*  RTSEXTERNAL_H */
