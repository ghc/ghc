/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2002
 *
 * External API for the scheduler.  For most uses, the functions in
 * RtsAPI.h should be enough.
 *
 * ---------------------------------------------------------------------------*/

#ifndef SCHEDAPI_H
#define SCHEDAPI_H

#if defined(GRAN)
/* Dummy def for NO_PRI if not in GranSim */
#define NO_PRI  0
#endif

/* 
 * Creating threads
 */
#if defined(GRAN)
StgTSO *createThread (Capability *cap, nat stack_size, StgInt pri);
#else
StgTSO *createThread (Capability *cap, nat stack_size);
#endif

Capability *scheduleWaitThread (StgTSO *tso, /*out*/HaskellObj* ret,
				Capability *cap);

StgTSO *createGenThread       (Capability *cap, nat stack_size,  
			       StgClosure *closure);
StgTSO *createIOThread        (Capability *cap, nat stack_size,  
			       StgClosure *closure);
StgTSO *createStrictIOThread  (Capability *cap, nat stack_size,  
			       StgClosure *closure);
#endif
