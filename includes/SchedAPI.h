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

/* 
 * Creating threads
 */
StgTSO *createThread (Capability *cap, nat stack_size);

Capability *scheduleWaitThread (StgTSO *tso, /*out*/HaskellObj* ret,
				Capability *cap);

StgTSO *createGenThread       (Capability *cap, nat stack_size,  
			       StgClosure *closure);
StgTSO *createIOThread        (Capability *cap, nat stack_size,  
			       StgClosure *closure);
StgTSO *createStrictIOThread  (Capability *cap, nat stack_size,  
			       StgClosure *closure);
#endif
