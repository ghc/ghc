/* -----------------------------------------------------------------------------
 * $Id: Updates.h,v 1.8 1999/03/02 19:44:23 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Definitions related to updates.
 *
 * ---------------------------------------------------------------------------*/

#ifndef UPDATES_H
#define UPDATES_H

/* -----------------------------------------------------------------------------
   Update a closure with an indirection.  This may also involve waking
   up a queue of blocked threads waiting on the result of this
   computation.
   -------------------------------------------------------------------------- */

/* ToDo: overwrite slop words with something safe in case sanity checking 
 *       is turned on.  
 *       (I think the fancy version of the GC is supposed to do this too.)
 */

/* This expands to a fair chunk of code, what with waking up threads 
 * and checking whether we're updating something in a old generation.
 * preferably don't use this macro inline in compiled code.
 */

#define UPD_IND(updclosure, heapptr)                            \
        AWAKEN_BQ(updclosure);                                  \
	updateWithIndirection((StgClosure *)updclosure,         \
			      (StgClosure *)heapptr);

/* -----------------------------------------------------------------------------
   Awaken any threads waiting on this computation
   -------------------------------------------------------------------------- */

extern void awaken_blocked_queue(StgTSO *q);

#define AWAKEN_BQ(closure)						\
     	if (closure->header.info == &BLACKHOLE_BQ_info) {		\
		StgTSO *bq = ((StgBlockingQueue *)closure)->blocking_queue;\
		if (bq != (StgTSO *)&END_TSO_QUEUE_closure) {		\
			STGCALL1(awaken_blocked_queue, bq);		\
		}							\
	}


/* -----------------------------------------------------------------------------
   Push an update frame on the stack.
   -------------------------------------------------------------------------- */

#if defined(PROFILING)
#define PUSH_STD_CCCS(frame) frame->header.prof.ccs = CCCS
#else
#define PUSH_STD_CCCS(frame)
#endif

extern DLL_IMPORT_DATA const StgPolyInfoTable Upd_frame_info; 

#define PUSH_UPD_FRAME(target, Sp_offset)			\
	{							\
		StgUpdateFrame *__frame;			\
		TICK_UPDF_PUSHED();  			        \
		__frame = stgCast(StgUpdateFrame*,Sp + (Sp_offset)) - 1; \
		SET_INFO(__frame,stgCast(StgInfoTable*,&Upd_frame_info));   \
		__frame->link = Su;				\
		__frame->updatee = (StgClosure *)(target);	\
		PUSH_STD_CCCS(__frame);				\
		Su = __frame;					\
	}

/* -----------------------------------------------------------------------------
   Entering CAFs

   When a CAF is first entered, it creates a black hole in the heap,
   and updates itself with an indirection to this new black hole.

   We update the CAF with an indirection to a newly-allocated black
   hole in the heap.  We also set the blocking queue on the newly
   allocated black hole to be empty.

   Why do we make a black hole in the heap when we enter a CAF?
      
       - for a  generational garbage collector, which needs a fast
         test for whether an updatee is in an old generation or not

       - for the parallel system, which can implement updates more
         easily if the updatee is always in the heap. (allegedly).

   When debugging, we maintain a separate CAF list so we can tell when
   a CAF has been garbage collected.
   -------------------------------------------------------------------------- */
   
/* ToDo: only call newCAF when debugging. */

extern void newCAF(StgClosure*);

#define UPD_CAF(cafptr, bhptr)					\
  {								\
    SET_INFO((StgInd *)cafptr,(const StgInfoTable*)&IND_STATIC_info);	        \
    ((StgInd *)cafptr)->indirectee   = (StgClosure *)(bhptr);	\
    STGCALL1(newCAF,(StgClosure *)cafptr);			\
  }

/* -----------------------------------------------------------------------------
   Update-related prototypes
   -------------------------------------------------------------------------- */

DLL_IMPORT_RTS extern STGFUN(Upd_frame_entry);

extern DLL_IMPORT_DATA const StgInfoTable PAP_info;
DLL_IMPORT_RTS STGFUN(PAP_entry);

EXTFUN_RTS(stg_update_PAP);

extern DLL_IMPORT_DATA const StgInfoTable AP_UPD_info;
DLL_IMPORT_RTS STGFUN(AP_UPD_entry);

extern DLL_IMPORT_DATA const StgInfoTable raise_info;

#endif /* UPDATES_H */
