/* -----------------------------------------------------------------------------
 * $Id: Updates.h,v 1.18 2000/05/15 14:38:11 simonmar Exp $
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

#ifdef TICKY_TICKY
# define UPD_IND(updclosure, heapptr) UPD_PERM_IND(updclosure,heapptr)
#else
# define UPD_IND(updclosure, heapptr) UPD_REAL_IND(updclosure,heapptr)
#endif

/* UPD_IND actually does a PERM_IND if TICKY_TICKY is on;
   if you *really* need an IND use UPD_REAL_IND
 */
#ifdef SMP
#define UPD_REAL_IND(updclosure, heapptr)				\
   {									\
	const StgInfoTable *info;					\
	if (Bdescr((P_)updclosure)->back != (bdescr *)BaseReg) {	\
		info = LOCK_CLOSURE(updclosure);			\
	} else {							\
		info = updclosure->header.info;				\
	}								\
        AWAKEN_BQ(info,updclosure);					\
	updateWithIndirection(info,					\
			      (StgClosure *)updclosure,			\
			      (StgClosure *)heapptr);			\
   }
#else
#define UPD_REAL_IND(updclosure, heapptr)		\
   {							\
	const StgInfoTable *info;			\
	info = ((StgClosure *)updclosure)->header.info;	\
        AWAKEN_BQ(info,updclosure);			\
	updateWithIndirection(info,			\
			      (StgClosure *)updclosure,	\
			      (StgClosure *)heapptr);	\
   }
#endif

#if defined(PROFILING) || defined(TICKY_TICKY)
#define UPD_PERM_IND(updclosure, heapptr)			\
   {								\
	const StgInfoTable *info;				\
	info = ((StgClosure *)updclosure)->header.info;		\
        AWAKEN_BQ(info,updclosure);				\
	updateWithPermIndirection(info,				\
				  (StgClosure *)updclosure,	\
				  (StgClosure *)heapptr);	\
   }
#endif

#ifdef SMP
#define UPD_IND_NOLOCK(updclosure, heapptr)				\
   {									\
	const StgInfoTable *info;					\
	info = updclosure->header.info;					\
        AWAKEN_BQ(info,updclosure);					\
	updateWithIndirection(info,					\
			      (StgClosure *)updclosure,			\
			      (StgClosure *)heapptr);			\
   }
#else
#define UPD_IND_NOLOCK(updclosure,heapptr) UPD_IND(updclosure,heapptr)
#endif

/* -----------------------------------------------------------------------------
   Awaken any threads waiting on this computation
   -------------------------------------------------------------------------- */

#if defined(PAR) 

/* 
   In a parallel setup several types of closures, might have a blocking queue:
     BLACKHOLE_BQ ... same as in the default concurrent setup; it will be
                      reawakened via calling UPD_IND on that closure after
		      having finished the computation of the graph
     FETCH_ME_BQ  ... a global indirection (FETCH_ME) may be entered by a 
                      local TSO, turning it into a FETCH_ME_BQ; it will be
		      reawakened via calling processResume
     RBH          ... a revertible black hole may be entered by another 
                      local TSO, putting it onto its blocking queue; since
		      RBHs only exist while the corresponding closure is in 
		      transit, they will be reawakened via calling 
		      convertToFetchMe (upon processing an ACK message)

   In a parallel setup a blocking queue may contain 3 types of closures:
     TSO           ... as in the default concurrent setup
     BLOCKED_FETCH ... indicating that a TSO on another PE is waiting for
                       the result of the current computation
     CONSTR        ... a RBHSave closure (which contains data ripped out of
                       the closure to make room for a blocking queue; since
		       it only contains data we use the exisiting type of
		       a CONSTR closure); this closure is the end of a 
		       blocking queue for an RBH closure; it only exists in
		       this kind of blocking queue and must be at the end
		       of the queue
*/		      
extern void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#define DO_AWAKEN_BQ(bqe, node)  STGCALL2(awakenBlockedQueue, bqe, node);

#define AWAKEN_BQ(info,closure)						\
     	if (info == &BLACKHOLE_BQ_info ||               \
	    info == &FETCH_ME_BQ_info ||                \
	    get_itbl(closure)->type == RBH) {		                \
		StgBlockingQueueElement *bqe = ((StgBlockingQueue *)closure)->blocking_queue;\
		ASSERT(bqe!=END_BQ_QUEUE);		                \
		DO_AWAKEN_BQ(bqe, closure);     	                \
	}

#elif defined(GRAN)

extern void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#define DO_AWAKEN_BQ(bq, node)  STGCALL2(awakenBlockedQueue, bq, node);

/* In GranSim we don't have FETCH_ME or FETCH_ME_BQ closures, so they are
   not checked. The rest of the code is the same as for GUM.
*/
#define AWAKEN_BQ(info,closure)						\
     	if (info == &BLACKHOLE_BQ_info ||               \
	    get_itbl(closure)->type == RBH) {		                \
		StgBlockingQueueElement *bqe = ((StgBlockingQueue *)closure)->blocking_queue;\
		ASSERT(bqe!=END_BQ_QUEUE);		                \
		DO_AWAKEN_BQ(bqe, closure);     	                \
	}


#else /* !GRAN && !PAR */

extern void awakenBlockedQueue(StgTSO *q);
#define DO_AWAKEN_BQ(closure)  	\
        STGCALL1(awakenBlockedQueue,		\
		 ((StgBlockingQueue *)closure)->blocking_queue);

#define AWAKEN_BQ(info,closure)						\
     	if (info == &BLACKHOLE_BQ_info) {				\
          DO_AWAKEN_BQ(closure);                                        \
	}

#endif /* GRAN || PAR */

/* -------------------------------------------------------------------------
   Push an update frame on the stack.
   ------------------------------------------------------------------------- */

#if defined(PROFILING)
#define PUSH_STD_CCCS(frame) frame->header.prof.ccs = CCCS
#else
#define PUSH_STD_CCCS(frame)
#endif

extern DLL_IMPORT_DATA const StgPolyInfoTable upd_frame_info; 

#define PUSH_UPD_FRAME(target, Sp_offset)			\
	{							\
		StgUpdateFrame *__frame;			\
		TICK_UPDF_PUSHED(target, GET_INFO((StgClosure*)target)); \
		__frame = (StgUpdateFrame *)(Sp + (Sp_offset)) - 1; \
		SET_INFO(__frame, (StgInfoTable *)&upd_frame_info);   \
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

/* newCAF must be called before the itbl ptr is overwritten, since
   newCAF records the old itbl ptr in order to do CAF reverting
   (which Hugs needs to do in order that combined mode works right.)
*/
#define UPD_CAF(cafptr, bhptr)						\
  {									\
    LOCK_CLOSURE(cafptr);						\
    STGCALL1(newCAF,(StgClosure *)cafptr);				\
    ((StgInd *)cafptr)->indirectee   = (StgClosure *)(bhptr);		\
    SET_INFO((StgInd *)cafptr,(const StgInfoTable*)&IND_STATIC_info);	\
  }

#ifdef INTERPRETER
extern void newCAF_made_by_Hugs(StgCAF*);
#endif

/* -----------------------------------------------------------------------------
   Update-related prototypes
   -------------------------------------------------------------------------- */

DLL_IMPORT_RTS extern STGFUN(upd_frame_entry);

extern DLL_IMPORT_DATA const StgInfoTable PAP_info;
DLL_IMPORT_RTS STGFUN(PAP_entry);

EXTFUN_RTS(stg_update_PAP);

extern DLL_IMPORT_DATA const StgInfoTable AP_UPD_info;
DLL_IMPORT_RTS STGFUN(AP_UPD_entry);

extern DLL_IMPORT_DATA const StgInfoTable raise_info;

#endif /* UPDATES_H */
