/* -----------------------------------------------------------------------------
 * $Id: Storage.h,v 1.48 2003/03/21 16:18:39 sof Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGE_H
#define STORAGE_H

#include "Block.h"
#include "MBlock.h"
#include "BlockAlloc.h"
#include "StoragePriv.h"
#ifdef PROFILING
#include "LdvProfile.h"
#endif

/* -----------------------------------------------------------------------------
   Initialisation / De-initialisation
   -------------------------------------------------------------------------- */

extern void initStorage(void);
extern void exitStorage(void);

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocate(nat n)       Allocates a chunk of contiguous store
   				n words long, returning a pointer to
				the first word.  Always succeeds.
				
   StgPtr allocatePinned(nat n) Allocates a chunk of contiguous store
   				n words long, which is at a fixed
				address (won't be moved by GC).  
				Returns a pointer to the first word.
				Always succeeds.
				
				NOTE: the GC can't in general handle
				pinned objects, so allocatePinned()
				can only be used for ByteArrays at the
				moment.

				Don't forget to TICK_ALLOC_XXX(...)
				after calling allocate or
				allocatePinned, for the
				benefit of the ticky-ticky profiler.

   rtsBool doYouWantToGC(void)  Returns True if the storage manager is
   				ready to perform a GC, False otherwise.

   lnat  allocated_bytes(void)  Returns the number of bytes allocated
                                via allocate() since the last GC.
				Used in the reporting of statistics.

   SMP: allocate and doYouWantToGC can be used from STG code, they are
   surrounded by a mutex.
   -------------------------------------------------------------------------- */

extern StgPtr  allocate        ( nat n );
extern StgPtr  allocatePinned  ( nat n );
extern lnat    allocated_bytes ( void );

static inline rtsBool
doYouWantToGC( void )
{
  return (alloc_blocks >= alloc_blocks_lim);
}

/* -----------------------------------------------------------------------------
   ExtendNursery(hp,hplim)      When hplim is reached, try to grab
   				some more allocation space.  Returns
				False if the allocation space is
				exhausted, and the application should
				call GarbageCollect().
  -------------------------------------------------------------------------- */

#define ExtendNursery(hp,hplim)			\
  (CurrentNursery->free = (P_)(hp)+1,		\
   CurrentNursery->link == NULL ? rtsFalse :	\
   (CurrentNursery = CurrentNursery->link,	\
    OpenNursery(hp,hplim),			\
    rtsTrue))

extern void PleaseStopAllocating(void);

/* -----------------------------------------------------------------------------
   Performing Garbage Collection

   GarbageCollect(get_roots)    Performs a garbage collection.  
				'get_roots' is called to find all the 
				roots that the system knows about.

   StgClosure 			Called by get_roots on each root.	
   MarkRoot(StgClosure *p)	Returns the new location of the root.
   -------------------------------------------------------------------------- */

extern void GarbageCollect(void (*get_roots)(evac_fn),rtsBool force_major_gc);

/* -----------------------------------------------------------------------------
   Generational garbage collection support

   recordMutable(StgPtr p)       Informs the garbage collector that a
				 previously immutable object has
				 become (permanently) mutable.  Used
				 by thawArray and similar.

   updateWithIndirection(p1,p2)  Updates the object at p1 with an
				 indirection pointing to p2.  This is
				 normally called for objects in an old
				 generation (>0) when they are updated.

   updateWithPermIndirection(p1,p2)  As above but uses a permanent indir.

   -------------------------------------------------------------------------- */

/*
 * Storage manager mutex
 */
#if defined(SMP)
extern Mutex sm_mutex;
#define ACQUIRE_SM_LOCK   ACQUIRE_LOCK(&sm_mutex)
#define RELEASE_SM_LOCK   RELEASE_LOCK(&sm_mutex)
#else
#define ACQUIRE_SM_LOCK
#define RELEASE_SM_LOCK
#endif

/* ToDo: shouldn't recordMutable and recordOldToNewPtrs acquire some
 * kind of lock in the SMP case?
 */
static inline void
recordMutable(StgMutClosure *p)
{
  bdescr *bd;

#ifdef SMP
  ASSERT(p->header.info == &stg_WHITEHOLE_info || closure_MUTABLE(p));
#else
  ASSERT(closure_MUTABLE(p));
#endif

  bd = Bdescr((P_)p);
  if (bd->gen_no > 0) {
    p->mut_link = generations[bd->gen_no].mut_list;
    generations[bd->gen_no].mut_list = p;
  }
}

static inline void
recordOldToNewPtrs(StgMutClosure *p)
{
  bdescr *bd;
  
  bd = Bdescr((P_)p);
  if (bd->gen_no > 0) {
    p->mut_link = generations[bd->gen_no].mut_once_list;
    generations[bd->gen_no].mut_once_list = p;
  }
}

// @LDV profiling
// We zero out the slop when PROFILING is on.
// #ifndef DEBUG
#if !defined(DEBUG) && !defined(PROFILING)
#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    bd = Bdescr((P_)p1);						\
    if (bd->gen_no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&stg_IND_info);					\
      TICK_UPD_NEW_IND();						\
    } else {								\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      if (info != &stg_BLACKHOLE_BQ_info) {				\
        ACQUIRE_SM_LOCK;					        \
        ((StgIndOldGen *)p1)->mut_link = generations[bd->gen_no].mut_once_list;	\
        generations[bd->gen_no].mut_once_list = (StgMutClosure *)p1;			\
        RELEASE_SM_LOCK;					        \
      }									\
      SET_INFO(p1,&stg_IND_OLDGEN_info);				\
      TICK_UPD_OLD_IND();						\
    }									\
  }
#elif defined(PROFILING)
// @LDV profiling
// We call LDV_recordDead_FILL_SLOP_DYNAMIC(p1) regardless of the generation in 
// which p1 resides.
//
// Note: 
//   After all, we do *NOT* need to call LDV_recordCreate() for both IND and 
//   IND_OLDGEN closures because they are inherently used. But, it corrupts
//   the invariants that every closure keeps its creation time in the profiling
//   field. So, we call LDV_recordCreate().

#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    LDV_recordDead_FILL_SLOP_DYNAMIC((p1));                             \
    bd = Bdescr((P_)p1);						\
    if (bd->gen_no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&stg_IND_info);					\
      LDV_recordCreate((p1));                                           \
      TICK_UPD_NEW_IND();						\
    } else {								\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      if (info != &stg_BLACKHOLE_BQ_info) {				\
        ACQUIRE_SM_LOCK;					        \
        ((StgIndOldGen *)p1)->mut_link = generations[bd->gen_no].mut_once_list;	\
        generations[bd->gen_no].mut_once_list = (StgMutClosure *)p1;    \
        RELEASE_SM_LOCK;					        \
      }									\
      SET_INFO(p1,&stg_IND_OLDGEN_info);				\
      LDV_recordCreate((p1));                                           \
    }									\
  }

#else

/* In the DEBUG case, we also zero out the slop of the old closure,
 * so that the sanity checker can tell where the next closure is.
 *
 * Two important invariants: we should never try to update a closure
 * to point to itself, and the closure being updated should not
 * already have been updated (the mutable list will get messed up
 * otherwise).
 */
#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    ASSERT( p1 != p2 && !closure_IND(p1) );				\
    bd = Bdescr((P_)p1);						\
    if (bd->gen_no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&stg_IND_info);					\
      TICK_UPD_NEW_IND();						\
    } else {								\
      if (info != &stg_BLACKHOLE_BQ_info) {				\
	{								\
          StgInfoTable *inf = get_itbl(p1);				\
	  nat np = inf->layout.payload.ptrs,				\
	      nw = inf->layout.payload.nptrs, i;			\
          if (inf->type != THUNK_SELECTOR) {				\
             for (i = 0; i < np + nw; i++) {				\
	        ((StgClosure *)p1)->payload[i] = 0;			\
             }								\
          }								\
        }								\
        ACQUIRE_SM_LOCK;					        \
        ((StgIndOldGen *)p1)->mut_link = generations[bd->gen_no].mut_once_list;	\
        generations[bd->gen_no].mut_once_list = (StgMutClosure *)p1;			\
        RELEASE_SM_LOCK;					        \
      }									\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      SET_INFO(p1,&stg_IND_OLDGEN_info);				\
      TICK_UPD_OLD_IND();						\
    }									\
  }
#endif

/* Static objects all live in the oldest generation
 */
#define updateWithStaticIndirection(info, p1, p2)			\
  {									\
    ASSERT( p1 != p2 && !closure_IND(p1) );				\
    ASSERT( ((StgMutClosure*)p1)->mut_link == NULL );			\
									\
    ACQUIRE_SM_LOCK;						        \
    ((StgMutClosure *)p1)->mut_link = oldest_gen->mut_once_list;	\
    oldest_gen->mut_once_list = (StgMutClosure *)p1;			\
    RELEASE_SM_LOCK;						        \
									\
    ((StgInd *)p1)->indirectee = p2;					\
    SET_INFO((StgInd *)p1, &stg_IND_STATIC_info);			\
    TICK_UPD_STATIC_IND();						\
  }

#if defined(TICKY_TICKY) || defined(PROFILING)
static inline void
updateWithPermIndirection(const StgInfoTable *info, StgClosure *p1, StgClosure *p2) 
{
  bdescr *bd;

  ASSERT( p1 != p2 && !closure_IND(p1) );

#ifdef PROFILING
  // @LDV profiling
  // Destroy the old closure.
  // Nb: LDV_* stuff cannot mix with ticky-ticky
  LDV_recordDead_FILL_SLOP_DYNAMIC(p1);
#endif
  bd = Bdescr((P_)p1);
  if (bd->gen_no == 0) {
    ((StgInd *)p1)->indirectee = p2;
    SET_INFO(p1,&stg_IND_PERM_info);
#ifdef PROFILING
    // @LDV profiling
    // We have just created a new closure.
    LDV_recordCreate(p1);
#endif
    TICK_UPD_NEW_PERM_IND(p1);
  } else {
    ((StgIndOldGen *)p1)->indirectee = p2;
    if (info != &stg_BLACKHOLE_BQ_info) {
      ACQUIRE_SM_LOCK;
      ((StgIndOldGen *)p1)->mut_link = generations[bd->gen_no].mut_once_list;
      generations[bd->gen_no].mut_once_list = (StgMutClosure *)p1;
      RELEASE_SM_LOCK;
    }
    SET_INFO(p1,&stg_IND_OLDGEN_PERM_info);
#ifdef PROFILING
    // @LDV profiling
    // We have just created a new closure.
    LDV_recordCreate(p1);
#endif
    TICK_UPD_OLD_PERM_IND();
  }
}
#endif

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

void revertCAFs( void );

/* -----------------------------------------------------------------------------
   DEBUGGING predicates for pointers

   LOOKS_LIKE_INFO_PTR(p)    returns False if p is definitely not an info ptr
   LOOKS_LIKE_CLOSURE_PTR(p) returns False if p is definitely not a closure ptr

   These macros are complete but not sound.  That is, they might
   return false positives.  Do not rely on them to distinguish info
   pointers from closure pointers, for example.

   We don't use address-space predicates these days, for portability
   reasons, and the fact that code/data can be scattered about the
   address space in a dynamically-linked environment.  Our best option
   is to look at the alleged info table and see whether it seems to
   make sense...
   -------------------------------------------------------------------------- */

#define LOOKS_LIKE_INFO_PTR(p) \
   (p && ((StgInfoTable *)(INFO_PTR_TO_STRUCT(p)))->type != INVALID_OBJECT && \
    ((StgInfoTable *)(INFO_PTR_TO_STRUCT(p)))->type < N_CLOSURE_TYPES)

#define LOOKS_LIKE_CLOSURE_PTR(p) \
   (LOOKS_LIKE_INFO_PTR(((StgClosure *)(p))->header.info))

/* -----------------------------------------------------------------------------
   Macros for calculating how big a closure will be (used during allocation)
   -------------------------------------------------------------------------- */

static __inline__ StgOffset PAP_sizeW   ( nat n_args )
{ return sizeofW(StgPAP) + n_args; }

static __inline__ StgOffset AP_STACK_sizeW ( nat size )
{ return sizeofW(StgAP_STACK) + size; }

static __inline__ StgOffset CONSTR_sizeW( nat p, nat np )
{ return sizeofW(StgHeader) + p + np; }

static __inline__ StgOffset THUNK_SELECTOR_sizeW ( void )
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

static __inline__ StgOffset BLACKHOLE_sizeW ( void )
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

/* --------------------------------------------------------------------------
   Sizes of closures
   ------------------------------------------------------------------------*/

static __inline__ StgOffset sizeW_fromITBL( const StgInfoTable* itbl ) 
{ return sizeofW(StgClosure) 
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

static __inline__ StgOffset ap_stack_sizeW( StgAP_STACK* x )
{ return AP_STACK_sizeW(x->size); }

static __inline__ StgOffset pap_sizeW( StgPAP* x )
{ return PAP_sizeW(x->n_args); }

static __inline__ StgOffset arr_words_sizeW( StgArrWords* x )
{ return sizeofW(StgArrWords) + x->words; }

static __inline__ StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x )
{ return sizeofW(StgMutArrPtrs) + x->ptrs; }

static __inline__ StgWord tso_sizeW ( StgTSO *tso )
{ return TSO_STRUCT_SIZEW + tso->stack_size; }

/* -----------------------------------------------------------------------------
   Sizes of stack frames
   -------------------------------------------------------------------------- */

static inline StgWord stack_frame_sizeW( StgClosure *frame )
{
    StgRetInfoTable *info;

    info = get_ret_itbl(frame);
    switch (info->i.type) {

    case RET_DYN:
    {
	StgRetDyn *dyn = (StgRetDyn *)frame;
	return  sizeofW(StgRetDyn) + RET_DYN_SIZE + 
	    GET_PTRS(dyn->liveness) + GET_NONPTRS(dyn->liveness);
    }
	    
    case RET_FUN:
	return sizeofW(StgRetFun) + ((StgRetFun *)frame)->size;

    case RET_BIG:
    case RET_VEC_BIG:
	return 1 + info->i.layout.large_bitmap->size;

    case RET_BCO:
	return 2 + BCO_BITMAP_SIZE((StgBCO *)((P_)frame)[1]);

    default:
	return 1 + BITMAP_SIZE(info->i.layout.bitmap);
    }
}

/* -----------------------------------------------------------------------------
   Debugging bits
   -------------------------------------------------------------------------- */

#if defined(DEBUG)
void printMutOnceList(generation *gen);
void printMutableList(generation *gen);
#endif

#endif // STORAGE_H
