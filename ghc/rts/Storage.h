/* -----------------------------------------------------------------------------
 * $Id: Storage.h,v 1.11 1999/11/09 15:46:59 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGE_H
#define STORAGE_H

#include "Block.h"
#include "BlockAlloc.h"
#include "StoragePriv.h"

/* -----------------------------------------------------------------------------
   Initialisation / De-initialisation
   -------------------------------------------------------------------------- */

extern void initStorage(void);
extern void exitStorage(void);

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocate(int n)       Allocates a chunk of contiguous store
   				n words long, returning a pointer to
				the first word.  Always succeeds.
				
				Don't forget to TICK_ALLOC_XXX(...)
				after calling allocate, for the
				benefit of the ticky-ticky profiler.

   rtsBool doYouWantToGC(void)  Returns True if the storage manager is
   				ready to perform a GC, False otherwise.

   lnat  allocated_bytes(void)  Returns the number of bytes allocated
                                via allocate() since the last GC.
				Used in the reoprting of statistics.

   SMP: allocate and doYouWantToGC can be used from STG code, they are
   surrounded by a mutex.
   -------------------------------------------------------------------------- */

extern StgPtr  allocate(nat n);
static inline rtsBool doYouWantToGC(void)
{
  return (alloc_blocks >= alloc_blocks_lim);
}
extern lnat allocated_bytes(void);

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

extern void   GarbageCollect(void (*get_roots)(void));
extern StgClosure *MarkRoot(StgClosure *p);

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

static inline void
recordMutable(StgMutClosure *p)
{
  bdescr *bd;

#ifdef SMP
  ASSERT(p->header.info == &WHITEHOLE_info || closure_MUTABLE(p));
#else
  ASSERT(closure_MUTABLE(p));
#endif

  bd = Bdescr((P_)p);
  if (bd->gen->no > 0) {
    p->mut_link = bd->gen->mut_list;
    bd->gen->mut_list = p;
  }
}

static inline void
recordOldToNewPtrs(StgMutClosure *p)
{
  bdescr *bd;
  
  bd = Bdescr((P_)p);
  if (bd->gen->no > 0) {
    p->mut_link = bd->gen->mut_once_list;
    bd->gen->mut_once_list = p;
  }
}

#define updateWithIndirection(info, p1, p2)				\
  {									\
    bdescr *bd;								\
									\
    bd = Bdescr((P_)p1);						\
    if (bd->gen->no == 0) {						\
      ((StgInd *)p1)->indirectee = p2;					\
      SET_INFO(p1,&IND_info);						\
      TICK_UPD_NEW_IND();						\
    } else {								\
      ((StgIndOldGen *)p1)->indirectee = p2;				\
      if (info != &BLACKHOLE_BQ_info) {					\
        ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;	\
        bd->gen->mut_once_list = (StgMutClosure *)p1;			\
      }									\
      SET_INFO(p1,&IND_OLDGEN_info);					\
      TICK_UPD_OLD_IND();						\
    }									\
  }

#if defined(TICKY_TICKY) || defined(PROFILING)
static inline void
updateWithPermIndirection(info, StgClosure *p1, StgClosure *p2) 
{
  bdescr *bd;

  bd = Bdescr((P_)p1);
  if (bd->gen->no == 0) {
    ((StgInd *)p1)->indirectee = p2;
    SET_INFO(p1,&IND_PERM_info);
    TICK_UPD_NEW_PERM_IND(p1);
  } else {
    ((StgIndOldGen *)p1)->indirectee = p2;
    if (info != &BLACKHOLE_BQ_info) {
      ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;
      bd->gen->mut_once_list = (StgMutClosure *)p1;
    }
    SET_INFO(p1,&IND_OLDGEN_PERM_info);
    TICK_UPD_OLD_PERM_IND();
  }
}
#endif

/* -----------------------------------------------------------------------------
   The CAF list - used to let us revert CAFs

   -------------------------------------------------------------------------- */

extern StgCAF* enteredCAFs;

#endif STORAGE_H

