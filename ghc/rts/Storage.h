/* -----------------------------------------------------------------------------
 * $Id: Storage.h,v 1.6 1999/02/02 14:21:34 simonm Exp $
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
  (current_nursery->free = (P_)(hp)+1,		\
   current_nursery->link == NULL ? rtsFalse :	\
   (current_nursery = current_nursery->link,	\
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

   RecordMutable(StgPtr p)       Informs the garbage collector that a
				 previously immutable object has
				 become (permanently) mutable.  Used
				 by thawArray and similar.

   UpdateWithIndirection(p1,p2)  Updates the object at p1 with an
				 indirection pointing to p2.  This is
				 normally called for objects in an old
				 generation (>0) when they are updated.

   -------------------------------------------------------------------------- */

static inline void
recordMutable(StgMutClosure *p)
{
  bdescr *bd;

  ASSERT(closure_MUTABLE(p));

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

static inline void
updateWithIndirection(StgClosure *p1, StgClosure *p2) 
{
  bdescr *bd;

  bd = Bdescr((P_)p1);
  if (bd->gen->no == 0) {
    SET_INFO(p1,&IND_info);
    ((StgInd *)p1)->indirectee = p2;
    TICK_UPD_NEW_IND();
  } else {
    SET_INFO(p1,&IND_OLDGEN_info);
    ((StgIndOldGen *)p1)->indirectee = p2;
    ((StgIndOldGen *)p1)->mut_link = bd->gen->mut_once_list;
    bd->gen->mut_once_list = (StgMutClosure *)p1;
    TICK_UPD_OLD_IND();
  }
}

/* -----------------------------------------------------------------------------
   The CAF list - used to let us revert CAFs

   -------------------------------------------------------------------------- */

extern StgCAF* enteredCAFs;

#endif STORAGE_H

