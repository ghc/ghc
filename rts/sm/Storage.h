/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_STORAGE_H
#define SM_STORAGE_H

#include "Capability.h"

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
   Initialisation / De-initialisation
   -------------------------------------------------------------------------- */

void initStorage(void);
void exitStorage(void);
void freeStorage(rtsBool free_heap);

/* -----------------------------------------------------------------------------
   Storage manager state
   -------------------------------------------------------------------------- */

INLINE_HEADER rtsBool
doYouWantToGC( Capability *cap )
{
  return (cap->r.rCurrentNursery->link == NULL ||
          g0->n_new_large_words >= large_alloc_lim);
}

/* for splitting blocks groups in two */
bdescr * splitLargeBlock (bdescr *bd, nat blocks);

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
#if defined(THREADED_RTS)
extern Mutex sm_mutex;
#endif

#if defined(THREADED_RTS)
#define ACQUIRE_SM_LOCK   ACQUIRE_LOCK(&sm_mutex);
#define RELEASE_SM_LOCK   RELEASE_LOCK(&sm_mutex);
#define ASSERT_SM_LOCK()  ASSERT_LOCK_HELD(&sm_mutex);
#else
#define ACQUIRE_SM_LOCK
#define RELEASE_SM_LOCK
#define ASSERT_SM_LOCK()
#endif

INLINE_HEADER void
recordMutableGen(StgClosure *p, nat gen_no)
{
    bdescr *bd;

    bd = generations[gen_no].mut_list;
    if (bd->free >= bd->start + BLOCK_SIZE_W) {
	bdescr *new_bd;
	new_bd = allocBlock();
	new_bd->link = bd;
	bd = new_bd;
	generations[gen_no].mut_list = bd;
    }
    *bd->free++ = (StgWord)p;

}

INLINE_HEADER void
recordMutableGenLock(StgClosure *p, nat gen_no)
{
    ACQUIRE_SM_LOCK;
    recordMutableGen(p,gen_no);
    RELEASE_SM_LOCK;
}

INLINE_HEADER void
recordMutable(StgClosure *p)
{
    bdescr *bd;
    ASSERT(closure_MUTABLE(p));
    bd = Bdescr((P_)p);
    if (bd->gen_no > 0) recordMutableGen(p, bd->gen_no);
}

INLINE_HEADER void
recordMutableLock(StgClosure *p)
{
    ACQUIRE_SM_LOCK;
    recordMutable(p);
    RELEASE_SM_LOCK;
}

/* -----------------------------------------------------------------------------
   The write barrier for MVARs
   -------------------------------------------------------------------------- */

void dirty_MVAR(StgRegTable *reg, StgClosure *p);

/* -----------------------------------------------------------------------------
   Nursery manipulation
   -------------------------------------------------------------------------- */

extern nursery *nurseries;

void     resetNurseries       ( void );
lnat     clearNurseries       ( void );
void     resizeNurseries      ( nat blocks );
void     resizeNurseriesFixed ( nat blocks );
lnat     countNurseryBlocks   ( void );

/* -----------------------------------------------------------------------------
   Stats 'n' DEBUG stuff
   -------------------------------------------------------------------------- */

lnat    calcAllocated  (rtsBool count_nurseries);
lnat    calcLiveBlocks (void);
lnat    calcLiveWords  (void);
lnat    countOccupied  (bdescr *bd);
lnat    calcNeeded     (void);

/* ----------------------------------------------------------------------------
   Storage manager internal APIs and globals
   ------------------------------------------------------------------------- */

extern bdescr *exec_block;

#define END_OF_STATIC_LIST ((StgClosure*)1)

void move_STACK  (StgStack *src, StgStack *dest);

extern StgClosure * caf_list;
extern StgClosure * revertible_caf_list;

#include "EndPrivate.h"

#endif /* SM_STORAGE_H */
