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

// Adding more Capabilities later: this function allocates nurseries
// and initialises other storage-related things.
void storageAddCapabilities (nat from, nat to);

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
bdescr * splitLargeBlock (bdescr *bd, W_ blocks);

/* -----------------------------------------------------------------------------
   Generational garbage collection support

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

/* -----------------------------------------------------------------------------
   The write barrier for MVARs and TVARs
   -------------------------------------------------------------------------- */

void dirty_MVAR(StgRegTable *reg, StgClosure *p);
void dirty_TVAR(Capability *cap, StgTVar *p);

/* -----------------------------------------------------------------------------
   Nursery manipulation
   -------------------------------------------------------------------------- */

extern nursery *nurseries;

void     resetNurseries       ( void );
void     clearNursery         ( Capability *cap );
void     resizeNurseries      ( W_ blocks );
void     resizeNurseriesFixed ( W_ blocks );
W_       countNurseryBlocks   ( void );

/* -----------------------------------------------------------------------------
   Stats 'n' DEBUG stuff
   -------------------------------------------------------------------------- */

void  updateNurseriesStats (void);
W_    countLargeAllocated  (void);
W_    countOccupied        (bdescr *bd);
W_    calcNeeded           (rtsBool force_major, W_ *blocks_needed);

W_    gcThreadLiveWords  (nat i, nat g);
W_    gcThreadLiveBlocks (nat i, nat g);

W_    genLiveWords  (generation *gen);
W_    genLiveBlocks (generation *gen);

W_    calcLiveBlocks (void);
W_    calcLiveWords  (void);

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
