/* -----------------------------------------------------------------------------
 * $Id: StoragePriv.h,v 1.8 1999/02/05 16:03:02 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Internal Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGEPRIV_H
#define STORAGEPRIV_H

/* GENERATION GC NOTES
 *
 * We support an arbitrary number of generations, with an arbitrary number
 * of steps per generation.  Notes (in no particular order):
 *
 *       - all generations except the oldest should have two steps.  This gives
 *         objects a decent chance to age before being promoted, and in
 *         particular will ensure that we don't end up with too many
 *         thunks being updated in older generations.
 *
 *       - the oldest generation has one step.  There's no point in aging
 *         objects in the oldest generation.
 *
 *       - generation 0, step 0 (G0S0) is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         are never freed.
 *
 *       - during garbage collection, each step which is an evacuation
 *         destination (i.e. all steps except G0S0) is allocated a to-space.
 *         evacuated objects are allocated into the step's to-space until
 *         GC is finished, when the original step's contents may be freed
 *         and replaced by the to-space.
 *
 *       - the mutable-list is per-generation (not per-step).  G0 doesn't 
 *         have one (since every garbage collection collects at least G0).
 * 
 *       - block descriptors contain pointers to both the step and the
 *         generation that the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-step, and are promoted in the same way
 *         as small objects, except that we may allocate large objects into
 *         generation 1 initially.
 */

typedef struct _step {
  nat no;			/* step number */
  bdescr *blocks;		/* blocks in this step */
  nat n_blocks;			/* number of blocks */
  struct _step *to;		/* where collected objects from this step go */
  struct _generation *gen;	/* generation this step belongs to */
  bdescr *large_objects;	/* large objects (doubly linked) */

  /* temporary use during GC: */
  StgPtr  hp;			/* next free locn in to-space */
  StgPtr  hpLim;		/* end of current to-space block */
  bdescr *hp_bd;		/* bdescr of current to-space block */
  bdescr *to_space;		/* bdescr of first to-space block */
  nat     to_blocks;		/* number of blocks in to-space */
  bdescr *scan_bd;		/* block currently being scanned */
  StgPtr  scan;			/* scan pointer in current block */
  bdescr *new_large_objects;    /* large objects collected so far */
  bdescr *scavenged_large_objects; /* live large objects after GC (dbl link) */
} step;

typedef struct _generation {
  nat no;			/* generation number */
  step *steps;			/* steps */
  nat n_steps;			/* number of steps */
  nat max_blocks;		/* max blocks in step 0 */
  StgMutClosure *mut_list;      /* mutable objects in this generation (not G0)*/
  StgMutClosure *mut_once_list; /* objects that point to younger generations */

  /* temporary use during GC: */
  StgMutClosure *saved_mut_list;

  /* stats information */
  nat collections;
  nat failed_promotions;
} generation;

#define END_OF_STATIC_LIST stgCast(StgClosure*,1)

extern generation *generations;

extern generation *g0;
extern step *g0s0;
extern generation *oldest_gen;

extern void newCAF(StgClosure*);
extern StgTSO *relocate_TSO(StgTSO *src, StgTSO *dest);

extern StgWeak    *weak_ptr_list;
extern StgClosure *caf_list;

extern bdescr *small_alloc_list;
extern bdescr *large_alloc_list;

extern StgPtr alloc_Hp;
extern StgPtr alloc_HpLim;

extern bdescr *nursery;

extern nat nursery_blocks;
extern nat alloc_blocks;
extern nat alloc_blocks_lim;

extern bdescr *allocNursery ( bdescr *last_bd, nat blocks );
extern void resizeNursery ( nat blocks );

extern lnat calcLive( void );
extern lnat calcNeeded( void );

static inline void
dbl_link_onto(bdescr *bd, bdescr **list)
{
  bd->link = *list;
  bd->back = NULL;
  if (*list) {
    (*list)->back = bd; /* double-link the list */
  }
  *list = bd;
}

/* MUTABLE LISTS
 * A mutable list is ended with END_MUT_LIST, so that we can use NULL
 * as an indication that an object is not on a mutable list.
 */
#define END_MUT_LIST ((StgMutClosure *)(void *)&END_MUT_LIST_closure)

#ifdef DEBUG
extern void memInventory(void);
extern void checkSanity(nat N);
#endif

#endif /* STORAGEPRIV_H */
