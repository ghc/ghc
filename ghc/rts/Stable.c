/* -----------------------------------------------------------------------------
 * $Id: Stable.c,v 1.16 2001/08/08 16:03:47 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Stable names and stable pointers.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Hash.h"
#include "StablePriv.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "RtsAPI.h"
#include "RtsFlags.h"

/* Comment from ADR's implementation in old RTS:

  This files (together with @ghc/runtime/storage/PerformIO.lhc@ and a
  small change in @HpOverflow.lc@) consists of the changes in the
  runtime system required to implement "Stable Pointers". But we're
  getting a bit ahead of ourselves --- what is a stable pointer and what
  is it used for?

  When Haskell calls C, it normally just passes over primitive integers,
  floats, bools, strings, etc.  This doesn't cause any problems at all
  for garbage collection because the act of passing them makes a copy
  from the heap, stack or wherever they are onto the C-world stack.
  However, if we were to pass a heap object such as a (Haskell) @String@
  and a garbage collection occured before we finished using it, we'd run
  into problems since the heap object might have been moved or even
  deleted.

  So, if a C call is able to cause a garbage collection or we want to
  store a pointer to a heap object between C calls, we must be careful
  when passing heap objects. Our solution is to keep a table of all
  objects we've given to the C-world and to make sure that the garbage
  collector collects these objects --- updating the table as required to
  make sure we can still find the object.


  Of course, all this rather begs the question: why would we want to
  pass a boxed value?

  One very good reason is to preserve laziness across the language
  interface. Rather than evaluating an integer or a string because it
  {\em might\/} be required by the C function, we can wait until the C
  function actually wants the value and then force an evaluation.

  Another very good reason (the motivating reason!) is that the C code
  might want to execute an object of sort $IO ()$ for the side-effects
  it will produce. For example, this is used when interfacing to an X
  widgets library to allow a direct implementation of callbacks.


  The @makeStablePointer :: a -> IO (StablePtr a)@ function
  converts a value into a stable pointer.  It is part of the @PrimIO@
  monad, because we want to be sure we don't allocate one twice by
  accident, and then only free one of the copies.

  \begin{verbatim}
  makeStablePtr#  :: a -> State# RealWorld -> (# RealWorld, a #)
  freeStablePtr#  :: StablePtr# a -> State# RealWorld -> State# RealWorld
  deRefStablePtr# :: StablePtr# a -> State# RealWorld -> 
        (# State# RealWorld, a #)
  \end{verbatim}

  There may be additional functions on the C side to allow evaluation,
  application, etc of a stable pointer.

  When Haskell calls C, it normally just passes over primitive integers,
  floats, bools, strings, etc.  This doesn't cause any problems at all
  for garbage collection because the act of passing them makes a copy
  from the heap, stack or wherever they are onto the C-world stack.
  However, if we were to pass a heap object such as a (Haskell) @String@
  and a garbage collection occured before we finished using it, we'd run
  into problems since the heap object might have been moved or even
  deleted.

  So, if a C call is able to cause a garbage collection or we want to
  store a pointer to a heap object between C calls, we must be careful
  when passing heap objects. Our solution is to keep a table of all
  objects we've given to the C-world and to make sure that the garbage
  collector collects these objects --- updating the table as required to
  make sure we can still find the object.
*/

snEntry *stable_ptr_table;
snEntry *stable_ptr_free;

unsigned int SPT_size;

/* This hash table maps Haskell objects to stable names, so that every
 * call to lookupStableName on a given object will return the same
 * stable name.
 *
 * Reference counting
 * ------------------
 * A plain stable name entry has a zero reference count, which means
 * the entry will dissappear when the object it points to is
 * unreachable.  For stable pointers, we need an entry that sticks
 * around and keeps the object it points to alive, so each stable name
 * entry has an associated reference count.
 *
 * A stable pointer has a weighted reference count N attached to it
 * (actually in its upper 5 bits), which represents the weight
 * 2^(N-1).  The stable name entry keeps a 32-bit reference count, which
 * represents any weight between 1 and 2^32 (represented as zero).
 * When the weight is 2^32, the stable name table owns "all" of the
 * stable pointers to this object, and the entry can be garbage
 * collected if the object isn't reachable.
 *
 * A new stable pointer is given the weight log2(W/2), where W is the
 * weight stored in the table entry.  The new weight in the table is W
 * - 2^log2(W/2).
 *
 * A stable pointer can be "split" into two stable pointers, by
 * dividing the weight by 2 and giving each pointer half.
 * When freeing a stable pointer, the weight of the pointer is added
 * to the weight stored in the table entry.
 * */

HashTable *addrToStableHash;

#define INIT_SPT_SIZE 64

static inline void
initFreeList(snEntry *table, nat n, snEntry *free)
{
  snEntry *p;

  for (p = table + n - 1; p >= table; p--) {
    p->addr   = (P_)free;
    p->old    = NULL;
    p->weight = 0;
    p->sn_obj = NULL;
    free = p;
  }
  stable_ptr_free = table;
}

void
initStablePtrTable(void)
{
  /* the table will be allocated the first time makeStablePtr is
   * called */
  stable_ptr_table = NULL;
  stable_ptr_free  = NULL;
  addrToStableHash = NULL;
  SPT_size = 0;
}

/*
 * get at the real stuff...remove indirections.
 *
 * ToDo: move to a better home.
 */
static
StgClosure*
removeIndirections(StgClosure* p)
{
  StgClosure* q = p;

  while (get_itbl(q)->type == IND ||
         get_itbl(q)->type == IND_STATIC ||
         get_itbl(q)->type == IND_OLDGEN ||
         get_itbl(q)->type == IND_PERM ||
         get_itbl(q)->type == IND_OLDGEN_PERM ) {
      q = ((StgInd *)q)->indirectee;
  }
  return q;
}

StgWord
lookupStableName(StgPtr p)
{
  StgWord sn;

  if (stable_ptr_free == NULL) {
    enlargeStablePtrTable();
  }

  /* removing indirections increases the likelihood
   * of finding a match in the stable name hash table.
   */
  p = (StgPtr)removeIndirections((StgClosure*)p);

  (void *)sn = lookupHashTable(addrToStableHash,(W_)p);
  
  if (sn != 0) {
    ASSERT(stable_ptr_table[sn].addr == p);
    IF_DEBUG(stable,fprintf(stderr,"cached stable name %d at %p\n",sn,p));
    return sn;
  } else {
    sn = stable_ptr_free - stable_ptr_table;
    (P_)stable_ptr_free  = stable_ptr_free->addr;
    stable_ptr_table[sn].weight = 0;
    stable_ptr_table[sn].addr = p;
    stable_ptr_table[sn].sn_obj = NULL;
    /* IF_DEBUG(stable,fprintf(stderr,"new stable name %d at
       %p\n",sn,p)); */
    
    /* add the new stable name to the hash table */
    insertHashTable(addrToStableHash, (W_)p, (void *)sn);

    return sn;
  }
}

static inline void
freeStableName(snEntry *sn)
{
  ASSERT(sn->sn_obj == NULL);
  if (sn->addr != NULL) {
    removeHashTable(addrToStableHash, (W_)sn->addr, NULL);
  }
  sn->addr = (P_)stable_ptr_free;
  stable_ptr_free = sn;
}

StgStablePtr
getStablePtr(StgPtr p)
{
  StgWord sn = lookupStableName(p);
  StgWord weight, n;
  weight = stable_ptr_table[sn].weight;
  if (weight == 0) {
    weight = (StgWord)1 << (BITS_IN(StgWord)-1);
    stable_ptr_table[sn].weight = weight;
    return (StgStablePtr)(sn + (BITS_IN(StgWord) << STABLEPTR_WEIGHT_SHIFT));
  } 
  else if (weight == 1) {
    barf("getStablePtr: too light");
  } 
  else {
    weight /= 2;
    /* find log2(weight) */
    for (n = 0; weight != 1; n++) {
      weight >>= 1;
    }
    stable_ptr_table[sn].weight -= 1 << n;
    return (StgStablePtr)(sn + ((n+1) << STABLEPTR_WEIGHT_SHIFT));
  }
}

void
enlargeStablePtrTable(void)
{
  nat old_SPT_size = SPT_size;
  
  if (SPT_size == 0) {
    // 1st time
    SPT_size = INIT_SPT_SIZE;
    stable_ptr_table = stgMallocWords(SPT_size * sizeof(snEntry), 
				      "initStablePtrTable");
    
    /* we don't use index 0 in the stable name table, because that
     * would conflict with the hash table lookup operations which
     * return NULL if an entry isn't found in the hash table.
     */
    initFreeList(stable_ptr_table+1,INIT_SPT_SIZE-1,NULL);
    addrToStableHash = allocHashTable();
  }
  else {
    // 2nd and subsequent times
    SPT_size *= 2;
    stable_ptr_table = 
      stgReallocWords(stable_ptr_table, SPT_size * sizeof(snEntry),
		      "enlargeStablePtrTable");
    
    initFreeList(stable_ptr_table + old_SPT_size, old_SPT_size, NULL);
  }
}

/* -----------------------------------------------------------------------------
 * Treat stable pointers as roots for the garbage collector.
 *
 * A stable pointer is any stable name entry with a weight > 0.  We'll
 * take the opportunity to zero the "keep" flags at the same time.
 * -------------------------------------------------------------------------- */

void
markStablePtrTable(evac_fn evac)
{
    snEntry *p, *end_stable_ptr_table;
    StgPtr q;
    
    end_stable_ptr_table = &stable_ptr_table[SPT_size];
    
    // Mark all the stable *pointers* (not stable names).
    // _starting_ at index 1; index 0 is unused.
    for (p = stable_ptr_table+1; p < end_stable_ptr_table; p++) {
	q = p->addr;

	// Internal pointers are free slots.  If q == NULL, it's a
	// stable name where the object has been GC'd, but the
	// StableName object (sn_obj) is still alive.
	if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {

	    // save the current addr away: we need to be able to tell
	    // whether the objects moved in order to be able to update
	    // the hash table later.
	    p->old = p->addr;

	    // if the weight is non-zero, treat addr as a root
	    if (p->weight != 0) {
		evac((StgClosure **)&p->addr);
	    }
	}
    }
}

/* -----------------------------------------------------------------------------
 * Thread the stable pointer table for compacting GC.
 * 
 * Here we must call the supplied evac function for each pointer into
 * the heap from the stable pointer table, because the compacting
 * collector may move the object it points to.
 * -------------------------------------------------------------------------- */

void
threadStablePtrTable( evac_fn evac )
{
    snEntry *p, *end_stable_ptr_table;
    StgPtr q;
    
    end_stable_ptr_table = &stable_ptr_table[SPT_size];
    
    for (p = stable_ptr_table+1; p < end_stable_ptr_table; p++) {
	
	if (p->sn_obj != NULL) {
	    evac((StgClosure **)&p->sn_obj);
	}

	q = p->addr;
	if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {
	    evac((StgClosure **)&p->addr);
	}
    }
}

/* -----------------------------------------------------------------------------
 * Garbage collect any dead entries in the stable pointer table.
 *
 * A dead entry has:
 *
 *          - a weight of zero (i.e. 2^32)
 *          - a dead sn_obj
 *
 * Both of these conditions must be true in order to re-use the stable
 * name table entry.  We can re-use stable name table entries for live
 * heap objects, as long as the program has no StableName objects that
 * refer to the entry.
 * -------------------------------------------------------------------------- */

void
gcStablePtrTable( void )
{
    snEntry *p, *end_stable_ptr_table;
    StgPtr q;
    
    end_stable_ptr_table = &stable_ptr_table[SPT_size];
    
    // NOTE: _starting_ at index 1; index 0 is unused.
    for (p = stable_ptr_table + 1; p < end_stable_ptr_table; p++) {
	
	// Update the pointer to the StableName object, if there is one
	if (p->sn_obj != NULL) {
	    p->sn_obj = isAlive(p->sn_obj);
	}
	
	// Internal pointers are free slots.  If q == NULL, it's a
	// stable name where the object has been GC'd, but the
	// StableName object (sn_obj) is still alive.
	q = p->addr;
	if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {

	    // StableNames only:
	    if (p->weight == 0) {
		if (p->sn_obj == NULL) {
		    // StableName object is dead
		    freeStableName(p);
		    IF_DEBUG(stable, fprintf(stderr,"GC'd Stable name %d\n", 
					     p - stable_ptr_table));
		    continue;
		    
		} else {
		    (StgClosure *)p->addr = isAlive((StgClosure *)p->addr);
		    IF_DEBUG(stable, fprintf(stderr,"Stable name %d still alive at %p, weight %d\n", p - stable_ptr_table, p->addr, p->weight));
		}
	    }
	}
    }
}

/* -----------------------------------------------------------------------------
 * Update the StablePtr/StableName hash table
 *
 * The boolean argument 'full' indicates that a major collection is
 * being done, so we might as well throw away the hash table and build
 * a new one.  For a minor collection, we just re-hash the elements
 * that changed.
 * -------------------------------------------------------------------------- */

void
updateStablePtrTable(rtsBool full)
{
    snEntry *p, *end_stable_ptr_table;
    
    if (full && addrToStableHash != NULL) {
	freeHashTable(addrToStableHash,NULL);
	addrToStableHash = allocHashTable();
    }
    
    end_stable_ptr_table = &stable_ptr_table[SPT_size];
    
    // NOTE: _starting_ at index 1; index 0 is unused.
    for (p = stable_ptr_table + 1; p < end_stable_ptr_table; p++) {
	
	if (p->addr == NULL) {
	    if (p->old != NULL) {
		// The target has been garbage collected.  Remove its
		// entry from the hash table.
		removeHashTable(addrToStableHash, (W_)p->old, NULL);
		p->old = NULL;
	    }
	}
	else if (p->addr < (P_)stable_ptr_table 
		 || p->addr >= (P_)end_stable_ptr_table) {
	    // Target still alive, Re-hash this stable name 
	    if (full) {
		insertHashTable(addrToStableHash, (W_)p->addr, 
				(void *)(p - stable_ptr_table));
	    } else if (p->addr != p->old) {
		removeHashTable(addrToStableHash, (W_)p->old, NULL);
		insertHashTable(addrToStableHash, (W_)p->addr, 
				(void *)(p - stable_ptr_table));
	    }
	}
    }
}
