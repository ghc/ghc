/* -----------------------------------------------------------------------------
 * $Id: Stable.c,v 1.1 1999/01/27 10:11:27 simonm Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Stable names and stable pointers.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Hash.h"
#include "StablePriv.h"
#include "GC.h"
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
  There is also a C procedure @FreeStablePtr@ which frees a stable pointer.

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
 * 2^N.  The stable name entry keeps a 32-bit reference count, which
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
    p->addr = (P_)free;
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

StgWord
lookupStableName(StgPtr p)
{
  StgWord sn;

  if (stable_ptr_free == NULL) {
    enlargeStablePtrTable();
  }
    
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
  sn->addr = (P_)stable_ptr_free;
  stable_ptr_free = sn;
}

StgStablePtr
getStablePtr(StgPtr p)
{
  StgWord sn = lookupStableName(p);
  StgWord weight, weight_2;

  weight = stable_ptr_table[sn].weight;
  if (weight == 0) {
    weight = 1 << (BITS_IN(StgWord)-1);
    stable_ptr_table[sn].weight = weight;
    return (StgStablePtr)(sn + ((BITS_IN(StgWord)-1) << STABLEPTR_WEIGHT_SHIFT));
  } 
  else if (weight == 1) {
    barf("getStablePtr: too light");
  } 
  else {
    weight /= 2;
    /* find log2(weight) */
    for (weight_2 = 1; weight != 1; weight_2++) {
      weight >>= 1;
    }
    stable_ptr_table[sn].weight -= 2^weight_2;
    return (StgStablePtr)(sn + (weight_2 << STABLEPTR_WEIGHT_SHIFT));
  }
}

void
enlargeStablePtrTable(void)
{
  nat old_SPT_size = SPT_size;
  
  if (SPT_size == 0) {
    /* 1st time */
    SPT_size = INIT_SPT_SIZE;
    stable_ptr_table = stgMallocWords(SPT_size * sizeof(snEntry), 
				      "initStablePtrTable");
    
    initFreeList(stable_ptr_table+1,INIT_SPT_SIZE-1,NULL);
    addrToStableHash = allocHashTable();
  }
  else {
    /* 2nd and subsequent times */
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
markStablePtrTable(rtsBool full)
{
  snEntry *p, *end_stable_ptr_table;
  StgPtr q;
  StgClosure *new;

  if (SPT_size == 0)
    return;

  if (full) {
    freeHashTable(addrToStableHash,NULL);
    addrToStableHash = allocHashTable();
  }

  end_stable_ptr_table = &stable_ptr_table[SPT_size];

  /* Mark all the stable *pointers* (not stable names) 
   */
  for (p = stable_ptr_table; p < end_stable_ptr_table; p++) {
    q = p->addr;
    /* internal pointers or NULL are free slots */
    if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {
      if (p->weight != 0) {
	new = MarkRoot((StgClosure *)q);
	/* Update the hash table */
	if (full) {
	  insertHashTable(addrToStableHash, (W_)new, (void *)(p - stable_ptr_table));
	  (StgClosure *)p->addr = new;
	} else if ((P_)new != q) {
	  removeHashTable(addrToStableHash, (W_)q, NULL);
	  insertHashTable(addrToStableHash, (W_)new, (void *)(p - stable_ptr_table));
	  (StgClosure *)p->addr = new;
	}
	/* IF_DEBUG(stable, fprintf(stderr,"Stable ptr %d still alive
	   at %p, weight %d\n", p - stable_ptr_table, new,
	   p->weight)); */
      }
      else { 
	/* reset the keep flag */
	p->keep = rtsFalse;
      }
    }
  }
}

/* -----------------------------------------------------------------------------
 * Garbage collect any dead entries in the stable pointer table.
 *
 * A dead entry has:
 *
 *          - a weight of zero (i.e. 2^32)
 *          - a false keep flag
 *
 * The keep flag is set by the garbage collector whenever it
 * encounters a StableName object on the heap.  
 *
 * The boolean argument 'full' indicates that a major collection is
 * being done, so we might as well throw away the hash table and build
 * a new one.  For a minor collection, we just re-hash the elements
 * that changed.
 * -------------------------------------------------------------------------- */

void
gcStablePtrTable(rtsBool full)
{
  snEntry *p, *end_stable_ptr_table;
  StgPtr q, new;

  if (SPT_size == 0) {
    return;
  }

  end_stable_ptr_table = &stable_ptr_table[SPT_size];

  for (p = stable_ptr_table; p < end_stable_ptr_table; p++) {
    q = p->addr;

    if (q && (q < (P_)stable_ptr_table || q >= (P_)end_stable_ptr_table)) {

      /* We're only interested in Stable Names here. */
      if (p->weight == 0) {
	
	if (((StgClosure *)new = isAlive((StgClosure *)q))) {
	  IF_DEBUG(stable, fprintf(stderr,"Stable name %d still alive at %p, weight %d\n", p - stable_ptr_table, new, p->weight));

	  p->addr = new;
	  /* Re-hash this stable name */
	  if (full) {
	    insertHashTable(addrToStableHash, (W_)new, (void *)(p - stable_ptr_table));
	  } else if (new != q) {
	    removeHashTable(addrToStableHash, (W_)q, NULL);
	    insertHashTable(addrToStableHash, (W_)new, (void *)(p - stable_ptr_table));
	  }
	}

	else {
	  /* If there are still StableName objects in the heap
	   * pointing to this entry (p->keep == rtsTrue), then
	   * don't free the entry just yet.
	   */
	  if (p->keep)
	    p->addr = NULL;
	  else
	    freeStableName(p);
	}
      }
    }
  }
}
