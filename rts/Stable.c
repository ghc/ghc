/* -*- tab-width: 4 -*- */

/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Stable names and stable pointers.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "Hash.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "Stable.h"

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

  One final reason is that we may want to store composite Haskell
  values in data structures implemented in the C side. Serializing and
  deserializing these structures into unboxed form suitable for C may
  be more expensive than maintaining the extra layer of indirection of
  stable pointers.

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

  Stable Pointers are exported to the outside world as indices and not
  pointers, because the stable pointer table is allowed to be
  reallocated for growth. The table is never shrunk for its space to
  be reclaimed.

  Future plans for stable ptrs include distinguishing them by the
  generation of the pointed object. See
  http://hackage.haskell.org/trac/ghc/ticket/7670 for details.
*/

snEntry *stable_name_table = NULL;
static snEntry *stable_name_free = NULL;
static unsigned int SNT_size = 0;
#define INIT_SNT_SIZE 64

spEntry *stable_ptr_table = NULL;
static spEntry *stable_ptr_free = NULL;
static unsigned int SPT_size = 0;
#define INIT_SPT_SIZE 64

#ifdef THREADED_RTS
Mutex stable_mutex;
#endif

static void enlargeStableNameTable(void);
static void enlargeStablePtrTable(void);

/* This hash table maps Haskell objects to stable names, so that every
 * call to lookupStableName on a given object will return the same
 * stable name.
 *
 * OLD COMMENTS about reference counting follow.  The reference count
 * in a stable name entry is now just a counter.
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

static HashTable *addrToStableHash = NULL;

STATIC_INLINE void
initSnEntryFreeList(snEntry *table, nat n, snEntry *free)
{
  snEntry *p;
  for (p = table + n - 1; p >= table; p--) {
    p->addr   = (P_)free;
    p->old    = NULL;
    p->sn_obj = NULL;
    free = p;
  }
  stable_name_free = table;
}

STATIC_INLINE void
initSpEntryFreeList(spEntry *table, nat n, spEntry *free)
{
  spEntry *p;
  for (p = table + n - 1; p >= table; p--) {
      p->addr = (P_)free;
      free = p;
  }
  stable_ptr_free = table;
}

void
initStableTables(void)
{
    if (SNT_size > 0) return;
    SNT_size = INIT_SNT_SIZE;
    stable_name_table = stgMallocBytes(SNT_size * sizeof *stable_name_table,
                                       "initStableNameTable");
    /* we don't use index 0 in the stable name table, because that
     * would conflict with the hash table lookup operations which
     * return NULL if an entry isn't found in the hash table.
     */
    initSnEntryFreeList(stable_name_table + 1,INIT_SNT_SIZE-1,NULL);
    addrToStableHash = allocHashTable();

    if (SPT_size > 0) return;
    SPT_size = INIT_SPT_SIZE;
    stable_ptr_table = stgMallocBytes(SPT_size * sizeof *stable_ptr_table,
                                      "initStablePtrTable");
    initSpEntryFreeList(stable_ptr_table,INIT_SPT_SIZE,NULL);

#ifdef THREADED_RTS
    initMutex(&stable_mutex);
#endif
}

void
exitStableTables(void)
{
    if (addrToStableHash)
        freeHashTable(addrToStableHash, NULL);
    addrToStableHash = NULL;

    if (stable_name_table)
        stgFree(stable_name_table);
    stable_name_table = NULL;
    SNT_size = 0;

    if (stable_ptr_table)
        stgFree(stable_ptr_table);
    stable_ptr_table = NULL;
    SPT_size = 0;

#ifdef THREADED_RTS
    closeMutex(&stable_mutex);
#endif
}

/*
 * get at the real stuff...remove indirections.
 * It untags pointers before dereferencing and
 * retags the real stuff with its tag (if there
 * is any) when returning.
 *
 * ToDo: move to a better home.
 */
static
StgClosure*
removeIndirections(StgClosure* p)
{
  StgWord tag = GET_CLOSURE_TAG(p);
  StgClosure* q = UNTAG_CLOSURE(p);

  while (get_itbl(q)->type == IND ||
         get_itbl(q)->type == IND_STATIC ||
         get_itbl(q)->type == IND_PERM) {
      q = ((StgInd *)q)->indirectee;
      tag = GET_CLOSURE_TAG(q);
      q = UNTAG_CLOSURE(q);
  }

  return TAG_CLOSURE(tag,q);
}

static StgWord
lookupStableName_(StgPtr p)
{
  StgWord sn;
  void* sn_tmp;

  if (stable_name_free == NULL) {
    enlargeStableNameTable();
  }

  /* removing indirections increases the likelihood
   * of finding a match in the stable name hash table.
   */
  p = (StgPtr)removeIndirections((StgClosure*)p);

  // register the untagged pointer.  This just makes things simpler.
  p = (StgPtr)UNTAG_CLOSURE((StgClosure*)p);

  sn_tmp = lookupHashTable(addrToStableHash,(W_)p);
  sn = (StgWord)sn_tmp;

  if (sn != 0) {
    ASSERT(stable_name_table[sn].addr == p);
    debugTrace(DEBUG_stable, "cached stable name %ld at %p",sn,p);
    return sn;
  }

  sn = stable_name_free - stable_name_table;
  stable_name_free  = (snEntry*)(stable_name_free->addr);
  stable_name_table[sn].addr = p;
  stable_name_table[sn].sn_obj = NULL;
  /* debugTrace(DEBUG_stable, "new stable name %d at %p\n",sn,p); */

  /* add the new stable name to the hash table */
  insertHashTable(addrToStableHash, (W_)p, (void *)sn);

  return sn;
}

StgWord
lookupStableName(StgPtr p)
{
    StgWord res;

    initStableTables();
    ACQUIRE_LOCK(&stable_mutex);
    res = lookupStableName_(p);
    RELEASE_LOCK(&stable_mutex);
    return res;
}

STATIC_INLINE void
freeSnEntry(snEntry *sn)
{
  ASSERT(sn->sn_obj == NULL);
  if(sn->addr != NULL) {
      /* StableName object may die before pointee, in which case we
       * need to remove from hash table, or after pointee, in which
       * case addr==NULL and we already removed it. */
      removeHashTable(addrToStableHash, (W_)sn->addr, NULL);
  }
  sn->addr = (P_)stable_name_free;
  stable_name_free = sn;
}

STATIC_INLINE void
freeSpEntry(spEntry *sp)
{
    sp->addr = (P_)stable_ptr_free;
    stable_ptr_free = sp;
}

StgStablePtr
getStablePtr(StgPtr p)
{
  StgWord sp;

  initStableTables();
  ACQUIRE_LOCK(&stable_mutex);
  if (!stable_ptr_free) enlargeStablePtrTable();
  sp = stable_ptr_free - stable_ptr_table;
  stable_ptr_free  = (spEntry*)(stable_ptr_free->addr);
  stable_ptr_table[sp].addr = p;
  RELEASE_LOCK(&stable_mutex);
  return (StgStablePtr)(sp);
}

void
freeStablePtrUnsafe(StgStablePtr sp)
{
    ASSERT((StgWord)sp < SPT_size);
    freeSpEntry(&stable_ptr_table[(StgWord)sp]);
}

void
freeStablePtr(StgStablePtr sp)
{
    initStableTables();
    ACQUIRE_LOCK(&stable_mutex);
    freeStablePtrUnsafe(sp);
    RELEASE_LOCK(&stable_mutex);
}

static void
enlargeStableNameTable(void)
{
    nat old_SNT_size = SNT_size;

    // 2nd and subsequent times
    SNT_size *= 2;
    stable_name_table =
        stgReallocBytes(stable_name_table,
                        SNT_size * sizeof *stable_name_table,
                        "enlargeStableNameTable");

    initSnEntryFreeList(stable_name_table + old_SNT_size, old_SNT_size, NULL);
}

static void
enlargeStablePtrTable(void)
{
    nat old_SPT_size = SPT_size;

    // 2nd and subsequent times
    SPT_size *= 2;
    stable_ptr_table =
        stgReallocBytes(stable_ptr_table,
                        SPT_size * sizeof *stable_ptr_table,
                        "enlargeStablePtrTable");

    initSpEntryFreeList(stable_ptr_table + old_SPT_size, old_SPT_size, NULL);
}

/* -----------------------------------------------------------------------------
 * We must lock the StablePtr table during GC, to prevent simultaneous
 * calls to freeStablePtr().
 * -------------------------------------------------------------------------- */

void
stableLock(void)
{
    initStableTables();
    ACQUIRE_LOCK(&stable_mutex);
}

void
stableUnlock(void)
{
    RELEASE_LOCK(&stable_mutex);
}

/* -----------------------------------------------------------------------------
 * Treat stable pointers as roots for the garbage collector.
 * -------------------------------------------------------------------------- */

#define FOR_EACH_STABLE_PTR(p, CODE)                                    \
    do {                                                                \
        spEntry *p;                                                     \
        spEntry *__end_ptr = &stable_ptr_table[SPT_size];               \
        for (p = stable_ptr_table; p < __end_ptr; p++) {                \
            /* Internal pointers are free slots. NULL is last in free */ \
            /* list. */                                                 \
            if (p->addr &&                                              \
                (p->addr < (P_)stable_ptr_table || p->addr >= (P_)__end_ptr)) \
            {                                                           \
                do { CODE } while(0);                                   \
            }                                                           \
        }                                                               \
    } while(0)

#define FOR_EACH_STABLE_NAME(p, CODE)                                   \
    do {                                                                \
        snEntry *p;                                                     \
        snEntry *__end_ptr = &stable_name_table[SNT_size];              \
        for (p = stable_name_table + 1; p < __end_ptr; p++) {           \
            /* Internal pointers are free slots.  */                    \
            /* If p->addr == NULL, it's a */                            \
            /* stable name where the object has been GC'd, but the */   \
            /* StableName object (sn_obj) is still alive. */            \
            if ((p->addr < (P_)stable_name_table ||                     \
                 p->addr >= (P_)__end_ptr))                             \
            {                                                           \
                /* NOTE: There is an ambiguity here if p->addr == NULL */ \
                /* it is either the last item in the free list or it */ \
                /* is a stable name whose pointee died. sn_obj == NULL */ \
                /* disambiguates as last free list item. */             \
                do { CODE } while(0);                                   \
            }                                                           \
        }                                                               \
    } while(0)

STATIC_INLINE void
markStablePtrTable(evac_fn evac, void *user)
{
    FOR_EACH_STABLE_PTR(p, evac(user, (StgClosure **)&p->addr););
}

STATIC_INLINE void
rememberOldStableNameAddresses(void)
{
    /* TODO: Only if !full GC */
    FOR_EACH_STABLE_NAME(p, p->old = p->addr;);
}

void
markStableTables(evac_fn evac, void *user)
{
    markStablePtrTable(evac, user);
    rememberOldStableNameAddresses();
}

/* -----------------------------------------------------------------------------
 * Thread the stable pointer table for compacting GC.
 *
 * Here we must call the supplied evac function for each pointer into
 * the heap from the stable tables, because the compacting
 * collector may move the object it points to.
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
threadStableNameTable( evac_fn evac, void *user )
{
    FOR_EACH_STABLE_NAME(p, {
        if (p->sn_obj != NULL) {
            evac(user, (StgClosure **)&p->sn_obj);
        }
        if (p->addr != NULL) {
            evac(user, (StgClosure **)&p->addr);
        }
    });
}

STATIC_INLINE void
threadStablePtrTable( evac_fn evac, void *user )
{
    FOR_EACH_STABLE_PTR(p, evac(user, (StgClosure **)&p->addr););
}

void
threadStableTables( evac_fn evac, void *user )
{
    threadStableNameTable(evac, user);
    threadStablePtrTable(evac, user);
}

/* -----------------------------------------------------------------------------
 * Garbage collect any dead entries in the stable pointer table.
 *
 * A dead entry has:
 *
 *          - a zero reference count
 *          - a dead sn_obj
 *
 * Both of these conditions must be true in order to re-use the stable
 * name table entry.  We can re-use stable name table entries for live
 * heap objects, as long as the program has no StableName objects that
 * refer to the entry.
 * -------------------------------------------------------------------------- */

void
gcStableTables( void )
{
    FOR_EACH_STABLE_NAME(
        p, {
            // Update the pointer to the StableName object, if there is one
            if (p->sn_obj != NULL) {
                p->sn_obj = isAlive(p->sn_obj);
                if(p->sn_obj == NULL) {
                    // StableName object died
                    debugTrace(DEBUG_stable, "GC'd StableName %ld (addr=%p)",
                               (long)(p - stable_name_table), p->addr);
                    freeSnEntry(p);
                    /* Can't "continue", so use goto */
                    goto next_stable_name;
                }
            }
            /* If sn_obj became NULL, the object died, and addr is now
             * invalid. But if sn_obj was null, then the StableName
             * object may not have been created yet, while the pointee
             * already exists and must be updated to new location. */
            if (p->addr != NULL) {
                p->addr = (StgPtr)isAlive((StgClosure *)p->addr);
                if(p->addr == NULL) {
                    // StableName pointee died
                    debugTrace(DEBUG_stable, "GC'd pointee %ld",
                               (long)(p - stable_name_table));
                }
            }
    next_stable_name:
            if (0) {}
        });
}

/* -----------------------------------------------------------------------------
 * Update the StableName hash table
 *
 * The boolean argument 'full' indicates that a major collection is
 * being done, so we might as well throw away the hash table and build
 * a new one.  For a minor collection, we just re-hash the elements
 * that changed.
 * -------------------------------------------------------------------------- */

void
updateStableTables(rtsBool full)
{
    if (full && addrToStableHash != NULL && 0 != keyCountHashTable(addrToStableHash)) {
        freeHashTable(addrToStableHash,NULL);
        addrToStableHash = allocHashTable();
    }

    if(full) {
        FOR_EACH_STABLE_NAME(
            p, {
                if (p->addr != NULL) {
                    // Target still alive, Re-hash this stable name
                    insertHashTable(addrToStableHash, (W_)p->addr, (void *)(p - stable_name_table));
                }
            });
    } else {
        FOR_EACH_STABLE_NAME(
            p, {
                if (p->addr != p->old) {
                    removeHashTable(addrToStableHash, (W_)p->old, NULL);
                    /* Movement happened: */
                    if (p->addr != NULL) {
                        insertHashTable(addrToStableHash, (W_)p->addr, (void *)(p - stable_name_table));
                    }
                }
            });
    }
}
