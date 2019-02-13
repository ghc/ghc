/* -*- tab-width: 4 -*- */

/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Stable names
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "Hash.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "StableName.h"

#include <string.h>

snEntry *stable_name_table = NULL;
static snEntry *stable_name_free = NULL;
unsigned int SNT_size = 0;
#define INIT_SNT_SIZE 64

#if defined(THREADED_RTS)
Mutex stable_name_mutex;
#endif

static void enlargeStableNameTable(void);

/*
 * This hash table maps Haskell objects to stable names, so that every
 * call to lookupStableName on a given object will return the same
 * stable name.
 */

static HashTable *addrToStableHash = NULL;

void
stableNameLock(void)
{
    initStableNameTable();
    ACQUIRE_LOCK(&stable_name_mutex);
}

void
stableNameUnlock(void)
{
    RELEASE_LOCK(&stable_name_mutex);
}

/* -----------------------------------------------------------------------------
 * Initialising the table
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
initSnEntryFreeList(snEntry *table, uint32_t n, snEntry *free)
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

void
initStableNameTable(void)
{
    if (SNT_size > 0) return;
    SNT_size = INIT_SNT_SIZE;
    stable_name_table = stgMallocBytes(SNT_size * sizeof(snEntry),
                                       "initStableNameTable");
    /* we don't use index 0 in the stable name table, because that
     * would conflict with the hash table lookup operations which
     * return NULL if an entry isn't found in the hash table.
     */
    initSnEntryFreeList(stable_name_table + 1,INIT_SNT_SIZE-1,NULL);
    addrToStableHash = allocHashTable();

#if defined(THREADED_RTS)
    initMutex(&stable_name_mutex);
#endif
}

/* -----------------------------------------------------------------------------
 * Enlarging the tables
 * -------------------------------------------------------------------------- */

static void
enlargeStableNameTable(void)
{
    uint32_t old_SNT_size = SNT_size;

    // 2nd and subsequent times
    SNT_size *= 2;
    stable_name_table =
        stgReallocBytes(stable_name_table,
                        SNT_size * sizeof(snEntry),
                        "enlargeStableNameTable");

    initSnEntryFreeList(stable_name_table + old_SNT_size, old_SNT_size, NULL);
}


/* -----------------------------------------------------------------------------
 * Freeing entries and tables
 * -------------------------------------------------------------------------- */

void
exitStableNameTable(void)
{
    if (addrToStableHash)
        freeHashTable(addrToStableHash, NULL);
    addrToStableHash = NULL;

    if (stable_name_table)
        stgFree(stable_name_table);
    stable_name_table = NULL;
    SNT_size = 0;

#if defined(THREADED_RTS)
    closeMutex(&stable_name_mutex);
#endif
}

void
freeSnEntry(snEntry *sn)
{
  ASSERT(sn->sn_obj == NULL);
  removeHashTable(addrToStableHash, (W_)sn->old, NULL);
  sn->addr = (P_)stable_name_free;
  stable_name_free = sn;
}

/* -----------------------------------------------------------------------------
 * Looking up
 * -------------------------------------------------------------------------- */

/*
 * get at the real stuff...remove indirections.
 */
static StgClosure*
removeIndirections (StgClosure* p)
{
    StgClosure* q;

    while (1)
    {
        q = UNTAG_CLOSURE(p);

        switch (get_itbl(q)->type) {
        case IND:
        case IND_STATIC:
            p = ((StgInd *)q)->indirectee;
            continue;

        case BLACKHOLE:
            p = ((StgInd *)q)->indirectee;
            if (GET_CLOSURE_TAG(p) != 0) {
                continue;
            } else {
                break;
            }

        default:
            break;
        }
        return p;
    }
}

StgWord
lookupStableName (StgPtr p)
{
  stableNameLock();

  if (stable_name_free == NULL) {
    enlargeStableNameTable();
  }

  /* removing indirections increases the likelihood
   * of finding a match in the stable name hash table.
   */
  p = (StgPtr)removeIndirections((StgClosure*)p);

  // register the untagged pointer.  This just makes things simpler.
  p = (StgPtr)UNTAG_CLOSURE((StgClosure*)p);

  StgWord sn = (StgWord)lookupHashTable(addrToStableHash,(W_)p);

  if (sn != 0) {
    ASSERT(stable_name_table[sn].addr == p);
    debugTrace(DEBUG_stable, "cached stable name %ld at %p",sn,p);
    stableNameUnlock();
    return sn;
  }

  sn = stable_name_free - stable_name_table;
  stable_name_free  = (snEntry*)(stable_name_free->addr);
  stable_name_table[sn].addr = p;
  stable_name_table[sn].sn_obj = NULL;
  /* debugTrace(DEBUG_stable, "new stable name %d at %p\n",sn,p); */

  /* add the new stable name to the hash table */
  insertHashTable(addrToStableHash, (W_)p, (void *)sn);

  stableNameUnlock();

  return sn;
}

/* -----------------------------------------------------------------------------
 * Remember old stable name addresses
 * -------------------------------------------------------------------------- */

void
rememberOldStableNameAddresses(void)
{
    /* TODO: Only if !full GC */
    FOR_EACH_STABLE_NAME(p, p->old = p->addr;);
}

/* -----------------------------------------------------------------------------
 * Thread the stable name table for compacting GC.
 *
 * Here we must call the supplied evac function for each pointer into
 * the heap from the stable name table, because the compacting
 * collector may move the object it points to.
 * -------------------------------------------------------------------------- */

void
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

/* -----------------------------------------------------------------------------
 * Garbage collect any dead entries in the stable name table.
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
gcStableNameTable( void )
{
    // We must take the stable name lock lest we race with the nonmoving
    // collector (namely nonmovingSweepStableNameTable).
    stableNameLock();
    FOR_EACH_STABLE_NAME(
        p, {
            // FOR_EACH_STABLE_NAME traverses free entries too, so
            // check sn_obj
            if (p->sn_obj != NULL) {
                // Update the pointer to the StableName object, if there is one
                p->sn_obj = isAlive(p->sn_obj);
                if (p->sn_obj == NULL) {
                    // StableName object died
                    debugTrace(DEBUG_stable, "GC'd StableName %ld (addr=%p)",
                               (long)(p - stable_name_table), p->addr);
                    freeSnEntry(p);
                } else if (p->addr != NULL) {
                    // sn_obj is alive, update pointee
                    p->addr = (StgPtr)isAlive((StgClosure *)p->addr);
                    if (p->addr == NULL) {
                        // Pointee died
                        debugTrace(DEBUG_stable, "GC'd pointee %ld",
                                   (long)(p - stable_name_table));
                    }
                }
            }
        });
    stableNameUnlock();
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
updateStableNameTable(bool full)
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
