/*
 * (c)2014 Tweag I/O
 *
 * The Static Pointer Table implementation.
 *
 * https://ghc.haskell.org/trac/ghc/wiki/StaticPointers
 * https://ghc.haskell.org/trac/ghc/wiki/StaticPointers/ImplementationPlan
 *
 */

#include "StaticPtrTable.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "Stable.h"

static HashTable * spt = NULL;

#if defined(THREADED_RTS)
static Mutex spt_lock;
#endif

/// Hash function for the SPT.
static int hashFingerprint(const HashTable *table, StgWord key) {
  const StgWord64* ptr = (StgWord64*) key;
  // Take half of the key to compute the hash.
  return hashWord(table, *(ptr + 1));
}

/// Comparison function for the SPT.
static int compareFingerprint(StgWord a, StgWord b) {
  const StgWord64* ptra = (StgWord64*) a;
  const StgWord64* ptrb = (StgWord64*) b;
  return *ptra == *ptrb && *(ptra + 1) == *(ptrb + 1);
}

void hs_spt_insert_stableptr(StgWord64 key[2], StgStablePtr *entry) {
  // hs_spt_insert is called from constructor functions, so
  // the SPT needs to be initialized here.
  if (spt == NULL) {
    spt = allocHashTable_(hashFingerprint, compareFingerprint);
#if defined(THREADED_RTS)
    initMutex(&spt_lock);
#endif
  }

  ACQUIRE_LOCK(&spt_lock);
  insertHashTable(spt, (StgWord)key, entry);
  RELEASE_LOCK(&spt_lock);
}

void hs_spt_insert(StgWord64 key[2], void *spe_closure) {
  // Cannot remove this indirection yet because getStablePtr()
  // might return NULL, in which case hs_spt_lookup() returns NULL
  // instead of the actual closure pointer.
  StgStablePtr * entry = stgMallocBytes( sizeof(StgStablePtr)
                                       , "hs_spt_insert: entry"
                                       );
  *entry = getStablePtr(spe_closure);
  hs_spt_insert_stableptr(key, entry);
}

static void freeSptEntry(void* entry) {
  freeStablePtr(*(StgStablePtr*)entry);
  stgFree(entry);
}

void hs_spt_remove(StgWord64 key[2]) {
   if (spt) {
     ACQUIRE_LOCK(&spt_lock);
     StgStablePtr* entry = removeHashTable(spt, (StgWord)key, NULL);
     RELEASE_LOCK(&spt_lock);

     if (entry)
       freeSptEntry(entry);
   }
}

StgPtr hs_spt_lookup(StgWord64 key[2]) {
  if (spt) {
    ACQUIRE_LOCK(&spt_lock);
    const StgStablePtr * entry = lookupHashTable(spt, (StgWord)key);
    const StgPtr ret = entry ? deRefStablePtr(*entry) : NULL;
    RELEASE_LOCK(&spt_lock);
    return ret;
  } else
    return NULL;
}

int hs_spt_keys(StgPtr keys[], int szKeys) {
  if (spt) {
    ACQUIRE_LOCK(&spt_lock);
    const int ret = keysHashTable(spt, (StgWord*)keys, szKeys);
    RELEASE_LOCK(&spt_lock);
    return ret;
  } else
    return 0;
}

int hs_spt_key_count() {
  return spt ? keyCountHashTable(spt) : 0;
}

void exitStaticPtrTable() {
  if (spt) {
    freeHashTable(spt, freeSptEntry);
    spt = NULL;
#if defined(THREADED_RTS)
    closeMutex(&spt_lock);
#endif
  }
}
