/*
 * (c)2014 Tweag I/O
 *
 * The Static Pointer Table implementation.
 *
 * https://gitlab.haskell.org/ghc/ghc/wikis/static-pointers
 * https://gitlab.haskell.org/ghc/ghc/wikis/static-pointers/implementation-plan
 *
 */

#include "Rts.h"
#include "StaticPtrTable.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "StablePtr.h"

static HashTable * spt = NULL;

#if defined(THREADED_RTS)
static Mutex spt_lock;
#endif

/// Hash function for the SPT.
STATIC_INLINE int hashFingerprint(const HashTable *table, StgWord key) {
  const StgWord64* ptr = (StgWord64*) key;
  // Take half of the key to compute the hash.
  return hashAddress(table, *(ptr + 1));
}

/// Comparison function for the SPT.
STATIC_INLINE int compareFingerprint(StgWord a, StgWord b) {
  const StgWord64* ptra = (StgWord64*) a;
  const StgWord64* ptrb = (StgWord64*) b;
  return *ptra == *ptrb && *(ptra + 1) == *(ptrb + 1);
}

void hs_spt_insert_stableptr(StgWord64 key[2], StgStablePtr *entry) {
  // hs_spt_insert is called from constructor functions, so
  // the SPT needs to be initialized here.
  if (spt == NULL) {
    spt = allocHashTable();
#if defined(THREADED_RTS)
    initMutex(&spt_lock);
#endif
  }

  ACQUIRE_LOCK(&spt_lock);
  insertHashTable_(spt, (StgWord)key, entry, hashFingerprint);
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
     StgStablePtr* entry = removeHashTable_(spt, (StgWord)key, NULL,
        hashFingerprint, compareFingerprint);
     RELEASE_LOCK(&spt_lock);

     if (entry)
       freeSptEntry(entry);
   }
}

StgPtr hs_spt_lookup(StgWord64 key[2]) {
  if (spt) {
    ACQUIRE_LOCK(&spt_lock);
    const StgStablePtr * entry = lookupHashTable_(spt, (StgWord)key,
        hashFingerprint, compareFingerprint);
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

int hs_spt_key_count(void) {
  return spt ? keyCountHashTable(spt) : 0;
}

void exitStaticPtrTable(void) {
  if (spt) {
    freeHashTable(spt, freeSptEntry);
    spt = NULL;
#if defined(THREADED_RTS)
    closeMutex(&spt_lock);
#endif
  }
}
