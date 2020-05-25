/*-----------------------------------------------------------------------------
 *
 * (c) The AQUA Project, Glasgow University, 1995-1998
 * (c) The GHC Team, 1999
 *
 * Dynamically expanding linear hash tables, as described in
 * Per-\AAke Larson, ``Dynamic Hash Tables,'' CACM 31(4), April 1988,
 * pp. 446 -- 457.
 * -------------------------------------------------------------------------- */

#include "PosixSource.h"
#include "Rts.h"

#include "Hash.h"
#include "RtsUtils.h"
#include "xxhash.h"

#include <string.h>

#define HSEGSIZE    1024    /* Size of a single hash table segment */
                            /* Also the minimum size of a hash table */
#define HDIRSIZE    1024    /* Size of the segment directory */
                            /* Maximum hash table size is HSEGSIZE * HDIRSIZE */
#define HLOAD       5       /* Maximum average load of a single hash bucket */

#define HCHUNK      (1024 * sizeof(W_) / sizeof(HashList))
                            /* Number of HashList cells to allocate in one go */


/* Linked list of (key, data) pairs for separate chaining */
typedef struct hashlist {
    StgWord key;
    const void *data;
    struct hashlist *next;  /* Next cell in bucket chain (same hash value) */
} HashList;

typedef struct chunklist {
  HashList *chunk;
  struct chunklist *next;
} HashListChunk;

struct hashtable {
    int split;              /* Next bucket to split when expanding */
    int max;                /* Max bucket of smaller table */
    int mask1;              /* Mask for doing the mod of h_1 (smaller table) */
    int mask2;              /* Mask for doing the mod of h_2 (larger table) */
    int kcount;             /* Number of keys */
    int bcount;             /* Number of buckets */
    HashList **dir[HDIRSIZE];   /* Directory of segments */
    HashList *freeList;         /* free list of HashLists */
    HashListChunk *chunks;
};

/* Create an identical structure, but is distinct on a type level,
 * for string hash table. Since it's a direct embedding of
 * a hashtable and not a reference, there shouldn't be
 * any overhead post-compilation.  */
struct strhashtable { struct hashtable table; };

/* -----------------------------------------------------------------------------
 * Hash first using the smaller table.  If the bucket is less than the
 * next bucket to be split, re-hash using the larger table.
 * -------------------------------------------------------------------------- */
int
hashWord(const HashTable *table, StgWord key)
{
    int bucket;

    /* Strip the boring zero bits */
    key >>= sizeof(StgWord);

    /* Mod the size of the hash table (a power of 2) */
    bucket = key & table->mask1;

    if (bucket < table->split) {
        /* Mod the size of the expanded hash table (also a power of 2) */
        bucket = key & table->mask2;
    }
    return bucket;
}

int
hashStr(const HashTable *table, StgWord w)
{
    const char *key = (char*) w;
#if defined(x86_64_HOST_ARCH)
    StgWord h = XXH64 (key, strlen(key), 1048583);
#else
    StgWord h = XXH32 (key, strlen(key), 1048583);
#endif

    /* Mod the size of the hash table (a power of 2) */
    int bucket = h & table->mask1;

    if (bucket < table->split) {
        /* Mod the size of the expanded hash table (also a power of 2) */
        bucket = h & table->mask2;
    }

    return bucket;
}

STATIC_INLINE int
compareWord(StgWord key1, StgWord key2)
{
    return (key1 == key2);
}

STATIC_INLINE int
compareStr(StgWord key1, StgWord key2)
{
    return (strcmp((char *)key1, (char *)key2) == 0);
}


/* -----------------------------------------------------------------------------
 * Allocate a new segment of the dynamically growing hash table.
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
allocSegment(HashTable *table, int segment)
{
    table->dir[segment] = stgMallocBytes(HSEGSIZE * sizeof(HashList *),
                                         "allocSegment");
}


/* -----------------------------------------------------------------------------
 * Expand the larger hash table by one bucket, and split one bucket
 * from the smaller table into two parts.  Only the bucket referenced
 * by @table->split@ is affected by the expansion.
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
expand(HashTable *table, HashFunction f)
{
    int oldsegment;
    int oldindex;
    int newbucket;
    int newsegment;
    int newindex;
    HashList *hl;
    HashList *next;
    HashList *old, *new;

    if (table->split + table->max >= HDIRSIZE * HSEGSIZE)
        /* Wow!  That's big.  Too big, so don't expand. */
        return;

    /* Calculate indices of bucket to split */
    oldsegment = table->split / HSEGSIZE;
    oldindex = table->split % HSEGSIZE;

    newbucket = table->max + table->split;

    /* And the indices of the new bucket */
    newsegment = newbucket / HSEGSIZE;
    newindex = newbucket % HSEGSIZE;

    if (newindex == 0)
        allocSegment(table, newsegment);

    if (++table->split == table->max) {
        table->split = 0;
        table->max *= 2;
        table->mask1 = table->mask2;
        table->mask2 = table->mask2 << 1 | 1;
    }
    table->bcount++;

    /* Split the bucket, paying no attention to the original order */

    old = new = NULL;
    for (hl = table->dir[oldsegment][oldindex]; hl != NULL; hl = next) {
        next = hl->next;
        if (f(table, hl->key) == newbucket) {
            hl->next = new;
            new = hl;
        } else {
            hl->next = old;
            old = hl;
        }
    }
    table->dir[oldsegment][oldindex] = old;
    table->dir[newsegment][newindex] = new;

    return;
}

STATIC_INLINE void*
lookupHashTable_inlined(const HashTable *table, StgWord key,
                        HashFunction f, CompareFunction cmp)
{
    int bucket;
    int segment;
    int index;

    HashList *hl;

    bucket = f(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    for (hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
        if (cmp(hl->key, key))
            return (void *) hl->data;
    }

    /* It's not there */
    return NULL;
}

void *
lookupHashTable_(const HashTable *table, StgWord key,
                 HashFunction f, CompareFunction cmp)
{
    return lookupHashTable_inlined(table, key, f, cmp);
}

void *
lookupHashTable(const HashTable *table, StgWord key)
{
    return lookupHashTable_inlined(table, key, hashWord, compareWord);
}

void *
lookupStrHashTable(const StrHashTable* table, const char* key)
{
    return lookupHashTable_inlined(&table->table, (StgWord) key,
                                   hashStr, compareStr);
}

// Puts up to szKeys keys of the hash table into the given array. Returns the
// actual amount of keys that have been retrieved.
//
// If the table is modified concurrently, the function behavior is undefined.
//
int keysHashTable(HashTable *table, StgWord keys[], int szKeys) {
    int segment, index;
    int k = 0;
    HashList *hl;


    /* The last bucket with something in it is table->max + table->split - 1 */
    segment = (table->max + table->split - 1) / HSEGSIZE;
    index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0 && k < szKeys) {
        while (index >= 0 && k < szKeys) {
            hl = table->dir[segment][index];
            while (hl && k < szKeys) {
                keys[k] = hl->key;
                k += 1;
                hl = hl->next;
            }
            index--;
        }
        segment--;
        index = HSEGSIZE - 1;
    }
    return k;
}

/* -----------------------------------------------------------------------------
 * We allocate the hashlist cells in large chunks to cut down on malloc
 * overhead.  Although we keep a free list of hashlist cells, we make
 * no effort to actually return the space to the malloc arena.
 * -------------------------------------------------------------------------- */

static HashList *
allocHashList (HashTable *table)
{
    HashList *hl, *p;
    HashListChunk *cl;

    if ((hl = table->freeList) != NULL) {
        table->freeList = hl->next;
    } else {
        hl = stgMallocBytes(HCHUNK * sizeof(HashList), "allocHashList");
        cl = stgMallocBytes(sizeof (*cl), "allocHashList: chunkList");
        cl->chunk = hl;
        cl->next = table->chunks;
        table->chunks = cl;

        table->freeList = hl + 1;
        for (p = table->freeList; p < hl + HCHUNK - 1; p++)
            p->next = p + 1;
        p->next = NULL;
    }
    return hl;
}

static void
freeHashList (HashTable *table, HashList *hl)
{
    hl->next = table->freeList;
    table->freeList = hl;
}

STATIC_INLINE void
insertHashTable_inlined(HashTable *table, StgWord key,
                        const void *data, HashFunction f)
{
    int bucket;
    int segment;
    int index;
    HashList *hl;

    // Disable this assert; sometimes it's useful to be able to
    // overwrite entries in the hash table.
    // ASSERT(lookupHashTable(table, key) == NULL);

    /* When the average load gets too high, we expand the table */
    if (++table->kcount >= HLOAD * table->bcount)
        expand(table, f);

    bucket = f(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    hl = allocHashList(table);

    hl->key = key;
    hl->data = data;
    hl->next = table->dir[segment][index];
    table->dir[segment][index] = hl;
}

void
insertHashTable_(HashTable *table, StgWord key,
                 const void *data, HashFunction f)
{
    return insertHashTable_inlined(table, key, data, f);
}

void
insertHashTable(HashTable *table, StgWord key, const void *data)
{
    insertHashTable_inlined(table, key, data, hashWord);
}

void
insertStrHashTable(StrHashTable *table, const char * key, const void *data)
{
    insertHashTable_inlined(&table->table, (StgWord) key, data, hashStr);
}

STATIC_INLINE void*
removeHashTable_inlined(HashTable *table, StgWord key, const void *data,
                        HashFunction f, CompareFunction cmp)
{
    int bucket;
    int segment;
    int index;
    HashList *hl;
    HashList *prev = NULL;

    bucket = f(table, key);
    segment = bucket / HSEGSIZE;
    index = bucket % HSEGSIZE;

    for (hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
        if (cmp(hl->key, key) && (data == NULL || hl->data == data)) {
            if (prev == NULL)
                table->dir[segment][index] = hl->next;
            else
                prev->next = hl->next;
            freeHashList(table,hl);
            table->kcount--;
            return (void *) hl->data;
        }
        prev = hl;
    }

    /* It's not there */
    ASSERT(data == NULL);
    return NULL;
}

void*
removeHashTable_(HashTable *table, StgWord key, const void *data,
                 HashFunction f, CompareFunction cmp)
{
    return removeHashTable_inlined(table, key, data, f, cmp);
}

void *
removeHashTable(HashTable *table, StgWord key, const void *data)
{
    return removeHashTable_inlined(table, key, data, hashWord, compareWord);
}

void *
removeStrHashTable(StrHashTable *table, const char * key, const void *data)
{
    return removeHashTable_inlined(&table->table, (StgWord) key,
                                   data, hashStr, compareStr);
}

/* -----------------------------------------------------------------------------
 * When we free a hash table, we are also good enough to free the
 * data part of each (key, data) pair, as long as our caller can tell
 * us how to do it.
 * -------------------------------------------------------------------------- */

void
freeHashTable(HashTable *table, void (*freeDataFun)(void *) )
{
    long segment;
    long index;
    HashList *hl;
    HashList *next;
    HashListChunk *cl, *cl_next;

    /* The last bucket with something in it is table->max + table->split - 1 */
    segment = (table->max + table->split - 1) / HSEGSIZE;
    index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0) {
        while (index >= 0) {
            for (hl = table->dir[segment][index]; hl != NULL; hl = next) {
                next = hl->next;
                if (freeDataFun != NULL)
                    (*freeDataFun)((void *) hl->data);
            }
            index--;
        }
        stgFree(table->dir[segment]);
        segment--;
        index = HSEGSIZE - 1;
    }
    for (cl = table->chunks; cl != NULL; cl = cl_next) {
        cl_next = cl->next;
        stgFree(cl->chunk);
        stgFree(cl);
    }
    stgFree(table);
}

/* -----------------------------------------------------------------------------
 * Map a function over all the keys/values in a HashTable
 * -------------------------------------------------------------------------- */

void
mapHashTable(HashTable *table, void *data, MapHashFn fn)
{
    /* The last bucket with something in it is table->max + table->split - 1 */
    long segment = (table->max + table->split - 1) / HSEGSIZE;
    long index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0) {
        while (index >= 0) {
            for (HashList *hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
                fn(data, hl->key, hl->data);
            }
            index--;
        }
        segment--;
        index = HSEGSIZE - 1;
    }
}

void
mapHashTableKeys(HashTable *table, void *data, MapHashFnKeys fn)
{
    /* The last bucket with something in it is table->max + table->split - 1 */
    long segment = (table->max + table->split - 1) / HSEGSIZE;
    long index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0) {
        while (index >= 0) {
            for (HashList *hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
                fn(data, &hl->key, hl->data);
            }
            index--;
        }
        segment--;
        index = HSEGSIZE - 1;
    }
}

void
iterHashTable(HashTable *table, void *data, IterHashFn fn)
{
    /* The last bucket with something in it is table->max + table->split - 1 */
    long segment = (table->max + table->split - 1) / HSEGSIZE;
    long index = (table->max + table->split - 1) % HSEGSIZE;

    while (segment >= 0) {
        while (index >= 0) {
            for (HashList *hl = table->dir[segment][index]; hl != NULL; hl = hl->next) {
                if (!fn(data, hl->key, hl->data)) {
                    return;
                }
            }
            index--;
        }
        segment--;
        index = HSEGSIZE - 1;
    }
}

/* -----------------------------------------------------------------------------
 * When we initialize a hash table, we set up the first segment as well,
 * initializing all of the first segment's hash buckets to NULL.
 * -------------------------------------------------------------------------- */

HashTable *
allocHashTable(void)
{
    HashTable *table;
    HashList **hb;

    table = stgMallocBytes(sizeof(HashTable),"allocHashTable");

    allocSegment(table, 0);

    for (hb = table->dir[0]; hb < table->dir[0] + HSEGSIZE; hb++)
        *hb = NULL;

    table->split = 0;
    table->max = HSEGSIZE;
    table->mask1 = HSEGSIZE - 1;
    table->mask2 = 2 * HSEGSIZE - 1;
    table->kcount = 0;
    table->bcount = HSEGSIZE;
    table->freeList = NULL;
    table->chunks = NULL;

    return table;
}

void
exitHashTable(void)
{
    /* nothing to do */
}

int keyCountHashTable (HashTable *table)
{
    return table->kcount;
}
