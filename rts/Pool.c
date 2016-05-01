/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * A pool of lazily allocated things
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "Pool.h"

/* used to mark an entry as needing to be freed when released */
#define FLAG_SHOULD_FREE (1 << 0)

typedef struct PoolEntry_ {
    struct PoolEntry_ *next;
    void *thing;
    StgWord flags;
} PoolEntry;

struct Pool_ {
    /* the maximum number of allocated resources in the pool */
    uint32_t max_size;
    /* the number of allocated resources to keep in the pool when idle */
    uint32_t desired_size;
    /* how many things are currently allocated? (sum of lengths of available and
     * taken lists) */
    uint32_t current_size;
#ifdef THREADED_RTS
    /* signaled when a thing is released */
    Condition cond;
#endif
    alloc_thing_fn alloc_fn;
    free_thing_fn free_fn;

    PoolEntry *available;
    PoolEntry *taken;
#ifdef THREADED_RTS
    /* protects entire data structure */
    Mutex mutex;
#endif
};

Pool *poolInit(uint32_t max_size, uint32_t desired_size,
               alloc_thing_fn alloc_fn, free_thing_fn free_fn) {
    Pool *pool = stgMallocBytes(sizeof(Pool), "pool_init");
    pool->max_size = max_size == 0 ? (uint32_t) -1 : max_size;
    pool->desired_size = desired_size;
    pool->current_size = 0;
    pool->alloc_fn = alloc_fn;
    pool->free_fn = free_fn;
    pool->available = NULL;
    pool->taken = NULL;
#ifdef THREADED_RTS
    initMutex(&pool->mutex);
    initCondition(&pool->cond);
#endif
    return pool;
}

int poolFree(Pool *pool) {
    if (pool->taken != NULL)
        return 1;

    poolSetMaxSize(pool, 0);
#ifdef THREADED_RTS
    closeCondition(&pool->cond);
    closeMutex(&pool->mutex);
#endif
    free(pool);
    return 0;
}

/* free available entries such that current_size <= size */
static void free_available(Pool *pool, uint32_t size) {
    while (pool->current_size > size && pool->available != NULL) {
        PoolEntry *ent = pool->available;
        pool->free_fn(ent->thing);
        pool->available = ent->next;
        free(ent);
        pool->current_size--;
    }
}

void poolSetDesiredSize(Pool *pool, uint32_t size) {
    ACQUIRE_LOCK(&pool->mutex);
    pool->desired_size = size;
    free_available(pool, size);
    RELEASE_LOCK(&pool->mutex);
}

void poolSetMaxSize(Pool *pool, uint32_t size) {
    ACQUIRE_LOCK(&pool->mutex);
    if (size == 0)
        size = (uint32_t) -1;
    pool->max_size = size;
    if (pool->desired_size > pool->max_size) {
        pool->desired_size = size;
        free_available(pool, size);
    }
    RELEASE_LOCK(&pool->mutex);
}

uint32_t poolGetMaxSize(Pool *pool) {
    return pool->max_size;
}

uint32_t poolGetDesiredSize(Pool *pool) {
    return pool->desired_size;
}

// Try taking a PoolEntry with an item from a pool,
// returning NULL if no items are available.
static PoolEntry *poolTryTake_(Pool *pool) {
    PoolEntry *ent = NULL;
    if (pool->available != NULL) {
        ent = pool->available;
        pool->available = ent->next;
    } else if (pool->current_size < pool->max_size) {
        ent = stgMallocBytes(sizeof(PoolEntry), "pool_take");
        ent->flags = 0;
        ent->thing = pool->alloc_fn();
        pool->current_size++;
    } else {
        return NULL;
    }

    ent->next = pool->taken;
    pool->taken = ent;
    return ent;
}

void *poolTryTake(Pool *pool) {
    ACQUIRE_LOCK(&pool->mutex);
    PoolEntry *ent = poolTryTake_(pool);
    RELEASE_LOCK(&pool->mutex);
    return ent ? ent->thing : NULL;
}

void *poolTake(Pool *pool) {
    PoolEntry *ent = NULL;
    ACQUIRE_LOCK(&pool->mutex);
    while (ent == NULL) {
        ent = poolTryTake_(pool);
        if (!ent) {
#ifdef THREADED_RTS
            waitCondition(&pool->cond, &pool->mutex);
#else
            barf("Tried to take from an empty pool");
#endif
        }
    }

    RELEASE_LOCK(&pool->mutex);
    return ent->thing;
}

void poolRelease(Pool *pool, void *thing) {
    ACQUIRE_LOCK(&pool->mutex);
    PoolEntry **last = &pool->taken;
    PoolEntry *ent = pool->taken;
    while (ent != NULL) {
        if (ent->thing == thing) {
            *last = ent->next;
            if (pool->current_size > pool->desired_size
                || ent->flags & FLAG_SHOULD_FREE) {
                pool->free_fn(ent->thing);
                free(ent);
            } else {
                ent->next = pool->available;
                pool->available = ent;
#ifdef THREADED_RTS
                signalCondition(&pool->cond);
#endif
            }

            RELEASE_LOCK(&pool->mutex);
            return;
        }

        last = &ent->next;
        ent = ent->next;
    }

    barf("pool_release: trying to release resource which doesn't belong to pool.");
}

void poolFlush(Pool *pool) {
    ACQUIRE_LOCK(&pool->mutex);
    free_available(pool, 0);
    PoolEntry *ent = pool->taken;
    while (ent != NULL) {
        ent->flags |= FLAG_SHOULD_FREE;
        ent = ent->next;
    }
    RELEASE_LOCK(&pool->mutex);
}
