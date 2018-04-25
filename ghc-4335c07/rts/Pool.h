#include "Rts.h"

/*
 * Resource pools
 *
 * This module provides an implementation of a simple thread-safe resource pool.
 * A pool is a shared set of resources, the size of which is bounded by a
 * maximum size (0 indicates unbounded). Consumers can request a resource from
 * the pool with pool_take and, when finished can return it to the pool with
 * pool_release. Resources will be lazily allocated with alloc_fn as necessary.
 * If the pool is already at its maximum size when a request is made, pool_take
 * will block until a resource is freed.
 *
 * The pool will free resources such that there are at most desired_size
 * resources in the pool when all resources have been released.
 *
 * invariant: desired_size <= max_size
 *
 */

typedef void *(*alloc_thing_fn)(void);
typedef void (*free_thing_fn)(void *);
typedef struct Pool_ Pool;

/* Create a pool of things. */
Pool *poolInit(uint32_t max_size, uint32_t desired_size,
               alloc_thing_fn alloc_fn, free_thing_fn free_fn);

/* Free a pool. Returns 0 on success or 1 on failure due to things
 * belonging to the pool currently being claimed. */
int poolFree(Pool *pool);

/* Set the maximum size of a pool (0 indicates unbounded). desired_size will be
 * lowered if necessary. */
void poolSetMaxSize(Pool *pool, uint32_t size);

/* Get the maximum size of a pool */
uint32_t poolGetMaxSize(Pool *pool);

/* Set the desired size of a pool */
void poolSetDesiredSize(Pool *pool, uint32_t size);

/* Get the desired size of a pool */
uint32_t poolGetDesiredSize(Pool *pool);

/* Try to grab an available thing from a pool, returning NULL if no things
 * are available.
 */
void *poolTryTake(Pool *pool);

/* Grab an available thing from a pool. This will block if no elements are
 * available in the case of a threaded runtime or abort in a single-threaded
 * environment.
 */
void *poolTake(Pool *pool);

/* Release a thing back to the pool from which it was taken */
void poolRelease(Pool *pool, void *thing);

/* Invalidate all currently allocated resources. Things which are currently
 * taken will be freed upon release instead of being returned to the pool. */
void poolFlush(Pool *pool);
