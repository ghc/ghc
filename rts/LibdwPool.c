/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * A pool of libdw sessions
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "LibdwPool.h"

#if USE_LIBDW

#include <unistd.h>

#include "Pool.h"

/*
 * Note [libdw session pool]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Building a libdw session requires a number of rather expensive steps,
 *
 *   - Examine the object files mapped into the address space
 *   - Find them on disk
 *   - Open them and map their debugging information
 *   - Build various index structures necessary for quick lookup
 *
 * The time to setup a session can be several milliseconds. In order to avoid
 * incurring this cost too often, we keep a pool of warm sessions around which
 * can be shared between capabilities.
 *
 */

static Pool *pool = NULL;
static uint32_t pool_size = 10; // TODO

void libdwPoolInit(void) {
    pool = poolInit(pool_size, pool_size,
                    (alloc_thing_fn) libdwInit,
                    (free_thing_fn) libdwFree);
}

LibdwSession *libdwPoolTake(void) {
    return poolTryTake(pool);
}

void libdwPoolRelease(LibdwSession *sess) {
    poolRelease(pool, sess);
}

void libdwPoolClear(void) {
    poolFlush(pool);
}

#else /* !USE_LIBDW */

LibdwSession *libdwPoolTake(void) { return NULL; }

void libdwPoolRelease(LibdwSession *sess STG_UNUSED) { }

void libdwPoolClear(void) { }

#endif /* USE_LIBDW */
