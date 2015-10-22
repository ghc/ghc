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

#ifdef USE_LIBDW

#include <unistd.h>

#include "Pool.h"

static Pool *pool = NULL;
static nat pool_size = 10; // TODO

void libdwPoolInit(void) {
    pool = poolInit(pool_size, pool_size,
                    (alloc_thing_fn) libdwInit,
                    (free_thing_fn) libdwFree);
}

LibdwSession *libdwPoolTake(void) {
    return poolTake(pool);
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
