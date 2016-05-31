/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2015-2016
 *
 * A pool of libdw sessions
 *
 * --------------------------------------------------------------------------*/

#ifndef LIBDW_POOL_H
#define LIBDW_POOL_H

#include "BeginPrivate.h"

#include "Rts.h"
#include "Libdw.h"

#if USE_LIBDW

/* Initialize the pool */
void libdwPoolInit(void);

#else

INLINE_HEADER void libdwPoolInit(void) {}

#endif /* USE_LIBDW */

#include "EndPrivate.h"

#endif /* LIBDW_POOL_H */
