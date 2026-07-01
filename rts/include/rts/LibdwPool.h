/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2015-2016
 *
 * A pool of libdw sessions
 *
 * --------------------------------------------------------------------------*/

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

/* Claim a session from the pool */
RTS_EXPORT LibdwSession *libdwPoolTake(void);

/* Return a session to the pool */
RTS_EXPORT void libdwPoolRelease(LibdwSession *sess);

/* Free any sessions in the pool forcing a reload of any loaded debug
 * information */
RTS_EXPORT void libdwPoolClear(void);
