/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2015-2016
 *
 * A pool of libdw sessions
 *
 * --------------------------------------------------------------------------*/

#pragma once

/* Claim a session from the pool */
RTS_PUBLIC LibdwSession *libdwPoolTake(void);

/* Return a session to the pool */
RTS_PUBLIC void libdwPoolRelease(LibdwSession *sess);

/* Free any sessions in the pool forcing a reload of any loaded debug
 * information */
RTS_PUBLIC void libdwPoolClear(void);
