/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2015-2016
 *
 * A pool of libdw sessions
 *
 * --------------------------------------------------------------------------*/

#pragma once

/* Claim a session from the pool */
LibdwSession *libdwPoolTake(void);

/* Return a session to the pool */
void libdwPoolRelease(LibdwSession *sess);

/* Free any sessions in the pool forcing a reload of any loaded debug
 * information */
void libdwPoolClear(void);
