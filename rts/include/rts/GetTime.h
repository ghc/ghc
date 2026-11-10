/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS time
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* The monotonic time source used within the RTS.
 *
 * Note that this uses StgWord64, rather than Time which is int64_t .
 */
StgWord64 getMonotonicNSec(void);

/* The monotonic time source used within the RTS.
 *
 * There is no good reason (except history) that we have both this and
 * getMonotonicNSec.
 */
Time getProcessElapsedTime(void);


/* It might be nice to tidy this up and export monotonic and wall clock time
 * and precision and bind them in ghc-internal. They should probably all
 * use int64_t / CInt64.
 *
 * The internal functions are getMonotonicTime() and getWallClockTime().
 * These would be better candidates for export.
 */
