/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS time
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_GETTIME_H
#define RTS_GETTIME_H

StgWord64 getMonotonicNSec (void);

#endif /* RTS_GETTIME_H */
