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

#pragma once

StgWord64 getMonotonicNSec (void);
