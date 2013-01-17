/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS external APIs.  This file declares everything that the GHC RTS
 * exposes externally.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_UTILS_H
#define RTS_UTILS_H

// Used in GHC (basicTypes/Unique.lhs, and Data.Unique in the base
// package.
HsInt genSymZh(void);
HsInt resetGenSymZh(void);

/* Alternate to raise(3) for threaded rts, for BSD-based OSes */
int genericRaise(int sig);

#endif /* RTS_UTILS_H */
