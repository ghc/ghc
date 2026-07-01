/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS external APIs.  This file declares everything that the GHC RTS
 * exposes externally.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

/* Alternate to raise(3) for threaded rts, for BSD-based OSes */
RTS_EXPORT int genericRaise(int sig);
