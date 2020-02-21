/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 *
 * Root profiling API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#if defined(PROFILING)
StgInt setRootProfPtrs(StgInt n, HsStablePtr *sps, const char** descs);
#endif
