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

#include <stddef.h>

/* Alternate to raise(3) for threaded rts, for BSD-based OSes */
int genericRaise(int sig);

/* Fast memcmp with memcmp(3) semantics, implemented by the RTS
 * (rts/HsMemcmp.c). Unlike ISO C memcmp, hs_memcmp(s1, s2, 0) returns 0
 * without dereferencing either pointer. */
int hs_memcmp(const void *s1, const void *s2, size_t n);
