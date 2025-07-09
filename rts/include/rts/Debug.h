/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2025
 *
 * Debug API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

typedef struct { void *addr; const char *sym; } DebugSymbolEntry;

void registerDebugSymbol( const DebugSymbolEntry entries[], int len );
