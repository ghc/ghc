/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow, 2009
 *
 * Heap Census Profiling
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* -----------------------------------------------------------------------------
 * Fine-grained control over heap census profiling which can be called from
 * Haskell to restrict the profile to portion(s) of the execution.
 * See the module GHC.Profiling.
 * ---------------------------------------------------------------------------*/

void requestHeapCensus ( void );
void startHeapProfTimer ( void );
void stopHeapProfTimer ( void );
