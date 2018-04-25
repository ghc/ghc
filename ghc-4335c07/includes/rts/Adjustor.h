/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Adjustor API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* Creating and destroying an adjustor thunk */
void* createAdjustor (int cconv, 
                      StgStablePtr hptr,
                      StgFunPtr wptr,
                      char *typeString);

void freeHaskellFunctionPtr (void* ptr);
