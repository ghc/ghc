/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2018-2019
 *
 * Non-moving garbage collector
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* This is called by the code generator */
extern DLL_IMPORT_RTS
void updateRemembSetPushClosure_(StgRegTable *reg, StgClosure *p);

void updateRemembSetPushClosure(Capability *cap, StgClosure *p);

void updateRemembSetPushThunk_(StgRegTable *reg, StgThunk *p);

// Note that RTS code should not condition on this directly by rather
// use the IF_NONMOVING_WRITE_BARRIER_ENABLED macro to ensure that
// the barrier is eliminated in the non-threaded RTS.
extern StgWord DLL_IMPORT_DATA_VAR(nonmoving_write_barrier_enabled);
