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

// Forward declaration for Stg.h
struct StgClosure_;
struct StgThunk_;
struct Capability_;

/* This is called by the code generator */
extern DLL_IMPORT_RTS
void updateRemembSetPushClosure_(StgRegTable *reg, struct StgClosure_ *p);

extern DLL_IMPORT_RTS
void updateRemembSetPushThunk_(StgRegTable *reg, struct StgThunk_ *p);

// Forward declaration for unregisterised backend.
EF_(stg_copyArray_barrier);

// Note that RTS code should not condition on this directly by rather
// use the IF_NONMOVING_WRITE_BARRIER_ENABLED macro to ensure that
// the barrier is eliminated in the non-threaded RTS.
extern StgWord DLL_IMPORT_DATA_VAR(nonmoving_write_barrier_enabled);

// A similar macro is defined in includes/Cmm.h for C-- code.
#if defined(THREADED_RTS)
#define IF_NONMOVING_WRITE_BARRIER_ENABLED \
    if (RTS_UNLIKELY(nonmoving_write_barrier_enabled))
#else
#define IF_NONMOVING_WRITE_BARRIER_ENABLED \
    if (0)
#endif
