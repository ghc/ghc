/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2020
 *
 * Definitions for package `base' which are visible in Haskell land.
 * Defines the streaming interface to the high speed xxHash function
 *
 * ---------------------------------------------------------------------------*/

/* This file needs to be compiled with vectorization enabled.  Unfortunately
   since we compile these things these days with cabal we can no longer
   specify optimization per file.  So we have to resort to pragmas.  */
#if defined(__GNUC__) || defined(__GNUG__)
#pragma GCC push_options
#pragma GCC optimize ("O3")
#endif

#define XXH_NAMESPACE __hsbase_
#define XXH_STATIC_LINKING_ONLY   /* access advanced declarations */
#define XXH_IMPLEMENTATION   /* access definitions */
#include "HsHash.h"

XXH_errorcode inline
__hsbase_hash_init (XXH3_state_t* statePtr) {
  return XXH3_128bits_reset (statePtr);
}

XXH_errorcode inline
__hsbase_hash_update (XXH3_state_t* state, const void* input, size_t len) {
  return XXH3_128bits_update (state, input, len);
}

void inline
__hsbase_hash_final (const XXH3_state_t* state, XXH128_hash_t *res) {
  *res = XXH3_128bits_digest (state);
}

#if defined(__GNUC__) || defined(__GNUG__)
#pragma GCC pop_options
#endif
