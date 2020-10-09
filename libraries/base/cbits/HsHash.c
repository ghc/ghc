/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2020
 *
 * Definitions for package `base' which are visible in Haskell land.
 * Defines the streaming interface to the high speed xxHash function
 *
 * ---------------------------------------------------------------------------*/

#pragma once

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