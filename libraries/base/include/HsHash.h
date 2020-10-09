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
#include "xxhash.h"

XXH_errorcode
__hsbase_hash_init (XXH3_state_t* statePtr);

XXH_errorcode
__hsbase_hash_update (XXH3_state_t* state, const void* input, size_t len);

void
__hsbase_hash_final (const XXH3_state_t* state, XXH128_hash_t *res);
