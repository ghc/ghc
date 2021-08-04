#ifndef CRYPTOHASH_BLAKE2BP_H
#define CRYPTOHASH_BLAKE2BP_H

#include "blake2.h"

typedef blake2bp_state blake2bp_ctx;

void cryptonite_blake2bp_init(blake2bp_ctx *ctx, uint32_t hashlen);
void cryptonite_blake2bp_update(blake2bp_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_blake2bp_finalize(blake2bp_ctx *ctx, uint32_t hashlen, uint8_t *out);

#endif
