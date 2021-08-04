#ifndef CRYPTOHASH_BLAKE2B_H
#define CRYPTOHASH_BLAKE2B_H

#include "blake2.h"

typedef blake2b_state blake2b_ctx;

void cryptonite_blake2b_init(blake2b_ctx *ctx, uint32_t hashlen);
void cryptonite_blake2b_update(blake2b_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_blake2b_finalize(blake2b_ctx *ctx, uint32_t hashlen, uint8_t *out);

#endif
