#ifndef CRYPTOHASH_BLAKE2S_H
#define CRYPTOHASH_BLAKE2S_H

#include "blake2.h"

typedef blake2s_state blake2s_ctx;

void cryptonite_blake2s_init(blake2s_ctx *ctx, uint32_t hashlen);
void cryptonite_blake2s_update(blake2s_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_blake2s_finalize(blake2s_ctx *ctx, uint32_t hashlen, uint8_t *out);

#endif
