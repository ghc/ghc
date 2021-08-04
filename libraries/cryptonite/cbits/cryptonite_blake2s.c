#include "cryptonite_blake2s.h"

void cryptonite_blake2s_init(blake2s_ctx *ctx, uint32_t hashlen)
{
	blake2s_init(ctx, hashlen / 8);
}

void cryptonite_blake2s_update(blake2s_ctx *ctx, const uint8_t *data, uint32_t len)
{
	blake2s_update(ctx, data, len);
}

void cryptonite_blake2s_finalize(blake2s_ctx *ctx, uint32_t hashlen, uint8_t *out)
{
	blake2s_final(ctx, out, hashlen / 8);
}
