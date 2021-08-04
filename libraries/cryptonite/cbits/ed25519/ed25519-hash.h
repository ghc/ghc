#include <cryptonite_sha512.h>
typedef struct sha512_ctx ed25519_hash_context;

static void
ed25519_hash_init(ed25519_hash_context *ctx) {
	cryptonite_sha512_init(ctx);
}

static void
ed25519_hash_update(ed25519_hash_context *ctx, const uint8_t *in, size_t inlen) {
	cryptonite_sha512_update(ctx, in, inlen);
}

static void
ed25519_hash_final(ed25519_hash_context *ctx, uint8_t *hash) {
	cryptonite_sha512_finalize(ctx, hash);
}

static void
ed25519_hash(uint8_t *hash, const uint8_t *in, size_t inlen) {
	ed25519_hash_context ctx;
	cryptonite_sha512_init(&ctx);
	cryptonite_sha512_update(&ctx, in, inlen);
	cryptonite_sha512_finalize(&ctx, hash);
	memset(&ctx, 0, sizeof(ctx));
}
