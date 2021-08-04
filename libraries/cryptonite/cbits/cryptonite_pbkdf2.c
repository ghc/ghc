/*
 * fast-pbkdf2 - Optimal PBKDF2-HMAC calculation
 * Written in 2015 by Joseph Birr-Pixton <jpixton@gmail.com>
 * Ported to cryptonite in 2017 by Nicolas Di Prima <nicolas@primetype.co.uk>
 *
 * To the extent possible under law, the author(s) have dedicated all
 * copyright and related and neighboring rights to this software to the
 * public domain worldwide. This software is distributed without any
 * warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication
 * along with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#include <assert.h>
#include <string.h>

#include "cryptonite_pbkdf2.h"
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"
#include "cryptonite_sha1.h"
#include "cryptonite_sha256.h"
#include "cryptonite_sha512.h"

/* --- MSVC doesn't support C99 --- */
#ifdef _MSC_VER
#define restrict
#define _Pragma __pragma
#endif

/* --- Common useful things --- */
#define MIN(a, b) ((a) > (b)) ? (b) : (a)

/* Prepare block (of blocksz bytes) to contain md padding denoting a msg-size
 * message (in bytes).  block has a prefix of used bytes.
 *
 * Message length is expressed in 32 bits (so suitable for sha1, sha256, sha512). */
static inline void md_pad(uint8_t *block, size_t blocksz, size_t used, size_t msg)
{
  memset(block + used, 0, blocksz - used - 4);
  block[used] = 0x80;
  block += blocksz - 4;
  store_be32(block, (uint32_t) (msg * 8));
}

/* Internal function/type names for hash-specific things. */
#define HMAC_CTX(_name) HMAC_ ## _name ## _ctx
#define HMAC_INIT(_name) HMAC_ ## _name ## _init
#define HMAC_UPDATE(_name) HMAC_ ## _name ## _update
#define HMAC_FINAL(_name) HMAC_ ## _name ## _final

#define PBKDF2_F(_name) pbkdf2_f_ ## _name
#define PBKDF2(_name) pbkdf2_ ## _name

/* This macro expands to decls for the whole implementation for a given
 * hash function.  Arguments are:
 *
 * _name like 'sha1', added to symbol names
 * _blocksz block size, in bytes
 * _hashsz digest output, in bytes
 * _ctx hash context type
 * _init hash context initialisation function
 *    args: (_ctx *c)
 * _update hash context update function
 *    args: (_ctx *c, const void *data, size_t ndata)
 * _final hash context finish function
 *    args: (void *out, _ctx *c)
 * _xform hash context raw block update function
 *    args: (_ctx *c, const void *data)
 * _xcpy hash context raw copy function (only need copy hash state)
 *    args: (_ctx * restrict out, const _ctx *restrict in)
 * _xtract hash context state extraction
 *    args: args (_ctx *restrict c, uint8_t *restrict out)
 * _xxor hash context xor function (only need xor hash state)
 *    args: (_ctx *restrict out, const _ctx *restrict in)
 *
 * The resulting function is named PBKDF2(_name).
 */
#define DECL_PBKDF2(_name, _blocksz, _hashsz, _ctx,                           \
                    _init, _update, _xform, _final, _xcpy, _xtract, _xxor)    \
  typedef struct {                                                            \
    _ctx inner;                                                               \
    _ctx outer;                                                               \
  } HMAC_CTX(_name);                                                          \
                                                                              \
  static inline void HMAC_INIT(_name)(HMAC_CTX(_name) *ctx,                   \
                                      const uint8_t *key, size_t nkey)        \
  {                                                                           \
    /* Prepare key: */                                                        \
    uint8_t k[_blocksz];                                                      \
                                                                              \
    /* Shorten long keys. */                                                  \
    if (nkey > _blocksz)                                                      \
    {                                                                         \
      _init(&ctx->inner);                                                     \
      _update(&ctx->inner, key, nkey);                                        \
      _final(&ctx->inner, k);                                                 \
                                                                              \
      key = k;                                                                \
      nkey = _hashsz;                                                         \
    }                                                                         \
                                                                              \
    /* Standard doesn't cover case where blocksz < hashsz. */                 \
    assert(nkey <= _blocksz);                                                 \
                                                                              \
    /* Right zero-pad short keys. */                                          \
    if (k != key)                                                             \
      memcpy(k, key, nkey);                                                   \
    if (_blocksz > nkey)                                                      \
      memset(k + nkey, 0, _blocksz - nkey);                                   \
                                                                              \
    /* Start inner hash computation */                                        \
    uint8_t blk_inner[_blocksz];                                              \
    uint8_t blk_outer[_blocksz];                                              \
                                                                              \
    for (size_t i = 0; i < _blocksz; i++)                                     \
    {                                                                         \
      blk_inner[i] = 0x36 ^ k[i];                                             \
      blk_outer[i] = 0x5c ^ k[i];                                             \
    }                                                                         \
                                                                              \
    _init(&ctx->inner);                                                       \
    _update(&ctx->inner, blk_inner, sizeof blk_inner);                        \
                                                                              \
    /* And outer. */                                                          \
    _init(&ctx->outer);                                                       \
    _update(&ctx->outer, blk_outer, sizeof blk_outer);                        \
  }                                                                           \
                                                                              \
  static inline void HMAC_UPDATE(_name)(HMAC_CTX(_name) *ctx,                 \
                                        const void *data, size_t ndata)       \
  {                                                                           \
    _update(&ctx->inner, data, ndata);                                        \
  }                                                                           \
                                                                              \
  static inline void HMAC_FINAL(_name)(HMAC_CTX(_name) *ctx,                  \
                                       uint8_t out[_hashsz])                  \
  {                                                                           \
    _final(&ctx->inner, out);                                                 \
    _update(&ctx->outer, out, _hashsz);                                       \
    _final(&ctx->outer, out);                                                 \
  }                                                                           \
                                                                              \
                                                                              \
  /* --- PBKDF2 --- */                                                        \
  static inline void PBKDF2_F(_name)(const HMAC_CTX(_name) *startctx,         \
                                     uint32_t counter,                        \
                                     const uint8_t *salt, size_t nsalt,       \
                                     uint32_t iterations,                     \
                                     uint8_t *out)                            \
  {                                                                           \
    uint8_t countbuf[4];                                                      \
    store_be32(countbuf, counter);                                            \
                                                                              \
    /* Prepare loop-invariant padding block. */                               \
    uint8_t Ublock[_blocksz];                                                 \
    md_pad(Ublock, _blocksz, _hashsz, _blocksz + _hashsz);                    \
                                                                              \
    /* First iteration:                                                       \
     *   U_1 = PRF(P, S || INT_32_BE(i))                                      \
     */                                                                       \
    HMAC_CTX(_name) ctx = *startctx;                                          \
    HMAC_UPDATE(_name)(&ctx, salt, nsalt);                                    \
    HMAC_UPDATE(_name)(&ctx, countbuf, sizeof countbuf);                      \
    HMAC_FINAL(_name)(&ctx, Ublock);                                          \
    _ctx result = ctx.outer;                                                  \
                                                                              \
    /* Subsequent iterations:                                                 \
     *   U_c = PRF(P, U_{c-1})                                                \
     */                                                                       \
    for (uint32_t i = 1; i < iterations; i++)                                 \
    {                                                                         \
      /* Complete inner hash with previous U */                               \
      _xcpy(&ctx.inner, &startctx->inner);                                    \
      _xform(&ctx.inner, Ublock);                                             \
      _xtract(&ctx.inner, Ublock);                                            \
      /* Complete outer hash with inner output */                             \
      _xcpy(&ctx.outer, &startctx->outer);                                    \
      _xform(&ctx.outer, Ublock);                                             \
      _xtract(&ctx.outer, Ublock);                                            \
      _xxor(&result, &ctx.outer);                                             \
    }                                                                         \
                                                                              \
    /* Reform result into output buffer. */                                   \
    _xtract(&result, out);                                                    \
  }                                                                           \
                                                                              \
  static inline void PBKDF2(_name)(const uint8_t *pw, size_t npw,             \
                     const uint8_t *salt, size_t nsalt,                       \
                     uint32_t iterations,                                     \
                     uint8_t *out, size_t nout)                               \
  {                                                                           \
    assert(iterations);                                                       \
    assert(out && nout);                                                      \
                                                                              \
    /* Starting point for inner loop. */                                      \
    HMAC_CTX(_name) ctx;                                                      \
    HMAC_INIT(_name)(&ctx, pw, npw);                                          \
                                                                              \
    /* How many blocks do we need? */                                         \
    uint32_t blocks_needed = (uint32_t)(nout + _hashsz - 1) / _hashsz;        \
                                                                              \
    for (uint32_t counter = 1; counter <= blocks_needed; counter++)           \
    {                                                                         \
      uint8_t block[_hashsz];                                                 \
      PBKDF2_F(_name)(&ctx, counter, salt, nsalt, iterations, block);         \
                                                                              \
      size_t offset = (counter - 1) * _hashsz;                                \
      size_t taken = MIN(nout - offset, _hashsz);                             \
      memcpy(out + offset, block, taken);                                     \
    }                                                                         \
  }

static inline void sha1_extract(struct sha1_ctx *restrict ctx, uint8_t *restrict out)
{
	store_be32(out   , ctx->h[0]);
	store_be32(out+4 , ctx->h[1]);
	store_be32(out+8 , ctx->h[2]);
	store_be32(out+12, ctx->h[3]);
	store_be32(out+16, ctx->h[4]);
}

static inline void sha1_cpy(struct sha1_ctx *restrict out, const struct sha1_ctx *restrict in)
{
  out->h[0] = in->h[0];
  out->h[1] = in->h[1];
  out->h[2] = in->h[2];
  out->h[3] = in->h[3];
  out->h[4] = in->h[4];
}

static inline void sha1_xor(struct sha1_ctx *restrict out, const struct sha1_ctx *restrict in)
{
  out->h[0] ^= in->h[0];
  out->h[1] ^= in->h[1];
  out->h[2] ^= in->h[2];
  out->h[3] ^= in->h[3];
  out->h[4] ^= in->h[4];
}

void cryptonite_sha1_transform(struct sha1_ctx* ctx, uint8_t block[SHA1_BLOCK_SIZE])
{
  cryptonite_sha1_update(ctx, block, SHA1_BLOCK_SIZE);
}

DECL_PBKDF2(sha1,
            SHA1_BLOCK_SIZE,
            SHA1_DIGEST_SIZE,
            struct sha1_ctx,
            cryptonite_sha1_init,
            cryptonite_sha1_update,
            cryptonite_sha1_transform,
            cryptonite_sha1_finalize,
            sha1_cpy,
            sha1_extract,
            sha1_xor);

static inline void sha256_extract(struct sha256_ctx *restrict ctx, uint8_t *restrict out)
{
	store_be32(out   , ctx->h[0]);
	store_be32(out+4 , ctx->h[1]);
	store_be32(out+8 , ctx->h[2]);
	store_be32(out+12, ctx->h[3]);
	store_be32(out+16, ctx->h[4]);
	store_be32(out+20, ctx->h[5]);
	store_be32(out+24, ctx->h[6]);
	store_be32(out+28, ctx->h[7]);
}

static inline void sha256_cpy(struct sha256_ctx *restrict out, const struct sha256_ctx *restrict in)
{
	out->h[0] = in->h[0];
	out->h[1] = in->h[1];
	out->h[2] = in->h[2];
	out->h[3] = in->h[3];
	out->h[4] = in->h[4];
	out->h[5] = in->h[5];
	out->h[6] = in->h[6];
	out->h[7] = in->h[7];
}

static inline void sha256_xor(struct sha256_ctx *restrict out, const struct sha256_ctx *restrict in)
{
	out->h[0] ^= in->h[0];
	out->h[1] ^= in->h[1];
	out->h[2] ^= in->h[2];
	out->h[3] ^= in->h[3];
	out->h[4] ^= in->h[4];
	out->h[5] ^= in->h[5];
	out->h[6] ^= in->h[6];
	out->h[7] ^= in->h[7];
}

void cryptonite_sha256_transform(struct sha256_ctx* ctx, uint8_t block[SHA256_BLOCK_SIZE])
{
  cryptonite_sha256_update(ctx, block, SHA256_BLOCK_SIZE);
}

DECL_PBKDF2(sha256,
            SHA256_BLOCK_SIZE,
            SHA256_DIGEST_SIZE,
            struct sha256_ctx,
            cryptonite_sha256_init,
            cryptonite_sha256_update,
            cryptonite_sha256_transform,
            cryptonite_sha256_finalize,
            sha256_cpy,
            sha256_extract,
            sha256_xor);

static inline void sha512_extract(struct sha512_ctx *restrict ctx, uint8_t *restrict out)
{
	store_be64(out   , ctx->h[0]);
	store_be64(out+8 , ctx->h[1]);
	store_be64(out+16, ctx->h[2]);
	store_be64(out+24, ctx->h[3]);
	store_be64(out+32, ctx->h[4]);
	store_be64(out+40, ctx->h[5]);
	store_be64(out+48, ctx->h[6]);
	store_be64(out+56, ctx->h[7]);
}

static inline void sha512_cpy(struct sha512_ctx *restrict out, const struct sha512_ctx *restrict in)
{
	out->h[0] = in->h[0];
	out->h[1] = in->h[1];
	out->h[2] = in->h[2];
	out->h[3] = in->h[3];
	out->h[4] = in->h[4];
	out->h[5] = in->h[5];
	out->h[6] = in->h[6];
	out->h[7] = in->h[7];
}

static inline void sha512_xor(struct sha512_ctx *restrict out, const struct sha512_ctx *restrict in)
{
	out->h[0] ^= in->h[0];
	out->h[1] ^= in->h[1];
	out->h[2] ^= in->h[2];
	out->h[3] ^= in->h[3];
	out->h[4] ^= in->h[4];
	out->h[5] ^= in->h[5];
	out->h[6] ^= in->h[6];
	out->h[7] ^= in->h[7];
}

void cryptonite_sha512_transform(struct sha512_ctx* ctx, uint8_t block[SHA512_BLOCK_SIZE])
{
	cryptonite_sha512_update(ctx, block, SHA512_BLOCK_SIZE);
}

DECL_PBKDF2(sha512,
            SHA512_BLOCK_SIZE,
            SHA512_DIGEST_SIZE,
            struct sha512_ctx,
            cryptonite_sha512_init,
            cryptonite_sha512_update,
            cryptonite_sha512_transform,
            cryptonite_sha512_finalize,
            sha512_cpy,
            sha512_extract,
            sha512_xor);

void cryptonite_fastpbkdf2_hmac_sha1( const uint8_t *pw, size_t npw
                                    , const uint8_t *salt, size_t nsalt
                                    , uint32_t iterations
                                    , uint8_t *out, size_t nout
                                    )
{
  PBKDF2(sha1)(pw, npw, salt, nsalt, iterations, out, nout);
}

void cryptonite_fastpbkdf2_hmac_sha256( const uint8_t *pw, size_t npw
                                      , const uint8_t *salt, size_t nsalt
                                      , uint32_t iterations
                                      , uint8_t *out, size_t nout
                                      )
{
  PBKDF2(sha256)(pw, npw, salt, nsalt, iterations, out, nout);
}

void cryptonite_fastpbkdf2_hmac_sha512( const uint8_t *pw, size_t npw
                                      , const uint8_t *salt, size_t nsalt
                                      , uint32_t iterations
                                      , uint8_t *out, size_t nout
                                      )
{
  PBKDF2(sha512)(pw, npw, salt, nsalt, iterations, out, nout);
}
