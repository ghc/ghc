/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef CRYPTONITE_DECAF_SHAKE_H
#define CRYPTONITE_DECAF_SHAKE_H

#include "cryptonite_sha3.h"

#include <decaf/common.h>

#define CHUNK_SIZE_32 0x80000000

typedef struct sha3_shake256_ctx
{
        struct sha3_ctx    sc[1];
        uint8_t            filler[136];    // 200 - 2*(256/8)
}
cryptonite_decaf_shake256_ctx_t[1];

static inline void cryptonite_decaf_shake256_init(cryptonite_decaf_shake256_ctx_t ctx)
{
        cryptonite_sha3_init(ctx -> sc, 256);
}

static inline void cryptonite_decaf_shake256_update(cryptonite_decaf_shake256_ctx_t ctx, const uint8_t *in, size_t inlen)
{
#if __SIZE_MAX__ > UINT32_MAX
        // split data over 4 GB in 2-GB chunks
        while (inlen > UINT32_MAX) {
                cryptonite_sha3_update(ctx -> sc, in, CHUNK_SIZE_32);
                inlen -= CHUNK_SIZE_32;
                in += CHUNK_SIZE_32;
        }
#endif
        cryptonite_sha3_update(ctx -> sc, in, (uint32_t) inlen);
}

static inline void cryptonite_decaf_shake256_output(cryptonite_decaf_shake256_ctx_t ctx, uint8_t *out, size_t outlen) {
#if __SIZE_MAX__ > UINT32_MAX
        // split data over 4 GB in 2-GB chunks
        while (outlen > UINT32_MAX) {
                cryptonite_sha3_output(ctx -> sc, out, CHUNK_SIZE_32);
                outlen -= CHUNK_SIZE_32;
                out += CHUNK_SIZE_32;
        }
#endif
        cryptonite_sha3_output(ctx -> sc, out, (uint32_t) outlen);
}

static inline void cryptonite_decaf_shake256_final(cryptonite_decaf_shake256_ctx_t ctx, uint8_t *out, size_t outlen)
{
        cryptonite_sha3_finalize_shake(ctx -> sc);
        cryptonite_decaf_shake256_output(ctx, out, outlen);

        cryptonite_decaf_shake256_init(ctx);
}

static inline void cryptonite_decaf_shake256_destroy(cryptonite_decaf_shake256_ctx_t ctx)
{
        cryptonite_decaf_bzero(ctx, sizeof(*ctx));
}

static inline void cryptonite_decaf_shake256_hash(uint8_t *out, size_t outlen, const uint8_t *in, size_t inlen)
{
        cryptonite_decaf_shake256_ctx_t ctx;

        cryptonite_decaf_shake256_init(ctx);
        cryptonite_decaf_shake256_update(ctx, in, inlen);

        cryptonite_sha3_finalize_shake(ctx -> sc);
        cryptonite_decaf_shake256_output(ctx, out, outlen);

        cryptonite_decaf_shake256_destroy(ctx);
}

#endif
