/*
 * Copyright (C) 2012 Vincent Hanquez <vincent@snarc.org>
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
#ifndef CRYPTOHASH_SHA3_H
#define CRYPTOHASH_SHA3_H

#include <stdint.h>

struct sha3_ctx
{
	uint32_t bufindex;
	uint32_t bufsz;
	uint64_t state[25];
	uint8_t  buf[0]; /* maximum SHAKE128 is 168 bytes, otherwise buffer can be decreased */
};

#define SHA3_CTX_SIZE		sizeof(struct sha3_ctx)
#define SHA3_CTX_BUF_MAX_SIZE   (SHA3_CTX_SIZE + SHA3_BUF_SIZE_MAX)
#define SHA3_BITSIZE_MIN   	128
#define SHA3_BITSIZE_MAX    	512

#define SHA3_BUF_SIZE(bitsize)  (200 - 2 * ((bitsize) / 8))

#define SHA3_BUF_SIZE_MIN       SHA3_BUF_SIZE(SHA3_BITSIZE_MAX)
#define SHA3_BUF_SIZE_MAX       SHA3_BUF_SIZE(SHA3_BITSIZE_MIN)

/*
 * buffer size:
 *
 * 128 bits (shake 128 bits) => 200 - 2 * (128 / 8) = 200 - 2*16 = 200 - 32  = 168 bytes
 * 224 bits (SHA3 224 bits)  => 200 - 2 * (224 / 8) = 200 - 2*28 = 200 - 56  = 144 bytes
 * 512 bits (SHA3 512 bits)  => 200 - 2 * (512 / 8) = 200 - 2*64 = 200 - 128 = 72 bytes
 */

void cryptonite_sha3_init(struct sha3_ctx *ctx, uint32_t hashlen);
void cryptonite_sha3_update(struct sha3_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_sha3_finalize(struct sha3_ctx *ctx, uint32_t hashlen, uint8_t *out);

void cryptonite_sha3_finalize_shake(struct sha3_ctx *ctx);
void cryptonite_sha3_finalize_cshake(struct sha3_ctx *ctx);
void cryptonite_sha3_output(struct sha3_ctx *ctx, uint8_t *out, uint32_t len);

void cryptonite_keccak_init(struct sha3_ctx *ctx, uint32_t hashlen);
void cryptonite_keccak_update(struct sha3_ctx *ctx, uint8_t *data, uint32_t len);
void cryptonite_keccak_finalize(struct sha3_ctx *ctx, uint32_t hashlen, uint8_t *out);

#endif
