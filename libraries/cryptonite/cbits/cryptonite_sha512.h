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
#ifndef CRYPTOHASH_SHA512_H
#define CRYPTOHASH_SHA512_H

#include <stdint.h>

# define SHA512_BLOCK_SIZE 128

struct sha512_ctx
{
	uint64_t sz[2];
	uint8_t  buf[SHA512_BLOCK_SIZE];
	uint64_t h[8];
};

#define sha384_ctx sha512_ctx

#define SHA384_DIGEST_SIZE	48
#define SHA384_CTX_SIZE		sizeof(struct sha384_ctx)

#define SHA512_DIGEST_SIZE	64
#define SHA512_CTX_SIZE		sizeof(struct sha512_ctx)

void cryptonite_sha384_init(struct sha384_ctx *ctx);
void cryptonite_sha384_update(struct sha384_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_sha384_finalize(struct sha384_ctx *ctx, uint8_t *out);
void cryptonite_sha384_finalize_prefix(struct sha384_ctx *ctx, const uint8_t *data, uint32_t len, uint32_t n, uint8_t *out);

void cryptonite_sha512_init(struct sha512_ctx *ctx);
void cryptonite_sha512_update(struct sha512_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_sha512_finalize(struct sha512_ctx *ctx, uint8_t *out);
void cryptonite_sha512_finalize_prefix(struct sha512_ctx *ctx, const uint8_t *data, uint32_t len, uint32_t n, uint8_t *out);

/* only multiples of 8 are supported as valid t values */
void cryptonite_sha512t_init(struct sha512_ctx *ctx, uint32_t hashlen);
void cryptonite_sha512t_update(struct sha512_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_sha512t_finalize(struct sha512_ctx *ctx, uint32_t hashlen, uint8_t *out);

#endif
