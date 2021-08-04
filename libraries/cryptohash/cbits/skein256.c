/*
 * Copyright (C) 2006-2010 Vincent Hanquez <vincent@snarc.org>
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

#include <string.h>
#include "skein.h"
#include "skein256.h"
#include "bitfn.h"

static const uint8_t K256_0[2] = { 14, 16, };
static const uint8_t K256_1[2] = { 52, 57, };
static const uint8_t K256_2[2] = { 23, 40, };
static const uint8_t K256_3[2] = {  5, 37, };
static const uint8_t K256_4[2] = { 25, 33, };
static const uint8_t K256_5[2] = { 46, 12, };
static const uint8_t K256_6[2] = { 58, 22, };
static const uint8_t K256_7[2] = { 32, 32, };

static inline void skein256_do_chunk(struct skein256_ctx *ctx, uint64_t *buf, uint32_t len)
{
	uint64_t x[4];
	uint64_t ts[3];
	uint64_t ks[4+1];

	ks[4] = 0x1bd11bdaa9fc1a22ULL;
	ks[0] = ctx->h[0]; ks[4] ^= ctx->h[0];
	ks[1] = ctx->h[1]; ks[4] ^= ctx->h[1];
	ks[2] = ctx->h[2]; ks[4] ^= ctx->h[2];
	ks[3] = ctx->h[3]; ks[4] ^= ctx->h[3];

	ts[0] = ctx->t0;
	ts[1] = ctx->t1;

	ts[0] += len;

	ts[2] = ts[0] ^ ts[1];

#define INJECTKEY(r) \
	x[0] += ks[((r)+0) % (4+1)];                   \
	x[1] += ks[((r)+1) % (4+1)] + ts[((r)+0) % 3]; \
	x[2] += ks[((r)+2) % (4+1)] + ts[((r)+1) % 3]; \
	x[3] += ks[((r)+3) % (4+1)] + (r)

#define ROUND(a,b,c,d,k) \
	x[a] += x[b]; x[b] = rol64(x[b],k[0]); x[b] ^= x[a]; \
	x[c] += x[d]; x[d] = rol64(x[d],k[1]); x[d] ^= x[c];

#define PASS(i) \
	ROUND(0,1,2,3,K256_0); \
	ROUND(0,3,2,1,K256_1); \
	ROUND(0,1,2,3,K256_2); \
	ROUND(0,3,2,1,K256_3); \
	INJECTKEY((i*2) + 1);          \
	ROUND(0,1,2,3,K256_4); \
	ROUND(0,3,2,1,K256_5); \
	ROUND(0,1,2,3,K256_6); \
	ROUND(0,3,2,1,K256_7); \
	INJECTKEY((i*2) + 2)

	x[0] = le64_to_cpu(buf[0]) + ks[0];
	x[1] = le64_to_cpu(buf[1]) + ks[1] + ts[0];
	x[2] = le64_to_cpu(buf[2]) + ks[2] + ts[1];
	x[3] = le64_to_cpu(buf[3]) + ks[3];

	/* 9 pass of 8 rounds = 72 rounds */
	PASS(0);
	PASS(1);
	PASS(2);
	PASS(3);
	PASS(4);
	PASS(5);
	PASS(6);
	PASS(7);
	PASS(8);

	ts[1] &= ~FLAG_FIRST;
	ctx->t0 = ts[0];
	ctx->t1 = ts[1];

	ctx->h[0] = x[0] ^ cpu_to_le64(buf[0]);
        ctx->h[1] = x[1] ^ cpu_to_le64(buf[1]);
        ctx->h[2] = x[2] ^ cpu_to_le64(buf[2]);
        ctx->h[3] = x[3] ^ cpu_to_le64(buf[3]);
}

void cryptohash_skein256_init(struct skein256_ctx *ctx, uint32_t hashlen)
{
	uint64_t buf[4];
	memset(ctx, 0, sizeof(*ctx));

	ctx->hashlen = (hashlen + 7) >> 3;
	SET_TYPE(ctx, FLAG_FIRST | FLAG_FINAL | FLAG_TYPE(TYPE_CFG));
	
	memset(buf, '\0', sizeof(buf));
	buf[0] = cpu_to_le64((SKEIN_VERSION << 32) | SKEIN_IDSTRING);
	buf[1] = cpu_to_le64(hashlen);
	buf[2] = 0; /* tree info, not implemented */
	skein256_do_chunk(ctx, buf, 4*8);

	SET_TYPE(ctx, FLAG_FIRST | FLAG_TYPE(TYPE_MSG));
}

void cryptohash_skein256_update(struct skein256_ctx *ctx, uint8_t *data, uint32_t len)
{
	uint32_t to_fill;

	if (!len)
		return;

	to_fill = 32 - ctx->bufindex;

	if (ctx->bufindex == 32) {
		skein256_do_chunk(ctx, (uint64_t *) ctx->buf, 32);
		ctx->bufindex = 0;
	}

	/* process partial buffer if there's enough data to make a block
	 * and there's without doubt further blocks */
	if (ctx->bufindex && len > to_fill) {
		memcpy(ctx->buf + ctx->bufindex, data, to_fill);
		skein256_do_chunk(ctx, (uint64_t *) ctx->buf, 32);
		len -= to_fill;
		data += to_fill;
		ctx->bufindex = 0;
	}

	/* process as much 32-block as possible except the last one in case we finalize */
	for (; len > 32; len -= 32, data += 32)
		skein256_do_chunk(ctx, (uint64_t *) data, 32);

	/* append data into buf */
	if (len) {
		memcpy(ctx->buf + ctx->bufindex, data, len);
		ctx->bufindex += len;
	}
}

void cryptohash_skein256_finalize(struct skein256_ctx *ctx, uint8_t *out)
{
	uint32_t outsize;
	uint64_t *p = (uint64_t *) out;
	uint64_t x[4];
	int i, j, n;

	ctx->t1 |= FLAG_FINAL;
	/* if buf is not complete pad with 0 bytes */
	if (ctx->bufindex < 32)
		memset(ctx->buf + ctx->bufindex, '\0', 32 - ctx->bufindex);
	skein256_do_chunk(ctx, (uint64_t *) ctx->buf, ctx->bufindex);

	memset(ctx->buf, '\0', 32);

	/* make sure we have a 8 bit rounded value */
	outsize = ctx->hashlen;

	/* backup h[0--4] */
	for (j = 0; j < 4; j++)
		x[j] = ctx->h[j];
	/* threefish in counter mode, 0 for 1st 64 bytes, 1 for 2nd 64 bytes, .. */
	for (i = 0; i*32 < outsize; i++) {
		uint64_t w[4];
		*((uint64_t *) ctx->buf) = cpu_to_le64(i);
		SET_TYPE(ctx, FLAG_FIRST | FLAG_FINAL | FLAG_TYPE(TYPE_OUT));
		skein256_do_chunk(ctx, (uint64_t *) ctx->buf, sizeof(uint64_t));

		n = outsize - i * 32;
		if (n >= 32) n = 32;

		cpu_to_le64_array(w, ctx->h, 4);
		memcpy(out + i*32, w, n);

		/* restore h[0--4] */
		for (j = 0; j < 4; j++)
			ctx->h[j] = x[j];
	}
}
