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
#include "skein512.h"
#include "bitfn.h"

static const uint8_t K512_0[4] = { 46, 36, 19, 37, };
static const uint8_t K512_1[4] = { 33, 27, 14, 42, };
static const uint8_t K512_2[4] = { 17, 49, 36, 39, };
static const uint8_t K512_3[4] = { 44,  9, 54, 56, };
static const uint8_t K512_4[4] = { 39, 30, 34, 24, };
static const uint8_t K512_5[4] = { 13, 50, 10, 17, };
static const uint8_t K512_6[4] = { 25, 29, 39, 43, };
static const uint8_t K512_7[4] = {  8, 35, 56, 22, };

static inline void skein512_do_chunk(struct skein512_ctx *ctx, uint64_t *buf, uint32_t len)
{
	uint64_t x[8];
	uint64_t ts[3];
	uint64_t ks[8+1];

	ks[8] = 0x1bd11bdaa9fc1a22ULL;
	ks[0] = ctx->h[0]; ks[8] ^= ctx->h[0];
	ks[1] = ctx->h[1]; ks[8] ^= ctx->h[1];
	ks[2] = ctx->h[2]; ks[8] ^= ctx->h[2];
	ks[3] = ctx->h[3]; ks[8] ^= ctx->h[3];
	ks[4] = ctx->h[4]; ks[8] ^= ctx->h[4];
	ks[5] = ctx->h[5]; ks[8] ^= ctx->h[5];
	ks[6] = ctx->h[6]; ks[8] ^= ctx->h[6];
	ks[7] = ctx->h[7]; ks[8] ^= ctx->h[7];

	ts[0] = ctx->t0;
	ts[1] = ctx->t1;

	ts[0] += len;

	ts[2] = ts[0] ^ ts[1];

#define INJECTKEY(r) \
	x[0] += ks[((r)+0) % (8+1)];                   \
	x[1] += ks[((r)+1) % (8+1)];                   \
	x[2] += ks[((r)+2) % (8+1)];                   \
	x[3] += ks[((r)+3) % (8+1)];                   \
	x[4] += ks[((r)+4) % (8+1)];                   \
	x[5] += ks[((r)+5) % (8+1)] + ts[((r)+0) % 3]; \
	x[6] += ks[((r)+6) % (8+1)] + ts[((r)+1) % 3]; \
	x[7] += ks[((r)+7) % (8+1)] + (r)

#define ROUND(a,b,c,d,e,f,g,h,k) \
	x[a] += x[b]; x[b] = rol64(x[b],k[0]); x[b] ^= x[a]; \
	x[c] += x[d]; x[d] = rol64(x[d],k[1]); x[d] ^= x[c]; \
	x[e] += x[f]; x[f] = rol64(x[f],k[2]); x[f] ^= x[e]; \
	x[g] += x[h]; x[h] = rol64(x[h],k[3]); x[h] ^= x[g];

#define PASS(i) \
	ROUND(0,1,2,3,4,5,6,7,K512_0); \
	ROUND(2,1,4,7,6,5,0,3,K512_1); \
	ROUND(4,1,6,3,0,5,2,7,K512_2); \
	ROUND(6,1,0,7,2,5,4,3,K512_3); \
	INJECTKEY((i*2) + 1);          \
	ROUND(0,1,2,3,4,5,6,7,K512_4); \
	ROUND(2,1,4,7,6,5,0,3,K512_5); \
	ROUND(4,1,6,3,0,5,2,7,K512_6); \
	ROUND(6,1,0,7,2,5,4,3,K512_7); \
	INJECTKEY((i*2) + 2)

	x[0] = le64_to_cpu(buf[0]) + ks[0];
	x[1] = le64_to_cpu(buf[1]) + ks[1];
	x[2] = le64_to_cpu(buf[2]) + ks[2];
	x[3] = le64_to_cpu(buf[3]) + ks[3];
	x[4] = le64_to_cpu(buf[4]) + ks[4];
	x[5] = le64_to_cpu(buf[5]) + ks[5] + ts[0];
	x[6] = le64_to_cpu(buf[6]) + ks[6] + ts[1];
	x[7] = le64_to_cpu(buf[7]) + ks[7];

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
        ctx->h[4] = x[4] ^ cpu_to_le64(buf[4]);
        ctx->h[5] = x[5] ^ cpu_to_le64(buf[5]);
        ctx->h[6] = x[6] ^ cpu_to_le64(buf[6]);
        ctx->h[7] = x[7] ^ cpu_to_le64(buf[7]);
}

void cryptohash_skein512_init(struct skein512_ctx *ctx, uint32_t hashlen)
{
	uint64_t buf[8];
	memset(ctx, 0, sizeof(*ctx));

	ctx->hashlen = (hashlen + 7) >> 3;
	SET_TYPE(ctx, FLAG_FIRST | FLAG_FINAL | FLAG_TYPE(TYPE_CFG));
	
	memset(buf, '\0', sizeof(buf));
	buf[0] = cpu_to_le64((SKEIN_VERSION << 32) | SKEIN_IDSTRING);
	buf[1] = cpu_to_le64(hashlen);
	buf[2] = 0; /* tree info, not implemented */
	skein512_do_chunk(ctx, buf, 4*8);

	SET_TYPE(ctx, FLAG_FIRST | FLAG_TYPE(TYPE_MSG));
}

void cryptohash_skein512_update(struct skein512_ctx *ctx, uint8_t *data, uint32_t len)
{
	uint32_t to_fill;

	if (!len)
		return;

	to_fill = 64 - ctx->bufindex;

	if (ctx->bufindex == 64) {
		skein512_do_chunk(ctx, (uint64_t *) ctx->buf, 64);
		ctx->bufindex = 0;
	}

	/* process partial buffer if there's enough data to make a block
	 * and there's without doubt further blocks */
	if (ctx->bufindex && len > to_fill) {
		memcpy(ctx->buf + ctx->bufindex, data, to_fill);
		skein512_do_chunk(ctx, (uint64_t *) ctx->buf, 64);
		len -= to_fill;
		data += to_fill;
		ctx->bufindex = 0;
	}

	/* process as much 64-block as possible except the last one in case we finalize */
	for (; len > 64; len -= 64, data += 64)
		skein512_do_chunk(ctx, (uint64_t *) data, 64);

	/* append data into buf */
	if (len) {
		memcpy(ctx->buf + ctx->bufindex, data, len);
		ctx->bufindex += len;
	}
}

void cryptohash_skein512_finalize(struct skein512_ctx *ctx, uint8_t *out)
{
	uint32_t outsize;
	uint64_t *p = (uint64_t *) out;
	uint64_t x[8];
	int i, j, n;

	ctx->t1 |= FLAG_FINAL;
	/* if buf is not complete pad with 0 bytes */
	if (ctx->bufindex < 64)
		memset(ctx->buf + ctx->bufindex, '\0', 64 - ctx->bufindex);
	skein512_do_chunk(ctx, (uint64_t *) ctx->buf, ctx->bufindex);

	memset(ctx->buf, '\0', 64);

	/* make sure we have a 8 bit rounded value */
	outsize = ctx->hashlen;

	/* backup h[0--7] */
	for (j = 0; j < 8; j++)
		x[j] = ctx->h[j];
	/* threefish in counter mode, 0 for 1st 64 bytes, 1 for 2nd 64 bytes, .. */
	for (i = 0; i*64 < outsize; i++) {
		uint64_t w[8];
		*((uint64_t *) ctx->buf) = cpu_to_le64(i);
		SET_TYPE(ctx, FLAG_FIRST | FLAG_FINAL | FLAG_TYPE(TYPE_OUT));
		skein512_do_chunk(ctx, (uint64_t *) ctx->buf, sizeof(uint64_t));

		n = outsize - i * 64;
		if (n >= 64) n = 64;

		cpu_to_le64_array(w, ctx->h, 8);
		memcpy(out + i*64, w, n);

		/* restore h[0--7] */
		for (j = 0; j < 8; j++)
			ctx->h[j] = x[j];
	}
}
