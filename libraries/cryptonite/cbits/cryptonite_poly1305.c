/*
 * Copyright (c) 2014 Vincent Hanquez <vincent@snarc.org>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the author nor the names of his contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The following code contains some code copied from and inspired by poly1305-donna
 * in poly1305_do_chunk and poly1305_finalize which is licensed under MIT or PUBLIC DOMAIN.
 * see [poly1305-donna](https://github.com/floodyberry/poly1305-donna)
 *
 */

#include <stdint.h>
#include <string.h>
#include "cryptonite_poly1305.h"
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"

static void poly1305_do_chunk(poly1305_ctx *ctx, uint8_t *data, int blocks, int final)
{
	/* following is a cleanup copy of code available poly1305-donna */
	const uint32_t hibit = (final) ? 0 : (1 << 24); /* 1 << 128 */
	uint32_t r0,r1,r2,r3,r4;
	uint32_t s1,s2,s3,s4;
	uint32_t h0,h1,h2,h3,h4;
	uint64_t d0,d1,d2,d3,d4;
	uint32_t c;

	/* load r[i], h[i] */
	h0 = ctx->h[0]; h1 = ctx->h[1]; h2 = ctx->h[2]; h3 = ctx->h[3]; h4 = ctx->h[4];
	r0 = ctx->r[0]; r1 = ctx->r[1]; r2 = ctx->r[2]; r3 = ctx->r[3]; r4 = ctx->r[4];

	/* s[i] = r[i] * 5 */
	s1 = r1 * 5; s2 = r2 * 5; s3 = r3 * 5; s4 = r4 * 5;

	while (blocks--) {
		h0 += (load_le32(data+ 0)     ) & 0x3ffffff;
		h1 += (load_le32(data+ 3) >> 2) & 0x3ffffff;
		h2 += (load_le32(data+ 6) >> 4) & 0x3ffffff;
		h3 += (load_le32(data+ 9) >> 6) & 0x3ffffff;
		h4 += (load_le32(data+12) >> 8) | hibit;

		d0 = ((uint64_t)h0 * r0) + ((uint64_t)h1 * s4) + ((uint64_t)h2 * s3) + ((uint64_t)h3 * s2) + ((uint64_t)h4 * s1);
		d1 = ((uint64_t)h0 * r1) + ((uint64_t)h1 * r0) + ((uint64_t)h2 * s4) + ((uint64_t)h3 * s3) + ((uint64_t)h4 * s2);
		d2 = ((uint64_t)h0 * r2) + ((uint64_t)h1 * r1) + ((uint64_t)h2 * r0) + ((uint64_t)h3 * s4) + ((uint64_t)h4 * s3);
		d3 = ((uint64_t)h0 * r3) + ((uint64_t)h1 * r2) + ((uint64_t)h2 * r1) + ((uint64_t)h3 * r0) + ((uint64_t)h4 * s4);
		d4 = ((uint64_t)h0 * r4) + ((uint64_t)h1 * r3) + ((uint64_t)h2 * r2) + ((uint64_t)h3 * r1) + ((uint64_t)h4 * r0);

		c = (uint32_t)(d0 >> 26); h0 = (uint32_t)d0 & 0x3ffffff;
		d1 += c;      c = (uint32_t)(d1 >> 26); h1 = (uint32_t)d1 & 0x3ffffff;
		d2 += c;      c = (uint32_t)(d2 >> 26); h2 = (uint32_t)d2 & 0x3ffffff;
		d3 += c;      c = (uint32_t)(d3 >> 26); h3 = (uint32_t)d3 & 0x3ffffff;
		d4 += c;      c = (uint32_t)(d4 >> 26); h4 = (uint32_t)d4 & 0x3ffffff;
		h0 += c * 5;  c =           (h0 >> 26); h0 = h0 & 0x3ffffff;
		h1 += c;

		data += 16;
	}

	/* store h[i] */
	ctx->h[0] = h0; ctx->h[1] = h1; ctx->h[2] = h2; ctx->h[3] = h3; ctx->h[4] = h4;
}

void cryptonite_poly1305_init(poly1305_ctx *ctx, poly1305_key *key)
{
	uint8_t *k = (uint8_t *) key;

	memset(ctx, 0, sizeof(poly1305_ctx));

	ctx->r[0] = (load_le32(&k[ 0])     ) & 0x3ffffff;
	ctx->r[1] = (load_le32(&k[ 3]) >> 2) & 0x3ffff03;
	ctx->r[2] = (load_le32(&k[ 6]) >> 4) & 0x3ffc0ff;
	ctx->r[3] = (load_le32(&k[ 9]) >> 6) & 0x3f03fff;
	ctx->r[4] = (load_le32(&k[12]) >> 8) & 0x00fffff;

	ctx->pad[0] = load_le32(&k[16]);
	ctx->pad[1] = load_le32(&k[20]);
	ctx->pad[2] = load_le32(&k[24]);
	ctx->pad[3] = load_le32(&k[28]);

	ctx->index = 0;
}

void cryptonite_poly1305_update(poly1305_ctx *ctx, uint8_t *data, uint32_t length)
{
	uint32_t to_fill, nb_blocks_bytes;

	to_fill = 16 - ctx->index;

	/* process partial buffer if there's enough data to make a block */
	if (ctx->index && length >= to_fill) {
		memcpy(ctx->buf + ctx->index, data, to_fill);
		poly1305_do_chunk(ctx, ctx->buf, 1, 0);
		ctx->index = 0;
		length -= to_fill;
		data += to_fill;
	}

	/* process as much 16-block as possible */
	nb_blocks_bytes = length & ~(16 - 1);
	poly1305_do_chunk(ctx, data, nb_blocks_bytes >> 4, 0);
	data += nb_blocks_bytes;
	length &= 0xf;

	/* fill the remaining bytes in the partial buffer */
	if (length) {
		memcpy(ctx->buf + ctx->index, data, length);
		ctx->index += length;
	}
}

void cryptonite_poly1305_finalize(poly1305_mac mac8, poly1305_ctx *ctx)
{
	uint32_t h0,h1,h2,h3,h4,c;
	uint32_t g0,g1,g2,g3,g4;
	uint64_t f;
	uint32_t mask;
	uint32_t *mac = (uint32_t *) mac8;
	int i;

	if (ctx->index) {
		/* append partial final buffer with 10* then process */
		ctx->buf[ctx->index] = 0x1;
		for (i = ctx->index + 1; i < 16; i++)
			ctx->buf[i] = 0x0;
		poly1305_do_chunk(ctx, ctx->buf, 1, 1);
	}

	/* following is a cleanup copy of code available poly1305-donna */

	/* fully carry h */
	h0 = ctx->h[0]; h1 = ctx->h[1]; h2 = ctx->h[2]; h3 = ctx->h[3]; h4 = ctx->h[4];

	             c = h1 >> 26; h1 = h1 & 0x3ffffff;
	h2 +=     c; c = h2 >> 26; h2 = h2 & 0x3ffffff;
	h3 +=     c; c = h3 >> 26; h3 = h3 & 0x3ffffff;
	h4 +=     c; c = h4 >> 26; h4 = h4 & 0x3ffffff;
	h0 += c * 5; c = h0 >> 26; h0 = h0 & 0x3ffffff;
	h1 +=     c;

	/* compute h + -p */
	g0 = h0 + 5; c = g0 >> 26; g0 &= 0x3ffffff;
	g1 = h1 + c; c = g1 >> 26; g1 &= 0x3ffffff;
	g2 = h2 + c; c = g2 >> 26; g2 &= 0x3ffffff;
	g3 = h3 + c; c = g3 >> 26; g3 &= 0x3ffffff;
	g4 = h4 + c - (1 << 26);

	/* select h if h < p, or h + -p if h >= p */
	mask = (g4 >> ((sizeof(uint32_t) * 8) - 1)) - 1;
	g0 &= mask;
	g1 &= mask;
	g2 &= mask;
	g3 &= mask;
	g4 &= mask;
	mask = ~mask;
	h0 = (h0 & mask) | g0;
	h1 = (h1 & mask) | g1;
	h2 = (h2 & mask) | g2;
	h3 = (h3 & mask) | g3;
	h4 = (h4 & mask) | g4;

	/* h = h % (2^128) */
	h0 = ((h0      ) | (h1 << 26)) & 0xffffffff;
	h1 = ((h1 >>  6) | (h2 << 20)) & 0xffffffff;
	h2 = ((h2 >> 12) | (h3 << 14)) & 0xffffffff;
	h3 = ((h3 >> 18) | (h4 <<  8)) & 0xffffffff;

	/* mac = (h + pad) % (2^128) */
	f = (uint64_t)h0 + ctx->pad[0];
	mac[0] = cpu_to_le32((uint32_t) f);

	f = (uint64_t)h1 + ctx->pad[1] + (f >> 32);
	mac[1] = cpu_to_le32((uint32_t) f);

	f = (uint64_t)h2 + ctx->pad[2] + (f >> 32);
	mac[2] = cpu_to_le32((uint32_t) f);

	f = (uint64_t)h3 + ctx->pad[3] + (f >> 32);
	mac[3] = cpu_to_le32((uint32_t) f);
}
