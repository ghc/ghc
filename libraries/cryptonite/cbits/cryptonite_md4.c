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

#include <string.h>
#include <stdio.h>
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"
#include "cryptonite_md4.h"

void cryptonite_md4_init(struct md4_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->sz = 0ULL;
	ctx->h[0] = 0x67452301;
	ctx->h[1] = 0xefcdab89;
	ctx->h[2] = 0x98badcfe;
	ctx->h[3] = 0x10325476;
}

#define f1(x, y, z)	((x & y) | ((~x) & z))
#define f2(x, y, z)	((x & y) | (x & z) | (y & z))
#define f3(x, y, z)	(x ^ y ^ z)

#define K1 	0x00000000
#define K2 	0x5A827999
#define K3 	0x6ED9EBA1
#define R(a,b,c,d,f,k,s,i) (a = rol32(a + f(b,c,d) + w[i] + k, s))

static void md4_do_chunk(struct md4_ctx *ctx, uint32_t *buf)
{
	uint32_t a, b, c, d;
#ifdef ARCH_IS_BIG_ENDIAN
	uint32_t w[16];
	cpu_to_le32_array(w, (uint32_t *) buf, 16);
#else
	uint32_t *w = buf;
#endif

	a = ctx->h[0]; b = ctx->h[1]; c = ctx->h[2]; d = ctx->h[3];

	R(a, b, c, d, f1, K1, 3, 0);
	R(d, a, b, c, f1, K1, 7, 1);
	R(c, d, a, b, f1, K1, 11, 2);
	R(b, c, d, a, f1, K1, 19, 3);
	R(a, b, c, d, f1, K1, 3, 4);
	R(d, a, b, c, f1, K1, 7, 5);
	R(c, d, a, b, f1, K1, 11, 6);
	R(b, c, d, a, f1, K1, 19, 7);
	R(a, b, c, d, f1, K1, 3, 8);
	R(d, a, b, c, f1, K1, 7, 9);
	R(c, d, a, b, f1, K1, 11, 10);
	R(b, c, d, a, f1, K1, 19, 11);
	R(a, b, c, d, f1, K1, 3, 12);
	R(d, a, b, c, f1, K1, 7, 13);
	R(c, d, a, b, f1, K1, 11, 14);
	R(b, c, d, a, f1, K1, 19, 15);

	R(a, b, c, d, f2, K2, 3, 0);
	R(d, a, b, c, f2, K2, 5, 4);
	R(c, d, a, b, f2, K2, 9, 8);
	R(b, c, d, a, f2, K2, 13, 12);
	R(a, b, c, d, f2, K2, 3, 1);
	R(d, a, b, c, f2, K2, 5, 5);
	R(c, d, a, b, f2, K2, 9, 9);
	R(b, c, d, a, f2, K2, 13, 13);
	R(a, b, c, d, f2, K2, 3, 2);
	R(d, a, b, c, f2, K2, 5, 6);
	R(c, d, a, b, f2, K2, 9, 10);
	R(b, c, d, a, f2, K2, 13, 14);
	R(a, b, c, d, f2, K2, 3, 3);
	R(d, a, b, c, f2, K2, 5, 7);
	R(c, d, a, b, f2, K2, 9, 11);
	R(b, c, d, a, f2, K2, 13, 15);

	R(a, b, c, d, f3, K3, 3, 0);
	R(d, a, b, c, f3, K3, 9, 8);
	R(c, d, a, b, f3, K3, 11, 4);
	R(b, c, d, a, f3, K3, 15, 12);
	R(a, b, c, d, f3, K3, 3, 2);
	R(d, a, b, c, f3, K3, 9, 10);
	R(c, d, a, b, f3, K3, 11, 6);
	R(b, c, d, a, f3, K3, 15, 14);
	R(a, b, c, d, f3, K3, 3, 1);
	R(d, a, b, c, f3, K3, 9, 9);
	R(c, d, a, b, f3, K3, 11, 5);
	R(b, c, d, a, f3, K3, 15, 13);
	R(a, b, c, d, f3, K3, 3, 3);
	R(d, a, b, c, f3, K3, 9, 11);
	R(c, d, a, b, f3, K3, 11, 7);
	R(b, c, d, a, f3, K3, 15, 15);

	ctx->h[0] += a; ctx->h[1] += b; ctx->h[2] += c; ctx->h[3] += d;
}

void cryptonite_md4_update(struct md4_ctx *ctx, const uint8_t *data, uint32_t len)
{
	uint32_t index, to_fill;

	index = (uint32_t) (ctx->sz & 0x3f);
	to_fill = 64 - index;

	ctx->sz += len;

	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		md4_do_chunk(ctx, (uint32_t *) ctx->buf);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	if (need_alignment(data, 4)) {
		uint32_t tramp[16];
		ASSERT_ALIGNMENT(tramp, 4);
		for (; len >= 64; len -= 64, data += 64) {
			memcpy(tramp, data, 64);
			md4_do_chunk(ctx, tramp);
		}
	} else {
		/* process as much 64-block as possible */
		for (; len >= 64; len -= 64, data += 64)
			md4_do_chunk(ctx, (uint32_t *) data);
	}

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}

void cryptonite_md4_finalize(struct md4_ctx *ctx, uint8_t *out)
{
	static uint8_t padding[64] = { 0x80, };
	uint64_t bits;
	uint32_t index, padlen;

	/* add padding and update data with it */
	bits = cpu_to_le64(ctx->sz << 3);

	/* pad out to 56 */
	index = (uint32_t) (ctx->sz & 0x3f);
	padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);
	cryptonite_md4_update(ctx, padding, padlen);

	/* append length */
	cryptonite_md4_update(ctx, (uint8_t *) &bits, sizeof(bits));

	/* output hash */
	store_le32(out   , ctx->h[0]);
	store_le32(out+ 4, ctx->h[1]);
	store_le32(out+ 8, ctx->h[2]);
	store_le32(out+12, ctx->h[3]);
}
