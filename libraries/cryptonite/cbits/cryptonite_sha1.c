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
#include "cryptonite_sha1.h"
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"

void cryptonite_sha1_init(struct sha1_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->h[0] = 0x67452301;
	ctx->h[1] = 0xefcdab89;
	ctx->h[2] = 0x98badcfe;
	ctx->h[3] = 0x10325476;
	ctx->h[4] = 0xc3d2e1f0;
}

#define f1(x, y, z)   (z ^ (x & (y ^ z)))
#define f2(x, y, z)   (x ^ y ^ z)
#define f3(x, y, z)   ((x & y) + (z & (x ^ y)))
#define f4(x, y, z)   f2(x, y, z)

#define K1  0x5a827999
#define K2  0x6ed9eba1
#define K3  0x8f1bbcdc
#define K4  0xca62c1d6

#define R(a, b, c, d, e, f, k, w)  \
	e += rol32(a, 5) + f(b, c, d) + k + w; b = rol32(b, 30)

#define M(i)  (w[i & 0x0f] = rol32(w[i & 0x0f] ^ w[(i - 14) & 0x0f] \
              ^ w[(i - 8) & 0x0f] ^ w[(i - 3) & 0x0f], 1))

static inline void sha1_do_chunk(struct sha1_ctx *ctx, uint32_t *buf)
{
	uint32_t a, b, c, d, e;
	uint32_t w[16];
#define CPY(i)	w[i] = be32_to_cpu(buf[i])
	CPY(0); CPY(1); CPY(2); CPY(3); CPY(4); CPY(5); CPY(6); CPY(7);
	CPY(8); CPY(9); CPY(10); CPY(11); CPY(12); CPY(13); CPY(14); CPY(15);
#undef CPY

	a = ctx->h[0]; b = ctx->h[1]; c = ctx->h[2]; d = ctx->h[3]; e = ctx->h[4];

	R(a, b, c, d, e, f1, K1, w[0]);
	R(e, a, b, c, d, f1, K1, w[1]);
	R(d, e, a, b, c, f1, K1, w[2]);
	R(c, d, e, a, b, f1, K1, w[3]);
	R(b, c, d, e, a, f1, K1, w[4]);
	R(a, b, c, d, e, f1, K1, w[5]);
	R(e, a, b, c, d, f1, K1, w[6]);
	R(d, e, a, b, c, f1, K1, w[7]);
	R(c, d, e, a, b, f1, K1, w[8]);
	R(b, c, d, e, a, f1, K1, w[9]);
	R(a, b, c, d, e, f1, K1, w[10]);
	R(e, a, b, c, d, f1, K1, w[11]);
	R(d, e, a, b, c, f1, K1, w[12]);
	R(c, d, e, a, b, f1, K1, w[13]);
	R(b, c, d, e, a, f1, K1, w[14]);
	R(a, b, c, d, e, f1, K1, w[15]);
	R(e, a, b, c, d, f1, K1, M(16));
	R(d, e, a, b, c, f1, K1, M(17));
	R(c, d, e, a, b, f1, K1, M(18));
	R(b, c, d, e, a, f1, K1, M(19));

	R(a, b, c, d, e, f2, K2, M(20));
	R(e, a, b, c, d, f2, K2, M(21));
	R(d, e, a, b, c, f2, K2, M(22));
	R(c, d, e, a, b, f2, K2, M(23));
	R(b, c, d, e, a, f2, K2, M(24));
	R(a, b, c, d, e, f2, K2, M(25));
	R(e, a, b, c, d, f2, K2, M(26));
	R(d, e, a, b, c, f2, K2, M(27));
	R(c, d, e, a, b, f2, K2, M(28));
	R(b, c, d, e, a, f2, K2, M(29));
	R(a, b, c, d, e, f2, K2, M(30));
	R(e, a, b, c, d, f2, K2, M(31));
	R(d, e, a, b, c, f2, K2, M(32));
	R(c, d, e, a, b, f2, K2, M(33));
	R(b, c, d, e, a, f2, K2, M(34));
	R(a, b, c, d, e, f2, K2, M(35));
	R(e, a, b, c, d, f2, K2, M(36));
	R(d, e, a, b, c, f2, K2, M(37));
	R(c, d, e, a, b, f2, K2, M(38));
	R(b, c, d, e, a, f2, K2, M(39));

	R(a, b, c, d, e, f3, K3, M(40));
	R(e, a, b, c, d, f3, K3, M(41));
	R(d, e, a, b, c, f3, K3, M(42));
	R(c, d, e, a, b, f3, K3, M(43));
	R(b, c, d, e, a, f3, K3, M(44));
	R(a, b, c, d, e, f3, K3, M(45));
	R(e, a, b, c, d, f3, K3, M(46));
	R(d, e, a, b, c, f3, K3, M(47));
	R(c, d, e, a, b, f3, K3, M(48));
	R(b, c, d, e, a, f3, K3, M(49));
	R(a, b, c, d, e, f3, K3, M(50));
	R(e, a, b, c, d, f3, K3, M(51));
	R(d, e, a, b, c, f3, K3, M(52));
	R(c, d, e, a, b, f3, K3, M(53));
	R(b, c, d, e, a, f3, K3, M(54));
	R(a, b, c, d, e, f3, K3, M(55));
	R(e, a, b, c, d, f3, K3, M(56));
	R(d, e, a, b, c, f3, K3, M(57));
	R(c, d, e, a, b, f3, K3, M(58));
	R(b, c, d, e, a, f3, K3, M(59));

	R(a, b, c, d, e, f4, K4, M(60));
	R(e, a, b, c, d, f4, K4, M(61));
	R(d, e, a, b, c, f4, K4, M(62));
	R(c, d, e, a, b, f4, K4, M(63));
	R(b, c, d, e, a, f4, K4, M(64));
	R(a, b, c, d, e, f4, K4, M(65));
	R(e, a, b, c, d, f4, K4, M(66));
	R(d, e, a, b, c, f4, K4, M(67));
	R(c, d, e, a, b, f4, K4, M(68));
	R(b, c, d, e, a, f4, K4, M(69));
	R(a, b, c, d, e, f4, K4, M(70));
	R(e, a, b, c, d, f4, K4, M(71));
	R(d, e, a, b, c, f4, K4, M(72));
	R(c, d, e, a, b, f4, K4, M(73));
	R(b, c, d, e, a, f4, K4, M(74));
	R(a, b, c, d, e, f4, K4, M(75));
	R(e, a, b, c, d, f4, K4, M(76));
	R(d, e, a, b, c, f4, K4, M(77));
	R(c, d, e, a, b, f4, K4, M(78));
	R(b, c, d, e, a, f4, K4, M(79));

	ctx->h[0] += a;
	ctx->h[1] += b;
	ctx->h[2] += c;
	ctx->h[3] += d;
	ctx->h[4] += e;
}

void cryptonite_sha1_update(struct sha1_ctx *ctx, const uint8_t *data, uint32_t len)
{
	uint32_t index, to_fill;

	index = (uint32_t) (ctx->sz & 0x3f);
	to_fill = 64 - index;

	ctx->sz += len;

	/* process partial buffer if there's enough data to make a block */
	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		sha1_do_chunk(ctx, (uint32_t *) ctx->buf);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	if (need_alignment(data, 4)) {
		uint32_t tramp[16];
		ASSERT_ALIGNMENT(tramp, 4);
		for (; len >= 64; len -= 64, data += 64) {
			memcpy(tramp, data, 64);
			sha1_do_chunk(ctx, tramp);
		}
	} else {
		/* process as much 64-block as possible */
		for (; len >= 64; len -= 64, data += 64)
			sha1_do_chunk(ctx, (uint32_t *) data);
	}

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}

void cryptonite_sha1_finalize(struct sha1_ctx *ctx, uint8_t *out)
{
	static uint8_t padding[64] = { 0x80, };
	uint64_t bits;
	uint32_t index, padlen;

	/* add padding and update data with it */
	bits = cpu_to_be64(ctx->sz << 3);

	/* pad out to 56 */
	index = (uint32_t) (ctx->sz & 0x3f);
	padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);
	cryptonite_sha1_update(ctx, padding, padlen);

	/* append length */
	cryptonite_sha1_update(ctx, (uint8_t *) &bits, sizeof(bits));

	/* output hash */
	store_be32(out   , ctx->h[0]);
	store_be32(out+ 4, ctx->h[1]);
	store_be32(out+ 8, ctx->h[2]);
	store_be32(out+12, ctx->h[3]);
	store_be32(out+16, ctx->h[4]);
}

#define HASHED(m) SHA1_##m
#define HASHED_LOWER(m) sha1_##m
#define CRYPTONITE_HASHED(m) cryptonite_sha1_##m
#define SHA1_BLOCK_SIZE 64
#define SHA1_BITS_ELEMS 1

static inline uint32_t cryptonite_sha1_get_index(const struct sha1_ctx *ctx)
{
	return (uint32_t) (ctx->sz & 0x3f);
}

static inline void cryptonite_sha1_incr_sz(struct sha1_ctx *ctx, uint64_t *bits, uint32_t n)
{
	ctx->sz += n;
	*bits = cpu_to_be64(ctx->sz << 3);
}

static inline void cryptonite_sha1_select_digest(const struct sha1_ctx *ctx, uint8_t *out, uint32_t out_mask)
{
	xor_be32(out   , ctx->h[0] & out_mask);
	xor_be32(out+ 4, ctx->h[1] & out_mask);
	xor_be32(out+ 8, ctx->h[2] & out_mask);
	xor_be32(out+12, ctx->h[3] & out_mask);
	xor_be32(out+16, ctx->h[4] & out_mask);
}

#include <cryptonite_hash_prefix.c>
