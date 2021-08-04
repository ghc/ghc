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
#include "cryptonite_md5.h"

void cryptonite_md5_init(struct md5_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->sz = 0ULL;
	ctx->h[0] = 0x67452301;
	ctx->h[1] = 0xefcdab89;
	ctx->h[2] = 0x98badcfe;
	ctx->h[3] = 0x10325476;
}

#define f1(x, y, z)	(z ^ (x & (y ^ z)))
#define f2(x, y, z)	f1(z, x, y)
#define f3(x, y, z)	(x ^ y ^ z)
#define f4(x, y, z)	(y ^ (x | ~z))
#define R(f, a, b, c, d, i, k, s) a += f(b, c, d) + w[i] + k; a = rol32(a, s); a += b

static void md5_do_chunk(struct md5_ctx *ctx, uint32_t *buf)
{
	uint32_t a, b, c, d;
#ifdef ARCH_IS_BIG_ENDIAN
	uint32_t w[16];
	cpu_to_le32_array(w, buf, 16);
#else
	uint32_t *w = buf;
#endif
	a = ctx->h[0]; b = ctx->h[1]; c = ctx->h[2]; d = ctx->h[3];

	R(f1, a, b, c, d, 0, 0xd76aa478, 7);
	R(f1, d, a, b, c, 1, 0xe8c7b756, 12);
	R(f1, c, d, a, b, 2, 0x242070db, 17);
	R(f1, b, c, d, a, 3, 0xc1bdceee, 22);
	R(f1, a, b, c, d, 4, 0xf57c0faf, 7);
	R(f1, d, a, b, c, 5, 0x4787c62a, 12);
	R(f1, c, d, a, b, 6, 0xa8304613, 17);
	R(f1, b, c, d, a, 7, 0xfd469501, 22);
	R(f1, a, b, c, d, 8, 0x698098d8, 7);
	R(f1, d, a, b, c, 9, 0x8b44f7af, 12);
	R(f1, c, d, a, b, 10, 0xffff5bb1, 17);
	R(f1, b, c, d, a, 11, 0x895cd7be, 22);
	R(f1, a, b, c, d, 12, 0x6b901122, 7);
	R(f1, d, a, b, c, 13, 0xfd987193, 12);
	R(f1, c, d, a, b, 14, 0xa679438e, 17);
	R(f1, b, c, d, a, 15, 0x49b40821, 22);

	R(f2, a, b, c, d, 1, 0xf61e2562, 5);
	R(f2, d, a, b, c, 6, 0xc040b340, 9);
	R(f2, c, d, a, b, 11, 0x265e5a51, 14);
	R(f2, b, c, d, a, 0, 0xe9b6c7aa, 20);
	R(f2, a, b, c, d, 5, 0xd62f105d, 5);
	R(f2, d, a, b, c, 10, 0x02441453, 9);
	R(f2, c, d, a, b, 15, 0xd8a1e681, 14);
	R(f2, b, c, d, a, 4, 0xe7d3fbc8, 20);
	R(f2, a, b, c, d, 9, 0x21e1cde6, 5);
	R(f2, d, a, b, c, 14, 0xc33707d6, 9);
	R(f2, c, d, a, b, 3, 0xf4d50d87, 14);
	R(f2, b, c, d, a, 8, 0x455a14ed, 20);
	R(f2, a, b, c, d, 13, 0xa9e3e905, 5);
	R(f2, d, a, b, c, 2, 0xfcefa3f8, 9);
	R(f2, c, d, a, b, 7, 0x676f02d9, 14);
	R(f2, b, c, d, a, 12, 0x8d2a4c8a, 20);

	R(f3, a, b, c, d, 5, 0xfffa3942, 4);
	R(f3, d, a, b, c, 8, 0x8771f681, 11);
	R(f3, c, d, a, b, 11, 0x6d9d6122, 16);
	R(f3, b, c, d, a, 14, 0xfde5380c, 23);
	R(f3, a, b, c, d, 1, 0xa4beea44, 4);
	R(f3, d, a, b, c, 4, 0x4bdecfa9, 11);
	R(f3, c, d, a, b, 7, 0xf6bb4b60, 16);
	R(f3, b, c, d, a, 10, 0xbebfbc70, 23);
	R(f3, a, b, c, d, 13, 0x289b7ec6, 4);
	R(f3, d, a, b, c, 0, 0xeaa127fa, 11);
	R(f3, c, d, a, b, 3, 0xd4ef3085, 16);
	R(f3, b, c, d, a, 6, 0x04881d05, 23);
	R(f3, a, b, c, d, 9, 0xd9d4d039, 4);
	R(f3, d, a, b, c, 12, 0xe6db99e5, 11);
	R(f3, c, d, a, b, 15, 0x1fa27cf8, 16);
	R(f3, b, c, d, a, 2, 0xc4ac5665, 23);

	R(f4, a, b, c, d, 0, 0xf4292244, 6);
	R(f4, d, a, b, c, 7, 0x432aff97, 10);
	R(f4, c, d, a, b, 14, 0xab9423a7, 15);
	R(f4, b, c, d, a, 5, 0xfc93a039, 21);
	R(f4, a, b, c, d, 12, 0x655b59c3, 6);
	R(f4, d, a, b, c, 3, 0x8f0ccc92, 10);
	R(f4, c, d, a, b, 10, 0xffeff47d, 15);
	R(f4, b, c, d, a, 1, 0x85845dd1, 21);
	R(f4, a, b, c, d, 8, 0x6fa87e4f, 6);
	R(f4, d, a, b, c, 15, 0xfe2ce6e0, 10);
	R(f4, c, d, a, b, 6, 0xa3014314, 15);
	R(f4, b, c, d, a, 13, 0x4e0811a1, 21);
	R(f4, a, b, c, d, 4, 0xf7537e82, 6);
	R(f4, d, a, b, c, 11, 0xbd3af235, 10);
	R(f4, c, d, a, b, 2, 0x2ad7d2bb, 15);
	R(f4, b, c, d, a, 9, 0xeb86d391, 21);

	ctx->h[0] += a; ctx->h[1] += b; ctx->h[2] += c; ctx->h[3] += d;
}

void cryptonite_md5_update(struct md5_ctx *ctx, const uint8_t *data, uint32_t len)
{
	uint32_t index, to_fill;

	index = (uint32_t) (ctx->sz & 0x3f);
	to_fill = 64 - index;

	ctx->sz += len;

	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		md5_do_chunk(ctx, (uint32_t *) ctx->buf);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	if (need_alignment(data, 4)) {
		uint32_t tramp[16];
		ASSERT_ALIGNMENT(tramp, 4);
		for (; len >= 64; len -= 64, data += 64) {
			memcpy(tramp, data, 64);
			md5_do_chunk(ctx, tramp);
		}
	} else {
		/* process as much 64-block as possible */
		for (; len >= 64; len -= 64, data += 64)
			md5_do_chunk(ctx, (uint32_t *) data);
	}

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}

void cryptonite_md5_finalize(struct md5_ctx *ctx, uint8_t *out)
{
	static uint8_t padding[64] = { 0x80, };
	uint64_t bits;
	uint32_t index, padlen;

	/* add padding and update data with it */
	bits = cpu_to_le64(ctx->sz << 3);

	/* pad out to 56 */
	index = (uint32_t) (ctx->sz & 0x3f);
	padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);
	cryptonite_md5_update(ctx, padding, padlen);

	/* append length */
	cryptonite_md5_update(ctx, (uint8_t *) &bits, sizeof(bits));

	/* output hash */
	store_le32(out   , ctx->h[0]);
	store_le32(out+ 4, ctx->h[1]);
	store_le32(out+ 8, ctx->h[2]);
	store_le32(out+12, ctx->h[3]);
}

#define HASHED(m) MD5_##m
#define HASHED_LOWER(m) md5_##m
#define CRYPTONITE_HASHED(m) cryptonite_md5_##m
#define MD5_BLOCK_SIZE 64
#define MD5_BITS_ELEMS 1

static inline uint32_t cryptonite_md5_get_index(const struct md5_ctx *ctx)
{
	return (uint32_t) (ctx->sz & 0x3f);
}

static inline void cryptonite_md5_incr_sz(struct md5_ctx *ctx, uint64_t *bits, uint32_t n)
{
	ctx->sz += n;
	*bits = cpu_to_le64(ctx->sz << 3);
}

static inline void cryptonite_md5_select_digest(const struct md5_ctx *ctx, uint8_t *out, uint32_t out_mask)
{
	xor_le32(out   , ctx->h[0] & out_mask);
	xor_le32(out+ 4, ctx->h[1] & out_mask);
	xor_le32(out+ 8, ctx->h[2] & out_mask);
	xor_le32(out+12, ctx->h[3] & out_mask);
}

#include <cryptonite_hash_prefix.c>
