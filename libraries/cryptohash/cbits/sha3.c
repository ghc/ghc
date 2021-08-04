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

#include <stdint.h>
#include <string.h>
#include "bitfn.h"
#include "sha3.h"

#define KECCAK_NB_ROUNDS 24

/* rounds constants */
static const uint64_t keccak_rndc[24] =
{
	0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808aULL,
	0x8000000080008000ULL, 0x000000000000808bULL, 0x0000000080000001ULL,
	0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008aULL,
	0x0000000000000088ULL, 0x0000000080008009ULL, 0x000000008000000aULL,
	0x000000008000808bULL, 0x800000000000008bULL, 0x8000000000008089ULL,
	0x8000000000008003ULL, 0x8000000000008002ULL, 0x8000000000000080ULL,
	0x000000000000800aULL, 0x800000008000000aULL, 0x8000000080008081ULL,
	0x8000000000008080ULL, 0x0000000080000001ULL, 0x8000000080008008ULL,
};

/* triangular numbers constants */
static const int keccak_rotc[24] =
	{ 1,3,6,10,15,21,28,36,45,55,2,14,27,41,56,8,25,43,62,18,39,61,20,44 };

static const int keccak_piln[24] =
	{ 10,7,11,17,18,3,5,16,8,21,24,4,15,23,19,13,12,2,20,14,22,9,6,1 };

static inline void sha3_do_chunk(uint64_t state[25], uint64_t buf[], int bufsz)
{
	int i, j, r;
	uint64_t tmp, bc[5];

	/* merge buf with state */
	for (i = 0; i < bufsz; i++)
		state[i] ^= le64_to_cpu(buf[i]);

	/* run keccak rounds */
	for (r = 0; r < KECCAK_NB_ROUNDS; r++) {
		/* compute the parity of each columns */
		for (i = 0; i < 5; i++)
			bc[i] = state[i] ^ state[i+5] ^ state[i+10] ^ state[i+15] ^ state[i+20];

		for (i = 0; i < 5; i++) {
			tmp = bc[(i + 4) % 5] ^ rol64(bc[(i + 1) % 5], 1);
			for (j = 0; j < 25; j += 5)
				state[j + i] ^= tmp;
		}

		/* rho pi */
		tmp = state[1];
		for (i = 0; i < 24; i++) {
			j = keccak_piln[i];
			bc[0] = state[j];
			state[j] = rol64(tmp, keccak_rotc[i]);
			tmp = bc[0];
		}

		/* bitwise combine along rows using a = a xor (not b and c) */
		for (j = 0; j < 25; j += 5) {
			for (i = 0; i < 5; i++)
				bc[i] = state[j + i];
			#define andn(b,c) (~(b) & (c))
			state[j + 0] ^= andn(bc[1], bc[2]);
			state[j + 1] ^= andn(bc[2], bc[3]);
			state[j + 2] ^= andn(bc[3], bc[4]);
			state[j + 3] ^= andn(bc[4], bc[0]);
			state[j + 4] ^= andn(bc[0], bc[1]);
			#undef andn
		}

		/* xor the round constant */
		state[0] ^= keccak_rndc[r];
	}
}

void cryptohash_sha3_init(struct sha3_ctx *ctx, uint32_t hashlen)
{
	memset(ctx, 0, sizeof(*ctx));
	ctx->hashlen = hashlen / 8;
	ctx->bufsz = 200 - 2 * ctx->hashlen;
}

void cryptohash_sha3_update(struct sha3_ctx *ctx, uint8_t *data, uint32_t len)
{
	uint32_t to_fill;

	to_fill = ctx->bufsz - ctx->bufindex;

	if (ctx->bufindex == ctx->bufsz) {
		sha3_do_chunk(ctx->state, (uint64_t *) ctx->buf, ctx->bufsz / 8);
		ctx->bufindex = 0;
	}

	/* process partial buffer if there's enough data to make a block */
	if (ctx->bufindex && len >= to_fill) {
		memcpy(ctx->buf + ctx->bufindex, data, to_fill);
		sha3_do_chunk(ctx->state, (uint64_t *) ctx->buf, ctx->bufsz / 8);
		len -= to_fill;
		data += to_fill;
		ctx->bufindex = 0;
	}

	/* process as much ctx->bufsz-block */
	for (; len >= ctx->bufsz; len -= ctx->bufsz, data += ctx->bufsz)
		sha3_do_chunk(ctx->state, (uint64_t *) data, ctx->bufsz / 8);

	/* append data into buf */
	if (len) {
		memcpy(ctx->buf + ctx->bufindex, data, len);
		ctx->bufindex += len;
	}
}

void cryptohash_sha3_finalize(struct sha3_ctx *ctx, uint8_t *out)
{
	uint64_t w[25];

	/* process full buffer if needed */
	if (ctx->bufindex == ctx->bufsz) {
		sha3_do_chunk(ctx->state, (uint64_t *) ctx->buf, ctx->bufsz / 8);
		ctx->bufindex = 0;
	}

	/* add the 10*1 padding */
	ctx->buf[ctx->bufindex++] = 1;
	memset(ctx->buf + ctx->bufindex, 0, ctx->bufsz - ctx->bufindex);
	ctx->buf[ctx->bufsz - 1] |= 0x80;

	/* process */
	sha3_do_chunk(ctx->state, (uint64_t *) ctx->buf, ctx->bufsz / 8);

	/* output */
	cpu_to_le64_array(w, ctx->state, 25);
	memcpy(out, w, ctx->hashlen);
}
