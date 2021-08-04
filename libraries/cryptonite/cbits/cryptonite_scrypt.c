/*
 * Copyright (C) 2014 Vincent Hanquez <vincent@snarc.org>
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
 *
 * Based on scrypt from Colin Percival's paper
 */

#include <stdint.h>
#include <string.h>
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"
#include "cryptonite_salsa.h"

static void blockmix_salsa8(uint32_t *in, uint32_t *out, uint32_t *X, const uint32_t r)
{
	int i;

	array_copy32(X, &in[(2 * r - 1) * 16], 16);

	for (i = 0; i < 2 * r; i += 2) {
		cryptonite_salsa_core_xor(8, (block *) X, (block *) &in[i*16]);
		array_copy32(&out[i * 8], X, 16);

		cryptonite_salsa_core_xor(8, (block *) X, (block *) &in[i*16+16]);
		array_copy32(&out[i * 8 + r * 16], X, 16);
	}
}

static inline uint64_t integerify(uint32_t *B, const uint32_t r)
{
	return B[(2*r-1) * 16] | (uint64_t)B[(2*r-1) * 16 + 1] << 32;
}

void cryptonite_scrypt_smix(uint8_t *B, const uint32_t r, const uint64_t N, uint32_t *V, uint32_t *XY)
{
	uint32_t *X = XY;
	uint32_t *Y = &XY[32 * r];
	uint32_t *Z = &XY[64 * r];
	uint64_t i, j;
	int k;
	const int r32 = 32*r;

	for (k = 0; k < r32; k++)
		X[k] = load_le32_aligned(&B[4 * k]);
	for (i = 0; i < N; i += 2) {
		array_copy32(&V[i * r32], X, r32);
		blockmix_salsa8(X, Y, Z, r);
		array_copy32(&V[(i + 1) * r32], Y, r32);
		blockmix_salsa8(Y, X, Z, r);
	}
	for (i = 0; i < N; i += 2) {
		j = integerify(X, r) & (N - 1);
		array_xor32(X, &V[j * r32], r32);
		blockmix_salsa8(X, Y, Z, r);

		j = integerify(Y, r) & (N - 1);
		array_xor32(Y, &V[j * r32], r32);
		blockmix_salsa8(Y, X, Z, r);
	}
	for (k = 0; k < r32; k++)
		store_le32_aligned(&B[4*k], X[k]);
}
