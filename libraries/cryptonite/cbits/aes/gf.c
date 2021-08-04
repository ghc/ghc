/*
 * Copyright (c) 2012 Vincent Hanquez <vincent@snarc.org>
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
 */

#include <stdio.h>
#include <stdint.h>
#include <cryptonite_cpu.h>
#include <aes/gf.h>
#include <aes/x86ni.h>

/* inplace GFMUL for xts mode */
void cryptonite_aes_generic_gf_mulx(block128 *a)
{
	const uint64_t gf_mask = cpu_to_le64(0x8000000000000000ULL);
	uint64_t r = ((a->q[1] & gf_mask) ? cpu_to_le64(0x87) : 0);
	a->q[1] = cpu_to_le64((le64_to_cpu(a->q[1]) << 1) | (a->q[0] & gf_mask ? 1 : 0));
	a->q[0] = cpu_to_le64(le64_to_cpu(a->q[0]) << 1) ^ r;
}


/*
 * GF multiplication with Shoup's method and 4-bit table.
 *
 * We precompute the products of H with all 4-bit polynomials and store them in
 * a 'table_4bit' array.  To avoid unnecessary byte swapping, the 16 blocks are
 * written to the table with qwords already converted to CPU order.  Table
 * indices use the reflected bit ordering, i.e. polynomials X^0, X^1, X^2, X^3
 * map to bit positions 3, 2, 1, 0 respectively.
 *
 * To multiply an arbitrary block with H, the input block is decomposed in 4-bit
 * segments.  We get the final result after 32 table lookups and additions, one
 * for each segment, interleaving multiplication by P(X)=X^4.
 */

/* convert block128 qwords between BE and CPU order */
static inline void block128_cpu_swap_be(block128 *a, const block128 *b)
{
	a->q[1] = cpu_to_be64(b->q[1]);
	a->q[0] = cpu_to_be64(b->q[0]);
}

/* multiplication by P(X)=X, assuming qwords already in CPU order */
static inline void cpu_gf_mulx(block128 *a, const block128 *b)
{
	uint64_t v0 = b->q[0];
	uint64_t v1 = b->q[1];
	a->q[1] = v1 >> 1 | v0 << 63;
	a->q[0] = v0 >> 1 ^ ((0-(v1 & 1)) & 0xe100000000000000ULL);
}

static const uint64_t r4_0[] =
	{ 0x0000000000000000ULL, 0x1c20000000000000ULL
	, 0x3840000000000000ULL, 0x2460000000000000ULL
	, 0x7080000000000000ULL, 0x6ca0000000000000ULL
	, 0x48c0000000000000ULL, 0x54e0000000000000ULL
	, 0xe100000000000000ULL, 0xfd20000000000000ULL
	, 0xd940000000000000ULL, 0xc560000000000000ULL
	, 0x9180000000000000ULL, 0x8da0000000000000ULL
	, 0xa9c0000000000000ULL, 0xb5e0000000000000ULL
	};

/* multiplication by P(X)=X^4, assuming qwords already in CPU order */
static inline void cpu_gf_mulx4(block128 *a, const block128 *b)
{
	uint64_t v0 = b->q[0];
	uint64_t v1 = b->q[1];
	a->q[1] = v1 >> 4 | v0 << 60;
	a->q[0] = v0 >> 4 ^ r4_0[v1 & 0xf];
}

/* initialize the 4-bit table given H */
void cryptonite_aes_generic_hinit(table_4bit htable, const block128 *h)
{
	block128 v, *p;
	int i, j;

	/* multiplication by 0 is 0 */
	block128_zero(&htable[0]);

	/* at index 8=2^3 we have H.X^0 = H */
	i = 8;
	block128_cpu_swap_be(&htable[i], h); /* in CPU order */
	p = &htable[i];

	/* for other powers of 2, repeat multiplication by P(X)=X */
	for (i = 4; i > 0; i >>= 1)
	{
		cpu_gf_mulx(&htable[i], p);
		p = &htable[i];
	}

	/* remaining elements are linear combinations */
	for (i = 2; i < 16; i <<= 1) {
		p = &htable[i];
		v = *p;
		for (j = 1; j < i; j++) {
			p[j] = v;
			block128_xor_aligned(&p[j], &htable[j]);
		}
	}
}

/* multiply a block with H */
void cryptonite_aes_generic_gf_mul(block128 *a, const table_4bit htable)
{
	block128 b;
	int i;
	block128_zero(&b);
	for (i = 15; i >= 0; i--)
	{
		uint8_t v = a->b[i];
		block128_xor_aligned(&b, &htable[v & 0xf]); /* high bits (reflected) */
		cpu_gf_mulx4(&b, &b);
		block128_xor_aligned(&b, &htable[v >> 4]);  /* low bits (reflected) */
		if (i > 0)
			cpu_gf_mulx4(&b, &b);
		else
			block128_cpu_swap_be(a, &b); /* restore BE order when done */
	}
}
