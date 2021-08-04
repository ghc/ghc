/*
 * Copyright (C) 2020 Olivier Chéron <olivier.cheron@gmail.com>
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

#include <cryptonite_hash_prefix.h>

void CRYPTONITE_HASHED(finalize_prefix)(struct HASHED_LOWER(ctx) *ctx, const uint8_t *data, uint32_t len, uint32_t n, uint8_t *out)
{
	uint64_t bits[HASHED(BITS_ELEMS)];
	uint8_t *p = (uint8_t *) &bits;
	uint32_t index, padidx, padlen, pos, out_mask;
	static const uint32_t cut_off = HASHED(BLOCK_SIZE) - sizeof(bits);

	/* Make sure n <= len */
	n += (len - n) & constant_time_lt(len, n);

	/* Initial index, based on current context state */
	index = CRYPTONITE_HASHED(get_index)(ctx);

	/* Final size after n bytes */
	CRYPTONITE_HASHED(incr_sz)(ctx, bits, n);

	/* Padding index and length */
	padidx = CRYPTONITE_HASHED(get_index)(ctx);
	padlen = HASHED(BLOCK_SIZE) + cut_off - padidx;
	padlen -= HASHED(BLOCK_SIZE) & constant_time_lt(padidx, cut_off);

	/* Initialize buffers because we will XOR into them */
	memset(ctx->buf + index, 0, HASHED(BLOCK_SIZE) - index);
	memset(out, 0, HASHED(DIGEST_SIZE));
	pos = 0;

	/* Iterate based on the full buffer length, regardless of n, and include
	 * the maximum overhead with padding and size bytes
	 */
	while (pos < len + HASHED(BLOCK_SIZE) + sizeof(bits)) {
		uint8_t b;

		/* Take as many bytes from the input buffer as possible */
		if (pos < len)
			b = *(data++) & (uint8_t) constant_time_lt(pos, n);
		else
			b = 0;

		/* First padding byte */
		b |= 0x80 & (uint8_t) constant_time_eq(pos, n);

		/* Size bytes are always at the end of a block */
		if (index >= cut_off)
			b |= p[index - cut_off] & (uint8_t) constant_time_ge(pos, n + padlen);

		/* Store this byte into the buffer */
		ctx->buf[index++] ^= b;
		pos++;

		/* Process a full block, at a boundary which is independent from n */
		if (index >= HASHED(BLOCK_SIZE)) {
			index = 0;
			HASHED_LOWER(do_chunk)(ctx, (void *) ctx->buf);
			memset(ctx->buf, 0, HASHED(BLOCK_SIZE));

			/* Try to store the result: this is a no-op except when we reach the
			 * actual size based on n, more iterations may continue after that
			 * when len is really larger
			 */
			out_mask = constant_time_eq(pos, n + padlen + sizeof(bits));
			CRYPTONITE_HASHED(select_digest)(ctx, out, out_mask);
		}
	}
}
