/*
 * Copyright (c) 2012-2013 Vincent Hanquez <vincent@snarc.org>
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

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_block)(aes_block *out, aes_key *key, aes_block *in)
{
	__m128i *k = (__m128i *) key->data;
	PRELOAD_ENC(k);
	__m128i m = _mm_loadu_si128((__m128i *) in);
	DO_ENC_BLOCK(m);
	_mm_storeu_si128((__m128i *) out, m);
}

TARGET_AESNI
void SIZED(cryptonite_aesni_decrypt_block)(aes_block *out, aes_key *key, aes_block *in)
{
	__m128i *k = (__m128i *) key->data;
	PRELOAD_DEC(k);
	__m128i m = _mm_loadu_si128((__m128i *) in);
	DO_DEC_BLOCK(m);
	_mm_storeu_si128((__m128i *) out, m);
}

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_ecb)(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks)
{
	__m128i *k = (__m128i *) key->data;

	PRELOAD_ENC(k);
	for (; blocks-- > 0; in += 1, out += 1) {
		__m128i m = _mm_loadu_si128((__m128i *) in);
		DO_ENC_BLOCK(m);
		_mm_storeu_si128((__m128i *) out, m);
	}
}

TARGET_AESNI
void SIZED(cryptonite_aesni_decrypt_ecb)(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks)
{
	__m128i *k = (__m128i *) key->data;

	PRELOAD_DEC(k);

	for (; blocks-- > 0; in += 1, out += 1) {
		__m128i m = _mm_loadu_si128((__m128i *) in);
		DO_DEC_BLOCK(m);
		_mm_storeu_si128((__m128i *) out, m);
	}
}

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_cbc)(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks)
{
	__m128i *k = (__m128i *) key->data;
	__m128i iv = _mm_loadu_si128((__m128i *) _iv);

	PRELOAD_ENC(k);

	for (; blocks-- > 0; in += 1, out += 1) {
		__m128i m = _mm_loadu_si128((__m128i *) in);
		m = _mm_xor_si128(m, iv);
		DO_ENC_BLOCK(m);
		iv = m;
		_mm_storeu_si128((__m128i *) out, m);
	}
}

TARGET_AESNI
void SIZED(cryptonite_aesni_decrypt_cbc)(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks)
{
	__m128i *k = (__m128i *) key->data;
	__m128i iv = _mm_loadu_si128((__m128i *) _iv);

	PRELOAD_DEC(k);

	for (; blocks-- > 0; in += 1, out += 1) {
		__m128i m = _mm_loadu_si128((__m128i *) in);
		__m128i ivnext = m;

		DO_DEC_BLOCK(m);
		m = _mm_xor_si128(m, iv);

		_mm_storeu_si128((__m128i *) out, m);
		iv = ivnext;
	}
}

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_ctr)(uint8_t *output, aes_key *key, aes_block *_iv, uint8_t *input, uint32_t len)
{
	__m128i *k = (__m128i *) key->data;
	__m128i bswap_mask = _mm_setr_epi8(7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8);
	__m128i one        = _mm_set_epi32(0,1,0,0);
	uint32_t nb_blocks = len / 16;
	uint32_t part_block_len = len % 16;

	/* get the IV in little endian format */
	__m128i iv = _mm_loadu_si128((__m128i *) _iv);
	iv = _mm_shuffle_epi8(iv, bswap_mask);

	PRELOAD_ENC(k);

	for (; nb_blocks-- > 0; output += 16, input += 16) {
		/* put back the iv in big endian mode,
		 * encrypt it and and xor it the input block
		 */
		__m128i tmp = _mm_shuffle_epi8(iv, bswap_mask);
		DO_ENC_BLOCK(tmp);
		__m128i m = _mm_loadu_si128((__m128i *) input);
		m = _mm_xor_si128(m, tmp);

		_mm_storeu_si128((__m128i *) output, m);
		/* iv += 1 */
		iv = _mm_add_epi64(iv, one);
	}

	if (part_block_len != 0) {
		aes_block block;
		memset(&block.b, 0, 16);
		memcpy(&block.b, input, part_block_len);

		__m128i m = _mm_loadu_si128((__m128i *) &block);
		__m128i tmp = _mm_shuffle_epi8(iv, bswap_mask);

		DO_ENC_BLOCK(tmp);
		m = _mm_xor_si128(m, tmp);
		_mm_storeu_si128((__m128i *) &block.b, m);
		memcpy(output, &block.b, part_block_len);
	}

	return ;
}

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_c32_)(uint8_t *output, aes_key *key, aes_block *_iv, uint8_t *input, uint32_t len)
{
	__m128i *k = (__m128i *) key->data;
	__m128i one        = _mm_set_epi32(0,0,0,1);
	uint32_t nb_blocks = len / 16;
	uint32_t part_block_len = len % 16;

	/* get the IV */
	__m128i iv = _mm_loadu_si128((__m128i *) _iv);

	PRELOAD_ENC(k);

	for (; nb_blocks-- > 0; output += 16, input += 16) {
		/* encrypt the iv and and xor it the input block */
		__m128i tmp = iv;
		DO_ENC_BLOCK(tmp);
		__m128i m = _mm_loadu_si128((__m128i *) input);
		m = _mm_xor_si128(m, tmp);

		_mm_storeu_si128((__m128i *) output, m);
		/* iv += 1 */
		iv = _mm_add_epi32(iv, one);
	}

	if (part_block_len != 0) {
		aes_block block;
		memset(&block.b, 0, 16);
		memcpy(&block.b, input, part_block_len);

		__m128i m = _mm_loadu_si128((__m128i *) &block);
		__m128i tmp = iv;

		DO_ENC_BLOCK(tmp);
		m = _mm_xor_si128(m, tmp);
		_mm_storeu_si128((__m128i *) &block.b, m);
		memcpy(output, &block.b, part_block_len);
	}

	return ;
}

TARGET_AESNI
void SIZED(cryptonite_aesni_encrypt_xts)(aes_block *out, aes_key *key1, aes_key *key2,
                               aes_block *_tweak, uint32_t spoint, aes_block *in, uint32_t blocks)
{
	__m128i tweak = _mm_loadu_si128((__m128i *) _tweak);

	do {
		__m128i *k2 = (__m128i *) key2->data;
		PRELOAD_ENC(k2);
		DO_ENC_BLOCK(tweak);

		while (spoint-- > 0)
			tweak = gfmulx(tweak);
	} while (0) ;

	do {
		__m128i *k1 = (__m128i *) key1->data;
		PRELOAD_ENC(k1);

		for ( ; blocks-- > 0; in += 1, out += 1, tweak = gfmulx(tweak)) {
			__m128i m = _mm_loadu_si128((__m128i *) in);

			m = _mm_xor_si128(m, tweak);
			DO_ENC_BLOCK(m);
			m = _mm_xor_si128(m, tweak);

			_mm_storeu_si128((__m128i *) out, m);
		}
	} while (0);
}

TARGET_AESNI
void SIZED(cryptonite_aesni_gcm_encrypt)(uint8_t *output, aes_gcm *gcm, aes_key *key, uint8_t *input, uint32_t length)
{
	__m128i *k = (__m128i *) key->data;
	__m128i bswap_mask = _mm_setr_epi8(7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8);
	__m128i one        = _mm_set_epi32(0,1,0,0);
	uint32_t nb_blocks = length / 16;
	uint32_t part_block_len = length % 16;

	gcm->length_input += length;

	__m128i tag = _mm_loadu_si128((__m128i *) &gcm->tag);
	__m128i iv = _mm_loadu_si128((__m128i *) &gcm->civ);
	iv = _mm_shuffle_epi8(iv, bswap_mask);

	PRELOAD_ENC(k);

	for (; nb_blocks-- > 0; output += 16, input += 16) {
		/* iv += 1 */
		iv = _mm_add_epi32(iv, one);

		/* put back iv in big endian, encrypt it,
		 * and xor it to input */
		__m128i tmp = _mm_shuffle_epi8(iv, bswap_mask);
		DO_ENC_BLOCK(tmp);
		__m128i m = _mm_loadu_si128((__m128i *) input);
		m = _mm_xor_si128(m, tmp);

		tag = ghash_add(tag, gcm->htable, m);

		/* store it out */
		_mm_storeu_si128((__m128i *) output, m);
	}
	if (part_block_len > 0) {
		__m128i mask;
		aes_block block;
		/* FIXME could do something a bit more clever (slli & sub & and maybe) ... */
		switch (part_block_len) {
		case 1: mask = _mm_setr_epi8(0,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 2: mask = _mm_setr_epi8(0,1,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 3: mask = _mm_setr_epi8(0,1,2,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 4: mask = _mm_setr_epi8(0,1,2,3,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 5: mask = _mm_setr_epi8(0,1,2,3,4,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 6: mask = _mm_setr_epi8(0,1,2,3,4,5,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 7: mask = _mm_setr_epi8(0,1,2,3,4,5,6,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 8: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 9: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,0x80,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 10: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,0x80,0x80,0x80,0x80,0x80,0x80); break;
		case 11: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,0x80,0x80,0x80,0x80,0x80); break;
		case 12: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,0x80,0x80,0x80,0x80); break;
		case 13: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,12,0x80,0x80,0x80); break;
		case 14: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,12,13,0x80,0x80); break;
		case 15: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0x80); break;
		default: mask = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15); break;
		}
		block128_zero(&block);
		block128_copy_bytes(&block, input, part_block_len);

		/* iv += 1 */
		iv = _mm_add_epi32(iv, one);

		/* put back iv in big endian mode, encrypt it and xor it with input */
		__m128i tmp = _mm_shuffle_epi8(iv, bswap_mask);
		DO_ENC_BLOCK(tmp);

		__m128i m = _mm_loadu_si128((__m128i *) &block);
		m = _mm_xor_si128(m, tmp);
		m = _mm_shuffle_epi8(m, mask);

		tag = ghash_add(tag, gcm->htable, m);

		/* make output */
		_mm_storeu_si128((__m128i *) &block.b, m);
		memcpy(output, &block.b, part_block_len);
	}
	/* store back IV & tag */
	__m128i tmp = _mm_shuffle_epi8(iv, bswap_mask);
	_mm_storeu_si128((__m128i *) &gcm->civ, tmp);
	_mm_storeu_si128((__m128i *) &gcm->tag, tag);
}
