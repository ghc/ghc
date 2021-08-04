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

#ifndef AES_X86NI_H
#define AES_X86NI_H

#ifdef WITH_AESNI

#if defined(__i386__) || defined(__x86_64__)

#include <wmmintrin.h>
#include <tmmintrin.h>
#include <cryptonite_aes.h>
#include <aes/block128.h>

#ifdef WITH_TARGET_ATTRIBUTES
#define TARGET_AESNI __attribute__((target("ssse3,aes")))
#define TARGET_AESNI_PCLMUL __attribute__((target("sse4.1,aes,pclmul")))
#else
#define TARGET_AESNI
#define TARGET_AESNI_PCLMUL
#endif

#ifdef IMPL_DEBUG
TARGET_AESNI
static void block128_sse_print(__m128i m)
{
	block128 b;
	_mm_storeu_si128((__m128i *) &b.b, m);
	block128_print(&b);
}
#endif

void cryptonite_aesni_init(aes_key *key, uint8_t *origkey, uint8_t size);
void cryptonite_aesni_encrypt_block128(aes_block *out, aes_key *key, aes_block *in);
void cryptonite_aesni_encrypt_block256(aes_block *out, aes_key *key, aes_block *in);
void cryptonite_aesni_decrypt_block128(aes_block *out, aes_key *key, aes_block *in);
void cryptonite_aesni_decrypt_block256(aes_block *out, aes_key *key, aes_block *in);
void cryptonite_aesni_encrypt_ecb128(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks);
void cryptonite_aesni_encrypt_ecb256(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks);
void cryptonite_aesni_decrypt_ecb128(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks);
void cryptonite_aesni_decrypt_ecb256(aes_block *out, aes_key *key, aes_block *in, uint32_t blocks);
void cryptonite_aesni_encrypt_cbc128(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks);
void cryptonite_aesni_encrypt_cbc256(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks);
void cryptonite_aesni_decrypt_cbc128(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks);
void cryptonite_aesni_decrypt_cbc256(aes_block *out, aes_key *key, aes_block *_iv, aes_block *in, uint32_t blocks);
void cryptonite_aesni_encrypt_ctr128(uint8_t *out, aes_key *key, aes_block *_iv, uint8_t *in, uint32_t length);
void cryptonite_aesni_encrypt_ctr256(uint8_t *out, aes_key *key, aes_block *_iv, uint8_t *in, uint32_t length);
void cryptonite_aesni_encrypt_c32_128(uint8_t *out, aes_key *key, aes_block *_iv, uint8_t *in, uint32_t length);
void cryptonite_aesni_encrypt_c32_256(uint8_t *out, aes_key *key, aes_block *_iv, uint8_t *in, uint32_t length);
void cryptonite_aesni_encrypt_xts128(aes_block *out, aes_key *key1, aes_key *key2,
                           aes_block *_tweak, uint32_t spoint, aes_block *in, uint32_t blocks);
void cryptonite_aesni_encrypt_xts256(aes_block *out, aes_key *key1, aes_key *key2,
                           aes_block *_tweak, uint32_t spoint, aes_block *in, uint32_t blocks);

void cryptonite_aesni_gcm_encrypt128(uint8_t *out, aes_gcm *gcm, aes_key *key, uint8_t *in, uint32_t length);
void cryptonite_aesni_gcm_encrypt256(uint8_t *out, aes_gcm *gcm, aes_key *key, uint8_t *in, uint32_t length);

#ifdef WITH_PCLMUL
void cryptonite_aesni_init_pclmul(void);
void cryptonite_aesni_hinit_pclmul(table_4bit htable, const block128 *h);
void cryptonite_aesni_gf_mul_pclmul(block128 *a, const table_4bit htable);
#endif

#endif

#endif

#endif
