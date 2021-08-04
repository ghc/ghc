/*
 *	Copyright (C) 2008 Vincent Hanquez <tab@snarc.org>
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
 * AES implementation
 */
#ifndef CRYPTONITE_AES_H
#define CRYPTONITE_AES_H

#include <stdint.h>
#include "aes/block128.h"

typedef block128 aes_block;

/* size = 456 */
typedef struct {
	uint8_t nbr; /* number of rounds: 10 (128), 12 (192), 14 (256) */
	uint8_t strength; /* 128 = 0, 192 = 1, 256 = 2 */
	uint8_t _padding[6];
	uint8_t data[16*14*2];
} aes_key;

/* size = 19*16+2*8= 320 */
typedef struct {
	aes_block tag;
	aes_block htable[16];
	aes_block iv;
	aes_block civ;
	uint64_t length_aad;
	uint64_t length_input;
} aes_gcm;

/* size = 4*16+4*4= 80 */
typedef struct {
	aes_block xi;
	aes_block header_cbcmac;
	aes_block b0;
	aes_block nonce;
	uint32_t length_aad;
	uint32_t length_input;
	uint32_t length_M;
	uint32_t length_L;
} aes_ccm;

typedef struct {
	block128 offset_aad;
	block128 offset_enc;
	block128 sum_aad;
	block128 sum_enc;
	block128 lstar;
	block128 ldollar;
	block128 li[4];
} aes_ocb;

/* size = 17*16= 272 */
typedef struct {
	aes_block htable[16];
	aes_block s;
} aes_polyval;

/* in bytes: either 16,24,32 */
void cryptonite_aes_initkey(aes_key *ctx, uint8_t *key, uint8_t size);

void cryptonite_aes_encrypt(aes_block *output, aes_key *key, aes_block *input);
void cryptonite_aes_decrypt(aes_block *output, aes_key *key, aes_block *input);

void cryptonite_aes_encrypt_ecb(aes_block *output, aes_key *key, aes_block *input, uint32_t nb_blocks);
void cryptonite_aes_decrypt_ecb(aes_block *output, aes_key *key, aes_block *input, uint32_t nb_blocks);

void cryptonite_aes_encrypt_cbc(aes_block *output, aes_key *key, aes_block *iv, aes_block *input, uint32_t nb_blocks);
void cryptonite_aes_decrypt_cbc(aes_block *output, aes_key *key, aes_block *iv, aes_block *input, uint32_t nb_blocks);

void cryptonite_aes_gen_ctr(aes_block *output, aes_key *key, const aes_block *iv, uint32_t nb_blocks);
void cryptonite_aes_gen_ctr_cont(aes_block *output, aes_key *key, aes_block *iv, uint32_t nb_blocks);

void cryptonite_aes_encrypt_xts(aes_block *output, aes_key *key, aes_key *key2, aes_block *sector,
                     uint32_t spoint, aes_block *input, uint32_t nb_blocks);
void cryptonite_aes_decrypt_xts(aes_block *output, aes_key *key, aes_key *key2, aes_block *sector,
                     uint32_t spoint, aes_block *input, uint32_t nb_blocks);

void cryptonite_aes_gcm_init(aes_gcm *gcm, aes_key *key, uint8_t *iv, uint32_t len);
void cryptonite_aes_gcm_aad(aes_gcm *gcm, uint8_t *input, uint32_t length);
void cryptonite_aes_gcm_encrypt(uint8_t *output, aes_gcm *gcm, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_gcm_decrypt(uint8_t *output, aes_gcm *gcm, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_gcm_finish(uint8_t *tag, aes_gcm *gcm, aes_key *key);

void cryptonite_aes_ocb_init(aes_ocb *ocb, aes_key *key, uint8_t *iv, uint32_t len);
void cryptonite_aes_ocb_aad(aes_ocb *ocb, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ocb_encrypt(uint8_t *output, aes_ocb *ocb, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ocb_decrypt(uint8_t *output, aes_ocb *ocb, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ocb_finish(uint8_t *tag, aes_ocb *ocb, aes_key *key);

void cryptonite_aes_ccm_init(aes_ccm *ccm, aes_key *key, uint8_t *nonce, uint32_t len, uint32_t msg_size, int m, int l);
void cryptonite_aes_ccm_aad(aes_ccm *ccm, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ccm_encrypt(uint8_t *output, aes_ccm *ccm, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ccm_decrypt(uint8_t *output, aes_ccm *ccm, aes_key *key, uint8_t *input, uint32_t length);
void cryptonite_aes_ccm_finish(uint8_t *tag, aes_ccm *ccm, aes_key *key);

uint8_t *cryptonite_aes_cpu_init(void);

void cryptonite_aes_polyval_init(aes_polyval *ctx, const aes_block *h);
void cryptonite_aes_polyval_update(aes_polyval *ctx, const uint8_t *input, uint32_t length);
void cryptonite_aes_polyval_finalize(aes_polyval *ctx, aes_block *dst);

#endif
