/*
 * Copyright (c) 2014 Vincent Hanquez <vincent@snarc.org>
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
#ifndef CRYPTONITE_CHACHA
#define CRYPTONITE_CHACHA

typedef union {
	uint64_t q[8];
	uint32_t d[16];
	uint8_t  b[64];
} block;

typedef block cryptonite_chacha_state;

typedef struct {
	cryptonite_chacha_state st;
	uint8_t prev[64];
	uint8_t prev_ofs;
	uint8_t prev_len;
	uint8_t nb_rounds;
} cryptonite_chacha_context;

void cryptonite_chacha_init_core(cryptonite_chacha_state *st, uint32_t keylen, const uint8_t *key, uint32_t ivlen, const uint8_t *iv);
void cryptonite_chacha_init(cryptonite_chacha_context *ctx, uint8_t nb_rounds, uint32_t keylen, const uint8_t *key, uint32_t ivlen, const uint8_t *iv);
void cryptonite_chacha_combine(uint8_t *dst, cryptonite_chacha_context *st, const uint8_t *src, uint32_t bytes);
void cryptonite_chacha_generate(uint8_t *dst, cryptonite_chacha_context *st, uint32_t bytes);

#endif
