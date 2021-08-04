/*
 * Copyright (c) 2016 Brandon Hamilton <brandon.hamilton@gmail.com>
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
#include <stdint.h>
#include <string.h>
#include "cryptonite_xsalsa.h"
#include "cryptonite_align.h"
#include "cryptonite_bitfn.h"

/* XSalsa20 algorithm as described in https://cr.yp.to/snuffle/xsalsa-20081128.pdf */
void cryptonite_xsalsa_init(cryptonite_salsa_context *ctx, uint8_t nb_rounds,
                            uint32_t keylen, const uint8_t *key,
                            uint32_t ivlen, const uint8_t *iv)
{
  memset(ctx, 0, sizeof(*ctx));
  ctx->nb_rounds = nb_rounds;

  /* Create initial 512-bit input block:
       (x0, x5, x10, x15) is the Salsa20 constant
       (x1, x2, x3, x4, x11, x12, x13, x14) is a 256-bit key
       (x6, x7, x8, x9) is the first 128 bits of a 192-bit nonce
  */
  cryptonite_salsa_init_core(&ctx->st, keylen, key, 8, iv);

  /* Continue initialization in a separate function that may also
     be called independently */
  cryptonite_xsalsa_derive(ctx, ivlen - 8, iv + 8);
}

void cryptonite_xsalsa_derive(cryptonite_salsa_context *ctx,
                              uint32_t ivlen, const uint8_t *iv)
{
  /* Finish creating initial 512-bit input block:
       (x6, x7, x8, x9) is the first 128 bits of a 192-bit nonce

     Except iv has been shifted by 64 bits so there are now only 128 bits ahead.
  */
  ctx->st.d[ 8] += load_le32(iv + 0);
  ctx->st.d[ 9] += load_le32(iv + 4);

  /* Compute (z0, z1, . . . , z15) = doubleround ^(r/2) (x0, x1, . . . , x15) */
  block hSalsa;
  memset(&hSalsa, 0, sizeof(block));
  cryptonite_salsa_core_xor(ctx->nb_rounds, &hSalsa, &ctx->st);
 
  /* Build a new 512-bit input block (x′0, x′1, . . . , x′15):
       (x′0, x′5, x′10, x′15) is the Salsa20 constant
       (x′1,x′2,x′3,x′4,x′11,x′12,x′13,x′14) = (z0,z5,z10,z15,z6,z7,z8,z9)
       (x′6,x′7) is the last 64 bits of the 192-bit nonce
       (x′8, x′9) is a 64-bit block counter.
  */
  ctx->st.d[ 1] = hSalsa.d[ 0] - ctx->st.d[ 0];
  ctx->st.d[ 2] = hSalsa.d[ 5] - ctx->st.d[ 5];
  ctx->st.d[ 3] = hSalsa.d[10] - ctx->st.d[10];
  ctx->st.d[ 4] = hSalsa.d[15] - ctx->st.d[15];
  ctx->st.d[11] = hSalsa.d[ 6] - ctx->st.d[ 6];
  ctx->st.d[12] = hSalsa.d[ 7] - ctx->st.d[ 7];
  ctx->st.d[13] = hSalsa.d[ 8] - ctx->st.d[ 8];
  ctx->st.d[14] = hSalsa.d[ 9] - ctx->st.d[ 9];
  ctx->st.d[ 6] = load_le32(iv + 8);
  ctx->st.d[ 7] = load_le32(iv + 12);
  ctx->st.d[ 8] = 0;
  ctx->st.d[ 9] = 0;
}
