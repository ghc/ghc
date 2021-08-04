/*
 * Copyright (C) 2006-2010 Vincent Hanquez <vincent@snarc.org>
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
#ifndef CRYPTOHASH_SKEIN_H
#define CRYPTOHASH_SKEIN_H

#define SKEIN_VERSION 1ULL
#define SKEIN_IDSTRING 0x33414853ULL

/*
t0 0-63 || t1 64-127
0-95      96-111      112-118    119     120-125  126    127
Position  reserved    TreeLevel  BitPad  Type     First  Final
*/
#define FLAG_FIRST   (1ULL << 62)
#define FLAG_FINAL   (1ULL << 63)
#define FLAG_TYPE(x) (((uint64_t) ((x) & 0x3f)) << 56)

#define TYPE_KEY     0x00
#define TYPE_CFG     0x04
#define TYPE_MSG     0x30
#define TYPE_OUT     0x3f

#define SET_TYPE(ctx, ty) ctx->t0 = 0; ctx->t1 = (ty)

#endif
