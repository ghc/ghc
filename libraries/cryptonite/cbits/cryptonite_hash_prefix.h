/*
 * Copyright (C) 2020 Olivier Chéron <olivier.cheron@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *	notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *	notice, this list of conditions and the following disclaimer in the
 *	documentation and/or other materials provided with the distribution.
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

#ifndef CRYPTONITE_HASH_PREFIX_H
#define CRYPTONITE_HASH_PREFIX_H

#include <stdint.h>

static inline uint32_t constant_time_msb(uint32_t a)
{
	return 0 - (a >> 31);
}

static inline uint32_t constant_time_lt(uint32_t a, uint32_t b)
{
	return constant_time_msb(a ^ ((a ^ b) | ((a - b) ^ b)));
}

static inline uint32_t constant_time_ge(uint32_t a, uint32_t b)
{
	return ~constant_time_lt(a, b);
}

static inline uint32_t constant_time_is_zero(uint32_t a)
{
	return constant_time_msb(~a & (a - 1));
}

static inline uint32_t constant_time_eq(uint32_t a, uint32_t b)
{
	return constant_time_is_zero(a ^ b);
}

static inline uint64_t constant_time_msb_64(uint64_t a)
{
	return 0 - (a >> 63);
}

static inline uint64_t constant_time_lt_64(uint64_t a, uint64_t b)
{
	return constant_time_msb_64(a ^ ((a ^ b) | ((a - b) ^ b)));
}

#endif
