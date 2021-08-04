/*
 * Copyright (C) Thomas DuBuisson
 * Copyright (C) 2013 Vincent Hanquez <tab@snarc.org>
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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int cryptonite_cpu_has_rdrand()
{
	uint32_t ax,bx,cx,dx,func=1;
#if defined(__PIC__) && defined(__i386__)
	__asm__ volatile ("mov %%ebx, %%edi;" "cpuid;" "xchgl %%ebx, %%edi;"
		: "=a" (ax), "=D" (bx), "=c" (cx), "=d" (dx) : "a" (func));
#else
	__asm__ volatile ("cpuid": "=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func));
#endif
	return (cx & 0x40000000);
}

/* inline encoding of 'rdrand %rax' to cover old binutils
 * - no inputs
 * - 'cc' to the clobber list as we modify condition code.
 * - output of rdrand in rax and have a 8 bit error condition
 */
#define inline_rdrand_rax(val, err) \
	asm(".byte 0x48,0x0f,0xc7,0xf0; setc %1" \
	   : "=a" (val), "=q" (err) \
	   : \
	   : "cc")

/* inline encoding of 'rdrand %eax' to cover old binutils
 * - no inputs
 * - 'cc' to the clobber list as we modify condition code.
 * - output of rdrand in eax and have a 8 bit error condition
 */
#define inline_rdrand_eax(val, err) \
	asm(".byte 0x0f,0xc7,0xf0; setc %1" \
	   : "=a" (val), "=q" (err) \
	   : \
	   : "cc")

#ifdef __x86_64__
# define RDRAND_SZ 8
# define RDRAND_T  uint64_t
#define inline_rdrand(val, err) err = cryptonite_rdrand_step(&val)
#else
# define RDRAND_SZ 4
# define RDRAND_T  uint32_t
#define inline_rdrand(val, err) err = cryptonite_rdrand_step(&val)
#endif

/* sadly many people are still using an old binutils,
 * leading to report that instruction is not recognized.
 */
#if 1
/* Returns 1 on success */
static inline int cryptonite_rdrand_step(RDRAND_T *buffer)
{
	unsigned char err;
	asm volatile ("rdrand %0; setc %1" : "=r" (*buffer), "=qm" (err));
	return (int) err;
}
#endif

/* Returns the number of bytes successfully generated */
int cryptonite_get_rand_bytes(uint8_t *buffer, size_t len)
{
	RDRAND_T tmp;
	int aligned = (intptr_t) buffer % RDRAND_SZ;
	int orig_len = len;
	int to_alignment = RDRAND_SZ - aligned;
	uint8_t ok;

	if (aligned != 0) {
		inline_rdrand(tmp, ok);
		if (!ok)
			return 0;
		memcpy(buffer, (uint8_t *) &tmp, to_alignment);
		buffer += to_alignment;
		len -= to_alignment;
	}

	for (; len >= RDRAND_SZ; buffer += RDRAND_SZ, len -= RDRAND_SZ) {
		inline_rdrand(tmp, ok);
		if (!ok)
			return (orig_len - len);
		*((RDRAND_T *) buffer) = tmp;
	}

	if (len > 0) {
		inline_rdrand(tmp, ok);
		if (!ok)
			return (orig_len - len);
		memcpy(buffer, (uint8_t *) &tmp, len);
	}
	return orig_len;
}
