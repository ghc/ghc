/* HS_MEMCMP_INTRO_BEGIN */
/* */
/* Copyright (c) 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group */
/* SPDX-License-Identifier: BSD-3-Clause */
/* */
/* AArch64 little-endian memcmp implementation for the GHC RTS */
/* (hs_memcmp_simd). */
/* */
/* Adapted from a musl libc memcmp patch by the same author (original MIT */
/* notice retained below); RTS renaming and guards under BSD-3-Clause. */
/* NEON, ARMv8-A baseline, no IFUNC. All reads are strictly bounded to */
/* [0, n); n == 0 returns 0 without dereferencing either pointer. Returns the */
/* exact unsigned byte difference of the first differing bytes. */
/* HS_MEMCMP_INTRO_END */

#include "ghcconfig.h"

#if (defined(linux_HOST_OS) && defined(aarch64_HOST_ARCH) && !defined(WORDS_BIGENDIAN)) || (defined(__ELF__) && defined(__aarch64__) && !defined(__AARCH64EB__))
	.text
/* MIT License */
/* */
/* Copyright (c) 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group */
/* */
/* Permission is hereby granted, free of charge, to any person obtaining a copy */
/* of this software and associated documentation files (the "Software"), to deal */
/* in the Software without restriction, including without limitation the rights */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell */
/* copies of the Software, and to permit persons to whom the Software is */
/* furnished to do so, subject to the following conditions: */
/* */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software. */
/* */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE */
/* SOFTWARE. */
/* */
/* memcmp for musl libc, AArch64 (NEON, ARMv8-A baseline). */
/* */
/* No IFUNC, no target_clones: one straight-line NEON implementation valid on */
/* every ARMv8-A core. */
/* */
/* Safety invariants: */
/* - n == 0 returns 0 without dereferencing either pointer. */
/* - 1 <= n < 16 uses overlapping bounded scalar loads (64/32/16/8-bit pairs */
/* at offsets 0 and n-width, rev for unsigned order, clz for the first */
/* differing byte), so every load satisfies offset + width <= n. */
/* - n >= 16 processes 64B blocks (ldp q pairs) and 16B blocks with */
/* offset + width <= n plus one final overlapping 16B vector, so no read */
/* ever crosses the object bounds (and therefore no page-crossing fault */
/* is possible on mapped inputs). */
/* The result is the exact unsigned lexicographic difference of the first */
/* differing bytes (*s1 - *s2), matching the generic C memcmp. */
/* */
/* Calling convention (AAPCS64): X0 = s1, X1 = s2, X2 = n. */
/* Returns W0 = sign-correct unsigned byte difference, or 0. */

	.arch armv8-a
	.text
	.p2align 2
	.globl hs_memcmp_simd
	.hidden hs_memcmp_simd
	.type hs_memcmp_simd, %function
hs_memcmp_simd:
	cbz x2, .Lret0
	cmp x2, #16
	b.lo .Lsmall
	/* x0 = s1 cursor, x1 = s2 cursor, x2 = remaining */
	b .Ltest64
.Lloop64:	/* 64B per iteration via ldp q pairs */
	ldp q0, q1, [x0, #0]
	ldp q2, q3, [x0, #32]
	ldp q4, q5, [x1, #0]
	ldp q6, q7, [x1, #32]
	cmeq v0.16b, v0.16b, v4.16b
	shrn v0.8b, v0.8h, #4	/* nibble mask: 0xf per equal source byte */
	fmov x4, d0
	cmp x4, #-1
	b.ne .Lres0
	cmeq v1.16b, v1.16b, v5.16b
	shrn v1.8b, v1.8h, #4
	fmov x4, d1
	cmp x4, #-1
	b.ne .Lres16
	cmeq v2.16b, v2.16b, v6.16b
	shrn v2.8b, v2.8h, #4
	fmov x4, d2
	cmp x4, #-1
	b.ne .Lres32
	cmeq v3.16b, v3.16b, v7.16b
	shrn v3.8b, v3.8h, #4
	fmov x4, d3
	cmp x4, #-1
	b.ne .Lres48
	add x0, x0, #64
	add x1, x1, #64
	sub x2, x2, #64
.Ltest64:
	cmp x2, #64
	b.hs .Lloop64
	b .Ltest16
.Lloop16:
	ldr q0, [x0]
	ldr q1, [x1]
	cmeq v0.16b, v0.16b, v1.16b
	shrn v0.8b, v0.8h, #4
	fmov x4, d0
	cmp x4, #-1
	b.ne .Lres0
	add x0, x0, #16
	add x1, x1, #16
	sub x2, x2, #16
.Ltest16:
	cmp x2, #16
	b.hs .Lloop16
	cbz x2, .Lret0
	/* Overlapping final 16B at (cursor + remaining - 16) covers the tail. */
	sub x4, x2, #16
	add x0, x0, x4
	add x1, x1, x4
	ldr q0, [x0]
	ldr q1, [x1]
	cmeq v0.16b, v0.16b, v1.16b
	shrn v0.8b, v0.8h, #4
	fmov x4, d0
	cmp x4, #-1
	b.ne .Lresolve
.Lret0:
	mov w0, #0
	ret
.Lres16:
	add x0, x0, #16
	add x1, x1, #16
	b .Lresolve
.Lres32:
	add x0, x0, #32
	add x1, x1, #32
	b .Lresolve
.Lres48:
	add x0, x0, #48
	add x1, x1, #48
	b .Lresolve
.Lres0:
	b .Lresolve
.Lresolve:	/* x0/x1 = 16B chunk base, x4 = nibble mask */
	mvn x4, x4
	rbit x4, x4
	clz x4, x4
	lsr x4, x4, #2	/* first differing byte index 0..15 */
	ldrb w5, [x0, x4]
	ldrb w6, [x1, x4]
	sub w0, w5, w6
	ret

/* ---- Small sizes, 1 <= n < 16: overlapping bounded scalar loads ---- */
.Lsmall:
	cmp x2, #8
	b.lo .Ls4
	ldr x4, [x0]
	ldr x5, [x1]
	cmp x4, x5
	b.ne .Ls8_d0
	cmp x2, #8
	b.eq .Lret0
	sub x6, x2, #8	/* pair offset n-8 */
	ldr x4, [x0, x6]
	ldr x5, [x1, x6]
	cmp x4, x5
	b.eq .Lret0
	b .Ls8_res
.Ls8_d0:
	mov x6, #0	/* pair offset 0 */
.Ls8_res:	/* x4/x5 = u64 pair, x6 = pair offset */
	rev x4, x4
	rev x5, x5
	eor x4, x4, x5
	clz x4, x4
	lsr x4, x4, #3	/* first differing byte index 0..7 */
	add x4, x4, x6
	ldrb w5, [x0, x4]
	ldrb w6, [x1, x4]
	sub w0, w5, w6
	ret
.Ls4:	/* 1 <= n <= 7 */
	cmp x2, #4
	b.lo .Ls2
	ldr w4, [x0]
	ldr w5, [x1]
	cmp w4, w5
	b.ne .Ls4_d0
	cmp x2, #4
	b.eq .Lret0
	sub x6, x2, #4	/* pair offset n-4 */
	ldr w4, [x0, x6]
	ldr w5, [x1, x6]
	cmp w4, w5
	b.eq .Lret0
	b .Ls4_res
.Ls4_d0:
	mov x6, #0
.Ls4_res:	/* w4/w5 = u32 pair, x6 = pair offset */
	rev w4, w4
	rev w5, w5
	eor w4, w4, w5
	clz w4, w4
	lsr w4, w4, #3	/* first differing byte index 0..3 */
	add x4, x4, x6
	ldrb w5, [x0, x4]
	ldrb w6, [x1, x4]
	sub w0, w5, w6
	ret
.Ls2:	/* 1 <= n <= 3 */
	cmp x2, #2
	b.lo .Ls1
	ldrh w4, [x0]
	ldrh w5, [x1]
	cmp w4, w5
	b.ne .Ls2_d0
	cmp x2, #2
	b.eq .Lret0
	sub x6, x2, #2	/* pair offset n-2 */
	ldrh w4, [x0, x6]
	ldrh w5, [x1, x6]
	cmp w4, w5
	b.eq .Lret0
	b .Ls2_res
.Ls2_d0:
	mov x6, #0
.Ls2_res:	/* w4/w5 = u16 pair, x6 = pair offset */
	rev16 w4, w4
	rev16 w5, w5
	eor w4, w4, w5
	clz w4, w4
	sub w4, w4, #16
	lsr w4, w4, #3	/* first differing byte index 0..1 */
	add x4, x4, x6
	ldrb w5, [x0, x4]
	ldrb w6, [x1, x4]
	sub w0, w5, w6
	ret
.Ls1:	/* n == 1 */
	ldrb w4, [x0]
	ldrb w5, [x1]
	sub w0, w4, w5
	ret

	.size hs_memcmp_simd,.-hs_memcmp_simd

#endif /* aarch64-le ELF / Linux */

#if defined(linux_HOST_OS) || defined(__ELF__)
	.section .note.GNU-stack,"",%progbits
#endif
