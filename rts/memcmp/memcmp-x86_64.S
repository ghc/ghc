/* HS_MEMCMP_INTRO_BEGIN */
/* */
/* Copyright (c) 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group */
/* SPDX-License-Identifier: BSD-3-Clause */
/* */
/* x86_64 memcmp implementation for the GHC RTS (hs_memcmp_simd). */
/* */
/* Adapted from a musl libc memcmp patch by the same author (original MIT */
/* notice retained below); RTS renaming and guards under BSD-3-Clause. */
/* SSE2 baseline with a runtime-dispatched AVX2 fast path, no IFUNC, */
/* thread-safe one-byte dispatch cache. All reads are strictly bounded to */
/* [0, n); n == 0 returns 0 without dereferencing either pointer. Returns the */
/* exact unsigned byte difference of the first differing bytes. */
/* HS_MEMCMP_INTRO_END */

#include "ghcconfig.h"

#if (defined(linux_HOST_OS) && defined(x86_64_HOST_ARCH)) || (defined(__ELF__) && defined(__x86_64__))
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
/* memcmp for musl libc, x86_64. */
/* */
/* SSE2 baseline with an AVX2 fast path selected at run time. No IFUNC, no */
/* target_clones: a single thread-safe .bss cache byte (0 = unknown, */
/* 1 = SSE2 only, 2 = AVX2) records CPUID/XGETBV detection, so the common */
/* case is one cached branch. Every AVX2-path return executes vzeroupper. */
/* */
/* Safety invariants: */
/* - n == 0 returns 0 without dereferencing either pointer. */
/* - 1 <= n < 16 uses overlapping bounded loads (8/4/2/1 byte pairs at */
/* offsets 0 and n-width) with bswap + unsigned compare, so every load */
/* satisfies offset + width <= n. */
/* - n >= 16/32/128 processes full blocks with offset + width <= n plus one */
/* final overlapping vector, so no read ever crosses the object bounds */
/* (and therefore no page-crossing fault is possible on mapped inputs). */
/* The result is the exact unsigned lexicographic difference of the first */
/* differing bytes (*s1 - *s2), matching the generic C memcmp. */
/* */
/* Calling convention (System V AMD64): RDI = s1, RSI = s2, RDX = n. */
/* Returns EAX = sign-correct unsigned byte difference, or 0. */

.text
.p2align 4
.globl hs_memcmp_simd
.hidden hs_memcmp_simd
.type hs_memcmp_simd, @function
hs_memcmp_simd:
	test %rdx,%rdx
	jz .Lret0
	movzbl hs_memcmp_simd_avx2_cache(%rip),%eax
	test %al,%al
	jnz .Lchecked
	/* Cold path: detect AVX2 support once. Byte-sized cache access is */
	/* atomic; concurrent threads may repeat detection but always store a */
	/* valid result, so no lock is needed. */
	push %rbx
	push %rdx	/* save n: cpuid writes ebx/ecx/edx, xgetbv writes edx:eax. */
					/* rdi/s1, rsi/s2 survive (untouched); r8 is caller-saved scratch. */
	xorl %eax,%eax
	cpuid	/* leaf 0: eax = max basic leaf (CPUID(1).EAX is the */
					/* processor signature, NOT the max leaf!) */
	mov %eax,%r8d	/* save max basic leaf in caller-saved r8d */
	mov $1,%eax
	cpuid	/* ecx: bit 27 OSXSAVE, bit 28 AVX */
	bt $27,%ecx
	jnc .Lnoavx
	bt $28,%ecx
	jnc .Lnoavx
	xor %ecx,%ecx
	xgetbv	/* xcr0 -> edx:eax; need SSE+AVX state */
	and $6,%eax
	cmp $6,%eax
	jne .Lnoavx
	cmp $7,%r8d	/* leaf 7 (AVX2 bit) must exist */
	jb .Lnoavx
	mov $7,%eax
	xor %ecx,%ecx
	cpuid	/* ebx bit 5 = AVX2 */
	bt $5,%ebx
	jnc .Lnoavx
	pop %rdx
	pop %rbx
	movb $2,hs_memcmp_simd_avx2_cache(%rip)
	jmp .Lavx2
.Lnoavx:
	pop %rdx
	pop %rbx
	movb $1,hs_memcmp_simd_avx2_cache(%rip)
	jmp .Lsse
.Lchecked:
	cmp $2,%al
	je .Lavx2

/* ---- SSE2 baseline (no tzcnt, no AVX) ---- */
.Lsse:
	cmp $16,%rdx
	jb .Lsmall
	xor %eax,%eax	/* i = block offset */
	mov %rdx,%rcx
	sub $16,%rcx	/* rcx = max full-block offset (n-16) */
	jmp .Lsse_test
.Lsse_loop:
	movdqu (%rdi,%rax),%xmm0
	movdqu (%rsi,%rax),%xmm1
	pcmpeqb %xmm1,%xmm0
	pmovmskb %xmm0,%r8d
	xor $0xffff,%r8d	/* diff mask, bit set = differing byte */
	jnz .Lsse_diff	/* (mask in r8d: rcx holds the n-16 bound) */
	add $16,%rax
.Lsse_test:
	cmp %rcx,%rax
	jbe .Lsse_loop
	cmp %rdx,%rax
	je .Lret0
	/* Overlapping final 16B at offset n-16 covers the tail. */
	mov %rcx,%rax
	movdqu (%rdi,%rax),%xmm0
	movdqu (%rsi,%rax),%xmm1
	pcmpeqb %xmm1,%xmm0
	pmovmskb %xmm0,%r8d
	xor $0xffff,%r8d
	jnz .Lsse_diff
.Lret0:
	xor %eax,%eax
	ret
.Lsse_diff:	/* r8d = diff mask (!=0), rax = block offset */
	bsfl %r8d,%r8d	/* first differing byte index (no tzcnt) */
	add %rax,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	ret

/* ---- Small sizes, 1 <= n < 16: overlapping bounded loads + bswap ---- */
.Lsmall:
	cmp $8,%rdx
	jae .Lsmall8
	cmp $4,%rdx
	jae .Lsmall4
	cmp $2,%rdx
	jae .Lsmall2
	movzbl (%rdi),%eax
	movzbl (%rsi),%edx
	sub %edx,%eax
	ret
.Lsmall8:	/* 8 <= n <= 15 */
	mov (%rdi),%rax
	mov (%rsi),%rcx
	cmp %rcx,%rax
	jne .Lsmall8_d0
	cmp $8,%rdx
	je .Lret0
	mov %rdx,%r8
	sub $8,%r8	/* pair offset n-8 */
	mov (%rdi,%r8),%rax
	mov (%rsi,%r8),%rcx
	cmp %rcx,%rax
	je .Lret0
	jmp .Lsmall8_res
.Lsmall8_d0:
	xor %r8d,%r8d	/* pair offset 0 */
.Lsmall8_res:	/* rax/rcx = u64 pair, r8 = pair offset */
	bswap %rax
	bswap %rcx
	xor %rcx,%rax
	bsr %rax,%rax	/* top differing bit (pair differs) */
	shr $3,%rax	/* byte position from top */
	xor $7,%rax	/* byte index 0..7 */
	add %r8,%rax
	movzbl (%rdi,%rax),%ecx
	movzbl (%rsi,%rax),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	ret
.Lsmall4:	/* 4 <= n <= 7 */
	mov (%rdi),%eax
	mov (%rsi),%ecx
	cmp %ecx,%eax
	jne .Lsmall4_d0
	cmp $4,%rdx
	je .Lret0
	mov %rdx,%r8
	sub $4,%r8	/* pair offset n-4 */
	mov (%rdi,%r8),%eax
	mov (%rsi,%r8),%ecx
	cmp %ecx,%eax
	je .Lret0
	jmp .Lsmall4_res
.Lsmall4_d0:
	xor %r8d,%r8d
.Lsmall4_res:	/* eax/ecx = u32 pair, r8 = pair offset */
	bswap %eax
	bswap %ecx
	xor %ecx,%eax
	bsr %eax,%eax
	shr $3,%eax
	xor $3,%eax	/* byte index 0..3 */
	add %r8,%rax
	movzbl (%rdi,%rax),%ecx
	movzbl (%rsi,%rax),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	ret
.Lsmall2:	/* 2 <= n <= 3 */
	movzwl (%rdi),%eax
	movzwl (%rsi),%ecx
	cmp %cx,%ax
	jne .Lsmall2_d0
	cmp $2,%rdx
	je .Lret0
	mov %rdx,%r8
	sub $2,%r8	/* pair offset n-2 */
	movzwl (%rdi,%r8),%eax
	movzwl (%rsi,%r8),%ecx
	cmp %cx,%ax
	je .Lret0
	jmp .Lsmall2_res
.Lsmall2_d0:
	xor %r8d,%r8d
.Lsmall2_res:	/* ax/cx = u16 pair, r8 = pair offset */
	xchg %al,%ah
	xchg %cl,%ch
	xor %ecx,%eax
	test $0xff00,%eax	/* first byte differs? */
	jnz .Lsmall2_first
	add $1,%r8	/* else second byte differs */
.Lsmall2_first:
	movzbl (%rdi,%r8),%ecx
	movzbl (%rsi,%r8),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	ret

/* ---- AVX2 fast path ---- */
.Lavx2:
	cmp $32,%rdx
	jb .Lavx_small
	xor %eax,%eax	/* i = block offset */
	mov %rdx,%rcx
	sub $32,%rcx	/* rcx = max full-block offset (n-32) */
	cmp $128,%rdx
	jb .Lavx32_test
.Lavx128_loop:	/* 4x32B per iteration */
	vmovdqu (%rdi,%rax),%ymm0
	vmovdqu (%rsi,%rax),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx_diff0
	vmovdqu 32(%rdi,%rax),%ymm0
	vmovdqu 32(%rsi,%rax),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx_diff32
	vmovdqu 64(%rdi,%rax),%ymm0
	vmovdqu 64(%rsi,%rax),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx_diff64
	vmovdqu 96(%rdi,%rax),%ymm0
	vmovdqu 96(%rsi,%rax),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx_diff96
	add $128,%rax
	mov %rdx,%r8
	sub %rax,%r8
	cmp $128,%r8
	jae .Lavx128_loop
	jmp .Lavx32_test
.Lavx32_loop:
	vmovdqu (%rdi,%rax),%ymm0
	vmovdqu (%rsi,%rax),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx32_diff
	add $32,%rax
.Lavx32_test:
	cmp %rcx,%rax
	jbe .Lavx32_loop
	cmp %rdx,%rax
	je .Lavx_ret0
	/* Overlapping final 32B at offset n-32 covers the tail. */
	vmovdqu (%rdi,%rcx),%ymm0
	vmovdqu (%rsi,%rcx),%ymm1
	vpcmpeqb %ymm1,%ymm0,%ymm0
	vpmovmskb %ymm0,%r8d
	xor $-1,%r8d
	jnz .Lavx_final_diff
.Lavx_ret0:
	vzeroupper
	xor %eax,%eax
	ret
.Lavx32_diff:	/* r8d = diff mask, rax = block offset */
	bsfl %r8d,%r8d
	add %rax,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_final_diff:	/* r8d = diff mask, rcx = n-32 */
	bsfl %r8d,%r8d
	add %rcx,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_diff0:	/* r8d = diff mask, rax = block offset */
	bsfl %r8d,%r8d
	add %rax,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_diff32:
	bsfl %r8d,%r8d
	add %rax,%r8
	add $32,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_diff64:
	bsfl %r8d,%r8d
	add %rax,%r8
	add $64,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_diff96:
	bsfl %r8d,%r8d
	add %rax,%r8
	add $96,%r8
	movzbl (%rdi,%r8),%eax
	movzbl (%rsi,%r8),%edx
	sub %edx,%eax
	vzeroupper
	ret

/* ---- Small sizes on the AVX2 path (SSE insns, vzeroupper on return) ---- */
.Lavx_small:	/* 1 <= n < 32 */
	cmp $16,%rdx
	jae .Lavx_small16
	cmp $8,%rdx
	jae .Lavx_sm8
	cmp $4,%rdx
	jae .Lavx_sm4
	cmp $2,%rdx
	jae .Lavx_sm2
	movzbl (%rdi),%eax
	movzbl (%rsi),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_small16:	/* 16 <= n <= 31 */
	movdqu (%rdi),%xmm0
	movdqu (%rsi),%xmm1
	pcmpeqb %xmm1,%xmm0
	pmovmskb %xmm0,%ecx
	xor $0xffff,%ecx
	jnz .Lavx_s16_diff0
	mov %rdx,%rax
	sub $16,%rax	/* rax = n-16, overlapping final block */
	movdqu (%rdi,%rax),%xmm0
	movdqu (%rsi,%rax),%xmm1
	pcmpeqb %xmm1,%xmm0
	pmovmskb %xmm0,%ecx
	xor $0xffff,%ecx
	jnz .Lavx_s16_difff
	vzeroupper
	xor %eax,%eax
	ret
.Lavx_s16_diff0:
	bsfl %ecx,%ecx
	movzbl (%rdi,%rcx),%eax
	movzbl (%rsi,%rcx),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_s16_difff:
	bsfl %ecx,%ecx
	add %rax,%rcx
	movzbl (%rdi,%rcx),%eax
	movzbl (%rsi,%rcx),%edx
	sub %edx,%eax
	vzeroupper
	ret
.Lavx_sm8:	/* 8 <= n <= 15 */
	mov (%rdi),%rax
	mov (%rsi),%rcx
	cmp %rcx,%rax
	jne .Lavx_sm8_d0
	cmp $8,%rdx
	je .Lavx_sm_ret0
	mov %rdx,%r8
	sub $8,%r8
	mov (%rdi,%r8),%rax
	mov (%rsi,%r8),%rcx
	cmp %rcx,%rax
	je .Lavx_sm_ret0
	jmp .Lavx_sm8_res
.Lavx_sm8_d0:
	xor %r8d,%r8d
.Lavx_sm8_res:
	bswap %rax
	bswap %rcx
	xor %rcx,%rax
	bsr %rax,%rax
	shr $3,%rax
	xor $7,%rax
	add %r8,%rax
	movzbl (%rdi,%rax),%ecx
	movzbl (%rsi,%rax),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	vzeroupper
	ret
.Lavx_sm4:	/* 4 <= n <= 7 */
	mov (%rdi),%eax
	mov (%rsi),%ecx
	cmp %ecx,%eax
	jne .Lavx_sm4_d0
	cmp $4,%rdx
	je .Lavx_sm_ret0
	mov %rdx,%r8
	sub $4,%r8
	mov (%rdi,%r8),%eax
	mov (%rsi,%r8),%ecx
	cmp %ecx,%eax
	je .Lavx_sm_ret0
	jmp .Lavx_sm4_res
.Lavx_sm4_d0:
	xor %r8d,%r8d
.Lavx_sm4_res:
	bswap %eax
	bswap %ecx
	xor %ecx,%eax
	bsr %eax,%eax
	shr $3,%eax
	xor $3,%eax
	add %r8,%rax
	movzbl (%rdi,%rax),%ecx
	movzbl (%rsi,%rax),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	vzeroupper
	ret
.Lavx_sm2:	/* 2 <= n <= 3 */
	movzwl (%rdi),%eax
	movzwl (%rsi),%ecx
	cmp %cx,%ax
	jne .Lavx_sm2_d0
	cmp $2,%rdx
	je .Lavx_sm_ret0
	mov %rdx,%r8
	sub $2,%r8
	movzwl (%rdi,%r8),%eax
	movzwl (%rsi,%r8),%ecx
	cmp %cx,%ax
	je .Lavx_sm_ret0
	jmp .Lavx_sm2_res
.Lavx_sm2_d0:
	xor %r8d,%r8d
.Lavx_sm2_res:
	xchg %al,%ah
	xchg %cl,%ch
	xor %ecx,%eax
	test $0xff00,%eax
	jnz .Lavx_sm2_first
	add $1,%r8
.Lavx_sm2_first:
	movzbl (%rdi,%r8),%ecx
	movzbl (%rsi,%r8),%eax
	sub %eax,%ecx
	mov %ecx,%eax
	vzeroupper
	ret
.Lavx_sm_ret0:
	vzeroupper
	xor %eax,%eax
	ret

.size hs_memcmp_simd,.-hs_memcmp_simd

.section .bss
hs_memcmp_simd_avx2_cache:
	.zero 1
	.type hs_memcmp_simd_avx2_cache,@object
	.size hs_memcmp_simd_avx2_cache,1


#endif /* x86_64 ELF / Linux */

#if defined(linux_HOST_OS) || defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif
