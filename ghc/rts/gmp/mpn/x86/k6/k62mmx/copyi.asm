dnl  AMD K6-2 mpn_copyi -- copy limb vector, incrementing.
dnl 
dnl  K6-2: 0.56 or 1.0 cycles/limb (at 32 limbs/loop), depending on data
dnl  alignment.


dnl  Copyright (C) 1999, 2000 Free Software Foundation, Inc.
dnl 
dnl  This file is part of the GNU MP Library.
dnl 
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl 
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl 
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 59 Temple Place -
dnl  Suite 330, Boston, MA 02111-1307, USA.


include(`../config.m4')


dnl  K6-2 aligned:
dnl  UNROLL_COUNT cycles/limb
dnl        8          0.75
dnl       16          0.625
dnl       32          0.5625
dnl       64          0.53
dnl  Maximum possible with the current code is 64, the minimum is 2.

deflit(UNROLL_COUNT, 32)


C void mpn_copyi (mp_ptr dst, mp_srcptr src, mp_size_t size);
C
C The MMX loop is faster than a rep movs when src and dst are both 0mod8.
C With one 0mod8 and one 4mod8 it's 1.056 c/l and the rep movs at 1.0 c/l is
C used instead.
C
C         mod8
C	src  dst
C	 0    0	   both aligned, use mmx
C	 0    4    unaligned, use rep movs
C	 4    0    unaligned, use rep movs
C	 4    4    do one movs, then both aligned, use mmx
C
C The MMX code on aligned data is 0.5 c/l, plus loop overhead of 2
C cycles/loop, which is 0.0625 c/l at 32 limbs/loop.
C
C A pattern of two movq loads and two movq stores (or four and four) was
C tried, but found to be the same speed as just one of each.
C
C Note that this code only suits K6-2 and K6-3.  Plain K6 does only one mmx
C instruction per cycle, so "movq"s are no faster than the simple 1 c/l rep
C movs.
C
C Enhancement:
C
C Addressing modes like disp(%esi,%ecx,4) aren't currently used.  They'd
C make it possible to avoid incrementing %esi and %edi in the loop and hence
C get loop overhead down to 1 cycle.  Care would be needed to avoid bad
C cache line crossings since the "movq"s would then be 5 code bytes rather
C than 4.


defframe(PARAM_SIZE,12)
defframe(PARAM_SRC, 8)
defframe(PARAM_DST, 4)
deflit(`FRAME',0)

	.text
	ALIGN(32)

PROLOGUE(mpn_copyi)
	movl	PARAM_SIZE, %ecx
	movl	%esi, %eax

	movl	PARAM_SRC, %esi
	movl	%edi, %edx

	cld

	movl	PARAM_DST, %edi
	cmpl	$UNROLL_COUNT, %ecx

	ja	L(unroll)

L(simple):
	rep
	movsl

	movl	%eax, %esi
	movl	%edx, %edi

	ret


L(unroll):
	C if src and dst are different alignments mod8, then use rep movs
	C if src and dst are both 4mod8 then process one limb to get 0mod8

	pushl	%ebx
	leal	(%esi,%edi), %ebx

	testb	$4, %bl
	popl	%ebx
	
	jnz	L(simple)
	testl	$4, %esi

	leal	-UNROLL_COUNT(%ecx), %ecx
	jz	L(already_aligned)

	decl	%ecx

	movsl
L(already_aligned):


ifelse(UNROLL_BYTES,256,`
	addl	$128, %esi
	addl	$128, %edi
')

	C this is offset 0x34, no alignment needed
L(top):
	C eax	saved esi
	C ebx
	C ecx	counter, limbs
	C edx	saved edi
	C esi	src, incrementing
	C edi	dst, incrementing
	C ebp
	C
	C Zdisp gets 0(%esi) left that way to avoid vector decode, and with
	C 0(%edi) keeps code aligned to 16 byte boundaries.

deflit(CHUNK_COUNT, 2)
forloop(`i', 0, UNROLL_COUNT/CHUNK_COUNT-1, `
	deflit(`disp', eval(i*CHUNK_COUNT*4 ifelse(UNROLL_BYTES,256,-128)))
Zdisp(	movq,	disp,(%esi), %mm0)
Zdisp(	movq,	%mm0, disp,(%edi))
')

	addl	$UNROLL_BYTES, %esi
	subl	$UNROLL_COUNT, %ecx

	leal	UNROLL_BYTES(%edi), %edi
	jns	L(top)


	C now %ecx is -UNROLL_COUNT to -1 representing repectively 0 to
	C UNROLL_COUNT-1 limbs remaining

	testb	$eval(UNROLL_COUNT/2), %cl

	leal	UNROLL_COUNT(%ecx), %ecx
	jz	L(not_half)

	C at an unroll count of 32 this block of code is 16 cycles faster than
	C the rep movs, less 3 or 4 to test whether to do it

forloop(`i', 0, UNROLL_COUNT/CHUNK_COUNT/2-1, `
	deflit(`disp', eval(i*CHUNK_COUNT*4 ifelse(UNROLL_BYTES,256,-128)))
	movq	disp(%esi), %mm0
	movq	%mm0, disp(%edi)
')
	addl	$eval(UNROLL_BYTES/2), %esi
	addl	$eval(UNROLL_BYTES/2), %edi

	subl	$eval(UNROLL_COUNT/2), %ecx
L(not_half):


ifelse(UNROLL_BYTES,256,`
	subl	$128, %esi
	subl	$128, %edi
')

	rep
	movsl

	movl	%eax, %esi
	movl	%edx, %edi

	femms
	ret

EPILOGUE()
