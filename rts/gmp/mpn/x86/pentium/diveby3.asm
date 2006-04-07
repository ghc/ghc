dnl  Intel P5 mpn_divexact_by3 -- mpn division by 3, expecting no remainder.
dnl       
dnl  P5: 15.0 cycles/limb


dnl  Copyright (C) 2000 Free Software Foundation, Inc.
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


C mp_limb_t mpn_divexact_by3c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                              mp_limb_t carry);

defframe(PARAM_CARRY,16)
defframe(PARAM_SIZE, 12)
defframe(PARAM_SRC,   8)
defframe(PARAM_DST,   4)

dnl  multiplicative inverse of 3, modulo 2^32
deflit(INVERSE_3,        0xAAAAAAAB)

dnl  ceil(b/3), ceil(b*2/3) and floor(b*2/3) where b=2^32
deflit(ONE_THIRD_CEIL,   0x55555556)
deflit(TWO_THIRDS_CEIL,  0xAAAAAAAB)
deflit(TWO_THIRDS_FLOOR, 0xAAAAAAAA)

	.text
	ALIGN(8)

PROLOGUE(mpn_divexact_by3c)
deflit(`FRAME',0)

	movl	PARAM_SRC, %ecx
	movl	PARAM_SIZE, %edx

	decl	%edx
	jnz	L(two_or_more)

	movl	(%ecx), %edx
	movl	PARAM_CARRY, %eax	C risk of cache bank clash here

	movl	PARAM_DST, %ecx
	subl	%eax, %edx

	sbbl	%eax, %eax		C 0 or -1

	imull	$INVERSE_3, %edx, %edx

	negl	%eax			C 0 or 1
	cmpl	$ONE_THIRD_CEIL, %edx

	sbbl	$-1, %eax		C +1 if edx>=ceil(b/3)
	cmpl	$TWO_THIRDS_CEIL, %edx

	sbbl	$-1, %eax		C +1 if edx>=ceil(b*2/3)
	movl	%edx, (%ecx)

	ret


L(two_or_more):
	C eax
	C ebx
	C ecx	src
	C edx	size-1
	C esi
	C edi
	C ebp

	pushl	%ebx	FRAME_pushl()
	pushl	%esi	FRAME_pushl()

	pushl	%edi	FRAME_pushl()
	pushl	%ebp	FRAME_pushl()

	movl	PARAM_DST, %edi
	movl	PARAM_CARRY, %esi

	movl	(%ecx), %eax		C src low limb
	xorl	%ebx, %ebx

	sub	%esi, %eax
	movl	$TWO_THIRDS_FLOOR, %esi

	leal	(%ecx,%edx,4), %ecx	C &src[size-1]
	leal	(%edi,%edx,4), %edi	C &dst[size-1]

	adcl	$0, %ebx		C carry, 0 or 1
	negl	%edx			C -(size-1)


C The loop needs a source limb ready at the top, which leads to one limb
C handled separately at the end, and the special case above for size==1.
C There doesn't seem to be any scheduling that would keep the speed but move
C the source load and carry subtract up to the top.
C
C The destination cache line prefetching adds 1 cycle to the loop but is
C considered worthwhile.  The slowdown is a factor of 1.07, but will prevent
C repeated write-throughs if the destination isn't in L1.  A version using
C an outer loop to prefetch only every 8 limbs (a cache line) proved to be
C no faster, due to unavoidable branch mispreditions in the inner loop.
C
C setc is 2 cycles on P54, so an adcl is used instead.  If the movl $0,%ebx
C could be avoided then the src limb fetch could pair up and save a cycle.
C This would probably mean going to a two limb loop with the carry limb
C alternately positive or negative, since an sbbl %ebx,%ebx will leave a
C value which is in the opposite sense to the preceding sbbl/adcl %ebx,%eax.
C
C A register is used for TWO_THIRDS_FLOOR because a cmp can't be done as
C "cmpl %edx, $n" with the immediate as the second operand.
C
C The "4" source displacement is in the loop rather than the setup because
C this gets L(top) aligned to 8 bytes at no cost.

	ALIGN(8)
L(top):
	C eax	source limb, carry subtracted
	C ebx	carry (0 or 1)
	C ecx	&src[size-1]
	C edx	counter, limbs, negative
	C esi	TWO_THIRDS_FLOOR
	C edi	&dst[size-1]
	C ebp	scratch (result limb)

	imull	$INVERSE_3, %eax, %ebp

	cmpl	$ONE_THIRD_CEIL, %ebp
	movl	(%edi,%edx,4), %eax	C dst cache line prefetch

	sbbl	$-1, %ebx		C +1 if ebp>=ceil(b/3)
	cmpl	%ebp, %esi

	movl	4(%ecx,%edx,4), %eax	C next src limb

	sbbl	%ebx, %eax		C and further -1 if ebp>=ceil(b*2/3)
	movl	$0, %ebx

	adcl	$0, %ebx		C new carry
	movl	%ebp, (%edi,%edx,4)

	incl	%edx
	jnz	L(top)



	imull	$INVERSE_3, %eax, %edx

	cmpl	$ONE_THIRD_CEIL, %edx
	movl	%edx, (%edi)

	sbbl	$-1, %ebx	C +1 if edx>=ceil(b/3)
	cmpl	$TWO_THIRDS_CEIL, %edx

	sbbl	$-1, %ebx	C +1 if edx>=ceil(b*2/3)
	popl	%ebp

	movl	%ebx, %eax
	popl	%edi

	popl	%esi
	popl	%ebx

	ret

EPILOGUE()
