dnl  x86 mpn_divexact_by3 -- mpn division by 3, expecting no remainder.


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


dnl  The following all have their own optimized versions of this routine,
dnl  but for reference the code here runs as follows.
dnl
dnl       cycles/limb
dnl  P54     18.0
dnl  P55     17.0
dnl  P6      14.5
dnl  K6      14.0
dnl  K7      10.0


include(`../config.m4')


C mp_limb_t mpn_divexact_by3c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                              mp_limb_t carry);

defframe(PARAM_CARRY,16)
defframe(PARAM_SIZE, 12)
defframe(PARAM_SRC,   8)
defframe(PARAM_DST,   4)

dnl  multiplicative inverse of 3, modulo 2^32
deflit(INVERSE_3,       0xAAAAAAAB)

dnl  ceil(b/3) and ceil(b*2/3) where b=2^32
deflit(ONE_THIRD_CEIL,  0x55555556)
deflit(TWO_THIRDS_CEIL, 0xAAAAAAAB)

	.text
	ALIGN(8)

PROLOGUE(mpn_divexact_by3c)
deflit(`FRAME',0)

	movl	PARAM_SRC, %ecx
	pushl	%ebp		FRAME_pushl()

	movl	PARAM_SIZE, %ebp
	pushl	%edi		FRAME_pushl()

	movl	PARAM_DST, %edi
	pushl	%esi		FRAME_pushl()

	movl	$INVERSE_3, %esi
	pushl	%ebx		FRAME_pushl()

	leal	(%ecx,%ebp,4), %ecx
	movl	PARAM_CARRY, %ebx

	leal	(%edi,%ebp,4), %edi
	negl	%ebp


	ALIGN(8)
L(top):
	C eax	scratch, low product
	C ebx	carry limb (0 to 3)
	C ecx	&src[size]
	C edx	scratch, high product
	C esi	multiplier
	C edi	&dst[size]
	C ebp	counter, limbs, negative

	movl	(%ecx,%ebp,4), %eax

	subl	%ebx, %eax

	setc	%bl

	imull	%esi

	cmpl	$ONE_THIRD_CEIL, %eax
	movl	%eax, (%edi,%ebp,4)

	sbbl	$-1, %ebx	C +1 if eax>=ceil(b/3)
	cmpl	$TWO_THIRDS_CEIL, %eax

	sbbl	$-1, %ebx	C +1 if eax>=ceil(b*2/3)
	incl	%ebp

	jnz	L(top)


	movl	%ebx, %eax
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret

EPILOGUE()
