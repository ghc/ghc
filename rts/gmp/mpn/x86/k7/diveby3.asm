dnl  AMD K7 mpn_divexact_by3 -- mpn division by 3, expecting no remainder.
dnl 
dnl  K7: 8.0 cycles/limb


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

dnl  ceil(b/3) and floor(b*2/3) where b=2^32
deflit(ONE_THIRD_CEIL,   0x55555556)
deflit(TWO_THIRDS_FLOOR, 0xAAAAAAAA)

	.text
	ALIGN(32)

PROLOGUE(mpn_divexact_by3c)
deflit(`FRAME',0)

	movl	PARAM_SRC, %ecx
	pushl	%ebx			defframe_pushl(SAVE_EBX)

	movl	PARAM_CARRY, %ebx
	pushl	%ebp			defframe_pushl(SAVE_EBP)

	movl	PARAM_SIZE, %ebp
	pushl	%edi			defframe_pushl(SAVE_EDI)

	movl	(%ecx), %eax		C src low limb
	pushl	%esi			defframe_pushl(SAVE_ESI)

	movl	PARAM_DST, %edi
	movl	$TWO_THIRDS_FLOOR, %esi
	leal	-4(%ecx,%ebp,4), %ecx	C &src[size-1]

	subl	%ebx, %eax

	setc	%bl
	decl	%ebp
	jz	L(last)

	leal	(%edi,%ebp,4), %edi	C &dst[size-1]
	negl	%ebp


	ALIGN(16)
L(top):
	C eax	src limb, carry subtracted
	C ebx	carry limb (0 or 1)
	C ecx	&src[size-1]
	C edx	scratch
	C esi	TWO_THIRDS_FLOOR
	C edi	&dst[size-1]
	C ebp	counter, limbs, negative

	imull	$INVERSE_3, %eax, %edx

	movl	4(%ecx,%ebp,4), %eax	C next src limb
	cmpl	$ONE_THIRD_CEIL, %edx

	sbbl	$-1, %ebx		C +1 if result>=ceil(b/3)
	cmpl	%edx, %esi

	sbbl	%ebx, %eax		C and further 1 if result>=ceil(b*2/3)
	movl	%edx, (%edi,%ebp,4)
	incl	%ebp

	setc	%bl			C new carry
	jnz	L(top)



L(last):
	C eax	src limb, carry subtracted
	C ebx	carry limb (0 or 1)
	C ecx	&src[size-1]
	C edx	scratch
	C esi	multiplier
	C edi	&dst[size-1]
	C ebp

	imull	$INVERSE_3, %eax

	cmpl	$ONE_THIRD_CEIL, %eax
	movl	%eax, (%edi)
	movl	SAVE_EBP, %ebp

	sbbl	$-1, %ebx		C +1 if eax>=ceil(b/3)
	cmpl	%eax, %esi
	movl	$0, %eax

	adcl	%ebx, %eax		C further +1 if eax>=ceil(b*2/3)
	movl	SAVE_EDI, %edi
	movl	SAVE_ESI, %esi

	movl	SAVE_EBX, %ebx
	addl	$FRAME, %esp

	ret

EPILOGUE()
