dnl  AMD K6 mpn_divexact_by3 -- mpn division by 3, expecting no remainder.
dnl 
dnl  K6: 11.0 cycles/limb


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
C
C Using %esi in (%esi,%ecx,4) or 0(%esi,%ecx,4) addressing modes doesn't
C lead to vector decoding, unlike plain (%esi) does.

defframe(PARAM_CARRY,16)
defframe(PARAM_SIZE, 12)
defframe(PARAM_SRC,   8)
defframe(PARAM_DST,   4)

dnl  multiplicative inverse of 3, modulo 2^32
deflit(INVERSE_3, 0xAAAAAAAB)

	.text
	ALIGN(32)

PROLOGUE(mpn_divexact_by3c)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%esi		defframe_pushl(SAVE_ESI)

	movl	PARAM_SRC, %esi
	pushl	%edi		defframe_pushl(SAVE_EDI)

	movl	PARAM_DST, %edi
	pushl	%ebx		defframe_pushl(SAVE_EBX)

	movl	PARAM_CARRY, %ebx
	leal	(%esi,%ecx,4), %esi

	pushl	$3		defframe_pushl(VAR_THREE)
	leal	(%edi,%ecx,4), %edi

	negl	%ecx


	C Need 32 alignment for claimed speed, to avoid the movl store
	C opcode/modrm crossing a cache line boundary

	ALIGN(32)
L(top):
	C eax	scratch, low product
	C ebx	carry limb (0 to 3)
	C ecx	counter, limbs, negative
	C edx	scratch, high product
	C esi	&src[size]
	C edi	&dst[size]
	C ebp
	C
	C The 0(%esi,%ecx,4) form pads so the finishup "movl %ebx, %eax"
	C doesn't cross a 32 byte boundary, saving a couple of cycles
	C (that's a fixed couple, not per loop).

Zdisp(	movl,	0,(%esi,%ecx,4), %eax)
	subl	%ebx, %eax

	setc	%bl

	imull	$INVERSE_3, %eax

	movl	%eax, (%edi,%ecx,4)
	addl	$2, %ecx

	mull	VAR_THREE

	addl	%edx, %ebx
	loop	L(top)


	movl	SAVE_ESI, %esi
	movl	%ebx, %eax

	movl	SAVE_EBX, %ebx

	movl	SAVE_EDI, %edi
	addl	$FRAME, %esp

	ret

EPILOGUE()
