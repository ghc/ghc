dnl PowerPC-32 mpn_rshift -- Shift a number right.

dnl Copyright (C) 1995, 2000 Free Software Foundation, Inc.

dnl This file is part of the GNU MP Library.

dnl The GNU MP Library is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU Lesser General Public License as published by
dnl the Free Software Foundation; either version 2.1 of the License, or (at your
dnl option) any later version.

dnl The GNU MP Library is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl License for more details.

dnl You should have received a copy of the GNU Lesser General Public License
dnl along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
dnl the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
dnl MA 02111-1307, USA.


dnl INPUT PARAMETERS
dnl res_ptr	r3
dnl s1_ptr	r4
dnl size	r5
dnl cnt		r6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_rshift)
	mtctr	r5		C copy size into CTR
	addi	r7,r3,-4	C move adjusted res_ptr to free return reg
	subfic	r8,r6,32
	lwz	r11,0(r4)	C load first s1 limb
	slw	r3,r11,r8	C compute function return value
	bdz	.Lend1

.Loop:	lwzu	r10,4(r4)
	srw	r9,r11,r6
	slw	r12,r10,r8
	or	r9,r9,r12
	stwu	r9,4(r7)
	bdz	.Lend2
	lwzu	r11,4(r4)
	srw	r9,r10,r6
	slw	r12,r11,r8
	or	r9,r9,r12
	stwu	r9,4(r7)
	bdnz	.Loop

.Lend1:	srw	r0,r11,r6
	stw	r0,4(r7)
	blr

.Lend2:	srw	r0,r10,r6
	stw	r0,4(r7)
	blr
EPILOGUE(mpn_rshift)
