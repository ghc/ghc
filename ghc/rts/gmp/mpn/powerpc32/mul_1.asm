dnl PowerPC-32 mpn_mul_1 -- Multiply a limb vector with a limb and store
dnl the result in a second limb vector.

dnl Copyright (C) 1995, 1997, 2000 Free Software Foundation, Inc.

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
dnl s2_limb	r6

dnl This is optimized for the PPC604 but it runs decently even on PPC601.  It
dnl has not been tested on a PPC603 since I don't have access to any such
dnl machines.

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_mul_1)
	mtctr	r5
	addi	r3,r3,-4	C adjust res_ptr, it's offset before it's used
	li	r12,0		C clear upper product reg
	addic	r0,r0,0		C clear cy
C Start software pipeline
	lwz	r8,0(r4)
	bdz	.Lend3
	stmw	r30,-8(r1)	C save registers we are supposed to preserve
	lwzu	r9,4(r4)
	mullw	r11,r8,r6
	mulhwu	r0,r8,r6
	bdz	.Lend1
C Software pipelined main loop
.Loop:	lwz	r8,4(r4)
	mullw	r10,r9,r6
	adde	r30,r11,r12
	mulhwu	r12,r9,r6
	stw	r30,4(r3)
	bdz	.Lend2
	lwzu	r9,8(r4)
	mullw	r11,r8,r6
	adde	r31,r10,r0
	mulhwu	r0,r8,r6
	stwu	r31,8(r3)
	bdnz	.Loop
C Finish software pipeline
.Lend1:	mullw	r10,r9,r6
	adde	r30,r11,r12
	mulhwu	r12,r9,r6
	stw	r30,4(r3)
	adde	r31,r10,r0
	stwu	r31,8(r3)
	addze	r3,r12
	lmw	r30,-8(r1)	C restore registers from stack
	blr
.Lend2:	mullw	r11,r8,r6
	adde	r31,r10,r0
	mulhwu	r0,r8,r6
	stwu	r31,8(r3)
	adde	r30,r11,r12
	stw	r30,4(r3)
	addze	r3,r0
	lmw	r30,-8(r1)	C restore registers from stack
	blr
.Lend3:	mullw	r11,r8,r6
	stw	r11,4(r3)
	mulhwu	r3,r8,r6
	blr
EPILOGUE(mpn_mul_1)
