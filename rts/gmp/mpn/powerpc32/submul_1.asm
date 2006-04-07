dnl PowerPC-32 mpn_submul_1 -- Multiply a limb vector with a limb and subtract
dnl the result from a second limb vector.

dnl Copyright (C) 1995, 1997, 1998, 2000 Free Software Foundation, Inc.

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

dnl This is optimized for the PPC604.  It has not been tested on PPC601, PPC603
dnl or PPC750 since I don't have access to any such machines.

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_submul_1)
	cmpi	cr0,r5,9	C more than 9 limbs?
	bgt	cr0,.Lbig	C branch if more than 9 limbs

	mtctr	r5
	lwz	r0,0(r4)
	mullw	r7,r0,r6
	mulhwu	r10,r0,r6
	lwz	r9,0(r3)
	subfc	r8,r7,r9
	addc	r7,r7,r8	C invert cy (r7 is junk)
	addi	r3,r3,-4
	bdz	.Lend
.Lloop:
	lwzu	r0,4(r4)
	stwu	r8,4(r3)
	mullw	r8,r0,r6
	adde	r7,r8,r10
	mulhwu	r10,r0,r6
	lwz	r9,4(r3)
	addze	r10,r10
	subfc	r8,r7,r9
	addc	r7,r7,r8	C invert cy (r7 is junk)
	bdnz	.Lloop
.Lend:	stw	r8,4(r3)
	addze	r3,r10
	blr

.Lbig:	stmw	r30,-32(r1)
	addi	r5,r5,-1
	srwi	r0,r5,2
	mtctr	r0

	lwz	r7,0(r4)
	mullw	r8,r7,r6
	mulhwu	r0,r7,r6
	lwz	r7,0(r3)
	subfc	r7,r8,r7
	addc	r8,r8,r7
	stw	r7,0(r3)

.LloopU:
	lwz	r7,4(r4)
	lwz	r12,8(r4)
	lwz	r30,12(r4)
	lwzu	r31,16(r4)
	mullw	r8,r7,r6
	mullw	r9,r12,r6
	mullw	r10,r30,r6
	mullw	r11,r31,r6
	adde	r8,r8,r0	C add cy_limb
	mulhwu	r0,r7,r6
	lwz	r7,4(r3)
	adde	r9,r9,r0
	mulhwu	r0,r12,r6
	lwz	r12,8(r3)
	adde	r10,r10,r0
	mulhwu	r0,r30,r6
	lwz	r30,12(r3)
	adde	r11,r11,r0
	mulhwu	r0,r31,r6
	lwz	r31,16(r3)
	addze	r0,r0		C new cy_limb
	subfc	r7,r8,r7
	stw	r7,4(r3)
	subfe	r12,r9,r12
	stw	r12,8(r3)
	subfe	r30,r10,r30
	stw	r30,12(r3)
	subfe	r31,r11,r31
	stwu	r31,16(r3)
	subfe	r11,r11,r11	C invert ...
	addic	r11,r11,1	C ... carry
	bdnz	.LloopU

	andi.	r31,r5,3
	mtctr	r31
	beq	cr0,.Lendx

.LloopE:
	lwzu	r7,4(r4)
	mullw	r8,r7,r6
	adde	r8,r8,r0	C add cy_limb
	mulhwu	r0,r7,r6
	lwz	r7,4(r3)
	addze	r0,r0		C new cy_limb
	subfc	r7,r8,r7
	addc	r8,r8,r7
	stwu	r7,4(r3)
	bdnz	.LloopE
.Lendx:
	addze	r3,r0
	lmw	r30,-32(r1)
	blr
EPILOGUE(mpn_submul_1)
