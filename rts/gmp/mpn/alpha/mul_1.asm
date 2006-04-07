dnl  Alpha __gmpn_mul_1 -- Multiply a limb vector with a limb and store
dnl  the result in a second limb vector.

dnl  Copyright (C) 1992, 1994, 1995, 2000 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published by
dnl  the Free Software Foundation; either version 2.1 of the License, or (at your
dnl  option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
dnl  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
dnl  MA 02111-1307, USA.

include(`../config.m4')

dnl  INPUT PARAMETERS
dnl  res_ptr	r16
dnl  s1_ptr	r17
dnl  size	r18
dnl  s2_limb	r19

dnl  This code runs at 42 cycles/limb on EV4, 18 cycles/limb on EV5, and 7
dnl  cycles/limb on EV6.

ASM_START()
PROLOGUE(mpn_mul_1)
	ldq	r2,0(r17)	C r2 = s1_limb
	subq	r18,1,r18	C size--
	mulq	r2,r19,r3	C r3 = prod_low
	bic	r31,r31,r4	C clear cy_limb
	umulh	r2,r19,r0	C r0 = prod_high
	beq	r18,$Lend1	C jump if size was == 1
	ldq	r2,8(r17)	C r2 = s1_limb
	subq	r18,1,r18	C size--
	stq	r3,0(r16)
	beq	r18,$Lend2	C jump if size was == 2

	ALIGN(8)
$Loop:	mulq	r2,r19,r3	C r3 = prod_low
	addq	r4,r0,r0	C cy_limb = cy_limb + 'cy'
	subq	r18,1,r18	C size--
	umulh	r2,r19,r4	C r4 = cy_limb
	ldq	r2,16(r17)	C r2 = s1_limb
	addq	r17,8,r17	C s1_ptr++
	addq	r3,r0,r3	C r3 = cy_limb + prod_low
	stq	r3,8(r16)
	cmpult	r3,r0,r0	C r0 = carry from (cy_limb + prod_low)
	addq	r16,8,r16	C res_ptr++
	bne	r18,$Loop

$Lend2:	mulq	r2,r19,r3	C r3 = prod_low
	addq	r4,r0,r0	C cy_limb = cy_limb + 'cy'
	umulh	r2,r19,r4	C r4 = cy_limb
	addq	r3,r0,r3	C r3 = cy_limb + prod_low
	cmpult	r3,r0,r0	C r0 = carry from (cy_limb + prod_low)
	stq	r3,8(r16)
	addq	r4,r0,r0	C cy_limb = prod_high + cy
	ret	r31,(r26),1
$Lend1:	stq	r3,0(r16)
	ret	r31,(r26),1
EPILOGUE(mpn_mul_1)
ASM_END()
