dnl  Alpha mpn_rshift -- Shift a number right.

dnl  Copyright (C) 1994, 1995, 2000 Free Software Foundation, Inc.

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
dnl  cnt	r19

dnl  This code runs at 4.8 cycles/limb on the 21064.  With infinite unrolling,
dnl  it would take 4 cycles/limb.  It should be possible to get down to 3
dnl  cycles/limb since both ldq and stq can be paired with the other used
dnl  instructions.  But there are many restrictions in the 21064 pipeline that
dnl  makes it hard, if not impossible, to get down to 3 cycles/limb:

dnl  1. ldq has a 3 cycle delay, srl and sll have a 2 cycle delay.
dnl  2. Only aligned instruction pairs can be paired.
dnl  3. The store buffer or silo might not be able to deal with the bandwidth.

ASM_START()
PROLOGUE(mpn_rshift)
	ldq	r4,0(r17)	C load first limb
	addq	r17,8,r17
	subq	r31,r19,r7
	subq	r18,1,r18
	and	r18,4-1,r20	C number of limbs in first loop
	sll	r4,r7,r0	C compute function result

	beq	r20,$L0
	subq	r18,r20,r18

	ALIGN(8)
$Loop0:
	ldq	r3,0(r17)
	addq	r16,8,r16
	addq	r17,8,r17
	subq	r20,1,r20
	srl	r4,r19,r5
	sll	r3,r7,r6
	bis	r3,r3,r4
	bis	r5,r6,r8
	stq	r8,-8(r16)
	bne	r20,$Loop0

$L0:	beq	r18,$Lend

	ALIGN(8)
$Loop:	ldq	r3,0(r17)
	addq	r16,32,r16
	subq	r18,4,r18
	srl	r4,r19,r5
	sll	r3,r7,r6

	ldq	r4,8(r17)
	srl	r3,r19,r1
	bis	r5,r6,r8
	stq	r8,-32(r16)
	sll	r4,r7,r2

	ldq	r3,16(r17)
	srl	r4,r19,r5
	bis	r1,r2,r8
	stq	r8,-24(r16)
	sll	r3,r7,r6

	ldq	r4,24(r17)
	srl	r3,r19,r1
	bis	r5,r6,r8
	stq	r8,-16(r16)
	sll	r4,r7,r2

	addq	r17,32,r17
	bis	r1,r2,r8
	stq	r8,-8(r16)

	bgt	r18,$Loop

$Lend:	srl	r4,r19,r8
	stq	r8,0(r16)
	ret	r31,(r26),1
EPILOGUE(mpn_rshift)
ASM_END()
