dnl  Alpha EV5 __gmpn_sub_n -- Subtract two limb vectors of the same length > 0
dnl  and store difference in a third limb vector.

dnl  Copyright (C) 1995, 1999, 2000 Free Software Foundation, Inc.

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
dnl  s2_ptr	r18
dnl  size	r19

ASM_START()
PROLOGUE(mpn_sub_n)
	bis	r31,r31,r25		C clear cy
	subq	r19,4,r19		C decr loop cnt
	blt	r19,$Lend2		C if less than 4 limbs, goto 2nd loop
C Start software pipeline for 1st loop
	ldq	r0,0(r18)
	ldq	r4,0(r17)
	ldq	r1,8(r18)
	ldq	r5,8(r17)
	addq	r17,32,r17		C update s1_ptr
	ldq	r2,16(r18)
	subq	r4,r0,r20		C 1st main subtract
	ldq	r3,24(r18)
	subq	r19,4,r19		C decr loop cnt
	ldq	r6,-16(r17)
	cmpult	r4,r0,r25		C compute cy from last subtract
	ldq	r7,-8(r17)
	subq	r5,r1,r28		C 2nd main subtract
	addq	r18,32,r18		C update s2_ptr
	subq	r28,r25,r21		C 2nd carry subtract
	cmpult	r5,r1,r8		C compute cy from last subtract
	blt	r19,$Lend1		C if less than 4 limbs remain, jump
C 1st loop handles groups of 4 limbs in a software pipeline
	ALIGN(16)
$Loop:	cmpult	r28,r25,r25		C compute cy from last subtract
	ldq	r0,0(r18)
	bis	r8,r25,r25		C combine cy from the two subtracts
	ldq	r1,8(r18)
	subq	r6,r2,r28		C 3rd main subtract
	ldq	r4,0(r17)
	subq	r28,r25,r22		C 3rd carry subtract
	ldq	r5,8(r17)
	cmpult	r6,r2,r8		C compute cy from last subtract
	cmpult	r28,r25,r25		C compute cy from last subtract
	stq	r20,0(r16)
	bis	r8,r25,r25		C combine cy from the two subtracts
	stq	r21,8(r16)
	subq	r7,r3,r28		C 4th main subtract
	subq	r28,r25,r23		C 4th carry subtract
	cmpult	r7,r3,r8		C compute cy from last subtract
	cmpult	r28,r25,r25		C compute cy from last subtract
		addq	r17,32,r17		C update s1_ptr
	bis	r8,r25,r25		C combine cy from the two subtracts
		addq	r16,32,r16		C update res_ptr
	subq	r4,r0,r28		C 1st main subtract
	ldq	r2,16(r18)
	subq	r28,r25,r20		C 1st carry subtract
	ldq	r3,24(r18)
	cmpult	r4,r0,r8		C compute cy from last subtract
	ldq	r6,-16(r17)
	cmpult	r28,r25,r25		C compute cy from last subtract
	ldq	r7,-8(r17)
	bis	r8,r25,r25		C combine cy from the two subtracts
	subq	r19,4,r19		C decr loop cnt
	stq	r22,-16(r16)
	subq	r5,r1,r28		C 2nd main subtract
	stq	r23,-8(r16)
	subq	r28,r25,r21		C 2nd carry subtract
		addq	r18,32,r18		C update s2_ptr
	cmpult	r5,r1,r8		C compute cy from last subtract
	bge	r19,$Loop
C Finish software pipeline for 1st loop
$Lend1:	cmpult	r28,r25,r25		C compute cy from last subtract
	bis	r8,r25,r25		C combine cy from the two subtracts
	subq	r6,r2,r28		C cy add
	subq	r28,r25,r22		C 3rd main subtract
	cmpult	r6,r2,r8		C compute cy from last subtract
	cmpult	r28,r25,r25		C compute cy from last subtract
	stq	r20,0(r16)
	bis	r8,r25,r25		C combine cy from the two subtracts
	stq	r21,8(r16)
	subq	r7,r3,r28		C cy add
	subq	r28,r25,r23		C 4th main subtract
	cmpult	r7,r3,r8		C compute cy from last subtract
	cmpult	r28,r25,r25		C compute cy from last subtract
	bis	r8,r25,r25		C combine cy from the two subtracts
	addq	r16,32,r16		C update res_ptr
	stq	r22,-16(r16)
	stq	r23,-8(r16)
$Lend2:	addq	r19,4,r19		C restore loop cnt
	beq	r19,$Lret
C Start software pipeline for 2nd loop
	ldq	r0,0(r18)
	ldq	r4,0(r17)
	subq	r19,1,r19
	beq	r19,$Lend0
C 2nd loop handles remaining 1-3 limbs
	ALIGN(16)
$Loop0:	subq	r4,r0,r28		C main subtract
	cmpult	r4,r0,r8		C compute cy from last subtract
	ldq	r0,8(r18)
	ldq	r4,8(r17)
	subq	r28,r25,r20		C carry subtract
	addq	r18,8,r18
	addq	r17,8,r17
	stq	r20,0(r16)
	cmpult	r28,r25,r25		C compute cy from last subtract
	subq	r19,1,r19		C decr loop cnt
	bis	r8,r25,r25		C combine cy from the two subtracts
	addq	r16,8,r16
	bne	r19,$Loop0
$Lend0:	subq	r4,r0,r28		C main subtract
	subq	r28,r25,r20		C carry subtract
	cmpult	r4,r0,r8		C compute cy from last subtract
	cmpult	r28,r25,r25		C compute cy from last subtract
	stq	r20,0(r16)
	bis	r8,r25,r25		C combine cy from the two subtracts

$Lret:	bis	r25,r31,r0		C return cy
	ret	r31,(r26),1
EPILOGUE(mpn_sub_n)
ASM_END()
