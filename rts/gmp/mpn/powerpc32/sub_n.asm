dnl PowerPC-32 mpn_sub_n -- Subtract two limb vectors of the same length > 0
dnl and store difference in a third limb vector.

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
dnl s2_ptr	r5
dnl size	r6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_sub_n)
	mtctr	r6		C copy size into CTR
	addic	r0,r6,-1	C set cy
	lwz	r8,0(r4)	C load least significant s1 limb
	lwz	r0,0(r5)	C load least significant s2 limb
	addi	r3,r3,-4	C offset res_ptr, it's updated before it's used
	bdz	.Lend		C If done, skip loop
.Loop:	lwz	r9,4(r4)	C load s1 limb
	lwz	r10,4(r5)	C load s2 limb
	subfe	r7,r0,r8	C subtract limbs with cy, set cy
	stw	r7,4(r3)	C store result limb
	bdz	.Lexit		C decrement CTR and exit if done
	lwzu	r8,8(r4)	C load s1 limb and update s1_ptr
	lwzu	r0,8(r5)	C load s2 limb and update s2_ptr
	subfe	r7,r10,r9	C subtract limbs with cy, set cy
	stwu	r7,8(r3)	C store result limb and update res_ptr
	bdnz	.Loop		C decrement CTR and loop back

.Lend:	subfe	r7,r0,r8
	stw	r7,4(r3)	C store ultimate result limb
	subfe	r3,r0,r0	C load !cy into ...
	subfic	r3,r3,0		C ... return value register
	blr
.Lexit:	subfe	r7,r10,r9
	stw	r7,8(r3)
	subfe	r3,r0,r0	C load !cy into ...
	subfic	r3,r3,0		C ... return value register
	blr
EPILOGUE(mpn_sub_n)
