# PowerPC-64 mpn_rshift -- Shift a number right.

# Copyright (C) 1999, 2000 Free Software Foundation, Inc.

# This file is part of the GNU MP Library.

# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.

# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.


# INPUT PARAMETERS
# res_ptr	r3
# s1_ptr	r4
# size		r5
# cnt		r6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_rshift)
	mtctr	r5		# copy size into CTR
	addi	r7,r3,-8	# move adjusted res_ptr to free return reg
	subfic	r8,r6,64
	ld	r11,0(r4)	# load first s1 limb
	sld	r3,r11,r8	# compute function return value
	bdz	.Lend1

.Loop:	ldu	r10,8(r4)
	srd	r9,r11,r6
	sld	r12,r10,r8
	or	r9,r9,r12
	stdu	r9,8(r7)
	bdz	.Lend2
	ldu	r11,8(r4)
	srd	r9,r10,r6
	sld	r12,r11,r8
	or	r9,r9,r12
	stdu	r9,8(r7)
	bdnz	.Loop

.Lend1:	srd	r0,r11,r6
	std	r0,8(r7)
	blr

.Lend2:	srd	r0,r10,r6
	std	r0,8(r7)
	blr
EPILOGUE(mpn_rshift)
