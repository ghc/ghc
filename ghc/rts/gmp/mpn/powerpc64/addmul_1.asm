# PowerPC-64 mpn_addmul_1 -- Multiply a limb vector with a limb and add
# the result to a second limb vector.

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
# s2_limb	r6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_addmul_1)
	mtctr	5
	li	9,0		# cy_limb = 0
	addic	0,0,0
	cal	3,-8(3)
	cal	4,-8(4)
.Loop:
	ldu	0,8(4)
	ld	10,8(3)
	mulld	7,0,6
	adde	7,7,9
	mulhdu	9,0,6
	addze	9,9
	addc	7,7,10
	stdu	7,8(3)
	bdnz	.Loop

	addze	3,9
	blr
EPILOGUE(mpn_addmul_1)
