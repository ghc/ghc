# PowerPC-64 mpn_addsub_n -- Simultaneous add and sub.

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
# s2_ptr	r5
# size		r6

include(`asm-syntax.m4')

define(SAVE_BORROW_RESTORE_CARRY,
	`sldi $1,$1,63
	adde $1,$1,$1')
define(SAVE_CARRY_RESTORE_BORROW,
	`sldi $1,$1,63
	adde $1,$1,$1')

# 19991117

# This is just crafted for testing some ideas, and verifying that we can make
# it run fast.  It runs at 2.55 cycles/limb on the 630, which is very good.
# We should play a little with the schedule.  No time has been spent on that.

# To finish this, the loop warm up and cool down code needs to be written,
# and the result need to be tested.  Also, the proper calling sequence should
# be used.

#             r1p r2p s1p s2p n
# Use reg r0, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12

ASM_START()
PROLOGUE(mpn_addsub_n)
	std	r14,-64(1)
	std	r15,-56(1)
	std	r16,-48(1)
	std	r17,-40(1)
	std	r18,-32(1)
	std	r19,-24(1)

	srdi	r7,r7,2
	mtctr	r7		# copy size into CTR
	addic	r0,r0,0		# clear cy
	addi	r3,r3,-8	# offset res_ptr, it's updated before it's used
	addi	r4,r4,-8	# offset res_ptr, it's updated before it's used

.Loop:
	adde	r12,r8,r9
	std	r12,8(r3)
	adde	r12,r10,r11
	std	r12,16(r3)

	SAVE_CARRY_RESTORE_BORROW(r0)

	subfe	r12,r8,r9
	std	r12,8(r4)
	ld	r8,8(r5)	# s1 L 1
	ld	r9,8(r6)	# s2 L 1
	subfe	r12,r10,r11
	std	r12,16(r4)
	ld	r10,16(r5)	# s1 L 2
	ld	r11,16(r6)	# s2 L 2
# pair -------------------------
	subfe	r12,r14,r15
	std	r12,24(r4)
	subfe	r12,r16,r17
	stdu	r12,32(r4)

	SAVE_BORROW_RESTORE_CARRY(r0)

	adde	r12,r14,r15
	std	r12,24(r3)
	ld	r14,24(r5)	# s1 L 3
	ld	r15,24(r6)	# s2 L 3
	adde	r12,r16,r17
	stdu	r12,32(r3)
	ldu	r16,32(r5)	# s1 L 4
	ldu	r17,32(r6)	# s2 L 4
	bdnz	.Loop

	ld	r14,-64(1)
	ld	r15,-56(1)
	ld	r16,-48(1)
	ld	r17,-40(1)
	ld	r18,-32(1)
	ld	r19,-24(1)
	blr
EPILOGUE(mpn_addsub_n)
