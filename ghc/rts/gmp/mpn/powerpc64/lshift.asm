#  PowerPC-64 mpn_lshift -- Shift a number left.

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
PROLOGUE(mpn_lshift)
	cmpdi	cr0,r5,20	# more than 20 limbs?
	sldi	r0,r5,3
	add	r4,r4,r0	# make r4 point at end of s1
	add	r7,r3,r0	# make r7 point at end of res
	bgt	.LBIG		# branch if more than 12 limbs

	mtctr	r5		# copy size into CTR
	subfic	r8,r6,64
	ldu	r11,-8(r4)	# load first s1 limb
	srd	r3,r11,r8	# compute function return value
	bdz	.Lend1

.Loop:	ldu	r10,-8(r4)
	sld	r9,r11,r6
	srd	r12,r10,r8
	or	r9,r9,r12
	stdu	r9,-8(r7)
	bdz	.Lend2
	ldu	r11,-8(r4)
	sld	r9,r10,r6
	srd	r12,r11,r8
	or	r9,r9,r12
	stdu	r9,-8(r7)
	bdnz	.Loop

.Lend1:	sld	r0,r11,r6
	std	r0,-8(r7)
	blr
.Lend2:	sld	r0,r10,r6
	std	r0,-8(r7)
	blr

.LBIG:
	std	r24,-64(1)
	std	r25,-56(1)
	std	r26,-48(1)
	std	r27,-40(1)
	std	r28,-32(1)
	std	r29,-24(1)
	std	r30,-16(1)
	std	r31,-8(1)
	ldu	r9,-8(r4)
	subfic	r8,r6,64
	srd	r3,r9,r8	# compute function return value
	sld	r0,r9,r6
	addi	r5,r5,-1

	andi.	r10,r5,3	# count for spill loop
	beq	.Le
	mtctr	r10
	ldu	r28,-8(r4)
	bdz	.Lxe0

.Loop0:	sld	r12,r28,r6
	srd	r24,r28,r8
	ldu	r28,-8(r4)
	or	r24,r0,r24
	stdu	r24,-8(r7)
	mr	r0,r12
	bdnz	.Loop0		# taken at most once!

.Lxe0:	sld	r12,r28,r6
	srd	r24,r28,r8
	or	r24,r0,r24
	stdu	r24,-8(r7)
	mr	r0,r12

.Le:	srdi	r5,r5,2		# count for unrolled loop
	addi	r5,r5,-1
	mtctr	r5
	ld	r28,-8(r4)
	ld	r29,-16(r4)
	ld	r30,-24(r4)
	ldu	r31,-32(r4)

.LoopU:	sld	r9,r28,r6
	srd	r24,r28,r8
	ld	r28,-8(r4)
	sld	r10,r29,r6
	srd	r25,r29,r8
	ld	r29,-16(r4)
	sld	r11,r30,r6
	srd	r26,r30,r8
	ld	r30,-24(r4)
	sld	r12,r31,r6
	srd	r27,r31,r8
	ldu	r31,-32(r4)
	or	r24,r0,r24
	std	r24,-8(r7)
	or	r25,r9,r25
	std	r25,-16(r7)
	or	r26,r10,r26
	std	r26,-24(r7)
	or	r27,r11,r27
	stdu	r27,-32(r7)
	mr	r0,r12
	bdnz	.LoopU

	sld	r9,r28,r6
	srd	r24,r28,r8
	sld	r10,r29,r6
	srd	r25,r29,r8
	sld	r11,r30,r6
	srd	r26,r30,r8
	sld	r12,r31,r6
	srd	r27,r31,r8
	or	r24,r0,r24
	std	r24,-8(r7)
	or	r25,r9,r25
	std	r25,-16(r7)
	or	r26,r10,r26
	std	r26,-24(r7)
	or	r27,r11,r27
	stdu	r27,-32(r7)
	mr	r0,r12

	std	r0,-8(r7)
	ld	r24,-64(1)
	ld	r25,-56(1)
	ld	r26,-48(1)
	ld	r27,-40(1)
	ld	r28,-32(1)
	ld	r29,-24(1)
	ld	r30,-16(1)
	ld	r31,-8(1)
	blr
EPILOGUE(mpn_lshift)
