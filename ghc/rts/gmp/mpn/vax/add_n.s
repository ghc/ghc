# VAX __gmpn_add_n -- Add two limb vectors of the same length > 0 and store
# sum in a third limb vector.

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
# res_ptr	(sp + 4)
# s1_ptr	(sp + 8)
# s2_ptr	(sp + 12)
# size		(sp + 16)

.text
	.align 1
.globl ___gmpn_add_n
___gmpn_add_n:
	.word	0x0
	movl	16(ap),r0
	movl	12(ap),r1
	movl	8(ap),r2
	movl	4(ap),r3
	mnegl	r0,r5
	addl2	$3,r0
	ashl	$-2,r0,r0	# unroll loop count
	bicl2	$-4,r5		# mask out low 2 bits
	movaq	(r5)[r5],r5	# 9x
	jmp	Loop(r5)

Loop:	movl	(r2)+,r4
	adwc	(r1)+,r4
	movl	r4,(r3)+
	movl	(r2)+,r4
	adwc	(r1)+,r4
	movl	r4,(r3)+
	movl	(r2)+,r4
	adwc	(r1)+,r4
	movl	r4,(r3)+
	movl	(r2)+,r4
	adwc	(r1)+,r4
	movl	r4,(r3)+
	sobgtr	r0,Loop

	adwc	r0,r0
	ret
