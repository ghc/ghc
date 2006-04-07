# VAX __gmpn_lshift -- left shift.

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
# rptr		(sp + 4)
# sptr		(sp + 8)
# size		(sp + 12)
# cnt		(sp + 16)
# r0=retval r1=size r2,r3=itmp r4,r5=otmp	call-used registers
# r6=sptr r7=rptr r8=cnt r9 r10 r11		call-saved registers

.text
	.align 1
.globl ___gmpn_lshift
___gmpn_lshift:
	.word	0x1c0
	movl	4(ap),r7
	movl	8(ap),r6
	movl	12(ap),r1
	movl	16(ap),r8

	moval	(r6)[r1],r6
	moval	(r7)[r1],r7
	clrl	r3
	movl	-(r6),r2
	ashq	r8,r2,r4
	movl	r5,r0
	movl	r2,r3
	decl	r1
	jeql	Lend

Loop:	movl	-(r6),r2
	ashq	r8,r2,r4
	movl	r5,-(r7)
	movl	r2,r3
	jsobgtr	r1,Loop

Lend:	movl	r4,-4(r7)
	ret
