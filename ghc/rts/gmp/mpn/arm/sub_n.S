@ ARM mpn_sub -- Subtract two limb vectors of the same length > 0 and store
@ difference in a third limb vector.
@ Contributed by Robert Harley.

@ Copyright (C) 1997, 2000 Free Software Foundation, Inc.

@ This file is part of the GNU MP Library.

@ The GNU MP Library is free software; you can redistribute it and/or modify
@ it under the terms of the GNU Lesser General Public License as published by
@ the Free Software Foundation; either version 2.1 of the License, or (at your
@ option) any later version.

@ The GNU MP Library is distributed in the hope that it will be useful, but
@ WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
@ or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
@ License for more details.

@ You should have received a copy of the GNU Lesser General Public License
@ along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
@ the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
@ MA 02111-1307, USA.

#define d r0
#define a r1
#define b r2
#define n r3

#define sl r10
#define fp r11
#define ip r12
#define sp r13
#define lr r14
#define pc r15

.text
	.align	0
	.global	__gmpn_sub_n
	.type	__gmpn_sub_n,%function
__gmpn_sub_n:
	stmfd	sp!, { r8, r9, lr }
	subs	ip, ip, ip
	tst	n, #1
	beq	skip1
	ldr	ip, [a], #4
	ldr	lr, [b], #4
	subs	ip, ip, lr
	str	ip, [d], #4
skip1:
	tst	n, #2
	beq	skip2
	ldmia	a!, { r8, r9 }
	ldmia	b!, { ip, lr }
	sbcs	r8, r8, ip
	sbcs	r9, r9, lr
	stmia	d!, { r8, r9 }
skip2:
	bics	n, n, #3
	beq	return
	stmfd	sp!, { r4, r5, r6, r7 }
sub_n_loop:
	ldmia	a!, { r4, r5, r6, r7 }
	ldmia	b!, { r8, r9, ip, lr }
	sbcs	r4, r4, r8
	ldr	r8, [d] /* Bring stuff into cache. */
	sbcs	r5, r5, r9
	sbcs	r6, r6, ip
	sbcs	r7, r7, lr
	stmia	d!, { r4, r5, r6, r7 }
	sub	n, n, #4
	teq	n, #0
	bne	sub_n_loop
	ldmfd	sp!, { r4, r5, r6, r7 }
return:
	sbc	r0, r0, r0
	and	r0, r0, #1
	ldmfd	sp!, { r8, r9, pc }
end:
	.size	__gmpn_sub_n, end - __gmpn_sub_n
