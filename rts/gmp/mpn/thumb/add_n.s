@ ARM/Thumb __gmpn_add -- Add two limb vectors of the same length > 0 and store
@ sum in a third limb vector.

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


@ INPUT PARAMETERS
@ RES_ptr	r0
@ S1_ptr	r1
@ S2_ptr	r2
@ SIZE		r3

@ NOT TESTED CODE

	.text
	.thumb
	.align	0
	.global	___gmpn_add_n
___gmpn_add_n:
	push	{r4, r5, r6, lr}
	mov	r6, #1			@ init carry save register

Loop:	sub	r6, #1			@ restore carry (set iff r6 was 0)
	ldmia	r1!, {r4}		@ load next limb from S1
	ldmia	r2!, {r5}		@ load next limb from S2
	adc	r4, r5
	stmia	r0!, {r4}		@ store result limb to RES
	sbc	r6, r6			@ save negated carry
	sub	r3, #1
	bge	Loop			@ loop back while remaining count >= 4

	mov	r0, r6
	pop	{r4, r5, r6, pc}
