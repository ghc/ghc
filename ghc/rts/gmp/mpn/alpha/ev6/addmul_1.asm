dnl Alpha ev6 mpn_addmul_1 -- Multiply a limb vector with a limb and add
dnl the result to a second limb vector.

dnl  Copyright (C) 2000 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.

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
dnl  size	r18
dnl  s2_limb	r19

dnl  This code runs at 42 cycles/limb on EV4, 18 cycles/limb on EV5, and
dnl  exactly 3.625 cycles/limb on EV6...

dnl This code was written in close cooperation with ev6 pipeline expert
dnl Steve Root (root@toober.hlo.dec.com).  Any errors are tege's fault, though.
dnl
dnl   Register usages for unrolled loop:
dnl	  0-3     mul's
dnl	  4-7     acc's
dnl	  8-15    mul results
dnl	  20,21   carry's
dnl	  22,23   save for stores

dnl   Sustains 8 mul-adds in 29 cycles in the unrolled inner loop.

dnl   The stores can issue a cycle late so we have paired no-op's to 'catch'
dnl   them, so that further disturbance to the schedule is damped.

dnl   We couldn't pair the loads, because the entangled schedule of the
dnl   carry's has to happen on one side {0} of the machine. Note, the total
dnl   use of U0, and the total use of L0 (after attending to the stores).
dnl   which is part of the reason why....

dnl   This is a great schedule for the d_cache, a poor schedule for the
dnl   b_cache. The lockup on U0 means that any stall can't be recovered
dnl   from. Consider a ldq in L1.  say that load gets stalled because it
dnl   collides with a fill from the b_Cache. On the next cycle, this load
dnl   gets priority. If first looks at L0, and goes there. The instruction
dnl   we intended for L0 gets to look at L1, which is NOT where we want
dnl   it. It either stalls 1, because it can't go in L0, or goes there, and
dnl   causes a further instruction to stall.

dnl   So for b_cache, we're likely going to want to put one or more cycles
dnl   back into the code! And, of course, put in prefetches. For the
dnl   accumulator, lds, intent to modify.  For the multiplier, you might
dnl   want ldq, evict next, if you're not wanting to use it again soon. Use
dnl   256 ahead of present pointer value. At a place where we have an mt
dnl   followed by a bookkeeping, put the bookkeeping in upper, and the
dnl   prefetch into lower.

dnl   Note, the usage of physical registers per cycle is smoothed off, as
dnl   much as possible.

dnl   Note, the ldq's and stq's are at the end of the quadpacks.  note, we'd
dnl   like not to have a ldq or stq to preceded a conditional branch in a
dnl   quadpack. The conditional branch moves the retire pointer one cycle
dnl   later.

dnl   Optimization notes:
dnl   Callee-saves regs: r9 r10 r11 r12 r13 r14 r15 r26 ?r27?
dnl   Reserved regs:	 r29 r30 r31
dnl   Free caller-saves regs in unrolled code: r24 r25 r28
dnl   We should swap some of the callee-saves regs for some of the free
dnl   caller-saves regs, saving some overhead cycles.
dnl   Most importantly, we should write fast code for the 0-7 case.
dnl   The code we use there are for the 21164, and runs at 7 cycles/limb
dnl   on the 21264.  Should not be hard, if we write specialized code for
dnl   1-7 limbs (the one for 0 limbs should be straightforward).  We then just
dnl   need a jump table indexed by the low 3 bits of the count argument.


ASM_START()
PROLOGUE(mpn_addmul_1)
	cmpult	r18,	8,	r1
	beq	r1,	$Large

	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	subq	r18,	1,	r18	C size--
	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	umulh	r2,	r19,	r0	C r0 = prod_high
	beq	r18,	$Lend0b		C jump if size was == 1
	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	subq	r18,	1,	r18	C size--
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r4
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	beq	r18,	$Lend0a		C jump if size was == 2

	ALIGN(8)
$Loop0:	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	addq	r4,	r0,	r0	C cy_limb = cy_limb + 'cy'
	subq	r18,	1,	r18	C size--
	umulh	r2,	r19,	r4	C r4 = cy_limb
	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	addq	r3,	r0,	r3	C r3 = cy_limb + prod_low
	cmpult	r3,	r0,	r0	C r0 = carry from (cy_limb + prod_low)
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	addq	r5,	r0,	r0	C combine carries
	bne	r18,	$Loop0
$Lend0a:
	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	addq	r4,	r0,	r0	C cy_limb = cy_limb + 'cy'
	umulh	r2,	r19,	r4	C r4 = cy_limb
	addq	r3,	r0,	r3	C r3 = cy_limb + prod_low
	cmpult	r3,	r0,	r0	C r0 = carry from (cy_limb + prod_low)
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r5,	r0,	r0	C combine carries
	addq	r4,	r0,	r0	C cy_limb = prod_high + cy
	ret	r31,	(r26),	1
$Lend0b:
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r0,	r5,	r0
	ret	r31,	(r26),	1

$Large:
	lda	$30,	-240($30)
	stq	$9,	8($30)
	stq	$10,	16($30)
	stq	$11,	24($30)
	stq	$12,	32($30)
	stq	$13,	40($30)
	stq	$14,	48($30)
	stq	$15,	56($30)

	and	r18,	7,	r20	C count for the first loop, 0-7
	srl	r18,	3,	r18	C count for unrolled loop
	bis	r31,	r31,	r0
	beq	r20,	$Lunroll
	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	subq	r20,	1,	r20	C size--
	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	umulh	r2,	r19,	r0	C r0 = prod_high
	beq	r20,	$Lend1b		C jump if size was == 1
	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	subq	r20,	1,	r20	C size--
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r4
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	beq	r20,	$Lend1a		C jump if size was == 2

	ALIGN(8)
$Loop1:	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	addq	r4,	r0,	r0	C cy_limb = cy_limb + 'cy'
	subq	r20,	1,	r20	C size--
	umulh	r2,	r19,	r4	C r4 = cy_limb
	ldq	r2,	0(r17)		C r2 = s1_limb
	addq	r17,	8,	r17	C s1_ptr++
	addq	r3,	r0,	r3	C r3 = cy_limb + prod_low
	cmpult	r3,	r0,	r0	C r0 = carry from (cy_limb + prod_low)
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	addq	r5,	r0,	r0	C combine carries
	bne	r20,	$Loop1

$Lend1a:
	mulq	r2,	r19,	r3	C r3 = prod_low
	ldq	r5,	0(r16)		C r5 = *res_ptr
	addq	r4,	r0,	r0	C cy_limb = cy_limb + 'cy'
	umulh	r2,	r19,	r4	C r4 = cy_limb
	addq	r3,	r0,	r3	C r3 = cy_limb + prod_low
	cmpult	r3,	r0,	r0	C r0 = carry from (cy_limb + prod_low)
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	addq	r5,	r0,	r0	C combine carries
	addq	r4,	r0,	r0	C cy_limb = prod_high + cy
	br	r31,	$Lunroll
$Lend1b:
	addq	r5,	r3,	r3
	cmpult	r3,	r5,	r5
	stq	r3,	0(r16)
	addq	r16,	8,	r16	C res_ptr++
	addq	r0,	r5,	r0

$Lunroll:
	lda	r17,	-16(r17)	C L1 bookkeeping
	lda	r16,	-16(r16)	C L1 bookkeeping
	bis	r0,	r31,	r12

C ____ UNROLLED LOOP SOFTWARE PIPELINE STARTUP ____

	ldq	r2,	16(r17)		C L1
	ldq	r3,	24(r17)		C L1
	lda	r18,	-1(r18)		C L1 bookkeeping
	ldq	r6,	16(r16)		C L1
	ldq	r7,	24(r16)		C L1
	ldq	r0,	32(r17)		C L1
	mulq	r19,	r2,	r13	C U1
	ldq	r1,	40(r17)		C L1
	umulh	r19,	r2,	r14	C U1
	mulq	r19,	r3,	r15	C U1
	lda	r17,	64(r17)		C L1 bookkeeping
	ldq	r4,	32(r16)		C L1
	ldq	r5,	40(r16)		C L1
	umulh	r19,	r3,	r8	C U1
	ldq	r2,	-16(r17)	C L1
	mulq	r19,	r0,	r9	C U1
	ldq	r3,	-8(r17)		C L1
	umulh	r19,	r0,	r10	C U1
	addq	r6,	r13,	r6	C L0 lo + acc
	mulq	r19,	r1,	r11	C U1
	cmpult	r6,	r13,	r20	C L0 lo add => carry
	lda	r16,	64(r16)		C L1 bookkeeping
	addq	r6,	r12,	r22	C U0 hi add => answer
	cmpult	r22,	r12,	r21	C L0 hi add => carry
	addq	r14,	r20,	r14	C U0 hi mul + carry
	ldq	r6,	-16(r16)	C L1
	addq	r7,	r15,	r23	C L0 lo + acc
	addq	r14,	r21,	r14	C U0 hi mul + carry
	ldq	r7,	-8(r16)		C L1
	umulh	r19,	r1,	r12	C U1
	cmpult	r23,	r15,	r20	C L0 lo add => carry
	addq	r23,	r14,	r23	C U0 hi add => answer
	ldq	r0,	(r17)		C L1
	mulq	r19,	r2,	r13	C U1
	cmpult	r23,	r14,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	ldq	r1,	8(r17)		C L1
	umulh	r19,	r2,	r14	C U1
	addq	r4,	r9,	r4	C L0 lo + acc
	stq	r22,	-48(r16)	C L0
	stq	r23,	-40(r16)	C L1
	mulq	r19,	r3,	r15	C U1
	addq	r8,	r21,	r8	C U0 hi mul + carry
	cmpult	r4,	r9,	r20	C L0 lo add => carry
	addq	r4,	r8,	r22	C U0 hi add => answer
	ble	r18,	$Lend		C U1 bookkeeping

C ____ MAIN UNROLLED LOOP ____
	ALIGN(16)
$Loop:
	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r10,	r20,	r10	C U0 hi mul + carry
	ldq	r4,	(r16)		C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r11,	r23	C L0 lo + acc
	addq	r10,	r21,	r10	C L0 hi mul + carry
	ldq	r5,	8(r16)		C L1

	umulh	r19,	r3,	r8	C U1
	cmpult	r23,	r11,	r20	C L0 lo add => carry
	addq	r23,	r10,	r23	C U0 hi add => answer
	ldq	r2,	16(r17)		C L1

	mulq	r19,	r0,	r9	C U1
	cmpult	r23,	r10,	r21	C L0 hi add => carry
	addq	r12,	r20,	r12	C U0 hi mul + carry
	ldq	r3,	24(r17)		C L1

	umulh	r19,	r0,	r10	C U1
	addq	r6,	r13,	r6	C L0 lo + acc
	stq	r22,	-32(r16)	C L0
	stq	r23,	-24(r16)	C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r11	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r12,	r21,	r12	C U0 hi mul + carry

	cmpult	r6,	r13,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r18,	-1(r18)		C L1 bookkeeping
	addq	r6,	r12,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r12,	r21	C L0 hi add => carry
	addq	r14,	r20,	r14	C U0 hi mul + carry
	ldq	r6,	16(r16)		C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r7,	r15,	r23	C L0 lo + acc
	addq	r14,	r21,	r14	C U0 hi mul + carry
	ldq	r7,	24(r16)		C L1

	umulh	r19,	r1,	r12	C U1
	cmpult	r23,	r15,	r20	C L0 lo add => carry
	addq	r23,	r14,	r23	C U0 hi add => answer
	ldq	r0,	32(r17)		C L1

	mulq	r19,	r2,	r13	C U1
	cmpult	r23,	r14,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	ldq	r1,	40(r17)		C L1

	umulh	r19,	r2,	r14	C U1
	addq	r4,	r9,	r4	C U0 lo + acc
	stq	r22,	-16(r16)	C L0
	stq	r23,	-8(r16)		C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r3,	r15	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r8,	r21,	r8	C L0 hi mul + carry

	cmpult	r4,	r9,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r17,	64(r17)		C L1 bookkeeping
	addq	r4,	r8,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r10,	r20,	r10	C U0 hi mul + carry
	ldq	r4,	32(r16)		C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r11,	r23	C L0 lo + acc
	addq	r10,	r21,	r10	C L0 hi mul + carry
	ldq	r5,	40(r16)		C L1

	umulh	r19,	r3,	r8	C U1
	cmpult	r23,	r11,	r20	C L0 lo add => carry
	addq	r23,	r10,	r23	C U0 hi add => answer
	ldq	r2,	-16(r17)	C L1

	mulq	r19,	r0,	r9	C U1
	cmpult	r23,	r10,	r21	C L0 hi add => carry
	addq	r12,	r20,	r12	C U0 hi mul + carry
	ldq	r3,	-8(r17)		C L1

	umulh	r19,	r0,	r10	C U1
	addq	r6,	r13,	r6	C L0 lo + acc
	stq	r22,	(r16)		C L0
	stq	r23,	8(r16)		C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r11	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r12,	r21,	r12	C U0 hi mul + carry

	cmpult	r6,	r13,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r16,	64(r16)		C L1 bookkeeping
	addq	r6,	r12,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r12,	r21	C L0 hi add => carry
	addq	r14,	r20,	r14	C U0 hi mul + carry
	ldq	r6,	-16(r16)	C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r7,	r15,	r23	C L0 lo + acc
	addq	r14,	r21,	r14	C U0 hi mul + carry
	ldq	r7,	-8(r16)		C L1

	umulh	r19,	r1,	r12	C U1
	cmpult	r23,	r15,	r20	C L0 lo add => carry
	addq	r23,	r14,	r23	C U0 hi add => answer
	ldq	r0,	(r17)		C L1

	mulq	r19,	r2,	r13	C U1
	cmpult	r23,	r14,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	ldq	r1,	8(r17)		C L1

	umulh	r19,	r2,	r14	C U1
	addq	r4,	r9,	r4	C L0 lo + acc
	stq	r22,	-48(r16)	C L0
	stq	r23,	-40(r16)	C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r3,	r15	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r8,	r21,	r8	C U0 hi mul + carry

	cmpult	r4,	r9,	r20	C L0 lo add => carry
	addq	r4,	r8,	r22	C U0 hi add => answer
	bis	r31,	r31,	r31	C L1 mt
	bgt	r18,	$Loop		C U1 bookkeeping

C ____ UNROLLED LOOP SOFTWARE PIPELINE FINISH ____
$Lend:
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r10,	r20,	r10	C U0 hi mul + carry
	ldq	r4,	(r16)		C L1
	addq	r5,	r11,	r23	C L0 lo + acc
	addq	r10,	r21,	r10	C L0 hi mul + carry
	ldq	r5,	8(r16)		C L1
	umulh	r19,	r3,	r8	C U1
	cmpult	r23,	r11,	r20	C L0 lo add => carry
	addq	r23,	r10,	r23	C U0 hi add => answer
	mulq	r19,	r0,	r9	C U1
	cmpult	r23,	r10,	r21	C L0 hi add => carry
	addq	r12,	r20,	r12	C U0 hi mul + carry
	umulh	r19,	r0,	r10	C U1
	addq	r6,	r13,	r6	C L0 lo + acc
	stq	r22,	-32(r16)	C L0
	stq	r23,	-24(r16)	C L1
	mulq	r19,	r1,	r11	C U1
	addq	r12,	r21,	r12	C U0 hi mul + carry
	cmpult	r6,	r13,	r20	C L0 lo add => carry
	addq	r6,	r12,	r22	C U0 hi add => answer
	cmpult	r22,	r12,	r21	C L0 hi add => carry
	addq	r14,	r20,	r14	C U0 hi mul + carry
	addq	r7,	r15,	r23	C L0 lo + acc
	addq	r14,	r21,	r14	C U0 hi mul + carry
	umulh	r19,	r1,	r12	C U1
	cmpult	r23,	r15,	r20	C L0 lo add => carry
	addq	r23,	r14,	r23	C U0 hi add => answer
	cmpult	r23,	r14,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	addq	r4,	r9,	r4	C U0 lo + acc
	stq	r22,	-16(r16)	C L0
	stq	r23,	-8(r16)		C L1
	bis	r31,	r31,	r31	C L0 st slosh
	addq	r8,	r21,	r8	C L0 hi mul + carry
	cmpult	r4,	r9,	r20	C L0 lo add => carry
	addq	r4,	r8,	r22	C U0 hi add => answer
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r10,	r20,	r10	C U0 hi mul + carry
	addq	r5,	r11,	r23	C L0 lo + acc
	addq	r10,	r21,	r10	C L0 hi mul + carry
	cmpult	r23,	r11,	r20	C L0 lo add => carry
	addq	r23,	r10,	r23	C U0 hi add => answer
	cmpult	r23,	r10,	r21	C L0 hi add => carry
	addq	r12,	r20,	r12	C U0 hi mul + carry
	stq	r22,	(r16)		C L0
	stq	r23,	8(r16)		C L1
	addq	r12,	r21,	r0	C U0 hi mul + carry

	ldq	$9,	8($30)
	ldq	$10,	16($30)
	ldq	$11,	24($30)
	ldq	$12,	32($30)
	ldq	$13,	40($30)
	ldq	$14,	48($30)
	ldq	$15,	56($30)
	lda	$30,	240($30)
	ret	r31,	(r26),	1
EPILOGUE(mpn_addmul_1)
ASM_END()
