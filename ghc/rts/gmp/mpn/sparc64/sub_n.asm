! SPARC v9 __gmpn_sub_n -- Subtract two limb vectors of the same length > 0 and
! store difference in a third limb vector.

! Copyright (C) 1999, 2000 Free Software Foundation, Inc.

! This file is part of the GNU MP Library.

! The GNU MP Library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation; either version 2.1 of the License, or (at your
! option) any later version.

! The GNU MP Library is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
! or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
! License for more details.

! You should have received a copy of the GNU Lesser General Public License
! along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
! the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
! MA 02111-1307, USA.


! INPUT PARAMETERS
! res_ptr	%o0
! s1_ptr	%o1
! s2_ptr	%o2
! size		%o3

include(`../config.m4')

ASM_START()
	.register	%g2,#scratch
	.register	%g3,#scratch
PROLOGUE(mpn_sub_n)

! 12 mem ops >= 12 cycles
! 8 shift insn >= 8 cycles
! 8 addccc, executing alone, +8 cycles
! Unrolling not mandatory...perhaps 2-way is best?
! Put one ldx/stx and one s?lx per issue tuple, fill with pointer arith and loop ctl
! All in all, it runs at 5 cycles/limb

	save	%sp,-160,%sp

	addcc	%g0,%g0,%g0

	add	%i3,-4,%i3
	brlz,pn	%i3,L(there)
	nop

	ldx	[%i1+0],%l0
	ldx	[%i2+0],%l4
	ldx	[%i1+8],%l1
	ldx	[%i2+8],%l5
	ldx	[%i1+16],%l2
	ldx	[%i2+16],%l6
	ldx	[%i1+24],%l3
	ldx	[%i2+24],%l7
	add	%i1,32,%i1
	add	%i2,32,%i2

	add	%i3,-4,%i3
	brlz,pn	%i3,L(skip)
	nop
	b	L(loop1)	! jump instead of executing many NOPs
	nop
	ALIGN(32)
!---------  Start main loop ---------
L(loop1):
	subccc	%l0,%l4,%g1
!-
	srlx	%l0,32,%o0
	ldx	[%i1+0],%l0
!-
	srlx	%l4,32,%o4
	ldx	[%i2+0],%l4
!-
	subccc	%o0,%o4,%g0
!-
	subccc	%l1,%l5,%g2
!-
	srlx	%l1,32,%o1
	ldx	[%i1+8],%l1
!-
	srlx	%l5,32,%o5
	ldx	[%i2+8],%l5
!-
	subccc	%o1,%o5,%g0
!-
	subccc	%l2,%l6,%g3
!-
	srlx	%l2,32,%o2
	ldx	[%i1+16],%l2
!-
	srlx	%l6,32,%g5	! asymmetry
	ldx	[%i2+16],%l6
!-
	subccc	%o2,%g5,%g0
!-
	subccc	%l3,%l7,%g4
!-
	srlx	%l3,32,%o3
	ldx	[%i1+24],%l3
	add	%i1,32,%i1
!-
	srlx	%l7,32,%o7
	ldx	[%i2+24],%l7
	add	%i2,32,%i2
!-
	subccc	%o3,%o7,%g0
!-
	stx	%g1,[%i0+0]
!-
	stx	%g2,[%i0+8]
!-
	stx	%g3,[%i0+16]
	add	%i3,-4,%i3
!-
	stx	%g4,[%i0+24]
	add	%i0,32,%i0

	brgez,pt	%i3,L(loop1)
	nop
!---------  End main loop ---------
L(skip):
	subccc	%l0,%l4,%g1
	srlx	%l0,32,%o0
	srlx	%l4,32,%o4
	subccc	%o0,%o4,%g0
	subccc	%l1,%l5,%g2
	srlx	%l1,32,%o1
	srlx	%l5,32,%o5
	subccc	%o1,%o5,%g0
	subccc	%l2,%l6,%g3
	srlx	%l2,32,%o2
	srlx	%l6,32,%g5	! asymmetry
	subccc	%o2,%g5,%g0
	subccc	%l3,%l7,%g4
	srlx	%l3,32,%o3
	srlx	%l7,32,%o7
	subccc	%o3,%o7,%g0
	stx	%g1,[%i0+0]
	stx	%g2,[%i0+8]
	stx	%g3,[%i0+16]
	stx	%g4,[%i0+24]
	add	%i0,32,%i0

L(there):
	add	%i3,4,%i3
	brz,pt	%i3,L(end)
	nop

L(loop2):
	ldx	[%i1+0],%l0
	add	%i1,8,%i1
	ldx	[%i2+0],%l4
	add	%i2,8,%i2
	srlx	%l0,32,%g2
	srlx	%l4,32,%g3
	subccc	%l0,%l4,%g1
	subccc	%g2,%g3,%g0
	stx	%g1,[%i0+0]
	add	%i0,8,%i0
	add	%i3,-1,%i3
	brgz,pt	%i3,L(loop2)
	nop

L(end):	addc	%g0,%g0,%i0
	ret
	restore
EPILOGUE(mpn_sub_n)
