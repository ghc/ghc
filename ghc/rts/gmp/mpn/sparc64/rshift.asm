! SPARC v9 __gmpn_rshift --

! Copyright (C) 1996, 2000 Free Software Foundation, Inc.

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
! src_ptr	%o1
! size		%o2
! cnt		%o3

include(`../config.m4')

ASM_START()
	.register	%g2,#scratch
	.register	%g3,#scratch
PROLOGUE(mpn_rshift)
	ldx	[%o1],%g2	! load first limb
	sub	%g0,%o3,%o5	! negate shift count
	add	%o2,-1,%o2
	and	%o2,4-1,%g4	! number of limbs in first loop
	sllx	%g2,%o5,%g1	! compute function result
	brz,pn	%g4,L(0)	! if multiple of 4 limbs, skip first loop
	mov	%g1,%g5

	sub	%o2,%g4,%o2	! adjust count for main loop

L(loop0):
	ldx	[%o1+8],%g3
	add	%o0,8,%o0
	add	%o1,8,%o1
	add	%g4,-1,%g4
	srlx	%g2,%o3,%o4
	sllx	%g3,%o5,%g1
	mov	%g3,%g2
	or	%o4,%g1,%o4
	brnz,pt	%g4,L(loop0)
	 stx	%o4,[%o0-8]

L(0):	brz,pn	%o2,L(end)
	 nop

L(loop1):
	ldx	[%o1+8],%g3
	add	%o0,32,%o0
	add	%o2,-4,%o2
	srlx	%g2,%o3,%o4
	sllx	%g3,%o5,%g1

	ldx	[%o1+16],%g2
	srlx	%g3,%o3,%g4
	or	%o4,%g1,%o4
	stx	%o4,[%o0-32]
	sllx	%g2,%o5,%g1

	ldx	[%o1+24],%g3
	srlx	%g2,%o3,%o4
	or	%g4,%g1,%g4
	stx	%g4,[%o0-24]
	sllx	%g3,%o5,%g1

	ldx	[%o1+32],%g2
	srlx	%g3,%o3,%g4
	or	%o4,%g1,%o4
	stx	%o4,[%o0-16]
	sllx	%g2,%o5,%g1

	add	%o1,32,%o1
	or	%g4,%g1,%g4
	brnz	%o2,L(loop1)
	 stx	%g4,[%o0-8]

L(end):	srlx	%g2,%o3,%g2
	stx	%g2,[%o0-0]
	retl
	mov	%g5,%o0
EPILOGUE(mpn_rshift)
