! SPARC v9 __gmpn_copy -- Copy a limb vector.

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
! rptr	%o0
! sptr	%o1
! n	%o2

include(`../config.m4')

ASM_START()
	.register	%g2,#scratch
	.register	%g3,#scratch
PROLOGUE(mpn_copyi)
	add	%o2,-8,%o2
	brlz,pn	%o2,L(skip)
	nop
	b,a	L(loop1)
	nop

	ALIGN(16)
L(loop1):
	ldx	[%o1+0],%g1
	ldx	[%o1+8],%g2
	ldx	[%o1+16],%g3
	ldx	[%o1+24],%g4
	ldx	[%o1+32],%g5
	ldx	[%o1+40],%o3
	ldx	[%o1+48],%o4
	ldx	[%o1+56],%o5
	add	%o1,64,%o1
	stx	%g1,[%o0+0]
	stx	%g2,[%o0+8]
	stx	%g3,[%o0+16]
	stx	%g4,[%o0+24]
	stx	%g5,[%o0+32]
	stx	%o3,[%o0+40]
	stx	%o4,[%o0+48]
	stx	%o5,[%o0+56]
	add	%o2,-8,%o2
	brgez,pt	%o2,L(loop1)
	add	%o0,64,%o0

L(skip):
	add	%o2,8,%o2
	brz,pt	%o2,L(end)
	nop

L(loop2):
	ldx	[%o1],%g1
	add	%o1,8,%o1
	add	%o2,-1,%o2
	stx	%g1,[%o0]
	add	%o0,8,%o0
	brgz,pt	%o2,L(loop2)
	nop

L(end):	retl
	nop
EPILOGUE(mpn_copyi)
