dnl  SPARC 64-bit mull -- Helper for mpn_mul_1.

dnl  Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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

mull:
	save %sp,-256,%sp

	sethi	%hi(0xffff0000),%o0
	andn	%i3,%o0,%o0
	st	%o0,[%fp-17]
	ld	[%fp-17],%f11
	fxtod	%f10,%f6

	srl	%i3,16,%o0
	st	%o0,[%fp-17]
	ld	[%fp-17],%f11
	fxtod	%f10,%f8

	mov	0,%g3			C cy = 0

	ld	[%i1+4],%f11
	subcc	%i2,1,%i2
dnl	be,pn	%icc,E(end1)
	add	%i1,4,%i1		C s1_ptr++

	fxtod	%f10,%f2
	ld	[%i1-4],%f11
	add	%i1,4,%i1		C s1_ptr++
	fmuld	%f2,%f8,%f16
	fmuld	%f2,%f6,%f4
	fdtox	%f16,%f14
	std	%f14,[%fp-25]
	fdtox	%f4,%f12
	subcc	%i2,1,%i2
	be,pn	%icc,E(end2)
	std	%f12,[%fp-17]

	fxtod	%f10,%f2
	ld	[%i1+4],%f11
	add	%i1,4,%i1		C s1_ptr++
	fmuld	%f2,%f8,%f16
	fmuld	%f2,%f6,%f4
	fdtox	%f16,%f14
	std	%f14,[%fp-41]
	fdtox	%f4,%f12
	subcc	%i2,1,%i2
dnl	be,pn	%icc,E(end3)
	std	%f12,[%fp-33]

	fxtod	%f10,%f2
	ld	[%i1-4],%f11
	add	%i1,4,%i1		C s1_ptr++
	ldx	[%fp-25],%g2		C p16
	fmuld	%f2,%f8,%f16
	ldx	[%fp-17],%g1		C p0
	fmuld	%f2,%f6,%f4
	sllx	%g2,16,%g2		C align p16
	fdtox	%f16,%f14
	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	std	%f14,[%fp-25]
	fdtox	%f4,%f12
	add	%i0,4,%i0		C res_ptr++
	subcc	%i2,1,%i2
	be,pn	%icc,E(end4)
	std	%f12,[%fp-17]

	b,a	E(loop)
	nop				C nop is cheap to nullify

	ALIGN(16)
C BEGIN LOOP
E(loop):
	fxtod	%f10,%f2
	ld	[%i1+4],%f11
	add	%i1,4,%i1		C s1_ptr++
	add	%g3,%g1,%g4		C p += cy
	srlx	%g4,32,%g3
	ldx	[%fp-41],%g2		C p16
	fmuld	%f2,%f8,%f16
	ldx	[%fp-33],%g1		C p0
	fmuld	%f2,%f6,%f4
	sllx	%g2,16,%g2		C align p16
	st	%g4,[%i0-4+DLO]
	fdtox	%f16,%f14
	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	std	%f14,[%fp-41]
	fdtox	%f4,%f12
	std	%f12,[%fp-33]
	sub	%i2,2,%i2
	add	%i0,4,%i0		C res_ptr++

	fxtod	%f10,%f2
	ld	[%i1-4],%f11
	add	%i1,4,%i1		C s1_ptr++
	add	%g3,%g1,%g4		C p += cy
	srlx	%g4,32,%g3
	ldx	[%fp-25],%g2		C p16
	fmuld	%f2,%f8,%f16
	ldx	[%fp-17],%g1		C p0
	fmuld	%f2,%f6,%f4
	sllx	%g2,16,%g2		C align p16
	st	%g4,[%i0-4+DHI]
	fdtox	%f16,%f14
	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	std	%f14,[%fp-25]
	fdtox	%f4,%f12
	std	%f12,[%fp-17]
	brnz,pt	%i2,E(loop)
	add	%i0,4,%i0		C res_ptr++
C END LOOP
E(loope):
E(end4):
	fxtod	%f10,%f2
	add	%g3,%g1,%g4		C p += cy
	srlx	%g4,32,%g3
	ldx	[%fp-41],%g2		C p16
	fmuld	%f2,%f8,%f16
	ldx	[%fp-33],%g1		C p0
	fmuld	%f2,%f6,%f4
	sllx	%g2,16,%g2		C align p16
	st	%g4,[%i0-4+DLO]
	fdtox	%f16,%f14
	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	std	%f14,[%fp-41]
	fdtox	%f4,%f12
	std	%f12,[%fp-33]
	add	%i0,4,%i0		C res_ptr++

	add	%g3,%g1,%g4		C p += cy
	srlx	%g4,32,%g3
	ldx	[%fp-25],%g2		C p16
	ldx	[%fp-17],%g1		C p0
	sllx	%g2,16,%g2		C align p16
	st	%g4,[%i0-4+DHI]
	b,a	E(yyy)

E(end2):
	fxtod	%f10,%f2
	fmuld	%f2,%f8,%f16
	fmuld	%f2,%f6,%f4
	fdtox	%f16,%f14
	std	%f14,[%fp-41]
	fdtox	%f4,%f12
	std	%f12,[%fp-33]
	ldx	[%fp-25],%g2		C p16
	ldx	[%fp-17],%g1		C p0
	sllx	%g2,16,%g2		C align p16
E(yyy):	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	add	%i0,4,%i0		C res_ptr++

	add	%g3,%g1,%g4		C p += cy
	srlx	%g4,32,%g3
	ldx	[%fp-41],%g2		C p16
	ldx	[%fp-33],%g1		C p0
	sllx	%g2,16,%g2		C align p16
	st	%g4,[%i0-4+DLO]
	add	%g2,%g1,%g1		C add p16 to p0 (ADD1)
	add	%i0,4,%i0		C res_ptr++

	add	%g3,%g1,%g4		C p += cy
	st	%g4,[%i0-4+DHI]
	srlx	%g4,32,%g4

	ret
	restore %g0,%g4,%o0		C sideeffect: put cy in retreg
EPILOGUE(mull)
