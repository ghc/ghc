dnl  SPARC 64-bit mpn_submul_1 -- Multiply a limb vector with a limb and
dnl  subtract the result from a second limb vector.

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

include(`../config.m4')

C INPUT PARAMETERS
C res_ptr	i0
C s1_ptr	i1
C size		i2
C s2_limb	i3

ASM_START()
	.register	%g2,#scratch
	.register	%g3,#scratch

PROLOGUE(mpn_submul_1)
	save	%sp,-256,%sp

C We store 0.0 in f10 and keep it invariant accross thw two
C function calls below.  Note that this is not ABI conformant,
C but since the functions are local, that's acceptable.
ifdef(`PIC',
`L(pc):	rd	%pc,%o7
	ld	[%o7+L(noll)-L(pc)],%f10',
`	sethi	%hh(L(noll)),%g2
	sethi	%lm(L(noll)),%g1
	or	%g2,%hm(L(noll)),%g2
	or	%g1,%lo(L(noll)),%g1
	sllx	%g2,32,%g2
	ld	[%g1+%g2],%f10')

	sub	%i1,%i0,%g1
	srlx	%g1,3,%g1
	cmp	%g1,%i2
	bcc,pt	%xcc,L(nooverlap)
	nop

	sllx	%i2,3,%g2		C compute stack allocation byte count
	add	%g2,15,%o0
	and	%o0,-16,%o0
	sub	%sp,%o0,%sp
	add	%sp,2223,%o0

	mov	%i1,%o1			C copy s1_ptr to mpn_copyi's srcp
	call	mpn_copyi
	mov	%i2,%o2			C copy n to mpn_copyi's count parameter

	add	%sp,2223,%i1

L(nooverlap):
C First multiply-add with low 32 bits of s2_limb
	mov	%i0,%o0
	mov	%i1,%o1
	add	%i2,%i2,%o2
	call	submull
	srl	%i3,0,%o3

	mov	%o0,%l0			C keep carry-out from accmull

C Now multiply-add with high 32 bits of s2_limb, unless it is zero.
	srlx	%i3,32,%o3
	brz,a,pn	%o3,L(small)
	 mov	%o0,%i0
	mov	%i1,%o1
	add	%i2,%i2,%o2
	call	submulu
	add	%i0,4,%o0

	add	%l0,%o0,%i0
L(small):
	ret
	restore	%g0,%g0,%g0
EPILOGUE(mpn_submul_1)

C Put a zero in the text segment to allow us to t the address
C quickly when compiling for PIC
	TEXT
	ALIGN(4)
L(noll):
	.word	0

define(`LO',`(+4)')
define(`HI',`(-4)')

define(`DLO',`(+4)')
define(`DHI',`(-4)')
define(`LOWPART')
define(`E',`L(l.$1)')
include_mpn(`sparc64/submul1h.asm')

define(`DLO',`(-4)')
define(`DHI',`(+4)')
undefine(`LOWPART')
define(`E',`L(u.$1)')
include_mpn(`sparc64/submul1h.asm')
