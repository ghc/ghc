dnl  AMD K6-2 mpn_popcount, mpn_hamdist -- mpn bit population count and
dnl  hamming distance.
dnl
dnl         popcount  hamdist
dnl  K6-2:    9.0       11.5   cycles/limb
dnl  K6:      12.5      13.0


dnl  Copyright (C) 2000 Free Software Foundation, Inc.
dnl 
dnl  This file is part of the GNU MP Library.
dnl 
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl 
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl 
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 59 Temple Place -
dnl  Suite 330, Boston, MA 02111-1307, USA.


include(`../config.m4')


C unsigned long mpn_popcount (mp_srcptr src, mp_size_t size);
C unsigned long mpn_hamdist (mp_srcptr src, mp_srcptr src2, mp_size_t size);
C
C The code here isn't optimal, but it's already a 2x speedup over the plain
C integer mpn/generic/popcount.c,hamdist.c.


ifdef(`OPERATION_popcount',,
`ifdef(`OPERATION_hamdist',,
`m4_error(`Need OPERATION_popcount or OPERATION_hamdist
')m4exit(1)')')

define(HAM,
m4_assert_numargs(1)
`ifdef(`OPERATION_hamdist',`$1')')

define(POP,
m4_assert_numargs(1)
`ifdef(`OPERATION_popcount',`$1')')

HAM(`
defframe(PARAM_SIZE,   12)
defframe(PARAM_SRC2,   8)
defframe(PARAM_SRC,    4)
define(M4_function,mpn_hamdist)
')
POP(`
defframe(PARAM_SIZE,   8)
defframe(PARAM_SRC,    4)
define(M4_function,mpn_popcount)
')

MULFUNC_PROLOGUE(mpn_popcount mpn_hamdist)


ifdef(`PIC',,`
	dnl  non-PIC

	DATA
	ALIGN(8)

define(LS,
m4_assert_numargs(1)
`LF(M4_function,`$1')')

LS(rodata_AAAAAAAAAAAAAAAA):
	.long	0xAAAAAAAA
	.long	0xAAAAAAAA

LS(rodata_3333333333333333):
	.long	0x33333333
	.long	0x33333333

LS(rodata_0F0F0F0F0F0F0F0F):
	.long	0x0F0F0F0F
	.long	0x0F0F0F0F

LS(rodata_000000FF000000FF):
	.long	0x000000FF
	.long	0x000000FF
')

	.text
	ALIGN(32)

POP(`ifdef(`PIC', `
	C avoid shrl crossing a 32-byte boundary
	nop')')

PROLOGUE(M4_function)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	orl	%ecx, %ecx
	jz	L(zero)

ifdef(`PIC',`
	movl	$0xAAAAAAAA, %eax
	movl	$0x33333333, %edx

	movd	%eax, %mm7
	movd	%edx, %mm6

	movl	$0x0F0F0F0F, %eax
	movl	$0x000000FF, %edx

	punpckldq %mm7, %mm7
	punpckldq %mm6, %mm6

	movd	%eax, %mm5
	movd	%edx, %mm4

	punpckldq %mm5, %mm5
	punpckldq %mm4, %mm4
',`

	movq	LS(rodata_AAAAAAAAAAAAAAAA), %mm7
	movq	LS(rodata_3333333333333333), %mm6
	movq	LS(rodata_0F0F0F0F0F0F0F0F), %mm5
	movq	LS(rodata_000000FF000000FF), %mm4
')

define(REG_AAAAAAAAAAAAAAAA, %mm7)
define(REG_3333333333333333, %mm6)
define(REG_0F0F0F0F0F0F0F0F, %mm5)
define(REG_000000FF000000FF, %mm4)


	movl	PARAM_SRC, %eax
HAM(`	movl	PARAM_SRC2, %edx')

	pxor	%mm2, %mm2	C total

	shrl	%ecx
	jnc	L(top)

Zdisp(	movd,	0,(%eax,%ecx,8), %mm1)

HAM(`
Zdisp(	movd,	0,(%edx,%ecx,8), %mm0)
	pxor	%mm0, %mm1
')

	incl	%ecx
	jmp	L(loaded)


	ALIGN(16)
POP(`	nop	C alignment to avoid crossing 32-byte boundaries')

L(top):
	C eax	src
	C ebx
	C ecx	counter, qwords, decrementing
	C edx	[hamdist] src2
	C
	C mm0	(scratch)
	C mm1	(scratch)
	C mm2	total (low dword)
	C mm3
	C mm4	\
	C mm5	| special constants
	C mm6	|
	C mm7	/

	movq	-8(%eax,%ecx,8), %mm1
HAM(`	pxor	-8(%edx,%ecx,8), %mm1')

L(loaded):
	movq	%mm1, %mm0
	pand	REG_AAAAAAAAAAAAAAAA, %mm1

	psrlq	$1, %mm1
HAM(`	nop			C code alignment')

	psubd	%mm1, %mm0	C bit pairs
HAM(`	nop			C code alignment')


	movq	%mm0, %mm1
	psrlq	$2, %mm0

	pand	REG_3333333333333333, %mm0
	pand	REG_3333333333333333, %mm1

	paddd	%mm1, %mm0	C nibbles


	movq	%mm0, %mm1
	psrlq	$4, %mm0

	pand	REG_0F0F0F0F0F0F0F0F, %mm0
	pand	REG_0F0F0F0F0F0F0F0F, %mm1

	paddd	%mm1, %mm0	C bytes

	movq	%mm0, %mm1
	psrlq	$8, %mm0


	paddb	%mm1, %mm0	C words


	movq	%mm0, %mm1
	psrlq	$16, %mm0

	paddd	%mm1, %mm0	C dwords

	pand	REG_000000FF000000FF, %mm0

	paddd	%mm0, %mm2	C low to total
	psrlq	$32, %mm0

	paddd	%mm0, %mm2	C high to total
	loop	L(top)



	movd	%mm2, %eax
	emms_or_femms
	ret

L(zero):
	movl	$0, %eax
	ret

EPILOGUE()
