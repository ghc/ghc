dnl  x86 mpn_mod_1 -- mpn by limb remainder.


dnl  Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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


dnl        cycles/limb
dnl  K6        20
dnl  P5        44
dnl  P6        39
dnl  486   approx 42 maybe
dnl
dnl  The following have their own optimized mod_1 implementations, but for
dnl  reference the code here runs as follows.
dnl
dnl  P6MMX     39
dnl  K7        41


include(`../config.m4')


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C
C Divide src,size by divisor and return the remainder.  The quotient is
C discarded.
C
C See mpn/x86/divrem_1.asm for some comments.

defframe(PARAM_CARRY,  16)
defframe(PARAM_DIVISOR,12)
defframe(PARAM_SIZE,   8)
defframe(PARAM_SRC,    4)

	.text
	ALIGN(16)

PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%ebx		FRAME_pushl()

	movl	PARAM_SRC, %ebx
	pushl	%esi		FRAME_pushl()

	movl	PARAM_DIVISOR, %esi
	orl	%ecx, %ecx

	movl	PARAM_CARRY, %edx
	jnz	LF(mpn_mod_1,top)

	popl	%esi
	movl	%edx, %eax

	popl	%ebx
	
	ret

EPILOGUE()


PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%ebx		FRAME_pushl()

	movl	PARAM_SRC, %ebx
	pushl	%esi		FRAME_pushl()

	orl	%ecx, %ecx
	jz	L(done_zero)

	movl	PARAM_DIVISOR, %esi
	movl	-4(%ebx,%ecx,4), %eax	C src high limb

	cmpl	%esi, %eax

	sbbl	%edx, %edx		C -1 if high<divisor

	addl	%edx, %ecx		C skip one division if high<divisor
	jz	L(done_eax)

	andl	%eax, %edx		C carry if high<divisor


L(top):
	C eax	scratch (quotient)
	C ebx	src
	C ecx	counter
	C edx	carry (remainder)
	C esi	divisor
	C edi
	C ebp

	movl	-4(%ebx,%ecx,4), %eax

	divl	%esi

	loop_or_decljnz	L(top)


	movl	%edx, %eax
L(done_eax):
	popl	%esi

	popl	%ebx

	ret


L(done_zero):
	popl	%esi
	xorl	%eax, %eax

	popl	%ebx

	ret
	

EPILOGUE()
