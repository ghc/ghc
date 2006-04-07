dnl  x86 mpn_copyd -- copy limb vector, decrementing.
dnl 
dnl  Future: On P6 an MMX loop should be able to go faster than this code.


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


include(`../config.m4')


C void mpn_copyd (mp_ptr dst, mp_srcptr src, mp_size_t size);
C
C Copy src,size to dst,size, working from high to low addresses.
C
C The code here is very generic and can be expected to be reasonable on all
C the x86 family.
C
C P5 - 1.0 cycles/limb.
C
C P6 - 2.4 cycles/limb, approx 40 cycles startup.

defframe(PARAM_SIZE,12)
defframe(PARAM_SRC, 8)
defframe(PARAM_DST, 4)
deflit(`FRAME',0)

	.text
	ALIGN(32)

PROLOGUE(mpn_copyd)
	C eax	saved esi
	C ebx
	C ecx	counter
	C edx	saved edi
	C esi	src
	C edi	dst
	C ebp

	movl	PARAM_SIZE, %ecx
	movl	%esi, %eax

	movl	PARAM_SRC, %esi
	movl	%edi, %edx

	movl	PARAM_DST, %edi
	leal	-4(%esi,%ecx,4), %esi

	leal	-4(%edi,%ecx,4), %edi
	
	std

	rep
	movsl

	cld

	movl	%eax, %esi
	movl	%edx, %edi

	ret

EPILOGUE()
