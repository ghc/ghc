dnl  AMD K6-2 mpn_copyd -- copy limb vector, decrementing.
dnl 
dnl  K6-2: 0.56 or 1.0 cycles/limb (at 32 limbs/loop), depending on data
dnl  alignment.


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


dnl  K6-2 aligned:
dnl  UNROLL_COUNT cycles/limb
dnl        8          0.75
dnl       16          0.625
dnl       32          0.5625
dnl       64          0.53
dnl  Maximum possible with the current code is 64, the minimum is 2.

deflit(UNROLL_COUNT, 32)


C void mpn_copyd (mp_ptr dst, mp_srcptr src, mp_size_t size);
C
C Copy src,size to dst,size, processing limbs from high to low addresses.
C
C The comments in copyi.asm apply here too.


defframe(PARAM_SIZE,12)
defframe(PARAM_SRC, 8)
defframe(PARAM_DST, 4)
deflit(`FRAME',0)

	.text
	ALIGN(32)

PROLOGUE(mpn_copyd)
	movl	PARAM_SIZE, %ecx
	movl	%esi, %eax

	movl	PARAM_SRC, %esi
	movl	%edi, %edx

	std

	movl	PARAM_DST, %edi
	cmpl	$UNROLL_COUNT, %ecx

	leal	-4(%esi,%ecx,4), %esi

	leal	-4(%edi,%ecx,4), %edi
	ja	L(unroll)

L(simple):
	rep
	movsl

	cld

	movl	%eax, %esi
	movl	%edx, %edi

	ret


L(unroll):
	C if src and dst are different alignments mod8, then use rep movs
	C if src and dst are both 4mod8 then process one limb to get 0mod8

	pushl	%ebx
	leal	(%esi,%edi), %ebx

	testb	$4, %bl
	popl	%ebx
	
	jnz	L(simple)
	testl	$4, %esi

	leal	-UNROLL_COUNT(%ecx), %ecx
	jnz	L(already_aligned)

	movsl

	decl	%ecx
L(already_aligned):


ifelse(UNROLL_BYTES,256,`
	subl	$128, %esi
	subl	$128, %edi
')

	C offset 0x3D here, but gets full speed without further alignment
L(top):
	C eax	saved esi
	C ebx
	C ecx	counter, limbs
	C edx	saved edi
	C esi	src, incrementing
	C edi	dst, incrementing
	C ebp
	C
	C `disp' is never 0, so don't need to force 0(%esi).

deflit(CHUNK_COUNT, 2)
forloop(`i', 0, UNROLL_COUNT/CHUNK_COUNT-1, `
	deflit(`disp', eval(-4-i*CHUNK_COUNT*4 ifelse(UNROLL_BYTES,256,+128)))
	movq	disp(%esi), %mm0
	movq	%mm0, disp(%edi)
')

	leal	-UNROLL_BYTES(%esi), %esi
	subl	$UNROLL_COUNT, %ecx

	leal	-UNROLL_BYTES(%edi), %edi
	jns	L(top)


	C now %ecx is -UNROLL_COUNT to -1 representing repectively 0 to
	C UNROLL_COUNT-1 limbs remaining

	testb	$eval(UNROLL_COUNT/2), %cl

	leal	UNROLL_COUNT(%ecx), %ecx
	jz	L(not_half)


	C at an unroll count of 32 this block of code is 16 cycles faster than
	C the rep movs, less 3 or 4 to test whether to do it

forloop(`i', 0, UNROLL_COUNT/CHUNK_COUNT/2-1, `
	deflit(`disp', eval(-4-i*CHUNK_COUNT*4 ifelse(UNROLL_BYTES,256,+128)))
	movq	disp(%esi), %mm0
	movq	%mm0, disp(%edi)
')

	subl	$eval(UNROLL_BYTES/2), %esi
	subl	$eval(UNROLL_BYTES/2), %edi

	subl	$eval(UNROLL_COUNT/2), %ecx
L(not_half):


ifelse(UNROLL_BYTES,256,`
	addl	$128, %esi
	addl	$128, %edi
')

	rep
	movsl

	cld

	movl	%eax, %esi
	movl	%edx, %edi

	femms
	ret

EPILOGUE()
