dnl  Intel Pentium-II mpn_mod_1 -- mpn by limb remainder.
dnl 
dnl  P6MMX: 24.0 cycles/limb.


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


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C
C The code here very similar to mpn_divrem_1, but with the quotient
C discarded.  What's here probably isn't optimal.
C
C See mpn/x86/p6/mmx/divrem_1.c and mpn/x86/k7/mmx/mod_1.asm for some
C comments.


dnl  MUL_THRESHOLD is the size at which the multiply by inverse method is
dnl  used, rather than plain "divl"s.  Minimum value 2.

deflit(MUL_THRESHOLD, 4)


defframe(PARAM_CARRY,  16)
defframe(PARAM_DIVISOR,12)
defframe(PARAM_SIZE,    8)
defframe(PARAM_SRC,     4)

defframe(SAVE_EBX,    -4)
defframe(SAVE_ESI,    -8)
defframe(SAVE_EDI,    -12)
defframe(SAVE_EBP,    -16)

defframe(VAR_NORM,    -20)
defframe(VAR_INVERSE, -24)
defframe(VAR_SRC_STOP,-28)

deflit(STACK_SPACE, 28)

	.text
	ALIGN(16)

PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)
	movl	PARAM_CARRY, %edx
	movl	PARAM_SIZE, %ecx
	subl	$STACK_SPACE, %esp
deflit(`FRAME',STACK_SPACE)

	movl	%ebp, SAVE_EBP
	movl	PARAM_DIVISOR, %ebp

	movl	%esi, SAVE_ESI
	movl	PARAM_SRC, %esi
	jmp	LF(mpn_mod_1,start_1c)

EPILOGUE()


	ALIGN(16)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	$0, %edx		C initial carry (if can't skip a div)
	movl	PARAM_SIZE, %ecx
	subl	$STACK_SPACE, %esp
deflit(`FRAME',STACK_SPACE)

	movl	%esi, SAVE_ESI
	movl	PARAM_SRC, %esi

	movl	%ebp, SAVE_EBP
	movl	PARAM_DIVISOR, %ebp

	orl	%ecx, %ecx
	jz	L(divide_done)

	movl	-4(%esi,%ecx,4), %eax	C src high limb

	cmpl	%ebp, %eax		C carry flag if high<divisor
					
	cmovc(	%eax, %edx)		C src high limb as initial carry
	sbbl	$0, %ecx		C size-1 to skip one div
	jz	L(divide_done)


	ALIGN(16)
L(start_1c):
	C eax	
	C ebx
	C ecx	size
	C edx	carry
	C esi	src
	C edi
	C ebp	divisor

	cmpl	$MUL_THRESHOLD, %ecx
	jae	L(mul_by_inverse)


	orl	%ecx, %ecx
	jz	L(divide_done)


L(divide_top):
	C eax	scratch (quotient)
	C ebx
	C ecx	counter, limbs, decrementing
	C edx	scratch (remainder)
	C esi	src
	C edi
	C ebp

	movl	-4(%esi,%ecx,4), %eax

	divl	%ebp

	decl	%ecx
	jnz	L(divide_top)


L(divide_done):
	movl	SAVE_ESI, %esi
	movl	%edx, %eax

	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	ret



C -----------------------------------------------------------------------------

L(mul_by_inverse):
	C eax
	C ebx
	C ecx	size
	C edx	carry
	C esi	src
	C edi
	C ebp	divisor

	movl	%ebx, SAVE_EBX
	leal	-4(%esi), %ebx

	movl	%ebx, VAR_SRC_STOP
	movl	%ecx, %ebx		C size

	movl	%edi, SAVE_EDI
	movl	%edx, %edi		C carry

	bsrl	%ebp, %ecx		C 31-l
	movl	$-1, %edx

	leal	1(%ecx), %eax		C 32-l
	xorl	$31, %ecx		C l

	movl	%ecx, VAR_NORM
	shll	%cl, %ebp		C d normalized

	movd	%eax, %mm7
	movl	$-1, %eax
	subl	%ebp, %edx		C (b-d)-1 so  edx:eax = b*(b-d)-1

	divl	%ebp			C floor (b*(b-d)-1) / d

	C

	movl	%eax, VAR_INVERSE
	leal	-12(%esi,%ebx,4), %eax	C &src[size-3]

	movl	8(%eax), %esi		C src high limb
	movl	4(%eax), %edx		C src second highest limb

	shldl(	%cl, %esi, %edi)	C n2 = carry,high << l

	shldl(	%cl, %edx, %esi)	C n10 = high,second << l

	movl	%eax, %ecx		C &src[size-3]


ifelse(MUL_THRESHOLD,2,`
	cmpl	$2, %ebx
	je	L(inverse_two_left)
')


C The dependent chain here is the same as in mpn_divrem_1, but a few
C instructions are saved by not needing to store the quotient limbs.  This
C gets it down to 24 c/l, which is still a bit away from a theoretical 19
C c/l.

	ALIGN(16)
L(inverse_top):
	C eax	scratch
	C ebx	scratch (nadj, q1)
	C ecx	src pointer, decrementing
	C edx	scratch
	C esi	n10
	C edi	n2
	C ebp	divisor
	C
	C mm0	scratch (src qword)
	C mm7	rshift for normalization


	movl	%esi, %eax
	movl	%ebp, %ebx

	sarl	$31, %eax          C -n1

	andl	%eax, %ebx         C -n1 & d
	negl	%eax               C n1

	addl	%esi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow
	addl	%edi, %eax         C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movq	(%ecx), %mm0       C next src limb and the one below it
	subl	$4, %ecx

	C

	C

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))
	movl	%ebp, %eax	   C d

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1
	jz	L(q1_ff)

	mull	%ebx		   C (q1+1)*d

	psrlq	%mm7, %mm0
	movl	VAR_SRC_STOP, %ebx

	C

	C

	C

	subl	%eax, %esi

	sbbl	%edx, %edi	   C n - (q1+1)*d
	movl	%esi, %edi	   C remainder -> n2
	leal	(%ebp,%esi), %edx

	cmovc(	%edx, %edi)	   C n - q1*d if underflow from using q1+1
	movd	%mm0, %esi
	cmpl	%ebx, %ecx

	jne	L(inverse_top)


L(inverse_loop_done):


C -----------------------------------------------------------------------------

L(inverse_two_left):
	C eax	scratch
	C ebx	scratch (nadj, q1)
	C ecx	&src[-1]
	C edx	scratch
	C esi	n10
	C edi	n2
	C ebp	divisor
	C
	C mm0	scratch (src dword)
	C mm7	rshift

	movl	%esi, %eax
	movl	%ebp, %ebx

	sarl	$31, %eax          C -n1

	andl	%eax, %ebx         C -n1 & d
	negl	%eax               C n1

	addl	%esi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow
	addl	%edi, %eax         C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movd	4(%ecx), %mm0	   C src low limb

	C

	C

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %ebx
	movl	%ebp, %eax	   C d

	mull	%ebx		   C (q1+1)*d

	psllq	$32, %mm0

	psrlq	%mm7, %mm0

	C

	C

	subl	%eax, %esi

	sbbl	%edx, %edi	   C n - (q1+1)*d
	movl	%esi, %edi	   C remainder -> n2
	leal	(%ebp,%esi), %edx

	cmovc(	%edx, %edi)	   C n - q1*d if underflow from using q1+1
	movd	%mm0, %esi


C One limb left

	C eax	scratch
	C ebx	scratch (nadj, q1)
	C ecx
	C edx	scratch
	C esi	n10
	C edi	n2
	C ebp	divisor
	C
	C mm0	src limb, shifted
	C mm7	rshift

	movl	%esi, %eax
	movl	%ebp, %ebx

	sarl	$31, %eax          C -n1

	andl	%eax, %ebx         C -n1 & d
	negl	%eax               C n1

	addl	%esi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow
	addl	%edi, %eax         C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movl	VAR_NORM, %ecx     C for final denorm

	C

	C

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %ebx
	movl	%ebp, %eax	   C d

	mull	%ebx		   C (q1+1)*d

	movl	SAVE_EBX, %ebx

	C

	C

	C

	subl	%eax, %esi

	sbbl	%edx, %edi	   C n - (q1+1)*d
	leal	(%ebp,%esi), %edx
	movl	SAVE_EBP, %ebp

	movl	%esi, %eax	   C remainder
	movl	SAVE_ESI, %esi

	cmovc(	%edx, %eax)	   C n - q1*d if underflow from using q1+1
	movl	SAVE_EDI, %edi

	shrl	%cl, %eax	   C denorm remainder
	addl	$STACK_SPACE, %esp
	emms

	ret


C -----------------------------------------------------------------------------
C
C Special case for q1=0xFFFFFFFF, giving q=0xFFFFFFFF meaning the low dword
C of q*d is simply -d and the remainder n-q*d = n10+d

L(q1_ff):
	C eax	(divisor)
	C ebx	(q1+1 == 0)
	C ecx	src pointer
	C edx
	C esi	n10
	C edi	(n2)
	C ebp	divisor

	leal	(%ebp,%esi), %edi	C n-q*d remainder -> next n2
	movl	VAR_SRC_STOP, %edx
	psrlq	%mm7, %mm0

	movd	%mm0, %esi		C next n10
	cmpl	%ecx, %edx
	jne	L(inverse_top)

	jmp	L(inverse_loop_done)

EPILOGUE()
