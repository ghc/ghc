dnl  AMD K7 mpn_mod_1 -- mpn by limb remainder.
dnl 
dnl  K7: 17.0 cycles/limb.


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
C The code here is the same as mpn_divrem_1, but with the quotient
C discarded.  See mpn/x86/k7/mmx/divrem_1.c for some comments.


dnl  MUL_THRESHOLD is the size at which the multiply by inverse method is
dnl  used, rather than plain "divl"s.  Minimum value 2.
dnl
dnl  The inverse takes about 50 cycles to calculate, but after that the
dnl  multiply is 17 c/l versus division at 41 c/l.
dnl
dnl  Using mul or div is about the same speed at 3 limbs, so the threshold
dnl  is set to 4 to get the smaller div code used at 3.

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
	ALIGN(32)

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


	ALIGN(32)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	movl	$0, %edx		C initial carry (if can't skip a div)
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



C With a MUL_THRESHOLD of 4, this "loop" only ever does 1 to 3 iterations,
C but it's already fast and compact, and there's nothing to gain by
C expanding it out.
C
C Using PARAM_DIVISOR in the divl is a couple of cycles faster than %ebp.

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

	divl	PARAM_DIVISOR

	decl	%ecx
	jnz	L(divide_top)


L(divide_done):
	movl	SAVE_ESI, %esi
	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	movl	%edx, %eax

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

	bsrl	%ebp, %eax		C 31-l

	movl	%ebx, SAVE_EBX
	leal	-4(%esi), %ebx

	movl	%ebx, VAR_SRC_STOP
	movl	%edi, SAVE_EDI

	movl	%ecx, %ebx		C size
	movl	$31, %ecx

	movl	%edx, %edi		C carry
	movl	$-1, %edx

	C

	xorl	%eax, %ecx		C l
	incl	%eax			C 32-l

	shll	%cl, %ebp		C d normalized
	movl	%ecx, VAR_NORM

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
C instructions are saved by not needing to store the quotient limbs.
C Unfortunately this doesn't get the code down to the theoretical 16 c/l.
C
C There's four dummy instructions in the loop, all of which are necessary
C for the claimed 17 c/l.  It's a 1 to 3 cycle slowdown if any are removed,
C or changed from load to store or vice versa.  They're not completely
C random, since they correspond to what mpn_divrem_1 has, but there's no
C obvious reason why they're necessary.  Presumably they induce something
C good in the out of order execution, perhaps through some load/store
C ordering and/or decoding effects.
C
C The q1==0xFFFFFFFF case is handled here the same as in mpn_divrem_1.  On
C on special data that comes out as q1==0xFFFFFFFF always, the loop runs at
C about 13.5 c/l.

	ALIGN(32)
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

	cmpl	$0x80000000, %esi  C n1 as 0=c, 1=nc
	movl	%edi, %eax         C n2
	movl	PARAM_SIZE, %ebx   C dummy

	leal	(%ebp,%esi), %ebx
	cmovc(	%esi, %ebx)	   C nadj = n10 + (-n1 & d), ignoring overflow
	sbbl	$-1, %eax          C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movq	(%ecx), %mm0       C next src limb and the one below it
	subl	$4, %ecx

	movl	%ecx, PARAM_SIZE   C dummy

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))
	movl	%ebp, %eax	   C d

	C

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1
	jz	L(q1_ff)
	nop                        C dummy

	mull	%ebx		   C (q1+1)*d

	psrlq	%mm7, %mm0
	leal	0(%ecx), %ecx      C dummy

	C

	C

	subl	%eax, %esi
	movl	VAR_SRC_STOP, %eax

	C

	sbbl	%edx, %edi	   C n - (q1+1)*d
	movl	%esi, %edi	   C remainder -> n2
	leal	(%ebp,%esi), %edx

	movd	%mm0, %esi

	cmovc(	%edx, %edi)	   C n - q1*d if underflow from using q1+1
	cmpl	%eax, %ecx
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

	cmpl	$0x80000000, %esi  C n1 as 0=c, 1=nc
	movl	%edi, %eax         C n2

	leal	(%ebp,%esi), %ebx
	cmovc(	%esi, %ebx)	   C nadj = n10 + (-n1 & d), ignoring overflow
	sbbl	$-1, %eax          C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movd	4(%ecx), %mm0	   C src low limb

	C

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))
	movl	%ebp, %eax	   C d

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %ebx

	mull	%ebx		   C (q1+1)*d

	psllq	$32, %mm0

	psrlq	%mm7, %mm0

	C

	subl	%eax, %esi

	C

	sbbl	%edx, %edi	   C n - (q1+1)*d
	movl	%esi, %edi	   C remainder -> n2
	leal	(%ebp,%esi), %edx

	movd	%mm0, %esi

	cmovc(	%edx, %edi)	   C n - q1*d if underflow from using q1+1


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

	cmpl	$0x80000000, %esi  C n1 as 0=c, 1=nc
	movl	%edi, %eax         C n2

	leal	(%ebp,%esi), %ebx
	cmovc(	%esi, %ebx)	   C nadj = n10 + (-n1 & d), ignoring overflow
	sbbl	$-1, %eax          C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	movl	VAR_NORM, %ecx     C for final denorm

	C

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2<<32 + m*(n2+n1))
	movl	%ebp, %eax	   C d

	C

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %ebx

	mull	%ebx		   C (q1+1)*d

	movl	SAVE_EBX, %ebx

	C

	C

	subl	%eax, %esi

	movl	%esi, %eax	   C remainder
	movl	SAVE_ESI, %esi

	sbbl	%edx, %edi	   C n - (q1+1)*d
	leal	(%ebp,%eax), %edx
	movl	SAVE_EBP, %ebp

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

	movl	VAR_SRC_STOP, %edx
	leal	(%ebp,%esi), %edi	C n-q*d remainder -> next n2
	psrlq	%mm7, %mm0

	movd	%mm0, %esi		C next n10

	cmpl	%ecx, %edx
	jne	L(inverse_top)
	jmp	L(inverse_loop_done)

EPILOGUE()
