dnl  AMD K7 mpn_mul_1 -- mpn by limb multiply.
dnl 
dnl  K7: 3.4 cycles/limb (at 16 limbs/loop).


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


dnl  K7: UNROLL_COUNT cycles/limb
dnl           8           3.9
dnl          16           3.4
dnl          32           3.4
dnl          64           3.35
dnl  Maximum possible with the current code is 64.

deflit(UNROLL_COUNT, 16)


C mp_limb_t mpn_mul_1 (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                      mp_limb_t multiplier);
C mp_limb_t mpn_mul_1c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                       mp_limb_t multiplier, mp_limb_t carry);
C
C Multiply src,size by mult and store the result in dst,size.
C Return the carry limb from the top of the result.
C
C mpn_mul_1c() accepts an initial carry for the calculation, it's added into
C the low limb of the destination.
C
C Variations on the unrolled loop have been tried, with the current
C registers or with the counter on the stack to free up ecx.  The current
C code is the fastest found.
C
C An interesting effect is that removing the stores "movl %ebx, disp0(%edi)"
C from the unrolled loop actually slows it down to 5.0 cycles/limb.  Code
C with this change can be tested on sizes of the form UNROLL_COUNT*n+1
C without having to change the computed jump.  There's obviously something
C fishy going on, perhaps with what execution units the mul needs.

defframe(PARAM_CARRY,     20)
defframe(PARAM_MULTIPLIER,16)
defframe(PARAM_SIZE,      12)
defframe(PARAM_SRC,       8)
defframe(PARAM_DST,       4)

defframe(SAVE_EBP, -4)
defframe(SAVE_EDI, -8)
defframe(SAVE_ESI, -12)
defframe(SAVE_EBX, -16)
deflit(STACK_SPACE, 16)

dnl  Must have UNROLL_THRESHOLD >= 2, since the unrolled loop can't handle 1.
ifdef(`PIC',`
deflit(UNROLL_THRESHOLD, 7)
',`
deflit(UNROLL_THRESHOLD, 5)
')

	.text
	ALIGN(32)
PROLOGUE(mpn_mul_1c)
deflit(`FRAME',0)
	movl	PARAM_CARRY, %edx
	jmp	LF(mpn_mul_1,start_nc)
EPILOGUE()


PROLOGUE(mpn_mul_1)
deflit(`FRAME',0)
	xorl	%edx, %edx	C initial carry
L(start_nc):
	movl	PARAM_SIZE, %ecx
	subl	$STACK_SPACE, %esp
deflit(`FRAME', STACK_SPACE)

	movl	%edi, SAVE_EDI
	movl	%ebx, SAVE_EBX
	movl	%edx, %ebx

	movl	%esi, SAVE_ESI
	movl	PARAM_SRC, %esi
	cmpl	$UNROLL_THRESHOLD, %ecx

	movl	PARAM_DST, %edi
	movl	%ebp, SAVE_EBP
	jae	L(unroll)

	leal	(%esi,%ecx,4), %esi
	leal	(%edi,%ecx,4), %edi
	negl	%ecx

	movl	PARAM_MULTIPLIER, %ebp

L(simple):
	C eax	scratch
	C ebx	carry
	C ecx	counter (negative)
	C edx	scratch
	C esi	src
	C edi	dst
	C ebp	multiplier

	movl	(%esi,%ecx,4), %eax

	mull	%ebp

	addl	%ebx, %eax
	movl	%eax, (%edi,%ecx,4)
	movl	$0, %ebx

	adcl	%edx, %ebx
	incl	%ecx
	jnz	L(simple)

	movl	%ebx, %eax
	movl	SAVE_EBX, %ebx
	movl	SAVE_ESI, %esi

	movl	SAVE_EDI, %edi
	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	ret


C -----------------------------------------------------------------------------
C The mov to load the next source limb is done well ahead of the mul, this
C is necessary for full speed.  It leads to one limb handled separately
C after the loop.
C
C When unrolling to 32 or more, an offset of +4 is used on the src pointer,
C to avoid having an 0x80 displacement in the code for the last limb in the
C unrolled loop.  This is for a fair comparison between 16 and 32 unrolling.

ifelse(eval(UNROLL_COUNT >= 32),1,`
deflit(SRC_OFFSET,4)
',`
deflit(SRC_OFFSET,)
')

	C this is offset 0x62, so close enough to aligned
L(unroll):
	C eax
	C ebx	initial carry
	C ecx	size
	C edx
	C esi	src
	C edi	dst
	C ebp
deflit(`FRAME', STACK_SPACE)

	leal	-1(%ecx), %edx	C one limb handled at end
	leal	-2(%ecx), %ecx	C and ecx is one less than edx
	movl	%ebp, SAVE_EBP

	negl	%edx
	shrl	$UNROLL_LOG2, %ecx	C unrolled loop counter
	movl	(%esi), %eax		C src low limb

	andl	$UNROLL_MASK, %edx
	movl	PARAM_DST, %edi

	movl	%edx, %ebp
	shll	$4, %edx

	C 17 code bytes per limb
ifdef(`PIC',`
	call	L(add_eip_to_edx)
L(here):
',`
	leal	L(entry) (%edx,%ebp), %edx
')
	negl	%ebp

	leal	ifelse(UNROLL_BYTES,256,128+) SRC_OFFSET(%esi,%ebp,4), %esi
	leal	ifelse(UNROLL_BYTES,256,128) (%edi,%ebp,4), %edi
	movl	PARAM_MULTIPLIER, %ebp

	jmp	*%edx


ifdef(`PIC',`
L(add_eip_to_edx):
	C See README.family about old gas bugs
	leal	(%edx,%ebp), %edx
	addl	$L(entry)-L(here), %edx
	addl	(%esp), %edx
	ret
')


C ----------------------------------------------------------------------------
	ALIGN(32)
L(top):
	C eax	next src limb
	C ebx	carry
	C ecx	counter
	C edx	scratch
	C esi	src+4
	C edi	dst
	C ebp	multiplier
	C
	C 17 code bytes per limb processed

L(entry):
forloop(i, 0, UNROLL_COUNT-1, `
	deflit(`disp_dst', eval(i*4 ifelse(UNROLL_BYTES,256,-128)))
	deflit(`disp_src', eval(disp_dst + 4-(SRC_OFFSET-0)))
 
	mull	%ebp
	
	addl	%eax, %ebx
Zdisp(	movl,	disp_src,(%esi), %eax)
Zdisp(	movl,	%ebx, disp_dst,(%edi))

	movl	$0, %ebx
	adcl	%edx, %ebx
')

	decl	%ecx

	leal	UNROLL_BYTES(%esi), %esi
	leal	UNROLL_BYTES(%edi), %edi
	jns	L(top)


deflit(`disp0', ifelse(UNROLL_BYTES,256,-128))

	mull	%ebp
	
	addl	%eax, %ebx
	movl	$0, %eax
	movl	SAVE_ESI, %esi

	movl	%ebx, disp0(%edi)
	movl	SAVE_EBX, %ebx
	movl	SAVE_EDI, %edi

	adcl	%edx, %eax
	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	ret
	
EPILOGUE()
