dnl  AMD K7 mpn_addmul_1/mpn_submul_1 -- add or subtract mpn multiple.
dnl 
dnl  K7: 3.9 cycles/limb.
dnl 
dnl  Future: It should be possible to avoid the separate mul after the
dnl  unrolled loop by moving the movl/adcl to the top.


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


dnl  K7: UNROLL_COUNT  cycles/limb
dnl           4            4.42
dnl           8            4.16
dnl          16            3.9
dnl          32            3.9
dnl          64            3.87
dnl  Maximum possible with the current code is 64.

deflit(UNROLL_COUNT, 16)


ifdef(`OPERATION_addmul_1',`
	define(M4_inst,        addl)
	define(M4_function_1,  mpn_addmul_1)
	define(M4_function_1c, mpn_addmul_1c)
	define(M4_description, add it to)
	define(M4_desc_retval, carry)
',`ifdef(`OPERATION_submul_1',`
	define(M4_inst,        subl)
	define(M4_function_1,  mpn_submul_1)
	define(M4_function_1c, mpn_submul_1c)
	define(M4_description, subtract it from)
	define(M4_desc_retval, borrow)
',`m4_error(`Need OPERATION_addmul_1 or OPERATION_submul_1
')')')

MULFUNC_PROLOGUE(mpn_addmul_1 mpn_addmul_1c mpn_submul_1 mpn_submul_1c)


C mp_limb_t M4_function_1 (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                            mp_limb_t mult);
C mp_limb_t M4_function_1c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                             mp_limb_t mult, mp_limb_t carry);
C
C Calculate src,size multiplied by mult and M4_description dst,size.
C Return the M4_desc_retval limb from the top of the result.

ifdef(`PIC',`
deflit(UNROLL_THRESHOLD, 9)
',`
deflit(UNROLL_THRESHOLD, 6)
')

defframe(PARAM_CARRY,     20)
defframe(PARAM_MULTIPLIER,16)
defframe(PARAM_SIZE,      12)
defframe(PARAM_SRC,       8)
defframe(PARAM_DST,       4)
deflit(`FRAME',0)

defframe(SAVE_EBX, -4)
defframe(SAVE_ESI, -8)
defframe(SAVE_EDI, -12)
defframe(SAVE_EBP, -16)
deflit(SAVE_SIZE, 16)

	.text
	ALIGN(32)
PROLOGUE(M4_function_1)
	movl	PARAM_SIZE, %edx
	movl	PARAM_SRC, %eax
	xorl	%ecx, %ecx

	decl	%edx
	jnz	LF(M4_function_1c,start_1)

	movl	(%eax), %eax
	movl	PARAM_DST, %ecx

	mull	PARAM_MULTIPLIER

	M4_inst	%eax, (%ecx)
	adcl	$0, %edx
	movl	%edx, %eax

	ret
EPILOGUE()

	ALIGN(16)
PROLOGUE(M4_function_1c)
	movl	PARAM_SIZE, %edx
	movl	PARAM_SRC, %eax

	decl	%edx
	jnz	L(more_than_one_limb)

	movl	(%eax), %eax
	movl	PARAM_DST, %ecx

	mull	PARAM_MULTIPLIER

	addl	PARAM_CARRY, %eax

	adcl	$0, %edx
	M4_inst	%eax, (%ecx)

	adcl	$0, %edx
	movl	%edx, %eax

	ret


	C offset 0x44 so close enough to aligned
L(more_than_one_limb):
	movl	PARAM_CARRY, %ecx
L(start_1):
	C eax	src
	C ecx	initial carry
	C edx	size-1
	subl	$SAVE_SIZE, %esp
deflit(`FRAME',16)

	movl	%ebx, SAVE_EBX
	movl	%esi, SAVE_ESI
	movl	%edx, %ebx	C size-1

	movl	PARAM_SRC, %esi
	movl	%ebp, SAVE_EBP
	cmpl	$UNROLL_THRESHOLD, %edx

	movl	PARAM_MULTIPLIER, %ebp
	movl	%edi, SAVE_EDI

	movl	(%esi), %eax	C src low limb
	movl	PARAM_DST, %edi
	ja	L(unroll)


	C simple loop

	leal	4(%esi,%ebx,4), %esi	C point one limb past last
	leal	(%edi,%ebx,4), %edi	C point at last limb
	negl	%ebx

	C The movl to load the next source limb is done well ahead of the
	C mul.  This is necessary for full speed, and leads to one limb
	C handled separately at the end.

L(simple):
	C eax	src limb
	C ebx	loop counter
	C ecx	carry limb
	C edx	scratch
	C esi	src
	C edi	dst
	C ebp	multiplier

	mull	%ebp

	addl	%eax, %ecx
	adcl	$0, %edx

	M4_inst	%ecx, (%edi,%ebx,4)
	movl	(%esi,%ebx,4), %eax
	adcl	$0, %edx

	incl	%ebx
	movl	%edx, %ecx
	jnz	L(simple)


	mull	%ebp

	movl	SAVE_EBX, %ebx
	movl	SAVE_ESI, %esi
	movl	SAVE_EBP, %ebp

	addl	%eax, %ecx
	adcl	$0, %edx

	M4_inst	%ecx, (%edi)
	adcl	$0, %edx
	movl	SAVE_EDI, %edi

	addl	$SAVE_SIZE, %esp
	movl	%edx, %eax
	ret



C -----------------------------------------------------------------------------
	ALIGN(16)
L(unroll):
	C eax	src low limb
	C ebx	size-1
	C ecx	carry
	C edx	size-1
	C esi	src
	C edi	dst
	C ebp	multiplier
	
dnl  overlapping with parameters no longer needed
define(VAR_COUNTER,`PARAM_SIZE')
define(VAR_JUMP,   `PARAM_MULTIPLIER')

	subl	$2, %ebx	C (size-2)-1
	decl	%edx		C size-2
	
	shrl	$UNROLL_LOG2, %ebx
	negl	%edx

	movl	%ebx, VAR_COUNTER
	andl	$UNROLL_MASK, %edx

	movl	%edx, %ebx
	shll	$4, %edx

ifdef(`PIC',`
	call	L(pic_calc)
L(here):
',`
	leal	L(entry) (%edx,%ebx,1), %edx
')
	negl	%ebx
	movl	%edx, VAR_JUMP

	mull	%ebp

	addl	%eax, %ecx	C initial carry, becomes low carry
	adcl	$0, %edx
	testb	$1, %bl

	movl	4(%esi), %eax	C src second limb
	leal	ifelse(UNROLL_BYTES,256,128+) 8(%esi,%ebx,4), %esi
	leal	ifelse(UNROLL_BYTES,256,128)   (%edi,%ebx,4), %edi

	movl	%edx, %ebx	C high carry
	cmovnz(	%ecx, %ebx)	C high,low carry other way around
	cmovnz(	%edx, %ecx)

	jmp	*VAR_JUMP


ifdef(`PIC',`
L(pic_calc):
	C See README.family about old gas bugs
	leal	(%edx,%ebx,1), %edx
	addl	$L(entry)-L(here), %edx
	addl	(%esp), %edx
	ret
')


C -----------------------------------------------------------------------------
C This code uses a "two carry limbs" scheme.  At the top of the loop the
C carries are ebx=lo, ecx=hi, then they swap for each limb processed.  For
C the computed jump an odd size means they start one way around, an even
C size the other.  Either way one limb is handled separately at the start of
C the loop.
C
C The positioning of the movl to load the next source limb is important.
C Moving it after the adcl with a view to avoiding a separate mul at the end
C of the loop slows the code down.

	ALIGN(32)
L(top):
	C eax	src limb
	C ebx	carry high
	C ecx	carry low
	C edx	scratch
	C esi	src+8
	C edi	dst
	C ebp	multiplier
	C
	C VAR_COUNTER  loop counter
	C
	C 17 bytes each limb

L(entry):
deflit(CHUNK_COUNT,2)
forloop(`i', 0, UNROLL_COUNT/CHUNK_COUNT-1, `
	deflit(`disp0', eval(i*CHUNK_COUNT*4 ifelse(UNROLL_BYTES,256,-128)))
	deflit(`disp1', eval(disp0 + 4))

	mull	%ebp

Zdisp(	M4_inst,%ecx, disp0,(%edi))
	movl	$0, %ecx

	adcl	%eax, %ebx

Zdisp(	movl,	disp0,(%esi), %eax)
	adcl	%edx, %ecx	


	mull	%ebp

	M4_inst	%ebx, disp1(%edi)
	movl	$0, %ebx

	adcl	%eax, %ecx

	movl	disp1(%esi), %eax
	adcl	%edx, %ebx
')

	decl	VAR_COUNTER
	leal	UNROLL_BYTES(%esi), %esi
	leal	UNROLL_BYTES(%edi), %edi

	jns	L(top)


	C eax	src limb
	C ebx	carry high
	C ecx	carry low
	C edx
	C esi
	C edi	dst (points at second last limb)
	C ebp	multiplier
deflit(`disp0', ifelse(UNROLL_BYTES,256,-128))
deflit(`disp1', eval(disp0-0 + 4))

	mull	%ebp

	M4_inst	%ecx, disp0(%edi)
	movl	SAVE_EBP, %ebp

	adcl	%ebx, %eax
	movl	SAVE_EBX, %ebx
	movl	SAVE_ESI, %esi

	adcl	$0, %edx
	M4_inst	%eax, disp1(%edi)
	movl	SAVE_EDI, %edi

	adcl	$0, %edx
	addl	$SAVE_SIZE, %esp

	movl	%edx, %eax
	ret

EPILOGUE()
