dnl  x86 mpn_divrem_1 -- mpn by limb division extending to fractional quotient.

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
dnl  486   approx 43 maybe
dnl
dnl
dnl  The following have their own optimized divrem_1 implementations, but
dnl  for reference the code here runs as follows.
dnl
dnl        cycles/limb
dnl  P6MMX     39
dnl  K7        42


include(`../config.m4')


C mp_limb_t mpn_divrem_1 (mp_ptr dst, mp_size_t xsize,
C                         mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_divrem_1c (mp_ptr dst, mp_size_t xsize,
C                          mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C
C Divide src,size by divisor and store the quotient in dst+xsize,size.
C Extend the division to fractional quotient limbs in dst,xsize.  Return the
C remainder.  Either or both xsize and size can be 0.
C
C mpn_divrem_1c takes a carry parameter which is an initial high limb,
C effectively one extra limb at the top of src,size.  Must have
C carry<divisor.
C
C
C Essentially the code is the same as the division based part of
C mpn/generic/divrem_1.c, but has the following advantages.
C
C - If gcc isn't being used then divrem_1.c will get the generic C
C   udiv_qrnnd() and be rather slow.
C
C - On K6, using the loop instruction is a 10% speedup, but gcc doesn't
C   generate that instruction (as of gcc 2.95.2 at least).
C
C A test is done to see if the high limb is less the the divisor, and if so
C one less div is done.  A div is between 20 and 40 cycles on the various
C x86s, so assuming high<divisor about half the time, then this test saves
C half that amount.  The branch misprediction penalty on each chip is less
C than half a div.
C  	
C
C K6: Back-to-back div instructions run at 20 cycles, the same as the loop
C     here, so it seems there's nothing to gain by rearranging the loop.
C     Pairing the mov and loop instructions was found to gain nothing.  (The
C     same is true of the mpn/x86/mod_1.asm loop.)
C
C     With a "decl/jnz" rather than a "loop" this code runs at 22 cycles.
C     The loop_or_decljnz macro is an easy way to get a 10% speedup.
C
C     The fast K6 multiply might be thought to suit a multiply-by-inverse,
C     but that algorithm has been found to suffer from the releatively poor
C     carry handling on K6 and too many auxiliary instructions.  The
C     fractional part however could be done at about 13 c/l.
C
C P5: Moving the load down to pair with the store might save 1 cycle, but
C     that doesn't seem worth bothering with, since it'd be only a 2.2%
C     saving.
C
C     Again here the auxiliary instructions hinder a multiply-by-inverse,
C     though there might be a 10-15% speedup available


defframe(PARAM_CARRY,  24)
defframe(PARAM_DIVISOR,20)
defframe(PARAM_SIZE,   16)
defframe(PARAM_SRC,    12)
defframe(PARAM_XSIZE,  8)
defframe(PARAM_DST,    4)

	.text
	ALIGN(16)

PROLOGUE(mpn_divrem_1c)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%edi		FRAME_pushl()
	
	movl	PARAM_SRC, %edi
	pushl	%esi		FRAME_pushl()

	movl	PARAM_DIVISOR, %esi
	pushl	%ebx		FRAME_pushl()

	movl	PARAM_DST, %ebx
	pushl	%ebp		FRAME_pushl()

	movl	PARAM_XSIZE, %ebp
	orl	%ecx, %ecx

	movl	PARAM_CARRY, %edx
	jz	LF(mpn_divrem_1,fraction)

	leal	-4(%ebx,%ebp,4), %ebx	C dst one limb below integer part
	jmp	LF(mpn_divrem_1,integer_top)

EPILOGUE()


PROLOGUE(mpn_divrem_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%edi		FRAME_pushl()
	
	movl	PARAM_SRC, %edi
	pushl	%esi		FRAME_pushl()

	movl	PARAM_DIVISOR, %esi
	orl	%ecx,%ecx

	jz	L(size_zero)
	pushl	%ebx		FRAME_pushl()

	movl	-4(%edi,%ecx,4), %eax	C src high limb
	xorl	%edx, %edx

	movl	PARAM_DST, %ebx
	pushl	%ebp		FRAME_pushl()

	movl	PARAM_XSIZE, %ebp
	cmpl	%esi, %eax

	leal	-4(%ebx,%ebp,4), %ebx	C dst one limb below integer part
	jae	L(integer_entry)


	C high<divisor, so high of dst is zero, and avoid one div

	movl	%edx, (%ebx,%ecx,4)
	decl	%ecx

	movl	%eax, %edx
	jz	L(fraction)


L(integer_top):
	C eax	scratch (quotient)
	C ebx	dst+4*xsize-4
	C ecx	counter
	C edx	scratch (remainder)
	C esi	divisor
	C edi	src
	C ebp	xsize

	movl	-4(%edi,%ecx,4), %eax
L(integer_entry):

	divl	%esi

	movl	%eax, (%ebx,%ecx,4)
	loop_or_decljnz	L(integer_top)


L(fraction):
	orl	%ebp, %ecx
	jz	L(done)

	movl	PARAM_DST, %ebx


L(fraction_top):
	C eax	scratch (quotient)
	C ebx	dst
	C ecx	counter
	C edx	scratch (remainder)
	C esi	divisor
	C edi
	C ebp

	xorl	%eax, %eax

	divl	%esi

	movl	%eax, -4(%ebx,%ecx,4)
	loop_or_decljnz	L(fraction_top)


L(done):
	popl	%ebp
	movl	%edx, %eax
	popl	%ebx
	popl	%esi
	popl	%edi
	ret


L(size_zero):
deflit(`FRAME',8)
	movl	PARAM_XSIZE, %ecx
	xorl	%eax, %eax

	movl	PARAM_DST, %edi

	cld	C better safe than sorry, see mpn/x86/README.family

	rep
	stosl

	popl	%esi
	popl	%edi
	ret
EPILOGUE()
