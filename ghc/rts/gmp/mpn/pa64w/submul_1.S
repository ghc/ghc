; HP-PA 2.0 64-bit __gmpn_submul_1 -- Multiply a limb vector with a limb and
; subtract the result from a second limb vector.

; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

; This file is part of the GNU MP Library.

; The GNU MP Library is free software; you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation; either version 2.1 of the License, or (at your
; option) any later version.

; The GNU MP Library is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
; License for more details.

; You should have received a copy of the GNU Lesser General Public License
; along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
; the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
; MA 02111-1307, USA.

; INPUT PARAMETERS
#define rptr		%r26
#define sptr		%r25
#define size		%r24
#define s2limb		%r23

; This runs at 11 cycles/limb on a PA8000.  It might be possible to make
; it faster, but the PA8000 pipeline is not publically documented and it
; is very complex to reverse engineer

#define t1 %r19
#define rlimb %r20
#define hi %r21
#define lo %r22
#define m0 %r28
#define m1 %r3
#define cylimb %r29
#define t3 %r4
#define t2 %r6
#define t5 %r23
#define t4 %r31
	.level  2.0w
	.code
	.export __gmpn_submul_1,entry
__gmpn_submul_1
	.proc
	.callinfo frame=128,no_calls
	.entry
	std		s2limb,-56(%r30)
        fldd		-56(%r30),%fr5
	ldo		128(%r30),%r30
	add		%r0,%r0,cylimb		; clear cy and cylimb

	std		%r3,-96(%r30)
	std		%r4,-88(%r30)
	std		%r5,-80(%r30)
	std		%r6,-72(%r30)
	depdi,z		1,31,1,%r5

	fldd		0(sptr),%fr4
	ldo		8(sptr),sptr

	xmpyu		%fr5R,%fr4R,%fr6
	fstd		%fr6,-128(%r30)
	xmpyu		%fr5R,%fr4L,%fr7
	fstd		%fr7,-120(%r30)
	xmpyu		%fr5L,%fr4R,%fr8
	fstd		%fr8,-112(%r30)
	xmpyu		%fr5L,%fr4L,%fr9
	fstd		%fr9,-104(%r30)
	ldd		-128(%r30),lo		; lo = low 64 bit of product
	ldd		-120(%r30),m0		; m0 = mid0 64 bit of product
	ldd		-112(%r30),m1		; m1 = mid1 64 bit of product
	ldd		-104(%r30),hi		; hi = high 64 bit of product
	addib,=		-1,%r24,L$end1
	nop
	fldd		0(sptr),%fr4
	ldo		8(sptr),sptr
	addib,=		-1,%r24,L$end2
	nop
L$loop
	xmpyu		%fr5R,%fr4R,%fr6
	fstd		%fr6,-128(%r30)
	xmpyu		%fr5R,%fr4L,%fr7
	fstd		%fr7,-120(%r30)
	xmpyu		%fr5L,%fr4R,%fr8
	fstd		%fr8,-112(%r30)
	xmpyu		%fr5L,%fr4L,%fr9
	fstd		%fr9,-104(%r30)
	ldd		0(rptr),rlimb
	extrd,u		lo,31,32,t1		; t1 = hi32(lo)
	extrd,u		lo,63,32,t4		; t4 = lo32(lo)
	add,l		m0,t1,t1		; t1 += m0
	add,l,*nuv	m1,t1,t1		; t1 += m1
	 add,l		%r5,hi,hi		; propagate carry
	extrd,u		t1,31,32,t2		; t2 = hi32(t1)
	depd,z		t1,31,32,t5		; t5 = lo32(t1)
	add,l		t5,t4,t4		; t4 += lo32(t1)
	ldd		-128(%r30),lo		; lo = low 64 bit of product
	add		cylimb,t4,t4
	ldd		-120(%r30),m0		; m0 = mid0 64 bit of product
	add,dc		t2,hi,cylimb
	ldd		-112(%r30),m1		; m1 = mid1 64 bit of product
	sub		rlimb,t4,t3
	add		t4,t3,%r0
	ldd		-104(%r30),hi		; hi = high 64 bit of product
	add,dc		%r0,cylimb,cylimb
	fldd		0(sptr),%fr4
	ldo		8(sptr),sptr
	std		t3,0(rptr)
	addib,<>	-1,%r24,L$loop
	ldo		8(rptr),rptr
L$end2
	xmpyu		%fr5R,%fr4R,%fr6
	fstd		%fr6,-128(%r30)
	xmpyu		%fr5R,%fr4L,%fr7
	fstd		%fr7,-120(%r30)
	xmpyu		%fr5L,%fr4R,%fr8
	fstd		%fr8,-112(%r30)
	xmpyu		%fr5L,%fr4L,%fr9
	fstd		%fr9,-104(%r30)
	ldd		0(rptr),rlimb
	extrd,u		lo,31,32,t1		; t1 = hi32(lo)
	extrd,u		lo,63,32,t4		; t4 = lo32(lo)
	add,l		m0,t1,t1		; t1 += m0
	add,l,*nuv	m1,t1,t1		; t1 += m0
	 add,l		%r5,hi,hi		; propagate carry
	extrd,u		t1,31,32,t2		; t2 = hi32(t1)
	depd,z		t1,31,32,t5		; t5 = lo32(t1)
	add,l		t5,t4,t4		; t4 += lo32(t1)
	ldd		-128(%r30),lo		; lo = low 64 bit of product
	add		cylimb,t4,t4
	ldd		-120(%r30),m0		; m0 = mid0 64 bit of product
	add,dc		t2,hi,cylimb
	ldd		-112(%r30),m1		; m1 = mid1 64 bit of product
	sub		rlimb,t4,t3
	add		t4,t3,%r0
	ldd		-104(%r30),hi		; hi = high 64 bit of product
	add,dc		%r0,cylimb,cylimb
	std		t3,0(rptr)
	ldo		8(rptr),rptr
L$end1
	ldd		0(rptr),rlimb
	extrd,u		lo,31,32,t1		; t1 = hi32(lo)
	extrd,u		lo,63,32,t4		; t4 = lo32(lo)
	add,l		m0,t1,t1		; t1 += m0
	add,l,*nuv	m1,t1,t1		; t1 += m0
	 add,l		%r5,hi,hi		; propagate carry
	extrd,u		t1,31,32,t2		; t2 = hi32(t1)
	depd,z		t1,31,32,t5		; t5 = lo32(t1)
	add,l		t5,t4,t4		; t4 += lo32(t1)
	add		cylimb,t4,t4
	add,dc		t2,hi,cylimb
	sub		rlimb,t4,t3
	add		t4,t3,%r0
	add,dc		%r0,cylimb,cylimb
	std		t3,0(rptr)
	ldo		8(rptr),rptr

	ldd		-96(%r30),%r3
	ldd		-88(%r30),%r4
	ldd		-80(%r30),%r5
	ldd		-72(%r30),%r6

	copy		cylimb,%r28
	bve		(%r2)
	.exit
	ldo		-128(%r30),%r30
	.procend
