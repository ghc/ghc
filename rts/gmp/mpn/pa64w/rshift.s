; HP-PA 2.0 __gmpn_rshift --

; Copyright (C) 1997, 2000 Free Software Foundation, Inc.

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
; res_ptr	gr26
; s1_ptr	gr25
; size		gr24
; cnt		gr23

; This runs at 1.5 cycles/limb on PA8000.

	.level	2.0w
	.code
	.export	__gmpn_rshift,entry
__gmpn_rshift
	.proc
	.callinfo frame=0,args_saved
	.entry

	mtsar		%r23
	ldd		0(%r25),%r21
	addib,=		-1,%r24,L$end
	shrpd		%r21,%r0,%sar,%r29	; compute carry out limb
	depw,z		%r24,31,3,%r28		; r28 = (size & 7)
	sub		%r0,%r24,%r22
	depw,z		%r22,28,3,%r22		; r22 = 8 * (-size & 7)
	sub		%r25,%r22,%r25		; offset s1_ptr
	blr		%r28,%r0		; branch into jump table
	sub		%r26,%r22,%r26		; offset res_ptr
	b		L$0
	nop
	b		L$1
	copy		%r21,%r20
	b		L$2
	nop
	b		L$3
	copy		%r21,%r20
	b		L$4
	nop
	b		L$5
	copy		%r21,%r20
	b		L$6
	nop
	b		L$7
	copy		%r21,%r20

L$loop
L$0	ldd		8(%r25),%r20
	shrpd		%r20,%r21,%sar,%r21
	std		%r21,0(%r26)
L$7	ldd		16(%r25),%r21
	shrpd		%r21,%r20,%sar,%r20
	std		%r20,8(%r26)
L$6	ldd		24(%r25),%r20
	shrpd		%r20,%r21,%sar,%r21
	std		%r21,16(%r26)
L$5	ldd		32(%r25),%r21
	shrpd		%r21,%r20,%sar,%r20
	std		%r20,24(%r26)
L$4	ldd		40(%r25),%r20
	shrpd		%r20,%r21,%sar,%r21
	std		%r21,32(%r26)
L$3	ldd		48(%r25),%r21
	shrpd		%r21,%r20,%sar,%r20
	std		%r20,40(%r26)
L$2	ldd		56(%r25),%r20
	shrpd		%r20,%r21,%sar,%r21
	std		%r21,48(%r26)
L$1	ldd		64(%r25),%r21
	ldo		64(%r25),%r25
	shrpd		%r21,%r20,%sar,%r20
	std		%r20,56(%r26)
	addib,>		-8,%r24,L$loop
	ldo		64(%r26),%r26

L$end	shrpd		%r0,%r21,%sar,%r21
	std		%r21,0(%r26)
	bve		(%r2)
	.exit
	copy		%r29,%r28
	.procend
