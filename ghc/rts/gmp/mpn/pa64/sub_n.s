; HP-PA 2.0 __gmpn_sub_n -- Subtract two limb vectors of the same length > 0
; and store difference in a third limb vector.

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
; s2_ptr	gr24
; size		gr23

; This runs at 2 cycles/limb on PA8000.

	.level	2.0n
	.code
	.export	__gmpn_sub_n,entry
__gmpn_sub_n
	.proc
	.callinfo frame=0,args_saved
	.entry

	sub		%r0,%r23,%r22
	depw,z		%r22,30,3,%r28		; r28 = 2 * (-n & 7)
	depw,z		%r22,28,3,%r22		; r22 = 8 * (-n & 7)
	sub		%r25,%r22,%r25		; offset s1_ptr
	sub		%r24,%r22,%r24		; offset s2_ptr
	blr		%r28,%r0		; branch into loop
	sub		%r26,%r22,%r26		; offset res_ptr and set carry

L$loop	ldd		0(%r25),%r20
	ldd		0(%r24),%r31
	sub,db		%r20,%r31,%r20
	std		%r20,0(%r26)
L$7	ldd		8(%r25),%r21
	ldd		8(%r24),%r19
	sub,db		%r21,%r19,%r21
	std		%r21,8(%r26)
L$6	ldd		16(%r25),%r20
	ldd		16(%r24),%r31
	sub,db		%r20,%r31,%r20
	std		%r20,16(%r26)
L$5	ldd		24(%r25),%r21
	ldd		24(%r24),%r19
	sub,db		%r21,%r19,%r21
	std		%r21,24(%r26)
L$4	ldd		32(%r25),%r20
	ldd		32(%r24),%r31
	sub,db		%r20,%r31,%r20
	std		%r20,32(%r26)
L$3	ldd		40(%r25),%r21
	ldd		40(%r24),%r19
	sub,db		%r21,%r19,%r21
	std		%r21,40(%r26)
L$2	ldd		48(%r25),%r20
	ldd		48(%r24),%r31
	sub,db		%r20,%r31,%r20
	std		%r20,48(%r26)
L$1	ldd		56(%r25),%r21
	ldo		64(%r25),%r25
	ldd		56(%r24),%r19
	sub,db		%r21,%r19,%r21
	std		%r21,56(%r26)
	ldo		64(%r24),%r24
	addib,>		-8,%r23,L$loop
	ldo		64(%r26),%r26

	add,dc		%r0,%r0,%r29
	subi		1,%r29,%r29
	bve		(%r2)
	.exit
	ldi		0,%r28
	.procend
