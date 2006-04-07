; HP-PA 2.0 32-bit __gmpn_sub_n -- Subtract two limb vectors of the same
; length > 0 and store difference in a third limb vector.

; Copyright (C) 1997, 1998, 2000 Free Software Foundation, Inc.

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

	.code
	.export	__gmpn_sub_n
__gmpn_sub_n
	.proc
	.callinfo frame=0,no_calls
	.entry

	sub		%r0,%r23,%r22
	zdep		%r22,30,3,%r28		; r28 = 2 * (-n & 7)
	zdep		%r22,29,3,%r22		; r22 = 4 * (-n & 7)
	sub		%r25,%r22,%r25		; offset s1_ptr
	sub		%r24,%r22,%r24		; offset s2_ptr
	blr		%r28,%r0		; branch into loop
	sub		%r26,%r22,%r26		; offset res_ptr and set carry

L$loop	ldw		0(%r25),%r20
	ldw		0(%r24),%r31
	subb		%r20,%r31,%r20
	stw		%r20,0(%r26)
L$7	ldw		4(%r25),%r21
	ldw		4(%r24),%r19
	subb		%r21,%r19,%r21
	stw		%r21,4(%r26)
L$6	ldw		8(%r25),%r20
	ldw		8(%r24),%r31
	subb		%r20,%r31,%r20
	stw		%r20,8(%r26)
L$5	ldw		12(%r25),%r21
	ldw		12(%r24),%r19
	subb		%r21,%r19,%r21
	stw		%r21,12(%r26)
L$4	ldw		16(%r25),%r20
	ldw		16(%r24),%r31
	subb		%r20,%r31,%r20
	stw		%r20,16(%r26)
L$3	ldw		20(%r25),%r21
	ldw		20(%r24),%r19
	subb		%r21,%r19,%r21
	stw		%r21,20(%r26)
L$2	ldw		24(%r25),%r20
	ldw		24(%r24),%r31
	subb		%r20,%r31,%r20
	stw		%r20,24(%r26)
L$1	ldw		28(%r25),%r21
	ldo		32(%r25),%r25
	ldw		28(%r24),%r19
	subb		%r21,%r19,%r21
	stw		%r21,28(%r26)
	ldo		32(%r24),%r24
	addib,>		-8,%r23,L$loop
	ldo		32(%r26),%r26

	addc		%r0,%r0,%r28
	bv		(%r2)
	.exit
	subi		1,%r28,%r28
	.procend
