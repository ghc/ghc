; Copyright (C) 1999 Free Software Foundation, Inc.

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

	.code
	.export		__umul_ppmm
	.align 4
__umul_ppmm
	.proc
	.callinfo frame=64,no_calls
	.entry

	ldo 64(%r30),%r30
	stw %r25,-16(0,%r30)
	fldws -16(0,%r30),%fr22R
	stw %r24,-16(0,%r30)
	fldws -16(0,%r30),%fr22L
	xmpyu %fr22R,%fr22L,%fr22
	fstds %fr22,-16(0,%r30)
	ldw -16(0,%r30),%r28
	ldw -12(0,%r30),%r29
	stw %r29,0(0,%r26)
	bv 0(%r2)
	ldo -64(%r30),%r30

	.exit
	.procend
