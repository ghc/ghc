dnl  Currently unused.


dnl  Copyright (C) 1999, 2000 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published by
dnl  the Free Software Foundation; either version 2.1 of the License, or (at your
dnl  option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
dnl  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
dnl  MA 02111-1307, USA.

	.set noreorder
	.set volatile
	.set noat

.text
	.align 3
	.globl __umul_ppmm
	.ent __umul_ppmm
__umul_ppmm:
__umul_ppmm..ng:
	.frame $30,0,$26,0
	.prologue 0
	mulq $17,$18,$1
	umulh $17,$18,$0
	stq $1,0($16)
	ret $31,($26),1
	.end __umul_ppmm
