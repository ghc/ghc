dnl  Alpha auxiliary for longlong.h's count_leading_zeros

dnl  Copyright (C) 1997, 2000 Free Software Foundation, Inc.

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

include(`../config.m4')

dnl  DISCUSSION:

dnl  Other methods have been tried, and using a 128-entry table actually trims
dnl  about 10% of the execution time (on a 21164) when the table is in the L1
dnl  cache.  But under non-benchmarking conditions, the table will hardly be in
dnl  the L1 cache.  Tricky bit-fiddling methods with multiplies and magic tables
dnl  are also possible, but they require many more instructions than the current
dnl  code.  (But for count_trailing_zeros, such tricks are beneficial.)
dnl  Finally, converting to floating-point and extracting the exponent is much
dnl  slower.

ASM_START()
PROLOGUE(MPN(count_leading_zeros))
	bis	r31,63,r0		C initialize partial result count

	srl	r16,32,r1		C shift down 32 steps -> r1
	cmovne	r1,r1,r16		C select r1 if non-zero
	cmovne	r1,31,r0		C if r1 is nonzero choose smaller count

	srl	r16,16,r1		C shift down 16 steps -> r1
	subq	r0,16,r2		C generate new partial result count
	cmovne	r1,r1,r16		C choose new r1 if non-zero
	cmovne	r1,r2,r0		C choose new count if r1 was non-zero

	srl	r16,8,r1
	subq	r0,8,r2
	cmovne	r1,r1,r16
	cmovne	r1,r2,r0

	srl	r16,4,r1
	subq	r0,4,r2
	cmovne	r1,r1,r16
	cmovne	r1,r2,r0

	srl	r16,2,r1
	subq	r0,2,r2
	cmovne	r1,r1,r16
	cmovne	r1,r2,r0

	srl	r16,1,r1		C extract bit 1
	subq	r0,r1,r0		C subtract it from partial result

	ret	r31,(r26),1
EPILOGUE(MPN(count_leading_zeros))
ASM_END()
