# Copyright (C) 1999 Free Software Foundation, Inc.

# This file is part of the GNU MP Library.

# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.

# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.

	.toc
	.globl	__umul_ppmm
	.globl	.__umul_ppmm
	.csect	__umul_ppmm[DS]
__umul_ppmm:
	.long	.__umul_ppmm, TOC[tc0], 0
	.csect	.text[PR]
	.align	2
.__umul_ppmm:
	mul	9,4,5
	srai	0,4,31
	and	0,0,5
	srai	5,5,31
	and	5,5,4
	cax	0,0,5
	mfmq	11
	st	11,0(3)
	cax	3,9,0
	br
