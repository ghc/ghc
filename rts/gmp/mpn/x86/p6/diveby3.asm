dnl  Intel P6 mpn_divexact_by3 -- mpn division by 3, expecting no remainder.
dnl       
dnl  P6: 8.5 cycles/limb


dnl  Copyright (C) 2000 Free Software Foundation, Inc.
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


dnl  The P5 code runs well on P6, in fact better than anything else found so
dnl  far.  An imul is 4 cycles, meaning the two cmp/sbbl pairs on the
dnl  dependent path are taking 4.5 cycles.
dnl
dnl  The destination cache line prefetching is unnecessary on P6, but
dnl  removing it is a 2 cycle slowdown (approx), so it must be inducing
dnl  something good in the out of order execution.

include(`../config.m4')

MULFUNC_PROLOGUE(mpn_divexact_by3c)
include_mpn(`x86/pentium/diveby3.asm')
