divert(-1)


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


define(`ASM_START',
	`
	.set noreorder
	.set noat')

define(`X',`0x$1')
define(`FLOAT64',
	`
	.align	3
$1:	.t_floating $2')

define(`PROLOGUE',
	`
	.text
	.align	3
	.globl	$1
	.ent	$1
$1:
	.frame r30,0,r26
	.prologue 0')

define(`PROLOGUE_GP',
	`
	.text
	.align	3
	.globl	$1
	.ent	$1
$1:
	ldgp	r29,0(r27)
	.frame	r30,0,r26
	.prologue 1')

define(`EPILOGUE',
	`
	.end	$1')

dnl Map register names r0, r1, etc, to `$0', `$1', etc.
dnl This is needed on all systems but Unicos
forloop(i,0,31,
`define(`r'i,``$''i)'
)
forloop(i,0,31,
`define(`f'i,``$f''i)'
)

define(`DATASTART',
	`dnl
	DATA
$1:')
define(`DATAEND',`dnl')

define(`ASM_END',`dnl')

divert
