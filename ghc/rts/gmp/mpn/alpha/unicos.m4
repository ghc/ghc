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
	`.ident	dummy')

define(`X',`^X$1')
define(`FLOAT64',
	`dnl
	.psect	$1@crud,data
$1:	.t_floating $2
	.endp')

define(`PROLOGUE',
	`dnl
	.stack	192		; What does this mean?  Only Cray knows.
	.psect	$1@code,code,cache
$1::')
define(`PROLOGUE_GP', `PROLOGUE($1)')

define(`EPILOGUE',
	`dnl
	.endp')

define(`DATASTART',
	`dnl
	.psect	$1@crud,data
$1:')
define(`DATAEND',
	`dnl
	.endp')

define(`ASM_END',
	`dnl
	.end')

define(`unop',`bis r31,r31,r31') ; Unicos assembler lacks unop
define(`cvttqc',`cvttq/c')

define(`ALIGN',`')		; Unicos assembler seems to align using garbage

divert

