/* Currently not working and not used. */

/*
Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
*/


#define SAVE_BORROW_RESTORE_CARRY(r)	adcl r,r; shll $31,r
#define SAVE_CARRY_RESTORE_BORROW(r)	adcl r,r

	.globl	mpn_addsub_n_0
	.globl	mpn_addsub_n_1

/* Cute i386/i486/p6 addsub loop for the "full overlap" case r1==s2,r2==s1.
   We let subtraction and addition alternate in being two limbs
   ahead of the other, thereby avoiding some SAVE_RESTORE. */
// r1 = r2 + r1    edi = esi + edi
// r2 = r2 - r1    esi = esi - edi
//			s1  s2
//	                r2  r1
//	eax,ebx,ecx,edx,esi,edi,ebp
mpn_addsub_n_0:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	pushl	%ebp

	movl	20(%esp),%edi		/* res_ptr */
	movl	24(%esp),%esi		/* s1_ptr */
	movl	36(%esp),%ebp		/* size */

	shrl	$2,%ebp
	xorl	%edx,%edx
	.align	4
Loop0:				// L=load E=execute S=store
	movl	(%esi),%ebx	// sub 0 L
	movl	4(%esi),%ecx	// sub 1 L
	sbbl	(%edi),%ebx	// sub 0 LE
	sbbl	4(%edi),%ecx	// sub 1 LE
//	SAVE_BORROW_RESTORE_CARRY(%edx)
	movl	(%esi),%eax	// add 0 L
	adcl	%eax,(%edi)	// add 0 LES
	movl	4(%esi),%eax	// add 1 L
	adcl	%eax,4(%edi)	// add 1 LES
	movl	%ebx,(%esi)	// sub 0 S
	movl	%ecx,4(%esi)	// sub 1 S
	movl	8(%esi),%ebx	// add 2 L
	adcl	8(%edi),%ebx	// add 2 LE
	movl	12(%esi),%ecx	// add 3 L
	adcl	12(%edi),%ecx	// add 3 LE
//	SAVE_CARRY_RESTORE_BORROW(%edx)
	movl	8(%edi),%eax	// sub 2 L
	sbbl	%eax,8(%esi)	// sub 2 LES
	movl	12(%edi),%eax	// sub 3 L
	sbbl	%eax,12(%esi)	// sub 3 LES
	movl	%ebx,8(%edi)	// add 2 S
	movl	%ecx,12(%edi)	// add 3 S
	leal	16(%esi),%esi
	leal	16(%edi),%edi
	decl	%ebp
	jnz	Loop0

	popl	%ebp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret

/* Cute i386/i486/p6 addsub loop for the "full overlap" case r1==s1,r2==s2.
   We let subtraction and addition alternate in being two limbs
   ahead of the other, thereby avoiding some SAVE_RESTORE. */
// r1 = r1 + r2    edi = edi + esi
// r2 = r1 - r2    esi = edi - esi
//			s2  s1
//	                r2  r1
//	eax,ebx,ecx,edx,esi,edi,ebp
mpn_addsub_n_1:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	pushl	%ebp

	movl	20(%esp),%edi		/* res_ptr */
	movl	24(%esp),%esi		/* s1_ptr */
	movl	36(%esp),%ebp		/* size */

	shrl	$2,%ebp
	xorl	%edx,%edx
	.align	4
Loop1:				// L=load E=execute S=store
	movl	(%edi),%ebx	// sub 0 L
	sbbl	(%esi),%ebx	// sub 0 LE
	movl	4(%edi),%ecx	// sub 1 L
	sbbl	4(%esi),%ecx	// sub 1 LE
//	SAVE_BORROW_RESTORE_CARRY(%edx)
	movl	(%esi),%eax	// add 0 L
	adcl	%eax,(%edi)	// add 0 LES
	movl	4(%esi),%eax	// add 1 L
	adcl	%eax,4(%edi)	// add 1 LES
	movl	%ebx,(%esi)	// sub 0 S
	movl	%ecx,4(%esi)	// sub 1 S
	movl	8(%esi),%ebx	// add 2 L
	adcl	8(%edi),%ebx	// add 2 LE
	movl	12(%esi),%ecx	// add 3 L
	adcl	12(%edi),%ecx	// add 3 LE
//	SAVE_CARRY_RESTORE_BORROW(%edx)
	movl	8(%edi),%eax	// sub 2 L
	sbbl	8(%esi),%eax	// sub 2 LES
	movl	%eax,8(%esi)	// sub 2 S
	movl	12(%edi),%eax	// sub 3 L
	sbbl	12(%esi),%eax	// sub 3 LE
	movl	%eax,12(%esi)	// sub 3 S
	movl	%ebx,8(%edi)	// add 2 S
	movl	%ecx,12(%edi)	// add 3 S
	leal	16(%esi),%esi
	leal	16(%edi),%edi
	decl	%ebp
	jnz	Loop1

	popl	%ebp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret

	.globl	mpn_copy
mpn_copy:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	pushl	%ebp

	movl	20(%esp),%edi		/* res_ptr */
	movl	24(%esp),%esi		/* s1_ptr */
	movl	28(%esp),%ebp		/* size */

	shrl	$2,%ebp
	.align	4
Loop2:
	movl	(%esi),%eax
	movl	4(%esi),%ebx
	movl	%eax,(%edi)
	movl	%ebx,4(%edi)
	movl	8(%esi),%eax
	movl	12(%esi),%ebx
	movl	%eax,8(%edi)
	movl	%ebx,12(%edi)
	leal	16(%esi),%esi
	leal	16(%edi),%edi
	decl	%ebp
	jnz	Loop2

	popl	%ebp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
