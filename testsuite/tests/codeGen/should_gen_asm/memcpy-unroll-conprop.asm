.section .text
.align 8
.globl callMemcpy
.type callMemcpy, @function
callMemcpy:
.Lcb:
	movq %rbx,%rax
	testq %rbx,%rbx
	je .Lc6
.Lc5:
	movl 0(%r14),%ebx
	movl %ebx,0(%rax)
	movl 4(%r14),%ebx
	movl %ebx,4(%rax)
	movl 8(%r14),%ebx
	movl %ebx,8(%rax)
	movl 12(%r14),%ebx
	movl %ebx,12(%rax)
.Lc6:
	jmp *(%rbp)
	.size callMemcpy, .-callMemcpy
.section .note.GNU-stack,"",@progbits
.ident "GHC 10.1.20261108"
