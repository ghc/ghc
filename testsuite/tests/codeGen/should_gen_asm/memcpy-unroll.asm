.text
	.align 8
.globl callMemcpy
.type callMemcpy, @object
callMemcpy:
.Lc3:
	movl 0(%r14),%eax
	movl %eax,0(%rbx)
	movl 4(%r14),%eax
	movl %eax,4(%rbx)
	movl 8(%r14),%eax
	movl %eax,8(%rbx)
	movl 12(%r14),%eax
	movl %eax,12(%rbx)
	jmp *(%rbp)
	.size callMemcpy, .-callMemcpy
.section .note.GNU-stack,"",@progbits
.ident "GHC 7.7.20121009"
