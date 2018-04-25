.text
	.align 8
.globl callMemset
.type callMemset, @object
callMemset:
.Lc5:
	movl $16843009,0(%rbx)
	movl $16843009,4(%rbx)
	movl $16843009,8(%rbx)
	movl $16843009,12(%rbx)
	jmp *(%rbp)
	.size callMemset, .-callMemset
.section .note.GNU-stack,"",@progbits
.ident "GHC 7.9.20140311"
