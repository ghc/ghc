.section .text
.align 8
.align 8
	.quad	8589934604
	.quad	0
	.long	14
	.long	0
.globl AddMulX86_f_info
.type AddMulX86_f_info, @function
AddMulX86_f_info:
.LcAx:
	leaq (%r14,%rsi,8),%rbx
	jmp *(%rbp)
	.size AddMulX86_f_info, .-AddMulX86_f_info
.section .data
.align 8
.align 1
.globl AddMulX86_f_closure
.type AddMulX86_f_closure, @object
AddMulX86_f_closure:
	.quad	AddMulX86_f_info
.section .text
.align 8
.align 8
	.quad	8589934604
	.quad	0
	.long	14
	.long	0
.globl AddMulX86_g_info
.type AddMulX86_g_info, @function
AddMulX86_g_info:
.LcAL:
	leaq (%r14,%rsi,8),%rbx
	jmp *(%rbp)
	.size AddMulX86_g_info, .-AddMulX86_g_info
.section .data
.align 8
.align 1
.globl AddMulX86_g_closure
.type AddMulX86_g_closure, @object
AddMulX86_g_closure:
	.quad	AddMulX86_g_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 9.3.20220228"


