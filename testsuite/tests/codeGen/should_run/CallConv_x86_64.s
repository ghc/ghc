	.globl _someFuncF
_someFuncF:
	.globl someFuncF
someFuncF:
	movss %xmm1,%xmm0
	subss %xmm2,%xmm0
	addss %xmm2,%xmm1
	movss %xmm0,%xmm2
	movss %xmm3,%xmm0
	divss %xmm4,%xmm0
	mulss %xmm4,%xmm3
	movss %xmm0,%xmm4
	jmp *(%rbp)

	.globl _someFuncD
_someFuncD:
	.globl someFuncD
someFuncD:
	movsd %xmm1,%xmm0
	subsd %xmm2,%xmm0
	addsd %xmm2,%xmm1
	movsd %xmm0,%xmm2
	movsd %xmm3,%xmm0
	divsd %xmm4,%xmm0
	mulsd %xmm4,%xmm3
	movsd %xmm0,%xmm4
	jmp *(%rbp)
