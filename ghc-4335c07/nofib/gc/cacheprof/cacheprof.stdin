	.file	"heimdall.c"
	.version	"01.01"
gcc2_compiled.:
.data
	.align 4
	.type	 num_insns_done,@object
	.size	 num_insns_done,4
num_insns_done:
	.long 0
.section	.rodata
	.align 32
.LC46:
	.string	"heimdall: fatal error near insn %d:\n\t%s\n"
.text
	.align 4
	.type	 panic,@function
panic:
	subl $12,%esp
	movl 16(%esp),%eax
	pushl %eax
	movl num_insns_done,%eax
	pushl %eax
	pushl $.LC46
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $1
	call exit
.Lfe1:
	.size	 panic,.Lfe1-panic
.section	.rodata
.LC47:
	.string	"getIMem"
.text
	.align 4
	.type	 getIMem,@function
getIMem:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $4,%edx
	jne .L341
	movl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L341:
	cmpl $1,%edx
	jne .L342
	movzbl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L342:
	cmpl $2,%edx
	je .L343
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
	jmp .L340
	.p2align 4,,7
.L343:
	movzwl (%eax),%eax
.L345:
.L340:
	addl $12,%esp
	ret
.Lfe2:
	.size	 getIMem,.Lfe2-getIMem
.section	.rodata
.LC48:
	.string	"setIMem"
.text
	.align 4
	.type	 setIMem,@function
setIMem:
	subl $12,%esp
	movl 16(%esp),%eax
	movl 20(%esp),%ecx
	movl 24(%esp),%edx
	cmpl $4,%eax
	jne .L347
	movl %edx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L347:
	cmpl $1,%eax
	jne .L348
	movb %dl,(%ecx)
	jmp .L346
	.p2align 4,,7
.L348:
	cmpl $2,%eax
	jne .L349
	movw %dx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L349:
	addl $-12,%esp
	pushl $.LC48
	call panic
	addl $16,%esp
.L346:
	addl $12,%esp
	ret
.Lfe3:
	.size	 setIMem,.Lfe3-setIMem
	.align 4
	.type	 getIRoM,@function
getIRoM:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpb $0,24(%esp)
	je .L373
	cmpl $4,%edx
	jne .L374
	movl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L374:
	cmpl $1,%edx
	jne .L376
	cmpl $3,%eax
	ja .L377
	movzbl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L377:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L376:
	cmpl $2,%edx
	jne .L379
	movzwl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L379:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	jmp .L383
	.p2align 4,,7
.L373:
	addl $-8,%esp
	pushl %eax
	pushl %edx
	call getIMem
.L383:
	addl $16,%esp
.L372:
	addl $12,%esp
	ret
.Lfe4:
	.size	 getIRoM,.Lfe4-getIRoM
.section	.rodata
.LC49:
	.string	"calcBSF(1)"
.LC50:
	.string	"calcBSF(2)"
.text
	.align 4
	.type	 calcBSF,@function
calcBSF:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 36(%esp),%ebx
	cmpl $4,%eax
	je .L385
	cmpl $2,%eax
	je .L385
	cmpl $1,%eax
	je .L385
	addl $-12,%esp
	pushl $.LC49
	call panic
	addl $16,%esp
.L385:
	testl %ebx,%ebx
	jne .L386
	addl $-12,%esp
	pushl $.LC50
	call panic
	addl $16,%esp
.L386:
	xorl %eax,%eax
	testb $1,%bl
	jne .L388
	.p2align 4,,7
.L389:
	incl %eax
	btl %eax,%ebx
	jnc .L389
.L388:
	popl %ebx
	addl $24,%esp
	ret
.Lfe5:
	.size	 calcBSF,.Lfe5-calcBSF
.section	.rodata
.LC51:
	.string	"calcBSR(1)"
.LC52:
	.string	"calcBSR(2)"
.text
	.align 4
	.type	 calcBSR,@function
calcBSR:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	cmpl $4,%ebx
	je .L392
	cmpl $2,%ebx
	je .L392
	cmpl $1,%ebx
	je .L392
	addl $-12,%esp
	pushl $.LC51
	call panic
	addl $16,%esp
.L392:
	testl %esi,%esi
	jne .L393
	addl $-12,%esp
	pushl $.LC52
	call panic
	addl $16,%esp
.L393:
	leal -1(,%ebx,8),%eax
	btl %eax,%esi
	jc .L395
	.p2align 4,,7
.L396:
	decl %eax
	btl %eax,%esi
	jnc .L396
.L395:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe6:
	.size	 calcBSR,.Lfe6-calcBSR
.section	.rodata
.LC53:
	.string	"calcSHLD"
.text
	.align 4
	.type	 calcSHLD,@function
calcSHLD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L401
	ja .L405
	cmpl $1,%edx
	je .L402
	jmp .L403
	.p2align 4,,7
.L405:
	cmpl $4,%edx
	jne .L403
	movl %eax,%edx
	sall %cl,%edx
	movl $32,%eax
	jmp .L407
	.p2align 4,,7
.L401:
	movl %eax,%edx
	sall %cl,%edx
	movl $16,%eax
	jmp .L407
	.p2align 4,,7
.L402:
	movl %eax,%edx
	sall %cl,%edx
	movl $8,%eax
.L407:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	shrl %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L406
	.p2align 4,,7
.L403:
	addl $-12,%esp
	pushl $.LC53
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L406:
	popl %ebx
	addl $24,%esp
	ret
.Lfe7:
	.size	 calcSHLD,.Lfe7-calcSHLD
.section	.rodata
.LC54:
	.string	"calcSHRD"
.text
	.align 4
	.type	 calcSHRD,@function
calcSHRD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L411
	ja .L415
	cmpl $1,%edx
	je .L412
	jmp .L413
	.p2align 4,,7
.L415:
	cmpl $4,%edx
	jne .L413
	movl %eax,%edx
	shrl %cl,%edx
	movl $32,%eax
	jmp .L417
	.p2align 4,,7
.L411:
	movl %eax,%edx
	shrl %cl,%edx
	movl $16,%eax
	jmp .L417
	.p2align 4,,7
.L412:
	movl %eax,%edx
	shrl %cl,%edx
	movl $8,%eax
.L417:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	sall %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L416
	.p2align 4,,7
.L413:
	addl $-12,%esp
	pushl $.LC54
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L416:
	popl %ebx
	addl $24,%esp
	ret
.Lfe8:
	.size	 calcSHRD,.Lfe8-calcSHRD
.section	.rodata
.LC55:
	.string	"calcROL"
.text
	.align 4
	.type	 calcROL,@function
calcROL:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L420
	.p2align 4,,7
.L421:
	cmpl $2,%edi
	je .L424
	ja .L428
	cmpl $1,%edi
	je .L425
	jmp .L426
	.p2align 4,,7
.L428:
	cmpl $4,%edi
	jne .L426
	movl %ebx,%eax
	shrl $31,%eax
	jmp .L430
	.p2align 4,,7
.L424:
	movl %ebx,%eax
	shrl $15,%eax
	jmp .L431
	.p2align 4,,7
.L425:
	movl %ebx,%eax
	shrl $7,%eax
.L431:
	andl $1,%eax
.L430:
	addl %ebx,%ebx
	orl %eax,%ebx
	jmp .L422
	.p2align 4,,7
.L426:
	addl $-12,%esp
	pushl $.LC55
	call panic
	addl $16,%esp
.L422:
	decl %esi
	jnz .L421
.L420:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe9:
	.size	 calcROL,.Lfe9-calcROL
.section	.rodata
.LC56:
	.string	"calcROR"
.text
	.align 4
	.type	 calcROR,@function
calcROR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L434
	.p2align 4,,7
.L435:
	cmpl $2,%edi
	je .L438
	ja .L442
	cmpl $1,%edi
	je .L439
	jmp .L440
	.p2align 4,,7
.L442:
	cmpl $4,%edi
	jne .L440
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $31,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L438:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $15,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L439:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $7,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L440:
	addl $-12,%esp
	pushl $.LC56
	call panic
	addl $16,%esp
.L436:
	decl %esi
	jnz .L435
.L434:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe10:
	.size	 calcROR,.Lfe10-calcROR
.section	.rodata
.LC57:
	.string	"calcSAR"
.text
	.align 4
	.type	 calcSAR,@function
calcSAR:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 40(%esp),%ebx
	cmpl $2,%eax
	je .L447
	ja .L451
	cmpl $1,%eax
	je .L448
	jmp .L449
	.p2align 4,,7
.L451:
	cmpl $4,%eax
	jne .L449
	jmp .L445
	.p2align 4,,7
.L447:
	movswl %bx,%ebx
	jmp .L445
	.p2align 4,,7
.L448:
	movsbl %bl,%ebx
	jmp .L445
	.p2align 4,,7
.L449:
	addl $-12,%esp
	pushl $.LC57
	call panic
	addl $16,%esp
.L445:
	movl 36(%esp),%ecx
	sarl %cl,%ebx
	movl %ebx,%eax
	popl %ebx
	addl $24,%esp
	ret
.Lfe11:
	.size	 calcSAR,.Lfe11-calcSAR
.section	.rodata
.LC58:
	.string	"topBitOf"
.text
	.align 4
	.type	 topBitOf,@function
topBitOf:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	cmpl $4,%esi
	je .L453
	cmpl $2,%esi
	je .L453
	cmpl $1,%esi
	je .L453
	addl $-12,%esp
	pushl $.LC58
	call panic
	addl $16,%esp
.L453:
	leal -1(,%esi,8),%ecx
	shrl %cl,%ebx
	movl %ebx,%eax
	andl $1,%eax
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe12:
	.size	 topBitOf,.Lfe12-topBitOf
	.align 4
	.type	 extend_s_8to16,@function
extend_s_8to16:
	movswl 4(%esp),%eax
	ret
.Lfe13:
	.size	 extend_s_8to16,.Lfe13-extend_s_8to16
	.align 4
	.type	 extend_s_16to32,@function
extend_s_16to32:
	movswl 4(%esp),%eax
	ret
.Lfe14:
	.size	 extend_s_16to32,.Lfe14-extend_s_16to32
.section	.rodata
.LC59:
	.string	"extend_s_to32"
.LC60:
	.string	"extend_s_8to"
.text
	.align 4
	.type	 extend_s_8to,@function
extend_s_8to:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $2,%edx
	je .L470
	ja .L474
	cmpl $1,%edx
	je .L466
	jmp .L472
	.p2align 4,,7
.L474:
	cmpl $4,%edx
	jne .L472
	movsbl %al,%eax
	jmp .L476
	.p2align 4,,7
.L470:
	addl $-12,%esp
	pushl %eax
	call extend_s_8to16
	addl $16,%esp
	jmp .L466
	.p2align 4,,7
.L472:
	addl $-12,%esp
	pushl $.LC60
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L476:
.L466:
	addl $12,%esp
	ret
.Lfe15:
	.size	 extend_s_8to,.Lfe15-extend_s_8to
.section	.rodata
.LC61:
	.string	"extend_s_16to"
.text
	.align 4
	.type	 extend_s_16to,@function
extend_s_16to:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $2,%edx
	je .L477
	cmpl $4,%edx
	jne .L481
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	addl $16,%esp
	jmp .L477
	.p2align 4,,7
.L481:
	addl $-12,%esp
	pushl $.LC61
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L477:
	addl $12,%esp
	ret
.Lfe16:
	.size	 extend_s_16to,.Lfe16-extend_s_16to
	.align 4
	.type	 getSDisp16,@function
getSDisp16:
	subl $12,%esp
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	addl $-12,%esp
	pushl %edx
	call extend_s_16to32
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe17:
	.size	 getSDisp16,.Lfe17-getSDisp16
	.align 4
	.type	 getUDisp16,@function
getUDisp16:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	ret
.Lfe18:
	.size	 getUDisp16,.Lfe18-getUDisp16
.section	.rodata
.LC62:
	.string	"getSDisp"
.text
	.align 4
	.type	 getSDisp,@function
getSDisp:
	subl $12,%esp
	movl 16(%esp),%eax
	cmpl $2,%eax
	je .L494
	jg .L500
	cmpl $1,%eax
	je .L495
	jmp .L498
	.p2align 4,,7
.L500:
	cmpl $4,%eax
	jne .L498
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	jmp .L501
	.p2align 4,,7
.L494:
	call getSDisp16
	jmp .L501
	.p2align 4,,7
.L495:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	jmp .L501
	.p2align 4,,7
.L498:
	addl $-12,%esp
	pushl $.LC62
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L501:
	addl $12,%esp
	ret
.Lfe19:
	.size	 getSDisp,.Lfe19-getSDisp
.section	.rodata
.LC63:
	.string	"getUDisp"
.text
	.align 4
	.type	 getUDisp,@function
getUDisp:
	subl $12,%esp
	movl 16(%esp),%eax
	cmpl $2,%eax
	je .L506
	jg .L511
	cmpl $1,%eax
	je .L507
	jmp .L509
	.p2align 4,,7
.L511:
	cmpl $4,%eax
	jne .L509
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	jmp .L512
	.p2align 4,,7
.L506:
	call getUDisp16
	jmp .L512
	.p2align 4,,7
.L507:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	jmp .L512
	.p2align 4,,7
.L509:
	addl $-12,%esp
	pushl $.LC63
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L512:
	addl $12,%esp
	ret
.Lfe20:
	.size	 getUDisp,.Lfe20-getUDisp
.data
	.align 32
	.type	 cases.468,@object
	.size	 cases.468,128
cases.468:
	.long .L515
	.long .L516
	.long .L517
	.long .L518
	.long .L519
	.long .L520
	.long .L521
	.long .L522
	.long .L523
	.long .L524
	.long .L525
	.long .L526
	.long .L527
	.long .L528
	.long .L529
	.long .L530
	.long .L531
	.long .L532
	.long .L533
	.long .L534
	.long .L535
	.long .L536
	.long .L537
	.long .L538
	.long .L539
	.long .L540
	.long .L541
	.long .L542
	.long .L543
	.long .L544
	.long .L545
	.long .L546
	.align 32
	.type	 parityIsEven,@object
	.size	 parityIsEven,256
parityIsEven:
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.section	.rodata
	.align 32
.LC64:
	.string	"SET_OCSZ_ARITH: src=%u(%d)   dst=%u(%d)   sres=%lld   ures=%llu\n"
.LC67:
	.string	"update_flags_ADD"
	.align 4
.LC65:
	.long -2147483648
	.long 0
	.align 4
.LC66:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_ADD
	.type	 update_flags_ADD,@function
update_flags_ADD:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 36(%esp),%ebp
	movl 40(%esp),%ecx
	movl %ebp,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	addl %ecx,%esi
	adcl $0,%edi
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	addl %eax,%ecx
	adcl %edx,%ebx
	cmpl $2,32(%esp)
	je .L624
	jg .L646
	cmpl $1,32(%esp)
	je .L634
	jmp .L644
	.p2align 4,,7
.L646:
	cmpl $4,32(%esp)
	jne .L644
	movl %ecx,%eax
	movl %ebx,%edx
	addl .LC65,%eax
	adcl .LC65+4,%edx
	je .L615
	orb $8,m_eflags+1
	jmp .L617
	.p2align 4,,7
.L615:
	andb $247,m_eflags+1
.L617:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L618
	orb $1,m_eflags
	jmp .L619
	.p2align 4,,7
.L618:
	andb $254,m_eflags
.L619:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC66,%edx
	andl .LC66+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L620
	orb $64,m_eflags
	jmp .L621
	.p2align 4,,7
.L620:
	andb $191,m_eflags
.L621:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC65,%edx
	andl .LC65+4,%ecx
.L648:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L647
	jmp .L642
	.p2align 4,,7
.L624:
	movl %ecx,%eax
	movl %ebx,%edx
	addl $32768,%eax
	adcl $0,%edx
	jne .L626
	jne .L625
	cmpl $65535,%eax
	jbe .L625
.L626:
	orb $8,m_eflags+1
	jmp .L627
	.p2align 4,,7
.L625:
	andb $247,m_eflags+1
.L627:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L628
	orb $1,m_eflags
	jmp .L629
	.p2align 4,,7
.L628:
	andb $254,m_eflags
.L629:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L630
	orb $64,m_eflags
	jmp .L631
	.p2align 4,,7
.L630:
	andb $191,m_eflags
.L631:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L648
	.p2align 4,,7
.L634:
	movl %ecx,%eax
	movl %ebx,%edx
	addl $128,%eax
	adcl $0,%edx
	jne .L636
	jne .L635
	cmpl $255,%eax
	jbe .L635
.L636:
	orb $8,m_eflags+1
	jmp .L637
	.p2align 4,,7
.L635:
	andb $247,m_eflags+1
.L637:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L638
	orb $1,m_eflags
	jmp .L639
	.p2align 4,,7
.L638:
	andb $254,m_eflags
.L639:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L640
	orb $64,m_eflags
	jmp .L641
	.p2align 4,,7
.L640:
	andb $191,m_eflags
.L641:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L642
.L647:
	orb $128,m_eflags
	jmp .L613
	.p2align 4,,7
.L642:
	andb $127,m_eflags
	jmp .L613
	.p2align 4,,7
.L644:
	addl $-12,%esp
	pushl $.LC67
	call panic
	addl $16,%esp
.L613:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe21:
	.size	 update_flags_ADD,.Lfe21-update_flags_ADD
.section	.rodata
.LC68:
	.string	"update_flags_SUB"
.text
	.align 4
	.type	 update_flags_SUB,@function
update_flags_SUB:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 36(%esp),%ebx
	movl 40(%esp),%ecx
	cmpl $1,%eax
	jne .L650
	movzbl %cl,%ecx
	movzbl %bl,%ebx
	movl %ecx,%edx
	subl %ebx,%edx
	movzbl %dl,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L651
	orb $4,m_eflags
	jmp .L652
	.p2align 4,,7
.L651:
	andb $251,m_eflags
.L652:
	testl %edx,%edx
	jne .L653
	orb $64,m_eflags
	jmp .L654
	.p2align 4,,7
.L653:
	andb $191,m_eflags
.L654:
	cmpl $127,%edx
	jbe .L655
	orb $128,m_eflags
	jmp .L656
	.p2align 4,,7
.L655:
	andb $127,m_eflags
.L656:
	cmpl %ecx,%ebx
	jbe .L657
	orb $1,m_eflags
	jmp .L658
	.p2align 4,,7
.L657:
	andb $254,m_eflags
.L658:
	andl $128,%ecx
	movl %ebx,%eax
	andl $128,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $128,%eax
.L687:
	cmpl %eax,%ecx
	jne .L686
	jmp .L683
	.p2align 4,,7
.L650:
	cmpl $4,%eax
	jne .L662
	movl %ecx,%edx
	subl %ebx,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L663
	orb $4,m_eflags
	jmp .L664
	.p2align 4,,7
.L663:
	andb $251,m_eflags
.L664:
	testl %edx,%edx
	jne .L665
	orb $64,m_eflags
	jmp .L666
	.p2align 4,,7
.L665:
	andb $191,m_eflags
.L666:
	testl %edx,%edx
	jge .L667
	orb $128,m_eflags
	jmp .L668
	.p2align 4,,7
.L667:
	andb $127,m_eflags
.L668:
	cmpl %ecx,%ebx
	jbe .L669
	orb $1,m_eflags
	jmp .L670
	.p2align 4,,7
.L669:
	andb $254,m_eflags
.L670:
	andl $-2147483648,%ecx
	movl %ebx,%eax
	andl $-2147483648,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $-2147483648,%eax
	jmp .L687
	.p2align 4,,7
.L662:
	cmpl $2,%eax
	jne .L674
	movzwl %cx,%ecx
	movzwl %bx,%ebx
	movl %ecx,%edx
	subl %ebx,%edx
	movzwl %dx,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L675
	orb $4,m_eflags
	jmp .L676
	.p2align 4,,7
.L675:
	andb $251,m_eflags
.L676:
	testl %edx,%edx
	jne .L677
	orb $64,m_eflags
	jmp .L678
	.p2align 4,,7
.L677:
	andb $191,m_eflags
.L678:
	cmpl $32767,%edx
	jbe .L679
	orb $128,m_eflags
	jmp .L680
	.p2align 4,,7
.L679:
	andb $127,m_eflags
.L680:
	cmpl %ecx,%ebx
	jbe .L681
	orb $1,m_eflags
	jmp .L682
	.p2align 4,,7
.L681:
	andb $254,m_eflags
.L682:
	andl $32768,%ecx
	movl %ebx,%eax
	andl $32768,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $32768,%eax
	cmpl %eax,%ecx
	je .L683
.L686:
	orb $8,m_eflags+1
	jmp .L661
	.p2align 4,,7
.L683:
	andb $247,m_eflags+1
	jmp .L661
	.p2align 4,,7
.L674:
	addl $-12,%esp
	pushl $.LC68
	call panic
	addl $16,%esp
.L661:
	popl %ebx
	addl $24,%esp
	ret
.Lfe22:
	.size	 update_flags_SUB,.Lfe22-update_flags_SUB
.section	.rodata
.LC71:
	.string	"update_flags_ADC"
	.align 4
.LC69:
	.long -2147483648
	.long 0
	.align 4
.LC70:
	.long -1
	.long 0
.text
	.align 4
	.type	 update_flags_ADC,@function
update_flags_ADC:
	subl $28,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 52(%esp),%ebp
	movl 56(%esp),%ecx
	movb m_eflags,%al
	andb $1,%al
	movb %al,31(%esp)
	movl %ebp,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	addl %ecx,%esi
	adcl $0,%edi
	cmpb $0,31(%esp)
	je .L689
	addl $1,%esi
	adcl $0,%edi
.L689:
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	addl %ecx,%eax
	adcl %ebx,%edx
	cmpb $0,31(%esp)
	je .L691
	addl $1,%eax
	adcl $0,%edx
.L691:
	cmpl $2,48(%esp)
	je .L703
	jg .L725
	cmpl $1,48(%esp)
	je .L713
	jmp .L723
	.p2align 4,,7
.L725:
	cmpl $4,48(%esp)
	jne .L723
	addl .LC69,%eax
	adcl .LC69+4,%edx
	je .L694
	orb $8,m_eflags+1
	jmp .L696
	.p2align 4,,7
.L694:
	andb $247,m_eflags+1
.L696:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L697
	orb $1,m_eflags
	jmp .L698
	.p2align 4,,7
.L697:
	andb $254,m_eflags
.L698:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC70,%edx
	andl .LC70+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L699
	orb $64,m_eflags
	jmp .L700
	.p2align 4,,7
.L699:
	andb $191,m_eflags
.L700:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC69,%edx
	andl .LC69+4,%ecx
.L727:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L726
	jmp .L721
	.p2align 4,,7
.L703:
	addl $32768,%eax
	adcl $0,%edx
	jne .L705
	jne .L704
	cmpl $65535,%eax
	jbe .L704
.L705:
	orb $8,m_eflags+1
	jmp .L706
	.p2align 4,,7
.L704:
	andb $247,m_eflags+1
.L706:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L707
	orb $1,m_eflags
	jmp .L708
	.p2align 4,,7
.L707:
	andb $254,m_eflags
.L708:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L709
	orb $64,m_eflags
	jmp .L710
	.p2align 4,,7
.L709:
	andb $191,m_eflags
.L710:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L727
	.p2align 4,,7
.L713:
	addl $128,%eax
	adcl $0,%edx
	jne .L715
	jne .L714
	cmpl $255,%eax
	jbe .L714
.L715:
	orb $8,m_eflags+1
	jmp .L716
	.p2align 4,,7
.L714:
	andb $247,m_eflags+1
.L716:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L717
	orb $1,m_eflags
	jmp .L718
	.p2align 4,,7
.L717:
	andb $254,m_eflags
.L718:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L719
	orb $64,m_eflags
	jmp .L720
	.p2align 4,,7
.L719:
	andb $191,m_eflags
.L720:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L721
.L726:
	orb $128,m_eflags
	jmp .L692
	.p2align 4,,7
.L721:
	andb $127,m_eflags
	jmp .L692
	.p2align 4,,7
.L723:
	addl $-12,%esp
	pushl $.LC71
	call panic
	addl $16,%esp
.L692:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $28,%esp
	ret
.Lfe23:
	.size	 update_flags_ADC,.Lfe23-update_flags_ADC
.section	.rodata
.LC74:
	.string	"update_flags_SBB"
	.align 4
.LC72:
	.long -2147483648
	.long 0
	.align 4
.LC73:
	.long -1
	.long 0
.text
	.align 4
	.type	 update_flags_SBB,@function
update_flags_SBB:
	subl $28,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 52(%esp),%ebp
	movl 56(%esp),%ecx
	movb m_eflags,%al
	andb $1,%al
	movb %al,31(%esp)
	movl %ecx,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	subl %ebp,%esi
	sbbl $0,%edi
	cmpb $0,31(%esp)
	je .L729
	addl $-1,%esi
	adcl $-1,%edi
.L729:
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	subl %eax,%ecx
	sbbl %edx,%ebx
	movl %ecx,%eax
	movl %ebx,%edx
	cmpb $0,31(%esp)
	je .L731
	addl $-1,%eax
	adcl $-1,%edx
.L731:
	cmpl $2,48(%esp)
	je .L743
	jg .L765
	cmpl $1,48(%esp)
	je .L753
	jmp .L763
	.p2align 4,,7
.L765:
	cmpl $4,48(%esp)
	jne .L763
	addl .LC72,%eax
	adcl .LC72+4,%edx
	je .L734
	orb $8,m_eflags+1
	jmp .L736
	.p2align 4,,7
.L734:
	andb $247,m_eflags+1
.L736:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L737
	orb $1,m_eflags
	jmp .L738
	.p2align 4,,7
.L737:
	andb $254,m_eflags
.L738:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC73,%edx
	andl .LC73+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L739
	orb $64,m_eflags
	jmp .L740
	.p2align 4,,7
.L739:
	andb $191,m_eflags
.L740:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC72,%edx
	andl .LC72+4,%ecx
.L767:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L766
	jmp .L761
	.p2align 4,,7
.L743:
	addl $32768,%eax
	adcl $0,%edx
	jne .L745
	jne .L744
	cmpl $65535,%eax
	jbe .L744
.L745:
	orb $8,m_eflags+1
	jmp .L746
	.p2align 4,,7
.L744:
	andb $247,m_eflags+1
.L746:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L747
	orb $1,m_eflags
	jmp .L748
	.p2align 4,,7
.L747:
	andb $254,m_eflags
.L748:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L749
	orb $64,m_eflags
	jmp .L750
	.p2align 4,,7
.L749:
	andb $191,m_eflags
.L750:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L767
	.p2align 4,,7
.L753:
	addl $128,%eax
	adcl $0,%edx
	jne .L755
	jne .L754
	cmpl $255,%eax
	jbe .L754
.L755:
	orb $8,m_eflags+1
	jmp .L756
	.p2align 4,,7
.L754:
	andb $247,m_eflags+1
.L756:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L757
	orb $1,m_eflags
	jmp .L758
	.p2align 4,,7
.L757:
	andb $254,m_eflags
.L758:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L759
	orb $64,m_eflags
	jmp .L760
	.p2align 4,,7
.L759:
	andb $191,m_eflags
.L760:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L761
.L766:
	orb $128,m_eflags
	jmp .L732
	.p2align 4,,7
.L761:
	andb $127,m_eflags
	jmp .L732
	.p2align 4,,7
.L763:
	addl $-12,%esp
	pushl $.LC74
	call panic
	addl $16,%esp
.L732:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $28,%esp
	ret
.Lfe24:
	.size	 update_flags_SBB,.Lfe24-update_flags_SBB
	.align 4
	.type	 update_flags_NEG,@function
update_flags_NEG:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	addl $-4,%esp
	pushl $0
	cmpl $2,%ebx
	je .L769
	ja .L770
	cmpl $1,%ebx
	je .L771
	jmp .L772
	.p2align 4,,7
.L770:
	cmpl $4,%ebx
	jne .L772
	movl %esi,%eax
	jmp .L774
	.p2align 4,,7
.L769:
	addl $-12,%esp
	pushl %esi
	call extend_s_16to32
	addl $16,%esp
	jmp .L774
	.p2align 4,,7
.L771:
	movl %esi,%edx
	movsbl %dl,%eax
	jmp .L774
	.p2align 4,,7
.L772:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L774:
	pushl %eax
	pushl %ebx
	call update_flags_SUB
	addl $16,%esp
	testl %esi,%esi
	je .L777
	orb $1,m_eflags
	jmp .L778
	.p2align 4,,7
.L777:
	andb $254,m_eflags
.L778:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe25:
	.size	 update_flags_NEG,.Lfe25-update_flags_NEG
.section	.rodata
	.align 32
.LC75:
	.string	"SET_OSZ_INCDEC: dst=%u(%d)   sres=%lld   ures=%llu\n"
.LC78:
	.string	"update_flags_INC"
	.align 4
.LC76:
	.long -2147483648
	.long 0
	.align 4
.LC77:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_INC
	.type	 update_flags_INC,@function
update_flags_INC:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	movl $1,%ebx
	movl $0,%esi
	addl %eax,%ebx
	adcl $0,%esi
	cltd
	addl $1,%eax
	adcl $0,%edx
	cmpl $2,%ecx
	je .L790
	jg .L808
	cmpl $1,%ecx
	je .L798
	jmp .L806
	.p2align 4,,7
.L808:
	cmpl $4,%ecx
	jne .L806
	addl .LC76,%eax
	adcl .LC76+4,%edx
	je .L783
	orb $8,m_eflags+1
	jmp .L785
	.p2align 4,,7
.L783:
	andb $247,m_eflags+1
.L785:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC77,%edx
	andl .LC77+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L786
	orb $64,m_eflags
	jmp .L787
	.p2align 4,,7
.L786:
	andb $191,m_eflags
.L787:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC76,%edx
	andl .LC76+4,%ecx
.L810:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L809
	jmp .L804
	.p2align 4,,7
.L790:
	addl $32768,%eax
	adcl $0,%edx
	jne .L792
	jne .L791
	cmpl $65535,%eax
	jbe .L791
.L792:
	orb $8,m_eflags+1
	jmp .L793
	.p2align 4,,7
.L791:
	andb $247,m_eflags+1
.L793:
	movl %ebx,%edx
	movl %esi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L794
	orb $64,m_eflags
	jmp .L795
	.p2align 4,,7
.L794:
	andb $191,m_eflags
.L795:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L810
	.p2align 4,,7
.L798:
	addl $128,%eax
	adcl $0,%edx
	jne .L800
	jne .L799
	cmpl $255,%eax
	jbe .L799
.L800:
	orb $8,m_eflags+1
	jmp .L801
	.p2align 4,,7
.L799:
	andb $247,m_eflags+1
.L801:
	movl %ebx,%edx
	movl %esi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L802
	orb $64,m_eflags
	jmp .L803
	.p2align 4,,7
.L802:
	andb $191,m_eflags
.L803:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L804
.L809:
	orb $128,m_eflags
	jmp .L781
	.p2align 4,,7
.L804:
	andb $127,m_eflags
	jmp .L781
	.p2align 4,,7
.L806:
	addl $-12,%esp
	pushl $.LC78
	call panic
	addl $16,%esp
.L781:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe26:
	.size	 update_flags_INC,.Lfe26-update_flags_INC
.section	.rodata
.LC81:
	.string	"update_flags_DEC"
	.align 4
.LC79:
	.long -2147483648
	.long 0
	.align 4
.LC80:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_DEC
	.type	 update_flags_DEC,@function
update_flags_DEC:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	movl $-1,%ebx
	movl $-1,%esi
	addl %eax,%ebx
	adcl $0,%esi
	cltd
	addl $-1,%eax
	adcl $-1,%edx
	cmpl $2,%ecx
	je .L822
	jg .L840
	cmpl $1,%ecx
	je .L830
	jmp .L838
	.p2align 4,,7
.L840:
	cmpl $4,%ecx
	jne .L838
	addl .LC79,%eax
	adcl .LC79+4,%edx
	je .L815
	orb $8,m_eflags+1
	jmp .L817
	.p2align 4,,7
.L815:
	andb $247,m_eflags+1
.L817:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC80,%edx
	andl .LC80+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L818
	orb $64,m_eflags
	jmp .L819
	.p2align 4,,7
.L818:
	andb $191,m_eflags
.L819:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC79,%edx
	andl .LC79+4,%ecx
.L842:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L841
	jmp .L836
	.p2align 4,,7
.L822:
	addl $32768,%eax
	adcl $0,%edx
	jne .L824
	jne .L823
	cmpl $65535,%eax
	jbe .L823
.L824:
	orb $8,m_eflags+1
	jmp .L825
	.p2align 4,,7
.L823:
	andb $247,m_eflags+1
.L825:
	movl %ebx,%edx
	movl %esi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L826
	orb $64,m_eflags
	jmp .L827
	.p2align 4,,7
.L826:
	andb $191,m_eflags
.L827:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L842
	.p2align 4,,7
.L830:
	addl $128,%eax
	adcl $0,%edx
	jne .L832
	jne .L831
	cmpl $255,%eax
	jbe .L831
.L832:
	orb $8,m_eflags+1
	jmp .L833
	.p2align 4,,7
.L831:
	andb $247,m_eflags+1
.L833:
	movl %ebx,%edx
	movl %esi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L834
	orb $64,m_eflags
	jmp .L835
	.p2align 4,,7
.L834:
	andb $191,m_eflags
.L835:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L836
.L841:
	orb $128,m_eflags
	jmp .L813
	.p2align 4,,7
.L836:
	andb $127,m_eflags
	jmp .L813
	.p2align 4,,7
.L838:
	addl $-12,%esp
	pushl $.LC81
	call panic
	addl $16,%esp
.L813:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe27:
	.size	 update_flags_DEC,.Lfe27-update_flags_DEC
.section	.rodata
	.align 32
.LC82:
	.string	"SET_OCSZP_LOGICAL: src=0x%x   dst=0x%x   ures=0x%x\n"
.LC83:
	.string	"SET_SIGN_FLAG_FROM"
.text
	.align 4
.globl update_flags_AND
	.type	 update_flags_AND,@function
update_flags_AND:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	andl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L845
	orb $4,m_eflags
	jmp .L846
	.p2align 4,,7
.L845:
	andb $251,m_eflags
.L846:
	testl %edx,%edx
	jne .L847
	orb $64,m_eflags
	jmp .L848
	.p2align 4,,7
.L847:
	andb $191,m_eflags
.L848:
	cmpl $2,%ecx
	je .L853
	jg .L861
	cmpl $1,%ecx
	je .L856
	jmp .L859
	.p2align 4,,7
.L861:
	cmpl $4,%ecx
	jne .L859
	testl %edx,%edx
	jl .L862
	jmp .L857
	.p2align 4,,7
.L853:
	testw %dx,%dx
	jl .L862
	jmp .L857
	.p2align 4,,7
.L856:
	testb %dl,%dl
	jge .L857
.L862:
	orb $128,m_eflags
	jmp .L849
	.p2align 4,,7
.L857:
	andb $127,m_eflags
	jmp .L849
	.p2align 4,,7
.L859:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L849:
	addl $12,%esp
	ret
.Lfe28:
	.size	 update_flags_AND,.Lfe28-update_flags_AND
	.align 4
.globl update_flags_OR
	.type	 update_flags_OR,@function
update_flags_OR:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	orl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L865
	orb $4,m_eflags
	jmp .L866
	.p2align 4,,7
.L865:
	andb $251,m_eflags
.L866:
	testl %edx,%edx
	jne .L867
	orb $64,m_eflags
	jmp .L868
	.p2align 4,,7
.L867:
	andb $191,m_eflags
.L868:
	cmpl $2,%ecx
	je .L873
	jg .L881
	cmpl $1,%ecx
	je .L876
	jmp .L879
	.p2align 4,,7
.L881:
	cmpl $4,%ecx
	jne .L879
	testl %edx,%edx
	jl .L882
	jmp .L877
	.p2align 4,,7
.L873:
	testw %dx,%dx
	jl .L882
	jmp .L877
	.p2align 4,,7
.L876:
	testb %dl,%dl
	jge .L877
.L882:
	orb $128,m_eflags
	jmp .L869
	.p2align 4,,7
.L877:
	andb $127,m_eflags
	jmp .L869
	.p2align 4,,7
.L879:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L869:
	addl $12,%esp
	ret
.Lfe29:
	.size	 update_flags_OR,.Lfe29-update_flags_OR
	.align 4
.globl update_flags_XOR
	.type	 update_flags_XOR,@function
update_flags_XOR:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	xorl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L885
	orb $4,m_eflags
	jmp .L886
	.p2align 4,,7
.L885:
	andb $251,m_eflags
.L886:
	testl %edx,%edx
	jne .L887
	orb $64,m_eflags
	jmp .L888
	.p2align 4,,7
.L887:
	andb $191,m_eflags
.L888:
	cmpl $2,%ecx
	je .L893
	jg .L901
	cmpl $1,%ecx
	je .L896
	jmp .L899
	.p2align 4,,7
.L901:
	cmpl $4,%ecx
	jne .L899
	testl %edx,%edx
	jl .L902
	jmp .L897
	.p2align 4,,7
.L893:
	testw %dx,%dx
	jl .L902
	jmp .L897
	.p2align 4,,7
.L896:
	testb %dl,%dl
	jge .L897
.L902:
	orb $128,m_eflags
	jmp .L889
	.p2align 4,,7
.L897:
	andb $127,m_eflags
	jmp .L889
	.p2align 4,,7
.L899:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L889:
	addl $12,%esp
	ret
.Lfe30:
	.size	 update_flags_XOR,.Lfe30-update_flags_XOR
	.align 4
.globl update_flags_TEST
	.type	 update_flags_TEST,@function
update_flags_TEST:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	andl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L905
	orb $4,m_eflags
	jmp .L906
	.p2align 4,,7
.L905:
	andb $251,m_eflags
.L906:
	testl %edx,%edx
	jne .L907
	orb $64,m_eflags
	jmp .L908
	.p2align 4,,7
.L907:
	andb $191,m_eflags
.L908:
	cmpl $2,%ecx
	je .L913
	jg .L921
	cmpl $1,%ecx
	je .L916
	jmp .L919
	.p2align 4,,7
.L921:
	cmpl $4,%ecx
	jne .L919
	testl %edx,%edx
	jl .L922
	jmp .L917
	.p2align 4,,7
.L913:
	testw %dx,%dx
	jl .L922
	jmp .L917
	.p2align 4,,7
.L916:
	testb %dl,%dl
	jge .L917
.L922:
	orb $128,m_eflags
	jmp .L909
	.p2align 4,,7
.L917:
	andb $127,m_eflags
	jmp .L909
	.p2align 4,,7
.L919:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L909:
	addl $12,%esp
	ret
.Lfe31:
	.size	 update_flags_TEST,.Lfe31-update_flags_TEST
.section	.rodata
.LC84:
	.string	"update_flags_IMUL"
.text
	.align 4
.globl update_flags_IMUL
	.type	 update_flags_IMUL,@function
update_flags_IMUL:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	imull 40(%esp)
	movb $1,%bl
	cmpl $2,%ecx
	je .L928
	jg .L936
	cmpl $1,%ecx
	je .L931
	jmp .L934
	.p2align 4,,7
.L936:
	cmpl $4,%ecx
	jne .L934
	movl %edx,%eax
	xorl %edx,%edx
	movl %eax,%ecx
	movb $0,%bl
	testl %eax,%eax
	je .L933
	cmpl $-1,%ecx
	jne .L924
	jmp .L933
	.p2align 4,,7
.L928:
	shrdl $16,%edx,%eax
	shrl $16,%edx
	movl %eax,%ecx
	movzwl %cx,%ecx
	movb $0,%bl
	testl %ecx,%ecx
	je .L933
	cmpl $65535,%ecx
	jne .L924
	jmp .L933
	.p2align 4,,7
.L931:
	shrdl $8,%edx,%eax
	shrl $8,%edx
	movl %eax,%ecx
	movzbl %cl,%ecx
	movb $0,%bl
	testl %ecx,%ecx
	je .L933
	cmpl $255,%ecx
	jne .L924
.L933:
	movb $1,%bl
	jmp .L937
	.p2align 4,,7
.L934:
	addl $-12,%esp
	pushl $.LC84
	call panic
	addl $16,%esp
.L924:
	testb %bl,%bl
	jne .L937
	orb $1,m_eflags
	jmp .L938
	.p2align 4,,7
.L937:
	andb $254,m_eflags
.L938:
	testb %bl,%bl
	jne .L939
	orb $8,m_eflags+1
	jmp .L940
	.p2align 4,,7
.L939:
	andb $247,m_eflags+1
.L940:
	popl %ebx
	addl $24,%esp
	ret
.Lfe32:
	.size	 update_flags_IMUL,.Lfe32-update_flags_IMUL
.section	.rodata
.LC85:
	.string	"update_flags_MUL"
.text
	.align 4
.globl update_flags_MUL
	.type	 update_flags_MUL,@function
update_flags_MUL:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	mull 40(%esp)
	movl %eax,%ebx
	movl %edx,%esi
	cmpl $2,%ecx
	je .L944
	jg .L948
	cmpl $1,%ecx
	je .L945
	jmp .L946
	.p2align 4,,7
.L948:
	cmpl $4,%ecx
	jne .L946
	movl %esi,%ebx
	xorl %esi,%esi
	jmp .L942
	.p2align 4,,7
.L944:
	shrdl $16,%esi,%ebx
	shrl $16,%esi
	jmp .L942
	.p2align 4,,7
.L945:
	shrdl $8,%esi,%ebx
	shrl $8,%esi
	jmp .L942
	.p2align 4,,7
.L946:
	addl $-12,%esp
	pushl $.LC85
	call panic
	addl $16,%esp
.L942:
	movl %ebx,%eax
	orl %esi,%eax
	je .L949
	orb $1,m_eflags
	jmp .L950
	.p2align 4,,7
.L949:
	andb $254,m_eflags
.L950:
	movl %ebx,%eax
	orl %esi,%eax
	je .L951
	orb $8,m_eflags+1
	jmp .L952
	.p2align 4,,7
.L951:
	andb $247,m_eflags+1
.L952:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe33:
	.size	 update_flags_MUL,.Lfe33-update_flags_MUL
	.align 4
.globl update_flags_SHL
	.type	 update_flags_SHL,@function
update_flags_SHL:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%edi
	movl 40(%esp),%ebp
	movl %ebp,%ebx
	movl %edi,%ecx
	sall %cl,%ebx
	testl %edi,%edi
	je .L954
	addl $-8,%esp
	leal -1(%edi),%ecx
	movl %ebp,%eax
	sall %cl,%eax
	pushl %eax
	pushl %esi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L955
	orb $1,m_eflags
	jmp .L956
	.p2align 4,,7
.L955:
	andb $254,m_eflags
.L956:
	testl %ebx,%ebx
	jne .L957
	orb $64,m_eflags
	jmp .L958
	.p2align 4,,7
.L957:
	andb $191,m_eflags
.L958:
	cmpl $2,%esi
	je .L963
	jg .L971
	cmpl $1,%esi
	je .L966
	jmp .L969
	.p2align 4,,7
.L971:
	cmpl $4,%esi
	jne .L969
	testl %ebx,%ebx
	jl .L975
	jmp .L967
	.p2align 4,,7
.L963:
	testw %bx,%bx
	jl .L975
	jmp .L967
	.p2align 4,,7
.L966:
	testb %bl,%bl
	jge .L967
.L975:
	orb $128,m_eflags
	jmp .L959
	.p2align 4,,7
.L967:
	andb $127,m_eflags
	jmp .L959
	.p2align 4,,7
.L969:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L959:
	cmpl $1,%edi
	jne .L954
	addl $-8,%esp
	pushl %ebp
	pushl %esi
	call topBitOf
	movl %eax,%ebx
	addl $-8,%esp
	leal (%ebp,%ebp),%eax
	pushl %eax
	pushl %esi
	call topBitOf
	addl $32,%esp
	cmpl %eax,%ebx
	je .L973
	orb $8,m_eflags+1
	jmp .L954
	.p2align 4,,7
.L973:
	andb $247,m_eflags+1
.L954:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe34:
	.size	 update_flags_SHL,.Lfe34-update_flags_SHL
	.align 4
.globl update_flags_SHR
	.type	 update_flags_SHR,@function
update_flags_SHR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	movl 40(%esp),%edi
	movl %edi,%edx
	movl %ebx,%ecx
	shrl %cl,%edx
	testl %ebx,%ebx
	je .L977
	leal -1(%ebx),%eax
	btl %eax,%edi
	jnc .L978
	orb $1,m_eflags
	jmp .L979
	.p2align 4,,7
.L978:
	andb $254,m_eflags
.L979:
	testl %edx,%edx
	jne .L980
	orb $64,m_eflags
	jmp .L981
	.p2align 4,,7
.L980:
	andb $191,m_eflags
.L981:
	cmpl $2,%esi
	je .L986
	jg .L994
	cmpl $1,%esi
	je .L989
	jmp .L992
	.p2align 4,,7
.L994:
	cmpl $4,%esi
	jne .L992
	testl %edx,%edx
	jl .L998
	jmp .L990
	.p2align 4,,7
.L986:
	testw %dx,%dx
	jl .L998
	jmp .L990
	.p2align 4,,7
.L989:
	testb %dl,%dl
	jge .L990
.L998:
	orb $128,m_eflags
	jmp .L982
	.p2align 4,,7
.L990:
	andb $127,m_eflags
	jmp .L982
	.p2align 4,,7
.L992:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L982:
	cmpl $1,%ebx
	jne .L977
	addl $-8,%esp
	pushl %edi
	pushl %esi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L996
	orb $8,m_eflags+1
	jmp .L977
	.p2align 4,,7
.L996:
	andb $247,m_eflags+1
.L977:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe35:
	.size	 update_flags_SHR,.Lfe35-update_flags_SHR
	.align 4
	.type	 update_flags_SAR,@function
update_flags_SAR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	movl 40(%esp),%edi
	movl %edi,%edx
	movl %esi,%ecx
	shrl %cl,%edx
	testl %esi,%esi
	je .L1000
	leal -1(%esi),%eax
	btl %eax,%edi
	jnc .L1001
	orb $1,m_eflags
	jmp .L1002
	.p2align 4,,7
.L1001:
	andb $254,m_eflags
.L1002:
	testl %edx,%edx
	jne .L1003
	orb $64,m_eflags
	jmp .L1004
	.p2align 4,,7
.L1003:
	andb $191,m_eflags
.L1004:
	cmpl $2,%ebx
	je .L1009
	jg .L1017
	cmpl $1,%ebx
	je .L1012
	jmp .L1015
	.p2align 4,,7
.L1017:
	cmpl $4,%ebx
	jne .L1015
	testl %edx,%edx
	jl .L1021
	jmp .L1013
	.p2align 4,,7
.L1009:
	testw %dx,%dx
	jl .L1021
	jmp .L1013
	.p2align 4,,7
.L1012:
	testb %dl,%dl
	jge .L1013
.L1021:
	orb $128,m_eflags
	jmp .L1005
	.p2align 4,,7
.L1013:
	andb $127,m_eflags
	jmp .L1005
	.p2align 4,,7
.L1015:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L1005:
	cmpl $1,%esi
	jne .L1000
	andb $247,m_eflags+1
.L1000:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe36:
	.size	 update_flags_SAR,.Lfe36-update_flags_SAR
	.align 4
	.type	 update_flags_ROL,@function
update_flags_ROL:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	movl 40(%esp),%eax
	addl $-4,%esp
	pushl %eax
	pushl %ebx
	pushl %esi
	call calcROL
	addl $16,%esp
	testb $1,%al
	je .L1023
	orb $1,m_eflags
	jmp .L1024
	.p2align 4,,7
.L1023:
	andb $254,m_eflags
.L1024:
	cmpl $1,%ebx
	jne .L1025
	movl %eax,%ebx
	andl $1,%ebx
	addl $-8,%esp
	pushl %eax
	pushl %esi
	call topBitOf
	addl $16,%esp
	cmpl %eax,%ebx
	je .L1026
	orb $8,m_eflags+1
	jmp .L1025
	.p2align 4,,7
.L1026:
	andb $247,m_eflags+1
.L1025:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe37:
	.size	 update_flags_ROL,.Lfe37-update_flags_ROL
	.align 4
	.type	 update_flags_ROR,@function
update_flags_ROR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	movl 40(%esp),%edi
	addl $-4,%esp
	pushl %edi
	pushl %ebx
	pushl %esi
	call calcROR
	addl $-8,%esp
	pushl %eax
	pushl %esi
	call topBitOf
	addl $32,%esp
	testl %eax,%eax
	je .L1029
	orb $1,m_eflags
	jmp .L1030
	.p2align 4,,7
.L1029:
	andb $254,m_eflags
.L1030:
	cmpl $1,%ebx
	jne .L1031
	addl $-8,%esp
	pushl %edi
	pushl %esi
	call topBitOf
	movl %eax,%ebx
	addl $-8,%esp
	leal (%edi,%edi),%eax
	pushl %eax
	pushl %esi
	call topBitOf
	addl $32,%esp
	cmpl %eax,%ebx
	je .L1032
	orb $8,m_eflags+1
	jmp .L1031
	.p2align 4,,7
.L1032:
	andb $247,m_eflags+1
.L1031:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe38:
	.size	 update_flags_ROR,.Lfe38-update_flags_ROR
	.align 4
	.type	 update_flags_and_calc_RCR,@function
update_flags_and_calc_RCR:
	subl $28,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 52(%esp),%ebp
	movl 56(%esp),%ebx
	movl %ebp,%esi
	testl %ebp,%ebp
	je .L1036
	movl 48(%esp),%eax
	leal -1(,%eax,8),%edi
	movl m_eflags,%ecx
	movl %ecx,28(%esp)
	.p2align 4,,7
.L1037:
	movl %ebx,%edx
	andl $1,%edx
	shrl $1,%ebx
	movl 28(%esp),%eax
	andl $1,%eax
	movl %edi,%ecx
	sall %cl,%eax
	orl %eax,%ebx
	testb %dl,%dl
	je .L1039
	orb $1,28(%esp)
	jmp .L1041
	.p2align 4,,7
.L1039:
	andb $254,28(%esp)
.L1041:
	decl %esi
	jnz .L1037
	movl 28(%esp),%eax
	movl %eax,m_eflags
.L1036:
	cmpl $1,%ebp
	jne .L1043
	addl $-8,%esp
	pushl %ebx
	movl 60(%esp),%ecx
	pushl %ecx
	call topBitOf
	movl %eax,%esi
	addl $-8,%esp
	leal (%ebx,%ebx),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call topBitOf
	addl $32,%esp
	cmpl %eax,%esi
	je .L1044
	orb $8,m_eflags+1
	jmp .L1043
	.p2align 4,,7
.L1044:
	andb $247,m_eflags+1
.L1043:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $28,%esp
	ret
.Lfe39:
	.size	 update_flags_and_calc_RCR,.Lfe39-update_flags_and_calc_RCR
	.align 4
	.type	 update_flags_SHLD,@function
update_flags_SHLD:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%ebx
	movl 40(%esp),%eax
	movl 44(%esp),%ebp
	pushl %ebp
	pushl %eax
	pushl %ebx
	pushl %edi
	call calcSHLD
	movl %eax,%esi
	addl $16,%esp
	testl %ebx,%ebx
	je .L1048
	addl $-8,%esp
	leal -1(%ebx),%ecx
	movl %ebp,%eax
	sall %cl,%eax
	pushl %eax
	pushl %edi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L1049
	orb $1,m_eflags
	jmp .L1050
	.p2align 4,,7
.L1049:
	andb $254,m_eflags
.L1050:
	testl %esi,%esi
	jne .L1051
	orb $64,m_eflags
	jmp .L1052
	.p2align 4,,7
.L1051:
	andb $191,m_eflags
.L1052:
	cmpl $2,%edi
	je .L1057
	jg .L1065
	cmpl $1,%edi
	je .L1060
	jmp .L1063
	.p2align 4,,7
.L1065:
	cmpl $4,%edi
	jne .L1063
	testl %esi,%esi
	jl .L1069
	jmp .L1061
	.p2align 4,,7
.L1057:
	testw %si,%si
	jl .L1069
	jmp .L1061
	.p2align 4,,7
.L1060:
	movl %esi,%eax
	testb %al,%al
	jge .L1061
.L1069:
	orb $128,m_eflags
	jmp .L1053
	.p2align 4,,7
.L1061:
	andb $127,m_eflags
	jmp .L1053
	.p2align 4,,7
.L1063:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L1053:
	cmpl $1,%ebx
	jne .L1048
	addl $-8,%esp
	pushl %ebp
	pushl %edi
	call topBitOf
	movl %eax,%ebx
	addl $-8,%esp
	pushl %esi
	pushl %edi
	call topBitOf
	addl $32,%esp
	cmpl %eax,%ebx
	je .L1067
	orb $8,m_eflags+1
	jmp .L1048
	.p2align 4,,7
.L1067:
	andb $247,m_eflags+1
.L1048:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe40:
	.size	 update_flags_SHLD,.Lfe40-update_flags_SHLD
	.align 4
	.type	 update_flags_SHRD,@function
update_flags_SHRD:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%ebx
	movl 40(%esp),%eax
	movl 44(%esp),%ebp
	pushl %ebp
	pushl %eax
	pushl %ebx
	pushl %edi
	call calcSHRD
	movl %eax,%esi
	addl $16,%esp
	testl %ebx,%ebx
	je .L1071
	addl $-8,%esp
	leal -1(%ebx),%ecx
	movl %ebp,%eax
	shrl %cl,%eax
	andl $1,%eax
	pushl %eax
	pushl %edi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L1072
	orb $1,m_eflags
	jmp .L1073
	.p2align 4,,7
.L1072:
	andb $254,m_eflags
.L1073:
	testl %esi,%esi
	jne .L1074
	orb $64,m_eflags
	jmp .L1075
	.p2align 4,,7
.L1074:
	andb $191,m_eflags
.L1075:
	cmpl $2,%edi
	je .L1080
	jg .L1088
	cmpl $1,%edi
	je .L1083
	jmp .L1086
	.p2align 4,,7
.L1088:
	cmpl $4,%edi
	jne .L1086
	testl %esi,%esi
	jl .L1092
	jmp .L1084
	.p2align 4,,7
.L1080:
	testw %si,%si
	jl .L1092
	jmp .L1084
	.p2align 4,,7
.L1083:
	movl %esi,%eax
	testb %al,%al
	jge .L1084
.L1092:
	orb $128,m_eflags
	jmp .L1076
	.p2align 4,,7
.L1084:
	andb $127,m_eflags
	jmp .L1076
	.p2align 4,,7
.L1086:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L1076:
	cmpl $1,%ebx
	jne .L1071
	addl $-8,%esp
	pushl %ebp
	pushl %edi
	call topBitOf
	movl %eax,%ebx
	addl $-8,%esp
	pushl %esi
	pushl %edi
	call topBitOf
	addl $32,%esp
	cmpl %eax,%ebx
	je .L1090
	orb $8,m_eflags+1
	jmp .L1071
	.p2align 4,,7
.L1090:
	andb $247,m_eflags+1
.L1071:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe41:
	.size	 update_flags_SHRD,.Lfe41-update_flags_SHRD
.section	.rodata
.LC86:
	.string	"CBW"
.LC87:
	.string	"CWD"
	.align 32
.LC88:
	.string	"heimdall: do_one_insn: unimplemented INT 0x%x\n"
.LC89:
	.string	"do_one_insn(INT)"
	.align 32
.LC90:
	.string	"heimdall: do_one_insn: unimplemented REPNE 0x%x\n"
.LC91:
	.string	"do_one_insn(REPNE)"
	.align 32
.LC92:
	.string	"heimdall: do_one_insn: unimplemented REP/REPE 0x%x\n"
.LC93:
	.string	"do_one_insn(REP/REPE)"
.LC94:
	.string	"bad case = %d\n"
.LC95:
	.string	"unhandled Grp2 case"
.LC96:
	.string	"Grp3: MUL"
.LC97:
	.string	"Grp3: IMUL"
.globl __udivdi3
.globl __umoddi3
.LC98:
	.string	"Grp3: DIV: divide error"
.LC99:
	.string	"Grp3: DIV"
.globl __divdi3
.globl __moddi3
.LC101:
	.string	"IDIV: divide error"
.LC102:
	.string	"DIV"
.LC103:
	.string	"unhandled Grp3 case"
.LC104:
	.string	"unhandled Grp4 case"
.LC105:
	.string	"unhandled Grp5 case"
	.align 32
.LC106:
	.string	"heimdall: do_one_insn: unimplemented opcode **TWO** 0x%x\n"
.LC107:
	.string	"do_one_insn(unimp2)"
	.align 32
.LC108:
	.string	"heimdall: do_one_insn: unimplemented opcode 0x%x\n"
.LC109:
	.string	"do_one_insn(unimp)"
.text
	.align 4
	.type	 read_bit_array,@function
read_bit_array:
	movl 4(%esp),%ecx
	movl 8(%esp),%edx
	movl %edx,%eax
	shrl $3,%eax
	movzbl (%eax,%ecx),%eax
	movl %edx,%ecx
	andl $7,%ecx
	sarl %cl,%eax
	andl $1,%eax
	ret
.Lfe42:
	.size	 read_bit_array,.Lfe42-read_bit_array
	.align 4
	.type	 write_bit_array,@function
write_bit_array:
	pushl %esi
	pushl %ebx
	movl 12(%esp),%ebx
	movl 16(%esp),%eax
	movl %eax,%esi
	shrl $3,%esi
	movl %eax,%ecx
	andl $7,%ecx
	movl $-2,%eax
	roll %cl,%eax
	movb (%esi,%ebx),%dl
	andb %al,%dl
	movl 20(%esp),%eax
	andl $1,%eax
	sall %cl,%eax
	orb %al,%dl
	movb %dl,(%esi,%ebx)
	popl %ebx
	popl %esi
	ret
.Lfe43:
	.size	 write_bit_array,.Lfe43-write_bit_array
	.align 4
	.type	 fp_double_from_extended,@function
fp_double_from_extended:
	subl $32,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 48(%esp),%edi
	leal 24(%esp),%esi
	movb 9(%edi),%al
	shrb $7,%al
	movzbl %al,%ebx
	movzbl 9(%edi),%ecx
	sall $8,%ecx
	movzbl 8(%edi),%eax
	orl %eax,%ecx
	andl $32767,%ecx
	je .L2878
	cmpl $32767,%ecx
	je .L2888
	addl $-15360,%ecx
	jns .L2881
	xorl %ecx,%ecx
.L2881:
	cmpl $2047,%ecx
	jle .L2878
.L2888:
	movl $2047,%ecx
.L2878:
	movb %cl,%al
	salb $4,%al
	movb %al,6(%esi)
	movl %ecx,%eax
	sarl $4,%eax
	movb %al,%dl
	andb $127,%dl
	movb %bl,%al
	salb $7,%al
	orb %al,%dl
	movb %dl,7(%esi)
	xorl %ebx,%ebx
	.p2align 4,,7
.L2886:
	addl $-4,%esp
	addl $-8,%esp
	leal 11(%ebx),%eax
	pushl %eax
	pushl %edi
	call read_bit_array
	pushl %eax
	pushl %ebx
	pushl %esi
	call write_bit_array
	addl $32,%esp
	incl %ebx
	cmpl $51,%ebx
	jle .L2886
	fldl 24(%esp)
	popl %ebx
	popl %esi
	popl %edi
	addl $32,%esp
	ret
.Lfe44:
	.size	 fp_double_from_extended,.Lfe44-fp_double_from_extended
	.align 4
	.type	 fp_extended_from_double,@function
fp_extended_from_double:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	leal 36(%esp),%esi
	movb 7(%esi),%al
	shrb $7,%al
	movzbl %al,%ebx
	movzbl 7(%esi),%ecx
	sall $4,%ecx
	movb 6(%esi),%al
	shrb $4,%al
	movzbl %al,%eax
	orl %eax,%ecx
	andl $2047,%ecx
	je .L2891
	cmpl $2047,%ecx
	jne .L2892
	movl $32767,%ecx
	jmp .L2891
	.p2align 4,,7
.L2892:
	addl $15360,%ecx
.L2891:
	movl %ecx,%eax
	sarl $8,%eax
	movb %al,%dl
	andb $127,%dl
	movb %bl,%al
	salb $7,%al
	orb %al,%dl
	movb %dl,9(%edi)
	movb %cl,8(%edi)
	xorl %ebx,%ebx
	.p2align 4,,7
.L2897:
	addl $-4,%esp
	addl $-8,%esp
	pushl %ebx
	pushl %esi
	call read_bit_array
	pushl %eax
	leal 11(%ebx),%eax
	pushl %eax
	pushl %edi
	call write_bit_array
	addl $32,%esp
	incl %ebx
	cmpl $51,%ebx
	jle .L2897
	xorl %ebx,%ebx
	.p2align 4,,7
.L2902:
	addl $-4,%esp
	pushl $0
	pushl %ebx
	pushl %edi
	call write_bit_array
	addl $16,%esp
	incl %ebx
	cmpl $10,%ebx
	jle .L2902
	xorl %ebx,%ebx
	addl $-8,%esp
	fldl 44(%esp)
	subl $8,%esp
	fstpl (%esp)
	call isnan
	addl $16,%esp
	testl %eax,%eax
	jne .L2905
	addl $-8,%esp
	fldl 44(%esp)
	subl $8,%esp
	fstpl (%esp)
	call isinf
	addl $16,%esp
	testl %eax,%eax
	jne .L2905
	fldl 36(%esp)
	fldz
	fucompp
	fnstsw %ax
	andb $69,%ah
	cmpb $64,%ah
	je .L2904
.L2905:
	movl $1,%ebx
.L2904:
	addl $-4,%esp
	pushl %ebx
	pushl $63
	pushl %edi
	call write_bit_array
	addl $16,%esp
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe45:
	.size	 fp_extended_from_double,.Lfe45-fp_extended_from_double
	.align 4
	.type	 fp_unpack_data_regs,@function
fp_unpack_data_regs:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	call fp_get_tos
	movl %eax,%ebx
	xorl %esi,%esi
	.p2align 4,,7
.L2910:
	addl $-12,%esp
	leal (%esi,%esi,4),%eax
	leal m_fpu_state+28(,%eax,2),%eax
	pushl %eax
	call fp_double_from_extended
	fstpl m_fpu_data_regs(,%ebx,8)
	addl $16,%esp
	cmpl $7,%ebx
	jne .L2911
	xorl %ebx,%ebx
	jmp .L2909
	.p2align 4,,7
.L2911:
	incl %ebx
.L2909:
	incl %esi
	cmpl $7,%esi
	jle .L2910
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe46:
	.size	 fp_unpack_data_regs,.Lfe46-fp_unpack_data_regs
	.align 4
	.type	 fp_repack_data_regs,@function
fp_repack_data_regs:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	call fp_get_tos
	movl %eax,%ebx
	xorl %esi,%esi
	.p2align 4,,7
.L2918:
	addl $-4,%esp
	fldl m_fpu_data_regs(,%ebx,8)
	subl $8,%esp
	fstpl (%esp)
	leal (%esi,%esi,4),%eax
	leal m_fpu_state+28(,%eax,2),%eax
	pushl %eax
	call fp_extended_from_double
	addl $16,%esp
	cmpl $7,%ebx
	jne .L2919
	xorl %ebx,%ebx
	jmp .L2917
	.p2align 4,,7
.L2919:
	incl %ebx
.L2917:
	incl %esi
	cmpl $7,%esi
	jle .L2918
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe47:
	.size	 fp_repack_data_regs,.Lfe47-fp_repack_data_regs
	.align 4
	.type	 setFMem,@function
setFMem:
	subl $28,%esp
	movl 32(%esp),%eax
	fldl 36(%esp)
	fstps (%eax)
	addl $28,%esp
	ret
.Lfe48:
	.size	 setFMem,.Lfe48-setFMem
	.align 4
	.type	 getFMem,@function
getFMem:
	subl $28,%esp
	movl 32(%esp),%eax
	flds (%eax)
	addl $28,%esp
	ret
.Lfe49:
	.size	 getFMem,.Lfe49-getFMem
	.align 4
	.type	 setDMem,@function
setDMem:
	movl 4(%esp),%eax
	fldl 8(%esp)
	fstpl (%eax)
	ret
.Lfe50:
	.size	 setDMem,.Lfe50-setDMem
	.align 4
	.type	 getDMem,@function
getDMem:
	movl 4(%esp),%eax
	fldl (%eax)
	ret
.Lfe51:
	.size	 getDMem,.Lfe51-getDMem
	.align 4
	.type	 setTMem,@function
setTMem:
	subl $12,%esp
	movl 16(%esp),%eax
	fldl 20(%esp)
	addl $-4,%esp
	subl $8,%esp
	fstpl (%esp)
	pushl %eax
	call fp_extended_from_double
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe52:
	.size	 setTMem,.Lfe52-setTMem
	.align 4
	.type	 getTMem,@function
getTMem:
	subl $12,%esp
	movl 16(%esp),%eax
	addl $-12,%esp
	pushl %eax
	call fp_double_from_extended
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe53:
	.size	 getTMem,.Lfe53-getTMem
.section	.rodata
.LC110:
	.string	"fp_get_statusword_flag"
.text
	.align 4
	.type	 fp_get_statusword_flag,@function
fp_get_statusword_flag:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $15,%ebx
	jbe .L2929
	addl $-12,%esp
	pushl $.LC110
	call panic
	addl $16,%esp
.L2929:
	movzwl m_fpu_state+4,%eax
	movl %ebx,%ecx
	sarl %cl,%eax
	andl $1,%eax
	popl %ebx
	addl $24,%esp
	ret
.Lfe54:
	.size	 fp_get_statusword_flag,.Lfe54-fp_get_statusword_flag
.section	.rodata
.LC111:
	.string	"fp_set_statusword_flag_to"
.text
	.align 4
	.type	 fp_set_statusword_flag_to,@function
fp_set_statusword_flag_to:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $15,%ebx
	jbe .L2931
	addl $-12,%esp
	pushl $.LC111
	call panic
	addl $16,%esp
.L2931:
	cmpl $0,36(%esp)
	je .L2932
	movl $1,%eax
	movl %ebx,%ecx
	sall %cl,%eax
	orw %ax,m_fpu_state+4
	jmp .L2933
	.p2align 4,,7
.L2932:
	movl $-2,%eax
	movl %ebx,%ecx
	roll %cl,%eax
	andw %ax,m_fpu_state+4
.L2933:
	popl %ebx
	addl $24,%esp
	ret
.Lfe55:
	.size	 fp_set_statusword_flag_to,.Lfe55-fp_set_statusword_flag_to
.section	.rodata
.LC112:
	.string	"--- FP STACK OVERFLOW!\n"
.text
	.align 4
	.type	 fp_set_stack_overflow,@function
fp_set_stack_overflow:
	subl $12,%esp
	addl $-8,%esp
	pushl $.LC112
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $1
	call exit
.Lfe56:
	.size	 fp_set_stack_overflow,.Lfe56-fp_set_stack_overflow
.section	.rodata
.LC113:
	.string	"--- FP STACK UNDERFLOW!\n"
.text
	.align 4
	.type	 fp_set_stack_underflow,@function
fp_set_stack_underflow:
	subl $12,%esp
	addl $-8,%esp
	pushl $.LC113
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $1
	call exit
.Lfe57:
	.size	 fp_set_stack_underflow,.Lfe57-fp_set_stack_underflow
	.align 4
	.type	 fp_get_tos,@function
fp_get_tos:
	movzwl m_fpu_state+4,%eax
	shrw $11,%ax
	andl $7,%eax
	ret
.Lfe58:
	.size	 fp_get_tos,.Lfe58-fp_get_tos
.section	.rodata
.LC114:
	.string	"fp_set_tos"
.text
	.align 4
	.type	 fp_set_tos,@function
fp_set_tos:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2938
	addl $-12,%esp
	pushl $.LC114
	call panic
	addl $16,%esp
.L2938:
	addl $-8,%esp
	pushl $0
	pushl $11
	call fp_set_statusword_flag_to
	addl $-8,%esp
	pushl $0
	pushl $12
	call fp_set_statusword_flag_to
	addl $32,%esp
	addl $-8,%esp
	pushl $0
	pushl $13
	call fp_set_statusword_flag_to
	movl %ebx,%eax
	sall $11,%eax
	orw %ax,m_fpu_state+4
	addl $16,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe59:
	.size	 fp_set_tos,.Lfe59-fp_set_tos
.section	.rodata
.LC115:
	.string	"fp_STno_to_regno"
.LC116:
	.string	"heimdall.c"
.LC117:
	.string	"regno >= 0 && regno < 8"
.text
	.align 4
	.type	 fp_STno_to_regno,@function
fp_STno_to_regno:
	subl $12,%esp
	call fp_get_tos
	cmpl $7,%eax
	jbe .L2941
	pushl $.LC115
	pushl $3617
	pushl $.LC116
	pushl $.LC117
	call __assert_fail
	.p2align 4,,7
.L2941:
	addl 16(%esp),%eax
	cmpl $7,%eax
	jbe .L2944
	addl $-8,%eax
	cmpl $7,%eax
	jbe .L2944
	pushl $.LC115
	pushl $3620
	pushl $.LC116
	pushl $.LC117
	call __assert_fail
	.p2align 4,,7
.L2944:
	addl $12,%esp
	ret
.Lfe60:
	.size	 fp_STno_to_regno,.Lfe60-fp_STno_to_regno
	.align 4
	.type	 fp_dec_tos,@function
fp_dec_tos:
	subl $12,%esp
	addl $-12,%esp
	addl $-12,%esp
	pushl $7
	call fp_STno_to_regno
	pushl %eax
	call fp_set_tos
	addl $32,%esp
	addl $12,%esp
	ret
.Lfe61:
	.size	 fp_dec_tos,.Lfe61-fp_dec_tos
	.align 4
	.type	 fp_inc_tos,@function
fp_inc_tos:
	subl $12,%esp
	addl $-12,%esp
	addl $-12,%esp
	pushl $1
	call fp_STno_to_regno
	pushl %eax
	call fp_set_tos
	addl $32,%esp
	addl $12,%esp
	ret
.Lfe62:
	.size	 fp_inc_tos,.Lfe62-fp_inc_tos
	.align 4
	.type	 fp_is_empty_tag,@function
fp_is_empty_tag:
	cmpl $3,4(%esp)
	sete %al
	movzbl %al,%eax
	ret
.Lfe63:
	.size	 fp_is_empty_tag,.Lfe63-fp_is_empty_tag
.section	.rodata
.LC118:
	.string	"fp_get_tag"
.text
	.align 4
	.type	 fp_get_tag,@function
fp_get_tag:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2949
	addl $-12,%esp
	pushl $.LC118
	call panic
	addl $16,%esp
.L2949:
	movzwl m_fpu_state+8,%eax
	leal (%ebx,%ebx),%ecx
	sarl %cl,%eax
	andl $3,%eax
	popl %ebx
	addl $24,%esp
	ret
.Lfe64:
	.size	 fp_get_tag,.Lfe64-fp_get_tag
	.align 4
	.type	 fp_get_tag_ST,@function
fp_get_tag_ST:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2951
	addl $-12,%esp
	pushl $.LC118
	call panic
	addl $16,%esp
.L2951:
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_STno_to_regno
	pushl %eax
	call fp_get_tag
	addl $32,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe65:
	.size	 fp_get_tag_ST,.Lfe65-fp_get_tag_ST
	.align 4
	.type	 fp_set_tag,@function
fp_set_tag:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	cmpl $7,%ebx
	ja .L2954
	cmpl $3,%esi
	jbe .L2953
.L2954:
	addl $-12,%esp
	pushl $.LC118
	call panic
	addl $16,%esp
.L2953:
	leal (%ebx,%ebx),%ecx
	movl $3,%eax
	sall %cl,%eax
	notl %eax
	andw %ax,m_fpu_state+8
	movl %esi,%eax
	sall %cl,%eax
	orw %ax,m_fpu_state+8
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe66:
	.size	 fp_set_tag,.Lfe66-fp_set_tag
.section	.rodata
.LC119:
	.string	"fp_set_reg"
.text
	.align 4
	.type	 fp_set_reg,@function
fp_set_reg:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2956
	addl $-12,%esp
	pushl $.LC119
	call panic
	addl $16,%esp
.L2956:
	fldl 36(%esp)
	fstpl m_fpu_data_regs(,%ebx,8)
	addl $-8,%esp
	fldz
	fldl 44(%esp)
	fucompp
	fnstsw %ax
	andb $69,%ah
	cmpb $64,%ah
	je .L2957
	addl $-8,%esp
	pushl 56(%esp)
	pushl 56(%esp)
	call finite
	addl $16,%esp
	testl %eax,%eax
	sete %al
	movzbl %al,%eax
	addl %eax,%eax
	jmp .L2958
	.p2align 4,,7
.L2957:
	movl $1,%eax
.L2958:
	pushl %eax
	pushl %ebx
	call fp_set_tag
	addl $16,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe67:
	.size	 fp_set_reg,.Lfe67-fp_set_reg
.section	.rodata
.LC120:
	.string	"fp_set_reg_ST"
.text
	.align 4
	.type	 fp_set_reg_ST,@function
fp_set_reg_ST:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2962
	addl $-12,%esp
	pushl $.LC120
	call panic
	addl $16,%esp
.L2962:
	addl $-12,%esp
	pushl %ebx
	call fp_STno_to_regno
	addl $-4,%esp
	fldl 56(%esp)
	subl $8,%esp
	fstpl (%esp)
	pushl %eax
	call fp_set_reg
	addl $32,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe68:
	.size	 fp_set_reg_ST,.Lfe68-fp_set_reg_ST
.section	.rodata
.LC121:
	.string	"fp_get_reg"
.LC122:
	.string	"fp_get_reg: Empty"
.text
	.align 4
	.type	 fp_get_reg,@function
fp_get_reg:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2965
	addl $-12,%esp
	pushl $.LC121
	call panic
	addl $16,%esp
.L2965:
	fldl m_fpu_data_regs(,%ebx,8)
	popl %ebx
	addl $24,%esp
	ret
.Lfe69:
	.size	 fp_get_reg,.Lfe69-fp_get_reg
.section	.rodata
.LC123:
	.string	"fp_get_reg_ST"
.text
	.align 4
	.type	 fp_get_reg_ST,@function
fp_get_reg_ST:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ebx
	cmpl $7,%ebx
	jbe .L2967
	addl $-12,%esp
	pushl $.LC123
	call panic
	addl $16,%esp
.L2967:
	addl $-12,%esp
	pushl %ebx
	call fp_STno_to_regno
	addl $-12,%esp
	pushl %eax
	call fp_get_reg
	addl $32,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe70:
	.size	 fp_get_reg_ST,.Lfe70-fp_get_reg_ST
	.align 4
	.type	 fp_set_tos_reg,@function
fp_set_tos_reg:
	subl $12,%esp
	fldl 16(%esp)
	addl $-4,%esp
	subl $8,%esp
	fstpl (%esp)
	call fp_get_tos
	pushl %eax
	call fp_set_reg
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe71:
	.size	 fp_set_tos_reg,.Lfe71-fp_set_tos_reg
	.align 4
	.type	 fp_get_tos_reg,@function
fp_get_tos_reg:
	subl $12,%esp
	addl $-12,%esp
	call fp_get_tos
	pushl %eax
	call fp_get_reg
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe72:
	.size	 fp_get_tos_reg,.Lfe72-fp_get_tos_reg
	.align 4
	.type	 fp_set_tos_reg_QNaN,@function
fp_set_tos_reg_QNaN:
	subl $28,%esp
	addl $-4,%esp
	movl $2143289344,%eax
	movl %eax,16(%esp)
	flds 16(%esp)
	subl $8,%esp
	fstpl (%esp)
	call fp_get_tos
	pushl %eax
	call fp_set_reg
	addl $16,%esp
	addl $28,%esp
	ret
.Lfe73:
	.size	 fp_set_tos_reg_QNaN,.Lfe73-fp_set_tos_reg_QNaN
	.align 4
	.type	 fp_pop,@function
fp_pop:
	subl $28,%esp
	call fp_get_tos_reg
	fstpl 8(%esp)
	addl $-8,%esp
	pushl $3
	call fp_get_tos
	pushl %eax
	call fp_set_tag
	call fp_inc_tos
	fldl 24(%esp)
	addl $16,%esp
	addl $28,%esp
	ret
.Lfe74:
	.size	 fp_pop,.Lfe74-fp_pop
	.align 4
	.type	 fp_push,@function
fp_push:
	subl $12,%esp
	addl $-12,%esp
	addl $-12,%esp
	pushl $7
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	je .L2973
	call fp_dec_tos
	addl $-8,%esp
	pushl 28(%esp)
	pushl 28(%esp)
	call fp_set_tos_reg
	addl $-8,%esp
	addl $16,%esp
	fldz
	fldl 24(%esp)
	fucompp
	fnstsw %ax
	andb $69,%ah
	cmpb $64,%ah
	sete %al
	movzbl %al,%eax
	pushl %eax
	pushl $9
	call fp_set_statusword_flag_to
	addl $16,%esp
	jmp .L2974
	.p2align 4,,7
.L2973:
	call fp_dec_tos
	call fp_set_tos_reg_QNaN
	call fp_set_stack_overflow
.L2974:
	addl $12,%esp
	ret
.Lfe75:
	.size	 fp_push,.Lfe75-fp_push
	.align 4
	.type	 fp_set_statusword_flags_COM,@function
fp_set_statusword_flags_COM:
	subl $24,%esp
	pushl %ebx
	addl $-8,%esp
	pushl 52(%esp)
	pushl 52(%esp)
	call isnan
	addl $16,%esp
	testl %eax,%eax
	jne .L2977
	addl $-8,%esp
	pushl 44(%esp)
	pushl 44(%esp)
	call isnan
	addl $16,%esp
	testl %eax,%eax
	je .L2976
.L2977:
	movl $7,%ebx
	jmp .L2978
	.p2align 4,,7
.L2976:
	xorl %ebx,%ebx
	fldl 32(%esp)
	fcoml 40(%esp)
	fnstsw %ax
	andb $69,%ah
	je .L2985
	movl $1,%ebx
	fcoml 40(%esp)
	fnstsw %ax
	andb $69,%ah
	cmpb $1,%ah
	je .L2986
	movl $7,%ebx
	fldl 40(%esp)
	fucompp
	fnstsw %ax
	andb $68,%ah
	xorb $64,%ah
	jne .L2978
	movl $4,%ebx
	jmp .L2978
.L2985:
.L2986:
	fstp %st(0)
.L2978:
	addl $-8,%esp
	movl %ebx,%eax
	shrl $2,%eax
	pushl %eax
	pushl $14
	call fp_set_statusword_flag_to
	addl $-8,%esp
	movl %ebx,%eax
	shrl $1,%eax
	andl $1,%eax
	pushl %eax
	pushl $10
	call fp_set_statusword_flag_to
	addl $32,%esp
	addl $-8,%esp
	movl %ebx,%eax
	andl $1,%eax
	pushl %eax
	pushl $8
	call fp_set_statusword_flag_to
	addl $16,%esp
	popl %ebx
	addl $24,%esp
	ret
.Lfe76:
	.size	 fp_set_statusword_flags_COM,.Lfe76-fp_set_statusword_flags_COM
	.align 4
	.type	 fp_set_statusword_flags_COM_STACKF,@function
fp_set_statusword_flags_COM_STACKF:
	subl $12,%esp
	addl $-8,%esp
	pushl $1
	pushl $14
	call fp_set_statusword_flag_to
	addl $-8,%esp
	pushl $1
	pushl $10
	call fp_set_statusword_flag_to
	addl $32,%esp
	addl $-8,%esp
	pushl $1
	pushl $8
	call fp_set_statusword_flag_to
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe77:
	.size	 fp_set_statusword_flags_COM_STACKF,.Lfe77-fp_set_statusword_flags_COM_STACKF
.section	.rodata
	.align 8
.LC125:
	.long 0x0,0x40000000
.text
	.align 4
	.type	 fp_calc_yl2xp1,@function
fp_calc_yl2xp1:
	fldl 12(%esp)
	fld1
	faddl 4(%esp)
#APP
	fldln2; fxch; fyl2x
#NO_APP
	fldl .LC125
#APP
	fldln2; fxch; fyl2x
#NO_APP
	fdivrp %st,%st(1)
	fmulp %st,%st(1)
	ret
.Lfe78:
	.size	 fp_calc_yl2xp1,.Lfe78-fp_calc_yl2xp1
.section	.rodata
	.align 8
.LC126:
	.long 0x0,0x40000000
.text
	.align 4
	.type	 fp_calc_yl2x,@function
fp_calc_yl2x:
	fldl 4(%esp)
	fldl 12(%esp)
	fxch %st(1)
#APP
	fldln2; fxch; fyl2x
#NO_APP
	fldl .LC126
#APP
	fldln2; fxch; fyl2x
#NO_APP
	fdivrp %st,%st(1)
	fmulp %st,%st(1)
	ret
.Lfe79:
	.size	 fp_calc_yl2x,.Lfe79-fp_calc_yl2x
.section	.rodata
.LC127:
	.string	"fp_do_op_ST_ST"
.text
	.align 4
	.type	 fp_do_op_ST_ST,@function
fp_do_op_ST_ST:
	subl $48,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 64(%esp),%esi
	movl 68(%esp),%ebx
	movl 72(%esp),%edi
	movb 76(%esp),%al
	movb %al,31(%esp)
	addl $-12,%esp
	addl $-12,%esp
	pushl %esi
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L2995
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L2995
	addl $-12,%esp
	pushl %ebx
	call fp_get_reg_ST
	fstpl 32(%esp)
	addl $-12,%esp
	pushl %esi
	call fp_get_reg_ST
	addl $32,%esp
	cmpl $5,%edi
	ja .L3007
	jmp *.L3004(,%edi,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3004:
	.long .L2997
	.long .L2998
	.long .L2999
	.long .L3000
	.long .L3001
	.long .L3002
.text
	.p2align 4,,7
.L2997:
	faddl 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
	.p2align 4,,7
.L2998:
	fsubrl 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
	.p2align 4,,7
.L2999:
	fmull 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
	.p2align 4,,7
.L3000:
	fdivrl 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
	.p2align 4,,7
.L3001:
	fsubl 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
	.p2align 4,,7
.L3002:
	fdivl 16(%esp)
	fstpl 16(%esp)
	jmp .L3005
.L3007:
	fstp %st(0)
	addl $-12,%esp
	pushl $.LC127
	call panic
	addl $16,%esp
	jmp .L3005
	.p2align 4,,7
.L2995:
	movl $2143289344,%eax
	movl %eax,12(%esp)
	flds 12(%esp)
	fstpl 16(%esp)
	call fp_set_stack_underflow
.L3005:
	addl $-4,%esp
	pushl 24(%esp)
	pushl 24(%esp)
	pushl %ebx
	call fp_set_reg_ST
	addl $16,%esp
	cmpb $0,31(%esp)
	je .L3006
	call fp_pop
	fstp %st(0)
.L3006:
	popl %ebx
	popl %esi
	popl %edi
	addl $48,%esp
	ret
.Lfe80:
	.size	 fp_do_op_ST_ST,.Lfe80-fp_do_op_ST_ST
	.align 4
	.type	 fp_do_COM_ST_ST,@function
fp_do_COM_ST_ST:
	subl $32,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 48(%esp),%edi
	movl 52(%esp),%esi
	movl 56(%esp),%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl %edi
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3009
	addl $-12,%esp
	addl $-12,%esp
	pushl %esi
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3009
	addl $-12,%esp
	pushl %esi
	call fp_get_reg_ST
	fstpl 40(%esp)
	addl $-12,%esp
	pushl %edi
	call fp_get_reg_ST
	addl $32,%esp
	subl $8,%esp
	fstpl (%esp)
	pushl 36(%esp)
	pushl 36(%esp)
	call fp_set_statusword_flags_COM
	addl $16,%esp
	jmp .L3010
	.p2align 4,,7
.L3009:
	call fp_set_statusword_flags_COM_STACKF
	call fp_set_stack_underflow
.L3010:
	testl %ebx,%ebx
	je .L3012
	.p2align 4,,7
.L3013:
	call fp_pop
	fstp %st(0)
	decl %ebx
	jnz .L3013
.L3012:
	popl %ebx
	popl %esi
	popl %edi
	addl $32,%esp
	ret
.Lfe81:
	.size	 fp_do_COM_ST_ST,.Lfe81-fp_do_COM_ST_ST
.section	.rodata
.LC128:
	.string	"fp_do_op_mem_ST_0"
.text
	.align 4
	.type	 fp_do_op_mem_ST_0,@function
fp_do_op_mem_ST_0:
	subl $48,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 64(%esp),%esi
	movl 68(%esp),%edi
	movl 72(%esp),%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3016
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 40(%esp)
	addl $16,%esp
	testb %bl,%bl
	je .L3017
	addl $-12,%esp
	pushl %esi
	call getDMem
	jmp .L3029
	.p2align 4,,7
.L3017:
	addl $-12,%esp
	pushl %esi
	call getFMem
.L3029:
	addl $16,%esp
	cmpl $5,%edi
	ja .L3030
	jmp *.L3027(,%edi,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3027:
	.long .L3020
	.long .L3021
	.long .L3022
	.long .L3023
	.long .L3024
	.long .L3025
.text
	.p2align 4,,7
.L3020:
	faddl 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
	.p2align 4,,7
.L3021:
	fsubrl 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
	.p2align 4,,7
.L3022:
	fmull 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
	.p2align 4,,7
.L3023:
	fdivrl 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
	.p2align 4,,7
.L3024:
	fsubl 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
	.p2align 4,,7
.L3025:
	fdivl 24(%esp)
	fstpl 24(%esp)
	jmp .L3028
.L3030:
	fstp %st(0)
	addl $-12,%esp
	pushl $.LC128
	call panic
	addl $16,%esp
	jmp .L3028
	.p2align 4,,7
.L3016:
	movl $2143289344,%eax
	movl %eax,20(%esp)
	flds 20(%esp)
	fstpl 24(%esp)
	call fp_set_stack_underflow
.L3028:
	addl $-4,%esp
	pushl 32(%esp)
	pushl 32(%esp)
	pushl $0
	call fp_set_reg_ST
	addl $16,%esp
	popl %ebx
	popl %esi
	popl %edi
	addl $48,%esp
	ret
.Lfe82:
	.size	 fp_do_op_mem_ST_0,.Lfe82-fp_do_op_mem_ST_0
	.align 4
	.type	 fp_do_COM_mem_ST_0,@function
fp_do_COM_mem_ST_0:
	subl $36,%esp
	pushl %esi
	pushl %ebx
	movl 48(%esp),%esi
	movl 52(%esp),%ebx
	movb 56(%esp),%al
	movb %al,31(%esp)
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3032
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 32(%esp)
	addl $16,%esp
	testb %bl,%bl
	je .L3033
	addl $-12,%esp
	pushl %esi
	call getDMem
	jmp .L3037
	.p2align 4,,7
.L3033:
	addl $-12,%esp
	pushl %esi
	call getFMem
.L3037:
	addl $16,%esp
	subl $8,%esp
	fstpl (%esp)
	pushl 28(%esp)
	pushl 28(%esp)
	call fp_set_statusword_flags_COM
	addl $16,%esp
	jmp .L3035
	.p2align 4,,7
.L3032:
	call fp_set_statusword_flags_COM_STACKF
	call fp_set_stack_underflow
.L3035:
	cmpb $0,31(%esp)
	je .L3036
	call fp_pop
	fstp %st(0)
.L3036:
	popl %ebx
	popl %esi
	addl $36,%esp
	ret
.Lfe83:
	.size	 fp_do_COM_mem_ST_0,.Lfe83-fp_do_COM_mem_ST_0
.section	.rodata
.LC129:
	.string	"do_one_insn_fp"
	.align 32
.LC130:
	.string	"(m_fpu_state.env[0] & 0x3F) == 0x3F"
	.align 32
.LC131:
	.string	"fp_get_statusword_flag(7) == 0"
.LC132:
	.string	"!isreg"
.LC133:
	.string	"unhandled opc_aux = 0x%2x\n"
	.align 32
.LC134:
	.string	"do_one_insn_fp: first_opcode == 0xD8"
	.align 32
.LC135:
	.string	"do_one_insn_fp: first_opcode == 0xD9"
	.align 32
.LC137:
	.string	"do_one_insn_fp: first_opcode == 0xDA"
	.align 32
.LC140:
	.string	"do_one_insn_fp: first_opcode == 0xDB"
	.align 32
.LC141:
	.string	"do_one_insn_fp: first_opcode == 0xDC"
	.align 32
.LC142:
	.string	"do_one_insn_fp: first_opcode == 0xDD"
	.align 32
.LC143:
	.string	"do_one_insn_fp: first_opcode == 0xDE"
	.align 32
.LC147:
	.string	"do_one_insn_fp: first_opcode == 0xDF"
	.align 32
.LC148:
	.string	"first opcode = 0x%2X, modRM = 0x%2X\n"
	.align 32
.LC149:
	.string	"do_one_insn_fp: unhandled first_opcode/modrm combination"
.LC150:
	.string	"0"
	.align 8
.LC136:
	.long 0xfefa39ef,0x3fe62e42
	.align 8
.LC138:
	.long 0x100000,0xc1e00000
	.align 8
.LC139:
	.long 0xffe00000,0x41dfffff
	.align 8
.LC144:
	.long 0x0,0xc3e00000
	.align 8
.LC145:
	.long 0x0,0x43e00000
	.align 4
.LC146:
	.long 0
	.long -2147483648
	.align 8
.LC151:
	.long 0x0,0x7ff80000
.text
	.align 4
	.type	 do_one_insn_fp,@function
do_one_insn_fp:
	subl $96,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movb 112(%esp),%bl
	movzwl m_fpu_state,%eax
	andl $63,%eax
	cmpw $63,%ax
	je .L3040
	pushl $.LC129
	pushl $3990
	pushl $.LC116
	pushl $.LC130
	call __assert_fail
	.p2align 4,,7
.L3040:
	addl $-12,%esp
	pushl $7
	call fp_get_statusword_flag
	addl $16,%esp
	testl %eax,%eax
	je .L3042
	pushl $.LC129
	pushl $3993
	pushl $.LC116
	pushl $.LC131
	call __assert_fail
	.p2align 4,,7
.L3042:
	movl m_eip,%eax
	movb (%eax),%dl
	cmpb $216,%bl
	jne .L3043
	cmpb $191,%dl
	ja .L3044
	movb %dl,%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3047
	pushl $.LC129
	pushl $4005
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3047:
	cmpl $7,%ebx
	ja .L3056
	jmp *.L3057(,%ebx,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3057:
	.long .L3049
	.long .L3050
	.long .L3051
	.long .L3052
	.long .L3053
	.long .L3056
	.long .L3054
	.long .L3055
.text
	.p2align 4,,7
.L3049:
	addl $-4,%esp
	pushl $0
	pushl $0
	jmp .L3307
	.p2align 4,,7
.L3050:
	addl $-4,%esp
	pushl $0
	pushl $2
	jmp .L3307
	.p2align 4,,7
.L3051:
	addl $-4,%esp
	pushl $0
	pushl $0
	jmp .L3308
	.p2align 4,,7
.L3052:
	addl $-4,%esp
	pushl $1
	pushl $0
	jmp .L3308
	.p2align 4,,7
.L3053:
	addl $-4,%esp
	pushl $0
	pushl $1
	jmp .L3307
	.p2align 4,,7
.L3054:
	addl $-4,%esp
	pushl $0
	pushl $3
	jmp .L3307
	.p2align 4,,7
.L3055:
	addl $-4,%esp
	pushl $0
	jmp .L3309
	.p2align 4,,7
.L3056:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC134
	jmp .L3310
	.p2align 4,,7
.L3044:
	incl m_eip
	movzbl %dl,%eax
	addl $-192,%eax
	cmpl $63,%eax
	ja .L3069
	jmp *.L3070(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3070:
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3060
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3061
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3062
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3063
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3064
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3065
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3066
	.long .L3067
	.long .L3067
	.long .L3067
	.long .L3067
	.long .L3067
	.long .L3067
	.long .L3067
	.long .L3067
.text
	.p2align 4,,7
.L3060:
	pushl $0
	pushl $0
	pushl $0
	movzbl %dl,%eax
	addl $-192,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3061:
	pushl $0
	pushl $2
	pushl $0
	movzbl %dl,%eax
	addl $-200,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3062:
	addl $-4,%esp
	pushl $0
	pushl $0
	movzbl %dl,%eax
	addl $-208,%eax
	pushl %eax
	jmp .L3312
	.p2align 4,,7
.L3063:
	addl $-4,%esp
	pushl $1
	pushl $0
	movzbl %dl,%eax
	addl $-216,%eax
	pushl %eax
	jmp .L3312
	.p2align 4,,7
.L3064:
	pushl $0
	pushl $1
	pushl $0
	movzbl %dl,%eax
	addl $-224,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3065:
	pushl $0
	pushl $4
	pushl $0
	movzbl %dl,%eax
	addl $-232,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3066:
	pushl $0
	pushl $3
	pushl $0
	movzbl %dl,%eax
	addl $-240,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3067:
	pushl $0
	pushl $5
	pushl $0
	movzbl %dl,%eax
	addl $-248,%eax
	pushl %eax
	jmp .L3311
	.p2align 4,,7
.L3043:
	cmpb $217,%bl
	jne .L3072
	cmpb $191,%dl
	ja .L3073
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3076
	pushl $.LC129
	pushl $4092
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3076:
	cmpl $7,%ebx
	ja .L3087
	jmp *.L3088(,%ebx,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3088:
	.long .L3078
	.long .L3087
	.long .L3079
	.long .L3082
	.long .L3087
	.long .L3085
	.long .L3087
	.long .L3086
.text
	.p2align 4,,7
.L3078:
	addl $-12,%esp
	movl 60(%esp),%eax
	pushl %eax
	call getFMem
	jmp .L3313
	.p2align 4,,7
.L3079:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3339
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 56(%esp)
	addl $16,%esp
	jmp .L3084
	.p2align 4,,7
.L3082:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3083
	call fp_pop
	fstpl 40(%esp)
	jmp .L3084
	.p2align 4,,7
.L3083:
	call fp_pop
	fstp %st(0)
.L3339:
	movl $2143289344,%eax
	movl %eax,12(%esp)
	flds 12(%esp)
	fstpl 40(%esp)
	call fp_set_stack_underflow
.L3084:
	addl $-4,%esp
	pushl 48(%esp)
	pushl 48(%esp)
	movl 60(%esp),%eax
	pushl %eax
	call setFMem
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3085:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $2
	call getIMem
	movw %ax,m_fpu_state
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3086:
	addl $-4,%esp
	movzwl m_fpu_state,%eax
	pushl %eax
	movl 56(%esp),%eax
	pushl %eax
	pushl $2
	jmp .L3314
	.p2align 4,,7
.L3087:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC135
	jmp .L3310
	.p2align 4,,7
.L3073:
	incl m_eip
	movzbl %dl,%eax
	addl $-192,%eax
	cmpl $63,%eax
	ja .L3069
	jmp *.L3134(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3134:
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3091
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3094
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3097
	.long .L3100
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3104
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3106
	.long .L3107
	.long .L3069
	.long .L3069
	.long .L3108
	.long .L3069
	.long .L3111
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3115
	.long .L3118
	.long .L3069
	.long .L3122
	.long .L3069
	.long .L3125
	.long .L3129
.text
	.p2align 4,,7
.L3091:
	movzbl %dl,%ebx
	addl $-192,%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3092
	addl $-12,%esp
	addl $-12,%esp
	pushl $7
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	je .L3092
	addl $-12,%esp
	pushl %ebx
	call fp_get_reg_ST
	fstpl 40(%esp)
	addl $16,%esp
	jmp .L3105
	.p2align 4,,7
.L3092:
	movl $2143289344,%eax
	movl %eax,12(%esp)
	flds 12(%esp)
	fstpl 24(%esp)
	call fp_set_stack_underflow
	jmp .L3105
	.p2align 4,,7
.L3094:
	movzbl %dl,%ebx
	addl $-200,%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3095
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3095
	addl $-12,%esp
	pushl %ebx
	call fp_get_reg_ST
	fstpl 40(%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 64(%esp)
	addl $32,%esp
	jmp .L3096
	.p2align 4,,7
.L3095:
	movl $2143289344,%eax
	movl %eax,12(%esp)
	flds 12(%esp)
	fstpl 24(%esp)
	fldl 24(%esp)
	fstpl 32(%esp)
	call fp_set_stack_underflow
.L3096:
	addl $-4,%esp
	pushl 40(%esp)
	pushl 40(%esp)
	pushl %ebx
	call fp_set_reg_ST
	addl $-4,%esp
	pushl 48(%esp)
	pushl 48(%esp)
	jmp .L3316
	.p2align 4,,7
.L3097:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3101
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fchs
	fstpl 40(%esp)
	addl $16,%esp
	jmp .L3132
	.p2align 4,,7
.L3100:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3101
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
	fabs
	fstpl 24(%esp)
	jmp .L3132
	.p2align 4,,7
.L3101:
	movl $2143289344,%eax
	movl %eax,12(%esp)
	flds 12(%esp)
	fstpl 24(%esp)
	jmp .L3326
	.p2align 4,,7
.L3104:
	movl $0,24(%esp)
	movl $1072693248,28(%esp)
	jmp .L3105
	.p2align 4,,7
.L3106:
	fldl .LC136
	fstpl 24(%esp)
	jmp .L3105
	.p2align 4,,7
.L3107:
	movl $0,24(%esp)
	movl $0,28(%esp)
.L3105:
	addl $-8,%esp
	pushl 36(%esp)
	pushl 36(%esp)
	jmp .L3315
	.p2align 4,,7
.L3108:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	addl $-12,%esp
	pushl $1
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	pushl $1
	call fp_get_reg_ST
	subl $8,%esp
	fstpl (%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
	subl $8,%esp
	fstpl (%esp)
	call fp_calc_yl2x
	jmp .L3345
	.p2align 4,,7
.L3111:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	addl $-12,%esp
	pushl $1
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	pushl $1
	call fp_get_reg_ST
	fstpl 32(%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $32,%esp
	fldl 16(%esp)
	fxch %st(1)
#APP
	fpatan
	
#NO_APP
	fstpl 24(%esp)
	jmp .L3117
	.p2align 4,,7
.L3115:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	addl $-12,%esp
	pushl $1
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3116
	addl $-12,%esp
	pushl $1
	call fp_get_reg_ST
	subl $8,%esp
	fstpl (%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
	subl $8,%esp
	fstpl (%esp)
	call fp_calc_yl2xp1
.L3345:
	fstpl 56(%esp)
	addl $32,%esp
	jmp .L3117
	.p2align 4,,7
.L3116:
	fldl .LC151
	fstpl 24(%esp)
	call fp_set_stack_underflow
.L3117:
	addl $-4,%esp
	pushl 32(%esp)
	pushl 32(%esp)
	pushl $1
	jmp .L3317
	.p2align 4,,7
.L3118:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3130
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
#APP
	fsqrt
#NO_APP
	fstpl 24(%esp)
	jmp .L3132
	.p2align 4,,7
.L3122:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3130
	addl $-8,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	subl $8,%esp
	fstpl (%esp)
	call rint
	fstpl 56(%esp)
	addl $32,%esp
	jmp .L3132
	.p2align 4,,7
.L3125:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3130
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
#APP
	fsin
#NO_APP
	fstpl 24(%esp)
	jmp .L3132
	.p2align 4,,7
.L3129:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3130
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	addl $16,%esp
#APP
	fcos
#NO_APP
	fstpl 24(%esp)
	jmp .L3132
	.p2align 4,,7
.L3130:
	fldl .LC151
	fstpl 24(%esp)
.L3326:
	call fp_set_stack_underflow
.L3132:
	addl $-4,%esp
	pushl 32(%esp)
	pushl 32(%esp)
	pushl $0
	call fp_set_reg_ST
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3072:
	cmpb $218,%bl
	jne .L3136
	cmpb $191,%dl
	ja .L3137
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3140
	pushl $.LC129
	pushl $4312
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3140:
	cmpl $7,%ebx
	ja .L3166
	jmp *.L3167(,%ebx,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3167:
	.long .L3142
	.long .L3145
	.long .L3148
	.long .L3151
	.long .L3154
	.long .L3157
	.long .L3160
	.long .L3163
.text
	.p2align 4,,7
.L3142:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	faddp %st,%st(1)
	jmp .L3318
	.p2align 4,,7
.L3145:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	fmulp %st,%st(1)
	jmp .L3318
	.p2align 4,,7
.L3148:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3252
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 40(%esp)
	movl %edi,84(%esp)
	fildl 84(%esp)
	fstl 48(%esp)
	subl $8,%esp
	fstpl (%esp)
	pushl 52(%esp)
	pushl 52(%esp)
	call fp_set_statusword_flags_COM
	addl $32,%esp
	jmp .L3038
	.p2align 4,,7
.L3151:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3255
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 40(%esp)
	movl %edi,84(%esp)
	fildl 84(%esp)
	fstl 48(%esp)
	subl $8,%esp
	fstpl (%esp)
	pushl 52(%esp)
	pushl 52(%esp)
	call fp_set_statusword_flags_COM
	addl $32,%esp
	jmp .L3256
	.p2align 4,,7
.L3154:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	fsubrp %st,%st(1)
	jmp .L3318
	.p2align 4,,7
.L3157:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	fsubp %st,%st(1)
	jmp .L3318
	.p2align 4,,7
.L3160:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	fdivrp %st,%st(1)
	jmp .L3318
	.p2align 4,,7
.L3163:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $48,%esp
	testb %al,%al
	jne .L3164
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	movl %edi,84(%esp)
	fildl 84(%esp)
	fdivp %st,%st(1)
.L3318:
	fstpl 56(%esp)
	addl $-4,%esp
	pushl 64(%esp)
	pushl 64(%esp)
.L3316:
	pushl $0
	call fp_set_reg_ST
	addl $32,%esp
	jmp .L3038
	.p2align 4,,7
.L3164:
	addl $-4,%esp
	fldl .LC151
	subl $8,%esp
	fstpl (%esp)
	pushl $0
	call fp_set_reg_ST
	call fp_set_stack_underflow
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3166:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC137
	jmp .L3310
	.p2align 4,,7
.L3137:
	incl m_eip
	cmpb $233,%dl
	jne .L3069
	jmp .L3272
	.p2align 4,,7
.L3136:
	cmpb $219,%bl
	jne .L3174
	cmpb $191,%dl
	ja .L3175
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3178
	pushl $.LC129
	pushl $4451
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3178:
	cmpl $7,%ebx
	ja .L3197
	jmp *.L3198(,%ebx,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3198:
	.long .L3180
	.long .L3197
	.long .L3181
	.long .L3187
	.long .L3197
	.long .L3193
	.long .L3197
	.long .L3194
.text
	.p2align 4,,7
.L3180:
	addl $-8,%esp
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	addl $-8,%esp
	movl %edi,92(%esp)
	fildl 92(%esp)
	subl $8,%esp
	fstpl (%esp)
	jmp .L3319
	.p2align 4,,7
.L3181:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3333
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstl 56(%esp)
	addl $16,%esp
	fcoml .LC138
	fnstsw %ax
	andb $69,%ah
	decb %ah
	cmpb $64,%ah
	jb .L3336
	fcompl .LC139
	fnstsw %ax
	andb $5,%ah
	je .L3190
	jmp .L3189
	.p2align 4,,7
.L3187:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3188
	call fp_pop
	fstl 40(%esp)
	fcoml .LC138
	fnstsw %ax
	andb $69,%ah
	decb %ah
	cmpb $64,%ah
	jb .L3337
	fcompl .LC139
	fnstsw %ax
	andb $5,%ah
	jne .L3189
	jmp .L3190
.L3336:
.L3337:
	fstp %st(0)
.L3190:
	movl $-2147483648,%edi
	jmp .L3192
	.p2align 4,,7
.L3189:
	fldl 40(%esp)
	fnstcw 66(%esp)
	movw 66(%esp),%ax
	orw $3072,%ax
	movw %ax,64(%esp)
	fldcw 64(%esp)
	fistpl 68(%esp)
	movl 68(%esp),%edi
	fldcw 66(%esp)
	jmp .L3192
	.p2align 4,,7
.L3188:
	call fp_pop
	fstp %st(0)
.L3333:
	movl $-2147483648,%edi
	call fp_set_stack_underflow
.L3192:
	addl $-4,%esp
	pushl %edi
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
.L3314:
	call setIMem
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3193:
	addl $-12,%esp
	movl 60(%esp),%eax
	pushl %eax
	call getTMem
	jmp .L3313
	.p2align 4,,7
.L3194:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3195
	call fp_pop
	fstpl 40(%esp)
	jmp .L3196
	.p2align 4,,7
.L3195:
	call fp_pop
	fstp %st(0)
	fldl .LC151
	fstpl 40(%esp)
	call fp_set_stack_underflow
.L3196:
	addl $-4,%esp
	pushl 48(%esp)
	pushl 48(%esp)
	movl 60(%esp),%eax
	pushl %eax
	call setTMem
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3197:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC140
	jmp .L3310
	.p2align 4,,7
.L3175:
	incl m_eip
	jmp .L3069
	.p2align 4,,7
.L3174:
	cmpb $220,%bl
	jne .L3204
	cmpb $191,%dl
	ja .L3205
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3208
	pushl $.LC129
	pushl $4534
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3208:
	cmpl $7,%ebx
	ja .L3218
	jmp *.L3219(,%ebx,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3219:
	.long .L3210
	.long .L3211
	.long .L3212
	.long .L3213
	.long .L3214
	.long .L3215
	.long .L3216
	.long .L3217
.text
	.p2align 4,,7
.L3210:
	addl $-4,%esp
	pushl $1
	pushl $0
	jmp .L3307
	.p2align 4,,7
.L3211:
	addl $-4,%esp
	pushl $1
	pushl $2
	jmp .L3307
	.p2align 4,,7
.L3212:
	addl $-4,%esp
	pushl $0
	jmp .L3320
	.p2align 4,,7
.L3213:
	addl $-4,%esp
	pushl $1
.L3320:
	pushl $1
.L3308:
	movl 60(%esp),%eax
	pushl %eax
	call fp_do_COM_mem_ST_0
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3214:
	addl $-4,%esp
	pushl $1
	pushl $1
	jmp .L3307
	.p2align 4,,7
.L3215:
	addl $-4,%esp
	pushl $1
	pushl $4
	jmp .L3307
	.p2align 4,,7
.L3216:
	addl $-4,%esp
	pushl $1
	pushl $3
	jmp .L3307
	.p2align 4,,7
.L3217:
	addl $-4,%esp
	pushl $1
.L3309:
	pushl $5
.L3307:
	movl 60(%esp),%eax
	pushl %eax
	call fp_do_op_mem_ST_0
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3218:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC141
	jmp .L3310
	.p2align 4,,7
.L3205:
	incl m_eip
	movzbl %dl,%eax
	addl $-192,%eax
	cmpl $63,%eax
	ja .L3069
	jmp *.L3228(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3228:
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3222
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3223
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3224
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3225
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3226
	.long .L3226
	.long .L3226
	.long .L3226
	.long .L3226
	.long .L3226
	.long .L3226
	.long .L3226
.text
	.p2align 4,,7
.L3222:
	pushl $0
	jmp .L3340
	.p2align 4,,7
.L3223:
	pushl $0
	jmp .L3341
	.p2align 4,,7
.L3224:
	pushl $0
	jmp .L3342
	.p2align 4,,7
.L3225:
	pushl $0
	jmp .L3343
	.p2align 4,,7
.L3226:
	pushl $0
	jmp .L3322
	.p2align 4,,7
.L3204:
	cmpb $221,%bl
	jne .L3230
	cmpb $191,%dl
	ja .L3231
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3234
	pushl $.LC129
	pushl $4613
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3234:
	movl %ebx,%eax
	cmpl $2,%eax
	je .L3237
	ja .L3245
	testl %eax,%eax
	je .L3236
	jmp .L3243
	.p2align 4,,7
.L3245:
	cmpl $3,%eax
	je .L3240
	jmp .L3243
	.p2align 4,,7
.L3236:
	addl $-12,%esp
	movl 60(%esp),%eax
	pushl %eax
	call getDMem
.L3313:
	fstpl 56(%esp)
	addl $-8,%esp
	pushl 68(%esp)
	pushl 68(%esp)
.L3319:
	call fp_push
	addl $32,%esp
	jmp .L3038
	.p2align 4,,7
.L3237:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3344
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 56(%esp)
	addl $16,%esp
	jmp .L3242
	.p2align 4,,7
.L3240:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3241
	call fp_pop
	fstpl 40(%esp)
	jmp .L3242
	.p2align 4,,7
.L3241:
	call fp_pop
	fstp %st(0)
.L3344:
	fldl .LC151
	fstpl 40(%esp)
	call fp_set_stack_underflow
.L3242:
	addl $-4,%esp
	pushl 48(%esp)
	pushl 48(%esp)
	movl 60(%esp),%eax
	pushl %eax
	call setDMem
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3243:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC142
	jmp .L3310
	.p2align 4,,7
.L3231:
	incl m_eip
	movzbl %dl,%eax
	addl $-216,%eax
	cmpl $23,%eax
	ja .L3069
	jmp *.L3258(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3258:
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3248
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3251
	.long .L3254
	.long .L3254
	.long .L3254
	.long .L3254
	.long .L3254
	.long .L3254
	.long .L3254
	.long .L3254
.text
	.p2align 4,,7
.L3248:
	movzbl %dl,%ebx
	addl $-216,%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3249
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 40(%esp)
	addl $16,%esp
	jmp .L3250
	.p2align 4,,7
.L3249:
	fldl .LC151
	fstpl 24(%esp)
	call fp_set_stack_underflow
.L3250:
	addl $-4,%esp
	pushl 32(%esp)
	pushl 32(%esp)
	pushl %ebx
.L3317:
	call fp_set_reg_ST
	call fp_pop
	fstp %st(0)
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3251:
	movzbl %dl,%ebx
	addl $-224,%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3252
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3252
	addl $-12,%esp
	pushl %ebx
	call fp_get_reg_ST
	fstpl 48(%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 56(%esp)
	addl $32,%esp
	pushl 36(%esp)
	pushl 36(%esp)
	pushl 36(%esp)
	pushl 36(%esp)
	call fp_set_statusword_flags_COM
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3252:
	call fp_set_statusword_flags_COM_STACKF
	call fp_set_stack_underflow
	jmp .L3038
	.p2align 4,,7
.L3254:
	movzbl %dl,%ebx
	addl $-232,%ebx
	addl $-12,%esp
	addl $-12,%esp
	pushl %ebx
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3255
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3255
	addl $-12,%esp
	pushl %ebx
	call fp_get_reg_ST
	fstpl 48(%esp)
	addl $-12,%esp
	pushl $0
	call fp_get_reg_ST
	fstpl 56(%esp)
	addl $32,%esp
	pushl 36(%esp)
	pushl 36(%esp)
	pushl 36(%esp)
	pushl 36(%esp)
	call fp_set_statusword_flags_COM
	addl $16,%esp
	jmp .L3256
	.p2align 4,,7
.L3255:
	call fp_set_statusword_flags_COM_STACKF
	call fp_set_stack_underflow
.L3256:
	call fp_pop
	fstp %st(0)
	jmp .L3038
	.p2align 4,,7
.L3230:
	cmpb $222,%bl
	jne .L3260
	cmpb $191,%dl
	ja .L3261
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3266
	pushl $.LC129
	pushl $4711
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3266:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC143
	jmp .L3310
	.p2align 4,,7
.L3261:
	incl m_eip
	movzbl %dl,%eax
	addl $-192,%eax
	cmpl $63,%eax
	ja .L3069
	jmp *.L3278(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L3278:
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3270
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3271
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3272
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3069
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3273
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3274
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3275
	.long .L3276
	.long .L3276
	.long .L3276
	.long .L3276
	.long .L3276
	.long .L3276
	.long .L3276
	.long .L3276
.text
	.p2align 4,,7
.L3270:
	pushl $1
.L3340:
	pushl $0
	movzbl %dl,%eax
	addl $-192,%eax
	jmp .L3321
	.p2align 4,,7
.L3271:
	pushl $1
.L3341:
	pushl $2
	movzbl %dl,%eax
	addl $-200,%eax
	jmp .L3321
	.p2align 4,,7
.L3272:
	addl $-4,%esp
	pushl $2
	pushl $0
	pushl $1
.L3312:
	call fp_do_COM_ST_ST
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3273:
	pushl $1
.L3342:
	pushl $4
	movzbl %dl,%eax
	addl $-224,%eax
	jmp .L3321
	.p2align 4,,7
.L3274:
	pushl $1
.L3343:
	pushl $1
	movzbl %dl,%eax
	addl $-232,%eax
	jmp .L3321
	.p2align 4,,7
.L3275:
	pushl $1
	pushl $5
	movzbl %dl,%eax
	addl $-240,%eax
	jmp .L3321
	.p2align 4,,7
.L3276:
	pushl $1
.L3322:
	pushl $3
	movzbl %dl,%eax
	addl $-248,%eax
.L3321:
	pushl %eax
	pushl $0
.L3311:
	call fp_do_op_ST_ST
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3260:
	cmpb $223,%bl
	jne .L3069
	cmpb $191,%dl
	ja .L3281
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ebx
	andl $7,%ebx
	addl $-4,%esp
	leal 59(%esp),%eax
	pushl %eax
	leal 56(%esp),%eax
	pushl %eax
	pushl $4
	call amode_from_modRM
	addl $16,%esp
	cmpb $0,55(%esp)
	je .L3284
	pushl $.LC129
	pushl $4766
	pushl $.LC116
	pushl $.LC132
	call __assert_fail
	.p2align 4,,7
.L3284:
	movl %ebx,%eax
	cmpl $5,%eax
	je .L3286
	cmpl $7,%eax
	je .L3287
	jmp .L3293
	.p2align 4,,7
.L3286:
	addl $-8,%esp
	movl 56(%esp),%eax
	addl $4,%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	movl %edi,%ebx
	movl %edi,%esi
	sarl $31,%esi
	movl %ebx,%esi
	xorl %ebx,%ebx
	addl $-8,%esp
	movl 72(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,%edi
	movl %edi,%eax
	cltd
	addl %eax,%ebx
	adcl %edx,%esi
	addl $32,%esp
	addl $-8,%esp
	movl %ebx,64(%esp)
	movl %esi,68(%esp)
	fildll 64(%esp)
	subl $8,%esp
	fstpl (%esp)
.L3315:
	call fp_push
	addl $16,%esp
	jmp .L3038
	.p2align 4,,7
.L3287:
	addl $-12,%esp
	addl $-12,%esp
	pushl $0
	call fp_get_tag_ST
	pushl %eax
	call fp_is_empty_tag
	addl $32,%esp
	testb %al,%al
	jne .L3288
	call fp_pop
	fstl 40(%esp)
	fcoml .LC144
	fnstsw %ax
	andb $69,%ah
	decb %ah
	cmpb $64,%ah
	jb .L3338
	fcompl .LC145
	fnstsw %ax
	andb $5,%ah
	jne .L3289
	jmp .L3290
.L3338:
	fstp %st(0)
.L3290:
	movl .LC146,%ebx
	movl .LC146+4,%esi
	jmp .L3292
	.p2align 4,,7
.L3289:
	fldl 40(%esp)
	fnstcw 66(%esp)
	movw 66(%esp),%ax
	orw $3072,%ax
	movw %ax,64(%esp)
	fldcw 64(%esp)
	fistpll 56(%esp)
	movl 56(%esp),%ebx
	movl 60(%esp),%esi
	fldcw 66(%esp)
	jmp .L3292
	.p2align 4,,7
.L3288:
	call fp_pop
	fstp %st(0)
	movl .LC146,%ebx
	movl .LC146+4,%esi
	call fp_set_stack_underflow
.L3292:
	addl $-4,%esp
	pushl %ebx
	movl 56(%esp),%eax
	pushl %eax
	pushl $4
	call setIMem
	addl $-4,%esp
	movl %ebx,%eax
	movl %esi,%edx
	movl %edx,%eax
	sarl $31,%edx
	pushl %eax
	movl 72(%esp),%eax
	addl $4,%eax
	pushl %eax
	pushl $4
	call setIMem
	addl $32,%esp
	jmp .L3038
	.p2align 4,,7
.L3293:
	addl $-8,%esp
	pushl %ebx
	pushl $.LC133
	call printf
	addl $-12,%esp
	pushl $.LC147
.L3310:
	call panic
	addl $32,%esp
	jmp .L3038
	.p2align 4,,7
.L3281:
	incl m_eip
	cmpb $224,%dl
	jne .L3069
	movzwl m_fpu_state+4,%eax
	movw $0,m_regs
	orl %eax,m_regs
	jmp .L3038
	.p2align 4,,7
.L3069:
	addl $-4,%esp
	movzbl %dl,%eax
	pushl %eax
	movzbl %bl,%eax
	pushl %eax
	pushl $.LC148
	call printf
	addl $-12,%esp
	pushl $.LC149
	call panic
	addl $32,%esp
	pushl $.LC129
	pushl $4839
	pushl $.LC116
	pushl $.LC150
	call __assert_fail
	.p2align 4,,7
.L3038:
	popl %ebx
	popl %esi
	popl %edi
	addl $96,%esp
	ret
.Lfe84:
	.size	 do_one_insn_fp,.Lfe84-do_one_insn_fp
.section	.rodata
.LC152:
	.string	"=== heimdall: startup\n"
.LC153:
	.string	"HD_STOP_AFTER"
	.align 32
.LC154:
	.string	"=== heimdall: will stop after %d instructions\n"
.LC155:
	.string	"done %d insns\n"
	.align 32
.LC156:
	.string	"=== heimdall: exiting, did %d insns\n"
.text
	.align 4
.globl hd_do_insns
	.type	 hd_do_insns,@function
hd_do_insns:
	subl $12,%esp
	call fp_unpack_data_regs
	movl $-1,n_to_simulate
	movl $0,num_insns_done
	addl $-12,%esp
	movl stderr,%eax
	pushl %eax
	call fflush
	addl $-8,%esp
	pushl $.LC152
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $32,%esp
	addl $-12,%esp
	pushl $.LC153
	call getenv
	addl $16,%esp
	testl %eax,%eax
	je .L3357
	pushl $0
	pushl $10
	pushl $0
	pushl %eax
	call __strtol_internal
	addl $16,%esp
	movl %eax,n_to_simulate
	addl $-4,%esp
	pushl %eax
	pushl $.LC154
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $16,%esp
	.p2align 4,,7
.L3357:
	cmpl $0,n_to_simulate
	je .L3353
	decl n_to_simulate
	testl $1048575,num_insns_done
	jne .L3356
	cmpl $0,num_insns_done
	je .L3356
	addl $-4,%esp
	movl num_insns_done,%eax
	pushl %eax
	pushl $.LC155
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $16,%esp
.L3356:
	call do_one_insn
	testb %al,%al
	je .L3357
.L3353:
	addl $-12,%esp
	movl stderr,%eax
	pushl %eax
	call fflush
	addl $-4,%esp
	movl num_insns_done,%eax
	pushl %eax
	pushl $.LC156
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $32,%esp
	call fp_repack_data_regs
	call hd_switch_to_real_CPU
	addl $12,%esp
	ret
.Lfe85:
	.size	 hd_do_insns,.Lfe85-hd_do_insns
	.comm	m_regs,32,32
	.comm	m_eflags,4,4
	.comm	m_eip,4,4
	.align 4
	.type	 amode_from_modRM,@function
amode_from_modRM:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 36(%esp),%esi
	movl 40(%esp),%edi
	movl m_eip,%eax
	movb (%eax),%cl
	incl m_eip
	andb $199,%cl
	movb %cl,%al
	shrb $3,%al
	orb %al,%cl
	andb $31,%cl
	movzbl %cl,%eax
	jmp *cases.468(,%eax,4)
	.p2align 4,,7
.L515:
	movl m_regs,%eax
	jmp .L3358
	.p2align 4,,7
.L516:
	movl m_regs+4,%eax
	jmp .L3358
	.p2align 4,,7
.L517:
	movl m_regs+8,%eax
	jmp .L3358
	.p2align 4,,7
.L518:
	movl m_regs+12,%eax
	jmp .L3358
	.p2align 4,,7
.L519:
	movl m_eip,%eax
	movb (%eax),%al
	incl m_eip
	movb %al,%dl
	shrb $6,%dl
	movb %dl,15(%esp)
	movb %al,%cl
	shrb $3,%cl
	andb $7,%cl
	movb %al,%dl
	andb $7,%dl
	cmpb $4,%cl
	je .L551
	movzbl %cl,%eax
	movl m_regs(,%eax,4),%ebx
	jmp .L552
	.p2align 4,,7
.L551:
	xorl %ebx,%ebx
.L552:
	cmpb $5,%dl
	jne .L554
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movzbl 15(%esp),%ecx
	movl %ebx,%eax
	sall %cl,%eax
	addl %eax,%edx
	movl %edx,(%esi)
	jmp .L3361
	.p2align 4,,7
.L554:
	movzbl %dl,%edx
	movzbl 15(%esp),%ecx
	movl %ebx,%eax
	sall %cl,%eax
	addl m_regs(,%edx,4),%eax
	jmp .L3358
	.p2align 4,,7
.L520:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	incl m_eip
	orl %eax,%edx
	jmp .L3360
	.p2align 4,,7
.L521:
	movl m_regs+24,%eax
	jmp .L3358
	.p2align 4,,7
.L522:
	movl m_regs+28,%eax
	jmp .L3358
	.p2align 4,,7
.L523:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs,%eax
	jmp .L3358
	.p2align 4,,7
.L524:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+4,%eax
	jmp .L3358
	.p2align 4,,7
.L525:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+8,%eax
	jmp .L3358
	.p2align 4,,7
.L526:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+12,%eax
	jmp .L3358
	.p2align 4,,7
.L528:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+20,%eax
	jmp .L3358
	.p2align 4,,7
.L529:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+24,%eax
	jmp .L3358
	.p2align 4,,7
.L530:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	addl m_regs+28,%eax
.L3358:
	movl %eax,(%esi)
.L3361:
	movb $0,(%edi)
	jmp .L514
	.p2align 4,,7
.L527:
	movl m_eip,%eax
	movb (%eax),%al
	incl m_eip
	movb %al,%dl
	shrb $6,%dl
	movb %dl,14(%esp)
	movb %al,%dl
	shrb $3,%dl
	andb $7,%dl
	movb %al,%cl
	andb $7,%cl
	cmpb $4,%dl
	je .L582
	movzbl %dl,%eax
	movl m_regs(,%eax,4),%ebx
	jmp .L583
	.p2align 4,,7
.L582:
	xorl %ebx,%ebx
.L583:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movsbl %dl,%edx
	movzbl %cl,%eax
	addl m_regs(,%eax,4),%edx
	movzbl 14(%esp),%ecx
	movl %ebx,%eax
	sall %cl,%eax
	addl %eax,%edx
	jmp .L3360
	.p2align 4,,7
.L531:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs,%edx
	jmp .L3360
	.p2align 4,,7
.L532:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs+4,%edx
	jmp .L3360
	.p2align 4,,7
.L533:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movl %ecx,%eax
	andl $7,%eax
	addl m_regs(,%eax,4),%edx
	jmp .L3360
	.p2align 4,,7
.L534:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs+12,%edx
	jmp .L3360
	.p2align 4,,7
.L536:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs+20,%edx
	jmp .L3360
	.p2align 4,,7
.L535:
	movl m_eip,%eax
	movb (%eax),%al
	incl m_eip
	movb %al,%dl
	shrb $6,%dl
	movb %dl,13(%esp)
	movb %al,%dl
	shrb $3,%dl
	andb $7,%dl
	movb %al,%cl
	andb $7,%cl
	cmpb $4,%dl
	je .L598
	movzbl %dl,%eax
	movl m_regs(,%eax,4),%ebx
	jmp .L599
	.p2align 4,,7
.L598:
	xorl %ebx,%ebx
.L599:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movzbl %cl,%eax
	addl m_regs(,%eax,4),%edx
	movzbl 13(%esp),%ecx
	movl %ebx,%eax
	sall %cl,%eax
	addl %eax,%edx
	jmp .L3360
	.p2align 4,,7
.L537:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs+24,%edx
	jmp .L3360
	.p2align 4,,7
.L538:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	addl m_regs+28,%edx
.L3360:
	movl %edx,(%esi)
	jmp .L3361
	.p2align 4,,7
.L539:
	movb $1,(%edi)
	movl $0,(%esi)
	jmp .L514
	.p2align 4,,7
.L540:
	movb $1,(%edi)
	movl $1,(%esi)
	jmp .L514
	.p2align 4,,7
.L541:
	movb $1,(%edi)
	movl $2,(%esi)
	jmp .L514
	.p2align 4,,7
.L542:
	movb $1,(%edi)
	movl $3,(%esi)
	jmp .L514
	.p2align 4,,7
.L543:
	movb $1,(%edi)
	movl $4,(%esi)
	jmp .L514
	.p2align 4,,7
.L544:
	movb $1,(%edi)
	movl $5,(%esi)
	jmp .L514
	.p2align 4,,7
.L545:
	movb $1,(%edi)
	movl $6,(%esi)
	jmp .L514
	.p2align 4,,7
.L546:
	movb $1,(%edi)
	movl $7,(%esi)
.L514:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe86:
	.size	 amode_from_modRM,.Lfe86-amode_from_modRM
.section	.rodata
	.align 4
.LC157:
	.long 0
	.long 1
.text
	.align 4
	.type	 do_one_insn,@function
do_one_insn:
	subl $76,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	incl num_insns_done
	movl m_eip,%eax
	movb (%eax),%al
	movb %al,63(%esp)
	incl m_eip
	movl $4,64(%esp)
	cmpb $102,%al
	jne .L1094
	movl $2,64(%esp)
	movl m_eip,%eax
	movb (%eax),%al
	movb %al,63(%esp)
	incl m_eip
.L1094:
	movzbl 63(%esp),%eax
	cmpl $255,%eax
	ja .L2872
	jmp *.L2873(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2873:
	.long .L1135
	.long .L1136
	.long .L1154
	.long .L1155
	.long .L1170
	.long .L1171
	.long .L2872
	.long .L2872
	.long .L1264
	.long .L1265
	.long .L1283
	.long .L1284
	.long .L1299
	.long .L1300
	.long .L2872
	.long .L2566
	.long .L1096
	.long .L1097
	.long .L1117
	.long .L1118
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1314
	.long .L2872
	.long .L1333
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1185
	.long .L1186
	.long .L1204
	.long .L1205
	.long .L1220
	.long .L1221
	.long .L2872
	.long .L2872
	.long .L1349
	.long .L1350
	.long .L1368
	.long .L1369
	.long .L1384
	.long .L1385
	.long .L2872
	.long .L2872
	.long .L1399
	.long .L1400
	.long .L1418
	.long .L1419
	.long .L1434
	.long .L1435
	.long .L2872
	.long .L2872
	.long .L1235
	.long .L1236
	.long .L1245
	.long .L1246
	.long .L1255
	.long .L1256
	.long .L2872
	.long .L2872
	.long .L1650
	.long .L1652
	.long .L1653
	.long .L1654
	.long .L2872
	.long .L1655
	.long .L1656
	.long .L1657
	.long .L1630
	.long .L1632
	.long .L1633
	.long .L1634
	.long .L2872
	.long .L1635
	.long .L1636
	.long .L1637
	.long .L1831
	.long .L1833
	.long .L1834
	.long .L1835
	.long .L1836
	.long .L1837
	.long .L1838
	.long .L1839
	.long .L1805
	.long .L1807
	.long .L1808
	.long .L1809
	.long .L2872
	.long .L1810
	.long .L1811
	.long .L1812
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1858
	.long .L2872
	.long .L1871
	.long .L1468
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1500
	.long .L1503
	.long .L1505
	.long .L1507
	.long .L1509
	.long .L1512
	.long .L1515
	.long .L1517
	.long .L1519
	.long .L2872
	.long .L1521
	.long .L1524
	.long .L1527
	.long .L1531
	.long .L2080
	.long .L2084
	.long .L2872
	.long .L2086
	.long .L1458
	.long .L1459
	.long .L2872
	.long .L2056
	.long .L1718
	.long .L1719
	.long .L1737
	.long .L1738
	.long .L2872
	.long .L1673
	.long .L2872
	.long .L2872
	.long .L1095
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1556
	.long .L1585
	.long .L2872
	.long .L1095
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1747
	.long .L2872
	.long .L1755
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1449
	.long .L1450
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1763
	.long .L1765
	.long .L1766
	.long .L1767
	.long .L1768
	.long .L1769
	.long .L1770
	.long .L1771
	.long .L1779
	.long .L1781
	.long .L1782
	.long .L1783
	.long .L2872
	.long .L1784
	.long .L1785
	.long .L1786
	.long .L2115
	.long .L2119
	.long .L1541
	.long .L1543
	.long .L2872
	.long .L2872
	.long .L1793
	.long .L1794
	.long .L2872
	.long .L1681
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1670
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2122
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2864
	.long .L2865
	.long .L2866
	.long .L2867
	.long .L2868
	.long .L2869
	.long .L2870
	.long .L2871
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1486
	.long .L1539
	.long .L2872
	.long .L1538
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1890
	.long .L1929
	.long .L2872
	.long .L2872
	.long .L2148
	.long .L2149
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L2872
	.long .L1887
	.long .L2872
	.long .L2488
	.long .L2513
.text
	.p2align 4,,7
.L1096:
	movl $1,64(%esp)
.L1097:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1100
	movl m_regs(,%eax,4),%esi
	jmp .L1101
	.p2align 4,,7
.L1100:
	cmpl $1,64(%esp)
	jne .L1102
	cmpl $3,%eax
	ja .L1103
	movzbl m_regs(,%eax,4),%esi
	jmp .L1101
	.p2align 4,,7
.L1103:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1101
	.p2align 4,,7
.L1102:
	cmpl $2,64(%esp)
	jne .L1105
	movzwl m_regs(,%eax,4),%esi
	jmp .L1101
	.p2align 4,,7
.L1105:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1101:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_ADC
	addl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L1107
	incl %ebx
.L1107:
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	je .L1108
	cmpl $4,64(%esp)
	je .L3363
	cmpl $1,64(%esp)
	jne .L1111
.L3399:
	cmpl $3,%eax
	jbe .L3364
	jmp .L2838
	.p2align 4,,7
.L1111:
	cmpl $2,64(%esp)
	je .L3365
	jmp .L2840
	.p2align 4,,7
.L1108:
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1117:
	movl $1,64(%esp)
.L1118:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1121
	movl m_regs(,%eax,4),%ebx
	jmp .L1122
	.p2align 4,,7
.L1121:
	cmpl $1,64(%esp)
	jne .L1123
	cmpl $3,%eax
	ja .L1124
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1122
	.p2align 4,,7
.L1124:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1122
	.p2align 4,,7
.L1123:
	cmpl $2,64(%esp)
	jne .L1126
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1122
	.p2align 4,,7
.L1126:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1122:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $16,%esp
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_ADC
	addl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L1128
	incl %ebx
.L1128:
	movl 72(%esp),%eax
.L3400:
	cmpl $4,64(%esp)
	je .L3363
	cmpl $1,64(%esp)
	je .L3399
	jmp .L1111
	.p2align 4,,7
.L1135:
	movl $1,64(%esp)
.L1136:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1139
	movl m_regs(,%eax,4),%esi
	jmp .L1140
	.p2align 4,,7
.L1139:
	cmpl $1,64(%esp)
	jne .L1141
	cmpl $3,%eax
	ja .L1142
	movzbl m_regs(,%eax,4),%esi
	jmp .L1140
	.p2align 4,,7
.L1142:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1140
	.p2align 4,,7
.L1141:
	cmpl $2,64(%esp)
	jne .L1144
	movzwl m_regs(,%eax,4),%esi
	jmp .L1140
	.p2align 4,,7
.L1144:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1140:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%ecx
	pushl %ecx
	call update_flags_ADD
	addl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L1154:
	movl $1,64(%esp)
.L1155:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1158
	movl m_regs(,%eax,4),%ebx
	jmp .L1159
	.p2align 4,,7
.L1158:
	cmpl $1,64(%esp)
	jne .L1160
	cmpl $3,%eax
	ja .L1161
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1159
	.p2align 4,,7
.L1161:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1159
	.p2align 4,,7
.L1160:
	cmpl $2,64(%esp)
	jne .L1163
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1159
	.p2align 4,,7
.L1163:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1159:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_ADD
	addl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L1170:
	movl $1,64(%esp)
.L1171:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1173
	movl m_regs,%ebx
	jmp .L1174
	.p2align 4,,7
.L1173:
	cmpl $1,64(%esp)
	jne .L1175
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1174
	.p2align 4,,7
.L1175:
	cmpl $2,64(%esp)
	jne .L1178
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1174
	.p2align 4,,7
.L1178:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1174:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_ADD
	addl %esi,%ebx
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3371
	cmpl $1,64(%esp)
	jne .L1181
	movb $0,m_regs
	movl %ebx,%eax
	jmp .L3368
	.p2align 4,,7
.L1181:
	cmpl $2,64(%esp)
	je .L3369
	jmp .L2840
	.p2align 4,,7
.L1185:
	movl $1,64(%esp)
.L1186:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1189
	movl m_regs(,%eax,4),%esi
	jmp .L1190
	.p2align 4,,7
.L1189:
	cmpl $1,64(%esp)
	jne .L1191
	cmpl $3,%eax
	ja .L1192
	movzbl m_regs(,%eax,4),%esi
	jmp .L1190
	.p2align 4,,7
.L1192:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1190
	.p2align 4,,7
.L1191:
	cmpl $2,64(%esp)
	jne .L1194
	movzwl m_regs(,%eax,4),%esi
	jmp .L1190
	.p2align 4,,7
.L1194:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1190:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_AND
	andl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1204:
	movl $1,64(%esp)
.L1205:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1208
	movl m_regs(,%eax,4),%ebx
	jmp .L1209
	.p2align 4,,7
.L1208:
	cmpl $1,64(%esp)
	jne .L1210
	cmpl $3,%eax
	ja .L1211
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1209
	.p2align 4,,7
.L1211:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1209
	.p2align 4,,7
.L1210:
	cmpl $2,64(%esp)
	jne .L1213
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1209
	.p2align 4,,7
.L1213:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1209:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_AND
	andl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L1220:
	movl $1,64(%esp)
.L1221:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1223
	movl m_regs,%ebx
	jmp .L1224
	.p2align 4,,7
.L1223:
	cmpl $1,64(%esp)
	jne .L1225
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1224
	.p2align 4,,7
.L1225:
	cmpl $2,64(%esp)
	jne .L1228
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1224
	.p2align 4,,7
.L1228:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1224:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_AND
	andl %esi,%ebx
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3371
	cmpl $1,64(%esp)
	jne .L1181
	movb $0,m_regs
	movl %ebx,%eax
	jmp .L3368
	.p2align 4,,7
.L1235:
	movl $1,64(%esp)
.L1236:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1239
	movl m_regs(,%eax,4),%esi
	jmp .L1240
	.p2align 4,,7
.L1239:
	cmpl $1,64(%esp)
	jne .L1241
	cmpl $3,%eax
	ja .L1242
	movzbl m_regs(,%eax,4),%esi
	jmp .L1240
	.p2align 4,,7
.L1242:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1240
	.p2align 4,,7
.L1241:
	cmpl $2,64(%esp)
	jne .L1244
	movzwl m_regs(,%eax,4),%esi
	jmp .L1240
	.p2align 4,,7
.L1244:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1240:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_SUB
	jmp .L3370
	.p2align 4,,7
.L1245:
	movl $1,64(%esp)
.L1246:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1249
	movl m_regs(,%eax,4),%ebx
	jmp .L1250
	.p2align 4,,7
.L1249:
	cmpl $1,64(%esp)
	jne .L1251
	cmpl $3,%eax
	ja .L1252
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1250
	.p2align 4,,7
.L1252:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1250
	.p2align 4,,7
.L1251:
	cmpl $2,64(%esp)
	jne .L1254
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1250
	.p2align 4,,7
.L1254:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1250:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%ecx
	pushl %ecx
	call update_flags_SUB
	jmp .L3370
	.p2align 4,,7
.L1255:
	movl $1,64(%esp)
.L1256:
	addl $-12,%esp
	movl 76(%esp),%eax
	pushl %eax
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1258
	movl m_regs,%ebx
	jmp .L1259
	.p2align 4,,7
.L1258:
	cmpl $1,64(%esp)
	jne .L1260
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1259
	.p2align 4,,7
.L1260:
	cmpl $2,64(%esp)
	jne .L1263
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1259
	.p2align 4,,7
.L1263:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1259:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SUB
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L1264:
	movl $1,64(%esp)
.L1265:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1268
	movl m_regs(,%eax,4),%esi
	jmp .L1269
	.p2align 4,,7
.L1268:
	cmpl $1,64(%esp)
	jne .L1270
	cmpl $3,%eax
	ja .L1271
	movzbl m_regs(,%eax,4),%esi
	jmp .L1269
	.p2align 4,,7
.L1271:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1269
	.p2align 4,,7
.L1270:
	cmpl $2,64(%esp)
	jne .L1273
	movzwl m_regs(,%eax,4),%esi
	jmp .L1269
	.p2align 4,,7
.L1273:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1269:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%ecx
	pushl %ecx
	call update_flags_OR
	orl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L1283:
	movl $1,64(%esp)
.L1284:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1287
	movl m_regs(,%eax,4),%ebx
	jmp .L1288
	.p2align 4,,7
.L1287:
	cmpl $1,64(%esp)
	jne .L1289
	cmpl $3,%eax
	ja .L1290
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1288
	.p2align 4,,7
.L1290:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1288
	.p2align 4,,7
.L1289:
	cmpl $2,64(%esp)
	jne .L1292
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1288
	.p2align 4,,7
.L1292:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1288:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_OR
	orl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L1299:
	movl $1,64(%esp)
.L1300:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1302
	movl m_regs,%ebx
	jmp .L1303
	.p2align 4,,7
.L1302:
	cmpl $1,64(%esp)
	jne .L1304
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1303
	.p2align 4,,7
.L1304:
	cmpl $2,64(%esp)
	jne .L1307
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1303
	.p2align 4,,7
.L1307:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1303:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_OR
	orl %esi,%ebx
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3371
	cmpl $1,64(%esp)
	jne .L1181
	movb $0,m_regs
	movl %ebx,%eax
	jmp .L3368
	.p2align 4,,7
.L1314:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1316
	movl m_regs(,%eax,4),%esi
	jmp .L1317
	.p2align 4,,7
.L1316:
	cmpl $1,64(%esp)
	jne .L1318
	cmpl $3,%eax
	ja .L1319
	movzbl m_regs(,%eax,4),%esi
	jmp .L1317
	.p2align 4,,7
.L1319:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1317
	.p2align 4,,7
.L1318:
	cmpl $2,64(%esp)
	jne .L1321
	movzwl m_regs(,%eax,4),%esi
	jmp .L1317
	.p2align 4,,7
.L1321:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1317:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SBB
	subl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L1323
	decl %ebx
.L1323:
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1333:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1335
	movl m_regs(,%eax,4),%ebx
	jmp .L1336
	.p2align 4,,7
.L1335:
	cmpl $1,64(%esp)
	jne .L1337
	cmpl $3,%eax
	ja .L1338
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1336
	.p2align 4,,7
.L1338:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1336
	.p2align 4,,7
.L1337:
	cmpl $2,64(%esp)
	jne .L1340
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1336
	.p2align 4,,7
.L1340:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1336:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $16,%esp
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SBB
	subl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L1128
	decl %ebx
	jmp .L1128
	.p2align 4,,7
.L1349:
	movl $1,64(%esp)
.L1350:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1353
	movl m_regs(,%eax,4),%esi
	jmp .L1354
	.p2align 4,,7
.L1353:
	cmpl $1,64(%esp)
	jne .L1355
	cmpl $3,%eax
	ja .L1356
	movzbl m_regs(,%eax,4),%esi
	jmp .L1354
	.p2align 4,,7
.L1356:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1354
	.p2align 4,,7
.L1355:
	cmpl $2,64(%esp)
	jne .L1358
	movzwl m_regs(,%eax,4),%esi
	jmp .L1354
	.p2align 4,,7
.L1358:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1354:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%ecx
	pushl %ecx
	call update_flags_SUB
	subl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L1368:
	movl $1,64(%esp)
.L1369:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1372
	movl m_regs(,%eax,4),%ebx
	jmp .L1373
	.p2align 4,,7
.L1372:
	cmpl $1,64(%esp)
	jne .L1374
	cmpl $3,%eax
	ja .L1375
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1373
	.p2align 4,,7
.L1375:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1373
	.p2align 4,,7
.L1374:
	cmpl $2,64(%esp)
	jne .L1377
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1373
	.p2align 4,,7
.L1377:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1373:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_SUB
	subl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L1384:
	movl $1,64(%esp)
.L1385:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1387
	movl m_regs,%ebx
	jmp .L1388
	.p2align 4,,7
.L1387:
	cmpl $1,64(%esp)
	jne .L1389
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1388
	.p2align 4,,7
.L1389:
	cmpl $2,64(%esp)
	jne .L1392
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1388
	.p2align 4,,7
.L1392:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1388:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_SUB
	subl %esi,%ebx
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3371
	cmpl $1,64(%esp)
	jne .L1181
	movb $0,m_regs
	movl %ebx,%eax
	jmp .L3368
	.p2align 4,,7
.L1399:
	movl $1,64(%esp)
.L1400:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1403
	movl m_regs(,%eax,4),%esi
	jmp .L1404
	.p2align 4,,7
.L1403:
	cmpl $1,64(%esp)
	jne .L1405
	cmpl $3,%eax
	ja .L1406
	movzbl m_regs(,%eax,4),%esi
	jmp .L1404
	.p2align 4,,7
.L1406:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1404
	.p2align 4,,7
.L1405:
	cmpl $2,64(%esp)
	jne .L1408
	movzwl m_regs(,%eax,4),%esi
	jmp .L1404
	.p2align 4,,7
.L1408:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1404:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_XOR
	xorl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1418:
	movl $1,64(%esp)
.L1419:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1422
	movl m_regs(,%eax,4),%ebx
	jmp .L1423
	.p2align 4,,7
.L1422:
	cmpl $1,64(%esp)
	jne .L1424
	cmpl $3,%eax
	ja .L1425
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1423
	.p2align 4,,7
.L1425:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1423
	.p2align 4,,7
.L1424:
	cmpl $2,64(%esp)
	jne .L1427
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1423
	.p2align 4,,7
.L1427:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1423:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_XOR
	xorl %esi,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L1434:
	movl $1,64(%esp)
.L1435:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1437
	movl m_regs,%ebx
	jmp .L1438
	.p2align 4,,7
.L1437:
	cmpl $1,64(%esp)
	jne .L1439
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1438
	.p2align 4,,7
.L1439:
	cmpl $2,64(%esp)
	jne .L1442
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1438
	.p2align 4,,7
.L1442:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1438:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_XOR
	xorl %esi,%ebx
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3371
	cmpl $1,64(%esp)
	jne .L1181
	movb $0,m_regs
	movl %ebx,%eax
	jmp .L3368
	.p2align 4,,7
.L1449:
	movl $1,64(%esp)
.L1450:
	addl $-12,%esp
	movl 76(%esp),%edx
	pushl %edx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1452
	movl m_regs,%ebx
	jmp .L1453
	.p2align 4,,7
.L1452:
	cmpl $1,64(%esp)
	jne .L1454
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L1453
	.p2align 4,,7
.L1454:
	cmpl $2,64(%esp)
	jne .L1457
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L1453
	.p2align 4,,7
.L1457:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1453:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_TEST
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L1458:
	movl $1,64(%esp)
.L1459:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1462
	movl m_regs(,%eax,4),%esi
	jmp .L1463
	.p2align 4,,7
.L1462:
	cmpl $1,64(%esp)
	jne .L1464
	cmpl $3,%eax
	ja .L1465
	movzbl m_regs(,%eax,4),%esi
	jmp .L1463
	.p2align 4,,7
.L1465:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1463
	.p2align 4,,7
.L1464:
	cmpl $2,64(%esp)
	jne .L1467
	movzwl m_regs(,%eax,4),%esi
	jmp .L1463
	.p2align 4,,7
.L1467:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1463:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call update_flags_TEST
	jmp .L3370
	.p2align 4,,7
.L1468:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	addl $16,%esp
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%ebx
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	addl $16,%esp
	cmpl $2,64(%esp)
	je .L1472
	ja .L1473
	cmpl $1,64(%esp)
	je .L1474
	jmp .L1475
	.p2align 4,,7
.L1473:
	cmpl $4,64(%esp)
	jne .L1475
	movl %eax,%ebp
	jmp .L1477
	.p2align 4,,7
.L1472:
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebp
	addl $16,%esp
	jmp .L1477
	.p2align 4,,7
.L1474:
	movsbl %al,%ebp
	jmp .L1477
	.p2align 4,,7
.L1475:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %ebp,%ebp
.L1477:
	addl $-4,%esp
	pushl %ebx
	pushl %ebp
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_IMUL
	imull %ebp,%ebx
	movl 88(%esp),%eax
	addl $16,%esp
	jmp .L3400
	.p2align 4,,7
.L1486:
	addl $-12,%esp
	movl 76(%esp),%eax
	pushl %eax
	call getSDisp
	movl %eax,%esi
	addl m_eip,%eax
	addl $16,%esp
	cmpl $hd_shutdown,%eax
	je .L3372
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	addl $-4,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	movl m_eip,%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	pushl $4
	call setIMem
	jmp .L3373
	.p2align 4,,7
.L1500:
	movb m_eflags,%bl
	andb $1,%bl
	jmp .L1502
	.p2align 4,,7
.L1503:
	testb $1,m_eflags
	sete %bl
	jmp .L1502
	.p2align 4,,7
.L1505:
	testb $64,m_eflags
	setne %bl
	jmp .L1502
	.p2align 4,,7
.L1507:
	testb $64,m_eflags
	sete %bl
	jmp .L1502
	.p2align 4,,7
.L1509:
	movb m_eflags,%dl
	andb $1,%dl
	testb $64,m_eflags
	setne %al
	jmp .L3374
	.p2align 4,,7
.L1512:
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	jmp .L3375
	.p2align 4,,7
.L1515:
	testb $128,m_eflags
	setne %bl
	jmp .L1502
	.p2align 4,,7
.L1517:
	testb $128,m_eflags
	sete %bl
	jmp .L1502
	.p2align 4,,7
.L1519:
	testb $4,m_eflags
	setne %bl
	jmp .L1502
	.p2align 4,,7
.L1521:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	setne %bl
	jmp .L1502
	.p2align 4,,7
.L1524:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	sete %bl
	jmp .L1502
	.p2align 4,,7
.L1527:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	xorl %ecx,%edx
.L3374:
	movb %dl,%bl
	orb %al,%bl
	jmp .L1502
	.p2align 4,,7
.L1531:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	xorl %ecx,%edx
.L3375:
	orl %eax,%edx
	movb %dl,%bl
	xorb $1,%bl
.L1502:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	movl %eax,%esi
	addl m_eip,%esi
	jmp .L3376
	.p2align 4,,7
.L1538:
	movl $1,64(%esp)
.L1539:
	addl $-12,%esp
	movl 76(%esp),%edx
	pushl %edx
	call getSDisp
	movl %eax,%esi
.L3373:
	addl %esi,m_eip
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L1541:
	call getUDisp16
	movl %eax,%esi
	jmp .L1542
	.p2align 4,,7
.L1543:
	xorl %esi,%esi
.L1542:
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	addl $-8,%esp
	movl 80(%esp),%eax
	pushl %eax
	pushl $4
	call getIMem
	movl %eax,m_eip
	addl $4,88(%esp)
	addl %esi,88(%esp)
	movl 88(%esp),%eax
	addl $16,%esp
	movl %eax,m_regs+16
	jmp .L1095
	.p2align 4,,7
.L1556:
	cmpl $2,64(%esp)
	jne .L1557
	addl $-12,%esp
	movl m_regs,%eax
	andl $255,%eax
	pushl %eax
	call extend_s_8to16
	movl %eax,%ebx
	addl $16,%esp
	movw $0,m_regs
	jmp .L3377
	.p2align 4,,7
.L1557:
	cmpl $4,64(%esp)
	jne .L1571
	addl $-12,%esp
	movl m_regs,%eax
	andl $65535,%eax
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebx
	addl $16,%esp
.L3371:
	movl %ebx,m_regs
	jmp .L1095
	.p2align 4,,7
.L1571:
	addl $-12,%esp
	pushl $.LC86
	jmp .L3378
	.p2align 4,,7
.L1585:
	cmpl $2,64(%esp)
	jne .L1586
	addl $-8,%esp
	movl m_regs,%eax
	andl $65535,%eax
	pushl %eax
	pushl $2
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L1603
	movw $0,m_regs+8
	orl $65535,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L1603:
	movw $0,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L1586:
	cmpl $4,64(%esp)
	jne .L1608
	addl $-8,%esp
	movl m_regs,%eax
	pushl %eax
	pushl $4
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L1609
	movl $-1,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L1609:
	movl $0,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L1608:
	addl $-12,%esp
	pushl $.LC87
	jmp .L3378
	.p2align 4,,7
.L1630:
	movl $0,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1632:
	movl $1,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1633:
	movl $2,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1634:
	movl $3,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1635:
	movl $5,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1636:
	movl $6,72(%esp)
	jmp .L1631
	.p2align 4,,7
.L1637:
	movl $7,72(%esp)
.L1631:
	movl 72(%esp),%eax
	cmpl $4,64(%esp)
	jne .L1638
	movl m_regs(,%eax,4),%ebx
	jmp .L1639
	.p2align 4,,7
.L1638:
	cmpl $1,64(%esp)
	jne .L1640
	cmpl $3,%eax
	ja .L1641
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1639
	.p2align 4,,7
.L1641:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1639
	.p2align 4,,7
.L1640:
	cmpl $2,64(%esp)
	jne .L1643
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1639
	.p2align 4,,7
.L1643:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1639:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_DEC
	decl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	jmp .L3400
	.p2align 4,,7
.L1650:
	movl $0,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1652:
	movl $1,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1653:
	movl $2,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1654:
	movl $3,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1655:
	movl $5,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1656:
	movl $6,72(%esp)
	jmp .L1651
	.p2align 4,,7
.L1657:
	movl $7,72(%esp)
.L1651:
	movl 72(%esp),%eax
	cmpl $4,64(%esp)
	jne .L1658
	movl m_regs(,%eax,4),%ebx
	jmp .L1659
	.p2align 4,,7
.L1658:
	cmpl $1,64(%esp)
	jne .L1660
	cmpl $3,%eax
	ja .L1661
	movzbl m_regs(,%eax,4),%ebx
	jmp .L1659
	.p2align 4,,7
.L1661:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L1659
	.p2align 4,,7
.L1660:
	cmpl $2,64(%esp)
	jne .L1663
	movzwl m_regs(,%eax,4),%ebx
	jmp .L1659
	.p2align 4,,7
.L1663:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L1659:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_INC
	incl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	jmp .L3400
	.p2align 4,,7
.L1670:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movl %eax,%ebx
	cmpl $128,%ebx
	je .L1672
	addl $-4,%esp
	pushl %ebx
	pushl $.LC88
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $.LC89
	call panic
	addl $32,%esp
.L1672:
	call fp_repack_data_regs
	call hd_do_syscall
	call fp_unpack_data_regs
	jmp .L1095
	.p2align 4,,7
.L1673:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%esi
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L1677
	cmpl $3,%eax
	ja .L1678
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L1678:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1677:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L1681:
	movl m_regs+20,%eax
	movl %eax,m_regs+16
	addl $-8,%esp
	pushl %eax
	pushl $4
	call getIMem
	addl $16,%esp
	movl %eax,m_regs+20
	addl $4,m_regs+16
	jmp .L1095
	.p2align 4,,7
.L1718:
	movl $1,64(%esp)
.L1719:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1722
	movl m_regs(,%eax,4),%esi
	jmp .L1723
	.p2align 4,,7
.L1722:
	cmpl $1,64(%esp)
	jne .L1724
	cmpl $3,%eax
	ja .L1725
	movzbl m_regs(,%eax,4),%esi
	jmp .L1723
	.p2align 4,,7
.L1725:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1723
	.p2align 4,,7
.L1724:
	cmpl $2,64(%esp)
	jne .L1727
	movzwl m_regs(,%eax,4),%esi
	jmp .L1723
	.p2align 4,,7
.L1727:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1723:
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	je .L1728
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L1731
	cmpl $3,%eax
	ja .L1732
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L1732:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1731:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L1728:
	addl $-4,%esp
	pushl %esi
	jmp .L3383
	.p2align 4,,7
.L1737:
	movl $1,64(%esp)
.L1738:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	movl 104(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L1743
	cmpl $3,%eax
	ja .L1744
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L1744:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1743:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L1747:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	incl m_eip
	orl %eax,%edx
	movl %edx,68(%esp)
	addl $-8,%esp
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIMem
	movl %eax,%esi
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1749
	movl %esi,m_regs
	jmp .L1095
	.p2align 4,,7
.L1749:
	cmpl $1,64(%esp)
	jne .L1751
	movb $0,m_regs
	movl %esi,%eax
.L3368:
	andl $255,%eax
	orl %eax,m_regs
	jmp .L1095
	.p2align 4,,7
.L1751:
	cmpl $2,64(%esp)
	jne .L2840
	movw $0,m_regs
	movl %esi,%eax
	jmp .L3377
	.p2align 4,,7
.L1755:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	incl m_eip
	orl %eax,%edx
	movl %edx,72(%esp)
	cmpl $4,64(%esp)
	jne .L1757
	movl m_regs,%esi
	jmp .L1758
	.p2align 4,,7
.L1757:
	cmpl $1,64(%esp)
	jne .L1759
	movl m_regs,%esi
	andl $255,%esi
	jmp .L1758
	.p2align 4,,7
.L1759:
	cmpl $2,64(%esp)
	jne .L1762
	movl m_regs,%esi
	movzwl %si,%esi
	jmp .L1758
	.p2align 4,,7
.L1762:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1758:
	addl $-4,%esp
	pushl %esi
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1763:
	movl $0,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1765:
	movl $1,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1766:
	movl $2,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1767:
	movl $3,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1768:
	movl $4,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1769:
	movl $5,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1770:
	movl $6,72(%esp)
	jmp .L1764
	.p2align 4,,7
.L1771:
	movl $7,72(%esp)
.L1764:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movl %eax,%esi
	movl 72(%esp),%eax
	cmpl $3,%eax
	ja .L1776
	sall $2,%eax
	movb $0,m_regs(%eax)
	orl %esi,m_regs(%eax)
	jmp .L1095
	.p2align 4,,7
.L1776:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1779:
	movl $0,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1781:
	movl $1,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1782:
	movl $2,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1783:
	movl $3,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1784:
	movl $5,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1785:
	movl $6,72(%esp)
	jmp .L1780
	.p2align 4,,7
.L1786:
	movl $7,72(%esp)
.L1780:
	addl $-12,%esp
	movl 76(%esp),%eax
	pushl %eax
	call getUDisp
	movl %eax,%esi
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L1789
	cmpl $3,%eax
	ja .L1790
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L1790:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1789:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L1793:
	movl $1,64(%esp)
.L1794:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $-12,%esp
	movl 92(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L1796
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L1799
	cmpl $3,%eax
	ja .L1800
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L1800:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L1799:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L1796:
	addl $-4,%esp
	pushl %esi
	jmp .L3383
	.p2align 4,,7
.L1805:
	movl $0,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1807:
	movl $1,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1808:
	movl $2,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1809:
	movl $3,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1810:
	movl $5,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1811:
	movl $6,72(%esp)
	jmp .L1806
	.p2align 4,,7
.L1812:
	movl $7,72(%esp)
.L1806:
	movl m_regs+16,%eax
	movl %eax,68(%esp)
	addl $-8,%esp
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIMem
	movl %eax,%edx
	movl 88(%esp),%eax
	movl %edx,%ecx
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L1819
	movl %ecx,m_regs(,%eax,4)
	jmp .L1820
	.p2align 4,,7
.L1819:
	cmpl $1,64(%esp)
	jne .L1821
	cmpl $3,%eax
	ja .L1822
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %ecx,%edx
	movzbl %dl,%edx
	orl %edx,m_regs(%eax)
	jmp .L1820
	.p2align 4,,7
.L1822:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %ecx,%eax
	andl $255,%eax
	sall $8,%eax
	orl %eax,m_regs(%edx)
	jmp .L1820
	.p2align 4,,7
.L1821:
	cmpl $2,64(%esp)
	jne .L1824
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %ecx,%edx
	movzwl %dx,%edx
	orl %edx,m_regs(%eax)
	jmp .L1820
	.p2align 4,,7
.L1824:
	addl $-12,%esp
	pushl $.LC48
	call panic
	addl $16,%esp
.L1820:
	movl 64(%esp),%ecx
	addl %ecx,68(%esp)
	movl 68(%esp),%eax
	movl %eax,m_regs+16
	jmp .L1095
	.p2align 4,,7
.L1831:
	movl $0,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1833:
	movl $1,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1834:
	movl $2,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1835:
	movl $3,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1836:
	movl $4,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1837:
	movl $5,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1838:
	movl $6,68(%esp)
	jmp .L1832
	.p2align 4,,7
.L1839:
	movl $7,68(%esp)
.L1832:
	movl 68(%esp),%eax
	cmpl $4,64(%esp)
	jne .L1840
	movl m_regs(,%eax,4),%esi
	jmp .L1841
	.p2align 4,,7
.L1840:
	cmpl $1,64(%esp)
	jne .L1842
	cmpl $3,%eax
	ja .L1843
	movzbl m_regs(,%eax,4),%esi
	jmp .L1841
	.p2align 4,,7
.L1843:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L1841
	.p2align 4,,7
.L1842:
	cmpl $2,64(%esp)
	jne .L1845
	movzwl m_regs(,%eax,4),%esi
	jmp .L1841
	.p2align 4,,7
.L1845:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L1841:
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	movl 64(%esp),%eax
	subl %eax,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	pushl %esi
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L1858:
	addl $-12,%esp
	movl 76(%esp),%ecx
	pushl %ecx
	call getUDisp
	movl %eax,%esi
	addl $16,%esp
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	movl 64(%esp),%eax
	subl %eax,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	pushl %esi
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L1871:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%ebp
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	movl 64(%esp),%ecx
	subl %ecx,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	pushl %ebp
	movl 80(%esp),%eax
	pushl %eax
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L1887:
	andb $251,m_eflags+1
	jmp .L1095
	.p2align 4,,7
.L1890:
	movl m_eip,%eax
	movb (%eax),%al
	movb %al,63(%esp)
	incl m_eip
	cmpb $174,%al
	jne .L1891
	movl m_regs+4,%edi
	testl %edi,%edi
	je .L1095
	movl m_regs,%ebx
	movzbl %bl,%ebx
	movl m_regs+28,%eax
	movl %eax,68(%esp)
	addl $-8,%esp
	movl 76(%esp),%eax
	pushl %eax
	pushl $1
	call getIMem
	movl %eax,%esi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	pushl $1
	call update_flags_SUB
	addl $32,%esp
	testb $4,m_eflags+1
	je .L1911
	decl 68(%esp)
	jmp .L1913
	.p2align 4,,7
.L1911:
	incl 68(%esp)
.L1913:
	movl 68(%esp),%eax
	movl %eax,m_regs+28
	decl %edi
	movl %edi,m_regs+4
	testb $64,m_eflags
	jne .L1095
	addl $-2,m_eip
	jmp .L1095
	.p2align 4,,7
.L1891:
	addl $-4,%esp
	movzbl 67(%esp),%eax
	pushl %eax
	pushl $.LC90
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $.LC91
	jmp .L3384
	.p2align 4,,7
.L1929:
	movl m_eip,%eax
	movb (%eax),%al
	movb %al,63(%esp)
	incl m_eip
	addb $92,%al
	cmpb $1,%al
	ja .L1930
	cmpb $164,63(%esp)
	jne .L1931
	movl $1,64(%esp)
.L1931:
	movl m_regs+4,%esi
	testl %esi,%esi
	je .L1095
	movl m_regs+24,%eax
	movl %eax,68(%esp)
	movl m_regs+28,%eax
	movl %eax,72(%esp)
	addl $-8,%esp
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIMem
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%edx
	pushl %edx
	call setIMem
	addl $32,%esp
	testb $4,m_eflags+1
	je .L1951
	movl 64(%esp),%ecx
	subl %ecx,68(%esp)
	subl %ecx,72(%esp)
	jmp .L1953
	.p2align 4,,7
.L1951:
	movl 64(%esp),%eax
	addl %eax,68(%esp)
	addl %eax,72(%esp)
.L1953:
	movl 68(%esp),%eax
	movl %eax,m_regs+24
	movl 72(%esp),%eax
	movl %eax,m_regs+28
	decl %esi
	movl %esi,m_regs+4
	addl $-2,m_eip
	cmpb $165,63(%esp)
	jne .L1095
	jmp .L3385
	.p2align 4,,7
.L1930:
	cmpb $166,63(%esp)
	jne .L1974
	movl m_regs+4,%edi
	testl %edi,%edi
	je .L1095
	movl m_regs+24,%eax
	movl %eax,68(%esp)
	movl m_regs+28,%eax
	movl %eax,72(%esp)
	addl $-8,%esp
	movl 76(%esp),%eax
	pushl %eax
	pushl $1
	call getIMem
	movl %eax,%esi
	addl $-8,%esp
	movl 96(%esp),%eax
	pushl %eax
	pushl $1
	call getIMem
	movl %eax,%ebx
	addl $32,%esp
	addl $-4,%esp
	pushl %esi
	pushl %ebx
	pushl $1
	call update_flags_SUB
	addl $16,%esp
	testb $4,m_eflags+1
	je .L1994
	decl 68(%esp)
	decl 72(%esp)
	jmp .L1996
	.p2align 4,,7
.L1994:
	incl 68(%esp)
	incl 72(%esp)
.L1996:
	movl 68(%esp),%eax
	movl %eax,m_regs+24
	movl 72(%esp),%eax
	movl %eax,m_regs+28
	decl %edi
	movl %edi,m_regs+4
	testb $64,m_eflags
	je .L1095
	addl $-2,m_eip
	jmp .L1095
	.p2align 4,,7
.L1974:
	movb 63(%esp),%al
	addb $86,%al
	cmpb $1,%al
	ja .L2018
	cmpb $170,63(%esp)
	jne .L2019
	movl $1,64(%esp)
.L2019:
	movl m_regs+4,%esi
	testl %esi,%esi
	je .L1095
	movl m_regs+28,%eax
	movl %eax,72(%esp)
	cmpl $4,64(%esp)
	jne .L2033
	movl m_regs,%ebx
	jmp .L2034
	.p2align 4,,7
.L2033:
	cmpl $1,64(%esp)
	jne .L2035
	movl m_regs,%ebx
	movzbl %bl,%ebx
	jmp .L2034
	.p2align 4,,7
.L2035:
	cmpl $2,64(%esp)
	jne .L2038
	movl m_regs,%ebx
	movzwl %bx,%ebx
	jmp .L2034
	.p2align 4,,7
.L2038:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L2034:
	addl $-4,%esp
	pushl %ebx
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call setIMem
	addl $16,%esp
	testb $4,m_eflags+1
	je .L2039
	movl 64(%esp),%ecx
	subl %ecx,72(%esp)
	jmp .L2041
	.p2align 4,,7
.L2039:
	movl 64(%esp),%eax
	addl %eax,72(%esp)
.L2041:
	movl 72(%esp),%eax
	movl %eax,m_regs+28
	decl %esi
	movl %esi,m_regs+4
	addl $-2,m_eip
	cmpb $171,63(%esp)
	jne .L1095
.L3385:
	cmpl $2,64(%esp)
	jne .L1095
	decl m_eip
	jmp .L1095
	.p2align 4,,7
.L2018:
	addl $-4,%esp
	movzbl 67(%esp),%eax
	pushl %eax
	pushl $.LC92
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $.LC93
	jmp .L3384
	.p2align 4,,7
.L2056:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2058
	movl m_regs(,%eax,4),%esi
	jmp .L2059
	.p2align 4,,7
.L2058:
	cmpl $1,64(%esp)
	jne .L2060
	cmpl $3,%eax
	ja .L2061
	movzbl m_regs(,%eax,4),%esi
	jmp .L2059
	.p2align 4,,7
.L2061:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L2059
	.p2align 4,,7
.L2060:
	cmpl $2,64(%esp)
	jne .L2063
	movzwl m_regs(,%eax,4),%esi
	jmp .L2059
	.p2align 4,,7
.L2063:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L2059:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2064
	movl %ebx,m_regs(,%eax,4)
	jmp .L2065
	.p2align 4,,7
.L2064:
	cmpl $1,64(%esp)
	jne .L2066
	cmpl $3,%eax
	ja .L2067
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %ebx,%edx
	movzbl %dl,%edx
	orl %edx,m_regs(%eax)
	jmp .L2065
	.p2align 4,,7
.L2067:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %ebx,%eax
	andl $255,%eax
	sall $8,%eax
	orl %eax,m_regs(%edx)
	jmp .L2065
	.p2align 4,,7
.L2066:
	cmpl $2,64(%esp)
	jne .L2069
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %ebx,%edx
	movzwl %dx,%edx
	orl %edx,m_regs(%eax)
	jmp .L2065
	.p2align 4,,7
.L2069:
	addl $-12,%esp
	pushl $.LC48
	call panic
	addl $16,%esp
.L2065:
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	je .L2070
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L2073
	cmpl $3,%eax
	ja .L2074
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L2074:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L2073:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L2070:
	addl $-4,%esp
	pushl %esi
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L2080:
	movl $1,64(%esp)
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	pushl $1
	call amode_from_modRM
	addl $16,%esp
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movl %eax,%esi
	jmp .L2083
	.p2align 4,,7
.L2084:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-12,%esp
	movl 92(%esp),%eax
	pushl %eax
	call getUDisp
	movl %eax,%esi
	addl $32,%esp
	jmp .L2083
	.p2align 4,,7
.L2086:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $16,%esp
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%esi
.L2083:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	cmpl $7,%ebp
	ja .L2090
	jmp *.L2103(,%ebp,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2103:
	.long .L2091
	.long .L2092
	.long .L2093
	.long .L2096
	.long .L2099
	.long .L2100
	.long .L2101
	.long .L2102
.text
	.p2align 4,,7
.L2091:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_ADD
	addl %esi,%ebx
	jmp .L3386
	.p2align 4,,7
.L2092:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_OR
	orl %esi,%ebx
	jmp .L3386
	.p2align 4,,7
.L2093:
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_ADC
	addl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L2090
	incl %ebx
	jmp .L2090
	.p2align 4,,7
.L2096:
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edi
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_SBB
	subl %esi,%ebx
	addl $16,%esp
	testl %edi,%edi
	je .L2090
	decl %ebx
	jmp .L2090
	.p2align 4,,7
.L2099:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_AND
	andl %esi,%ebx
	jmp .L3386
	.p2align 4,,7
.L2100:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_SUB
	subl %esi,%ebx
	jmp .L3386
	.p2align 4,,7
.L2101:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_XOR
	xorl %esi,%ebx
	jmp .L3386
	.p2align 4,,7
.L2102:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SUB
.L3386:
	addl $16,%esp
.L2090:
	cmpl $6,%ebp
	ja .L1095
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L2115:
	movl $1,64(%esp)
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	pushl $1
	jmp .L3387
	.p2align 4,,7
.L2119:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
.L3387:
	call amode_from_modRM
	addl $16,%esp
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movl %eax,%esi
	jmp .L2118
	.p2align 4,,7
.L2122:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $16,%esp
	movl m_regs+4,%esi
	andl $255,%esi
.L2118:
	andl $31,%esi
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	cmpl $7,%ebp
	ja .L2137
	jmp *.L2138(,%ebp,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2138:
	.long .L2131
	.long .L2132
	.long .L2137
	.long .L2133
	.long .L2134
	.long .L2135
	.long .L2137
	.long .L2136
.text
	.p2align 4,,7
.L2131:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_ROL
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call calcROL
	movl %eax,%ebx
	jmp .L3388
	.p2align 4,,7
.L2132:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_ROR
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%eax
	pushl %eax
	call calcROR
	movl %eax,%ebx
	jmp .L3388
	.p2align 4,,7
.L2133:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_and_calc_RCR
	movl %eax,%ebx
	addl $16,%esp
	jmp .L2130
	.p2align 4,,7
.L2134:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call update_flags_SHL
	movl %esi,%ecx
	sall %cl,%ebx
	addl $16,%esp
	jmp .L2130
	.p2align 4,,7
.L2135:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_SHR
	movl %esi,%ecx
	shrl %cl,%ebx
	addl $16,%esp
	jmp .L2130
	.p2align 4,,7
.L2136:
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_SAR
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%edx
	pushl %edx
	call calcSAR
	movl %eax,%ebx
	jmp .L3388
	.p2align 4,,7
.L2137:
	addl $-8,%esp
	pushl %ebp
	pushl $.LC94
	call printf
	addl $-12,%esp
	pushl $.LC95
	call panic
.L3388:
	addl $32,%esp
.L2130:
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L2148:
	movl $1,64(%esp)
.L2149:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	cmpl $7,%ebp
	ja .L2486
	jmp *.L2487(,%ebp,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2487:
	.long .L2153
	.long .L2486
	.long .L2154
	.long .L2164
	.long .L2182
	.long .L2238
	.long .L2318
	.long .L2398
.text
	.p2align 4,,7
.L2153:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	addl $-12,%esp
	movl 92(%esp),%edx
	pushl %edx
	call getUDisp
	movl %eax,%esi
	addl $32,%esp
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call getIRoM
	movl %eax,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	movl 92(%esp),%eax
	pushl %eax
	call update_flags_AND
	jmp .L3370
	.p2align 4,,7
.L2154:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%ecx
	pushl %ecx
	call getIRoM
	movl %eax,%ebx
	notl %ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L2164:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%ecx
	pushl %ecx
	call getIRoM
	addl $32,%esp
	cmpl $2,64(%esp)
	je .L2165
	ja .L2166
	cmpl $1,64(%esp)
	je .L2167
	jmp .L2168
	.p2align 4,,7
.L2166:
	cmpl $4,64(%esp)
	jne .L2168
	movl %eax,%ebx
	jmp .L2170
	.p2align 4,,7
.L2165:
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebx
	addl $16,%esp
	jmp .L2170
	.p2align 4,,7
.L2167:
	movsbl %al,%ebx
	jmp .L2170
	.p2align 4,,7
.L2168:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %ebx,%ebx
.L2170:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_NEG
	negl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L2182:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $32,%esp
	cmpl $2,64(%esp)
	je .L2200
	jg .L2237
	cmpl $1,64(%esp)
	je .L2185
	jmp .L2235
	.p2align 4,,7
.L2237:
	cmpl $4,64(%esp)
	je .L2216
	jmp .L2235
	.p2align 4,,7
.L2185:
	movl m_regs,%ebx
	movzbl %bl,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	pushl $1
	call update_flags_MUL
	imull %esi,%ebx
	jmp .L3389
	.p2align 4,,7
.L2200:
	movl m_regs,%ebx
	movzwl %bx,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	pushl $2
	call update_flags_MUL
	imull %esi,%ebx
	jmp .L3390
	.p2align 4,,7
.L2216:
	movl m_regs,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %esi
	pushl $4
	call update_flags_MUL
	movl %ebx,%ecx
	xorl %ebx,%ebx
	movl %esi,52(%esp)
	movl $0,56(%esp)
	movl %ecx,%eax
	mull 52(%esp)
	movl %eax,%edi
	movl %edx,%ebp
	movl %ecx,%esi
	imull 56(%esp),%esi
	addl %esi,%ebp
	movl 52(%esp),%eax
	imull %ebx,%eax
	addl %eax,%ebp
	movl %ebp,%eax
	xorl %edx,%edx
	addl $16,%esp
	movl %eax,m_regs+8
	movl %edi,m_regs
	jmp .L1095
	.p2align 4,,7
.L2235:
	addl $-12,%esp
	pushl $.LC96
	jmp .L3378
	.p2align 4,,7
.L2238:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%ecx
	pushl %ecx
	call getIRoM
	addl $32,%esp
	cmpl $2,64(%esp)
	je .L2239
	ja .L2240
	cmpl $1,64(%esp)
	je .L2241
	jmp .L2242
	.p2align 4,,7
.L2240:
	cmpl $4,64(%esp)
	jne .L2242
	movl %eax,%ebp
	jmp .L2244
	.p2align 4,,7
.L2239:
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebp
	addl $16,%esp
	jmp .L2244
	.p2align 4,,7
.L2241:
	movsbl %al,%ebp
	jmp .L2244
	.p2align 4,,7
.L2242:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %ebp,%ebp
.L2244:
	cmpl $2,64(%esp)
	je .L2272
	jg .L2317
	cmpl $1,64(%esp)
	je .L2249
	jmp .L2315
	.p2align 4,,7
.L2317:
	cmpl $4,64(%esp)
	je .L2296
	jmp .L2315
	.p2align 4,,7
.L2249:
	movsbl m_regs,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %ebp
	pushl $1
	call update_flags_IMUL
	imull %ebp,%ebx
.L3389:
	addl $16,%esp
.L3369:
	movw $0,m_regs
	movl %ebx,%eax
.L3377:
	andl $65535,%eax
	orl %eax,m_regs
	jmp .L1095
	.p2align 4,,7
.L2272:
	movl m_regs,%eax
	andl $65535,%eax
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebx
	addl $16,%esp
	addl $-4,%esp
	pushl %ebx
	pushl %ebp
	pushl $2
	call update_flags_IMUL
	imull %ebp,%ebx
.L3390:
	addl $16,%esp
	movw $0,m_regs+8
	movl %ebx,%eax
	shrl $16,%eax
	orl %eax,m_regs+8
	movl %ebx,%eax
	andl $65535,%eax
	movw $0,m_regs
	orl %eax,m_regs
	jmp .L1095
	.p2align 4,,7
.L2296:
	movl m_regs,%ebx
	addl $-4,%esp
	pushl %ebx
	pushl %ebp
	pushl $4
	call update_flags_IMUL
	movl %ebx,68(%esp)
	sarl $31,%ebx
	movl %ebx,72(%esp)
	movl %ebp,44(%esp)
	sarl $31,%ebp
	movl %ebp,48(%esp)
	movl 68(%esp),%eax
	mull 44(%esp)
	movl %eax,%esi
	movl %edx,%edi
	movl 68(%esp),%eax
	imull 48(%esp),%eax
	addl %eax,%edi
	movl 44(%esp),%eax
	imull 72(%esp),%eax
	addl %eax,%edi
	movl %edi,%eax
	xorl %edx,%edx
	addl $16,%esp
	movl %eax,m_regs+8
	movl %esi,m_regs
	jmp .L1095
	.p2align 4,,7
.L2315:
	addl $-12,%esp
	pushl $.LC97
	jmp .L3378
	.p2align 4,,7
.L2318:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%ecx
	pushl %ecx
	call getIRoM
	movl %eax,%esi
	addl $32,%esp
	testl %esi,%esi
	je .L2320
	cmpl $2,64(%esp)
	je .L2345
	jg .L2397
	cmpl $1,64(%esp)
	je .L2325
	jmp .L2395
	.p2align 4,,7
.L2397:
	cmpl $4,64(%esp)
	je .L2368
	jmp .L2395
	.p2align 4,,7
.L2325:
	movl m_regs,%ebx
	movzwl %bx,%ebx
	movl %ebx,%eax
	xorl %edx,%edx
	divl %esi
	cmpl $255,%eax
	ja .L2320
	movb $0,m_regs
	andl $255,%eax
	orl %eax,m_regs
	movb $0,m_regs+1
	movzbl %dl,%eax
	jmp .L3391
	.p2align 4,,7
.L2345:
	movl m_regs,%eax
	andl $65535,%eax
	movl m_regs+8,%ebx
	sall $16,%ebx
	orl %eax,%ebx
	movl %ebx,%eax
	xorl %edx,%edx
	divl %esi
	cmpl $65535,%eax
	ja .L2320
	movw $0,m_regs
	andl $65535,%eax
	orl %eax,m_regs
	movw $0,m_regs+8
	movzwl %dx,%eax
	orl %eax,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L2368:
	movl m_regs+8,%edi
	xorl %ebp,%ebp
	movl %edi,%ebp
	xorl %edi,%edi
	movl m_regs,%eax
	xorl %edx,%edx
	orl %eax,%edi
	orl %edx,%ebp
	movl %esi,%eax
	xorl %edx,%edx
	pushl %edx
	pushl %eax
	pushl %ebp
	pushl %edi
	call __udivdi3
	addl $16,%esp
	testl %edx,%edx
	ja .L2320
	movl %eax,m_regs
	movl %esi,%eax
	xorl %edx,%edx
	pushl %edx
	pushl %eax
	pushl %ebp
	pushl %edi
	call __umoddi3
	addl $16,%esp
	movl %eax,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L2320:
	addl $-12,%esp
	pushl $.LC98
	jmp .L3378
	.p2align 4,,7
.L2395:
	addl $-12,%esp
	pushl $.LC99
	jmp .L3378
	.p2align 4,,7
.L2398:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%edx
	pushl %edx
	call getIRoM
	addl $32,%esp
	cmpl $2,64(%esp)
	je .L2399
	ja .L2400
	cmpl $1,64(%esp)
	je .L2401
	jmp .L2402
	.p2align 4,,7
.L2400:
	cmpl $4,64(%esp)
	jne .L2402
	movl %eax,%ebp
	jmp .L2404
	.p2align 4,,7
.L2399:
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebp
	addl $16,%esp
	jmp .L2404
	.p2align 4,,7
.L2401:
	movsbl %al,%ebp
	jmp .L2404
	.p2align 4,,7
.L2402:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %ebp,%ebp
.L2404:
	testl %ebp,%ebp
	je .L2408
	cmpl $2,64(%esp)
	je .L2433
	jg .L2485
	cmpl $1,64(%esp)
	je .L2410
	jmp .L2483
	.p2align 4,,7
.L2485:
	cmpl $4,64(%esp)
	je .L2456
	jmp .L2483
	.p2align 4,,7
.L2410:
	addl $-12,%esp
	movl m_regs,%eax
	andl $65535,%eax
	pushl %eax
	call extend_s_16to32
	movl %eax,%ebx
	cltd
	idivl %ebp
	movl %eax,%ebx
	movl %edx,%ecx
	leal 128(%ebx),%eax
	addl $16,%esp
	cmpl $255,%eax
	ja .L2408
	movb $0,m_regs
	movl %ebx,%eax
	andl $255,%eax
	orl %eax,m_regs
	movb $0,m_regs+1
	movzbl %cl,%eax
.L3391:
	sall $8,%eax
	orl %eax,m_regs
	jmp .L1095
	.p2align 4,,7
.L2433:
	movl m_regs,%eax
	andl $65535,%eax
	movl m_regs+8,%ebx
	sall $16,%ebx
	orl %eax,%ebx
	movl %ebx,%eax
	cltd
	idivl %ebp
	movl %eax,%ebx
	movl %edx,%ecx
	leal 65536(%ebx),%eax
	cmpl $131071,%eax
	ja .L2408
	movw $0,m_regs
	movl %ebx,%eax
	andl $65535,%eax
	orl %eax,m_regs
	movw $0,m_regs+8
	movzwl %cx,%eax
	orl %eax,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L2456:
	movl m_regs+8,%eax
	xorl %edx,%edx
	movl %eax,%edx
	xorl %eax,%eax
	movl %eax,44(%esp)
	movl %edx,48(%esp)
	movl m_regs,%eax
	xorl %edx,%edx
	orl %eax,44(%esp)
	orl %edx,48(%esp)
	movl %ebp,%eax
	cltd
	pushl %edx
	pushl %eax
	movl 52(%esp),%edx
	movl 56(%esp),%ecx
	pushl %ecx
	pushl %edx
	call __divdi3
	addl $16,%esp
	movl %eax,%esi
	movl %edx,%edi
	movl %esi,%eax
	movl %edi,%edx
	addl .LC157,%eax
	adcl .LC157+4,%edx
	cmpl $1,%edx
	ja .L2408
	movl %esi,m_regs
	movl %ebp,%eax
	cltd
	pushl %edx
	pushl %eax
	movl 52(%esp),%eax
	movl 56(%esp),%edx
	pushl %edx
	pushl %eax
	call __moddi3
	addl $16,%esp
	movl %eax,%esi
	movl %edx,%edi
	movl %esi,m_regs+8
	jmp .L1095
	.p2align 4,,7
.L2408:
	addl $-12,%esp
	pushl $.LC101
	jmp .L3378
	.p2align 4,,7
.L2483:
	addl $-12,%esp
	pushl $.LC102
	jmp .L3378
	.p2align 4,,7
.L2486:
	addl $-8,%esp
	pushl %ebp
	pushl $.LC94
	call printf
	addl $-12,%esp
	pushl $.LC103
	jmp .L3384
	.p2align 4,,7
.L2488:
	movl $1,64(%esp)
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	pushl $1
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	pushl $1
	call getIRoM
	movl %eax,%ebx
	addl $32,%esp
	movl %ebp,%eax
	testl %eax,%eax
	je .L2491
	cmpl $1,%eax
	je .L2501
	jmp .L2511
	.p2align 4,,7
.L2491:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_INC
	incl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L2501:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_DEC
	decl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L2511:
	addl $-8,%esp
	pushl %ebp
	pushl $.LC94
	call printf
	addl $-12,%esp
	pushl $.LC104
	jmp .L3384
	.p2align 4,,7
.L2513:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl $7,%ebp
	andl %eax,%ebp
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $32,%esp
	cmpl $6,%ebp
	ja .L2564
	jmp *.L2565(,%ebp,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2565:
	.long .L2516
	.long .L2526
	.long .L2536
	.long .L2564
	.long .L2550
	.long .L2564
	.long .L2551
.text
	.p2align 4,,7
.L2516:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_INC
	incl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L2526:
	addl $-8,%esp
	pushl %ebx
	movl 76(%esp),%eax
	pushl %eax
	call update_flags_DEC
	decl %ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpb $0,79(%esp)
	jne .L3400
	addl $-4,%esp
	pushl %ebx
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	jmp .L3366
	.p2align 4,,7
.L2536:
	cmpl $hd_shutdown,%ebx
	jne .L2537
.L3372:
	movl $1,%eax
	jmp .L3362
	.p2align 4,,7
.L2537:
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	addl $-4,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	movl m_eip,%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	pushl $4
	call setIMem
	movl %ebx,m_eip
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L2550:
	movl %ebx,m_eip
	jmp .L1095
	.p2align 4,,7
.L2551:
	movl m_regs+16,%eax
	movl %eax,72(%esp)
	movl 64(%esp),%ecx
	subl %ecx,72(%esp)
	movl 72(%esp),%eax
	movl %eax,m_regs+16
	addl $-4,%esp
	pushl %ebx
	movl 80(%esp),%eax
	pushl %eax
	pushl %ecx
	jmp .L3366
	.p2align 4,,7
.L2564:
	addl $-8,%esp
	pushl %ebp
	pushl $.LC94
	call printf
	addl $-12,%esp
	pushl $.LC105
	jmp .L3384
	.p2align 4,,7
.L2566:
	movl m_eip,%eax
	movb (%eax),%al
	movb %al,63(%esp)
	incl m_eip
	movzbl %al,%eax
	addl $-130,%eax
	cmpl $77,%eax
	ja .L2861
	jmp *.L2862(,%eax,4)
	.p2align 4,,7
.section	.rodata
	.align 4
	.align 4
.L2862:
	.long .L2619
	.long .L2622
	.long .L2624
	.long .L2626
	.long .L2628
	.long .L2631
	.long .L2634
	.long .L2636
	.long .L2861
	.long .L2861
	.long .L2638
	.long .L2641
	.long .L2644
	.long .L2648
	.long .L2699
	.long .L2702
	.long .L2704
	.long .L2706
	.long .L2708
	.long .L2710
	.long .L2712
	.long .L2715
	.long .L2718
	.long .L2720
	.long .L2861
	.long .L2861
	.long .L2722
	.long .L2725
	.long .L2728
	.long .L2732
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2568
	.long .L2745
	.long .L2763
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2579
	.long .L2861
	.long .L2786
	.long .L2861
	.long .L2653
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2599
	.long .L2861
	.long .L2861
	.long .L2683
	.long .L2691
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2809
	.long .L2825
	.long .L2667
	.long .L2675
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2861
	.long .L2841
	.long .L2843
	.long .L2844
	.long .L2845
	.long .L2861
	.long .L2846
	.long .L2847
	.long .L2848
.text
	.p2align 4,,7
.L2568:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	movl 100(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	jne .L2570
	movl m_regs(,%eax,4),%eax
	jmp .L2571
	.p2align 4,,7
.L2570:
	cmpl $1,64(%esp)
	jne .L2572
	cmpl $3,%eax
	ja .L2573
	movzbl m_regs(,%eax,4),%eax
	jmp .L2571
	.p2align 4,,7
.L2573:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L2571
	.p2align 4,,7
.L2572:
	cmpl $2,64(%esp)
	jne .L2575
	movzwl m_regs(,%eax,4),%eax
	jmp .L2571
	.p2align 4,,7
.L2575:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
.L2571:
	movl %eax,%esi
	andl $31,%esi
	btl %esi,%ebx
	jnc .L2576
	orb $1,m_eflags
	jmp .L1095
	.p2align 4,,7
.L2576:
	andb $254,m_eflags
	jmp .L1095
	.p2align 4,,7
.L2579:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	movl 100(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	jne .L2581
	movl m_regs(,%eax,4),%eax
	jmp .L2582
	.p2align 4,,7
.L2581:
	cmpl $1,64(%esp)
	jne .L2583
	cmpl $3,%eax
	ja .L2584
	movzbl m_regs(,%eax,4),%eax
	jmp .L2582
	.p2align 4,,7
.L2584:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L2582
	.p2align 4,,7
.L2583:
	cmpl $2,64(%esp)
	jne .L2586
	movzwl m_regs(,%eax,4),%eax
	jmp .L2582
	.p2align 4,,7
.L2586:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
.L2582:
	movl %eax,%esi
	andl $31,%esi
	btl %esi,%ebx
	jnc .L2587
	orb $1,m_eflags
	jmp .L2589
	.p2align 4,,7
.L2587:
	andb $254,m_eflags
.L2589:
	movl $1,%eax
	movl %esi,%ecx
	sall %cl,%eax
	orl %eax,%ebx
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L2599:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 96(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	movl 100(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	jne .L2601
	movl m_regs(,%eax,4),%eax
	jmp .L2602
	.p2align 4,,7
.L2601:
	cmpl $1,64(%esp)
	jne .L2603
	cmpl $3,%eax
	ja .L2604
	movzbl m_regs(,%eax,4),%eax
	jmp .L2602
	.p2align 4,,7
.L2604:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L2602
	.p2align 4,,7
.L2603:
	cmpl $2,64(%esp)
	jne .L2606
	movzwl m_regs(,%eax,4),%eax
	jmp .L2602
	.p2align 4,,7
.L2606:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
.L2602:
	movl %eax,%esi
	andl $31,%esi
	btl %esi,%ebx
	jnc .L2607
	orb $1,m_eflags
	jmp .L2609
	.p2align 4,,7
.L2607:
	andb $254,m_eflags
.L2609:
	movl $-2,%eax
	movl %esi,%ecx
	roll %cl,%eax
	andl %eax,%ebx
	movl 72(%esp),%eax
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L2619:
	movb m_eflags,%bl
	andb $1,%bl
	jmp .L2621
	.p2align 4,,7
.L2622:
	testb $1,m_eflags
	sete %bl
	jmp .L2621
	.p2align 4,,7
.L2624:
	testb $64,m_eflags
	setne %bl
	jmp .L2621
	.p2align 4,,7
.L2626:
	testb $64,m_eflags
	sete %bl
	jmp .L2621
	.p2align 4,,7
.L2628:
	movb m_eflags,%dl
	andb $1,%dl
	testb $64,m_eflags
	setne %al
	jmp .L3392
	.p2align 4,,7
.L2631:
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	jmp .L3393
	.p2align 4,,7
.L2634:
	testb $128,m_eflags
	setne %bl
	jmp .L2621
	.p2align 4,,7
.L2636:
	testb $128,m_eflags
	sete %bl
	jmp .L2621
	.p2align 4,,7
.L2638:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	setne %bl
	jmp .L2621
	.p2align 4,,7
.L2641:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	sete %bl
	jmp .L2621
	.p2align 4,,7
.L2644:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	xorl %ecx,%edx
.L3392:
	movb %dl,%bl
	orb %al,%bl
	jmp .L2621
	.p2align 4,,7
.L2648:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	xorl %ecx,%edx
.L3393:
	orl %eax,%edx
	movb %dl,%bl
	xorb $1,%bl
.L2621:
	addl $-12,%esp
	movl 76(%esp),%edx
	pushl %edx
	call getSDisp
	movl %eax,%esi
	addl m_eip,%esi
	addl $16,%esp
.L3376:
	testb %bl,%bl
	je .L1095
	movl %esi,m_eip
	jmp .L1095
	.p2align 4,,7
.L2653:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2655
	movl m_regs(,%eax,4),%ebx
	jmp .L2656
	.p2align 4,,7
.L2655:
	cmpl $1,64(%esp)
	jne .L2657
	cmpl $3,%eax
	ja .L2658
	movzbl m_regs(,%eax,4),%ebx
	jmp .L2656
	.p2align 4,,7
.L2658:
	movzbl m_regs-15(,%eax,4),%ebx
	jmp .L2656
	.p2align 4,,7
.L2657:
	cmpl $2,64(%esp)
	jne .L2660
	movzwl m_regs(,%eax,4),%ebx
	jmp .L2656
	.p2align 4,,7
.L2660:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %ebx,%ebx
	addl $16,%esp
.L2656:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%ebp
	addl $-4,%esp
	pushl %ebx
	pushl %ebp
	movl 92(%esp),%ecx
	pushl %ecx
	call update_flags_IMUL
	imull %ebp,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	jmp .L3400
	.p2align 4,,7
.L2667:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	pushl $1
	call amode_from_modRM
	addl $-8,%esp
	addl $-4,%esp
	movzbl 107(%esp),%eax
	pushl %eax
	movl 100(%esp),%eax
	pushl %eax
	pushl $1
	call getIRoM
	pushl %eax
	movl 108(%esp),%ecx
	pushl %ecx
	call extend_s_8to
	movl %eax,%ebp
	movl 120(%esp),%eax
	addl $48,%esp
	cmpl $4,64(%esp)
	je .L3394
	cmpl $1,64(%esp)
	jne .L2671
	cmpl $3,%eax
	ja .L2672
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %ebp,%edx
	jmp .L3380
	.p2align 4,,7
.L2672:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %ebp,%eax
	jmp .L3381
	.p2align 4,,7
.L2671:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %ebp,%edx
	jmp .L3382
	.p2align 4,,7
.L2675:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	pushl $2
	call amode_from_modRM
	addl $-8,%esp
	addl $-4,%esp
	movzbl 107(%esp),%eax
	pushl %eax
	movl 100(%esp),%eax
	pushl %eax
	pushl $2
	call getIRoM
	pushl %eax
	movl 108(%esp),%ecx
	pushl %ecx
	call extend_s_16to
	movl %eax,%ebp
	movl 120(%esp),%eax
	addl $48,%esp
	cmpl $4,64(%esp)
	jne .L2677
.L3394:
	movl %ebp,m_regs(,%eax,4)
	jmp .L1095
	.p2align 4,,7
.L2677:
	cmpl $1,64(%esp)
	jne .L2679
	cmpl $3,%eax
	ja .L2680
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %ebp,%edx
	jmp .L3380
	.p2align 4,,7
.L2680:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %ebp,%eax
	jmp .L3381
	.p2align 4,,7
.L2679:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %ebp,%edx
	jmp .L3382
	.p2align 4,,7
.L2683:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	pushl $1
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	pushl $1
	call getIRoM
	movl %eax,%esi
	andl $255,%esi
	movl 104(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	je .L3379
	cmpl $1,64(%esp)
	jne .L2687
	cmpl $3,%eax
	ja .L2688
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L2688:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L2687:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L2691:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	pushl $2
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	pushl $2
	call getIRoM
	movl %eax,%esi
	movzwl %si,%esi
	movl 104(%esp),%eax
	addl $32,%esp
	cmpl $4,64(%esp)
	jne .L2693
.L3379:
	movl %esi,m_regs(,%eax,4)
	jmp .L1095
	.p2align 4,,7
.L2693:
	cmpl $1,64(%esp)
	jne .L2695
	cmpl $3,%eax
	ja .L2696
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3380
	.p2align 4,,7
.L2696:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %esi,%eax
	jmp .L3381
	.p2align 4,,7
.L2695:
	cmpl $2,64(%esp)
	jne .L2840
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %esi,%edx
	jmp .L3382
	.p2align 4,,7
.L2699:
	testb $8,m_eflags+1
	setne %bl
	jmp .L2701
	.p2align 4,,7
.L2702:
	testb $8,m_eflags+1
	sete %bl
	jmp .L2701
	.p2align 4,,7
.L2704:
	movb m_eflags,%bl
	andb $1,%bl
	jmp .L2701
	.p2align 4,,7
.L2706:
	testb $1,m_eflags
	sete %bl
	jmp .L2701
	.p2align 4,,7
.L2708:
	testb $64,m_eflags
	setne %bl
	jmp .L2701
	.p2align 4,,7
.L2710:
	testb $64,m_eflags
	sete %bl
	jmp .L2701
	.p2align 4,,7
.L2712:
	movb m_eflags,%dl
	andb $1,%dl
	testb $64,m_eflags
	setne %al
	jmp .L3395
	.p2align 4,,7
.L2715:
	movb m_eflags,%al
	andb $1,%al
	movzbl %al,%edx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	jmp .L3396
	.p2align 4,,7
.L2718:
	testb $128,m_eflags
	setne %bl
	jmp .L2701
	.p2align 4,,7
.L2720:
	testb $128,m_eflags
	sete %bl
	jmp .L2701
	.p2align 4,,7
.L2722:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	setne %bl
	jmp .L2701
	.p2align 4,,7
.L2725:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%eax
	cmpl %eax,%edx
	sete %bl
	jmp .L2701
	.p2align 4,,7
.L2728:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	xorl %ecx,%edx
.L3395:
	movb %dl,%bl
	orb %al,%bl
	jmp .L2701
	.p2align 4,,7
.L2732:
	testb $128,m_eflags
	setne %al
	movzbl %al,%edx
	testb $8,m_eflags+1
	setne %al
	movzbl %al,%ecx
	testb $64,m_eflags
	setne %al
	movzbl %al,%eax
	xorl %ecx,%edx
.L3396:
	orl %eax,%edx
	movb %dl,%bl
	xorb $1,%bl
.L2701:
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	pushl $1
	call amode_from_modRM
	addl $16,%esp
	movl 72(%esp),%eax
	movl %ebx,%edx
	andl $1,%edx
	cmpb $0,79(%esp)
	je .L2736
	cmpl $3,%eax
	ja .L2740
	sall $2,%eax
	movb $0,m_regs(%eax)
	orl %edx,m_regs(%eax)
	jmp .L1095
	.p2align 4,,7
.L2740:
	leal -16(,%eax,4),%eax
	movb $0,m_regs+1(%eax)
	sall $8,%edx
	orl %edx,m_regs(%eax)
	jmp .L1095
	.p2align 4,,7
.L2736:
	addl $-4,%esp
	pushl %edx
	pushl %eax
	pushl $1
	jmp .L3366
	.p2align 4,,7
.L2745:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2747
	movl m_regs(,%eax,4),%esi
	jmp .L2748
	.p2align 4,,7
.L2747:
	cmpl $1,64(%esp)
	jne .L2749
	cmpl $3,%eax
	ja .L2750
	movzbl m_regs(,%eax,4),%esi
	jmp .L2748
	.p2align 4,,7
.L2750:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L2748
	.p2align 4,,7
.L2749:
	cmpl $2,64(%esp)
	jne .L2752
	movzwl m_regs(,%eax,4),%esi
	jmp .L2748
	.p2align 4,,7
.L2752:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L2748:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movl %eax,%ebp
	andl $31,%ebp
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SHLD
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 92(%esp),%ecx
	pushl %ecx
	call calcSHLD
	movl %eax,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L2763:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2765
	movl m_regs(,%eax,4),%esi
	jmp .L2766
	.p2align 4,,7
.L2765:
	cmpl $1,64(%esp)
	jne .L2767
	cmpl $3,%eax
	ja .L2768
	movzbl m_regs(,%eax,4),%esi
	jmp .L2766
	.p2align 4,,7
.L2768:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L2766
	.p2align 4,,7
.L2767:
	cmpl $2,64(%esp)
	jne .L2770
	movzwl m_regs(,%eax,4),%esi
	jmp .L2766
	.p2align 4,,7
.L2770:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L2766:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	movl m_regs+4,%ebp
	andl $31,%ebp
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SHLD
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 92(%esp),%ecx
	pushl %ecx
	call calcSHLD
	movl %eax,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	je .L2800
	jmp .L3400
	.p2align 4,,7
.L2786:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,68(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	movl 84(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2788
	movl m_regs(,%eax,4),%esi
	jmp .L2789
	.p2align 4,,7
.L2788:
	cmpl $1,64(%esp)
	jne .L2790
	cmpl $3,%eax
	ja .L2791
	movzbl m_regs(,%eax,4),%esi
	jmp .L2789
	.p2align 4,,7
.L2791:
	movzbl m_regs-15(,%eax,4),%esi
	jmp .L2789
	.p2align 4,,7
.L2790:
	cmpl $2,64(%esp)
	jne .L2793
	movzwl m_regs(,%eax,4),%esi
	jmp .L2789
	.p2align 4,,7
.L2793:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %esi,%esi
	addl $16,%esp
.L2789:
	addl $-4,%esp
	movzbl 83(%esp),%eax
	pushl %eax
	movl 80(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%ebx
	addl $16,%esp
	movl m_regs+4,%ebp
	andl $31,%ebp
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 76(%esp),%edx
	pushl %edx
	call update_flags_SHRD
	pushl %ebx
	pushl %esi
	pushl %ebp
	movl 92(%esp),%ecx
	pushl %ecx
	call calcSHRD
	movl %eax,%ebx
	movl 104(%esp),%eax
	addl $32,%esp
	cmpb $0,79(%esp)
	jne .L3400
.L2800:
	addl $-4,%esp
	pushl %ebx
.L3383:
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
.L3366:
	call setIMem
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L2809:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%edx
	andl $7,%edx
	movl %edx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%ecx
	pushl %ecx
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	call getIRoM
	movl %eax,%esi
	addl $32,%esp
	testl %esi,%esi
	je .L3397
	andb $191,m_eflags
	addl $-8,%esp
	pushl %esi
	movl 76(%esp),%edx
	pushl %edx
	call calcBSF
	movl %eax,%ebx
	movl 88(%esp),%eax
	addl $16,%esp
	jmp .L3400
	.p2align 4,,7
.L2825:
	movl m_eip,%eax
	movb (%eax),%al
	shrb $3,%al
	movl %eax,%ecx
	andl $7,%ecx
	movl %ecx,72(%esp)
	addl $-4,%esp
	leal 83(%esp),%eax
	pushl %eax
	leal 76(%esp),%eax
	pushl %eax
	movl 76(%esp),%eax
	pushl %eax
	call amode_from_modRM
	addl $-4,%esp
	movzbl 99(%esp),%eax
	pushl %eax
	movl 92(%esp),%eax
	pushl %eax
	movl 92(%esp),%edx
	pushl %edx
	call getIRoM
	movl %eax,%esi
	addl $32,%esp
	testl %esi,%esi
	jne .L2832
.L3397:
	orb $64,m_eflags
	jmp .L1095
	.p2align 4,,7
.L2832:
	andb $191,m_eflags
	addl $-8,%esp
	pushl %esi
	movl 76(%esp),%ecx
	pushl %ecx
	call calcBSR
	movl %eax,%ebx
	movl 88(%esp),%eax
	addl $16,%esp
	cmpl $4,64(%esp)
	jne .L2835
.L3363:
	movl %ebx,m_regs(,%eax,4)
	jmp .L1095
	.p2align 4,,7
.L2835:
	cmpl $1,64(%esp)
	jne .L2837
	cmpl $3,%eax
	ja .L2838
.L3364:
	sall $2,%eax
	movb $0,m_regs(%eax)
	movl %ebx,%edx
.L3380:
	movzbl %dl,%edx
	orl %edx,m_regs(%eax)
	jmp .L1095
	.p2align 4,,7
.L2838:
	leal -16(,%eax,4),%edx
	movb $0,m_regs+1(%edx)
	movl %ebx,%eax
.L3381:
	andl $255,%eax
	sall $8,%eax
	orl %eax,m_regs(%edx)
	jmp .L1095
	.p2align 4,,7
.L2837:
	cmpl $2,64(%esp)
	jne .L2840
.L3365:
	sall $2,%eax
	movw $0,m_regs(%eax)
	movl %ebx,%edx
.L3382:
	movzwl %dx,%edx
	orl %edx,m_regs(%eax)
	jmp .L1095
	.p2align 4,,7
.L2840:
	addl $-12,%esp
	pushl $.LC48
.L3378:
	call panic
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L2841:
	movl $0,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2843:
	movl $1,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2844:
	movl $2,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2845:
	movl $3,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2846:
	movl $5,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2847:
	movl $6,72(%esp)
	jmp .L2842
	.p2align 4,,7
.L2848:
	movl $7,72(%esp)
.L2842:
	movl 72(%esp),%ecx
	sall $2,%ecx
	movl m_regs(%ecx),%ebx
	movl %ebx,%edx
	sall $24,%edx
	movl %ebx,%eax
	sall $8,%eax
	andl $16711680,%eax
	orl %eax,%edx
	movl %ebx,%eax
	shrl $8,%eax
	andl $65280,%eax
	orl %eax,%edx
	movl %ebx,%eax
	shrl $24,%eax
	orl %eax,%edx
	movl %edx,m_regs(%ecx)
	jmp .L1095
	.p2align 4,,7
.L2861:
	addl $-4,%esp
	movzbl 67(%esp),%eax
	pushl %eax
	pushl $.LC106
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $.LC107
	jmp .L3384
	.p2align 4,,7
.L2864:
	addl $-12,%esp
	pushl $216
	jmp .L3398
	.p2align 4,,7
.L2865:
	addl $-12,%esp
	pushl $217
	jmp .L3398
	.p2align 4,,7
.L2866:
	addl $-12,%esp
	pushl $218
	jmp .L3398
	.p2align 4,,7
.L2867:
	addl $-12,%esp
	pushl $219
	jmp .L3398
	.p2align 4,,7
.L2868:
	addl $-12,%esp
	pushl $220
	jmp .L3398
	.p2align 4,,7
.L2869:
	addl $-12,%esp
	pushl $221
	jmp .L3398
	.p2align 4,,7
.L2870:
	addl $-12,%esp
	pushl $222
	jmp .L3398
	.p2align 4,,7
.L2871:
	addl $-12,%esp
	pushl $223
.L3398:
	call do_one_insn_fp
	addl $16,%esp
	jmp .L1095
	.p2align 4,,7
.L2872:
	addl $-4,%esp
	movzbl 67(%esp),%eax
	pushl %eax
	pushl $.LC108
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $.LC109
.L3384:
	call panic
.L3370:
	addl $32,%esp
.L1095:
	xorl %eax,%eax
.L3362:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $76,%esp
	ret
.Lfe87:
	.size	 do_one_insn,.Lfe87-do_one_insn
	.comm	m_fpu_state,108,32
	.comm	m_fpu_data_regs,64,32
	.comm	real_fpu_state_saved_over_syscall,108,32
	.comm	esp_saved_over_syscall,4,4
	.comm	heimdall_stack,40000,32
	.comm	n_to_simulate,4,4
	.ident	"GCC: (GNU) 2.95.2 19991024 (release)"
