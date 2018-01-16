
.section .ctors,"aw"
	.long cacheprof_register2_me
.section .dtors,"aw"
	.long cacheprof_finalise2_me
	
.text
        .align 4

cacheprof_hook_Rd1:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Rd1
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Rd2:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Rd2
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Rd4:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Rd4
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret

cacheprof_hook_Rd8:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Rd8
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret

cacheprof_hook_Mo1:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Mo1
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Mo2:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Mo2
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Mo4:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Mo4
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret

cacheprof_hook_Mo8:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Mo8
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret

cacheprof_hook_Wr1:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Wr1
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Wr2:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Wr2
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_hook_Wr4:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Wr4
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret

cacheprof_hook_Wr8:
	pushl %ecx
	pushl %edx
	pushl %esi
	pushl %edi
	pushl %ebp
	pushfl
	pushl %eax         #  eax has addr referenced
	pushl %ebx         #  ebx has addr of cc
	call cacheprof_log_Wr8
	addl $8,%esp
	popfl
	popl %ebp
	popl %edi
	popl %esi
	popl %edx
	popl %ecx
	ret
	
cacheprof_register2_me:
        pushal
	pushfl
	movl $.Lcacheprof_magic_table,%eax
	pushl %eax
	call cacheprof_register2_module
	popl %eax
	popfl
	popal
	ret

cacheprof_finalise2_me:
        pushal
	pushfl
	movl $.Lcacheprof_magic_table,%eax
	pushl %eax
	call cacheprof_finalise2_module
	popl %eax
	popfl
	popal
	ret
