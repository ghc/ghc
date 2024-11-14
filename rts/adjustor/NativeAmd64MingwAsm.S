#include "include/ghcconfig.h"

#define HPTR_OFF    0
#define WPTR_OFF    8

#if defined(LEADING_UNDERSCORE)
#define CSYM(x) _ ## x
#else
#define CSYM(x) x
#endif

#define DECLARE_CSYM(x) \
    .globl CSYM(x) ; \
    CSYM(x):

/* ------------------------------------------------------------------
 * Adjustor for a call with less than 4 integer arguments.
 * ------------------------------------------------------------------ */
DECLARE_CSYM(simple_ccall_adjustor)
    // Shuffle the argument registers down
    mov %r8, %r9
    mov %rdx, %r8
    mov %rcx, %rdx
    movsd %xmm2, %xmm3
    movsd %xmm1, %xmm2
    movsd %xmm0, %xmm1
    mov lcl_simple_ccall_adjustor_context(%rip), %rax  // load the address of the context
    mov HPTR_OFF(%rax), %rcx                 // load the StablePtr
    jmp *WPTR_OFF(%rax)                      // jump to the entrypoint

.align 8
DECLARE_CSYM(simple_ccall_adjustor_context)
// See Note [Adjustors: Local symbol references] in NativeAmd64Asm.S
lcl_simple_ccall_adjustor_context:
    // this will be overwritten with a pointer to the AdjustorContext
    .quad 0
DECLARE_CSYM(simple_ccall_adjustor_end)

/* ------------------------------------------------------------------
 * Adjustor for a call with 4 or more integer arguments and where the fourth
 * argument is not floating-point.
 * ------------------------------------------------------------------ */
DECLARE_CSYM(complex_nofloat_ccall_adjustor)
    sub $8,%rsp
    // Handle the fourth argument; this is the only difference between the
    // float/non-float cases
    mov %r9, 0x20(%rsp)
    // Push the new return address onto the stack
    pushq complex_nofloat_ccall_ret_code_ptr(%rip)
    // But the old return address has been moved up into a spill slot, so we
    // need to move it above them
    mov 0x10(%rsp), %r9
    mov %r9, 0x30(%rsp)
    // Now do the normal argument shuffle
    mov %r8, %r9
    mov %rdx, %r8
    mov %rcx, %rdx
    movsd %xmm2, %xmm3
    movsd %xmm1, %xmm2
    movsd %xmm0, %xmm1
    // Load the address of the context
    mov lcl_complex_nofloat_ccall_adjustor_context(%rip), %rax
    mov HPTR_OFF(%rax), %rcx
    jmpq *WPTR_OFF(%rax)

.align 8
complex_nofloat_ccall_ret_code_ptr:
    .quad complex_ccall_ret_code
DECLARE_CSYM(complex_nofloat_ccall_adjustor_context)
// See Note [Adjustors: Local symbol references] in NativeAmd64Asm.S
lcl_complex_nofloat_ccall_adjustor_context:
    // this will be overwritten with a pointer to the AdjustorContext
    .quad 0x0
DECLARE_CSYM(complex_nofloat_ccall_adjustor_end)

/* ------------------------------------------------------------------
 * Adjustor for a call with 4 or more integer arguments and where the fourth
 * argument is floating point.
 * ------------------------------------------------------------------ */
DECLARE_CSYM(complex_float_ccall_adjustor)
    sub $8,%rsp
    // Handle the fourth argument; this is the only difference between the
    // float/non-float cases
    movsd %xmm3,0x20(%rsp)
    // Push the new return address onto the stack
    pushq complex_float_ccall_ret_code_ptr(%rip)
    // But the old return address has been moved up into a spill slot, so we
    // need to move it above them
    mov 0x10(%rsp),%r9
    mov %r9,0x30(%rsp)
    // Now do the normal argument shuffle
    mov %r8, %r9
    mov %rdx, %r8
    mov %rcx, %rdx
    movsd %xmm2, %xmm3
    movsd %xmm1, %xmm2
    movsd %xmm0, %xmm1
    // Load the address of the context
    mov complex_float_ccall_adjustor_context(%rip), %rax
    mov HPTR_OFF(%rax), %rcx
    jmpq *WPTR_OFF(%rax)

.align 8
complex_float_ccall_ret_code_ptr:
    .quad complex_ccall_ret_code
DECLARE_CSYM(complex_float_ccall_adjustor_context)
// See Note [Adjustors: Local symbol references] in NativeAmd64Asm.S
lcl_complex_float_ccall_adjustor_context:
    // this will be overwritten with a pointer to the AdjustorContext
    .quad 0x0
DECLARE_CSYM(complex_float_ccall_adjustor_end)


/*
  When generating an adjustor thunk that uses the C calling
  convention, we have to make sure that the thunk kicks off
  the process of jumping into Haskell with a tail jump. Why?
  Because as a result of jumping in into Haskell we may end
  up freeing the very adjustor thunk we came from using
  freeHaskellFunctionPtr(). Hence, we better not return to
  the adjustor code on our way  out, since it could by then
  point to junk.

  The fix is readily at hand, just include the opcodes
  for the C stack fixup code that we need to perform when
  returning in some static piece of memory and arrange
  to return to it before tail jumping from the adjustor thunk.
*/
complex_ccall_ret_code:
    addq $0x8, %rsp
    /* On Win64, we had to put the original return address after the
       arg 1-4 spill slots, ro now we have to move it back */
    movq 0x20(%rsp), %rcx
    movq %rcx, (%rsp)
    ret

/* mark stack as nonexecutable */
#if defined(HAVE_GNU_NONEXEC_STACK)
.section .note.GNU-stack,"",@progbits
#endif
