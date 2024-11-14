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


/*
 * Note [Adjustor templates live in data section]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Some linkers (namely Apple's ld64 and some ld.lld configurations on Linux) do not allow relocations in the text section; in
 * principle this should be mitigated by only using local symbol references, as
 * described in Note [Adjustors: Local symbol references]. However, for reasons
 * that remain a mystery the assembler produces a relocations regardless.
 * To work around this we must declare this code to be data. This is okay since
 * we will never execute it here; it will always be copied to an executable
 * page first.
 */
#if defined(darwin_HOST_OS)
.section __DATA,__data
#else
.section .data
#endif

/*
 * Note [Adjustors: Local symbol references]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Some platforms (e.g. Darwin) don't allow relocations in text sections. However, 
 * the assembler tends to produce relocations for references to a global symbols.
 * Consequently we must refer to RIP-relative things only via local symbols.
 */

/* ------------------------------------------------------------------
 * Adjustor for a call with less than 6 integer arguments.
 * ------------------------------------------------------------------ */
DECLARE_CSYM(simple_ccall_adjustor)
    // Shuffle the argument registers down
    mov %r8, %r9
    mov %rcx, %r8
    mov %rdx, %rcx
    mov %rsi, %rdx
    mov %rdi, %rsi
    mov lcl_simple_ccall_adjustor_context(%rip), %rax  // load the address of the context
    mov HPTR_OFF(%rax), %rdi                 // load the StablePtr
    jmp *WPTR_OFF(%rax)                      // jump to the entrypoint

.align 8
DECLARE_CSYM(simple_ccall_adjustor_context)
lcl_simple_ccall_adjustor_context: // See Note [Adjustors: Local symbol references]
    // this will be overwritten with a pointer to the AdjustorContext
    .quad 0
DECLARE_CSYM(simple_ccall_adjustor_end)

/* ------------------------------------------------------------------
 * Adjustor for a call with 6 or more integer arguments.
 * ------------------------------------------------------------------ */
DECLARE_CSYM(complex_ccall_adjustor)
    push %r9
    pushq complex_ccall_ret_code_ptr(%rip)
    // Shuffle the argument registers down
    mov %r8, %r9
    mov %rcx, %r8
    mov %rdx, %rcx
    mov %rsi, %rdx
    mov %rdi, %rsi
    mov lcl_complex_ccall_adjustor_context(%rip), %rax  // load the address of the context
    mov HPTR_OFF(%rax), %rdi         // load the StablePtr
    jmpq *WPTR_OFF(%rax)             // jump to the entrypoint

.align 8
complex_ccall_ret_code_ptr:
    .quad complex_ccall_ret_code
DECLARE_CSYM(complex_ccall_adjustor_context)
lcl_complex_ccall_adjustor_context: // See Note [Adjustors: Local symbol references]
    // this will be overwritten with a pointer to the AdjustorContext
    .quad 0x0
DECLARE_CSYM(complex_ccall_adjustor_end)


/* See Note [Adjustor templates live in data section]. */
#if defined(darwin_HOST_OS)
.section __TEXT,__text
#else
.section .text
#endif

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
    ret

/* mark stack as nonexecutable */
#if defined(HAVE_GNU_NONEXEC_STACK)
.section .note.GNU-stack,"",@progbits
#endif
