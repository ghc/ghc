/* -----------------------------------------------------------------------------
   The x86 register mapping

   Ok, we've only got 6 general purpose registers, a frame pointer and a
   stack pointer.  \tr{%eax} and \tr{%edx} are return values from C functions,
   hence they get trashed across ccalls and are caller saves. \tr{%ebx},
   \tr{%esi}, \tr{%edi}, \tr{%ebp} are all callee-saves.

   Reg     STG-Reg
   ---------------
   ebx     Base
   ebp     Sp
   esi     R1
   edi     Hp

   Leaving SpLim out of the picture.
   -------------------------------------------------------------------------- */

#if defined(MACHREGS_i386)

#define REG(x) __asm__("%" #x)

#if !defined(not_doing_dynamic_linking)
#define REG_Base    ebx
#endif
#define REG_Sp      ebp

#if !defined(STOLEN_X86_REGS)
#define STOLEN_X86_REGS 4
#endif

#if STOLEN_X86_REGS >= 3
# define REG_R1     esi
#endif

#if STOLEN_X86_REGS >= 4
# define REG_Hp     edi
#endif
#define REG_MachSp  esp

#define REG_XMM1    xmm0
#define REG_XMM2    xmm1
#define REG_XMM3    xmm2
#define REG_XMM4    xmm3

#define REG_YMM1    ymm0
#define REG_YMM2    ymm1
#define REG_YMM3    ymm2
#define REG_YMM4    ymm3

#define REG_ZMM1    zmm0
#define REG_ZMM2    zmm1
#define REG_ZMM3    zmm2
#define REG_ZMM4    zmm3

#define MAX_REAL_VANILLA_REG 1  /* always, since it defines the entry conv */
#define MAX_REAL_FLOAT_REG   0
#define MAX_REAL_DOUBLE_REG  0
#define MAX_REAL_LONG_REG    0
#define MAX_REAL_XMM_REG     4
#define MAX_REAL_YMM_REG     4
#define MAX_REAL_ZMM_REG     4

/* -----------------------------------------------------------------------------
  The x86-64 register mapping

  %rax          caller-saves, don't steal this one
  %rbx          YES
  %rcx          arg reg, caller-saves
  %rdx          arg reg, caller-saves
  %rsi          arg reg, caller-saves
  %rdi          arg reg, caller-saves
  %rbp          YES (our *prime* register)
  %rsp          (unavailable - stack pointer)
  %r8           arg reg, caller-saves
  %r9           arg reg, caller-saves
  %r10          caller-saves
  %r11          caller-saves
  %r12          YES
  %r13          YES
  %r14          YES
  %r15          YES

  %xmm0-7       arg regs, caller-saves
  %xmm8-15      caller-saves

  Use the caller-saves regs for Rn, because we don't always have to
  save those (as opposed to Sp/Hp/SpLim etc. which always have to be
  saved).

  --------------------------------------------------------------------------- */

#elif defined(MACHREGS_x86_64)

#define REG(x) __asm__("%" #x)

#define REG_Base  r13
#define REG_Sp    rbp
#define REG_Hp    r12
#define REG_R1    rbx
#define REG_R2    r14
#define REG_R3    rsi
#define REG_R4    rdi
#define REG_R5    r8
#define REG_R6    r9
#define REG_SpLim r15
#define REG_MachSp  rsp

/*
Map Fn, Dn and XMMn to register xmmn.
This unfortunately conflicts with the C calling convention, where the first
argument and destination registers is xmm0, but the GHC calling convention in
LLVM starts with xmm1 instead (and we can't easily change that).

The aliasing allows us to pass a function any combination of up to
six Float#, Double# or vector arguments without touching the stack
(when using the System V calling convention).
See Note [Overlapping global registers] for implications.
*/

#define REG_F1    xmm1
#define REG_F2    xmm2
#define REG_F3    xmm3
#define REG_F4    xmm4
#define REG_F5    xmm5
#define REG_F6    xmm6

#define REG_D1    xmm1
#define REG_D2    xmm2
#define REG_D3    xmm3
#define REG_D4    xmm4
#define REG_D5    xmm5
#define REG_D6    xmm6

#define REG_XMM1    xmm1
#define REG_XMM2    xmm2
#define REG_XMM3    xmm3
#define REG_XMM4    xmm4
#define REG_XMM5    xmm5
#define REG_XMM6    xmm6

#define REG_YMM1    ymm1
#define REG_YMM2    ymm2
#define REG_YMM3    ymm3
#define REG_YMM4    ymm4
#define REG_YMM5    ymm5
#define REG_YMM6    ymm6

#define REG_ZMM1    zmm1
#define REG_ZMM2    zmm2
#define REG_ZMM3    zmm3
#define REG_ZMM4    zmm4
#define REG_ZMM5    zmm5
#define REG_ZMM6    zmm6

#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_R3
#define CALLER_SAVES_R4
#endif
#define CALLER_SAVES_R5
#define CALLER_SAVES_R6

#define CALLER_SAVES_F1
#define CALLER_SAVES_F2
#define CALLER_SAVES_F3
#define CALLER_SAVES_F4
#define CALLER_SAVES_F5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_F6
#endif

#define CALLER_SAVES_D1
#define CALLER_SAVES_D2
#define CALLER_SAVES_D3
#define CALLER_SAVES_D4
#define CALLER_SAVES_D5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_D6
#endif

#define CALLER_SAVES_XMM1
#define CALLER_SAVES_XMM2
#define CALLER_SAVES_XMM3
#define CALLER_SAVES_XMM4
#define CALLER_SAVES_XMM5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_XMM6
#endif

#define CALLER_SAVES_YMM1
#define CALLER_SAVES_YMM2
#define CALLER_SAVES_YMM3
#define CALLER_SAVES_YMM4
#define CALLER_SAVES_YMM5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_YMM6
#endif

#define CALLER_SAVES_ZMM1
#define CALLER_SAVES_ZMM2
#define CALLER_SAVES_ZMM3
#define CALLER_SAVES_ZMM4
#define CALLER_SAVES_ZMM5
#if !defined(mingw32_HOST_OS)
#define CALLER_SAVES_ZMM6
#endif

#define MAX_REAL_VANILLA_REG 6
#define MAX_REAL_FLOAT_REG   6
#define MAX_REAL_DOUBLE_REG  6
#define MAX_REAL_LONG_REG    0
#define MAX_REAL_XMM_REG     6
#define MAX_REAL_YMM_REG     6
#define MAX_REAL_ZMM_REG     6

#endif  /* MACHREGS_i386 || MACHREGS_x86_64 */