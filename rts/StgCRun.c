/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2011
 *
 * STG-to-C glue.
 *
 * To run an STG function from C land, call
 *
 *              rv = StgRun(f,BaseReg);
 *
 * where "f" is the STG function to call, and BaseReg is the address of the
 * RegTable for this run (we might have separate RegTables if we're running
 * multiple threads on an SMP machine).
 *
 * In the end, "f" must JMP to StgReturn (defined below), passing the
 * return-value "rv" in R1, to return to the caller of StgRun returning "rv" in
 * the whatever way C returns a value.
 *
 * NOTE: StgRun/StgReturn do *NOT* load or store Hp or any other registers
 * (other than saving the C callee-saves registers). Instead, the called
 * function "f" must do that in STG land.
 *
 * We also initially make sure that there are @RESERVED_C_STACK_BYTES@ on the
 * C-stack. This is done to reserve some space for the allocation of
 * temporaries in STG code.
 *
 * -------------------------------------------------------------------------- */

#include "PosixSource.h"
#include "ghcconfig.h"

#if defined(sparc_HOST_ARCH) || defined(USE_MINIINTERPRETER)
/* include Stg.h first because we want real machine regs in here: we
 * have to get the value of R1 back from Stg land to C land intact.
 */

/* We include windows.h very early, as on Win64 the CONTEXT type has
   fields "R8", "R9" and "R10", which goes bad if we've already
   #define'd those names for our own purposes (in stg/Regs.h) */
#if defined(HAVE_WINDOWS_H)
#include <windows.h>
#endif

#define IN_STGCRUN 1
#include "Stg.h"
#include "Rts.h"
#else
/* The other architectures do not require the actual register macro definitions
 * here because they use hand written assembly to implement the StgRun
 * function. Including Stg.h first will define the R1 values using GCC specific
 * techniques, which we don't want for LLVM based C compilers. Since we don't
 * actually need the real machine register definitions here, we include the
 * headers in the opposite order to allow LLVM-based C compilers to work.
 */
#include "Rts.h"
#include "Stg.h"
#endif

#include "StgRun.h"
#include "Capability.h"

#ifdef DEBUG
#include "RtsUtils.h"
#include "Printer.h"
#endif

#ifdef USE_MINIINTERPRETER

/* -----------------------------------------------------------------------------
   any architecture (using miniinterpreter)
   -------------------------------------------------------------------------- */

StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg STG_UNUSED)
{
    while (f) {
        IF_DEBUG(interpreter,
                 debugBelch("Jumping to ");
                 printPtr((P_)f); fflush(stdout);
                 debugBelch("\n");
                 );
        f = (StgFunPtr) (f)();
    }
    return (StgRegTable *)R1.p;
}

StgFunPtr StgReturn(void)
{
    return 0;
}

#else /* !USE_MINIINTERPRETER */

#ifdef LEADING_UNDERSCORE
#define STG_RUN "_StgRun"
#define STG_RETURN "_StgReturn"
#else
#define STG_RUN "StgRun"
#define STG_RETURN "StgReturn"
#endif

#if defined(mingw32_HOST_OS)
// On windows the stack has to be allocated 4k at a time, otherwise
// we get a segfault.  The C compiler knows how to do this (it calls
// _alloca()), so we make sure that we can allocate as much stack as
// we need:
StgWord8 *win32AllocStack(void)
{
    StgWord8 stack[RESERVED_C_STACK_BYTES + 16 + 12];
    return stack;
}
#endif

/* -----------------------------------------------------------------------------
   x86 architecture
   -------------------------------------------------------------------------- */

#ifdef i386_HOST_ARCH

#ifdef darwin_HOST_OS
#define STG_GLOBAL ".globl "
#else
#define STG_GLOBAL ".global "
#endif

/*
 * Note [Stack Alignment on X86]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * On X86 (both 32bit and 64bit) we keep the stack aligned on function calls at
 * a 16-byte boundary. This is done because on a number of architectures the
 * ABI requires this (x64, Mac OSX 32bit/64bit) as well as interfacing with
 * other libraries through the FFI.
 *
 * As part of this arrangment we must maintain the stack at a 16-byte boundary
 * - word_size-bytes (so 16n - 4 for i386 and 16n - 8 for x64) on entry to a
 * procedure since both GCC and LLVM expect this. This is because the stack
 * should have been 16-byte boundary aligned and then a call made which pushes
 * a return address onto the stack (so word_size more space used). In STG code
 * we only jump to other STG procedures, so we maintain the 16n - word_size
 * alignment for these jumps.
 *
 * This gives us binary compatability with LLVM and GCC as well as dealing
 * with the FFI. Previously we just maintianed a 16n byte alignment for
 * procedure entry and calls, which led to bugs (see #4211 and #5250).
 *
 * To change this convention you need to change the code here, and in
 * compiler/nativeGen/X86/CodeGen.hs::GenCCall, and maybe the adjustor
 * code for thunks in rts/AdjustorAsm.s, rts/Adjustor.c.
 *
 * A quick way to see if this is wrong is to compile this code:
 *
 *    main = System.Exit.exitWith ExitSuccess
 *
 * And run it with +RTS -sstderr.  The stats code in the RTS, in
 * particular statsPrintf(), relies on the stack alignment because
 * it saves the %xmm regs on the stack, so it'll fall over if the
 * stack isn't aligned, and calling exitWith from Haskell invokes
 * shutdownHaskellAndExit using a C call.
 *
 */

static void GNUC3_ATTRIBUTE(used)
StgRunIsImplementedInAssembler(void)
{
    __asm__ volatile (
        STG_GLOBAL STG_RUN "\n"
        STG_RUN ":\n\t"

        /*
         * move %esp down to reserve an area for temporary storage
         * during the execution of STG code.
         *
         * The stack pointer has to be aligned to a multiple of 16
         * bytes from here - this is a requirement of the C ABI, so
         * that C code can assign SSE2 registers directly to/from
         * stack locations.
         */
        "subl %0, %%esp\n\t"

        /*
         * save callee-saves registers on behalf of the STG code.
         */
        "movl %%esp, %%eax\n\t"
        "addl %0-16, %%eax\n\t"
        "movl %%ebx,0(%%eax)\n\t"
        "movl %%esi,4(%%eax)\n\t"
        "movl %%edi,8(%%eax)\n\t"
        "movl %%ebp,12(%%eax)\n\t"
        /*
         * Set BaseReg
         */
        "movl 24(%%eax),%%ebx\n\t"
        /*
         * grab the function argument from the stack
         */
        "movl 20(%%eax),%%eax\n\t"
        /*
         * jump to it
         */
        "jmp *%%eax\n\t"

        STG_GLOBAL STG_RETURN "\n"
        STG_RETURN ":\n\t"

        "movl %%esi, %%eax\n\t"   /* Return value in R1  */

        /*
         * restore callee-saves registers.  (Don't stomp on %%eax!)
         */
        "movl %%esp, %%edx\n\t"
        "addl %0-16, %%edx\n\t"
        "movl 0(%%edx),%%ebx\n\t"       /* restore the registers saved above */
        "movl 4(%%edx),%%esi\n\t"
        "movl 8(%%edx),%%edi\n\t"
        "movl 12(%%edx),%%ebp\n\t"

        "addl %0, %%esp\n\t"
        "ret"

      : : "i" (RESERVED_C_STACK_BYTES + 16)
        // + 16 to make room for the 4 registers we have to save
        // See Note [Stack Alignment on X86]
    );
}

#endif

/* ----------------------------------------------------------------------------
   x86-64 is almost the same as plain x86.

   I've done it using entirely inline assembler, because I couldn't
   get gcc to generate the correct subtraction from %rsp by using
   the local array variable trick.  It didn't seem to reserve
   enough space.  Oh well, it's not much harder this way.
   ------------------------------------------------------------------------- */

#ifdef x86_64_HOST_ARCH

extern StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg);

static void GNUC3_ATTRIBUTE(used)
StgRunIsImplementedInAssembler(void)
{
    __asm__ volatile (
        /*
         * save callee-saves registers on behalf of the STG code.
         */
        ".globl " STG_RUN "\n"
        STG_RUN ":\n\t"
        "subq %1, %%rsp\n\t"
        "movq %%rsp, %%rax\n\t"
        "subq %0, %%rsp\n\t"
        "movq %%rbx,0(%%rax)\n\t"
        "movq %%rbp,8(%%rax)\n\t"
        "movq %%r12,16(%%rax)\n\t"
        "movq %%r13,24(%%rax)\n\t"
        "movq %%r14,32(%%rax)\n\t"
        "movq %%r15,40(%%rax)\n\t"
#if defined(mingw32_HOST_OS)
        "movq %%rdi,48(%%rax)\n\t"
        "movq %%rsi,56(%%rax)\n\t"
        "movq %%xmm6,64(%%rax)\n\t"
#endif
        /*
         * Set BaseReg
         */
#if defined(mingw32_HOST_OS)
        "movq %%rdx,%%r13\n\t"
#else
        "movq %%rsi,%%r13\n\t"
#endif
        /*
         * grab the function argument from the stack, and jump to it.
         */
#if defined(mingw32_HOST_OS)
        "movq %%rcx,%%rax\n\t"
#else
        "movq %%rdi,%%rax\n\t"
#endif
        "jmp *%%rax\n\t"

        ".globl " STG_RETURN "\n"
         STG_RETURN ":\n\t"

        "movq %%rbx, %%rax\n\t"   /* Return value in R1  */

        /*
         * restore callee-saves registers.  (Don't stomp on %%rax!)
         */
        "addq %0, %%rsp\n\t"
        "movq %%rsp, %%rdx\n\t"
        "addq %1, %%rsp\n\t"
        "movq 0(%%rdx),%%rbx\n\t"       /* restore the registers saved above */
        "movq 8(%%rdx),%%rbp\n\t"
        "movq 16(%%rdx),%%r12\n\t"
        "movq 24(%%rdx),%%r13\n\t"
        "movq 32(%%rdx),%%r14\n\t"
        "movq 40(%%rdx),%%r15\n\t"
#if defined(mingw32_HOST_OS)
        "movq 48(%%rdx),%%rdi\n\t"
        "movq 56(%%rdx),%%rsi\n\t"
        "movq 64(%%rdx),%%xmm6\n\t"
#endif
        "retq"

        :
        : "i"(RESERVED_C_STACK_BYTES),
#if defined(mingw32_HOST_OS)
          "i"(80 /*stack frame size; 8 too large to make the alignment right*/)
#else
          "i"(48 /*stack frame size*/)
#endif
        );
        /*
         * See Note [Stack Alignment on X86]
         */
}

#endif /* x86-64 */

/* -----------------------------------------------------------------------------
   Sparc architecture

   --
   OLD COMMENT from GHC-3.02:

   We want tailjumps to be calls, because `call xxx' is the only Sparc
   branch that allows an arbitrary label as a target.  (Gcc's ``goto
   *target'' construct ends up loading the label into a register and
   then jumping, at the cost of two extra instructions for the 32-bit
   load.)

   When entering the threaded world, we stash our return address in a
   known location so that \tr{%i7} is available as an extra
   callee-saves register.  Of course, we have to restore this when
   coming out of the threaded world.

   I hate this god-forsaken architecture.  Since the top of the
   reserved stack space is used for globals and the bottom is reserved
   for outgoing arguments, we have to stick our return address
   somewhere in the middle.  Currently, I'm allowing 100 extra
   outgoing arguments beyond the first 6.  --JSM

   Updated info (GHC 4.06): we don't appear to use %i7 any more, so
   I'm not sure whether we still need to save it.  Incedentally, what
   does the last paragraph above mean when it says "the top of the
   stack is used for globals"?  What globals?  --SDM

   Updated info (GHC 4.08.2): not saving %i7 any more (see below).
   -------------------------------------------------------------------------- */

#ifdef sparc_HOST_ARCH

StgRegTable *
StgRun(StgFunPtr f, StgRegTable *basereg) {

    unsigned char space[RESERVED_C_STACK_BYTES];
#if 0
    register void *i7 __asm__("%i7");
    ((void **)(space))[100] = i7;
#endif
    f();
    __asm__ volatile (
                 ".align 4\n"
                 ".global " STG_RETURN "\n"
                 STG_RETURN ":"
                 : : "p" (space) : "l0","l1","l2","l3","l4","l5","l6","l7");
    /* we tell the C compiler that l0-l7 are clobbered on return to
     * StgReturn, otherwise it tries to use these to save eg. the
     * address of space[100] across the call.  The correct thing
     * to do would be to save all the callee-saves regs, but we
     * can't be bothered to do that.
     *
     * We also explicitly mark space as used since gcc eliminates it
     * otherwise.
     *
     * The code that gcc generates for this little fragment is now
     * terrible.  We could do much better by coding it directly in
     * assembler.
     */
#if 0
    /* updated 4.08.2: we don't save %i7 in the middle of the reserved
     * space any more, since gcc tries to save its address across the
     * call to f(), this gets clobbered in STG land and we end up
     * dereferencing a bogus pointer in StgReturn.
     */
    __asm__ volatile ("ld %1,%0"
                                : "=r" (i7) : "m" (((void **)(space))[100]));
#endif
    return (StgRegTable *)R1.i;
}

#endif

/* -----------------------------------------------------------------------------
   PowerPC architecture

   Everything is in assembler, so we don't have to deal with GCC...
   -------------------------------------------------------------------------- */

#ifdef powerpc_HOST_ARCH

extern StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg);

#ifdef darwin_HOST_OS
void StgRunIsImplementedInAssembler(void)
{
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
            // if the toolchain supports deadstripping, we have to
            // prevent it here (it tends to get confused here).
        __asm__ volatile (".no_dead_strip _StgRunIsImplementedInAssembler");
#endif
        __asm__ volatile (
                "\n.globl _StgRun\n"
                "_StgRun:\n"
                "\tmflr r0\n"
                "\tbl saveFP # f14\n"
                "\tstmw r13,-220(r1)\n"
                "\tstwu r1,-%0(r1)\n"
                "\tmr r27,r4\n" // BaseReg == r27
                "\tmtctr r3\n"
                "\tmr r12,r3\n"
                "\tbctr\n"
                ".globl _StgReturn\n"
                "_StgReturn:\n"
                "\tmr r3,r14\n"
                "\tla r1,%0(r1)\n"
                "\tlmw r13,-220(r1)\n"
                "\tb restFP # f14\n"
        : : "i"(RESERVED_C_STACK_BYTES+224 /*stack frame size*/));
}
#else

// This version is for PowerPC Linux.

// Differences from the Darwin/Mac OS X version:
// *) Different Assembler Syntax
// *) Doesn't use Register Saving Helper Functions (although they exist somewhere)
// *) We may not access positive stack offsets
//    (no "Red Zone" as in the Darwin ABI)
// *) The Link Register is saved to a different offset in the caller's stack frame
//    (Linux: 4(r1), Darwin 8(r1))

static void GNUC3_ATTRIBUTE(used)
StgRunIsImplementedInAssembler(void)
{
        __asm__ volatile (
                "\t.globl StgRun\n"
                "\t.type StgRun,@function\n"
                "StgRun:\n"
                "\tmflr 0\n"
                "\tstw 0,4(1)\n"
                "\tmr 5,1\n"
                "\tstwu 1,-%0(1)\n"
                "\tstmw 13,-220(5)\n"
                "\tstfd 14,-144(5)\n"
                "\tstfd 15,-136(5)\n"
                "\tstfd 16,-128(5)\n"
                "\tstfd 17,-120(5)\n"
                "\tstfd 18,-112(5)\n"
                "\tstfd 19,-104(5)\n"
                "\tstfd 20,-96(5)\n"
                "\tstfd 21,-88(5)\n"
                "\tstfd 22,-80(5)\n"
                "\tstfd 23,-72(5)\n"
                "\tstfd 24,-64(5)\n"
                "\tstfd 25,-56(5)\n"
                "\tstfd 26,-48(5)\n"
                "\tstfd 27,-40(5)\n"
                "\tstfd 28,-32(5)\n"
                "\tstfd 29,-24(5)\n"
                "\tstfd 30,-16(5)\n"
                "\tstfd 31,-8(5)\n"
                "\tmr 27,4\n"  // BaseReg == r27
                "\tmtctr 3\n"
                "\tmr 12,3\n"
                "\tbctr\n"
                ".globl StgReturn\n"
                "\t.type StgReturn,@function\n"
                "StgReturn:\n"
                "\tmr 3,14\n"
                "\tla 5,%0(1)\n"
                "\tlmw 13,-220(5)\n"
                "\tlfd 14,-144(5)\n"
                "\tlfd 15,-136(5)\n"
                "\tlfd 16,-128(5)\n"
                "\tlfd 17,-120(5)\n"
                "\tlfd 18,-112(5)\n"
                "\tlfd 19,-104(5)\n"
                "\tlfd 20,-96(5)\n"
                "\tlfd 21,-88(5)\n"
                "\tlfd 22,-80(5)\n"
                "\tlfd 23,-72(5)\n"
                "\tlfd 24,-64(5)\n"
                "\tlfd 25,-56(5)\n"
                "\tlfd 26,-48(5)\n"
                "\tlfd 27,-40(5)\n"
                "\tlfd 28,-32(5)\n"
                "\tlfd 29,-24(5)\n"
                "\tlfd 30,-16(5)\n"
                "\tlfd 31,-8(5)\n"
                "\tmr 1,5\n"
                "\tlwz 0,4(1)\n"
                "\tmtlr 0\n"
                "\tblr\n"
        : : "i"(RESERVED_C_STACK_BYTES+224 /*stack frame size*/));
}
#endif

#endif

/* -----------------------------------------------------------------------------
   PowerPC 64 architecture

   Everything is in assembler, so we don't have to deal with GCC...
   -------------------------------------------------------------------------- */

#ifdef powerpc64_HOST_ARCH

#ifdef linux_HOST_OS
extern StgRegTable * StgRun(StgFunPtr f, StgRegTable *basereg);

static void GNUC3_ATTRIBUTE(used)
StgRunIsImplementedInAssembler(void)
{
        // r0 volatile
        // r1 stack pointer
        // r2 toc - needs to be saved
        // r3-r10 argument passing, volatile
        // r11, r12 very volatile (not saved across cross-module calls)
        // r13 thread local state (never modified, don't need to save)
        // r14-r31 callee-save
        __asm__ volatile (
                ".section \".opd\",\"aw\"\n"
                ".align 3\n"
                ".globl StgRun\n"
                "StgRun:\n"
                "\t.quad\t.StgRun,.TOC.@tocbase,0\n"
                "\t.size StgRun,24\n"
                ".globl StgReturn\n"
                "StgReturn:\n"
                "\t.quad\t.StgReturn,.TOC.@tocbase,0\n"
                "\t.size StgReturn,24\n"
                ".previous\n"
                ".globl .StgRun\n"
                ".type .StgRun,@function\n"
                ".StgRun:\n"
                "\tmflr 0\n"
                "\tmr 5, 1\n"
                "\tstd 0, 16(1)\n"
                "\tstdu 1, -%0(1)\n"
                "\tstd 2, -296(5)\n"
                "\tstd 14, -288(5)\n"
                "\tstd 15, -280(5)\n"
                "\tstd 16, -272(5)\n"
                "\tstd 17, -264(5)\n"
                "\tstd 18, -256(5)\n"
                "\tstd 19, -248(5)\n"
                "\tstd 20, -240(5)\n"
                "\tstd 21, -232(5)\n"
                "\tstd 22, -224(5)\n"
                "\tstd 23, -216(5)\n"
                "\tstd 24, -208(5)\n"
                "\tstd 25, -200(5)\n"
                "\tstd 26, -192(5)\n"
                "\tstd 27, -184(5)\n"
                "\tstd 28, -176(5)\n"
                "\tstd 29, -168(5)\n"
                "\tstd 30, -160(5)\n"
                "\tstd 31, -152(5)\n"
                "\tstfd 14, -144(5)\n"
                "\tstfd 15, -136(5)\n"
                "\tstfd 16, -128(5)\n"
                "\tstfd 17, -120(5)\n"
                "\tstfd 18, -112(5)\n"
                "\tstfd 19, -104(5)\n"
                "\tstfd 20, -96(5)\n"
                "\tstfd 21, -88(5)\n"
                "\tstfd 22, -80(5)\n"
                "\tstfd 23, -72(5)\n"
                "\tstfd 24, -64(5)\n"
                "\tstfd 25, -56(5)\n"
                "\tstfd 26, -48(5)\n"
                "\tstfd 27, -40(5)\n"
                "\tstfd 28, -32(5)\n"
                "\tstfd 29, -24(5)\n"
                "\tstfd 30, -16(5)\n"
                "\tstfd 31, -8(5)\n"
                "\tmr 27, 4\n"  // BaseReg == r27
                "\tld 2, 8(3)\n"
                "\tld 3, 0(3)\n"
                "\tmtctr 3\n"
                "\tbctr\n"
                ".globl .StgReturn\n"
                ".type .StgReturn,@function\n"
                ".StgReturn:\n"
                "\tmr 3,14\n"
                "\tla 5, %0(1)\n" // load address == addi r5, r1, %0
                "\tld 2, -296(5)\n"
                "\tld 14, -288(5)\n"
                "\tld 15, -280(5)\n"
                "\tld 16, -272(5)\n"
                "\tld 17, -264(5)\n"
                "\tld 18, -256(5)\n"
                "\tld 19, -248(5)\n"
                "\tld 20, -240(5)\n"
                "\tld 21, -232(5)\n"
                "\tld 22, -224(5)\n"
                "\tld 23, -216(5)\n"
                "\tld 24, -208(5)\n"
                "\tld 25, -200(5)\n"
                "\tld 26, -192(5)\n"
                "\tld 27, -184(5)\n"
                "\tld 28, -176(5)\n"
                "\tld 29, -168(5)\n"
                "\tld 30, -160(5)\n"
                "\tld 31, -152(5)\n"
                "\tlfd 14, -144(5)\n"
                "\tlfd 15, -136(5)\n"
                "\tlfd 16, -128(5)\n"
                "\tlfd 17, -120(5)\n"
                "\tlfd 18, -112(5)\n"
                "\tlfd 19, -104(5)\n"
                "\tlfd 20, -96(5)\n"
                "\tlfd 21, -88(5)\n"
                "\tlfd 22, -80(5)\n"
                "\tlfd 23, -72(5)\n"
                "\tlfd 24, -64(5)\n"
                "\tlfd 25, -56(5)\n"
                "\tlfd 26, -48(5)\n"
                "\tlfd 27, -40(5)\n"
                "\tlfd 28, -32(5)\n"
                "\tlfd 29, -24(5)\n"
                "\tlfd 30, -16(5)\n"
                "\tlfd 31, -8(5)\n"
                "\tmr 1, 5\n"
                "\tld 0, 16(1)\n"
                "\tmtlr 0\n"
                "\tblr\n"
        : : "i"(RESERVED_C_STACK_BYTES+304 /*stack frame size*/));
}

#else // linux_HOST_OS
#error Only linux support for power64 right now.
#endif

#endif

/* -----------------------------------------------------------------------------
   ARM architecture
   -------------------------------------------------------------------------- */

#ifdef arm_HOST_ARCH

#if defined(__thumb__)
#define THUMB_FUNC ".thumb\n\t.thumb_func\n\t"
#else
#define THUMB_FUNC
#endif

StgRegTable *
StgRun(StgFunPtr f, StgRegTable *basereg) {
    StgRegTable * r;
    __asm__ volatile (
        /*
         * save callee-saves registers on behalf of the STG code.
         */
        "stmfd sp!, {r4-r11, ip, lr}\n\t"
#if !defined(arm_HOST_ARCH_PRE_ARMv6)
        "vstmdb sp!, {d8-d11}\n\t"
#endif
        /*
         * allocate some space for Stg machine's temporary storage.
         * Note: RESERVER_C_STACK_BYTES has to be a round number here or
         * the assembler can't assemble it.
         */
        "sub sp, sp, %3\n\t"
        /*
         * Set BaseReg
         */
        "mov r4, %2\n\t"
        /*
         * Jump to function argument.
         */
        "bx %1\n\t"

        ".globl " STG_RETURN "\n\t"
        THUMB_FUNC
        ".type " STG_RETURN ", %%function\n"
        STG_RETURN ":\n\t"
        /*
         * Free the space we allocated
         */
        "add sp, sp, %3\n\t"
        /*
         * Return the new register table, taking it from Stg's R1 (ARM's R7).
         */
        "mov %0, r7\n\t"
        /*
         * restore callee-saves registers.
         */
#if !defined(arm_HOST_ARCH_PRE_ARMv6)
        "vldmia sp!, {d8-d11}\n\t"
#endif
        "ldmfd sp!, {r4-r11, ip, lr}\n\t"
      : "=r" (r)
      : "r" (f), "r" (basereg), "i" (RESERVED_C_STACK_BYTES)
#if !defined(__thumb__)
        /* In ARM mode, r11/fp is frame-pointer and so we cannot mark
           it as clobbered. If we do so, GCC complains with error. */
      : "%r4", "%r5", "%r6", "%r7", "%r8", "%r9", "%r10", "%ip", "%lr"
#else
        /* In Thumb mode r7 is frame-pointer and so we cannot mark it
           as clobbered. On the other hand we mark as clobbered also
           those regs not used in Thumb mode. Hard to judge if this is
           needed, but certainly Haskell code is using them for
           placing GHC's virtual registers there. See
           includes/stg/MachRegs.h Please note that Haskell code is
           compiled by GHC/LLVM into ARM code (not Thumb!), at least
           as of February 2012 */
      : "%r4", "%r5", "%r6", "%r8", "%r9", "%r10", "%11", "%ip", "%lr"
#endif
    );
    return r;
}
#endif

#endif /* !USE_MINIINTERPRETER */
