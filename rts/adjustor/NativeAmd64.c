/* -----------------------------------------------------------------------------
 * AMD64 architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "StablePtr.h"

#if defined(LEADING_UNDERSCORE)
#define UNDERSCORE "_"
#else
#define UNDERSCORE ""
#endif

/*
  Now here's something obscure for you:

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
static void GNUC3_ATTRIBUTE(used) obscure_ccall_wrapper(void)
{
  __asm__ (
   ".globl " UNDERSCORE "obscure_ccall_ret_code\n"
   UNDERSCORE "obscure_ccall_ret_code:\n\t"
   "addq $0x8, %rsp\n\t"
#if defined(mingw32_HOST_OS)
   /* On Win64, we had to put the original return address after the
      arg 1-4 spill slots, ro now we have to move it back */
   "movq 0x20(%rsp), %rcx\n"
   "movq %rcx, (%rsp)\n"
#endif /* defined(mingw32_HOST_OS) */
   "ret"
  );
}

extern void obscure_ccall_ret_code(void);

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString
    )
{
    switch (cconv)
    {
    case 1: /* _ccall */
#if defined(mingw32_HOST_OS)
    /*
      stack at call:
               argn
               ...
               arg5
               return address
               %rcx,%rdx,%r8,%r9 = arg1..arg4

      if there are <4 integer args, then we can just push the
      StablePtr into %rcx and shuffle the other args up.

      If there are >=4 integer args, then we have to flush one arg
      to the stack, and arrange to adjust the stack ptr on return.
      The stack will be rearranged to this:

             argn
             ...
             arg5
             return address  *** <-- dummy arg in stub fn.
             arg4
             obscure_ccall_ret_code

      This unfortunately means that the type of the stub function
      must have a dummy argument for the original return address
      pointer inserted just after the 4th integer argument.

      Code for the simple case:

   0:   4d 89 c1                mov    %r8,%r9
   3:   49 89 d0                mov    %rdx,%r8
   6:   48 89 ca                mov    %rcx,%rdx
   9:   f2 0f 10 da             movsd  %xmm2,%xmm3
   d:   f2 0f 10 d1             movsd  %xmm1,%xmm2
  11:   f2 0f 10 c8             movsd  %xmm0,%xmm1
  15:   48 8b 0d 0c 00 00 00    mov    0xc(%rip),%rcx    # 28 <.text+0x28>
  1c:   ff 25 0e 00 00 00       jmpq   *0xe(%rip)        # 30 <.text+0x30>
  22:   90                      nop
  [...]


  And the version for >=4 integer arguments:

[we want to push the 4th argument (either %r9 or %xmm3, depending on
 whether it is a floating arg or not) and the return address onto the
 stack. However, slots 1-4 are reserved for code we call to spill its
 args 1-4 into, so we can't just push them onto the bottom of the stack.
 So first put the 4th argument onto the stack, above what will be the
 spill slots.]
   0:   48 83 ec 08             sub    $0x8,%rsp
[if non-floating arg, then do this:]
   4:   90                      nop
   5:   4c 89 4c 24 20          mov    %r9,0x20(%rsp)
[else if floating arg then do this:]
   4:   f2 0f 11 5c 24 20       movsd  %xmm3,0x20(%rsp)
[end if]
[Now push the new return address onto the stack]
   a:   ff 35 30 00 00 00       pushq  0x30(%rip)        # 40 <.text+0x40>
[But the old return address has been moved up into a spill slot, so
 we need to move it above them]
  10:   4c 8b 4c 24 10          mov    0x10(%rsp),%r9
  15:   4c 89 4c 24 30          mov    %r9,0x30(%rsp)
[Now we do the normal register shuffle-up etc]
  1a:   4d 89 c1                mov    %r8,%r9
  1d:   49 89 d0                mov    %rdx,%r8
  20:   48 89 ca                mov    %rcx,%rdx
  23:   f2 0f 10 da             movsd  %xmm2,%xmm3
  27:   f2 0f 10 d1             movsd  %xmm1,%xmm2
  2b:   f2 0f 10 c8             movsd  %xmm0,%xmm1
  2f:   48 8b 0d 12 00 00 00    mov    0x12(%rip),%rcx        # 48 <.text+0x48>
  36:   ff 25 14 00 00 00       jmpq   *0x14(%rip)        # 50 <.text+0x50>
  3c:   90                      nop
  3d:   90                      nop
  3e:   90                      nop
  3f:   90                      nop
  [...]

    */
    {
        // determine whether we have 4 or more integer arguments,
        // and therefore need to flush one to the stack.
        if ((typeString[0] == '\0') ||
            (typeString[1] == '\0') ||
            (typeString[2] == '\0') ||
            (typeString[3] == '\0')) {

            ExecPage *page = allocateExecPage();
            StgWord8 *adj_code = (StgWord8*) page;

            *(StgInt32 *)adj_code        = 0x49c1894d;
            *(StgInt32 *)(adj_code+0x4)  = 0x8948d089;
            *(StgInt32 *)(adj_code+0x8)  = 0x100ff2ca;
            *(StgInt32 *)(adj_code+0xc)  = 0x100ff2da;
            *(StgInt32 *)(adj_code+0x10) = 0x100ff2d1;
            *(StgInt32 *)(adj_code+0x14) = 0x0d8b48c8;
            *(StgInt32 *)(adj_code+0x18) = 0x0000000c;

            *(StgInt32 *)(adj_code+0x1c) = 0x000e25ff;
            *(StgInt32 *)(adj_code+0x20) = 0x00000000;
            *(StgInt64 *)(adj_code+0x28) = (StgInt64)hptr;
            *(StgInt64 *)(adj_code+0x30) = (StgInt64)wptr;

            freezeExecPage(page);
            return page;
        }
        else
        {
            bool fourthFloating = (typeString[3] == 'f' || typeString[3] == 'd');
            ExecPage *page = allocateExecPage();
            StgWord8 *adj_code = (StgWord8*) page;

            *(StgInt32 *)adj_code        = 0x08ec8348;
            *(StgInt32 *)(adj_code+0x4)  = fourthFloating ? 0x5c110ff2
                                                          : 0x4c894c90;
            *(StgInt32 *)(adj_code+0x8)  = 0x35ff2024;
            *(StgInt32 *)(adj_code+0xc)  = 0x00000030;
            *(StgInt32 *)(adj_code+0x10) = 0x244c8b4c;
            *(StgInt32 *)(adj_code+0x14) = 0x4c894c10;
            *(StgInt32 *)(adj_code+0x18) = 0x894d3024;
            *(StgInt32 *)(adj_code+0x1c) = 0xd08949c1;
            *(StgInt32 *)(adj_code+0x20) = 0xf2ca8948;
            *(StgInt32 *)(adj_code+0x24) = 0xf2da100f;
            *(StgInt32 *)(adj_code+0x28) = 0xf2d1100f;
            *(StgInt32 *)(adj_code+0x2c) = 0x48c8100f;
            *(StgInt32 *)(adj_code+0x30) = 0x00120d8b;
            *(StgInt32 *)(adj_code+0x34) = 0x25ff0000;
            *(StgInt32 *)(adj_code+0x38) = 0x00000014;
            *(StgInt32 *)(adj_code+0x3c) = 0x90909090;
            *(StgInt64 *)(adj_code+0x40) = (StgInt64)obscure_ccall_ret_code;
            *(StgInt64 *)(adj_code+0x48) = (StgInt64)hptr;
            *(StgInt64 *)(adj_code+0x50) = (StgInt64)wptr;

            freezeExecPage(page);
            return page;
        }
    }

# else
    /*
      stack at call:
               argn
               ...
               arg7
               return address
               %rdi,%rsi,%rdx,%rcx,%r8,%r9 = arg1..arg6

      if there are <6 integer args, then we can just push the
      StablePtr into %edi and shuffle the other args up.

      If there are >=6 integer args, then we have to flush one arg
      to the stack, and arrange to adjust the stack ptr on return.
      The stack will be rearranged to this:

             argn
             ...
             arg7
             return address  *** <-- dummy arg in stub fn.
             arg6
             obscure_ccall_ret_code

      This unfortunately means that the type of the stub function
      must have a dummy argument for the original return address
      pointer inserted just after the 6th integer argument.

      Code for the simple case:

   0:   4d 89 c1                mov    %r8,%r9
   3:   49 89 c8                mov    %rcx,%r8
   6:   48 89 d1                mov    %rdx,%rcx
   9:   48 89 f2                mov    %rsi,%rdx
   c:   48 89 fe                mov    %rdi,%rsi
   f:   48 8b 3d 0a 00 00 00    mov    10(%rip),%rdi
  16:   ff 25 0c 00 00 00       jmpq   *12(%rip)
  ...
  20: .quad 0  # aligned on 8-byte boundary
  28: .quad 0  # aligned on 8-byte boundary


  And the version for >=6 integer arguments:

   0:   41 51                   push   %r9
   2:   ff 35 20 00 00 00       pushq  32(%rip)        # 28 <ccall_adjustor+0x28>
   8:   4d 89 c1                mov    %r8,%r9
   b:   49 89 c8                mov    %rcx,%r8
   e:   48 89 d1                mov    %rdx,%rcx
  11:   48 89 f2                mov    %rsi,%rdx
  14:   48 89 fe                mov    %rdi,%rsi
  17:   48 8b 3d 12 00 00 00    mov    18(%rip),%rdi        # 30 <ccall_adjustor+0x30>
  1e:   ff 25 14 00 00 00       jmpq   *20(%rip)        # 38 <ccall_adjustor+0x38>
  ...
  28: .quad 0  # aligned on 8-byte boundary
  30: .quad 0  # aligned on 8-byte boundary
  38: .quad 0  # aligned on 8-byte boundary
    */

    {
        int i = 0;
        char *c;

        // determine whether we have 6 or more integer arguments,
        // and therefore need to flush one to the stack.
        for (c = typeString; *c != '\0'; c++) {
            if (*c != 'f' && *c != 'd') i++;
            if (i == 6) break;
        }

        if (i < 6) {
            ExecPage *page = allocateExecPage();
            StgWord8 *adj_code = (StgWord8*) page;

            *(StgInt32 *)adj_code        = 0x49c1894d;
            *(StgInt32 *)(adj_code+0x4)  = 0x8948c889;
            *(StgInt32 *)(adj_code+0x8)  = 0xf28948d1;
            *(StgInt32 *)(adj_code+0xc)  = 0x48fe8948;
            *(StgInt32 *)(adj_code+0x10) = 0x000a3d8b;
            *(StgInt32 *)(adj_code+0x14) = 0x25ff0000;
            *(StgInt32 *)(adj_code+0x18) = 0x0000000c;
            *(StgInt64 *)(adj_code+0x20) = (StgInt64)hptr;
            *(StgInt64 *)(adj_code+0x28) = (StgInt64)wptr;

            freezeExecPage(page);
            return page;
        }
        else
        {
            ExecPage *page = allocateExecPage();
            StgWord8 *adj_code = (StgWord8*) page;

            *(StgInt32 *)adj_code        = 0x35ff5141;
            *(StgInt32 *)(adj_code+0x4)  = 0x00000020;
            *(StgInt32 *)(adj_code+0x8)  = 0x49c1894d;
            *(StgInt32 *)(adj_code+0xc)  = 0x8948c889;
            *(StgInt32 *)(adj_code+0x10) = 0xf28948d1;
            *(StgInt32 *)(adj_code+0x14) = 0x48fe8948;
            *(StgInt32 *)(adj_code+0x18) = 0x00123d8b;
            *(StgInt32 *)(adj_code+0x1c) = 0x25ff0000;
            *(StgInt32 *)(adj_code+0x20) = 0x00000014;

            *(StgInt64 *)(adj_code+0x28) = (StgInt64)obscure_ccall_ret_code;
            *(StgInt64 *)(adj_code+0x30) = (StgInt64)hptr;
            *(StgInt64 *)(adj_code+0x38) = (StgInt64)wptr;

            freezeExecPage(page);
            return page;
        }
    }
#endif /* defined(mingw32_HOST_OS) */
        break;

    default:
        barf("createAdjustor: Unsupported calling convention: %d", cconv);
        break;
    }
}

void freeHaskellFunctionPtr(void* ptr)
{
    if ( *(StgWord16 *)ptr == 0x894d ) {
        freeStablePtr(*(StgStablePtr*)((StgWord8*)ptr+
#if defined(mingw32_HOST_OS)
                                       0x28
#else
                                       0x20
#endif
                          ));
#if !defined(mingw32_HOST_OS)
    } else if ( *(StgWord16 *)ptr == 0x5141 ) {
        freeStablePtr(*(StgStablePtr*)((StgWord8*)ptr+0x30));
#endif
#if defined(mingw32_HOST_OS)
    } else if ( *(StgWord16 *)ptr == 0x8348 ) {
        freeStablePtr(*(StgStablePtr*)((StgWord8*)ptr+0x48));
#endif
    } else {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }

    freeExecPage((ExecPage *) ptr);
}
