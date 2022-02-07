/* -----------------------------------------------------------------------------
 * AMD64 architecture adjustor thunk logic.
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
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
   "ret"
  );
}

extern void obscure_ccall_ret_code(void);

void initAdjustors() { }

void*
createAdjustor(int cconv, StgStablePtr hptr,
               StgFunPtr wptr,
               char *typeString
    )
{
    switch (cconv)
    {
    case 1: /* _ccall */
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
            if (page == NULL) {
                barf("createAdjustor: failed to allocate executable page\n");
            }
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
            if (page == NULL) {
                barf("createAdjustor: failed to allocate executable page\n");
            }
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

    default:
        barf("createAdjustor: Unsupported calling convention");
        break;
    }
}

void freeHaskellFunctionPtr(void* ptr)
{
    if ( *(StgWord16 *)ptr == 0x894d ) {
        freeStablePtr(*(StgStablePtr*)((StgWord8*)ptr+0x20));
    } else if ( *(StgWord16 *)ptr == 0x5141 ) {
        freeStablePtr(*(StgStablePtr*)((StgWord8*)ptr+0x30));
    } else {
        errorBelch("freeHaskellFunctionPtr: not for me, guv! %p\n", ptr);
        return;
    }

    freeExecPage((ExecPage *) ptr);
}
