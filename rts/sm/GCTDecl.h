/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2009
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_GCTDECL_H
#define SM_GCTDECL_H

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
   The gct variable is thread-local and points to the current thread's
   gc_thread structure.  It is heavily accessed, so we try to put gct
   into a global register variable if possible; if we don't have a
   register then use gcc's __thread extension to create a thread-local
   variable.
   -------------------------------------------------------------------------- */

#if defined(THREADED_RTS)

#define GLOBAL_REG_DECL(type,name,reg) register type name REG(reg);

#ifdef llvm_CC_FLAVOR
#define SET_GCT(to) (pthread_setspecific(gctKey, to))
#else
#define SET_GCT(to) gct = (to)
#endif



#if (defined(i386_HOST_ARCH) && defined(linux_HOST_OS))
// Using __thread is better than stealing a register on x86/Linux, because
// we have too few registers available.  In my tests it was worth
// about 5% in GC performance, but of course that might change as gcc
// improves. -- SDM 2009/04/03
//
// For MacOSX, we can use an llvm-based C compiler which will store the gct
// in a thread local variable using pthreads.

extern __thread gc_thread* gct;
#define DECLARE_GCT __thread gc_thread* gct;

#elif defined(llvm_CC_FLAVOR)
// LLVM does not support the __thread extension and will generate
// incorrect code for global register variables. If we are compiling
// with a C compiler that uses an LLVM back end (clang or llvm-gcc) then we
// use pthread_getspecific() to handle the thread local storage for gct.
#define gct ((gc_thread *)(pthread_getspecific(gctKey)))
#define DECLARE_GCT ThreadLocalKey gctKey;

#elif defined(sparc_HOST_ARCH)
// On SPARC we can't pin gct to a register. Names like %l1 are just offsets
//	into the register window, which change on each function call.
//	
//	There are eight global (non-window) registers, but they're used for other purposes.
//	%g0     -- always zero
//	%g1     -- volatile over function calls, used by the linker
//	%g2-%g3 -- used as scratch regs by the C compiler (caller saves)
//	%g4	-- volatile over function calls, used by the linker
//	%g5-%g7	-- reserved by the OS

extern __thread gc_thread* gct;
#define DECLARE_GCT __thread gc_thread* gct;


#elif defined(REG_Base) && !defined(i386_HOST_ARCH)
// on i386, REG_Base is %ebx which is also used for PIC, so we don't
// want to steal it

GLOBAL_REG_DECL(gc_thread*, gct, REG_Base)
#define DECLARE_GCT /* nothing */


#elif defined(REG_R1)

GLOBAL_REG_DECL(gc_thread*, gct, REG_R1)
#define DECLARE_GCT /* nothing */


#elif defined(__GNUC__)

extern __thread gc_thread* gct;
#define DECLARE_GCT __thread gc_thread* gct;

#else

#error Cannot find a way to declare the thread-local gct

#endif

#else  // not the threaded RTS

extern StgWord8 the_gc_thread[];

#define gct ((gc_thread*)&the_gc_thread)
#define SET_GCT(to) /*nothing*/
#define DECLARE_GCT /*nothing*/

#endif // THREADED_RTS

#include "EndPrivate.h"

#endif // SM_GCTDECL_H
