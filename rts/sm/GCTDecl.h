/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2014
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

/* The gct variable is thread-local and points to the current thread's
   gc_thread structure. It is heavily accessed, and thus high
   performance access is crucial to parallel (-threaded) workloads.

   First, we try to use a 'global register variable' which is a GCC
   extension. This reserves the register globally.

   If that's not possible, then we need to use __thread, which is a
   compiler/OS specific TLS storage mechanism (assumed to be Fast
   Enough.)

   BUT, some older versions of OS X compilers (llvm-gcc, older Clangs)
   do not support __thread at all. Modern clang however, does - but on
   OS X it's not as fast as the Linux (which can write directly into a
   segment register - see #7602.)

   If we don't support __thread then we do the absolute worst thing:
   we just use pthread_getspecific and pthread_setspecific (which are
   horribly slow.)
*/

#define GCT_REG_DECL(type,name,reg) register type name REG(reg);


/* -------------------------------------------------------------------------- */

/* First: if we're not using the threaded RTS, it's easy: just fake it. */
#if !defined(THREADED_RTS)
extern StgWord8 the_gc_thread[];
#define gct ((gc_thread*)&the_gc_thread)
#define SET_GCT(to) /*nothing*/
#define DECLARE_GCT /*nothing*/

#else /* defined(THREADED_RTS) */

/* -------------------------------------------------------------------------- */

/* Now, llvm-gcc and some older Clang compilers do not support
   __thread. So we have to fallback to the extremely slow case,
   unfortunately. Note: clang_CC_FLAVOR implies llvm_CC_FLAVOR.

   Also, the iOS Clang compiler doesn't support __thread either for
   some bizarre reason, so there's not much we can do about that... */
#if (defined(llvm_CC_FLAVOR) && (CC_SUPPORTS_TLS == 0)) || defined(ios_HOST_OS)
#define gct ((gc_thread *)(pthread_getspecific(gctKey)))
#define SET_GCT(to) (pthread_setspecific(gctKey, to))
#define DECLARE_GCT ThreadLocalKey gctKey;

/* -------------------------------------------------------------------------- */

/* However, if we *are* using an LLVM based compiler with __thread
   support, then use that (since LLVM doesn't support global register
   variables.) */
#elif defined(llvm_CC_FLAVOR) && (CC_SUPPORTS_TLS == 1)
extern __thread gc_thread* gct;
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT __thread gc_thread* gct;

/* -------------------------------------------------------------------------- */

/* Next up: Using __thread is better than stealing a register on
   x86/Linux, because we have too few registers available. In my
   tests it was worth about 5% in GC performance, but of course that
   might change as gcc improves. -- SDM 2009/04/03 */
#elif (defined(i386_HOST_ARCH) && defined(linux_HOST_OS))
extern __thread gc_thread* gct;
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT __thread gc_thread* gct;

/* -------------------------------------------------------------------------- */

/* Next up: On SPARC we can't pin gct to a register. Names like %l1
   are just offsets into the register window, which change on each
   function call.

   There are eight global (non-window) registers, but they're used for other
   purposes:

    %g0     -- always zero
    %g1     -- volatile over function calls, used by the linker
    %g2-%g3 -- used as scratch regs by the C compiler (caller saves)
    %g4     -- volatile over function calls, used by the linker
    %g5-%g7 -- reserved by the OS
*/
#elif defined(sparc_HOST_ARCH)
extern __thread gc_thread* gct;
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT __thread gc_thread* gct;

/* -------------------------------------------------------------------------- */

/* Next up: generally, if REG_Base is defined and we're *not* using
   i386, then actually declare the needed register. The catch for i386
   here is that REG_Base is %ebx, but that is also used for -fPIC, so
   it can't be stolen */
#elif defined(REG_Base) && !defined(i386_HOST_ARCH)
GCT_REG_DECL(gc_thread*, gct, REG_Base);
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT /* nothing */

/* -------------------------------------------------------------------------- */

/* Next up: if REG_R1 is available after checking REG_Base, we're
   gonna steal it in every case we can. */
#elif defined(REG_R1)
GCT_REG_DECL(gc_thread*, gct, REG_R1);
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT /* nothing */

/* -------------------------------------------------------------------------- */

/* Finally, as an absolute fallback, if none of the above tests check
   out but we *do* have __thread support, then use that. */
#elif CC_SUPPORTS_TLS == 1
extern __thread gc_thread* gct;
#define SET_GCT(to) gct = (to)
#define DECLARE_GCT __thread gc_thread* gct;

/* -------------------------------------------------------------------------- */

/* Impossible! */
#else
#error Cannot find a way to declare the thread-local gc variable!
#endif

#endif // THREADED_RTS

#include "EndPrivate.h"

#endif // SM_GCTDECL_H
