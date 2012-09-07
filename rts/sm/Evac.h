/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: evacuation functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_EVAC_H
#define SM_EVAC_H

#include "BeginPrivate.h"

// Use a register argument for evacuate, if available.
// Earlier, the regparm attribute was used whenever __GNUC__ >= 2, but this
// generated warnings on PPC. So the use is restricted further.
// See http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html that says
//     regparm (number)
//         On the Intel 386, the regparm attribute causes the compiler to pass
//         arguments number one to number if they are of integral type in
//         registers EAX, EDX, and ECX instead of on the stack. Functions that
//         take a variable number of arguments will continue to be passed all of
//         their arguments on the stack.
#if __GNUC__ >= 2 && (defined(x86_64_HOST_ARCH) || defined(i386_HOST_ARCH))
#define REGPARM1 __attribute__((regparm(1)))
#else
#define REGPARM1
#endif

REGPARM1 void evacuate  (StgClosure **p);
REGPARM1 void evacuate1 (StgClosure **p);

extern W_ thunk_selector_depth;

#include "EndPrivate.h"

#endif /* SM_EVAC_H */

