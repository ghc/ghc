/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Stuff for implementing proper tail jumps.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef TAILCALLS_H
#define TAILCALLS_H

/* -----------------------------------------------------------------------------
   Unmangled tail-jumping: use the mini interpretter.
   -------------------------------------------------------------------------- */

#ifdef USE_MINIINTERPRETER

#define JMP_(cont) return((StgFunPtr)(cont))
#define FB_
#define FE_

#else

extern void __DISCARD__(void);

/* -----------------------------------------------------------------------------
   Tail calling on x86
   -------------------------------------------------------------------------- */

#if i386_HOST_ARCH

/* Note about discard: possibly there to fool GCC into clearing up
   before we do the jump eg. if there are some arguments left on the C
   stack that GCC hasn't popped yet.  Also possibly to fool any
   optimisations (a function call often acts as a barrier).  Not sure
   if any of this is necessary now -- SDM

   Comment to above note: I don't think the __DISCARD__() in JMP_ is 
   necessary.  Arguments should be popped from the C stack immediately
   after returning from a function, as long as we pass -fno-defer-pop
   to gcc.  Moreover, a goto to a first-class label acts as a barrier 
   for optimisations in the same way a function call does. 
   -= chak
   */

/* The goto here seems to cause gcc -O2 to delete all the code after
   it - including the FE_ marker and the epilogue code - exactly what
   we want! -- SDM
   */

#define JMP_(cont)			\
    { 					\
      void *__target;			\
      __DISCARD__();			\
      __target = (void *)(cont);    	\
      goto *__target; 	    	    	\
    }

#endif /* i386_HOST_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on x86_64
   -------------------------------------------------------------------------- */

#if x86_64_HOST_ARCH

/*
  NOTE about __DISCARD__():

  On x86_64 this is necessary to work around bugs in the register
  variable support in gcc.  Without the __DISCARD__() call, gcc will
  silently throw away assignements to global register variables that
  happen before the jump.

  Here's the example:

  extern void g(void);
  static void f(void) {
    R1 = g;
    __DISCARD__()
    goto *R1;
  }

  without the dummy function call, gcc throws away the assignment to R1
  (gcc 3.4.3) gcc bug #20359.
*/

#define JMP_(cont)			\
    { 					\
      __DISCARD__();			\
      goto *(void *)(cont); 	    	    	\
    }

#endif /* x86_64_HOST_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on Sparc
   -------------------------------------------------------------------------- */

#ifdef sparc_HOST_ARCH

#define JMP_(cont)	((F_) (cont))()
	/* Oh so happily, the above turns into a "call" instruction,
	   which, on a SPARC, is nothing but a "jmpl" with the
	   return address in %o7 [which we don't care about].
	*/

/* Don't need these for sparc mangling */
#define FB_
#define FE_

#endif /* sparc_HOST_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on PowerPC
   -------------------------------------------------------------------------- */

#ifdef powerpc_HOST_ARCH

#define JMP_(cont)			\
    { 					\
      void *target;			\
      target = (void *)(cont);    	\
      __DISCARD__();			\
      goto *target; 	    	    	\
    }

/*
	The __DISCARD__ is there because Apple's April 2002 Beta of GCC 3.1
	sometimes generates incorrect code otherwise.
	It tends to "forget" to update global register variables in the presence
	of decrement/increment operators:
	JMP_(*(--Sp)) is wrongly compiled as JMP_(Sp[-1]).
	Calling __DISCARD__ in between works around this problem.
*/

/*
	I would _love_ to use the following instead,
	but some versions of Apple's GCC fail to generate code for it
	if it is called for a casted data pointer - which is exactly what
	we are going to do...

	#define JMP_(cont)	((F_) (cont))()
*/

#endif /* powerpc_HOST_ARCH */

#ifdef powerpc64_HOST_ARCH
#define JMP_(cont) ((F_) (cont))()
#endif

/* -----------------------------------------------------------------------------
  FUNBEGIN and FUNEND.

  These are markers indicating the start and end of Real Code in a
  function.  All instructions between the actual start and end of the
  function and these markers is shredded by the mangler.
  -------------------------------------------------------------------------- */

/*  The following __DISCARD__() has become necessary with gcc 2.96 on x86.
 *  It prevents gcc from moving stack manipulation code from the function
 *  body (aka the Real Code) into the function prologue, ie, from moving it
 *  over the --- BEGIN --- marker.  It should be noted that (like some
 *  other black magic in GHC's code), there is no essential reason why gcc
 *  could not move some stack manipulation code across the __DISCARD__() -
 *  it just doesn't choose to do it at the moment.
 *  -= chak
 */
 
#ifndef FB_
#define FB_    __asm__ volatile ("--- BEGIN ---"); __DISCARD__ ();
#endif

#ifndef FE_
#define FE_    __asm__ volatile ("--- END ---");
#endif

#endif /* !USE_MINIINTERPRETER */

#endif /* TAILCALLS_H */
