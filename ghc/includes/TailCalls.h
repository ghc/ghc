/* -----------------------------------------------------------------------------
 * $Id: TailCalls.h,v 1.6 2000/11/07 10:42:56 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Stuff for implementing proper tail jumps.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TAILCALLS_H
#define TAILCALLS_H

/* -----------------------------------------------------------------------------
   Unmangled tail-jumping: use the mini interpretter.
   -------------------------------------------------------------------------- */

#ifdef USE_MINIINTERPRETER

#define JMP_(cont) return(stgCast(StgFunPtr,cont))
#define FB_
#define FE_

#else

/* -----------------------------------------------------------------------------
   Tail calling on x86
   -------------------------------------------------------------------------- */

#if i386_TARGET_ARCH

extern void __DISCARD__(void);

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
      void *target;			\
      __DISCARD__();			\
      target = (void *)(cont);    	\
      goto *target; 	    	    	\
    }

#endif /* i386_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on Sparc
   -------------------------------------------------------------------------- */

#ifdef sparc_TARGET_ARCH

#define JMP_(cont)	((F_) (cont))()
	/* Oh so happily, the above turns into a "call" instruction,
	   which, on a SPARC, is nothing but a "jmpl" with the
	   return address in %o7 [which we don't care about].
	*/

/* Don't need these for sparc mangling */
#define FB_
#define FE_

#endif /* sparc_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on Alpha
   -------------------------------------------------------------------------- */

#ifdef alpha_TARGET_ARCH

register void *_procedure __asm__("$27");

#define JMP_(cont)	    	    	    	\
    do { _procedure = (void *)(cont);    	\
         goto *_procedure;    	    	    	\
       } while(0)

/* Don't need these for alpha mangling */
#define FB_
#define FE_

#endif /* alpha_TARGET_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on HP
   -------------------------------------------------------------------------- */

#ifdef hppa1_1_hp_hpux_TARGET

#define JMP_(cont)				\
    do { void *_procedure = (void *)(cont);	\
         goto *_procedure;			\
       } while(0)

#endif /* hppa1_1_hp_hpux_TARGET */

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
