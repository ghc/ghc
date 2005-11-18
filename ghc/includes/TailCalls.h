/* -----------------------------------------------------------------------------
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
      void *__target;			\
      __DISCARD__();			\
      __target = (void *)(cont);    	\
      goto *__target; 	    	    	\
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
   Tail calling on Alpha
   -------------------------------------------------------------------------- */

#ifdef alpha_HOST_ARCH

#if IN_STG_CODE
register void *_procedure __asm__("$27");
#endif

#define JMP_(cont)				\
    do { _procedure = (void *)(cont);		\
         __DISCARD__();				\
         goto *_procedure;			\
       } while(0)

/* Don't need these for alpha mangling */
#define FB_
#define FE_

#endif /* alpha_HOST_ARCH */

/* -----------------------------------------------------------------------------
   Tail calling on HP

Description of HP's weird procedure linkage, many thanks to Andy Bennet
<andy_bennett@hp.com>:

I've been digging a little further into the problem of how HP-UX does
dynamic procedure calls. My solution in the last e-mail inserting an extra
'if' statement into the JMP_ I think is probably the best general solution I
can come up with. There are still a few problems with it however: It wont
work, if JMP_ ever has to call anything in a shared library, if this is
likely to be required it'll need something more elaborate. It also wont work
with PA-RISC 2.0 wide mode (64-bit) which uses a different format PLT.

I had some feedback from someone in HP's compiler lab and the problem
relates to the linker on HP-UX, not gcc as I first suspected. The reason the
'hsc' executable works is most likely due to a change in 'ld's behaviour for
performance reasons between your revision and mine.

The major issue relating to this is shared libraries and how they are
implented under HP-UX. The whole point of the Procedure Label Table (PLT) is
to allow a function pointer to hold the address of the function and a
pointer to the library's global data lookup table (DLT) used by position
independent code (PIC). This makes the PLT absolutely essential for shared
library calls. HP has two linker introduced assembly functions for dealing
with dynamic calls, $$dyncall and $$dyncall_external. The former does a
check to see if the address is a PLT pointer and dereferences if necessary
or just calls the address otherwise; the latter skips the check and just
does the indirect jump no matter what.

Since $$dyncall_external runs faster due to its not having the test, the
linker nowadays prefers to generate calls to that, rather than $$dyncall. It
makes this decision based on the presence of any shared library. If it even
smells an sl's existence at link time, it rigs the runtime system to
generate PLT references for everything on the assumption that the result
will be slightly more efficient. This is what is crashing GHC since the
calls it is generating have no understanding of the procedure label proper.
The only way to get real addresses is to link everything archive, including
system libraries, at which point it assumes you probably are going to be
using calls similar to GHC's (its rigged for HP's +ESfic compiler option)
but uses $$dyncall if necessary to cope, just in case you aren't.

   -------------------------------------------------------------------------- */

#ifdef hppa1_1_hp_hpux_TARGET

#define JMP_(cont)                              \
    do { void *_procedure = (void *)(cont);     \
         if (((int) _procedure) & 2)            \
            _procedure = (void *)(*((int *) (_procedure - 2))); \
         goto *_procedure;                      \
       } while(0)

#endif /* hppa1_1_hp_hpux_TARGET */

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
   Tail calling on IA64
   -------------------------------------------------------------------------- */

#ifdef ia64_HOST_ARCH

/* The compiler can more intelligently decide how to do this.  We therefore
 * implement it as a call and optimise to a jump at mangle time. */
#define JMP_(cont)	((F_) (cont))(); __asm__ volatile ("--- TAILCALL ---");

/* Don't emit calls to __DISCARD__ as this causes hassles */
#define __DISCARD__()

#endif

/* -----------------------------------------------------------------------------
  FUNBEGIN and FUNEND.

  These are markers indicating the start and end of Real Code in a
  function.  All instructions between the actual start and end of the
  function and these markers is shredded by the mangler.
  -------------------------------------------------------------------------- */

#ifndef FB_
#if __GNUC__ < 3
/*  The following __DISCARD__() has become necessary with gcc 2.96 on x86.
 *  It prevents gcc from moving stack manipulation code from the function
 *  body (aka the Real Code) into the function prologue, ie, from moving it
 *  over the --- BEGIN --- marker.  It should be noted that (like some
 *  other black magic in GHC's code), there is no essential reason why gcc
 *  could not move some stack manipulation code across the __DISCARD__() -
 *  it just doesn't choose to do it at the moment.
 *  -= chak
 */
#define FB_    __asm__ volatile ("--- BEGIN ---"); __DISCARD__ ();
#else
/* The __DISCARD__() doesn't appear to be necessary with gcc >= 3.2 at
 * least, and it does cause some difficulty, preventing gcc from
 * optimising around the beginning of the function.  In particular,
 * gcc leaves some stack assignments in the prologue when the call is
 * present. --SDM

 */
#define FB_    __asm__ volatile ("--- BEGIN ---");
#endif
#endif

#ifndef FE_
#define FE_    __asm__ volatile ("--- END ---");
#endif

#endif /* !USE_MINIINTERPRETER */

#endif /* TAILCALLS_H */
