/* -----------------------------------------------------------------------------
 * $Id: StgTypes.h,v 1.8 2000/04/04 13:40:27 panne Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Various C datatypes used in the run-time system.

 * Specifically:

	StgInt8,  16, 32, 64
	StgWord8, 16, 32, 64
	StgChar, StgFloat, StgDouble

	***** All the same size: *****
	StgPtr			Basic pointer type
	StgWord			Unit of heap allocation
	StgInt			Signed version of StgWord
	StgAddr			Generic address type
	

 *
 * ---------------------------------------------------------------------------*/

#ifndef STGTYPES_H
#define STGTYPES_H

/*
 * First, platform-dependent definitions of size-specific integers.
 * Assume for now that the int type is 32 bits.
 * NOTE: Synch the following definitions with MachDeps.h!
 * ToDo: move these into a platform-dependent file.
 */

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;

typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;


#if SIZEOF_UNSIGNED_INT == 4
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
#else
#error GHC untested on this architecture: sizeof(unsigned int) != 4
#endif

/* This #define controls whether we need to support long longs on a particular
 * platform. 
 *
 * ToDo: find a proper home for (derived) configuration information like this.
 */
#if HAVE_LONG_LONG && SIZEOF_VOID_P < 8
#define SUPPORT_LONG_LONGS
#endif

#ifdef SUPPORT_LONG_LONGS
/* assume long long is 64 bits */
typedef unsigned long long int StgWord64;
typedef signed long long int   StgInt64;
#elif SIZEOF_LONG == 8
typedef signed   long          StgInt64;
typedef unsigned long          StgWord64;
#else
#error GHC untested on this architecture: sizeof(void *) < 8 and no long longs.
#endif


/*
 * Define the standard word size we'll use on this machine: make it
 * big enough to hold a pointer.
 */

#if SIZEOF_VOID_P == 8
typedef StgInt64           StgInt;
typedef StgWord64          StgWord;
#else
#if SIZEOF_VOID_P == 4
typedef StgInt32           StgInt; 
typedef StgWord32          StgWord;
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
#endif

typedef void*              StgAddr;

/*
 * Other commonly-used STG datatypes.
 */

typedef StgWord8           StgChar;
typedef int                StgBool;
/*
 * If a double fits in an StgWord, don't bother using floats.
 */

#if SIZEOF_DOUBLE == SIZEOF_VOID_P
typedef double		   StgFloat;
typedef double		   StgDouble;
#define FLOATS_AS_DOUBLES  1
#else
typedef float		   StgFloat;
typedef double		   StgDouble;
#endif
                           
typedef void               StgVoid;
                           
typedef struct StgClosure_* StgClosurePtr;
typedef StgWord*           StgPtr;           /* pointer into closure       */
typedef StgWord            StgOffset;        /* byte offset within closure */
                           
typedef struct StgTSO_*    StgTSOPtr;

typedef void *             StgForeignPtr;

typedef StgInt             StgStackOffset;   /* offset in words! */

typedef StgWord*           StgStackPtr;

typedef StgWord8 	   StgCode;  	    /* close enough */
typedef StgCode*	   StgCodePtr;	

typedef StgPtr*            StgArray;        /* the goods of an Array# */
typedef char*		   StgByteArray;    /* the goods of a ByteArray# */

typedef StgInt64	       LI_;
typedef StgWord64	       LW_;

/* Stable Pointers:  A stable pointer is represented as an index into
 * the stable pointer table in the low 24 bits with a weight in the
 * upper 8 bits.
 */
typedef StgWord            StgStablePtr;

#define STABLEPTR_WEIGHT_MASK   ((StgWord)0xff << ((sizeof(StgWord)-1) * BITS_PER_BYTE))
#define STABLEPTR_WEIGHT_SHIFT  (BITS_IN(StgWord) - 8)

/*
  Types for the generated C functions
  take no arguments
  return a pointer to the next function to be called
  use: Ptr to Fun that returns a Ptr to Fun which returns Ptr to void

  Note: Neither StgFunPtr not StgFun is quite right (that is, 
  StgFunPtr != StgFun*).  So, the functions we define all have type
  StgFun but we always have to cast them to StgFunPtr when we assign
  them to something.
  The only way round this would be to write a recursive type but
  C only allows that if you're defining a struct or union.
*/

typedef void  *(*(*StgFunPtr)(void))(void);
typedef StgFunPtr StgFun(void);

typedef union {
    StgWord        w;
    StgAddr        a;
    StgChar        c;
    StgFloat       f;
    StgInt         i;
    StgPtr         p;
    StgClosurePtr  cl;
    StgStackOffset offset;	/* unused? */
    StgByteArray   b;
    StgTSOPtr      t;
} StgUnion;

/*
 * Shorthand forms
 */

typedef StgChar		C_;
typedef StgWord		W_;
typedef StgWord*	P_;
typedef P_*		PP_;
typedef StgInt		I_;
typedef StgAddr	        A_;
typedef const StgWord*  D_;
typedef StgFunPtr       F_;
typedef StgByteArray    B_;
typedef StgClosurePtr   L_;

/*
 * We often want to know the size of something in units of an
 * StgWord... (rounded up, of course!)
 */

#define sizeofW(t) ((sizeof(t)+sizeof(W_)-1)/sizeof(W_))

/* 
 * It's nice to be able to grep for casts
 */

#define stgCast(ty,e) ((ty)(e))

#endif STGTYPES_H

