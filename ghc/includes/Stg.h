/* -----------------------------------------------------------------------------
 * $Id: Stg.h,v 1.66 2005/01/28 12:55:52 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2004
 *
 * Top-level include file for everything STG-ish.  
 *
 * This file is included *automatically* by all .hc files.
 *
 * NOTE: always include Stg.h *before* any other headers, because we
 * define some register variables which must be done before any inline
 * functions are defined (some system headers have been known to
 * define the odd inline function).
 *
 * We generally try to keep as little visible as possible when
 * compiling .hc files.  So for example the definitions of the
 * InfoTable structs, closure structs and other RTS types are not
 * visible here.  The compiler knows enough about the representations
 * of these types to generate code which manipulates them directly
 * with pointer arithmetic.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STG_H
#define STG_H


/* If we include "Stg.h" directly, we're in STG code, and we therefore
 * get all the global register variables, macros etc. that go along
 * with that.  If "Stg.h" is included via "Rts.h", we're assumed to
 * be in vanilla C.
 */
#ifndef IN_STG_CODE
# define IN_STG_CODE 1
#endif

#if IN_STG_CODE == 0
# define NO_GLOBAL_REG_DECLS	/* don't define fixed registers */
#endif

/* Configuration */
#include "ghcconfig.h"
#include "RtsConfig.h"

/* -----------------------------------------------------------------------------
   Useful definitions
   -------------------------------------------------------------------------- */

/*
 * The C backend like to refer to labels by just mentioning their
 * names.  Howevver, when a symbol is declared as a variable in C, the
 * C compiler will implicitly dereference it when it occurs in source.
 * So we must subvert this behaviour for .hc files by declaring
 * variables as arrays, which eliminates the implicit dereference.
 */
#if IN_STG_CODE
#define RTS_VAR(x) (x)[]
#define RTS_DEREF(x) (*(x))
#else
#define RTS_VAR(x) x
#define RTS_DEREF(x) x
#endif

/* bit macros
 */
#define BITS_PER_BYTE 8
#define BITS_IN(x) (BITS_PER_BYTE * sizeof(x))

/*
 * 'Portable' inlining
 */
#if defined(__GNUC__) || defined( __INTEL_COMPILER)
# define INLINE_HEADER static inline
# define INLINE_ME inline
# define STATIC_INLINE INLINE_HEADER
#elif defined(_MSC_VER)
# define INLINE_HEADER __inline static
# define INLINE_ME __inline
# define STATIC_INLINE INLINE_HEADER
#else
# error "Don't know how to inline functions with your C compiler."
#endif

/* -----------------------------------------------------------------------------
   Global type definitions
   -------------------------------------------------------------------------- */

#include "MachDeps.h"
#include "StgTypes.h"

/* -----------------------------------------------------------------------------
   Shorthand forms
   -------------------------------------------------------------------------- */

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

typedef StgInt64        LI_;
typedef StgWord64       LW_;

#define IF_(f)		static F_ f(void)
#define FN_(f)		F_ f(void)
#define EF_(f)		extern F_ f(void)

typedef StgWord StgWordArray[];
#define EI_             extern StgWordArray
#define II_             static StgWordArray

/* -----------------------------------------------------------------------------
   Tail calls

   This needs to be up near the top as the register line on alpha needs
   to be before all procedures (inline & out-of-line).
   -------------------------------------------------------------------------- */

#include "TailCalls.h"

/* -----------------------------------------------------------------------------
   Other Stg stuff...
   -------------------------------------------------------------------------- */

#include "StgDLL.h"
#include "MachRegs.h"
#include "Regs.h"
#include "StgProf.h"  /* ToDo: separate out RTS-only stuff from here */

#if IN_STG_CODE
/*
 * This is included later for RTS sources, after definitions of
 * StgInfoTable, StgClosure and so on. 
 */
#include "StgMiscClosures.h"
#endif

/* RTS external interface */
#include "RtsExternal.h"

/* -----------------------------------------------------------------------------
   Moving Floats and Doubles

   ASSIGN_FLT is for assigning a float to memory (usually the
              stack/heap).  The memory address is guaranteed to be
	      StgWord aligned (currently == sizeof(void *)).

   PK_FLT     is for pulling a float out of memory.  The memory is
              guaranteed to be StgWord aligned.
   -------------------------------------------------------------------------- */

INLINE_HEADER void	  ASSIGN_FLT (W_ [], StgFloat);
INLINE_HEADER StgFloat    PK_FLT     (W_ []);

#if ALIGNMENT_FLOAT <= ALIGNMENT_LONG

INLINE_HEADER void     ASSIGN_FLT(W_ p_dest[], StgFloat src) { *(StgFloat *)p_dest = src; }
INLINE_HEADER StgFloat PK_FLT    (W_ p_src[])                { return *(StgFloat *)p_src; }

#else  /* ALIGNMENT_FLOAT > ALIGNMENT_UNSIGNED_INT */

INLINE_HEADER void ASSIGN_FLT(W_ p_dest[], StgFloat src)
{
    float_thing y;
    y.f = src;
    *p_dest = y.fu;
}

INLINE_HEADER StgFloat PK_FLT(W_ p_src[])
{
    float_thing y;
    y.fu = *p_src;
    return(y.f);
}

#endif /* ALIGNMENT_FLOAT > ALIGNMENT_LONG */

#if ALIGNMENT_DOUBLE <= ALIGNMENT_LONG

INLINE_HEADER void	  ASSIGN_DBL (W_ [], StgDouble);
INLINE_HEADER StgDouble   PK_DBL     (W_ []);

INLINE_HEADER void      ASSIGN_DBL(W_ p_dest[], StgDouble src) { *(StgDouble *)p_dest = src; }
INLINE_HEADER StgDouble PK_DBL    (W_ p_src[])                 { return *(StgDouble *)p_src; }

#else	/* ALIGNMENT_DOUBLE > ALIGNMENT_LONG */

/* Sparc uses two floating point registers to hold a double.  We can
 * write ASSIGN_DBL and PK_DBL by directly accessing the registers
 * independently - unfortunately this code isn't writable in C, we
 * have to use inline assembler.
 */
#if sparc_HOST_ARCH

#define ASSIGN_DBL(dst0,src) \
    { StgPtr dst = (StgPtr)(dst0); \
      __asm__("st %2,%0\n\tst %R2,%1" : "=m" (((P_)(dst))[0]), \
	"=m" (((P_)(dst))[1]) : "f" (src)); \
    }

#define PK_DBL(src0) \
    ( { StgPtr src = (StgPtr)(src0); \
        register double d; \
      __asm__("ld %1,%0\n\tld %2,%R0" : "=f" (d) : \
	"m" (((P_)(src))[0]), "m" (((P_)(src))[1])); d; \
    } )

#else /* ! sparc_HOST_ARCH */

INLINE_HEADER void	  ASSIGN_DBL (W_ [], StgDouble);
INLINE_HEADER StgDouble   PK_DBL     (W_ []);

typedef struct
  { StgWord dhi;
    StgWord dlo;
  } unpacked_double;

typedef union
  { StgDouble d;
    unpacked_double du;
  } double_thing;

INLINE_HEADER void ASSIGN_DBL(W_ p_dest[], StgDouble src)
{
    double_thing y;
    y.d = src;
    p_dest[0] = y.du.dhi;
    p_dest[1] = y.du.dlo;
}

/* GCC also works with this version, but it generates
   the same code as the previous one, and is not ANSI

#define ASSIGN_DBL( p_dest, src ) \
	*p_dest = ((double_thing) src).du.dhi; \
	*(p_dest+1) = ((double_thing) src).du.dlo \
*/

INLINE_HEADER StgDouble PK_DBL(W_ p_src[])
{
    double_thing y;
    y.du.dhi = p_src[0];
    y.du.dlo = p_src[1];
    return(y.d);
}

#endif /* ! sparc_HOST_ARCH */

#endif /* ALIGNMENT_DOUBLE > ALIGNMENT_UNSIGNED_INT */


/* -----------------------------------------------------------------------------
   Moving 64-bit quantities around

   ASSIGN_Word64      assign an StgWord64/StgInt64 to a memory location
   PK_Word64          load an StgWord64/StgInt64 from a amemory location

   In both cases the memory location might not be 64-bit aligned.
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

typedef struct
  { StgWord dhi;
    StgWord dlo;
  } unpacked_double_word;

typedef union
  { StgInt64 i;
    unpacked_double_word iu;
  } int64_thing;

typedef union
  { StgWord64 w;
    unpacked_double_word wu;
  } word64_thing;

INLINE_HEADER void ASSIGN_Word64(W_ p_dest[], StgWord64 src)
{
    word64_thing y;
    y.w = src;
    p_dest[0] = y.wu.dhi;
    p_dest[1] = y.wu.dlo;
}

INLINE_HEADER StgWord64 PK_Word64(W_ p_src[])
{
    word64_thing y;
    y.wu.dhi = p_src[0];
    y.wu.dlo = p_src[1];
    return(y.w);
}

INLINE_HEADER void ASSIGN_Int64(W_ p_dest[], StgInt64 src)
{
    int64_thing y;
    y.i = src;
    p_dest[0] = y.iu.dhi;
    p_dest[1] = y.iu.dlo;
}

INLINE_HEADER StgInt64 PK_Int64(W_ p_src[])
{
    int64_thing y;
    y.iu.dhi = p_src[0];
    y.iu.dlo = p_src[1];
    return(y.i);
}

#elif SIZEOF_VOID_P == 8

INLINE_HEADER void ASSIGN_Word64(W_ p_dest[], StgWord64 src)
{
	p_dest[0] = src;
}

INLINE_HEADER StgWord64 PK_Word64(W_ p_src[])
{
    return p_src[0];
}

INLINE_HEADER void ASSIGN_Int64(W_ p_dest[], StgInt64 src)
{
    p_dest[0] = src;
}

INLINE_HEADER StgInt64 PK_Int64(W_ p_src[])
{
    return p_src[0];
}

#endif

/* -----------------------------------------------------------------------------
   Split markers
   -------------------------------------------------------------------------- */

#if defined(USE_SPLIT_MARKERS)
#if defined(LEADING_UNDERSCORE)
#define __STG_SPLIT_MARKER __asm__("\n___stg_split_marker:");
#else
#define __STG_SPLIT_MARKER __asm__("\n__stg_split_marker:");
#endif
#else
#define __STG_SPLIT_MARKER /* nothing */
#endif

/* -----------------------------------------------------------------------------
   Integer multiply with overflow
   -------------------------------------------------------------------------- */

/* Multiply with overflow checking.
 *
 * This is tricky - the usual sign rules for add/subtract don't apply.  
 *
 * On 32-bit machines we use gcc's 'long long' types, finding
 * overflow with some careful bit-twiddling.
 *
 * On 64-bit machines where gcc's 'long long' type is also 64-bits,
 * we use a crude approximation, testing whether either operand is
 * larger than 32-bits; if neither is, then we go ahead with the
 * multiplication.
 *
 * Return non-zero if there is any possibility that the signed multiply
 * of a and b might overflow.  Return zero only if you are absolutely sure
 * that it won't overflow.  If in doubt, return non-zero.
 */

#if SIZEOF_VOID_P == 4

#ifdef WORDS_BIGENDIAN
#define RTS_CARRY_IDX__ 0
#define RTS_REM_IDX__  1
#else
#define RTS_CARRY_IDX__ 1
#define RTS_REM_IDX__ 0
#endif

typedef union {
    StgInt64 l;
    StgInt32 i[2];
} long_long_u ;

#define mulIntMayOflo(a,b)			\
({                                              \
  StgInt32 r, c;				\
  long_long_u z;				\
  z.l = (StgInt64)a * (StgInt64)b;		\
  r = z.i[RTS_REM_IDX__];			\
  c = z.i[RTS_CARRY_IDX__];			\
  if (c == 0 || c == -1) {			\
    c = ((StgWord)((a^b) ^ r))			\
      >> (BITS_IN (I_) - 1);			\
  }						\
  c;                                            \
})

/* Careful: the carry calculation above is extremely delicate.  Make sure
 * you test it thoroughly after changing it.
 */

#else

#define HALF_INT  (((I_)1) << (BITS_IN (I_) / 2))

#define stg_abs(a) (((I_)(a)) < 0 ? -((I_)(a)) : ((I_)(a)))

#define mulIntMayOflo(a,b)			\
({                                              \
  I_ c; 					\
  if (stg_abs(a) >= HALF_INT ||			\
      stg_abs(b) >= HALF_INT) {			\
    c = 1;					\
  } else {					\
    c = 0;					\
  }						\
  c;                                            \
})
#endif

#endif /* STG_H */
