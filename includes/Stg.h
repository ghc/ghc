/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Top-level include file for everything required when compiling .hc
 * code.  NOTE: in .hc files, Stg.h must be included *before* any
 * other headers, because we define some register variables which must
 * be done before any inline functions are defined (some system
 * headers have been known to define the odd inline function).
 *
 * We generally try to keep as little visible as possible when
 * compiling .hc files.  So for example the definitions of the
 * InfoTable structs, closure structs and other RTS types are not
 * visible here.  The compiler knows enough about the representations
 * of these types to generate code which manipulates them directly
 * with pointer arithmetic.
 *
 * In ordinary C code, do not #include this file directly: #include
 * "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef STG_H
#define STG_H

/*
 * If we are compiling a .hc file, then we want all the register
 * variables.  This is the what happens if you #include "Stg.h" first:
 * we assume this is a .hc file, and set IN_STG_CODE==1, which later
 * causes the register variables to be enabled in stg/Regs.h.
 *
 * If instead "Rts.h" is included first, then we are compiling a
 * vanilla C file.  Everything from Stg.h is provided, except that
 * IN_STG_CODE is not defined, and the register variables will not be
 * active.
 */
#ifndef IN_STG_CODE
# define IN_STG_CODE 1

// Turn on C99 for .hc code.  This gives us the INFINITY and NAN
// constants from math.h, which we occasionally need to use in .hc (#1861)
# define _ISOC99_SOURCE

// We need _BSD_SOURCE so that math.h defines things like gamma
// on Linux
# define _BSD_SOURCE
#endif

#if IN_STG_CODE == 0 || defined(llvm_CC_FLAVOR)
// C compilers that use an LLVM back end (clang or llvm-gcc) do not
// correctly support global register variables so we make sure that
// we do not declare them for these compilers.
# define NO_GLOBAL_REG_DECLS	/* don't define fixed registers */
#endif

/* Configuration */
#include "ghcconfig.h"

/* The code generator calls the math functions directly in .hc code.
   NB. after configuration stuff above, because this sets #defines
   that depend on config info, such as __USE_FILE_OFFSET64 */
#include <math.h>

// On Solaris, we don't get the INFINITY and NAN constants unless we
// #define _STDC_C99, and we can't do that unless we also use -std=c99,
// because _STDC_C99 causes the headers to use C99 syntax (e.g. restrict).
// We aren't ready for -std=c99 yet, so define INFINITY/NAN by hand using
// the gcc builtins.
#if !defined(INFINITY)
#if defined(__GNUC__)
#define INFINITY __builtin_inf()
#else
#error No definition for INFINITY
#endif
#endif

#if !defined(NAN)
#if defined(__GNUC__)
#define NAN __builtin_nan("")
#else
#error No definition for NAN
#endif
#endif

/* -----------------------------------------------------------------------------
   Useful definitions
   -------------------------------------------------------------------------- */

/*
 * The C backend likes to refer to labels by just mentioning their
 * names.  However, when a symbol is declared as a variable in C, the
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

/* Compute offsets of struct fields
 */
#define STG_FIELD_OFFSET(s_type, field) ((StgWord)&(((s_type*)0)->field))

/*
 * 'Portable' inlining:
 * INLINE_HEADER is for inline functions in header files (macros)
 * STATIC_INLINE is for inline functions in source files
 * EXTERN_INLINE is for functions that we want to inline sometimes
 * (we also compile a static version of the function; see Inlines.c)
 */
#if defined(__GNUC__) || defined( __INTEL_COMPILER)

# define INLINE_HEADER static inline
# define INLINE_ME inline
# define STATIC_INLINE INLINE_HEADER

// The special "extern inline" behaviour is now only supported by gcc
// when _GNUC_GNU_INLINE__ is defined, and you have to use
// __attribute__((gnu_inline)).  So when we don't have this, we use
// ordinary static inline.
//
// Apple's gcc defines __GNUC_GNU_INLINE__ without providing
// gnu_inline, so we exclude MacOS X and fall through to the safe
// version.
//
#if defined(__GNUC_GNU_INLINE__) && !defined(__APPLE__)
#  if defined(KEEP_INLINES)
#    define EXTERN_INLINE inline
#  else
#    define EXTERN_INLINE extern inline __attribute__((gnu_inline))
#  endif
#else
#  if defined(KEEP_INLINES)
#    define EXTERN_INLINE
#  else
#    define EXTERN_INLINE INLINE_HEADER
#  endif
#endif

#elif defined(_MSC_VER)

# define INLINE_HEADER __inline static
# define INLINE_ME __inline
# define STATIC_INLINE INLINE_HEADER

# if defined(KEEP_INLINES)
#  define EXTERN_INLINE __inline
# else
#  define EXTERN_INLINE __inline extern
# endif

#else

# error "Don't know how to inline functions with your C compiler."

#endif


/*
 * GCC attributes
 */
#if defined(__GNUC__)
#define GNU_ATTRIBUTE(at) __attribute__((at))
#else
#define GNU_ATTRIBUTE(at)
#endif

#if __GNUC__ >= 3
#define GNUC3_ATTRIBUTE(at) __attribute__((at))
#else
#define GNUC3_ATTRIBUTE(at)
#endif

#if __GNUC__ > 4 || __GNUC__ == 4 && __GNUC_MINOR__ >= 3
#define GNUC_ATTR_HOT __attribute__((hot))
#else
#define GNUC_ATTR_HOT /* nothing */
#endif

#define STG_UNUSED    GNUC3_ATTRIBUTE(__unused__)

/* -----------------------------------------------------------------------------
   Global type definitions
   -------------------------------------------------------------------------- */

#include "MachDeps.h"
#include "stg/Types.h"

/* -----------------------------------------------------------------------------
   Shorthand forms
   -------------------------------------------------------------------------- */

typedef StgChar      C_;
typedef StgWord      W_;
typedef StgWord*  P_;
typedef StgInt    I_;
typedef StgWord StgWordArray[];
typedef StgFunPtr       F_;

#define EI_(X)          extern StgWordArray (X) GNU_ATTRIBUTE(aligned (8))
#define II_(X)          static StgWordArray (X) GNU_ATTRIBUTE(aligned (8))
#define IF_(f)    static StgFunPtr GNUC3_ATTRIBUTE(used) f(void)
#define FN_(f)    StgFunPtr f(void)
#define EF_(f)    extern StgFunPtr f(void)

/* -----------------------------------------------------------------------------
   Tail calls
   -------------------------------------------------------------------------- */

#define JMP_(cont) return((StgFunPtr)(cont))
#define FB_
#define FE_

/* -----------------------------------------------------------------------------
   Other Stg stuff...
   -------------------------------------------------------------------------- */

#include "stg/DLL.h"
#include "stg/RtsMachRegs.h"
#include "stg/Regs.h"
#include "stg/Ticky.h"

#if IN_STG_CODE
/*
 * This is included later for RTS sources, after definitions of
 * StgInfoTable, StgClosure and so on.
 */
#include "stg/MiscClosures.h"
#endif

#include "stg/SMP.h" // write_barrier() inline is required

/* -----------------------------------------------------------------------------
   Moving Floats and Doubles

   ASSIGN_FLT is for assigning a float to memory (usually the
              stack/heap).  The memory address is guaranteed to be
         StgWord aligned (currently == sizeof(void *)).

   PK_FLT     is for pulling a float out of memory.  The memory is
              guaranteed to be StgWord aligned.
   -------------------------------------------------------------------------- */

INLINE_HEADER void     ASSIGN_FLT (W_ [], StgFloat);
INLINE_HEADER StgFloat    PK_FLT     (W_ []);

#if ALIGNMENT_FLOAT <= ALIGNMENT_VOID_P

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

#endif /* ALIGNMENT_FLOAT > ALIGNMENT_VOID_P */

#if ALIGNMENT_DOUBLE <= ALIGNMENT_VOID_P

INLINE_HEADER void     ASSIGN_DBL (W_ [], StgDouble);
INLINE_HEADER StgDouble   PK_DBL     (W_ []);

INLINE_HEADER void      ASSIGN_DBL(W_ p_dest[], StgDouble src) { *(StgDouble *)p_dest = src; }
INLINE_HEADER StgDouble PK_DBL    (W_ p_src[])                 { return *(StgDouble *)p_src; }

#else /* ALIGNMENT_DOUBLE > ALIGNMENT_VOID_P */

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

INLINE_HEADER void     ASSIGN_DBL (W_ [], StgDouble);
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

#if SIZEOF_HSWORD == 4

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

#endif /* SIZEOF_HSWORD == 4 */

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
   Write-combining store
   -------------------------------------------------------------------------- */

INLINE_HEADER void
wcStore (StgPtr p, StgWord w)
{
#ifdef x86_64_HOST_ARCH
    __asm__(
   "movnti\t%1, %0"
   : "=m" (*p)
   : "r" (w)
   );
#else
      *p = w;
#endif
}

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

#define mulIntMayOflo(a,b)       \
({                                              \
  StgInt32 r, c;           \
  long_long_u z;           \
  z.l = (StgInt64)a * (StgInt64)b;     \
  r = z.i[RTS_REM_IDX__];        \
  c = z.i[RTS_CARRY_IDX__];         \
  if (c == 0 || c == -1) {       \
    c = ((StgWord)((a^b) ^ r))         \
      >> (BITS_IN (I_) - 1);        \
  }                  \
  c;                                            \
})

/* Careful: the carry calculation above is extremely delicate.  Make sure
 * you test it thoroughly after changing it.
 */

#else

/* Approximate version when we don't have long arithmetic (on 64-bit archs) */

/* If we have n-bit words then we have n-1 bits after accounting for the
 * sign bit, so we can fit the result of multiplying 2 (n-1)/2-bit numbers */
#define HALF_POS_INT  (((I_)1) << ((BITS_IN (I_) - 1) / 2))
#define HALF_NEG_INT  (-HALF_POS_INT)

#define mulIntMayOflo(a,b)       \
({                                              \
  I_ c;              \
  if ((I_)a <= HALF_NEG_INT || a >= HALF_POS_INT    \
      || (I_)b <= HALF_NEG_INT || b >= HALF_POS_INT) {\
    c = 1;              \
  } else {              \
    c = 0;              \
  }                  \
  c;                                            \
})
#endif

#endif /* STG_H */
