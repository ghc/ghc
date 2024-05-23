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
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

// Ensure that we don't get a -Wundef warning for __STDC_VERSION if compiling
// with a C++ compiler. See #20394.
#if defined(__cplusplus)
#define __STDC_VERSION__ 0
#endif

#if !(__STDC_VERSION__ >= 199901L) && !(__cplusplus >= 201103L)
# error __STDC_VERSION__ does not advertise C99, C++11 or later
#endif

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
#if !defined(IN_STG_CODE)
# define IN_STG_CODE 1

// Turn on C99 for .hc code.  This gives us the INFINITY and NAN
// constants from math.h, which we occasionally need to use in .hc (#1861)
# define _ISOC99_SOURCE

// We need _BSD_SOURCE so that math.h defines things like gamma
// on Linux
# define _BSD_SOURCE

// On AIX we need _BSD defined, otherwise <math.h> includes <stdlib.h>
# if defined(_AIX)
#  define _BSD 1
# endif

// '_BSD_SOURCE' is deprecated since glibc-2.20
// in favour of '_DEFAULT_SOURCE'
# define _DEFAULT_SOURCE
#endif

#if IN_STG_CODE == 0 || defined(CC_LLVM_BACKEND)
// C compilers that use an LLVM back end (clang or llvm-gcc) do not
// correctly support global register variables so we make sure that
// we do not declare them for these compilers.
# define NO_GLOBAL_REG_DECLS    /* don't define fixed registers */
#endif

/* Configuration */
#include "ghcconfig.h"

/* The code generator calls the math functions directly in .hc code.
   NB. after configuration stuff above, because this sets #defines
   that depend on config info, such as __USE_FILE_OFFSET64 */
#include <math.h>

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

// We generally assume C99 semantics albeit these two definitions work fine even
// when gnu90 semantics are active (i.e. when __GNUC_GNU_INLINE__ is defined or
// when a GCC older than 4.2 is used)
//
// The problem, however, is with 'extern inline' whose semantics significantly
// differs between gnu90 and C99
#define INLINE_HEADER static inline
#define STATIC_INLINE static inline

// Figure out whether `__attributes__((gnu_inline))` is needed
// to force gnu90-style 'external inline' semantics.
#if defined(FORCE_GNU_INLINE)
// disable auto-detection since HAVE_GNU_INLINE has been defined externally
#elif defined(__GNUC_GNU_INLINE__) && __GNUC__ == 4 && __GNUC_MINOR__ == 2
// GCC 4.2.x didn't properly support C99 inline semantics (GCC 4.3 was the first
// release to properly support C99 inline semantics), and therefore warned when
// using 'extern inline' while in C99 mode unless `__attributes__((gnu_inline))`
// was explicitly set.
# define FORCE_GNU_INLINE 1
#endif

#if defined(FORCE_GNU_INLINE)
// Force compiler into gnu90 semantics
# if defined(KEEP_INLINES)
#  define EXTERN_INLINE inline __attribute__((gnu_inline))
# else
#  define EXTERN_INLINE extern inline __attribute__((gnu_inline))
# endif
#elif defined(__GNUC_GNU_INLINE__)
// we're currently in gnu90 inline mode by default and
// __attribute__((gnu_inline)) may not be supported, so better leave it off
# if defined(KEEP_INLINES)
#  define EXTERN_INLINE inline
# else
#  define EXTERN_INLINE extern inline
# endif
#else
// Assume C99 semantics (yes, this curiously results in swapped definitions!)
// This is the preferred branch, and at some point we may drop support for
// compilers not supporting C99 semantics altogether.
# if defined(KEEP_INLINES)
#  define EXTERN_INLINE extern inline
# else
#  define EXTERN_INLINE inline
# endif
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

/* Used to mark a switch case that falls-through */
#if (defined(__GNUC__) && __GNUC__ >= 7)
// N.B. Don't enable fallthrough annotations when compiling with Clang.
// Apparently clang doesn't enable implicitly fallthrough warnings by default
// http://llvm.org/viewvc/llvm-project?revision=167655&view=revision
// when compiling C and the attribute cause warnings of their own (#16019).
#define FALLTHROUGH GNU_ATTRIBUTE(fallthrough)
#else
#define FALLTHROUGH ((void)0)
#endif /* __GNUC__ >= 7 */

#if !defined(DEBUG) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3))
#define GNUC_ATTR_HOT __attribute__((hot))
#else
#define GNUC_ATTR_HOT /* nothing */
#endif

#define STG_UNUSED    GNUC3_ATTRIBUTE(__unused__)
#define STG_USED      GNUC3_ATTRIBUTE(__used__)
#define STG_WARN_UNUSED_RESULT GNUC3_ATTRIBUTE(warn_unused_result)

/* Prevent functions from being optimized.
   See Note [Windows Stack allocations] */
#if defined(__clang__)
#define STG_NO_OPTIMIZE __attribute__((optnone))
#elif defined(__GNUC__) || defined(__GNUG__)
#define STG_NO_OPTIMIZE __attribute__((optimize("O0")))
#else
#define STG_NO_OPTIMIZE /* nothing */
#endif

// Mark a function as accepting a printf-like format string.
#if !defined(__GNUC__) && defined(mingw32_HOST_OS)
/* On Win64, if we say "printf" then gcc thinks we are going to use
   MS format specifiers like %I64d rather than %llu */
#define STG_PRINTF_ATTR(fmt_arg, rest) GNUC3_ATTRIBUTE(format(gnu_printf, fmt_arg, rest))
#else
/* However, on OS X, "gnu_printf" isn't recognised */
#define STG_PRINTF_ATTR(fmt_arg, rest) GNUC3_ATTRIBUTE(format(printf, fmt_arg, rest))
#endif

#define STG_RESTRICT __restrict__

#define STG_NORETURN GNU_ATTRIBUTE(__noreturn__)

#define STG_MALLOC GNUC3_ATTRIBUTE(__malloc__)

/* Instead of relying on GCC version checks to expand attributes,
 * use `__has_attribute` which is supported by GCC >= 5 and Clang. Hence, the
 * following macros won't expand on older compiler versions, but since they're
 * purely for optimization or static analysis purposes, there's no harm done.
 *
 * See: https://gcc.gnu.org/onlinedocs/cpp/_005f_005fhas_005fattribute.html
 * See: https://clang.llvm.org/docs/LanguageExtensions.html#has-attribute
 */
#ifdef __has_attribute
# define stg__has_attribute(attr) __has_attribute(attr)
#else
# define stg__has_attribute(attr) (0)
#endif

#ifdef __GNUC__
# define STG_GNUC_GUARD_VERSION(major, minor) \
    ((__GNUC__ > (major)) || \
      ((__GNUC__ == (major)) && (__GNUC_MINOR__ >= (minor))))
#else
# define STG_GNUC_GUARD_VERSION(major, minor) (0)
#endif

/*
 * The versions of the `__malloc__` attribute which take arguments are only
 * supported in GCC 11 and later.
 *
 * See: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-malloc-function-attribute
 * See: https://developers.redhat.com/blog/2021/04/30/detecting-memory-management-bugs-with-gcc-11-part-1-understanding-dynamic-allocation#attribute_malloc
 */
#if stg__has_attribute(__malloc__) && STG_GNUC_GUARD_VERSION(11, 0)
# define STG_MALLOC1(deallocator) __attribute__((__malloc__(deallocator)))
# define STG_MALLOC2(deallocator, ptrIndex) __attribute__((__malloc__(deallocator, ptrIndex)))
#else
# define STG_MALLOC1(deallocator)
# define STG_MALLOC2(deallocator, ptrIndex)
#endif

/*
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-alloc_005fsize-function-attribute
 */
#if stg__has_attribute(__alloc_size__)
# define STG_ALLOC_SIZE1(position) __attribute__((__alloc_size__(position)))
# define STG_ALLOC_SIZE2(position1, position2) __attribute__((__alloc_size__(position1, position2)))
#else
# define STG_ALLOC_SIZE1(position)
# define STG_ALLOC_SIZE2(position1, position2)
#endif

/*
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-returns_005fnonnull-function-attribute
 */
#if stg__has_attribute(__returns_nonnull__)
# define STG_RETURNS_NONNULL __attribute__((__returns_nonnull__))
#else
# define STG_RETURNS_NONNULL
#endif

/* -----------------------------------------------------------------------------
   Suppressing C warnings
   -------------------------------------------------------------------------- */

#define DO_PRAGMA(x) _Pragma(#x)
#define NO_WARN(warnoption, ...)                   \
    DO_PRAGMA(GCC diagnostic push)                 \
    DO_PRAGMA(GCC diagnostic ignored #warnoption)  \
    __VA_ARGS__                                    \
    DO_PRAGMA(GCC diagnostic pop)

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

/* byte arrays (and strings): */
#define EB_(X)    extern const char X[]
#define IB_(X)    static const char X[]
/* static (non-heap) closures (requires alignment for pointer tagging): */
#define EC_(X)    extern       StgWordArray (X) GNU_ATTRIBUTE(aligned (SIZEOF_VOID_P))
#define IC_(X)    static       StgWordArray (X) GNU_ATTRIBUTE(aligned (SIZEOF_VOID_P))
/* writable data (does not require alignment): */
#define ERW_(X)   extern       StgWordArray (X)
#define IRW_(X)   static       StgWordArray (X)
/* read-only data (does not require alignment): */
#define ERO_(X)   extern const StgWordArray (X)
#define IRO_(X)   static const StgWordArray (X)
/* foreign functions: */
#define EFF_(f)   void f() /* See Note [External function prototypes] */

/* Note [External function prototypes]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(see #8965, #11395)

In generated C code we need to distinct between two types
of external symbols:
1.  Cmm functions declared by 'EF_' macro (External Functions)
2.    C functions declared by 'EFF_' macro (External Foreign Functions)

Cmm functions are simple as they are internal to GHC.

C functions are trickier:

The external-function macro EFF_(F) used to be defined as
    extern StgFunPtr f(void)
i.e a function of zero arguments.  On most platforms this doesn't
matter very much: calls to these functions put the parameters in the
usual places anyway, and (with the exception of varargs) things just
work.

However, the ELFv2 ABI on ppc64 optimises stack allocation
(http://gcc.gnu.org/ml/gcc-patches/2013-11/msg01149.html): a call to a
function that has a prototype, is not varargs, and receives all parameters
in registers rather than on the stack does not require the caller to
allocate an argument save area.  The incorrect prototypes cause GCC to
believe that all functions declared this way can be called without an
argument save area, but if the callee has sufficiently many arguments then
it will expect that area to be present, and will thus corrupt the caller's
stack.  This happens in particular with calls to runInteractiveProcess in
libraries/process/cbits/runProcess.c, and led to #8965.

The simplest fix appears to be to declare these external functions with an
unspecified argument list rather than a void argument list.  This is no
worse for platforms that don't care either way, and allows a successful
bootstrap of GHC 7.8 on little-endian Linux ppc64 (which uses the ELFv2
ABI).

Another case is m68k ABI where 'void*' return type is returned by 'a0'
register while 'long' return type is returned by 'd0'. Thus we trick
external prototype return neither of these types to workaround #11395.
*/


/* -----------------------------------------------------------------------------
   Tail calls
   -------------------------------------------------------------------------- */

#if defined(HAS_MUSTTAIL)
/* stg-native functions: */
#define IF_(f)    static void GNUC3_ATTRIBUTE(used) f(void)
#define FN_(f)           void f(void)
#define EF_(f)           void f(void) /* External Cmm functions */
#define JMP_(cont) { __attribute__((musttail)) return ((StgFunPtr)(cont))(); }
#else
/* stg-native functions: */
#define IF_(f)    static StgFunPtr GNUC3_ATTRIBUTE(used) f(void)
#define FN_(f)           StgFunPtr f(void)
#define EF_(f)           StgFunPtr f(void) /* External Cmm functions */
#define JMP_(cont) return (StgFunPtr)(cont)
#endif

/* -----------------------------------------------------------------------------
   Other Stg stuff...
   -------------------------------------------------------------------------- */

#include "stg/DLL.h"
#include "stg/MachRegsForHost.h"
#include "stg/Regs.h"
#include "stg/Ticky.h"
#include "rts/TSANUtils.h"

#if IN_STG_CODE
/*
 * This is included later for RTS sources, after definitions of
 * StgInfoTable, StgClosure and so on.
 */
#include "stg/MiscClosures.h"
#endif

#include "stg/Prim.h" /* ghc-prim fallbacks */
#include "stg/SMP.h"

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

#if defined(WORDS_BIGENDIAN)
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
