/* -----------------------------------------------------------------------------
 * $Id: PrimOps.h,v 1.90 2001/12/18 15:23:16 sewardj Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Macros for primitive operations in STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

/* As of 5 Dec 01, this file no longer implements the primops, since they are
   translated into standard C in compiler/absCSyn/AbsCUtils during the absC
   flattening pass.  Only {add,sub,mul}IntCzh remain untranslated.  Most of
   what is here is now EXTFUN_RTS declarations for the out-of-line primop
   implementations which live in compiler/rts/PrimOps.hc.
*/

#ifndef PRIMOPS_H
#define PRIMOPS_H

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 32
#error GHC C backend requires 32+-bit words
#endif


/* -----------------------------------------------------------------------------
 * Int operations with carry.
 * -------------------------------------------------------------------------- */

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
#define C 0
#define R 1
#else
#define C 1
#define R 0
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
  r = z.i[R];					\
  c = z.i[C];					\
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


/* -----------------------------------------------------------------------------
   Integer PrimOps.
   -------------------------------------------------------------------------- */

/* NOTE: gcdIntzh and gcdIntegerIntzh work only for positive inputs! */

/* Some of these are out-of-line: -------- */

/* Integer arithmetic */
EXTFUN_RTS(plusIntegerzh_fast);
EXTFUN_RTS(minusIntegerzh_fast);
EXTFUN_RTS(timesIntegerzh_fast);
EXTFUN_RTS(gcdIntegerzh_fast);
EXTFUN_RTS(quotRemIntegerzh_fast);
EXTFUN_RTS(quotIntegerzh_fast);
EXTFUN_RTS(remIntegerzh_fast);
EXTFUN_RTS(divExactIntegerzh_fast);
EXTFUN_RTS(divModIntegerzh_fast);

EXTFUN_RTS(cmpIntegerIntzh_fast);
EXTFUN_RTS(cmpIntegerzh_fast);
EXTFUN_RTS(integer2Intzh_fast);
EXTFUN_RTS(integer2Wordzh_fast);
EXTFUN_RTS(gcdIntegerIntzh_fast);
EXTFUN_RTS(gcdIntzh_fast);

/* Conversions */
EXTFUN_RTS(int2Integerzh_fast);
EXTFUN_RTS(word2Integerzh_fast);

/* Floating-point decodings */
EXTFUN_RTS(decodeFloatzh_fast);
EXTFUN_RTS(decodeDoublezh_fast);

/* Bit operations */
EXTFUN_RTS(andIntegerzh_fast);
EXTFUN_RTS(orIntegerzh_fast);
EXTFUN_RTS(xorIntegerzh_fast);
EXTFUN_RTS(complementIntegerzh_fast);


/* -----------------------------------------------------------------------------
   Word64 PrimOps.
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

/* Conversions */
EXTFUN_RTS(int64ToIntegerzh_fast);
EXTFUN_RTS(word64ToIntegerzh_fast);

/* The rest are (way!) out of line, implemented in vanilla C. */
I_ stg_gtWord64 (StgWord64, StgWord64);
I_ stg_geWord64 (StgWord64, StgWord64);
I_ stg_eqWord64 (StgWord64, StgWord64);
I_ stg_neWord64 (StgWord64, StgWord64);
I_ stg_ltWord64 (StgWord64, StgWord64);
I_ stg_leWord64 (StgWord64, StgWord64);

I_ stg_gtInt64 (StgInt64, StgInt64);
I_ stg_geInt64 (StgInt64, StgInt64);
I_ stg_eqInt64 (StgInt64, StgInt64);
I_ stg_neInt64 (StgInt64, StgInt64);
I_ stg_ltInt64 (StgInt64, StgInt64);
I_ stg_leInt64 (StgInt64, StgInt64);

LW_ stg_remWord64  (StgWord64, StgWord64);
LW_ stg_quotWord64 (StgWord64, StgWord64);

LI_ stg_remInt64    (StgInt64, StgInt64);
LI_ stg_quotInt64   (StgInt64, StgInt64);
LI_ stg_negateInt64 (StgInt64);
LI_ stg_plusInt64   (StgInt64, StgInt64);
LI_ stg_minusInt64  (StgInt64, StgInt64);
LI_ stg_timesInt64  (StgInt64, StgInt64);

LW_ stg_and64  (StgWord64, StgWord64);
LW_ stg_or64   (StgWord64, StgWord64);
LW_ stg_xor64  (StgWord64, StgWord64);
LW_ stg_not64  (StgWord64);

LW_ stg_uncheckedShiftL64   (StgWord64, StgInt);
LW_ stg_uncheckedShiftRL64  (StgWord64, StgInt);
LI_ stg_uncheckedIShiftL64  (StgInt64, StgInt);
LI_ stg_uncheckedIShiftRL64 (StgInt64, StgInt);
LI_ stg_uncheckedIShiftRA64 (StgInt64, StgInt);

LI_ stg_intToInt64    (StgInt);
I_  stg_int64ToInt    (StgInt64);
LW_ stg_int64ToWord64 (StgInt64);

LW_ stg_wordToWord64  (StgWord);
W_  stg_word64ToWord  (StgWord64);
LI_ stg_word64ToInt64 (StgWord64);

LI_ stg_integerToInt64 (I_ sa, StgByteArray /* Really: mp_limb_t* */ da);
LW_ stg_integerToWord64 (I_ sa, StgByteArray /* Really: mp_limb_t* */ da);

#endif

/* -----------------------------------------------------------------------------
   Array PrimOps.
   -------------------------------------------------------------------------- */

/* We cast to void* instead of StgChar* because this avoids a warning
 * about increasing the alignment requirements.
 */
#define REAL_BYTE_ARR_CTS(a)   ((void *) (((StgArrWords *)(a))->payload))
#define REAL_PTRS_ARR_CTS(a)   ((P_)   (((StgMutArrPtrs  *)(a))->payload))

#ifdef DEBUG
#define BYTE_ARR_CTS(a)				  \
 ({ ASSERT(GET_INFO((StgArrWords *)(a)) == &stg_ARR_WORDS_info); 	  \
    REAL_BYTE_ARR_CTS(a); })
#define PTRS_ARR_CTS(a)				  \
 ({ ASSERT((GET_INFO((StgMutArrPtrs  *)(a)) == &stg_MUT_ARR_PTRS_FROZEN_info)	  \
	|| (GET_INFO((StgMutArrPtrs  *)(a)) == &stg_MUT_ARR_PTRS_info));  \
    REAL_PTRS_ARR_CTS(a); })
#else
#define BYTE_ARR_CTS(a)		REAL_BYTE_ARR_CTS(a)
#define PTRS_ARR_CTS(a)		REAL_PTRS_ARR_CTS(a)
#endif


extern I_ genSymZh(void);
extern I_ resetGenSymZh(void);

/*--- Almost everything in line. */

EXTFUN_RTS(unsafeThawArrayzh_fast);
EXTFUN_RTS(newByteArrayzh_fast);
EXTFUN_RTS(newPinnedByteArrayzh_fast);
EXTFUN_RTS(newArrayzh_fast);

/* The decode operations are out-of-line because they need to allocate
 * a byte array.
 */

/* We only support IEEE floating point formats. */
#include "ieee-flpt.h"
EXTFUN_RTS(decodeFloatzh_fast);
EXTFUN_RTS(decodeDoublezh_fast);

/* grimy low-level support functions defined in StgPrimFloat.c */
extern StgDouble __encodeDouble (I_ size, StgByteArray arr, I_ e);
extern StgDouble __int_encodeDouble (I_ j, I_ e);
extern StgFloat  __encodeFloat (I_ size, StgByteArray arr, I_ e);
extern StgFloat  __int_encodeFloat (I_ j, I_ e);
extern void      __decodeDouble (MP_INT *man, I_ *_exp, StgDouble dbl);
extern void      __decodeFloat  (MP_INT *man, I_ *_exp, StgFloat flt);
extern StgInt    isDoubleNaN(StgDouble d);
extern StgInt    isDoubleInfinite(StgDouble d);
extern StgInt    isDoubleDenormalized(StgDouble d);
extern StgInt    isDoubleNegativeZero(StgDouble d);
extern StgInt    isFloatNaN(StgFloat f);
extern StgInt    isFloatInfinite(StgFloat f);
extern StgInt    isFloatDenormalized(StgFloat f);
extern StgInt    isFloatNegativeZero(StgFloat f);


/* -----------------------------------------------------------------------------
   Mutable variables

   newMutVar is out of line.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(newMutVarzh_fast);


/* -----------------------------------------------------------------------------
   MVar PrimOps.

   All out of line, because they either allocate or may block.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(isEmptyMVarzh_fast);
EXTFUN_RTS(newMVarzh_fast);
EXTFUN_RTS(takeMVarzh_fast);
EXTFUN_RTS(putMVarzh_fast);
EXTFUN_RTS(tryTakeMVarzh_fast);
EXTFUN_RTS(tryPutMVarzh_fast);


/* -----------------------------------------------------------------------------
   Delay/Wait PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(waitReadzh_fast);
EXTFUN_RTS(waitWritezh_fast);
EXTFUN_RTS(delayzh_fast);


/* -----------------------------------------------------------------------------
   Primitive I/O, error-handling PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(catchzh_fast);
EXTFUN_RTS(raisezh_fast);

extern void stg_exit(I_ n)  __attribute__ ((noreturn));


/* -----------------------------------------------------------------------------
   Stable Name / Stable Pointer  PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(makeStableNamezh_fast);
EXTFUN_RTS(makeStablePtrzh_fast);
EXTFUN_RTS(deRefStablePtrzh_fast);


/* -----------------------------------------------------------------------------
   Concurrency/Exception PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(forkzh_fast);
EXTFUN_RTS(yieldzh_fast);
EXTFUN_RTS(killThreadzh_fast);
EXTFUN_RTS(seqzh_fast);
EXTFUN_RTS(blockAsyncExceptionszh_fast);
EXTFUN_RTS(unblockAsyncExceptionszh_fast);
EXTFUN_RTS(myThreadIdzh_fast);

extern int cmp_thread(const StgTSO *tso1, const StgTSO *tso2);
extern int rts_getThreadId(const StgTSO *tso);


/* -----------------------------------------------------------------------------
   Weak Pointer PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(mkWeakzh_fast);
EXTFUN_RTS(finalizzeWeakzh_fast);
EXTFUN_RTS(deRefWeakzh_fast);


/* -----------------------------------------------------------------------------
   Foreign Object PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(mkForeignObjzh_fast);


/* -----------------------------------------------------------------------------
   BCOs and BCO linkery
   -------------------------------------------------------------------------- */

EXTFUN_RTS(newBCOzh_fast);
EXTFUN_RTS(mkApUpd0zh_fast);


/* -----------------------------------------------------------------------------
   Signal handling.  Not really primops, but called directly from Haskell. 
   -------------------------------------------------------------------------- */

#define STG_SIG_DFL  (-1)
#define STG_SIG_IGN  (-2)
#define STG_SIG_ERR  (-3)
#define STG_SIG_HAN  (-4)

extern StgInt stg_sig_install (StgInt, StgInt, StgStablePtr, sigset_t *);
#define stg_sig_default(sig,mask) stg_sig_install(sig,STG_SIG_DFL,0,(sigset_t *)mask)
#define stg_sig_ignore(sig,mask) stg_sig_install(sig,STG_SIG_IGN,0,(sigset_t *)mask)
#define stg_sig_catch(sig,ptr,mask) stg_sig_install(sig,STG_SIG_HAN,ptr,(sigset_t *)mask)


/* ------------------------------------------------------------------------
   Parallel PrimOps

   A par in the Haskell code is ultimately translated to a parzh macro
   (with a case wrapped around it to guarantee that the macro is actually 
    executed; see compiler/prelude/PrimOps.lhs)
   In GUM and SMP we only add a pointer to the spark pool.
   In GranSim we call an RTS fct, forwarding additional parameters which
   supply info on granularity of the computation, size of the result value
   and the degree of parallelism in the sparked expression.
   ---------------------------------------------------------------------- */

#if defined(GRAN)
//@cindex _par_
#define parzh(r,node)             parAny(r,node,1,0,0,0,0,0)

//@cindex _parAt_
#define parAtzh(r,node,where,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,1)

//@cindex _parAtAbs_
#define parAtAbszh(r,node,proc,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,proc,identifier,gran_info,size_info,par_info,rest,2)

//@cindex _parAtRel_
#define parAtRelzh(r,node,proc,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,proc,identifier,gran_info,size_info,par_info,rest,3)

//@cindex _parAtForNow_
#define parAtForNowzh(r,node,where,identifier,gran_info,size_info,par_info,rest)	\
	parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,0)

#define parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,local)	\
{							        \
  if (closure_SHOULD_SPARK((StgClosure*)node)) {		\
    rtsSparkQ result;						\
    PEs p;                                                      \
                                                                \
    STGCALL6(newSpark, node,identifier,gran_info,size_info,par_info,local); \
    switch (local) {                                                        \
      case 2: p = where;  /* parAtAbs means absolute PE no. expected */     \
              break;                                                        \
      case 3: p = CurrentProc+where; /* parAtRel means rel PE no. expected */\
              break;                                                        \
      default: p = where_is(where); /* parAt means closure expected */      \
              break;                                                        \
    }                                                                       \
    /* update GranSim state according to this spark */                      \
    STGCALL3(GranSimSparkAtAbs, result, (I_)p, identifier);                 \
  }                                                                         \
}

//@cindex _parLocal_
#define parLocalzh(r,node,identifier,gran_info,size_info,par_info,rest)	\
	parAny(r,node,rest,identifier,gran_info,size_info,par_info,1)

//@cindex _parGlobal_
#define parGlobalzh(r,node,identifier,gran_info,size_info,par_info,rest) \
	parAny(r,node,rest,identifier,gran_info,size_info,par_info,0)

#define parAny(r,node,rest,identifier,gran_info,size_info,par_info,local) \
{                                                                        \
  if (closure_SHOULD_SPARK((StgClosure*)node)) {                         \
    rtsSpark *result;						         \
    result = RET_STGCALL6(rtsSpark*, newSpark,                           \
                          node,identifier,gran_info,size_info,par_info,local);\
    STGCALL1(add_to_spark_queue,result); 				\
    STGCALL2(GranSimSpark, local,(P_)node);	                        \
  }							                \
}

#define copyablezh(r,node)				\
  /* copyable not yet implemented!! */

#define noFollowzh(r,node)				\
  /* noFollow not yet implemented!! */

#elif defined(SMP) || defined(PAR)

#define parzh(r,node)					\
{							\
  extern unsigned int context_switch; 			\
  if (closure_SHOULD_SPARK((StgClosure *)node) &&	\
      SparkTl < SparkLim) {				\
    *SparkTl++ = (StgClosure *)(node);			\
  }							\
  r = context_switch = 1;				\
}
#else /* !GRAN && !SMP && !PAR */
#define parzh(r,node) r = 1
#endif

/* -----------------------------------------------------------------------------
   ForeignObj - the C backend still needs this. 
   -------------------------------------------------------------------------- */
#define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)

#endif /* PRIMOPS_H */
