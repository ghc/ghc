/* -----------------------------------------------------------------------------
 * $Id: PrimOps.h,v 1.14 1999/02/02 14:19:49 simonm Exp $
 *
 * Macros for primitive operations in STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRIMOPS_H
#define PRIMOPS_H

/* -----------------------------------------------------------------------------
   Comparison PrimOps.
   -------------------------------------------------------------------------- */

#define gtCharzh(r,a,b)	r=(I_)((a)> (b))
#define geCharzh(r,a,b)	r=(I_)((a)>=(b))
#define eqCharzh(r,a,b)	r=(I_)((a)==(b))
#define neCharzh(r,a,b)	r=(I_)((a)!=(b))
#define ltCharzh(r,a,b)	r=(I_)((a)< (b))
#define leCharzh(r,a,b)	r=(I_)((a)<=(b))

/* Int comparisons: >#, >=# etc */
#define zgzh(r,a,b)	r=(I_)((I_)(a) >(I_)(b))
#define zgzezh(r,a,b)	r=(I_)((I_)(a)>=(I_)(b))
#define zezezh(r,a,b)	r=(I_)((I_)(a)==(I_)(b))
#define zszezh(r,a,b)	r=(I_)((I_)(a)!=(I_)(b))
#define zlzh(r,a,b)	r=(I_)((I_)(a) <(I_)(b))
#define zlzezh(r,a,b)	r=(I_)((I_)(a)<=(I_)(b))

#define gtWordzh(r,a,b)	r=(I_)((W_)(a) >(W_)(b))
#define geWordzh(r,a,b)	r=(I_)((W_)(a)>=(W_)(b))
#define eqWordzh(r,a,b)	r=(I_)((W_)(a)==(W_)(b))
#define neWordzh(r,a,b)	r=(I_)((W_)(a)!=(W_)(b))
#define ltWordzh(r,a,b)	r=(I_)((W_)(a) <(W_)(b))
#define leWordzh(r,a,b)	r=(I_)((W_)(a)<=(W_)(b))

#define gtAddrzh(r,a,b)	r=(I_)((a) >(b))
#define geAddrzh(r,a,b)	r=(I_)((a)>=(b))
#define eqAddrzh(r,a,b)	r=(I_)((a)==(b))
#define neAddrzh(r,a,b)	r=(I_)((a)!=(b))
#define ltAddrzh(r,a,b)	r=(I_)((a) <(b))
#define leAddrzh(r,a,b)	r=(I_)((a)<=(b))

#define gtFloatzh(r,a,b)  r=(I_)((a)> (b))
#define geFloatzh(r,a,b)  r=(I_)((a)>=(b))
#define eqFloatzh(r,a,b)  r=(I_)((a)==(b))
#define neFloatzh(r,a,b)  r=(I_)((a)!=(b))
#define ltFloatzh(r,a,b)  r=(I_)((a)< (b))
#define leFloatzh(r,a,b)  r=(I_)((a)<=(b))

/* Double comparisons: >##, >=#@ etc */
#define zgzhzh(r,a,b)	r=(I_)((a) >(b))
#define zgzezhzh(r,a,b)	r=(I_)((a)>=(b))
#define zezezhzh(r,a,b)	r=(I_)((a)==(b))
#define zszezhzh(r,a,b)	r=(I_)((a)!=(b))
#define zlzhzh(r,a,b)	r=(I_)((a) <(b))
#define zlzezhzh(r,a,b)	r=(I_)((a)<=(b))

/*  used by returning comparison primops, defined in Prims.hc. */
extern const StgClosure *PrelBase_Bool_closure_tbl[];

/* -----------------------------------------------------------------------------
   Char# PrimOps.
   -------------------------------------------------------------------------- */

#define ordzh(r,a)	r=(I_)((W_) (a))
#define chrzh(r,a)	r=(StgChar)((W_)(a))

/* -----------------------------------------------------------------------------
   Int# PrimOps.
   -------------------------------------------------------------------------- */

I_ stg_div (I_ a, I_ b);

#define zpzh(r,a,b)		r=(a)+(b)
#define zmzh(r,a,b)		r=(a)-(b)
#define ztzh(r,a,b)		r=(a)*(b)
#define quotIntzh(r,a,b)	r=(a)/(b)
#define zszh(r,a,b)		r=ULTRASAFESTGCALL2(I_,(void *, I_, I_),stg_div,(a),(b))
#define remIntzh(r,a,b)		r=(a)%(b)
#define negateIntzh(r,a)	r=-(a)

/* The following operations are the standard add,subtract and multiply
 * except that they return a carry if the operation overflows.
 *
 * They are all defined in terms of 32-bit integers and use the GCC
 * 'long long' extension to get a 64-bit result.  We'd like to use
 * 64-bit integers on 64-bit architectures, but it seems that gcc's
 * 'long long' type is set at 64-bits even on a 64-bit machine.  
 */

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

#define addWithCarryzh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a + b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}


#define subWithCarryzh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a + b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}

#define mulWithCarryzh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a * b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}

/* -----------------------------------------------------------------------------
   Word PrimOps.
   -------------------------------------------------------------------------- */

#define quotWordzh(r,a,b)	r=((W_)a)/((W_)b)
#define remWordzh(r,a,b)	r=((W_)a)%((W_)b)

#define andzh(r,a,b)		r=(a)&(b)
#define orzh(r,a,b)		r=(a)|(b)
#define xorzh(r,a,b)            r=(a)^(b)
#define notzh(r,a)		r=~(a)

#define shiftLzh(r,a,b)	  	r=(a)<<(b)
#define shiftRLzh(r,a,b)  	r=(a)>>(b)
#define iShiftLzh(r,a,b)  	r=(a)<<(b)
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix/document. -- sof 8/98
*/
#define iShiftRAzh(r,a,b) 	r=(a)>>(b)
#define iShiftRLzh(r,a,b) 	r=(a)>>(b)

#define int2Wordzh(r,a) 	r=(W_)(a)
#define word2Intzh(r,a) 	r=(I_)(a)

/* -----------------------------------------------------------------------------
   Addr PrimOps.
   -------------------------------------------------------------------------- */

#define int2Addrzh(r,a) 	r=(A_)(a)
#define addr2Intzh(r,a) 	r=(I_)(a)

#define indexCharOffAddrzh(r,a,i)   r= ((C_ *)(a))[i]
#define indexIntOffAddrzh(r,a,i)    r= ((I_ *)(a))[i]
#define indexAddrOffAddrzh(r,a,i)   r= ((PP_)(a))[i]
#define indexFloatOffAddrzh(r,a,i)  r= PK_FLT((P_) (((StgFloat *)(a)) + i))
#define indexDoubleOffAddrzh(r,a,i) r= PK_DBL((P_) (((StgDouble *)(a)) + i))
#define indexStablePtrOffAddrzh(r,a,i)    r= ((StgStablePtr *)(a))[i]
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffAddrzh(r,a,i)  r= ((LI_ *)(a))[i]
#define indexWord64OffAddrzh(r,a,i) r= ((LW_ *)(a))[i]
#endif

#define writeCharOffAddrzh(a,i,v)       ((C_ *)(a))[i] = (v)
#define writeIntOffAddrzh(a,i,v)        ((I_ *)(a))[i] = (v)
#define writeWordOffAddrzh(a,i,v)       ((W_ *)(a))[i] = (v)
#define writeAddrOffAddrzh(a,i,v)       ((PP_)(a))[i] = (v)
#define writeForeignObjOffAddrzh(a,i,v) ((PP_)(a))[i] = ForeignObj_CLOSURE_DATA(v)
#define writeFloatOffAddrzh(a,i,v)      ASSIGN_FLT((P_) (((StgFloat *)(a)) + i),v)
#define writeDoubleOffAddrzh(a,i,v)     ASSIGN_DBL((P_) (((StgDouble *)(a)) + i),v)
#define writeStablePtrOffAddrzh(a,i,v)  ((StgStablePtr *)(a))[i] = (v)
#ifdef SUPPORT_LONG_LONGS
#define writeInt64OffAddrzh(a,i,v)   ((LI_ *)(a))[i] = (v)
#define writeWord64OffAddrzh(a,i,v)  ((LW_ *)(a))[i] = (v)
#endif

/* -----------------------------------------------------------------------------
   Float PrimOps.
   -------------------------------------------------------------------------- */

#define plusFloatzh(r,a,b)   r=(a)+(b)
#define minusFloatzh(r,a,b)  r=(a)-(b)
#define timesFloatzh(r,a,b)  r=(a)*(b)
#define divideFloatzh(r,a,b) r=(a)/(b)
#define negateFloatzh(r,a)   r=-(a)
			     
#define int2Floatzh(r,a)     r=(StgFloat)(a)
#define float2Intzh(r,a)     r=(I_)(a)
			     
#define expFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,exp,a)
#define logFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,log,a)
#define sqrtFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sqrt,a)
#define sinFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sin,a)
#define cosFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cos,a)
#define tanFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tan,a)
#define asinFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,asin,a)
#define acosFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,acos,a)
#define atanFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,atan,a)
#define sinhFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sinh,a)
#define coshFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cosh,a)
#define tanhFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tanh,a)
#define powerFloatzh(r,a,b)  r=(StgFloat) RET_PRIM_STGCALL2(StgDouble,pow,a,b)

/* -----------------------------------------------------------------------------
   Double PrimOps.
   -------------------------------------------------------------------------- */

#define zpzhzh(r,a,b)	     r=(a)+(b)
#define zmzhzh(r,a,b)	     r=(a)-(b)
#define ztzhzh(r,a,b)	     r=(a)*(b)
#define zszhzh(r,a,b)	     r=(a)/(b)
#define negateDoublezh(r,a)  r=-(a)
			     
#define int2Doublezh(r,a)    r=(StgDouble)(a)
#define double2Intzh(r,a)    r=(I_)(a)
			     
#define float2Doublezh(r,a)  r=(StgDouble)(a)
#define double2Floatzh(r,a)  r=(StgFloat)(a)
			     
#define expDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,exp,a)
#define logDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,log,a)
#define sqrtDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sqrt,a)
#define sinDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sin,a)
#define cosDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cos,a)
#define tanDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tan,a)
#define asinDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,asin,a)
#define acosDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,acos,a)
#define atanDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,atan,a)
#define sinhDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sinh,a)
#define coshDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cosh,a)
#define tanhDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tanh,a)
/* Power: **## */
#define ztztzhzh(r,a,b)	r=(StgDouble) RET_PRIM_STGCALL2(StgDouble,pow,a,b)

/* -----------------------------------------------------------------------------
   Integer PrimOps.
   -------------------------------------------------------------------------- */

/* We can do integer2Int and cmpInteger inline, since they don't need
 * to allocate any memory.
 */

#define integer2Intzh(r, aa,sa,da)					\
{ MP_INT arg;								\
									\
  arg._mp_alloc	= (aa);							\
  arg._mp_size	= (sa);							\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da));		\
									\
  (r) = RET_PRIM_STGCALL1(I_,mpz_get_si,&arg);				\
}

#define integer2Wordzh(r, aa,sa,da)					\
{ MP_INT arg;								\
									\
  arg._mp_alloc	= (aa);							\
  arg._mp_size	= (sa);							\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da));		\
									\
  (r) = RET_PRIM_STGCALL1(I_,mpz_get_ui,&arg);				\
}

#define cmpIntegerzh(r, a1,s1,d1, a2,s2,d2)				\
{ MP_INT arg1;								\
  MP_INT arg2;								\
									\
  arg1._mp_alloc= (a1);							\
  arg1._mp_size	= (s1);							\
  arg1._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(d1));		\
  arg2._mp_alloc= (a2);							\
  arg2._mp_size	= (s2);							\
  arg2._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(d2));		\
									\
  (r) = RET_PRIM_STGCALL2(I_,mpz_cmp,&arg1,&arg2);			\
}

/* A glorious hack: calling mpz_neg would entail allocation and
 * copying, but by looking at what mpz_neg actually does, we can
 * derive a better version:
 */

#define negateIntegerzh(ra, rs, rd, a, s, d)				\
{ 									\
  (ra) = (a);								\
  (rs) = -(s);								\
  (rd) = d;								\
}

/* The rest are all out-of-line: -------- */

/* Integer arithmetic */
EF_(plusIntegerzh_fast);
EF_(minusIntegerzh_fast);
EF_(timesIntegerzh_fast);
EF_(gcdIntegerzh_fast);
EF_(quotRemIntegerzh_fast);
EF_(divModIntegerzh_fast);

/* Conversions */
EF_(int2Integerzh_fast);
EF_(word2Integerzh_fast);
EF_(addr2Integerzh_fast);

/* Floating-point encodings/decodings */
EF_(encodeFloatzh_fast);
EF_(decodeFloatzh_fast);

EF_(encodeDoublezh_fast);
EF_(decodeDoublezh_fast);

/* -----------------------------------------------------------------------------
   Word64 PrimOps.
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

#define integerToWord64zh(r, aa,sa,da)					\
{ unsigned long int* d; 						\
  StgNat64 res;								\
									\
  d		= (unsigned long int *) (BYTE_ARR_CTS(da));		\
  if ( (aa) == 0 ) {							\
     res = (LW_)0;							\
  } else if ( (aa) == 1) {						\
     res = (LW_)d[0];							\
  } else {								\
     res = (LW_)d[0] + (LW_)d[1] * 0x100000000ULL;			\
  }									\
  (r) = res;								\
}

#define integerToInt64zh(r, aa,sa,da)					\
{ unsigned long int* d; 						\
  StgInt64 res;								\
									\
  d		= (unsigned long int *) (BYTE_ARR_CTS(da));		\
  if ( (aa) == 0 ) {							\
     res = (LI_)0;							\
  } else if ( (aa) == 1) {						\
     res = (LI_)d[0];							\
  } else {								\
     res = (LI_)d[0] + (LI_)d[1] * 0x100000000LL;			\
     if ( sa < 0 ) {                                                    \
	   res = (LI_)-res;                                             \
     }                                                                  \
  }									\
  (r) = res;						                \
}

/* Conversions */
EF_(int64ToIntegerzh_fast);
EF_(word64ToIntegerzh_fast);

/* The rest are (way!) out of line, implemented via C entry points.
 */
I_ stg_gtWord64 (StgNat64, StgNat64);
I_ stg_geWord64 (StgNat64, StgNat64);
I_ stg_eqWord64 (StgNat64, StgNat64);
I_ stg_neWord64 (StgNat64, StgNat64);
I_ stg_ltWord64 (StgNat64, StgNat64);
I_ stg_leWord64 (StgNat64, StgNat64);

I_ stg_gtInt64 (StgInt64, StgInt64);
I_ stg_geInt64 (StgInt64, StgInt64);
I_ stg_eqInt64 (StgInt64, StgInt64);
I_ stg_neInt64 (StgInt64, StgInt64);
I_ stg_ltInt64 (StgInt64, StgInt64);
I_ stg_leInt64 (StgInt64, StgInt64);

LW_ stg_remWord64  (StgNat64, StgNat64);
LW_ stg_quotWord64 (StgNat64, StgNat64);

LI_ stg_remInt64    (StgInt64, StgInt64);
LI_ stg_quotInt64   (StgInt64, StgInt64);
LI_ stg_negateInt64 (StgInt64);
LI_ stg_plusInt64   (StgInt64, StgInt64);
LI_ stg_minusInt64  (StgInt64, StgInt64);
LI_ stg_timesInt64  (StgInt64, StgInt64);

LW_ stg_and64  (StgNat64, StgNat64);
LW_ stg_or64   (StgNat64, StgNat64);
LW_ stg_xor64  (StgNat64, StgNat64);
LW_ stg_not64  (StgNat64);

LW_ stg_shiftL64   (StgNat64, StgInt);
LW_ stg_shiftRL64  (StgNat64, StgInt);
LI_ stg_iShiftL64  (StgInt64, StgInt);
LI_ stg_iShiftRL64 (StgInt64, StgInt);
LI_ stg_iShiftRA64 (StgInt64, StgInt);

LI_ stg_intToInt64    (StgInt);
I_ stg_int64ToInt     (StgInt64);
LW_ stg_int64ToWord64 (StgInt64);

LW_ stg_wordToWord64  (StgWord);
W_  stg_word64ToWord  (StgNat64);
LI_ stg_word64ToInt64 (StgNat64);
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
 ({ ASSERT((GET_INFO(a) == &ARR_WORDS_info) 	  \
        || (GET_INFO(a) == &MUT_ARR_WORDS_info)); \
    REAL_BYTE_ARR_CTS(a); })
#define PTRS_ARR_CTS(a)				  \
 ({ ASSERT((GET_INFO(a) == &ARR_PTRS_info)	  \
	|| (GET_INFO(a) == &MUT_ARR_PTRS_info));  \
    REAL_PTRS_ARR_CTS(a); })
#else
#define BYTE_ARR_CTS(a)		REAL_BYTE_ARR_CTS(a)
#define PTRS_ARR_CTS(a)		REAL_PTRS_ARR_CTS(a)
#endif

extern I_ genSymZh(void);
extern I_ resetGenSymZh(void);

/*--- everything except new*Array is done inline: */

#define sameMutableArrayzh(r,a,b)	r=(I_)((a)==(b))
#define sameMutableByteArrayzh(r,a,b)	r=(I_)((a)==(b))

#define readArrayzh(r,a,i)	 r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define readCharArrayzh(r,a,i)	 indexCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readIntArrayzh(r,a,i)	 indexIntOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWordArrayzh(r,a,i)	 indexWordOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readAddrArrayzh(r,a,i)	 indexAddrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readFloatArrayzh(r,a,i)	 indexFloatOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readDoubleArrayzh(r,a,i) indexDoubleOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readStablePtrArrayzh(r,a,i) indexStablePtrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#ifdef SUPPORT_LONG_LONGS
#define readInt64Arrayzh(r,a,i)  indexInt64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWord64Arrayzh(r,a,i) indexWord64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#endif

/* result ("r") arg ignored in write macros! */
#define writeArrayzh(a,i,v)	((PP_) PTRS_ARR_CTS(a))[(i)]=(v)

#define writeCharArrayzh(a,i,v)	  ((C_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeIntArrayzh(a,i,v)	  ((I_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeWordArrayzh(a,i,v)	  ((W_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeAddrArrayzh(a,i,v)	  ((PP_)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeFloatArrayzh(a,i,v)  \
	ASSIGN_FLT((P_) (((StgFloat *)(BYTE_ARR_CTS(a))) + i),v)
#define writeDoubleArrayzh(a,i,v) \
	ASSIGN_DBL((P_) (((StgDouble *)(BYTE_ARR_CTS(a))) + i),v)
#define writeStablePtrArrayzh(a,i,v)	  ((StgStablePtr *)(BYTE_ARR_CTS(a)))[i] = (v)
#ifdef SUPPORT_LONG_LONGS
#define writeInt64Arrayzh(a,i,v)  ((LI_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeWord64Arrayzh(a,i,v) ((LW_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#endif

#define indexArrayzh(r,a,i)	  r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define indexCharArrayzh(r,a,i)	  indexCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexIntArrayzh(r,a,i)	  indexIntOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWordArrayzh(r,a,i)	  indexWordOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexAddrArrayzh(r,a,i)	  indexAddrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexFloatArrayzh(r,a,i)  indexFloatOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexDoubleArrayzh(r,a,i) indexDoubleOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexStablePtrArrayzh(r,a,i) indexStablePtrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#ifdef SUPPORT_LONG_LONGS
#define indexInt64Arrayzh(r,a,i)  indexInt64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWord64Arrayzh(r,a,i) indexWord64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#endif

#define indexCharOffForeignObjzh(r,fo,i)   indexCharOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexIntOffForeignObjzh(r,fo,i)    indexIntOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWordOffForeignObjzh(r,fo,i)   indexWordOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexAddrOffForeignObjzh(r,fo,i)   indexAddrOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexFloatOffForeignObjzh(r,fo,i)  indexFloatOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexDoubleOffForeignObjzh(r,fo,i) indexDoubleOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexStablePtrOffForeignObjzh(r,fo,i)  indexStablePtrOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffForeignObjzh(r,fo,i)  indexInt64OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord64OffForeignObjzh(r,fo,i) indexWord64OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#endif

#define indexCharOffAddrzh(r,a,i)   r= ((C_ *)(a))[i]
#define indexIntOffAddrzh(r,a,i)    r= ((I_ *)(a))[i]
#define indexWordOffAddrzh(r,a,i)   r= ((W_ *)(a))[i]
#define indexAddrOffAddrzh(r,a,i)   r= ((PP_)(a))[i]
#define indexFloatOffAddrzh(r,a,i)  r= PK_FLT((P_) (((StgFloat *)(a)) + i))
#define indexDoubleOffAddrzh(r,a,i) r= PK_DBL((P_) (((StgDouble *)(a)) + i))
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffAddrzh(r,a,i)  r= ((LI_ *)(a))[i]
#define indexWord64OffAddrzh(r,a,i) r= ((LW_ *)(a))[i]
#endif

/* Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 */

#define unsafeFreezzeArrayzh(r,a)					\
	{								\
        SET_INFO((StgClosure *)a,&MUT_ARR_PTRS_FROZEN_info);            \
	r = a;								\
	}

#define unsafeFreezzeByteArrayzh(r,a)	r=(a)

#define sizzeofByteArrayzh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))
#define sizzeofMutableByteArrayzh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))

/* and the out-of-line ones... */

EF_(newCharArrayzh_fast);
EF_(newIntArrayzh_fast);
EF_(newWordArrayzh_fast);
EF_(newAddrArrayzh_fast);
EF_(newFloatArrayzh_fast);
EF_(newDoubleArrayzh_fast);
EF_(newStablePtrArrayzh_fast);
EF_(newArrayzh_fast);

/* encoding and decoding of floats/doubles. */

/* We only support IEEE floating point format */
#include "ieee-flpt.h"

#if FLOATS_AS_DOUBLES  /* i.e. 64-bit machines */
#define encodeFloatzh(r, aa,sa,da, expon)   encodeDoublezh(r, aa,sa,da, expon)
#else
#define encodeFloatzh(r, aa,sa,da, expon)	\
{ MP_INT arg;					\
  /* Does not allocate memory */		\
						\
  arg._mp_alloc	= aa;				\
  arg._mp_size	= sa;				\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da)); \
						\
  r = RET_PRIM_STGCALL2(StgFloat, __encodeFloat,&arg,(expon));\
}
#endif /* FLOATS_AS_DOUBLES */

#define encodeDoublezh(r, aa,sa,da, expon)	\
{ MP_INT arg;					\
  /* Does not allocate memory */		\
						\
  arg._mp_alloc	= aa;				\
  arg._mp_size	= sa;				\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da)); \
						\
  r = RET_PRIM_STGCALL2(StgDouble, __encodeDouble,&arg,(expon));\
}

/* The decode operations are out-of-line because they need to allocate
 * a byte array.
 */
 
#ifdef FLOATS_AS_DOUBLES
#define decodeFloatzh_fast decodeDoublezh_fast
#else
EF_(decodeFloatzh_fast);
#endif

EF_(decodeDoublezh_fast);

/* grimy low-level support functions defined in StgPrimFloat.c */

extern StgDouble __encodeDouble (MP_INT *s, I_ e);
extern StgFloat  __encodeFloat  (MP_INT *s, I_ e);
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

EF_(newMutVarzh_fast);

#define readMutVarzh(r,a)	 r=(P_)(((StgMutVar *)(a))->var)
#define writeMutVarzh(a,v)       (P_)(((StgMutVar *)(a))->var)=(v)
#define sameMutVarzh(r,a,b)      r=(I_)((a)==(b))

/* -----------------------------------------------------------------------------
   MVar PrimOps.

   All out of line, because they either allocate or may block.
   -------------------------------------------------------------------------- */
#define sameMVarzh(r,a,b)        r=(I_)((a)==(b))

/* Assume external decl of EMPTY_MVAR_info is in scope by now */
#define isEmptyMVarzh(r,a)       r=(I_)((GET_INFO((StgMVar*)(a))) == &EMPTY_MVAR_info )
EF_(newMVarzh_fast);
EF_(takeMVarzh_fast);
EF_(putMVarzh_fast);


/* -----------------------------------------------------------------------------
   Delay/Wait PrimOps
   -------------------------------------------------------------------------- */

/* Hmm, I'll think about these later. */

/* -----------------------------------------------------------------------------
   Primitive I/O, error-handling PrimOps
   -------------------------------------------------------------------------- */

EF_(catchzh_fast);
EF_(raisezh_fast);

extern void stg_exit(I_ n)  __attribute__ ((noreturn));

/* -----------------------------------------------------------------------------
   Stable Name / Stable Pointer  PrimOps
   -------------------------------------------------------------------------- */

#ifndef PAR

EF_(makeStableNamezh_fast);

#define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)

#define eqStableNamezh(r,sn1,sn2)					\
    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))

#define makeStablePtrzh(r,a) \
   r = RET_STGCALL1(StgStablePtr,getStablePtr,a)

#define deRefStablePtrzh(r,sp) do {		\
  ASSERT(stable_ptr_table[sp & ~STABLEPTR_WEIGHT_MASK].weight > 0);	\
  r = stable_ptr_table[sp & ~STABLEPTR_WEIGHT_MASK].addr; \
} while (0);

#define eqStablePtrzh(r,sp1,sp2) \
    (r = ((sp1 & ~STABLEPTR_WEIGHT_MASK) == (sp2 & ~STABLEPTR_WEIGHT_MASK)))

#endif

/* -----------------------------------------------------------------------------
   Parallel PrimOps.
   -------------------------------------------------------------------------- */

EF_(forkzh_fast);
EF_(killThreadzh_fast);
EF_(seqzh_fast);

/* Hmm, I'll think about these later. */
/* -----------------------------------------------------------------------------
   Pointer equality
   -------------------------------------------------------------------------- */

/* warning: extremely non-referentially transparent, need to hide in
   an appropriate monad.

   ToDo: follow indirections.  
*/

#define reallyUnsafePtrEqualityzh(r,a,b) r=((StgPtr)(a) == (StgPtr)(b))

/* -----------------------------------------------------------------------------
   Weak Pointer PrimOps.
   -------------------------------------------------------------------------- */

#ifndef PAR

EF_(mkWeakzh_fast);
EF_(finaliseWeakzh_fast);

#define deRefWeakzh(code,val,w)				\
  if (((StgWeak *)w)->header.info == &WEAK_info) {	\
	code = 1;					\
	val = (P_)((StgWeak *)w)->value;		\
  } else {						\
	code = 0;					\
	val = (P_)w;					\
  }

#define sameWeakzh(w1,w2)  ((w1)==(w2))

#endif

/* -----------------------------------------------------------------------------
   Foreign Object PrimOps.
   -------------------------------------------------------------------------- */

#ifndef PAR

#define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)

EF_(makeForeignObjzh_fast);

#define writeForeignObjzh(res,datum) \
   (ForeignObj_CLOSURE_DATA(res) = (P_)(datum))

#define eqForeignObj(f1,f2)  ((f1)==(f2))

#endif

/* -----------------------------------------------------------------------------
   Signal processing.  Not really primops, but called directly from
   Haskell. 
   -------------------------------------------------------------------------- */

#define STG_SIG_DFL  (-1)
#define STG_SIG_IGN  (-2)
#define STG_SIG_ERR  (-3)
#define STG_SIG_HAN  (-4)

extern StgInt sig_install (StgInt, StgInt, StgStablePtr, sigset_t *);
#define stg_sig_default(sig,mask) sig_install(sig,STG_SIG_DFL,0,(sigset_t *)mask)
#define stg_sig_ignore(sig,mask) sig_install(sig,STG_SIG_IGN,0,(sigset_t *)mask)
#define stg_sig_catch(sig,ptr,mask) sig_install(sig,STG_SIG_HAN,ptr,(sigset_t *)mask)

#endif PRIMOPS_H
