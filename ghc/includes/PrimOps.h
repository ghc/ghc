/* -----------------------------------------------------------------------------
 * $Id: PrimOps.h,v 1.9 1999/01/23 17:48:23 sof Exp $
 *
 * Macros for primitive operations in STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRIMOPS_H
#define PRIMOPS_H

/* -----------------------------------------------------------------------------
   Comparison PrimOps.
   -------------------------------------------------------------------------- */

#define gtCharZh(r,a,b)	r=(I_)((a)> (b))
#define geCharZh(r,a,b)	r=(I_)((a)>=(b))
#define eqCharZh(r,a,b)	r=(I_)((a)==(b))
#define neCharZh(r,a,b)	r=(I_)((a)!=(b))
#define ltCharZh(r,a,b)	r=(I_)((a)< (b))
#define leCharZh(r,a,b)	r=(I_)((a)<=(b))

/* Int comparisons: >#, >=# etc */
#define ZgZh(r,a,b)	r=(I_)((I_)(a) >(I_)(b))
#define ZgZeZh(r,a,b)	r=(I_)((I_)(a)>=(I_)(b))
#define ZeZeZh(r,a,b)	r=(I_)((I_)(a)==(I_)(b))
#define ZdZeZh(r,a,b)	r=(I_)((I_)(a)!=(I_)(b))
#define ZlZh(r,a,b)	r=(I_)((I_)(a) <(I_)(b))
#define ZlZeZh(r,a,b)	r=(I_)((I_)(a)<=(I_)(b))

#define gtWordZh(r,a,b)	r=(I_)((W_)(a) >(W_)(b))
#define geWordZh(r,a,b)	r=(I_)((W_)(a)>=(W_)(b))
#define eqWordZh(r,a,b)	r=(I_)((W_)(a)==(W_)(b))
#define neWordZh(r,a,b)	r=(I_)((W_)(a)!=(W_)(b))
#define ltWordZh(r,a,b)	r=(I_)((W_)(a) <(W_)(b))
#define leWordZh(r,a,b)	r=(I_)((W_)(a)<=(W_)(b))

#define gtAddrZh(r,a,b)	r=(I_)((a) >(b))
#define geAddrZh(r,a,b)	r=(I_)((a)>=(b))
#define eqAddrZh(r,a,b)	r=(I_)((a)==(b))
#define neAddrZh(r,a,b)	r=(I_)((a)!=(b))
#define ltAddrZh(r,a,b)	r=(I_)((a) <(b))
#define leAddrZh(r,a,b)	r=(I_)((a)<=(b))

#define gtFloatZh(r,a,b)  r=(I_)((a)> (b))
#define geFloatZh(r,a,b)  r=(I_)((a)>=(b))
#define eqFloatZh(r,a,b)  r=(I_)((a)==(b))
#define neFloatZh(r,a,b)  r=(I_)((a)!=(b))
#define ltFloatZh(r,a,b)  r=(I_)((a)< (b))
#define leFloatZh(r,a,b)  r=(I_)((a)<=(b))

/* Double comparisons: >##, >=#@ etc */
#define ZgZhZh(r,a,b)	r=(I_)((a) >(b))
#define ZgZeZhZh(r,a,b)	r=(I_)((a)>=(b))
#define ZeZeZhZh(r,a,b)	r=(I_)((a)==(b))
#define ZdZeZhZh(r,a,b)	r=(I_)((a)!=(b))
#define ZlZhZh(r,a,b)	r=(I_)((a) <(b))
#define ZlZeZhZh(r,a,b)	r=(I_)((a)<=(b))

/*  used by returning comparison primops, defined in Prims.hc. */
extern const StgClosure *PrelBase_Bool_closure_tbl[];

/* -----------------------------------------------------------------------------
   Char# PrimOps.
   -------------------------------------------------------------------------- */

#define ordZh(r,a)	r=(I_)((W_) (a))
#define chrZh(r,a)	r=(StgChar)((W_)(a))

/* -----------------------------------------------------------------------------
   Int# PrimOps.
   -------------------------------------------------------------------------- */

I_ stg_div (I_ a, I_ b);

#define ZpZh(r,a,b)		r=(a)+(b)
#define ZmZh(r,a,b)		r=(a)-(b)
#define ZtZh(r,a,b)		r=(a)*(b)
#define quotIntZh(r,a,b)	r=(a)/(b)
#define ZdZh(r,a,b)		r=ULTRASAFESTGCALL2(I_,(void *, I_, I_),stg_div,(a),(b))
#define remIntZh(r,a,b)		r=(a)%(b)
#define negateIntZh(r,a)	r=-(a)

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

#define addWithCarryZh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a + b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}



#define subWithCarryZh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a + b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}

#define mulWithCarryZh(r,c,a,b)			\
{ long_long_u z;				\
  z.l = a * b;					\
  r = z.i[R];					\
  c = z.i[C];					\
}

/* -----------------------------------------------------------------------------
   Word PrimOps.
   -------------------------------------------------------------------------- */

#define quotWordZh(r,a,b)	r=((W_)a)/((W_)b)
#define remWordZh(r,a,b)	r=((W_)a)%((W_)b)

#define andZh(r,a,b)		r=(a)&(b)
#define orZh(r,a,b)		r=(a)|(b)
#define xorZh(r,a,b)            r=(a)^(b)
#define notZh(r,a)		r=~(a)

#define shiftLZh(r,a,b)	  	r=(a)<<(b)
#define shiftRLZh(r,a,b)  	r=(a)>>(b)
#define iShiftLZh(r,a,b)  	r=(a)<<(b)
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix/document. -- sof 8/98
*/
#define iShiftRAZh(r,a,b) 	r=(a)>>(b)
#define iShiftRLZh(r,a,b) 	r=(a)>>(b)

#define int2WordZh(r,a) 	r=(W_)(a)
#define word2IntZh(r,a) 	r=(I_)(a)

/* -----------------------------------------------------------------------------
   Addr PrimOps.
   -------------------------------------------------------------------------- */

#define int2AddrZh(r,a) 	r=(A_)(a)
#define addr2IntZh(r,a) 	r=(I_)(a)

#define indexCharOffAddrZh(r,a,i)   r= ((C_ *)(a))[i]
#define indexIntOffAddrZh(r,a,i)    r= ((I_ *)(a))[i]
#define indexAddrOffAddrZh(r,a,i)   r= ((PP_)(a))[i]
#define indexFloatOffAddrZh(r,a,i)  r= PK_FLT((P_) (((StgFloat *)(a)) + i))
#define indexDoubleOffAddrZh(r,a,i) r= PK_DBL((P_) (((StgDouble *)(a)) + i))
#define indexStablePtrOffAddrZh(r,a,i)    r= ((StgStablePtr *)(a))[i]
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffAddrZh(r,a,i)  r= ((LI_ *)(a))[i]
#define indexWord64OffAddrZh(r,a,i) r= ((LW_ *)(a))[i]
#endif

#define writeCharOffAddrZh(a,i,v)       ((C_ *)(a))[i] = (v)
#define writeIntOffAddrZh(a,i,v)        ((I_ *)(a))[i] = (v)
#define writeWordOffAddrZh(a,i,v)       ((W_ *)(a))[i] = (v)
#define writeAddrOffAddrZh(a,i,v)       ((PP_)(a))[i] = (v)
#define writeForeignObjOffAddrZh(a,i,v) ((PP_)(a))[i] = ForeignObj_CLOSURE_DATA(v)
#define writeFloatOffAddrZh(a,i,v)      ASSIGN_FLT((P_) (((StgFloat *)(a)) + i),v)
#define writeDoubleOffAddrZh(a,i,v)     ASSIGN_DBL((P_) (((StgDouble *)(a)) + i),v)
#define writeStablePtrOffAddrZh(a,i,v)  ((StgStablePtr *)(a))[i] = (v)
#ifdef SUPPORT_LONG_LONGS
#define writeInt64OffAddrZh(a,i,v)   ((LI_ *)(a))[i] = (v)
#define writeWord64OffAddrZh(a,i,v)  ((LW_ *)(a))[i] = (v)
#endif

/* -----------------------------------------------------------------------------
   Float PrimOps.
   -------------------------------------------------------------------------- */

#define plusFloatZh(r,a,b)   r=(a)+(b)
#define minusFloatZh(r,a,b)  r=(a)-(b)
#define timesFloatZh(r,a,b)  r=(a)*(b)
#define divideFloatZh(r,a,b) r=(a)/(b)
#define negateFloatZh(r,a)   r=-(a)
			     
#define int2FloatZh(r,a)     r=(StgFloat)(a)
#define float2IntZh(r,a)     r=(I_)(a)
			     
#define expFloatZh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,exp,a)
#define logFloatZh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,log,a)
#define sqrtFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sqrt,a)
#define sinFloatZh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sin,a)
#define cosFloatZh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cos,a)
#define tanFloatZh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tan,a)
#define asinFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,asin,a)
#define acosFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,acos,a)
#define atanFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,atan,a)
#define sinhFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sinh,a)
#define coshFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cosh,a)
#define tanhFloatZh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tanh,a)
#define powerFloatZh(r,a,b)  r=(StgFloat) RET_PRIM_STGCALL2(StgDouble,pow,a,b)

/* -----------------------------------------------------------------------------
   Double PrimOps.
   -------------------------------------------------------------------------- */

#define ZpZhZh(r,a,b)	     r=(a)+(b)
#define ZmZhZh(r,a,b)	     r=(a)-(b)
#define ZtZhZh(r,a,b)	     r=(a)*(b)
#define ZdZhZh(r,a,b)	     r=(a)/(b)
#define negateDoubleZh(r,a)  r=-(a)
			     
#define int2DoubleZh(r,a)    r=(StgDouble)(a)
#define double2IntZh(r,a)    r=(I_)(a)
			     
#define float2DoubleZh(r,a)  r=(StgDouble)(a)
#define double2FloatZh(r,a)  r=(StgFloat)(a)
			     
#define expDoubleZh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,exp,a)
#define logDoubleZh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,log,a)
#define sqrtDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sqrt,a)
#define sinDoubleZh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sin,a)
#define cosDoubleZh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cos,a)
#define tanDoubleZh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tan,a)
#define asinDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,asin,a)
#define acosDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,acos,a)
#define atanDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,atan,a)
#define sinhDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sinh,a)
#define coshDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cosh,a)
#define tanhDoubleZh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tanh,a)
/* Power: **## */
#define ZtZtZhZh(r,a,b)	r=(StgDouble) RET_PRIM_STGCALL2(StgDouble,pow,a,b)

/* -----------------------------------------------------------------------------
   Integer PrimOps.
   -------------------------------------------------------------------------- */

/* We can do integer2Int and cmpInteger inline, since they don't need
 * to allocate any memory.
 */

#define integer2IntZh(r, aa,sa,da)					\
{ MP_INT arg;								\
									\
  arg._mp_alloc	= (aa);							\
  arg._mp_size	= (sa);							\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da));		\
									\
  (r) = RET_PRIM_STGCALL1(I_,mpz_get_si,&arg);				\
}

#define integer2WordZh(r, aa,sa,da)					\
{ MP_INT arg;								\
									\
  arg._mp_alloc	= (aa);							\
  arg._mp_size	= (sa);							\
  arg._mp_d	= (unsigned long int *) (BYTE_ARR_CTS(da));		\
									\
  (r) = RET_PRIM_STGCALL1(I_,mpz_get_ui,&arg);				\
}

#define cmpIntegerZh(r, a1,s1,d1, a2,s2,d2)				\
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

#define negateIntegerZh(ra, rs, rd, a, s, d)				\
{ 									\
  (ra) = (a);								\
  (rs) = -(s);								\
  (rd) = d;								\
}

/* The rest are all out-of-line: -------- */

/* Integer arithmetic */
EF_(plusIntegerZh_fast);
EF_(minusIntegerZh_fast);
EF_(timesIntegerZh_fast);
EF_(gcdIntegerZh_fast);
EF_(quotRemIntegerZh_fast);
EF_(divModIntegerZh_fast);

/* Conversions */
EF_(int2IntegerZh_fast);
EF_(word2IntegerZh_fast);
EF_(addr2IntegerZh_fast);

/* Floating-point encodings/decodings */
EF_(encodeFloatZh_fast);
EF_(decodeFloatZh_fast);

EF_(encodeDoubleZh_fast);
EF_(decodeDoubleZh_fast);

/* -----------------------------------------------------------------------------
   Word64 PrimOps.
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

#define integerToWord64Zh(r, aa,sa,da)					\
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

#define integerToInt64Zh(r, aa,sa,da)					\
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
EF_(int64ToIntegerZh_fast);
EF_(word64ToIntegerZh_fast);

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

#define sameMutableArrayZh(r,a,b)	r=(I_)((a)==(b))
#define sameMutableByteArrayZh(r,a,b)	r=(I_)((a)==(b))

#define readArrayZh(r,a,i)	 r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define readCharArrayZh(r,a,i)	 indexCharOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readIntArrayZh(r,a,i)	 indexIntOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readWordArrayZh(r,a,i)	 indexWordOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readAddrArrayZh(r,a,i)	 indexAddrOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readFloatArrayZh(r,a,i)	 indexFloatOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readDoubleArrayZh(r,a,i) indexDoubleOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readStablePtrArrayZh(r,a,i) indexStablePtrOffAddrZh(r,BYTE_ARR_CTS(a),i)
#ifdef SUPPORT_LONG_LONGS
#define readInt64ArrayZh(r,a,i)  indexInt64OffAddrZh(r,BYTE_ARR_CTS(a),i)
#define readWord64ArrayZh(r,a,i) indexWord64OffAddrZh(r,BYTE_ARR_CTS(a),i)
#endif

/* result ("r") arg ignored in write macros! */
#define writeArrayZh(a,i,v)	((PP_) PTRS_ARR_CTS(a))[(i)]=(v)

#define writeCharArrayZh(a,i,v)	  ((C_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeIntArrayZh(a,i,v)	  ((I_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeWordArrayZh(a,i,v)	  ((W_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeAddrArrayZh(a,i,v)	  ((PP_)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeFloatArrayZh(a,i,v)  \
	ASSIGN_FLT((P_) (((StgFloat *)(BYTE_ARR_CTS(a))) + i),v)
#define writeDoubleArrayZh(a,i,v) \
	ASSIGN_DBL((P_) (((StgDouble *)(BYTE_ARR_CTS(a))) + i),v)
#define writeStablePtrArrayZh(a,i,v)	  ((StgStablePtr *)(BYTE_ARR_CTS(a)))[i] = (v)
#ifdef SUPPORT_LONG_LONGS
#define writeInt64ArrayZh(a,i,v)  ((LI_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#define writeWord64ArrayZh(a,i,v) ((LW_ *)(BYTE_ARR_CTS(a)))[i] = (v)
#endif

#define indexArrayZh(r,a,i)	  r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define indexCharArrayZh(r,a,i)	  indexCharOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexIntArrayZh(r,a,i)	  indexIntOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexWordArrayZh(r,a,i)	  indexWordOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexAddrArrayZh(r,a,i)	  indexAddrOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexFloatArrayZh(r,a,i)  indexFloatOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexDoubleArrayZh(r,a,i) indexDoubleOffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexStablePtrArrayZh(r,a,i) indexStablePtrOffAddrZh(r,BYTE_ARR_CTS(a),i)
#ifdef SUPPORT_LONG_LONGS
#define indexInt64ArrayZh(r,a,i)  indexInt64OffAddrZh(r,BYTE_ARR_CTS(a),i)
#define indexWord64ArrayZh(r,a,i) indexWord64OffAddrZh(r,BYTE_ARR_CTS(a),i)
#endif

#define indexCharOffForeignObjZh(r,fo,i)   indexCharOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexIntOffForeignObjZh(r,fo,i)    indexIntOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWordOffForeignObjZh(r,fo,i)   indexWordOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexAddrOffForeignObjZh(r,fo,i)   indexAddrOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexFloatOffForeignObjZh(r,fo,i)  indexFloatOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexDoubleOffForeignObjZh(r,fo,i) indexDoubleOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexStablePtrOffForeignObjZh(r,fo,i)  indexStablePtrOffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffForeignObjZh(r,fo,i)  indexInt64OffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord64OffForeignObjZh(r,fo,i) indexWord64OffAddrZh(r,ForeignObj_CLOSURE_DATA(fo),i)
#endif

#define indexCharOffAddrZh(r,a,i)   r= ((C_ *)(a))[i]
#define indexIntOffAddrZh(r,a,i)    r= ((I_ *)(a))[i]
#define indexWordOffAddrZh(r,a,i)   r= ((W_ *)(a))[i]
#define indexAddrOffAddrZh(r,a,i)   r= ((PP_)(a))[i]
#define indexFloatOffAddrZh(r,a,i)  r= PK_FLT((P_) (((StgFloat *)(a)) + i))
#define indexDoubleOffAddrZh(r,a,i) r= PK_DBL((P_) (((StgDouble *)(a)) + i))
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffAddrZh(r,a,i)  r= ((LI_ *)(a))[i]
#define indexWord64OffAddrZh(r,a,i) r= ((LW_ *)(a))[i]
#endif

/* Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 */

#define unsafeFreezeArrayZh(r,a)					\
	{								\
        SET_INFO((StgClosure *)a,&MUT_ARR_PTRS_FROZEN_info);            \
	r = a;								\
	}

#define unsafeFreezeByteArrayZh(r,a)	r=(a)

#define sizeofByteArrayZh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))
#define sizeofMutableByteArrayZh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))

/* and the out-of-line ones... */

EF_(newCharArrayZh_fast);
EF_(newIntArrayZh_fast);
EF_(newWordArrayZh_fast);
EF_(newAddrArrayZh_fast);
EF_(newFloatArrayZh_fast);
EF_(newDoubleArrayZh_fast);
EF_(newStablePtrArrayZh_fast);
EF_(newArrayZh_fast);

/* encoding and decoding of floats/doubles. */

/* We only support IEEE floating point format */
#include "ieee-flpt.h"

#if FLOATS_AS_DOUBLES  /* i.e. 64-bit machines */
#define encodeFloatZh(r, aa,sa,da, expon)   encodeDoubleZh(r, aa,sa,da, expon)
#else
#define encodeFloatZh(r, aa,sa,da, expon)	\
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

#define encodeDoubleZh(r, aa,sa,da, expon)	\
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
#define decodeFloatZh_fast decodeDoubleZh_fast
#else
EF_(decodeFloatZh_fast);
#endif

EF_(decodeDoubleZh_fast);

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

EF_(newMutVarZh_fast);

#define readMutVarZh(r,a)	 r=(P_)(((StgMutVar *)(a))->var)
#define writeMutVarZh(a,v)       (P_)(((StgMutVar *)(a))->var)=(v)
#define sameMutVarZh(r,a,b)      r=(I_)((a)==(b))

/* -----------------------------------------------------------------------------
   MVar PrimOps.

   All out of line, because they either allocate or may block.
   -------------------------------------------------------------------------- */

#define sameMVarZh(r,a,b)        r=(I_)((a)==(b))

/* Assume external decl of EMPTY_MVAR_info is in scope by now */
#define isEmptyMVarZh(r,a)       r=(I_)((GET_INFO((StgMVar*)(a))) == &EMPTY_MVAR_info )
EF_(newMVarZh_fast);
EF_(takeMVarZh_fast);
EF_(putMVarZh_fast);

/* -----------------------------------------------------------------------------
   Delay/Wait PrimOps
   -------------------------------------------------------------------------- */

/* Hmm, I'll think about these later. */

/* -----------------------------------------------------------------------------
   Primitive I/O, error-handling PrimOps
   -------------------------------------------------------------------------- */

EF_(catchZh_fast);
EF_(raiseZh_fast);

extern void stg_exit(I_ n)  __attribute__ ((noreturn));

/* -----------------------------------------------------------------------------
   Stable Pointer PrimOps.
   -------------------------------------------------------------------------- */

#ifndef PAR

extern StgPtr *stable_ptr_table;
extern StgPtr *stable_ptr_free;
#define deRefStablePtrZh(r,sp)   (r=stable_ptr_table[(sp)])
#define eqStablePtrZh(r,sp1,sp2) (r=(sp1==sp2))

#define freeStablePointer(stable_ptr)			\
 {							\
  stable_ptr_table[stable_ptr] = (P_)stable_ptr_free;	\
  stable_ptr_free = &stable_ptr_table[stable_ptr];	\
 }

EF_(makeStablePtrZh_fast);

#else /* PAR */
#define deRefStablePtrZh(ri,sp)					    \
do {								    \
    fflush(stdout);						    \
    fprintf(stderr, "deRefStablePtr#: no stable pointer support.\n");\
    stg_exit(EXIT_FAILURE);					    \
} while(0)

#define eqStablePtrZh(ri,sp1,sp2)				    \
do {								    \
    fflush(stdout);						    \
    fprintf(stderr, "eqStablePtr#: no stable pointer support.\n");  \
    stg_exit(EXIT_FAILURE);					    \
} while(0)

#define makeStablePtrZh(stablePtr,liveness,unstablePtr)		    \
do {								    \
    fflush(stdout);						    \
    fprintf(stderr, "makeStablePtr#: no stable pointer support.\n");\
    EXIT(EXIT_FAILURE);						    \
} while(0)

#define freeStablePtrZh(stablePtr,liveness,unstablePtr)		    \
do {								    \
    fflush(stdout);						    \
    fprintf(stderr, "makeStablePtr#: no stable pointer support.\n");\
    EXIT(EXIT_FAILURE);						    \
} while(0)
#endif


/* -----------------------------------------------------------------------------
   Parallel PrimOps.
   -------------------------------------------------------------------------- */

EF_(forkZh_fast);
EF_(killThreadZh_fast);
EF_(seqZh_fast);

/* Hmm, I'll think about these later. */
/* -----------------------------------------------------------------------------
   Pointer equality
   -------------------------------------------------------------------------- */

/* warning: extremely non-referentially transparent, need to hide in
   an appropriate monad.

   ToDo: follow indirections.  
*/

#define reallyUnsafePtrEqualityZh(r,a,b) r=((StgPtr)(a) == (StgPtr)(b))

/* -----------------------------------------------------------------------------
   Weak Pointer PrimOps.
   -------------------------------------------------------------------------- */

#ifndef PAR

EF_(mkWeakZh_fast);
EF_(deRefWeakZh_fast);
#define sameWeakZh(w1,w2)  ((w1)==(w2))

#endif

/* -----------------------------------------------------------------------------
   Foreign Object PrimOps.
   -------------------------------------------------------------------------- */

#ifndef PAR

#define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)

EF_(makeForeignObjZh_fast);

#define writeForeignObjZh(res,datum) \
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
