/* -----------------------------------------------------------------------------
 * $Id: PrimOps.h,v 1.84 2001/10/27 22:05:48 sof Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Macros for primitive operations in STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRIMOPS_H
#define PRIMOPS_H

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 32
#error GHC C backend requires 32+-bit words
#endif

/* -----------------------------------------------------------------------------
   Helpers for the bytecode linker.             
   -------------------------------------------------------------------------- */

#define addrToHValuezh(r,a) r=(P_)a


/* -----------------------------------------------------------------------------
   Comparison PrimOps.
   -------------------------------------------------------------------------- */

#define gtCharzh(r,a,b)	r=((C_)(a))> ((C_)(b))
#define geCharzh(r,a,b)	r=((C_)(a))>=((C_)(b))
#define eqCharzh(r,a,b)	r=((C_)(a))==((C_)(b))
#define neCharzh(r,a,b)	r=((C_)(a))!=((C_)(b))
#define ltCharzh(r,a,b)	r=((C_)(a))< ((C_)(b))
#define leCharzh(r,a,b)	r=((C_)(a))<=((C_)(b))

/* Int comparisons: >#, >=# etc */
#define zgzh(r,a,b)	r=((I_)(a))> ((I_)(b))
#define zgzezh(r,a,b)	r=((I_)(a))>=((I_)(b))
#define zezezh(r,a,b)	r=((I_)(a))==((I_)(b))
#define zszezh(r,a,b)	r=((I_)(a))!=((I_)(b))
#define zlzh(r,a,b)	r=((I_)(a))< ((I_)(b))
#define zlzezh(r,a,b)	r=((I_)(a))<=((I_)(b))

#define gtWordzh(r,a,b)	r=((W_)(a))> ((W_)(b))
#define geWordzh(r,a,b)	r=((W_)(a))>=((W_)(b))
#define eqWordzh(r,a,b)	r=((W_)(a))==((W_)(b))
#define neWordzh(r,a,b)	r=((W_)(a))!=((W_)(b))
#define ltWordzh(r,a,b)	r=((W_)(a))< ((W_)(b))
#define leWordzh(r,a,b)	r=((W_)(a))<=((W_)(b))

#define gtAddrzh(r,a,b)	r=((A_)(a))> ((A_)(b))
#define geAddrzh(r,a,b)	r=((A_)(a))>=((A_)(b))
#define eqAddrzh(r,a,b)	r=((A_)(a))==((A_)(b))
#define neAddrzh(r,a,b)	r=((A_)(a))!=((A_)(b))
#define ltAddrzh(r,a,b)	r=((A_)(a))< ((A_)(b))
#define leAddrzh(r,a,b)	r=((A_)(a))<=((A_)(b))

#define gtFloatzh(r,a,b)  r=((StgFloat)(a))> ((StgFloat)(b))
#define geFloatzh(r,a,b)  r=((StgFloat)(a))>=((StgFloat)(b))
#define eqFloatzh(r,a,b)  r=((StgFloat)(a))==((StgFloat)(b))
#define neFloatzh(r,a,b)  r=((StgFloat)(a))!=((StgFloat)(b))
#define ltFloatzh(r,a,b)  r=((StgFloat)(a))< ((StgFloat)(b))
#define leFloatzh(r,a,b)  r=((StgFloat)(a))<=((StgFloat)(b))

/* Double comparisons: >##, >=## etc */
#define zgzhzh(r,a,b)	r=((StgDouble)(a))> ((StgDouble)(b))
#define zgzezhzh(r,a,b)	r=((StgDouble)(a))>=((StgDouble)(b))
#define zezezhzh(r,a,b)	r=((StgDouble)(a))==((StgDouble)(b))
#define zszezhzh(r,a,b)	r=((StgDouble)(a))!=((StgDouble)(b))
#define zlzhzh(r,a,b)	r=((StgDouble)(a))< ((StgDouble)(b))
#define zlzezhzh(r,a,b)	r=((StgDouble)(a))<=((StgDouble)(b))

/* -----------------------------------------------------------------------------
   Char# PrimOps.
   -------------------------------------------------------------------------- */

#define ordzh(r,a)	r=(I_)(a)
#define chrzh(r,a)	r=(C_)(a)

/* -----------------------------------------------------------------------------
   Int# PrimOps.
   -------------------------------------------------------------------------- */

#define zpzh(r,a,b)		r=((I_)(a))+((I_)(b))
#define zmzh(r,a,b)		r=((I_)(a))-((I_)(b))
#define ztzh(r,a,b)		r=((I_)(a))*((I_)(b))
#define quotIntzh(r,a,b)	r=((I_)(a))/((I_)(b))
#define remIntzh(r,a,b)		r=((I_)(a))%((I_)(b))
#define negateIntzh(r,a)	r=-((I_)(a))

/* -----------------------------------------------------------------------------
 * Int operations with carry.
 * -------------------------------------------------------------------------- */

/* With some bit-twiddling, we can define int{Add,Sub}Czh portably in
 * C, and without needing any comparisons.  This may not be the
 * fastest way to do it - if you have better code, please send it! --SDM
 *
 * Return : r = a + b,  c = 0 if no overflow, 1 on overflow.
 *
 * We currently don't make use of the r value if c is != 0 (i.e. 
 * overflow), we just convert to big integers and try again.  This
 * could be improved by making r and c the correct values for
 * plugging into a new J#.  
 */
#define addIntCzh(r,c,a,b)					\
{ r = ((I_)(a)) + ((I_)(b));					\
  c = ((StgWord)(~(((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
    >> (BITS_IN (I_) - 1);					\
}


#define subIntCzh(r,c,a,b)					\
{ r = ((I_)(a)) - ((I_)(b));					\
  c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
    >> (BITS_IN (I_) - 1);					\
}

/* Multiply with overflow checking.
 *
 * This is slightly more tricky - the usual sign rules for add/subtract
 * don't apply.  
 *
 * On x86 hardware we use a hand-crafted assembly fragment to do the job.
 *
 * On other 32-bit machines we use gcc's 'long long' types, finding
 * overflow with some careful bit-twiddling.
 *
 * On 64-bit machines where gcc's 'long long' type is also 64-bits,
 * we use a crude approximation, testing whether either operand is
 * larger than 32-bits; if neither is, then we go ahead with the
 * multiplication.
 */

#if i386_TARGET_ARCH

#define mulIntCzh(r,c,a,b)				\
{							\
  __asm__("xorl %1,%1\n\t				\
	   imull %2,%3\n\t				\
	   jno 1f\n\t					\
	   movl $1,%1\n\t				\
	   1:" 						\
	: "=r" (r), "=&r" (c) : "r" (a), "0" (b));	\
}

#elif SIZEOF_VOID_P == 4

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

#define mulIntCzh(r,c,a,b)			\
{						\
  long_long_u z;				\
  z.l = (StgInt64)a * (StgInt64)b;		\
  r = z.i[R];					\
  c = z.i[C];					\
  if (c == 0 || c == -1) {			\
    c = ((StgWord)((a^b) ^ r))			\
      >> (BITS_IN (I_) - 1);			\
  }						\
}
/* Careful: the carry calculation above is extremely delicate.  Make sure
 * you test it thoroughly after changing it.
 */

#else

#define HALF_INT  (((I_)1) << (BITS_IN (I_) / 2))

#define stg_abs(a) (((I_)(a)) < 0 ? -((I_)(a)) : ((I_)(a)))

#define mulIntCzh(r,c,a,b)			\
{						\
  if (stg_abs(a) >= HALF_INT ||			\
      stg_abs(b) >= HALF_INT) {			\
    c = 1;					\
  } else {					\
    r = ((I_)(a)) * ((I_)(b));			\
    c = 0;					\
  }						\
}
#endif

/* -----------------------------------------------------------------------------
   Word# PrimOps.
   -------------------------------------------------------------------------- */

#define plusWordzh(r,a,b)	r=((W_)(a))+((W_)(b))
#define minusWordzh(r,a,b)	r=((W_)(a))-((W_)(b))
#define timesWordzh(r,a,b)	r=((W_)(a))*((W_)(b))
#define quotWordzh(r,a,b)	r=((W_)(a))/((W_)(b))
#define remWordzh(r,a,b)	r=((W_)(a))%((W_)(b))

#define andzh(r,a,b)		r=((W_)(a))&((W_)(b))
#define orzh(r,a,b)		r=((W_)(a))|((W_)(b))
#define xorzh(r,a,b)            r=((W_)(a))^((W_)(b))
#define notzh(r,a)		r=~((W_)(a))

/* The extra tests below properly define the behaviour when shifting
 * by offsets larger than the width of the value being shifted.  Doing
 * so is undefined in C (and in fact gives different answers depending
 * on whether the operation is constant folded or not with gcc on x86!)
 */

#define shiftLzh(r,a,b)	  	r=(((I_)(b)) >= BITS_IN(W_)) ? 0 : ((W_)(a))<<((I_)(b))
#define shiftRLzh(r,a,b)  	r=(((I_)(b)) >= BITS_IN(W_)) ? 0 : ((W_)(a))>>((I_)(b))
#define iShiftLzh(r,a,b)  	r=(((I_)(b)) >= BITS_IN(W_)) ? 0 : ((W_)(a))<<((I_)(b))
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix/document. -- sof 8/98
*/
#define iShiftRAzh(r,a,b) 	r=(((I_)(b)) >= BITS_IN(I_)) ? ((((I_)(a)) < 0) ? -1 : 0) : ((I_)(a))>>((I_)(b))
#define iShiftRLzh(r,a,b) 	r=(((I_)(b)) >= BITS_IN(I_)) ? 0 : (I_)((W_)((I_)(a))>>((I_)(b)))

#define int2Wordzh(r,a) 	r=(W_)((I_)(a))
#define word2Intzh(r,a) 	r=(I_)((W_)(a))

/* -----------------------------------------------------------------------------
   Explicitly sized Int# and Word# PrimOps.
   -------------------------------------------------------------------------- */

#define narrow8Intzh(r,a)	r=(StgInt8)((I_)(a))
#define narrow16Intzh(r,a)	r=(StgInt16)((I_)(a))
#define narrow32Intzh(r,a)	r=(StgInt32)((I_)(a))
#define narrow8Wordzh(r,a)	r=(StgWord8)((W_)(a))
#define narrow16Wordzh(r,a)	r=(StgWord16)((W_)(a))
#define narrow32Wordzh(r,a)	r=(StgWord32)((W_)(a))

/* -----------------------------------------------------------------------------
   Addr# PrimOps.
   -------------------------------------------------------------------------- */

#define nullAddrzh(r,i)         r=(A_)(0)
#define plusAddrzh(r,a,i)       r=((char *)(a)) + (i)
#define minusAddrzh(r,a,b)      r=((char *)(a)) - ((char *)(b))
#define remAddrzh(r,a,i)        r=((W_)(a))%(i)
#define int2Addrzh(r,a) 	r=(A_)(a)
#define addr2Intzh(r,a) 	r=(I_)(a)

#define readCharOffAddrzh(r,a,i)	r=((StgWord8 *)(a))[i]
#define readWideCharOffAddrzh(r,a,i)	r=((C_ *)(a))[i]
#define readIntOffAddrzh(r,a,i)		r=((I_ *)(a))[i]
#define readWordOffAddrzh(r,a,i)	r=((W_ *)(a))[i]
#define readAddrOffAddrzh(r,a,i)	r=((PP_)(a))[i]
#define readFloatOffAddrzh(r,a,i)	r=PK_FLT((P_) (((StgFloat *)(a)) + i))
#define readDoubleOffAddrzh(r,a,i)	r=PK_DBL((P_) (((StgDouble *)(a)) + i))
#define readStablePtrOffAddrzh(r,a,i)	r=((StgStablePtr *)(a))[i]
#define readInt8OffAddrzh(r,a,i)	r=((StgInt8 *)(a))[i]
#define readInt16OffAddrzh(r,a,i)	r=((StgInt16 *)(a))[i]
#define readWord8OffAddrzh(r,a,i)	r=((StgWord8 *)(a))[i]
#define readWord16OffAddrzh(r,a,i)	r=((StgWord16 *)(a))[i]
#define readInt32OffAddrzh(r,a,i)	r=((StgInt32 *)(a))[i]
#define readWord32OffAddrzh(r,a,i)	r=((StgWord32 *)(a))[i]
#ifdef SUPPORT_LONG_LONGS
#define readInt64OffAddrzh(r,a,i)	r=((LI_ *)(a))[i]
#define readWord64OffAddrzh(r,a,i)	r=((LW_ *)(a))[i]
#else
#define readInt64OffAddrzh(r,a,i)	r=((I_ *)(a))[i]
#define readWord64OffAddrzh(r,a,i)	r=((W_ *)(a))[i]
#endif

#define writeCharOffAddrzh(a,i,v)	((StgWord8 *)(a))[i] = (v)
#define writeWideCharOffAddrzh(a,i,v)	((C_ *)(a))[i] = (v)
#define writeIntOffAddrzh(a,i,v)	((I_ *)(a))[i] = (v)
#define writeWordOffAddrzh(a,i,v)	((W_ *)(a))[i] = (v)
#define writeAddrOffAddrzh(a,i,v)	((PP_)(a))[i] = (v)
#define writeForeignObjOffAddrzh(a,i,v) ((PP_)(a))[i] = ForeignObj_CLOSURE_DATA(v)
#define writeFloatOffAddrzh(a,i,v)	ASSIGN_FLT((P_) (((StgFloat *)(a)) + i),v)
#define writeDoubleOffAddrzh(a,i,v)	ASSIGN_DBL((P_) (((StgDouble *)(a)) + i),v)
#define writeStablePtrOffAddrzh(a,i,v)	((StgStablePtr *)(a))[i] = (v)
#define writeInt8OffAddrzh(a,i,v)	((StgInt8 *)(a))[i] = (v)
#define writeInt16OffAddrzh(a,i,v)	((StgInt16 *)(a))[i] = (v)
#define writeInt32OffAddrzh(a,i,v)	((StgInt32 *)(a))[i] = (v)
#define writeWord8OffAddrzh(a,i,v)	((StgWord8 *)(a))[i] = (v)
#define writeWord16OffAddrzh(a,i,v)	((StgWord16 *)(a))[i] = (v)
#define writeWord32OffAddrzh(a,i,v)	((StgWord32 *)(a))[i] = (v)
#ifdef SUPPORT_LONG_LONGS
#define writeInt64OffAddrzh(a,i,v)	((LI_ *)(a))[i] = (v)
#define writeWord64OffAddrzh(a,i,v)	((LW_ *)(a))[i] = (v)
#else
#define writeInt64OffAddrzh(a,i,v)	((I_ *)(a))[i] = (v)
#define writeWord64OffAddrzh(a,i,v)	((W_ *)(a))[i] = (v)
#endif

#define indexCharOffAddrzh(r,a,i)   	r=((StgWord8 *)(a))[i]
#define indexWideCharOffAddrzh(r,a,i)	r=((C_ *)(a))[i]
#define indexIntOffAddrzh(r,a,i)    	r=((I_ *)(a))[i]
#define indexWordOffAddrzh(r,a,i)   	r=((W_ *)(a))[i]
#define indexAddrOffAddrzh(r,a,i)   	r=((PP_)(a))[i]
#define indexFloatOffAddrzh(r,a,i)  	r=PK_FLT((P_) (((StgFloat *)(a)) + i))
#define indexDoubleOffAddrzh(r,a,i) 	r=PK_DBL((P_) (((StgDouble *)(a)) + i))
#define indexStablePtrOffAddrzh(r,a,i)  r=((StgStablePtr *)(a))[i]
#define indexInt8OffAddrzh(r,a,i)    	r=((StgInt8 *)(a))[i]
#define indexInt16OffAddrzh(r,a,i)    	r=((StgInt16 *)(a))[i]
#define indexInt32OffAddrzh(r,a,i)    	r=((StgInt32 *)(a))[i]
#define indexWord8OffAddrzh(r,a,i)    	r=((StgWord8 *)(a))[i]
#define indexWord16OffAddrzh(r,a,i)    	r=((StgWord16 *)(a))[i]
#define indexWord32OffAddrzh(r,a,i)    	r=((StgWord32 *)(a))[i]
#ifdef SUPPORT_LONG_LONGS
#define indexInt64OffAddrzh(r,a,i)  	r=((LI_ *)(a))[i]
#define indexWord64OffAddrzh(r,a,i) 	r=((LW_ *)(a))[i]
#else
#define indexInt64OffAddrzh(r,a,i)  	r=((I_ *)(a))[i]
#define indexWord64OffAddrzh(r,a,i) 	r=((W_ *)(a))[i]
#endif

/* -----------------------------------------------------------------------------
   Float PrimOps.
   -------------------------------------------------------------------------- */

#define plusFloatzh(r,a,b)   r=((StgFloat)(a))+((StgFloat)(b))
#define minusFloatzh(r,a,b)  r=((StgFloat)(a))-((StgFloat)(b))
#define timesFloatzh(r,a,b)  r=((StgFloat)(a))*((StgFloat)(b))
#define divideFloatzh(r,a,b) r=((StgFloat)(a))/((StgFloat)(b))
#define negateFloatzh(r,a)   r=-((StgFloat)(a))
			     
#define int2Floatzh(r,a)     r=(StgFloat)((I_)(a))
#define float2Intzh(r,a)     r=(I_)((StgFloat)(a))
			     
#define expFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,exp,((StgFloat)(a)))
#define logFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,log,((StgFloat)(a)))
#define sqrtFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sqrt,((StgFloat)(a)))
#define sinFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sin,((StgFloat)(a)))
#define cosFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cos,((StgFloat)(a)))
#define tanFloatzh(r,a)	     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tan,((StgFloat)(a)))
#define asinFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,asin,((StgFloat)(a)))
#define acosFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,acos,((StgFloat)(a)))
#define atanFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,atan,((StgFloat)(a)))
#define sinhFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,sinh,((StgFloat)(a)))
#define coshFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,cosh,((StgFloat)(a)))
#define tanhFloatzh(r,a)     r=(StgFloat) RET_PRIM_STGCALL1(StgDouble,tanh,((StgFloat)(a)))
#define powerFloatzh(r,a,b)  r=(StgFloat) RET_PRIM_STGCALL2(StgDouble,pow,((StgFloat)(a)),((StgFloat)(b)))

/* -----------------------------------------------------------------------------
   Double PrimOps.
   -------------------------------------------------------------------------- */

#define zpzhzh(r,a,b)	     r=((StgDouble)(a))+((StgDouble)(b))
#define zmzhzh(r,a,b)	     r=((StgDouble)(a))-((StgDouble)(b))
#define ztzhzh(r,a,b)	     r=((StgDouble)(a))*((StgDouble)(b))
#define zszhzh(r,a,b)	     r=((StgDouble)(a))/((StgDouble)(b))
#define negateDoublezh(r,a)  r=-((StgDouble)(a))
			     
#define int2Doublezh(r,a)    r=(StgDouble)((I_)(a))
#define double2Intzh(r,a)    r=(I_)((StgDouble)(a))
			     
#define float2Doublezh(r,a)  r=(StgDouble)((StgFloat)(a))
#define double2Floatzh(r,a)  r=(StgFloat)((StgDouble)(a))
			     
#define expDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,exp,((StgDouble)(a)))
#define logDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,log,((StgDouble)(a)))
#define sqrtDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sqrt,((StgDouble)(a)))
#define sinDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sin,((StgDouble)(a)))
#define cosDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cos,((StgDouble)(a)))
#define tanDoublezh(r,a)     r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tan,((StgDouble)(a)))
#define asinDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,asin,((StgDouble)(a)))
#define acosDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,acos,((StgDouble)(a)))
#define atanDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,atan,((StgDouble)(a)))
#define sinhDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,sinh,((StgDouble)(a)))
#define coshDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,cosh,((StgDouble)(a)))
#define tanhDoublezh(r,a)    r=(StgDouble) RET_PRIM_STGCALL1(StgDouble,tanh,((StgDouble)(a)))
/* Power: **## */
#define ztztzhzh(r,a,b)	r=(StgDouble) RET_PRIM_STGCALL2(StgDouble,pow,((StgDouble)(a)),((StgDouble)(b)))

/* -----------------------------------------------------------------------------
   Integer PrimOps.
   -------------------------------------------------------------------------- */

/* We can do integer2Int and cmpInteger inline, since they don't need
 * to allocate any memory.
 *
 * integer2Int# is now modular.
 */

#define integer2Intzh(r, sa,da)				\
{ I_ s, res;						\
							\
  s = (sa);						\
  if (s == 0)						\
    res = 0;						\
  else {						\
    res = ((mp_limb_t *) (BYTE_ARR_CTS(da)))[0];	\
    if (s < 0) res = -res;				\
  }							\
  (r) = res;						\
}

#define integer2Wordzh(r, sa,da)			\
{ I_ s;							\
  W_ res;						\
							\
  s = (sa);						\
  if (s == 0)						\
    res = 0;						\
  else {						\
    res = ((mp_limb_t *) (BYTE_ARR_CTS(da)))[0];	\
    if (s < 0) res = -res;				\
  }							\
  (r) = res;						\
}

#define cmpIntegerzh(r, s1,d1, s2,d2)				\
{ MP_INT arg1;							\
  MP_INT arg2;							\
								\
  arg1._mp_size	= (s1);						\
  arg1._mp_alloc= ((StgArrWords *)d1)->words;			\
  arg1._mp_d	= (mp_limb_t *) (BYTE_ARR_CTS(d1));		\
  arg2._mp_size	= (s2);						\
  arg2._mp_alloc= ((StgArrWords *)d2)->words;			\
  arg2._mp_d	= (mp_limb_t *) (BYTE_ARR_CTS(d2));		\
								\
  (r) = RET_PRIM_STGCALL2(I_,mpz_cmp,&arg1,&arg2);		\
}

#define cmpIntegerIntzh(r, s,d, i)				\
{ MP_INT arg;							\
								\
  arg._mp_size	= (s);						\
  arg._mp_alloc = ((StgArrWords *)d)->words;			\
  arg._mp_d	= (mp_limb_t *) (BYTE_ARR_CTS(d));		\
								\
  (r) = RET_PRIM_STGCALL2(I_,mpz_cmp_si,&arg,i);		\
}

/* NOTE: gcdIntzh and gcdIntegerIntzh work only for positive inputs! */

/* mp_limb_t must be able to hold an StgInt for this to work properly */
#define gcdIntzh(r,a,b) \
{ mp_limb_t aa = (mp_limb_t)(a); \
  r = RET_STGCALL3(StgInt, mpn_gcd_1, (mp_limb_t *)(&aa), 1, (mp_limb_t)(b)); \
}

#define gcdIntegerIntzh(r,sa,a,b) \
  r = RET_STGCALL3(StgInt, mpn_gcd_1, (mp_limb_t *)(BYTE_ARR_CTS(a)), sa, b)

/* The rest are all out-of-line: -------- */

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

#define integerToWord64zh(r,sa,da)			\
{ mp_limb_t* d;						\
  I_ s;							\
  StgWord64 res;					\
							\
  d = (mp_limb_t *) (BYTE_ARR_CTS(da));			\
  s = (sa);						\
  switch (s) {						\
    case  0: res = 0;     break;			\
    case  1: res = d[0];  break;			\
    case -1: res = -d[0]; break;			\
    default:						\
      res = d[0] + ((StgWord64) d[1] << (BITS_IN (mp_limb_t))); \
      if (s < 0) res = -res;				\
  }							\
  (r) = res;						\
}

#define integerToInt64zh(r,sa,da)			\
{ mp_limb_t* d;						\
  I_ s;							\
  StgInt64 res;						\
							\
  d = (mp_limb_t *) (BYTE_ARR_CTS(da));			\
  s = (sa);						\
  switch (s) {						\
    case  0: res = 0;     break;			\
    case  1: res = d[0];  break;			\
    case -1: res = -d[0]; break;			\
    default:						\
      res = d[0] + ((StgWord64) d[1] << (BITS_IN (mp_limb_t))); \
      if (s < 0) res = -res;				\
  }							\
  (r) = res;						\
}

/* Conversions */
EXTFUN_RTS(int64ToIntegerzh_fast);
EXTFUN_RTS(word64ToIntegerzh_fast);

/* The rest are (way!) out of line, implemented via C entry points.
 */
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

LW_ stg_shiftL64   (StgWord64, StgInt);
LW_ stg_shiftRL64  (StgWord64, StgInt);
LI_ stg_iShiftL64  (StgInt64, StgInt);
LI_ stg_iShiftRL64 (StgInt64, StgInt);
LI_ stg_iShiftRA64 (StgInt64, StgInt);

LI_ stg_intToInt64    (StgInt);
I_  stg_int64ToInt    (StgInt64);
LW_ stg_int64ToWord64 (StgInt64);

LW_ stg_wordToWord64  (StgWord);
W_  stg_word64ToWord  (StgWord64);
LI_ stg_word64ToInt64 (StgWord64);
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

/*--- everything except new*Array is done inline: */

#define sameMutableArrayzh(r,a,b)	r=(I_)((a)==(b))
#define sameMutableByteArrayzh(r,a,b)	r=(I_)((a)==(b))

#define readArrayzh(r,a,i)		r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define readCharArrayzh(r,a,i)		indexCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWideCharArrayzh(r,a,i)	indexWideCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readIntArrayzh(r,a,i)		indexIntOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWordArrayzh(r,a,i)		indexWordOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readAddrArrayzh(r,a,i)		indexAddrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readFloatArrayzh(r,a,i)		indexFloatOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readDoubleArrayzh(r,a,i)	indexDoubleOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readStablePtrArrayzh(r,a,i)	indexStablePtrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readInt8Arrayzh(r,a,i)		indexInt8OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readInt16Arrayzh(r,a,i)		indexInt16OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readInt32Arrayzh(r,a,i)		indexInt32OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWord8Arrayzh(r,a,i)		indexWord8OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWord16Arrayzh(r,a,i)	indexWord16OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWord32Arrayzh(r,a,i)	indexWord32OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readInt64Arrayzh(r,a,i)		indexInt64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define readWord64Arrayzh(r,a,i)	indexWord64OffAddrzh(r,BYTE_ARR_CTS(a),i)

/* result ("r") arg ignored in write macros! */
#define writeArrayzh(a,i,v)		((PP_) PTRS_ARR_CTS(a))[(i)]=(v)

#define writeCharArrayzh(a,i,v)		writeCharOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWideCharArrayzh(a,i,v)	writeWideCharOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeIntArrayzh(a,i,v)		writeIntOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWordArrayzh(a,i,v)		writeWordOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeAddrArrayzh(a,i,v)		writeAddrOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeFloatArrayzh(a,i,v)	writeFloatOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeDoubleArrayzh(a,i,v)	writeDoubleOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeStablePtrArrayzh(a,i,v)	writeStablePtrOffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeInt8Arrayzh(a,i,v)		writeInt8OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeInt16Arrayzh(a,i,v)	writeInt16OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeInt32Arrayzh(a,i,v)	writeInt32OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWord8Arrayzh(a,i,v)	writeWord8OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWord16Arrayzh(a,i,v)	writeWord16OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWord32Arrayzh(a,i,v)	writeWord32OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeInt64Arrayzh(a,i,v)	writeInt64OffAddrzh(BYTE_ARR_CTS(a),i,v)
#define writeWord64Arrayzh(a,i,v)	writeWord64OffAddrzh(BYTE_ARR_CTS(a),i,v)

#define indexArrayzh(r,a,i)		r=((PP_) PTRS_ARR_CTS(a))[(i)]

#define indexCharArrayzh(r,a,i)		indexCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWideCharArrayzh(r,a,i)	indexWideCharOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexIntArrayzh(r,a,i)		indexIntOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWordArrayzh(r,a,i)		indexWordOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexAddrArrayzh(r,a,i)		indexAddrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexFloatArrayzh(r,a,i)	indexFloatOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexDoubleArrayzh(r,a,i)	indexDoubleOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexStablePtrArrayzh(r,a,i)	indexStablePtrOffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexInt8Arrayzh(r,a,i)		indexInt8OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexInt16Arrayzh(r,a,i)	indexInt16OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexInt32Arrayzh(r,a,i)	indexInt32OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWord8Arrayzh(r,a,i)	indexWord8OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWord16Arrayzh(r,a,i)	indexWord16OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWord32Arrayzh(r,a,i)	indexWord32OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexInt64Arrayzh(r,a,i)	indexInt64OffAddrzh(r,BYTE_ARR_CTS(a),i)
#define indexWord64Arrayzh(r,a,i)	indexWord64OffAddrzh(r,BYTE_ARR_CTS(a),i)

/* Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 */

#define unsafeFreezzeArrayzh(r,a)					\
	{								\
        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN_info);        \
	r = a;								\
	}

#define unsafeFreezzeByteArrayzh(r,a)	r=(a)

EXTFUN_RTS(unsafeThawArrayzh_fast);

#define sizzeofByteArrayzh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))
#define sizzeofMutableByteArrayzh(r,a) \
     r = (((StgArrWords *)(a))->words * sizeof(W_))

/* and the out-of-line ones... */

EXTFUN_RTS(newByteArrayzh_fast);
EXTFUN_RTS(newPinnedByteArrayzh_fast);
EXTFUN_RTS(newArrayzh_fast);

// Highly unsafe, for use with a pinned ByteArray 
// being kept alive with touch# 
#define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)

/* encoding and decoding of floats/doubles. */

/* We only support IEEE floating point format */
#include "ieee-flpt.h"

/* The decode operations are out-of-line because they need to allocate
 * a byte array.
 */
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

#define readMutVarzh(r,a)	 r=(P_)(((StgMutVar *)(a))->var)
#define writeMutVarzh(a,v)       (P_)(((StgMutVar *)(a))->var)=(v)
#define sameMutVarzh(r,a,b)      r=(I_)((a)==(b))

/* -----------------------------------------------------------------------------
   MVar PrimOps.

   All out of line, because they either allocate or may block.
   -------------------------------------------------------------------------- */
#define sameMVarzh(r,a,b)        r=(I_)((a)==(b))

/* Assume external decl of EMPTY_MVAR_info is in scope by now */
#define isEmptyMVarzh(r,a)       r=(I_)((GET_INFO((StgMVar*)(a))) == &stg_EMPTY_MVAR_info )
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

#define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)

#define eqStableNamezh(r,sn1,sn2)					\
    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))

#define makeStablePtrzh(r,a) \
   r = RET_STGCALL1(StgStablePtr,getStablePtr,a)

#define deRefStablePtrzh(r,sp) do {		\
  ASSERT(stable_ptr_table[stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK].weight > 0);	\
  r = stable_ptr_table[stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK].addr; \
} while (0);

#define eqStablePtrzh(r,sp1,sp2) \
    (r = ((stgCast(StgWord,sp1) & ~STABLEPTR_WEIGHT_MASK) == (stgCast(StgWord,sp2) & ~STABLEPTR_WEIGHT_MASK)))

/* -----------------------------------------------------------------------------
   Concurrency/Exception PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(forkzh_fast);
EXTFUN_RTS(yieldzh_fast);
EXTFUN_RTS(killThreadzh_fast);
EXTFUN_RTS(seqzh_fast);
EXTFUN_RTS(blockAsyncExceptionszh_fast);
EXTFUN_RTS(unblockAsyncExceptionszh_fast);

#define myThreadIdzh(t) (t = CurrentTSO)

extern int cmp_thread(const StgTSO *tso1, const StgTSO *tso2);
extern int rts_getThreadId(const StgTSO *tso);

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

EXTFUN_RTS(mkWeakzh_fast);
EXTFUN_RTS(finalizzeWeakzh_fast);

#define deRefWeakzh(code,val,w)				\
  if (((StgWeak *)w)->header.info == &stg_WEAK_info) {	\
	code = 1;					\
	val = (P_)((StgWeak *)w)->value;		\
  } else {						\
	code = 0;					\
	val = (P_)w;					\
  }

#define sameWeakzh(w1,w2)  ((w1)==(w2))


/* -----------------------------------------------------------------------------
   Foreign Object PrimOps.
   -------------------------------------------------------------------------- */

#define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)

#define foreignObjToAddrzh(r,fo)    r=ForeignObj_CLOSURE_DATA(fo)
#define touchzh(o)                  /* nothing */

EXTFUN_RTS(mkForeignObjzh_fast);

#define writeForeignObjzh(res,datum) \
   (ForeignObj_CLOSURE_DATA(res) = (P_)(datum))

#define eqForeignObjzh(r,f1,f2)                 r=(f1)==(f2)
#define indexCharOffForeignObjzh(r,fo,i)	indexCharOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWideCharOffForeignObjzh(r,fo,i)	indexWideCharOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexIntOffForeignObjzh(r,fo,i)		indexIntOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWordOffForeignObjzh(r,fo,i)	indexWordOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexAddrOffForeignObjzh(r,fo,i)	indexAddrOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexFloatOffForeignObjzh(r,fo,i)	indexFloatOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexDoubleOffForeignObjzh(r,fo,i)	indexDoubleOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexStablePtrOffForeignObjzh(r,fo,i)	indexStablePtrOffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexInt8OffForeignObjzh(r,fo,i)	indexInt8OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexInt16OffForeignObjzh(r,fo,i)	indexInt16OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexInt32OffForeignObjzh(r,fo,i)	indexInt32OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord8OffForeignObjzh(r,fo,i)	indexWord8OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord16OffForeignObjzh(r,fo,i)	indexWord16OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord32OffForeignObjzh(r,fo,i)	indexWord32OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexInt64OffForeignObjzh(r,fo,i)	indexInt64OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)
#define indexWord64OffForeignObjzh(r,fo,i)	indexWord64OffAddrzh(r,ForeignObj_CLOSURE_DATA(fo),i)

/* -----------------------------------------------------------------------------
   Constructor tags
   -------------------------------------------------------------------------- */

#define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))

/*  tagToEnum# is handled directly by the code generator. */

/* -----------------------------------------------------------------------------
   BCOs and BCO linkery
   -------------------------------------------------------------------------- */

EXTFUN_RTS(newBCOzh_fast);
EXTFUN_RTS(mkApUpd0zh_fast);

/* -----------------------------------------------------------------------------
   Signal processing.  Not really primops, but called directly from
   Haskell. 
   -------------------------------------------------------------------------- */

#define STG_SIG_DFL  (-1)
#define STG_SIG_IGN  (-2)
#define STG_SIG_ERR  (-3)
#define STG_SIG_HAN  (-4)

extern StgInt stg_sig_install (StgInt, StgInt, StgStablePtr, sigset_t *);
#define stg_sig_default(sig,mask) stg_sig_install(sig,STG_SIG_DFL,0,(sigset_t *)mask)
#define stg_sig_ignore(sig,mask) stg_sig_install(sig,STG_SIG_IGN,0,(sigset_t *)mask)
#define stg_sig_catch(sig,ptr,mask) stg_sig_install(sig,STG_SIG_HAN,ptr,(sigset_t *)mask)

#endif /* PRIMOPS_H */
