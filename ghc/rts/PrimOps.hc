/* -----------------------------------------------------------------------------
 * $Id: PrimOps.hc,v 1.2 1998/12/02 13:28:32 simonm Exp $
 *
 * Primitive functions / data
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef COMPILER

#include "RtsFlags.h"
#include "StgStartup.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "BlockAlloc.h" /* tmp */
#include "StablePtr.h"

/* ** temporary **

   classes CCallable and CReturnable don't really exist, but the
   compiler insists on generating dictionaries containing references
   to GHC_ZcCCallable_static_info etc., so we provide dummy symbols
   for these.
*/

W_ GHC_ZcCCallable_static_info[0];
W_ GHC_ZcCReturnable_static_info[0];

#ifndef aix_TARGET_OS /* AIX gives link errors with this as a const (RO assembler section) */
const 
#endif 
      StgClosure *PrelBase_Bool_closure_tbl[] = {
    &False_closure,
    &True_closure
};

/* -----------------------------------------------------------------------------
   Macros for Hand-written primitives.
   -------------------------------------------------------------------------- */

/*
 * Horrible macros for returning unboxed tuples.
 *
 * How an unboxed tuple is returned depends on two factors:
 *    - the number of real registers we have available
 *    - the boxedness of the returned fields.
 *
 * To return an unboxed tuple from a primitive operation, we have macros
 * RET_<layout> where <layout> describes the boxedness of each field of the
 * unboxed tuple:  N indicates a non-pointer field, and P indicates a pointer.
 *
 * We only define the cases actually used, to avoid having too much
 * garbage in this section.  Warning: any bugs in here will be hard to
 * track down.
 */

/*------ All Regs available */
#ifdef REG_R8
# define RET_P(a)     R1.w = (W_)(a); JMP_(ENTRY_CODE(Sp[0]));
# define RET_N(a)     RET_P(a)

# define RET_PP(a,b)  R1.w = (W_)(a); R2.w = (W_)(b); JMP_(ENTRY_CODE(Sp[0]));
# define RET_NN(a,b)  RET_PP(a,b)
# define RET_NP(a,b)  RET_PP(a,b)

# define RET_PPP(a,b,c) \
	R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); JMP_(ENTRY_CODE(Sp[0]));
# define RET_NNP(a,b,c) RET_PPP(a,b,c)

# define RET_NNNP(a,b,c,d) \
        R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); R4.w = (W_)d; \
        JMP_(ENTRY_CODE(Sp[0]));

# define RET_NNPNNP(a,b,c,d,e,f) \
        R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); \
        R4.w = (W_)(d); R5.w = (W_)(e); R6.w = (W_)(f); \
	JMP_(ENTRY_CODE(Sp[0]));

#else

#if defined(REG_R7) || defined(REG_R6) || defined(REG_R5) || \
    defined(REG_R4) || defined(REG_R3) || defined(REG_R2)
# error RET_n macros not defined for this setup.
#else

/*------ 1 Register available */
#ifdef REG_R1
# define RET_P(a)     R1.w = (W_)(a); JMP_(ENTRY_CODE(Sp[0]));
# define RET_N(a)     RET_P(a)

# define RET_PP(a,b)   R1.w = (W_)(a); Sp[-1] = (W_)(b); Sp -= 1; \
		       JMP_(ENTRY_CODE(Sp[1]));
# define RET_NN(a,b)   R1.w = (W_)(a); Sp[-1] = (W_)(b); Sp -= 2; \
		       JMP_(ENTRY_CODE(Sp[2]));
# define RET_NP(a,b)   RET_PP(a,b)

# define RET_PPP(a,b,c) \
	R1.w = (W_)(a); Sp[-2] = (W_)(b); Sp[-1] = (W_)(c); Sp -= 2; \
	JMP_(ENTRY_CODE(Sp[2]));
# define RET_NNP(a,b,c) \
	R1.w = (W_)(a); Sp[-2] = (W_)(b); Sp[-1] = (W_)(c); Sp -= 3; \
	JMP_(ENTRY_CODE(Sp[3]));

# define RET_NNNP(a,b,c,d)			\
	R1.w = (W_)(a); 			\
    /*  Sp[-5] = ARGTAG(1); */			\
        Sp[-4] = (W_)(b); 			\
    /*  Sp[-3] = ARGTAG(1); */			\
        Sp[-2] = (W_)(c); 			\
        Sp[-1] = (W_)(d); 			\
        Sp -= 5;				\
        JMP_(ENTRY_CODE(Sp[5]));

# define RET_NNPNNP(a,b,c,d,e,f)		\
        R1.w = (W_)(a);				\
	Sp[-1] = (W_)(f);			\
	Sp[-2] = (W_)(e);			\
	/* Sp[-3] = ARGTAG(1); */		\
	Sp[-4] = (W_)(d);			\
	/* Sp[-5] = ARGTAG(1); */		\
	Sp[-6] = (W_)(c);			\
	Sp[-7] = (W_)(b);			\
	/* Sp[-8] = ARGTAG(1); */		\
	Sp -= 8;				\
	JMP_(ENTRY_CODE(Sp[8]));

#else /* 0 Regs available */

#define PUSH_P(o,x) Sp[-o] = (W_)(x)
#define PUSH_N(o,x) Sp[1-o] = (W_)(x); /* Sp[-o] = ARGTAG(1) */
#define PUSHED(m)   Sp -= (m); JMP_(ENTRY_CODE(Sp[m]));

/* Here's how to construct these macros:
 *
 *   N = number of N's in the name;
 *   P = number of P's in the name;
 *   s = N * 2 + P;
 *   while (nonNull(name)) {
 *     if (nextChar == 'P') {
 *       PUSH_P(s,_);
 *       s -= 1;
 *     } else {
 *       PUSH_N(s,_);
 *       s -= 2
 *     }
 *   }
 *   PUSHED(N * 2 + P);
 */

# define RET_P(a)     PUSH_P(1,a); PUSHED(1)
# define RET_N(a)     PUSH_N(2,a); PUSHED(2)

# define RET_PP(a,b)   PUSH_P(2,a); PUSH_P(1,b); PUSHED(2)
# define RET_NN(a,b)   PUSH_N(4,a); PUSH_N(2,b); PUSHED(4)
# define RET_NP(a,b)   PUSH_N(3,a); PUSH_P(1,b); PUSHED(3)

# define RET_PPP(a,b,c) PUSH_P(3,a); PUSH_P(2,b); PUSH_P(1,c); PUSHED(3)
# define RET_NNP(a,b,c) PUSH_N(6,a); PUSH_N(4,b); PUSH_N(2,c); PUSHED(6)

# define RET_NNNP(a,b,c,d) PUSH_N(7,a); PUSH_N(5,b); PUSH_N(3,c); PUSH_P(1,d); PUSHED(7)	
# define RET_NNPNNP(a,b,c,d,e,f) PUSH_N(10,a); PUSH_N(8,b); PUSH_P(6,c); PUSH_N(5,d); PUSH_N(3,e); PUSH_P(1,f); PUSHED(10)

#endif

#endif
#endif

/*-----------------------------------------------------------------------------
  Array Primitives

  Basically just new*Array - the others are all inline macros.

  The size arg is always passed in R1, and the result returned in R1.

  The slow entry point is for returning from a heap check, the saved
  size argument must be re-loaded from the stack.
  -------------------------------------------------------------------------- */

/* for objects that are *less* than the size of a word, make sure we
 * round up to the nearest word for the size of the array.
 */

#define BYTES_TO_STGWORDS(n) ((n) + sizeof(W_) - 1)/sizeof(W_)

#define newByteArray(ty,scale)				\
 FN_(new##ty##ArrayZh_fast)				\
 {							\
   W_ stuff_size, size, n;				\
   StgArrWords* p;					\
   FB_							\
     MAYBE_GC(NO_PTRS,new##ty##ArrayZh_fast);		\
     n = R1.w;						\
     stuff_size = BYTES_TO_STGWORDS(n*scale);		\
     size = sizeofW(StgArrWords)+ stuff_size;		\
     p = (StgArrWords *)allocate(size);			\
     SET_HDR(p, &MUT_ARR_WORDS_info, CCCS);		\
     p->words = stuff_size;				\
     RET_P(p);						\
   FE_							\
 }

newByteArray(Char,   sizeof(C_))
newByteArray(Int,    sizeof(I_));
newByteArray(Word,   sizeof(W_));
newByteArray(Addr,   sizeof(P_));
newByteArray(Float,  sizeof(StgFloat));
newByteArray(Double, sizeof(StgDouble));
newByteArray(StablePtr, sizeof(StgStablePtr));

FN_(newArrayZh_fast)
{
  W_ size, n, init;
  StgArrPtrs* arr;
  StgPtr p;
  FB_
    n = R1.w;

    MAYBE_GC(R2_PTR,newArrayZh_fast);

    size = sizeofW(StgArrPtrs) + n;
    arr = (StgArrPtrs *)allocate(size);

    SET_HDR(arr,&MUT_ARR_PTRS_info,CCCS);
    arr->ptrs = n;

    init = R2.w;
    for (p = (P_)arr + sizeofW(StgArrPtrs); 
	 p < (P_)arr + size; p++) {
	*p = (W_)init;
    }

    RET_P(arr);
  FE_
}

FN_(newMutVarZh_fast)
{
  StgMutVar* mv;
  /* Args: R1.p = initialisation value */
  FB_

  HP_CHK_GEN(sizeofW(StgMutVar), R1_PTR, newMutVarZh_fast,);
  TICK_ALLOC_PRIM(sizeofW(StgMutVar),wibble,wibble,wibble)
  CCS_ALLOC(CCCS,sizeofW(StgMutVar));

  mv = stgCast(StgMutVar*,Hp-sizeofW(StgMutVar)+1);
  SET_HDR(mv,&MUT_VAR_info,CCCS);
  mv->var = R1.cl;

  RET_P(mv);

  FE_
}

/* -----------------------------------------------------------------------------
   Foreign Object Primitives

   -------------------------------------------------------------------------- */

#ifndef PAR
FN_(makeForeignObjZh_fast)
{
  /* R1.p = ptr to foreign object,
  */
  StgForeignObj *result;
  FB_

  HP_CHK_GEN(sizeofW(StgForeignObj), NO_PTRS, makeForeignObjZh_fast,);
  TICK_ALLOC_PRIM(sizeofW(StgForeignObj),wibble,wibble,wibble)
  CCS_ALLOC(CCCS,sizeofW(StgForeignObj)); /* ccs prof */

  result = (StgForeignObj *) (Hp + 1 - sizeofW(StgForeignObj));
  SET_HDR(result,&FOREIGN_info,CCCS);
  result->data = R1.p;

  /* returns (# s#, ForeignObj# #) */
  RET_P(result);
  FE_
}
#endif

/* -----------------------------------------------------------------------------
   Weak Pointer Primitives
   -------------------------------------------------------------------------- */

#ifndef PAR

FN_(mkWeakZh_fast)
{
  /* R1.p = key
     R2.p = value
     R3.p = finaliser
  */
  StgWeak *w;
  FB_

  HP_CHK_GEN(sizeofW(StgWeak), R1_PTR|R2_PTR|R3_PTR, mkWeakZh_fast,);
  TICK_ALLOC_PRIM(sizeofW(StgWeak),wibble,wibble,wibble);
  CCS_ALLOC(CCCS,sizeofW(StgWeak)); /* ccs prof */

  w = (StgWeak *) (Hp + 1 - sizeofW(StgWeak));
  SET_HDR(w, &WEAK_info, CCCS);

  w->key        = R1.cl;
  w->value      = R2.cl;
  w->finaliser  = R3.cl;

  w->link       = weak_ptr_list;
  weak_ptr_list = w;
  IF_DEBUG(weak, fprintf(stderr,"New weak pointer at %p\n",w));

  RET_P(w);
  FE_
}

FN_(deRefWeakZh_fast)
{
  /* R1.p = weak ptr
   */
  StgWeak *w;
  FB_
  
  w = (StgWeak *)R1.p;
  if (w->header.info == &WEAK_info) {
	RET_NP(1, w->value);
  } else {
	RET_NP(0, w);
  }
  FE_
}

#endif /* !PAR */

/* -----------------------------------------------------------------------------
   Arbitrary-precision Integer operations.
   -------------------------------------------------------------------------- */

FN_(int2IntegerZh_fast)
{
   /* arguments: R1 = Int# */

   I_ val, s;  		/* to avoid aliasing */
   StgArrWords* p;	/* address of array result */
   FB_

   val = R1.i;
   HP_CHK_GEN(sizeofW(StgArrWords)+1, NO_PTRS, int2IntegerZh_fast,)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords)+1,wibble,wibble,wibble)
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

   p = stgCast(StgArrWords*,Hp)-1;
   SET_ARR_HDR(p, &ARR_WORDS_info, CCCS, 1);

   /* mpz_set_si is inlined here, makes things simpler */
   if (val < 0) { 
	s  = -1;
	*Hp = -val;
   } else if (val > 0) {
	s = 1;
	*Hp = val;
   } else {
	s = 0;
   }

   /* returns (# alloc :: Int#, 
		 size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   RET_NNP(1,s,p);
   FE_
}

FN_(word2IntegerZh_fast)
{
   /* arguments: R1 = Word# */

   W_ val;  		/* to avoid aliasing */
   I_  s;
   StgArrWords* p;	/* address of array result */
   FB_

   val = R1.w;
   HP_CHK_GEN(sizeofW(StgArrWords)+1, NO_PTRS, word2IntegerZh_fast,)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords)+1,wibble,wibble,wibble)
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

   p = stgCast(StgArrWords*,Hp)-1;
   SET_ARR_HDR(p, &ARR_WORDS_info, CCCS, 1);

   if (val != 0) {
	s = 1;
	*Hp = val;
   } else {
	s = 0;
   }

   /* returns (# alloc :: Int#, 
		 size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   RET_NNP(1,s,p);
   FE_
}

FN_(addr2IntegerZh_fast)
{
  MP_INT result;
  char *str;
  FB_

  MAYBE_GC(NO_PTRS,addr2IntegerZh_fast);

  /* args:   R1 :: Addr# */
  str = R1.a;

  /* Perform the operation */
  if (RET_STGCALL3(int, mpz_init_set_str,&result,(str),/*base*/10))
      abort();

  RET_NNP(result._mp_alloc, result._mp_size, 
	  result._mp_d - sizeofW(StgArrWords));
  FE_
}

/*
 * 'long long' primops for converting to/from Integers.
 */

#ifdef SUPPORT_LONG_LONGS

FN_(int64ToIntegerZh_fast)
{
   /* arguments: L1 = Int64# */

   StgInt64 val; /* to avoid aliasing */
   W_ hi;
   I_  s,a, neg, words_needed;
   StgArrWords* p;	/* address of array result */
   FB_

     /* ToDo: extend StgUnion?? */
   val = (LI_)L1;
   neg = 0;
   if ((LW_)(val) >= 0x100000000ULL)  { 
       words_needed = 2;
   } else { 
       /* minimum is one word */
       words_needed = 1;
   }
   HP_CHK_GEN(sizeofW(StgArrWords)+words_needed, NO_PTRS, int64ToIntegerZh_fast,)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords)+words_needed,wibble,wibble,wibble)
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+words_needed); /* ccs prof */

   p = stgCast(StgArrWords*,Hp)-1;
   SET_ARR_HDR(p, &ARR_WORDS_info, CCCS, words_needed);

   if ( val < 0LL ) {
     neg = 1;
     val = -val;
   }
   hi = (W_)((LW_)val / 0x100000000ULL);
   if ((LW_)(val) >= 0x100000000ULL)  { 
      s = 2; 
      a = 2;
      Hp[0] = (W_)val;
      Hp[1] = hi;
   } else if ( val != 0 ) {
      s = 1;
      a = 1;
     Hp[0] =  (W_)val;
   }  else /* val==0 */   {
      s = 0;
      a = 1;
   }
  s = ( neg ? -s : s );

   /* returns (# alloc :: Int#, 
		 size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   RET_NNP(a,s,p);
   FE_
}

FN_(word64ToIntegerZh_fast)
{
   /* arguments: L1 = Word64# */

   StgNat64 val; /* to avoid aliasing */
   StgWord hi;
   I_  s,a,words_needed;
   StgArrWords* p;	/* address of array result */
   FB_

   val = (LW_)L1;
   if ( val >= 0x100000000ULL ) {
      words_needed = 2;
   } else {
      words_needed = 1;
   }
   HP_CHK_GEN(sizeofW(StgArrWords)+words_needed, NO_PTRS, word64ToIntegerZh_fast,)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords)+words_needed,wibble,wibble,wibble)
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+words_needed); /* ccs prof */

   p = stgCast(StgArrWords*,Hp)-1;
   SET_ARR_HDR(p, &ARR_WORDS_info, CCCS, words_needed);

   hi = (W_)((LW_)val / 0x100000000ULL);
   if ( val >= 0x100000000ULL ) { 
     s = 2;
     a = 2;
     Hp[0] = ((W_)val);
     Hp[1] = (hi);
   } else if ( val != 0 )      {
      s = 1;
      a = 1;
      Hp[0] = ((W_)val);
   } else /* val==0 */         {
      s = 0;
      a = 1;
   }

   /* returns (# alloc :: Int#, 
		 size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   RET_NNP(a,s,p);
   FE_
}


#endif /* HAVE_LONG_LONG */

/* ToDo: this is shockingly inefficient */

#define GMP_TAKE2_RET1(name,mp_fun)					\
FN_(name)								\
{									\
  MP_INT arg1, arg2, result;						\
  I_ a1, s1, a2, s2;							\
  StgArrWords* d1;							\
  StgArrWords* d2;							\
  FB_									\
 									\
  /* call doYouWantToGC() */						\
  MAYBE_GC(R3_PTR | R6_PTR, name);					\
									\
  a1 = R1.i;								\
  s1 = R2.i;								\
  d1 = stgCast(StgArrWords*,R3.p);					\
  a2 = R4.i;								\
  s2 = R5.i;								\
  d2 = stgCast(StgArrWords*,R6.p);					\
									\
  arg1._mp_alloc	= (a1);						\
  arg1._mp_size		= (s1);						\
  arg1._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d1));	\
  arg2._mp_alloc	= (a2);						\
  arg2._mp_size		= (s2);						\
  arg2._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d2));	\
									\
  STGCALL1(mpz_init,&result);						\
									\
  /* Perform the operation */						\
  STGCALL3(mp_fun,&result,&arg1,&arg2);					\
									\
  RET_NNP(result._mp_alloc, 						\
	  result._mp_size, 						\
          result._mp_d-sizeofW(StgArrWords));				\
  FE_									\
}

#define GMP_TAKE2_RET2(name,mp_fun)					\
FN_(name)								\
{									\
  MP_INT arg1, arg2, result1, result2;					\
  I_ a1, s1, a2, s2;							\
  StgArrWords* d1;							\
  StgArrWords* d2;							\
  FB_									\
 									\
  /* call doYouWantToGC() */						\
  MAYBE_GC(R3_PTR | R6_PTR, name);					\
									\
  a1 = R1.i;								\
  s1 = R2.i;								\
  d1 = stgCast(StgArrWords*,R3.p);					\
  a2 = R4.i;								\
  s2 = R5.i;								\
  d2 = stgCast(StgArrWords*,R6.p);					\
									\
  arg1._mp_alloc	= (a1);						\
  arg1._mp_size		= (s1);						\
  arg1._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d1));	\
  arg2._mp_alloc	= (a2);						\
  arg2._mp_size		= (s2);						\
  arg2._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d2));	\
									\
  STGCALL1(mpz_init,&result1);						\
  STGCALL1(mpz_init,&result2);						\
									\
  /* Perform the operation */						\
  STGCALL4(mp_fun,&result1,&result2,&arg1,&arg2);			\
									\
  RET_NNPNNP(result1._mp_alloc,						\
	     result1._mp_size, 						\
             result1._mp_d-sizeofW(StgArrWords),			\
	     result2._mp_alloc,						\
	     result2._mp_size, 						\
             result2._mp_d-sizeofW(StgArrWords));			\
  FE_									\
}

GMP_TAKE2_RET1(plusIntegerZh_fast,  mpz_add);
GMP_TAKE2_RET1(minusIntegerZh_fast, mpz_sub);
GMP_TAKE2_RET1(timesIntegerZh_fast, mpz_mul);
GMP_TAKE2_RET1(gcdIntegerZh_fast,   mpz_gcd);

GMP_TAKE2_RET2(quotRemIntegerZh_fast, mpz_tdiv_qr);
GMP_TAKE2_RET2(divModIntegerZh_fast,  mpz_fdiv_qr);

#ifndef FLOATS_AS_DOUBLES
FN_(decodeFloatZh_fast)
{ 
  MP_INT mantissa;
  I_ exponent;
  StgArrWords* p;
  StgFloat arg;
  FB_

  /* arguments: F1 = Float# */
  arg = F1;

  HP_CHK_GEN(sizeof(StgArrWords)+1, NO_PTRS, decodeFloatZh_fast,);
  TICK_ALLOC_PRIM(sizeofW(StgArrWords)+1,wibble,wibble,wibble)
  CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

  /* Be prepared to tell Lennart-coded __decodeFloat	*/
  /* where mantissa._mp_d can be put (it does not care about the rest) */
  p = stgCast(StgArrWords*,Hp)-1;
  SET_ARR_HDR(p,&ARR_WORDS_info,CCCS,1)
  mantissa._mp_d = (void *)BYTE_ARR_CTS(p);

  /* Perform the operation */
  STGCALL3(__decodeFloat,&mantissa,&exponent,arg);

  /* returns: (R1 = Int# (expn), R2 = Int#, R3 = Int#, R4 = ByteArray#) */
  RET_NNNP(exponent,mantissa._mp_alloc,mantissa._mp_size,p);
  FE_
}
#endif /* !FLOATS_AS_DOUBLES */

#define DOUBLE_MANTISSA_SIZE (sizeof(StgDouble)/sizeof(W_))
#define ARR_SIZE (sizeof(StgArrWords) + DOUBLE_MANTISSA_SIZE)

FN_(decodeDoubleZh_fast)
{ MP_INT mantissa;
  I_ exponent;
  StgDouble arg;
  StgArrWords* p;
  FB_

  /* arguments: D1 = Double# */
  arg = D1;

  HP_CHK_GEN(ARR_SIZE, NO_PTRS, decodeDoubleZh_fast,);
  TICK_ALLOC_PRIM(ARR_SIZE,wibble,wibble,wibble)
  CCS_ALLOC(CCCS,ARR_SIZE); /* ccs prof */

  /* Be prepared to tell Lennart-coded __decodeDouble	*/
  /* where mantissa.d can be put (it does not care about the rest) */
  p = stgCast(StgArrWords*,Hp-ARR_SIZE+1);
  SET_ARR_HDR(p, &ARR_WORDS_info, CCCS, DOUBLE_MANTISSA_SIZE);
  mantissa._mp_d = (void *)BYTE_ARR_CTS(p);

  /* Perform the operation */
  STGCALL3(__decodeDouble,&mantissa,&exponent,arg);

  /* returns: (R1 = Int# (expn), R2 = Int#, R3 = Int#, R4 = ByteArray#) */
  RET_NNNP(exponent,mantissa._mp_alloc,mantissa._mp_size,p);
  FE_
}

/* -----------------------------------------------------------------------------
 * Concurrency primitives
 * -------------------------------------------------------------------------- */

FN_(forkZh_fast)
{
  FB_
  /* args: R1 = closure to spark */
  
  if (closure_SHOULD_SPARK(stgCast(StgClosure*,R1.p))) {

    MAYBE_GC(R1_PTR, forkZh_fast);

    /* create it right now, return ThreadID in R1 */
    R1.t = RET_STGCALL2(StgTSO *, createIOThread, 
			RtsFlags.GcFlags.initialStkSize, R1.cl);
      
    /* switch at the earliest opportunity */ 
    context_switch = 1;
  }
  
  JMP_(*Sp);

  FE_
}

FN_(killThreadZh_fast)
{
  FB_
  /* args: R1.p = TSO to kill */

  /* The thread is dead, but the TSO sticks around for a while.  That's why
   * we don't have to explicitly remove it from any queues it might be on.
   */
  STGCALL1(deleteThread, (StgTSO *)R1.p);

  /* We might have killed ourselves.  In which case, better return to the
   * scheduler...
   */
  if ((StgTSO *)R1.p == CurrentTSO) {
	JMP_(stg_stop_thread_entry); /* leave semi-gracefully */
  }

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(newMVarZh_fast)
{
  StgMVar *mvar;

  FB_
  /* args: none */

  HP_CHK_GEN(sizeofW(StgMVar), NO_PTRS, newMVarZh_fast,);
  TICK_ALLOC_PRIM(sizeofW(StgMVar),wibble,wibble,wibble)
  CCS_ALLOC(CCCS,sizeofW(StgMVar)); /* ccs prof */
  
  mvar = (StgMVar *) (Hp - sizeofW(StgMVar) + 1);
  SET_INFO(mvar,&EMPTY_MVAR_info);
  mvar->head = mvar->tail = (StgTSO *)&END_TSO_QUEUE_closure;
  mvar->value = (StgClosure *)&END_TSO_QUEUE_closure;

  R1.p = (P_)mvar;

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(takeMVarZh_fast)
{
  StgMVar *mvar;

  FB_
  /* args: R1 = MVar closure */

  mvar = (StgMVar *)R1.p;

  /* If the MVar is empty, put ourselves on its blocking queue,
   * and wait until we're woken up.
   */
  if (GET_INFO(mvar) != &FULL_MVAR_info) {
    if (mvar->head == (StgTSO *)&END_TSO_QUEUE_closure) {
      mvar->head = CurrentTSO;
    } else {
      mvar->tail->link = CurrentTSO;
    }
    CurrentTSO->link = (StgTSO *)&END_TSO_QUEUE_closure;
    mvar->tail = CurrentTSO;

    BLOCK(R1_PTR, takeMVarZh_fast);
  }

  SET_INFO(mvar,&EMPTY_MVAR_info);
  R1.cl = mvar->value;
  mvar->value = (StgClosure *)&END_TSO_QUEUE_closure;

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(putMVarZh_fast)
{
  StgMVar *mvar;
  StgTSO *tso;

  FB_
  /* args: R1 = MVar, R2 = value */

  mvar = (StgMVar *)R1.p;
  if (GET_INFO(mvar) == &FULL_MVAR_info) {
    fflush(stdout);
    fprintf(stderr, "putMVar#: MVar already full.\n");
    stg_exit(EXIT_FAILURE);
  }
  
  SET_INFO(mvar,&FULL_MVAR_info);
  mvar->value = R2.cl;

  /* wake up the first thread on the queue,
   * it will continue with the takeMVar operation and mark the MVar
   * empty again.
   */
  tso = mvar->head;
  if (tso != (StgTSO *)&END_TSO_QUEUE_closure) {
    PUSH_ON_RUN_QUEUE(tso);
    mvar->head = tso->link;
    tso->link = (StgTSO *)&END_TSO_QUEUE_closure;
    if (mvar->head == (StgTSO *)&END_TSO_QUEUE_closure) {
      mvar->tail = (StgTSO *)&END_TSO_QUEUE_closure;
    }
  }

  /* ToDo: yield here for better communication performance? */
  JMP_(ENTRY_CODE(*Sp));
  FE_
}

/* -----------------------------------------------------------------------------
   Stable pointer primitives
   -------------------------------------------------------------------------  */

FN_(makeStablePtrZh_fast)
{
  StgInt stable_ptr;
  FB_ 

    if (stable_ptr_free == NULL) {
      enlargeStablePtrTable();
    }

    stable_ptr = stable_ptr_free - stable_ptr_table;
    (P_)stable_ptr_free  = *stable_ptr_free;
    stable_ptr_table[stable_ptr] = R1.p;

    R1.i = stable_ptr;
    JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

#endif /* COMPILER */
