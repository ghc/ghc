/* -*- mode: hugs-c; -*- */
extern  Void   linkPreludeTC    Args((Void));
extern  Void   linkPreludeCM    Args((Void));
extern  Void   linkPreludeNames Args((Void));

extern Module modulePreludeHugs;

/* --------------------------------------------------------------------------
 * Primitive constructor functions 
 * ------------------------------------------------------------------------*/

extern Name  nameFalse, nameTrue;
extern Name  nameNil,   nameCons;
extern Name  nameUnit;

extern Name  nameFromInt, nameFromDouble;/*coercion of numerics            */
extern Name  nameFromInteger;
extern Name  nameReturn,  nameBind;     /* for translating monad comps     */
extern Name  nameZero;                  /* for monads with a zero          */
#if EVAL_INSTANCES
extern Name  nameStrict,  nameSeq;      /* Members of class Eval           */
#endif

extern Name  nameId;
extern Name  nameRunIO;
extern Name  namePrint;

extern Name nameForce;

#if TREX
extern Name  nameInsFld;                /* Field insertion routine         */
extern Type  typeRec;                   /* Record formation                */
extern Name  nameNoRec;                 /* The empty record                */
extern Type  typeNoRow;                 /* The empty row                   */
#endif

/* The following data constructors are used to box unboxed
 * arguments and are treated differently by the code generator.
 * That is, they have primop `elem` {INT_REP,FLOAT_REP,...}.
 */
#define boxingConRep(con) ((AsmRep)(name(con).primop))
#define isBoxingCon(con) (isName(con) && boxingConRep(con) != 0)

extern Name nameMkC;
extern Name nameMkI;
#ifdef PROVIDE_INT64
extern Name nameMkInt64;
#endif
#ifdef PROVIDE_WORD
extern Name nameMkW;
#endif
#ifdef PROVIDE_ADDR
extern Name nameMkA;
#endif
extern Name nameMkF;
extern Name nameMkD;
#ifdef PROVIDE_STABLE
extern Name nameMkStable;    
#endif

/* The following data constructors are used to make boxed but 
 * unpointed values pointed and require no special treatment
 * by the code generator.
 */
#ifdef PROVIDE_INTEGER
extern Name nameMkInteger;
#endif
#ifdef PROVIDE_ARRAY
extern Name nameMkPrimArray;            
extern Name nameMkPrimByteArray;
extern Name nameMkRef;                  
extern Name nameMkPrimMutableArray;     
extern Name nameMkPrimMutableByteArray; 
#endif
#ifdef PROVIDE_FOREIGN
extern Name nameMkForeign;   
#endif
#ifdef PROVIDE_WEAK
extern Name nameMkWeak;
#endif
#ifdef PROVIDE_CONCURRENT
extern Name nameMkThreadId;  
extern Name nameMkMVar;  
#endif

extern Type typeArrow;                  /* Builtin type constructors       */

#define fn(from,to)  ap2(typeArrow,from,to)     /* make type:  from -> to  */

/* For every primitive type provided by the runtime system,
 * we construct a Haskell type using a declaration of the form:
 *
 *   data Int  -- no constructors given
 */
extern Type typeChar;
extern Type typeInt;
#ifdef PROVIDE_INT64
extern Type typeInt64;
#endif
#ifdef PROVIDE_INTEGER
extern Type typeInteger;
#endif
#ifdef PROVIDE_WORD
extern Type typeWord;
#endif
#ifdef PROVIDE_ADDR
extern Type typeAddr;
#endif
#ifdef PROVIDE_ARRAY
Type typePrimArray;            
Type typePrimByteArray;
Type typeRef;                  
Type typePrimMutableArray;     
Type typePrimMutableByteArray; 
#endif
extern Type typeFloat;
extern Type typeDouble;
#ifdef PROVIDE_STABLE
extern Type typeStable;
#endif
#ifdef PROVIDE_WEAK
extern Type typeWeak;
#endif
#ifdef PROVIDE_FOREIGN
extern Type typeForeign;
#endif
#ifdef PROVIDE_CONCURRENT
extern Type typeThreadId;
extern Type typeMVar;
#endif

/* And a smaller number of types defined in plain Haskell */
extern Type typeList;
extern Type typeUnit;
extern Type typeString;
extern Type typeBool;
extern Type typeST;
extern Type typeIO;
extern Type typeException;

/* copied out of K&R2, Appendix A */
#define cat(x,y) x ## y
#define xcat(x,y) cat(x,y)

#ifdef BIGNUMTYPE
#define typeBignum   xcat(type,BIGNUMTYPE)
#define nameMkBignum xcat(nameMk,BIGNUMTYPE)
#else
#warning BIGNUMTYPE undefined
#endif

extern List  stdDefaults;               /* List of standard default types  */

extern Class classEq;                   /* `standard' classes              */
extern Class classOrd;
extern Class classShow;
extern Class classRead;
extern Class classIx;
extern Class classEnum;
extern Class classBounded;
#if EVAL_INSTANCES
extern Class classEval;
#endif

extern Class classReal;                 /* `numeric' classes               */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Class classMonad;                /* Monads and monads with a zero   */
extern Class classMonad0;

/* used in typechecker */
extern Name nameError;
extern Name nameInd;

/* used while desugaring */
extern Name nameId;
extern Name nameOtherwise;
extern Name nameUndefined;              /* generic undefined value         */

/* used in pattern match */
#if NPLUSK
extern Name namePmSub; 
#endif
extern Name nameSel;

/* used in translation */
extern Name nameEq;     
extern Name namePMFail;
extern Name nameEqChar;
extern Name nameEqInt;
extern Name nameEqInteger;
extern Name nameEqDouble;
extern Name namePmInt;
extern Name namePmInteger;
extern Name namePmDouble;
extern Name namePmLe;
extern Name namePmSubtract;
extern Name namePmFromInteger;
extern Name nameMkIO;
extern Name nameUnpackString;

