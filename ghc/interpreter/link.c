
/* --------------------------------------------------------------------------
 * Load symbols required from the Prelude
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: link.c,v $
 * $Revision: 1.6 $
 * $Date: 1999/03/09 14:51:08 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "Assembler.h" /* for asmPrimOps and AsmReps */

#include "link.h"

////Module modulePreludeHugs;


Type typeArrow  =BOGUS(1);                         /* Function spaces                 */

Type typeChar  =BOGUS(2);
Type typeInt  =BOGUS(3);
#ifdef PROVIDE_INT64
Type typeInt64  =BOGUS(4);
#endif
#ifdef PROVIDE_INTEGER
Type typeInteger  =BOGUS(5);
#endif
#ifdef PROVIDE_WORD
Type typeWord  =BOGUS(6);
#endif
#ifdef PROVIDE_ADDR
Type typeAddr  =BOGUS(7);
#endif
#ifdef PROVIDE_ARRAY
Type typePrimArray  =BOGUS(8);            
Type typePrimByteArray  =BOGUS(9);
Type typeRef  =BOGUS(10);                  
Type typePrimMutableArray  =BOGUS(11);     
Type typePrimMutableByteArray  =BOGUS(12); 
#endif
Type typeFloat  =BOGUS(13);
Type typeDouble  =BOGUS(14);
#ifdef PROVIDE_STABLE
Type typeStable  =BOGUS(15);
#endif
#ifdef PROVIDE_WEAK
Type typeWeak  =BOGUS(16);
#endif
#ifdef PROVIDE_FOREIGN
Type typeForeign  =BOGUS(17);
#endif
#ifdef PROVIDE_CONCURRENT
Type typeThreadId  =BOGUS(18);
Type typeMVar  =BOGUS(19);
#endif

Type typeList  =BOGUS(20);
Type typeUnit  =BOGUS(21);
Type typeString  =BOGUS(22);
Type typeBool  =BOGUS(23);
Type typeST  =BOGUS(24);
Type typeIO  =BOGUS(25);
Type typeException  =BOGUS(26);

Class classEq  =BOGUS(27);                          /* `standard' classes              */
Class classOrd  =BOGUS(28);
Class classShow  =BOGUS(29);
Class classRead  =BOGUS(30);
Class classIx  =BOGUS(31);
Class classEnum  =BOGUS(32);
Class classBounded  =BOGUS(33);
#if EVAL_INSTANCES
Class classEval  =BOGUS(34);
#endif

Class classReal  =BOGUS(35);                        /* `numeric' classes               */
Class classIntegral  =BOGUS(36);
Class classRealFrac  =BOGUS(37);
Class classRealFloat  =BOGUS(38);
Class classFractional  =BOGUS(39);
Class classFloating  =BOGUS(40);
Class classNum  =BOGUS(41);

Class classMonad  =BOGUS(42);                       /* Monads and monads with a zero   */
/*Class classMonad0  =BOGUS();*/

List stdDefaults  =BOGUS(43);                       /* standard default values         */

Name nameTrue  =BOGUS(44),    
     nameFalse  =BOGUS(45);            /* primitive boolean constructors  */
Name nameNil  =BOGUS(46),     
     nameCons  =BOGUS(47);             /* primitive list constructors     */
Name nameUnit  =BOGUS(48);                          /* primitive Unit type constructor */

Name nameEq  =BOGUS(49);    
Name nameFromInt  =BOGUS(50),
     nameFromDouble  =BOGUS(51);       /* coercion of numerics            */
Name nameFromInteger  =BOGUS(52);
Name nameReturn  =BOGUS(53),  
     nameBind  =BOGUS(54);             /* for translating monad comps     */
Name nameZero  =BOGUS(55);                          /* for monads with a zero          */
#if EVAL_INSTANCES
Name nameStrict  =BOGUS(56);                        /* Members of class Eval           */
Name nameSeq  =BOGUS(57);   
#endif

Name nameId  =BOGUS(58);
Name nameRunIO  =BOGUS(59);
Name namePrint  =BOGUS(60);

Name nameOtherwise  =BOGUS(61);
Name nameUndefined  =BOGUS(62);                     /* generic undefined value         */
#if NPLUSK
Name namePmSub  =BOGUS(63); 
#endif
Name namePMFail  =BOGUS(64);
Name namePMFailBUG = BOGUS(666);
Name nameEqChar  =BOGUS(65);
Name nameEqInt  =BOGUS(66);
#if !OVERLOADED_CONSTANTS
Name nameEqInteger  =BOGUS(67);
#endif
Name nameEqDouble  =BOGUS(68);
Name namePmInt  =BOGUS(69);
Name namePmInteger  =BOGUS(70);
Name namePmDouble  =BOGUS(71);
Name namePmLe  =BOGUS(72);
Name namePmSubtract  =BOGUS(73);
Name namePmFromInteger  =BOGUS(74);
Name nameMkIO  =BOGUS(75);
Name nameUnpackString  =BOGUS(76);
Name nameError  =BOGUS(77);
Name nameInd  =BOGUS(78);

Name nameAnd  =BOGUS(80);
Name nameConCmp  =BOGUS(82);
Name nameCompAux  =BOGUS(83);
Name nameEnFrTh  =BOGUS(84);
Name nameEnFrTo  =BOGUS(85);
Name nameEnFrom  =BOGUS(86);
Name nameEnFrEn  =BOGUS(87);
Name nameEnToEn  =BOGUS(88);
Name nameEnInRng  =BOGUS(89);
Name nameEnIndex  =BOGUS(90);
Name nameEnRange  =BOGUS(91);
Name nameRangeSize  =BOGUS(92);
Name nameComp  =BOGUS(93);
Name nameShowField  =BOGUS(94);
Name nameApp  =BOGUS(95);
Name nameShowParen  =BOGUS(96);
Name nameReadParen  =BOGUS(97);
Name nameLex  =BOGUS(98);
Name nameReadField  =BOGUS(99);
Name nameFlip  =BOGUS(100);

Name namePrimSeq =BOGUS(1000);
Name namePrimCatch =BOGUS(1001);
Name namePrimRaise =BOGUS(1002);

Name nameFromTo  =BOGUS(101);
Name nameFromThen  =BOGUS(102);
Name nameFrom  =BOGUS(103);
Name nameFromThenTo  =BOGUS(104);
Name nameNegate  =BOGUS(105);

/* these names are required before we've had a chance to do the right thing */
Name nameSel  =BOGUS(106);
Name nameUnsafeUnpackCString  =BOGUS(107);

/* constructors used during translation and codegen */
Name nameMkC  =BOGUS(108);                           /* Char#        -> Char           */
Name nameMkI  =BOGUS(109);                           /* Int#         -> Int            */
#ifdef PROVIDE_INT64                                                       
Name nameMkInt64  =BOGUS(110);                       /* Int64#       -> Int64          */
#endif                                                                     
#ifdef PROVIDE_INTEGER                                                     
Name nameMkInteger  =BOGUS(111);                     /* Integer#     -> Integer        */
#endif                                                                     
#ifdef PROVIDE_WORD                                                        
Name nameMkW  =BOGUS(112);                           /* Word#        -> Word           */
#endif                                                                     
#ifdef PROVIDE_ADDR                                                        
Name nameMkA  =BOGUS(113);                           /* Addr#        -> Addr            */
#endif                                                                     
Name nameMkF  =BOGUS(114);                           /* Float#       -> Float           */
Name nameMkD  =BOGUS(115);                           /* Double#      -> Double          */
#ifdef PROVIDE_ARRAY
Name nameMkPrimArray  =BOGUS(116);            
Name nameMkPrimByteArray  =BOGUS(117);
Name nameMkRef  =BOGUS(118);                  
Name nameMkPrimMutableArray  =BOGUS(119);     
Name nameMkPrimMutableByteArray  =BOGUS(120); 
#endif
#ifdef PROVIDE_STABLE
Name nameMkStable  =BOGUS(121);                      /* StablePtr# a -> StablePtr a     */
#endif
#ifdef PROVIDE_WEAK
Name nameMkWeak  =BOGUS(122);                        /* Weak# a      -> Weak a          */
#endif
#ifdef PROVIDE_FOREIGN
Name nameMkForeign  =BOGUS(123);                     /* ForeignObj#  -> ForeignObj      */
#endif
#ifdef PROVIDE_CONCURRENT
Name nameMkThreadId  =BOGUS(124);                    /* ThreadId#    -> ThreadId        */
Name nameMkMVar  =BOGUS(125);                        /* MVar#        -> MVar            */
#endif



Name nameMinBnd =BOGUS(400);
Name  nameMaxBnd =BOGUS(401);
Name  nameCompare =BOGUS(402);
Name nameShowsPrec =BOGUS(403);
Name  nameIndex =BOGUS(404);
Name nameReadsPrec =BOGUS(405); 
Name nameRange =BOGUS(406);
Name nameEQ =BOGUS(407);
Name nameInRange =BOGUS(408);
Name nameGt =BOGUS(409);
Name nameLe =BOGUS(410);
Name namePlus =BOGUS(411);
Name nameMult =BOGUS(412);
Name nameMFail =BOGUS(413);
Type typeOrdering =BOGUS(414);
Module modulePrelude =BOGUS(415);
Name nameMap  = BOGUS(416);
Name nameMinus = BOGUS(417);

#define QQ(lval) assert(lval != 0); assert(lval <= -900000); lval 

/* --------------------------------------------------------------------------
 * Frequently used type skeletons:
 * ------------------------------------------------------------------------*/

/* ToDo: move these to link.c and call them 'typeXXXX' */
       Type  arrow=BOGUS(500);                     /* mkOffset(0) -> mkOffset(1)      */
       Type  boundPair=BOGUS(500);;                 /* (mkOffset(0),mkOffset(0))       */
       Type  listof=BOGUS(500);;                    /* [ mkOffset(0) ]                 */
       Type  typeVarToVar=BOGUS(500);;              /* mkOffset(0) -> mkOffset(0)      */

       Cell  predNum=BOGUS(500);;                   /* Num (mkOffset(0))               */
       Cell  predFractional=BOGUS(500);;            /* Fractional (mkOffset(0))        */
       Cell  predIntegral=BOGUS(500);;              /* Integral (mkOffset(0))          */
       Kind  starToStar=BOGUS(500);;                /* Type -> Type                    */
       Cell  predMonad=BOGUS(500);;                 /* Monad (mkOffset(0))             */

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static Tycon linkTycon ( String s );
static Tycon linkClass ( String s );
static Name  linkName  ( String s );
static Void  mkTypes   ( void );
static Name  predefinePrim ( String s );


static Tycon linkTycon( String s )
{
    Tycon tc = findTycon(findText(s));
    if (nonNull(tc)) {
        return tc;
    }
    ERRMSG(0) "Prelude does not define standard type \"%s\"", s
    EEND;
}

static Class linkClass( String s )
{
    Class cc = findClass(findText(s));
    if (nonNull(cc)) {
        return cc;
    }
    ERRMSG(0) "Prelude does not define standard class \"%s\"", s
    EEND;
}

static Name linkName( String s )
{
    Name n = findName(findText(s));
    if (nonNull(n)) {
        return n;
    }
    ERRMSG(0) "Prelude does not define standard name \"%s\"", s
    EEND;
}

static Name predefinePrim ( String s )
{
    Name nm;
    Text t = findText(s);
    nm = findName(t);
    if (nonNull(nm)) {
       //fprintf(stderr, "predefinePrim: %s already exists\n", s );
    } else {
       nm = newName(t,NIL);
       name(nm).defn=PREDEFINED;
    }
    return nm;
}

Void linkPreludeTC(void) {              /* Hook to tycons and classes in   */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;
        setCurrModule(modulePrelude);

        QQ(typeChar      )  = linkTycon("Char");
        QQ(typeInt       )  = linkTycon("Int");
#ifdef PROVIDE_INT64
        QQ(typeInt64     )  = linkTycon("Int64");
#endif
#ifdef PROVIDE_INTEGER
        QQ(typeInteger   )  = linkTycon("Integer");
#endif
#ifdef PROVIDE_WORD
        QQ(typeWord      )  = linkTycon("Word");
#endif
#ifdef PROVIDE_ADDR
        QQ(typeAddr      )  = linkTycon("Addr");
#endif
#ifdef PROVIDE_ARRAY
        QQ(typePrimArray )           = linkTycon("PrimArray");
        QQ(typePrimByteArray)        = linkTycon("PrimByteArray");
        QQ(typeRef       )           = linkTycon("Ref");
        QQ(typePrimMutableArray)     = linkTycon("PrimMutableArray");
        QQ(typePrimMutableByteArray) = linkTycon("PrimMutableByteArray");
#endif
        QQ(typeFloat     )  = linkTycon("Float");
        QQ(typeDouble    )  = linkTycon("Double");
#ifdef PROVIDE_STABLE
        QQ(typeStable    )  = linkTycon("StablePtr");
#endif
#ifdef PROVIDE_WEAK
        QQ(typeWeak      )  = linkTycon("Weak");
#endif
#ifdef PROVIDE_FOREIGN
        QQ(typeForeign   )  = linkTycon("ForeignObj");
#endif
#ifdef PROVIDE_CONCURRENT
        QQ(typeThreadId  )  = linkTycon("ThreadId");
        QQ(typeMVar      )  = linkTycon("MVar");
#endif

        QQ(typeBool      )  = linkTycon("Bool");
        QQ(typeST        )  = linkTycon("ST");
        QQ(typeIO        )  = linkTycon("IO");
        QQ(typeException )  = linkTycon("Exception");
        //qqfail QQ(typeList      )  = linkTycon("[]");
        //qqfail QQ(typeUnit      )  = linkTycon("()");
        QQ(typeString    )  = linkTycon("String");
        QQ(typeOrdering  )  = linkTycon("Ordering");

        QQ(classEq       )  = linkClass("Eq");
        QQ(classOrd      )  = linkClass("Ord");
        QQ(classIx       )  = linkClass("Ix");
        QQ(classEnum     )  = linkClass("Enum");
        QQ(classShow     )  = linkClass("Show");
        QQ(classRead     )  = linkClass("Read");
        QQ(classBounded  )  = linkClass("Bounded");
#if EVAL_INSTANCES
        classEval       = linkClass("Eval");
#endif
        QQ(classReal     )  = linkClass("Real");
        QQ(classIntegral )  = linkClass("Integral");
        QQ(classRealFrac )  = linkClass("RealFrac");
        QQ(classRealFloat)  = linkClass("RealFloat");
        QQ(classFractional) = linkClass("Fractional");
        QQ(classFloating )  = linkClass("Floating");
        QQ(classNum      )  = linkClass("Num");
        QQ(classMonad    )  = linkClass("Monad");

        stdDefaults     = NIL;
        stdDefaults     = cons(typeDouble,stdDefaults);
#if DEFAULT_BIGNUM
        stdDefaults     = cons(typeBignum,stdDefaults);
#else
        stdDefaults     = cons(typeInt,stdDefaults);
#endif
        mkTypes();

        QQ(nameMkC       )  = addPrimCfunREP(findText("C#"),1,0,CHAR_REP);
        QQ(nameMkI       )  = addPrimCfunREP(findText("I#"),1,0,INT_REP);
#ifdef PROVIDE_INT64
        QQ(nameMkInt64   )  = addPrimCfunREP(findText("Int64#"),1,0,INT64_REP);
#endif
#ifdef PROVIDE_WORD
        QQ(nameMkW       )  = addPrimCfunREP(findText("W#"),1,0,WORD_REP);
#endif
#ifdef PROVIDE_ADDR
        QQ(nameMkA       )  = addPrimCfunREP(findText("A#"),1,0,ADDR_REP);
#endif
        QQ(nameMkF       )  = addPrimCfunREP(findText("F#"),1,0,FLOAT_REP);
        QQ(nameMkD       )  = addPrimCfunREP(findText("D#"),1,0,DOUBLE_REP);
#ifdef PROVIDE_STABLE
        QQ(nameMkStable  )  = addPrimCfunREP(findText("Stable#"),1,0,STABLE_REP);
#endif

#ifdef PROVIDE_INTEGER
        QQ(nameMkInteger )  = addPrimCfunREP(findText("Integer#"),1,0,0);
#endif
#ifdef PROVIDE_FOREIGN
        QQ(nameMkForeign )  = addPrimCfunREP(findText("Foreign#"),1,0,0);
#endif
#ifdef PROVIDE_WEAK
        QQ(nameMkWeak    )  = addPrimCfunREP(findText("Weak#"),1,0,0);
#endif
#ifdef PROVIDE_ARRAY
        QQ(nameMkPrimArray           ) = addPrimCfunREP(findText("PrimArray#"),1,0,0);
        QQ(nameMkPrimByteArray       ) = addPrimCfunREP(findText("PrimByteArray#"),1,0,0);
        QQ(nameMkRef                 ) = addPrimCfunREP(findText("Ref#"),1,0,0);
        QQ(nameMkPrimMutableArray    ) = addPrimCfunREP(findText("PrimMutableArray#"),1,0,0);
        QQ(nameMkPrimMutableByteArray) = addPrimCfunREP(findText("PrimMutableByteArray#"),1,0,0);
#endif
#ifdef PROVIDE_CONCURRENT
        QQ(nameMkThreadId)  = addPrimCfun(findTextREP("ThreadId#"),1,0,0);
        QQ(nameMkMVar    )  = addPrimCfun(findTextREP("MVar#"),1,0,0);
#endif
        /* The following primitives are referred to in derived instances and
         * hence require types; the following types are a little more general
         * than we might like, but they are the closest we can get without a
         * special datatype class.
         */
        name(nameConCmp).type
            = mkPolyType(starToStar,fn(aVar,fn(aVar,typeOrdering)));
        name(nameEnRange).type
            = mkPolyType(starToStar,fn(boundPair,listof));
        name(nameEnIndex).type
            = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeInt)));
        name(nameEnInRng).type
            = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeBool)));
        name(nameEnToEn).type
            = mkPolyType(starToStar,fn(aVar,fn(typeInt,aVar)));
        name(nameEnFrEn).type
            = mkPolyType(starToStar,fn(aVar,typeInt));
        name(nameEnFrom).type
            = mkPolyType(starToStar,fn(aVar,listof));
        name(nameEnFrTo).type
            = name(nameEnFrTh).type
            = mkPolyType(starToStar,fn(aVar,fn(aVar,listof)));

        name(namePrimSeq).type
            = primType(MONAD_Id, "ab", "b");
        name(namePrimCatch).type
            = primType(MONAD_Id, "aH", "a");
        name(namePrimRaise).type
            = primType(MONAD_Id, "E", "a");
#if EVAL_INSTANCES
        addEvalInst(0,typeArrow,2,NIL); /* Add Eval instances for (->)     */
#endif

        for (i=2; i<=NUM_DTUPLES; i++) {/* Add derived instances of tuples */
#if EVAL_INSTANCES
            addEvalInst(0,mkTuple(i),i,NIL);
#endif
#if DERIVE_EQ
            addTupInst(classEq,i);
#endif
#if DERIVE_ORD
            addTupInst(classOrd,i);
#endif
#if DERIVE_IX
            addTupInst(classIx,i);
#endif
#if DERIVE_SHOW
            addTupInst(classShow,i);
#endif
#if DERIVE_READ
            addTupInst(classRead,i);
#endif
#if DERIVE_BOUNDED
            addTupInst(classBounded,i);
#endif
        }
    }
}

static Void mkTypes ( void )
{
        //qqfail QQ(arrow         ) = fn(aVar,mkOffset(1));
        //qqfail QQ(listof        ) = ap(typeList,aVar);
        QQ(predNum       ) = ap(classNum,aVar);
        QQ(predFractional) = ap(classFractional,aVar);
        QQ(predIntegral  ) = ap(classIntegral,aVar);
        QQ(predMonad     ) = ap(classMonad,aVar);
}

Void linkPreludeCM(void) {              /* Hook to cfuns and mfuns in      */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;
        ////setCurrModule(modulePreludeHugs);
        setCurrModule(modulePrelude);
        /* constructors */
        QQ(nameFalse     )  = linkName("False");
        QQ(nameTrue      )  = linkName("True");
        //qqfail QQ(nameNil       )  = linkName("[]");
        //qqfail QQ(nameCons      )  = linkName(":");
        //qqfail QQ(nameUnit      )  = linkName("()");
        /* members */
        QQ(nameEq        )  = linkName("==");
        QQ(nameFromInt   )  = linkName("fromInt");
        QQ(nameFromInteger) = linkName("fromInteger");
        QQ(nameFromDouble)  = linkName("fromDouble");
#if EVAL_INSTANCES
        nameStrict      = linkName("strict");
        nameSeq         = linkName("seq");
#endif
        QQ(nameReturn    )  = linkName("return");
        QQ(nameBind      )  = linkName(">>=");

        QQ(nameLe        )  = linkName("<=");
        QQ(nameGt        )  = linkName(">");
        QQ(nameShowsPrec )  = linkName("showsPrec");
        QQ(nameReadsPrec )  = linkName("readsPrec");
        QQ(nameEQ        )  = linkName("EQ");
        QQ(nameCompare   )  = linkName("compare");
        QQ(nameMinBnd    )  = linkName("minBound");
        QQ(nameMaxBnd    )  = linkName("maxBound");
        QQ(nameRange     )  = linkName("range");
        QQ(nameIndex     )  = linkName("index");
        QQ(namePlus      )  = linkName("+");
        QQ(nameMult      )  = linkName("*");
        QQ(nameRangeSize )  = linkName("rangeSize");
        QQ(nameInRange   )  = linkName("inRange");
        QQ(nameMinus     )  = linkName("-");
        /* These come before calls to implementPrim */
        for(i=0; i<NUM_TUPLES; ++i) {
            implementTuple(i);
        }
    }
}

Void linkPreludeNames(void) {           /* Hook to names defined in Prelude */
    static Bool initialised = FALSE;
    if (!initialised) {
        Int i;
        initialised = TRUE;

        setCurrModule(modulePrelude);

        /* primops */
        QQ(nameMkIO)          = linkName("primMkIO");
        for (i=0; asmPrimOps[i].name; ++i) {
            Text t = findText(asmPrimOps[i].name);
            Name n = findName(t);
            if (isNull(n)) {
                n = newName(t,NIL);
            }
            name(n).line   = 0;
            name(n).defn   = NIL;
            name(n).type   = primType(asmPrimOps[i].monad,
                                      asmPrimOps[i].args,
                                      asmPrimOps[i].results);
            name(n).arity  = strlen(asmPrimOps[i].args);
            name(n).primop = &(asmPrimOps[i]);
            implementPrim(n);
        }

        /* static(tidyInfix)                        */
        QQ(nameNegate    )    = linkName("negate");
        /* user interface                           */
        QQ(nameRunIO     )    = linkName("primRunIO");
        QQ(namePrint     )    = linkName("print");
        /* typechecker (undefined member functions) */
        //qqfail QQ(nameError     )    = linkName("error");
        /* desugar                                  */
        //qqfail QQ(nameId        )    = linkName("id");
        QQ(nameOtherwise )    = linkName("otherwise");
        QQ(nameUndefined )    = linkName("undefined");
        /* pmc                                      */
#if NPLUSK                      
        namePmSub         = linkName("primPmSub");
#endif                          
        /* translator                               */
        ////nameUnpackString  = linkName("primUnpackString");
        ////namePMFail        = linkName("primPmFail");
        QQ(nameEqChar    )    = linkName("primEqChar");
        QQ(nameEqInt     )    = linkName("primEqInt");
#if !OVERLOADED_CONSTANTS
        QQ(nameEqInteger )    = linkName("primEqInteger");
#endif /* !OVERLOADED_CONSTANTS */
        QQ(nameEqDouble  )    = linkName("primEqDouble");
        QQ(namePmInt     )    = linkName("primPmInt");
        ////namePmInteger     = linkName("primPmInteger");
        ////namePmDouble      = linkName("primPmDouble");
        ////namePmLe          = linkName("primPmLe");
        ////namePmSubtract    = linkName("primPmSubtract");
        ////namePmFromInteger = linkName("primPmFromInteger");
        ////QQ(nameMap       )    = linkName("map");
    }
}


/* ToDo: fix pFun (or eliminate its use) */
#define pFun(n,s)    QQ(n) = predefinePrim(s)

Void linkControl(what)
Int what; {
    switch (what) {
        case RESET   :
        case MARK    : 
                       break;

        case INSTALL : linkControl(RESET);

                       modulePrelude = newModule(textPrelude);
                       setCurrModule(modulePrelude);

                       typeArrow = addPrimTycon(findText("(->)"),
                                                pair(STAR,pair(STAR,STAR)),
                                                2,DATATYPE,NIL);

                       /* newtype and USE_NEWTYPE_FOR_DICTS     */
                       pFun(nameId,             "id");

                       /* desugaring                            */
                       pFun(nameInd,            "_indirect");
                       name(nameInd).number = DFUNNAME;

                       /* pmc                                   */
                       pFun(nameSel,            "_SEL");

                       /* strict constructors                   */
                       pFun(nameFlip,           "flip"     );

                       /* parser                                */
                       pFun(nameFromTo,         "enumFromTo");
                       pFun(nameFromThenTo,     "enumFromThenTo");
                       pFun(nameFrom,           "enumFrom");
                       pFun(nameFromThen,       "enumFromThen");

                       /* deriving                              */
                       pFun(nameApp,            "++");
                       pFun(nameReadParen,      "readParen");
                       pFun(nameShowParen,      "showParen");
                       pFun(nameLex,            "lex");
                       pFun(nameEnToEn,         "toEnumPR"); //not sure
                       pFun(nameEnFrEn,         "fromEnum"); //not sure
                       pFun(nameEnFrom,         "enumFrom"); //not sure
                       pFun(nameEnFrTh,         "enumFromThen"); //not sure
                       pFun(nameEnFrTo,         "enumFromTo"); //not sure
                       pFun(nameEnRange,        "range"); //not sure
                       pFun(nameEnIndex,        "index"); //not sure
                       pFun(nameEnInRng,        "inRange"); //not sure
                       pFun(nameConCmp,         "_concmp"); //very not sure
                       pFun(nameComp,           ".");
                       pFun(nameAnd,            "&&");
                       pFun(nameCompAux,        "primCompAux");
                       pFun(nameMap,            "map");

                       /* implementTagToCon                     */
                       pFun(namePMFail,         "primPmFail");
                       pFun(namePMFailBUG,      "primPmFailBUG");
		       pFun(nameError,          "error");
		       pFun(nameUnpackString,   "primUnpackString");

                       /* hooks for handwritten bytecode */
                       pFun(namePrimSeq,        "primSeq");
                       pFun(namePrimCatch,      "primCatch");
                       pFun(namePrimRaise,      "primRaise");
                       {
                          StgVar vv = mkStgVar(NIL,NIL);
                          Name n = namePrimSeq;
                          name(n).line = 0;
                          name(n).arity = 1;
                          name(n).type = NIL;
                          vv = mkStgVar(NIL,NIL);
                          stgVarInfo(vv) = mkPtr ( asm_BCO_seq() );
                          name(n).stgVar = vv;
                          stgGlobals=cons(pair(n,vv),stgGlobals);
                          namePrimSeq = n;
                       }
                       {
                          StgVar vv = mkStgVar(NIL,NIL);
                          Name n = namePrimCatch;
                          name(n).line = 0;
                          name(n).arity = 2;
                          name(n).type = NIL;
                          stgVarInfo(vv) = mkPtr ( asm_BCO_catch() );
                          name(n).stgVar = vv;
                          stgGlobals=cons(pair(n,vv),stgGlobals);
                       }
                       {
                          StgVar vv = mkStgVar(NIL,NIL);
                          Name n = namePrimRaise;
                          name(n).line = 0;
                          name(n).arity = 1;
                          name(n).type = NIL;
                          stgVarInfo(vv) = mkPtr ( asm_BCO_raise() );
                          name(n).stgVar = vv;
                          stgGlobals=cons(pair(n,vv),stgGlobals);
                       }

                       break;
    }
}
#undef pFun


/*-------------------------------------------------------------------------*/
