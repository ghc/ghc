
/* --------------------------------------------------------------------------
 * Load symbols required from the Prelude
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: link.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/02/03 17:08:31 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "Assembler.h" /* for asmPrimOps and AsmReps */

#include "link.h"

Module modulePreludeHugs;

Type typeArrow;                         /* Function spaces                 */

Type typeChar;
Type typeInt;
#ifdef PROVIDE_INT64
Type typeInt64;
#endif
#ifdef PROVIDE_INTEGER
Type typeInteger;
#endif
#ifdef PROVIDE_WORD
Type typeWord;
#endif
#ifdef PROVIDE_ADDR
Type typeAddr;
#endif
#ifdef PROVIDE_ARRAY
Type typePrimArray;            
Type typePrimByteArray;
Type typeRef;                  
Type typePrimMutableArray;     
Type typePrimMutableByteArray; 
#endif
Type typeFloat;
Type typeDouble;
#ifdef PROVIDE_STABLE
Type typeStable;
#endif
#ifdef PROVIDE_WEAK
Type typeWeak;
#endif
#ifdef PROVIDE_FOREIGN
Type typeForeign;
#endif
#ifdef PROVIDE_CONCURRENT
Type typeThreadId;
Type typeMVar;
#endif

Type typeList;
Type typeUnit;
Type typeString;
Type typeBool;
Type typeST;
Type typeIO;
Type typeException;

Class classEq;                          /* `standard' classes              */
Class classOrd;
Class classShow;
Class classRead;
Class classIx;
Class classEnum;
Class classBounded;
#if EVAL_INSTANCES
Class classEval;
#endif

Class classReal;                        /* `numeric' classes               */
Class classIntegral;
Class classRealFrac;
Class classRealFloat;
Class classFractional;
Class classFloating;
Class classNum;

Class classMonad;                       /* Monads and monads with a zero   */
/*Class classMonad0;*/

List stdDefaults;                       /* standard default values         */

Name nameTrue,    nameFalse;            /* primitive boolean constructors  */
Name nameNil,     nameCons;             /* primitive list constructors     */
Name nameUnit;                          /* primitive Unit type constructor */

Name nameEq;    
Name nameFromInt, nameFromDouble;       /* coercion of numerics            */
Name nameFromInteger;
Name nameReturn,  nameBind;             /* for translating monad comps     */
Name nameZero;                          /* for monads with a zero          */
#if EVAL_INSTANCES
Name nameStrict;                        /* Members of class Eval           */
Name nameSeq;   
#endif

Name nameId;
Name nameRunIO;
Name namePrint;

Name nameOtherwise;
Name nameUndefined;                     /* generic undefined value         */
#if NPLUSK
Name namePmSub; 
#endif
Name namePMFail;
Name nameEqChar;
Name nameEqInt;
#if !OVERLOADED_CONSTANTS
Name nameEqInteger;
#endif
Name nameEqDouble;
Name namePmInt;
Name namePmInteger;
Name namePmDouble;
Name namePmLe;
Name namePmSubtract;
Name namePmFromInteger;
Name nameMkIO;
Name nameUnpackString;
Name nameError;
Name nameInd;

Name nameForce;

Name nameAnd;
Name nameHw;
Name nameConCmp;
Name nameCompAux;
Name nameEnFrTh;
Name nameEnFrTo;
Name nameEnFrom;
Name nameEnFrEn;
Name nameEnToEn;
Name nameEnInRng;
Name nameEnIndex;
Name nameEnRange;
Name nameRangeSize;
Name nameComp;
Name nameShowField;
Name nameApp;
Name nameShowParen;
Name nameReadParen;
Name nameLex;
Name nameReadField;
Name nameFlip;
Name nameFromTo;
Name nameFromThen;
Name nameFrom;
Name nameFromThenTo;
Name nameNegate;

/* these names are required before we've had a chance to do the right thing */
Name nameSel;
Name nameUnsafeUnpackCString;

/* constructors used during translation and codegen */
Name nameMkC;                           /* Char#        -> Char           */
Name nameMkI;                           /* Int#         -> Int            */
#ifdef PROVIDE_INT64                                                       
Name nameMkInt64;                       /* Int64#       -> Int64          */
#endif                                                                     
#ifdef PROVIDE_INTEGER                                                     
Name nameMkInteger;                     /* Integer#     -> Integer        */
#endif                                                                     
#ifdef PROVIDE_WORD                                                        
Name nameMkW;                           /* Word#        -> Word           */
#endif                                                                     
#ifdef PROVIDE_ADDR                                                        
Name nameMkA;                           /* Addr#        -> Addr            */
#endif                                                                     
Name nameMkF;                           /* Float#       -> Float           */
Name nameMkD;                           /* Double#      -> Double          */
#ifdef PROVIDE_ARRAY
Name nameMkPrimArray;            
Name nameMkPrimByteArray;
Name nameMkRef;                  
Name nameMkPrimMutableArray;     
Name nameMkPrimMutableByteArray; 
#endif
#ifdef PROVIDE_STABLE
Name nameMkStable;                      /* StablePtr# a -> StablePtr a     */
#endif
#ifdef PROVIDE_WEAK
Name nameMkWeak;                        /* Weak# a      -> Weak a          */
#endif
#ifdef PROVIDE_FOREIGN
Name nameMkForeign;                     /* ForeignObj#  -> ForeignObj      */
#endif
#ifdef PROVIDE_CONCURRENT
Name nameMkThreadId;                    /* ThreadId#    -> ThreadId        */
Name nameMkMVar;                        /* MVar#        -> MVar            */
#endif

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static Tycon linkTycon ( String s );
static Tycon linkClass ( String s );
static Name  linkName  ( String s );
static Void  mkTypes   ();


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

/* ToDo: kill this! */
static Name  predefinePrim ( String s );
static Name  predefinePrim ( String s )
{
    Name nm = newName(findText(s),NIL); 
    name(nm).defn=PREDEFINED;
    return nm;
}

Void linkPreludeTC() {                  /* Hook to tycons and classes in   */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;
        setCurrModule(modulePreludeHugs);

        typeChar        = linkTycon("Char");
        typeInt         = linkTycon("Int");
#ifdef PROVIDE_INT64
        typeInt64       = linkTycon("Int64");
#endif
#ifdef PROVIDE_INTEGER
        typeInteger     = linkTycon("Integer");
#endif
#ifdef PROVIDE_WORD
        typeWord        = linkTycon("Word");
#endif
#ifdef PROVIDE_ADDR
        typeAddr        = linkTycon("Addr");
#endif
#ifdef PROVIDE_ARRAY
        typePrimArray            = linkTycon("PrimArray");
        typePrimByteArray        = linkTycon("PrimByteArray");
        typeRef                  = linkTycon("Ref");
        typePrimMutableArray     = linkTycon("PrimMutableArray");
        typePrimMutableByteArray = linkTycon("PrimMutableByteArray");
#endif
        typeFloat       = linkTycon("Float");
        typeDouble      = linkTycon("Double");
#ifdef PROVIDE_STABLE
        typeStable      = linkTycon("StablePtr");
#endif
#ifdef PROVIDE_WEAK
        typeWeak        = linkTycon("Weak");
#endif
#ifdef PROVIDE_FOREIGN
        typeForeign     = linkTycon("ForeignObj");
#endif
#ifdef PROVIDE_CONCURRENT
        typeThreadId    = linkTycon("ThreadId");
        typeMVar        = linkTycon("MVar");
#endif

        typeBool        = linkTycon("Bool");
        typeST          = linkTycon("ST");
        typeIO          = linkTycon("IO");
        typeException   = linkTycon("Exception");
        typeList        = linkTycon("[]");
        typeUnit        = linkTycon("()");
        typeString      = linkTycon("String");

        classEq         = linkClass("Eq");
        classOrd        = linkClass("Ord");
        classIx         = linkClass("Ix");
        classEnum       = linkClass("Enum");
        classShow       = linkClass("Show");
        classRead       = linkClass("Read");
        classBounded    = linkClass("Bounded");
#if EVAL_INSTANCES
        classEval       = linkClass("Eval");
#endif
        classReal       = linkClass("Real");
        classIntegral   = linkClass("Integral");
        classRealFrac   = linkClass("RealFrac");
        classRealFloat  = linkClass("RealFloat");
        classFractional = linkClass("Fractional");
        classFloating   = linkClass("Floating");
        classNum        = linkClass("Num");
        classMonad      = linkClass("Monad");
        /*classMonad0     = linkClass("MonadZero");*/

        stdDefaults     = NIL;
        stdDefaults     = cons(typeDouble,stdDefaults);
#if DEFAULT_BIGNUM
        stdDefaults     = cons(typeBignum,stdDefaults);
#else
        stdDefaults     = cons(typeInt,stdDefaults);
#endif
        mkTypes();

        nameMkC         = addPrimCfun(findText("C#"),1,0,CHAR_REP);
        nameMkI         = addPrimCfun(findText("I#"),1,0,INT_REP);
#ifdef PROVIDE_INT64
        nameMkInt64     = addPrimCfun(findText("Int64#"),1,0,INT64_REP);
#endif
#ifdef PROVIDE_WORD
        nameMkW         = addPrimCfun(findText("W#"),1,0,WORD_REP);
#endif
#ifdef PROVIDE_ADDR
        nameMkA         = addPrimCfun(findText("A#"),1,0,ADDR_REP);
#endif
        nameMkF         = addPrimCfun(findText("F#"),1,0,FLOAT_REP);
        nameMkD         = addPrimCfun(findText("D#"),1,0,DOUBLE_REP);
#ifdef PROVIDE_STABLE
        nameMkStable    = addPrimCfun(findText("Stable#"),1,0,STABLE_REP);
#endif

#ifdef PROVIDE_INTEGER
        nameMkInteger   = addPrimCfun(findText("Integer#"),1,0,0);
#endif
#ifdef PROVIDE_FOREIGN
        nameMkForeign   = addPrimCfun(findText("Foreign#"),1,0,0);
#endif
#ifdef PROVIDE_WEAK
        nameMkWeak      = addPrimCfun(findText("Weak#"),1,0,0);
#endif
#ifdef PROVIDE_ARRAY
        nameMkPrimArray            = addPrimCfun(findText("PrimArray#"),1,0,0);
        nameMkPrimByteArray        = addPrimCfun(findText("PrimByteArray#"),1,0,0);
        nameMkRef                  = addPrimCfun(findText("Ref#"),1,0,0);
        nameMkPrimMutableArray     = addPrimCfun(findText("PrimMutableArray#"),1,0,0);
        nameMkPrimMutableByteArray = addPrimCfun(findText("PrimMutableByteArray#"),1,0,0);
#endif
#ifdef PROVIDE_CONCURRENT
        nameMkThreadId  = addPrimCfun(findText("ThreadId#"),1,0,0);
        nameMkMVar      = addPrimCfun(findText("MVar#"),1,0,0);
#endif

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

static Void mkTypes()
{
    arrow          = fn(aVar,mkOffset(1));
    listof         = ap(typeList,aVar);
    predNum        = ap(classNum,aVar);
    predFractional = ap(classFractional,aVar);
    predIntegral   = ap(classIntegral,aVar);
    predMonad      = ap(classMonad,aVar);
    /*predMonad0     = ap(classMonad0,aVar);*/
}

Void linkPreludeCM() {                  /* Hook to cfuns and mfuns in      */
    static Bool initialised = FALSE;    /* prelude when first loaded       */
    if (!initialised) {
        Int i;
        initialised = TRUE;
        setCurrModule(modulePreludeHugs);
        /* constructors */
        nameFalse       = linkName("False");
        nameTrue        = linkName("True");
        nameNil         = linkName("[]");
        nameCons        = linkName(":");
        nameUnit        = linkName("()");
        /* members */
        nameEq          = linkName("==");
        nameFromInt     = linkName("fromInt");
        nameFromInteger = linkName("fromInteger");
        nameFromDouble  = linkName("fromDouble");
#if EVAL_INSTANCES
        nameStrict      = linkName("strict");
        nameSeq         = linkName("seq");
#endif
        nameReturn      = linkName("return");
        nameBind        = linkName(">>=");
        nameZero        = linkName("zero");

        /* These come before calls to implementPrim */
        for(i=0; i<NUM_TUPLES; ++i) {
            implementTuple(i);
        }
    }
}

Void linkPreludeNames() {               /* Hook to names defined in Prelude */
    static Bool initialised = FALSE;
    if (!initialised) {
        Int i;
        initialised = TRUE;
        setCurrModule(modulePreludeHugs);

        /* primops */
        nameMkIO          = linkName("primMkIO");
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

        /* user interface                           */
        nameRunIO         = linkName("primRunIO");
        namePrint         = linkName("print");
        /* typechecker (undefined member functions) */
        nameError         = linkName("error");
        /* desugar                                  */
        nameId            = linkName("id");
        nameOtherwise     = linkName("otherwise");
        nameUndefined     = linkName("undefined");
        /* pmc                                      */
#if NPLUSK                      
        namePmSub         = linkName("primPmSub");
#endif                          
        /* translator                               */
        nameUnpackString  = linkName("primUnpackString");
        namePMFail        = linkName("primPmFail");
        nameEqChar        = linkName("primEqChar");
        nameEqInt         = linkName("primEqInt");
#if !OVERLOADED_CONSTANTS
        nameEqInteger     = linkName("primEqInteger");
#endif /* !OVERLOADED_CONSTANTS */
        nameEqDouble      = linkName("primEqDouble");
        namePmInt         = linkName("primPmInt");
        namePmInteger     = linkName("primPmInteger");
        namePmDouble      = linkName("primPmDouble");
        namePmLe          = linkName("primPmLe");
        namePmSubtract    = linkName("primPmSubtract");
        namePmFromInteger = linkName("primPmFromInteger");
    }
}

Void linkControl(what)
Int what; {
    Int  i;

    switch (what) {
        case RESET   :
        case MARK    : 
                       break;

        case INSTALL : linkControl(RESET);

                       modulePreludeHugs = newModule(findText("PreludeBuiltin"));

                       setCurrModule(modulePreludeHugs);

                       typeArrow = addPrimTycon(findText("(->)"),
                                                pair(STAR,pair(STAR,STAR)),
                                                2,DATATYPE,NIL);

                       /* ToDo: fix pFun (or eliminate its use) */
#define pFun(n,s,t)    n = predefinePrim(s)
                       /* newtype and USE_NEWTYPE_FOR_DICTS     */
                       pFun(nameId,             "id",       "id");
                       /* desugaring                            */
                       pFun(nameInd,            "_indirect","error");
                       name(nameInd).number = DFUNNAME;
                       /* pmc                                   */
                       pFun(nameSel,            "_SEL",     "sel");
                       /* strict constructors                   */
                       pFun(nameForce,          "primForce","id");
                       /* implementTagToCon                     */
                       pFun(namePMFail,         "primPmFail","primPmFail");
		       pFun(nameError,          "error","error");
		       pFun(nameUnpackString, "primUnpackString", "primUnpackString");
#undef pFun

                       break;
    }
}

/*-------------------------------------------------------------------------*/


#if 0
--## this stuff from 98
--## 
--## 
--## Void linkPreludeTC() {                  /* Hook to tycons and classes in   */
--##     if (isNull(typeBool)) {             /* prelude when first loaded       */
--##         Int i;
--## 
--##         typeBool     = findTycon(findText("Bool"));
--##         typeChar     = findTycon(findText("Char"));
--##         typeString   = findTycon(findText("String"));
--##         typeInt      = findTycon(findText("Int"));
--##         typeInteger  = findTycon(findText("Integer"));
--##         typeDouble   = findTycon(findText("Double"));
--##         typeAddr     = findTycon(findText("Addr"));
--##         typeMaybe    = findTycon(findText("Maybe"));
--##         typeOrdering = findTycon(findText("Ordering"));
--##         if (isNull(typeBool) || isNull(typeChar)   || isNull(typeString)  ||
--##             isNull(typeInt)  || isNull(typeDouble) || isNull(typeInteger) ||
--##             isNull(typeAddr) || isNull(typeMaybe)  || isNull(typeOrdering)) {
--##             ERRMSG(0) "Prelude does not define standard types"
--##             EEND;
--##         }
--##         stdDefaults  = cons(typeInteger,cons(typeDouble,NIL));
--## 
--##         classEq      = findClass(findText("Eq"));
--##         classOrd     = findClass(findText("Ord"));
--##         classIx      = findClass(findText("Ix"));
--##         classEnum    = findClass(findText("Enum"));
--##         classShow    = findClass(findText("Show"));
--##         classRead    = findClass(findText("Read"));
--## #if EVAL_INSTANCES
--##         classEval    = findClass(findText("Eval"));
--## #endif
--##         classBounded = findClass(findText("Bounded"));
--##         if (isNull(classEq)   || isNull(classOrd) || isNull(classRead) ||
--##             isNull(classShow) || isNull(classIx)  || isNull(classEnum) ||
--## #if EVAL_INSTANCES
--##             isNull(classEval) ||
--## #endif
--##             isNull(classBounded)) {
--##             ERRMSG(0) "Prelude does not define standard classes"
--##             EEND;
--##         }
--## 
--##         classReal       = findClass(findText("Real"));
--##         classIntegral   = findClass(findText("Integral"));
--##         classRealFrac   = findClass(findText("RealFrac"));
--##         classRealFloat  = findClass(findText("RealFloat"));
--##         classFractional = findClass(findText("Fractional"));
--##         classFloating   = findClass(findText("Floating"));
--##         classNum        = findClass(findText("Num"));
--##         if (isNull(classReal)       || isNull(classIntegral)  ||
--##             isNull(classRealFrac)   || isNull(classRealFloat) ||
--##             isNull(classFractional) || isNull(classFloating)  ||
--##             isNull(classNum)) {
--##             ERRMSG(0) "Prelude does not define numeric classes"
--##             EEND;
--##         }
--##         predNum         = ap(classNum,aVar);
--##         predFractional  = ap(classFractional,aVar);
--##         predIntegral    = ap(classIntegral,aVar);
--## 
--##         classMonad  = findClass(findText("Monad"));
--##         if (isNull(classMonad)) {
--##             ERRMSG(0) "Prelude does not define Monad class"
--##             EEND;
--##         }
--##         predMonad  = ap(classMonad,aVar);
--## 
--## #if IO_MONAD
--##         {   Type typeIO = findTycon(findText("IO"));
--##             if (isNull(typeIO)) {
--##                 ERRMSG(0) "Prelude does not define IO monad constructor"
--##                 EEND;
--##             }
--##             typeProgIO = ap(typeIO,aVar);
--##         }
--## #endif
--## 
--##         /* The following primitives are referred to in derived instances and
--##          * hence require types; the following types are a little more general
--##          * than we might like, but they are the closest we can get without a
--##          * special datatype class.
--##          */
--##         name(nameConCmp).type
--##             = mkPolyType(starToStar,fn(aVar,fn(aVar,typeOrdering)));
--##         name(nameEnRange).type
--##             = mkPolyType(starToStar,fn(boundPair,listof));
--##         name(nameEnIndex).type
--##             = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeInt)));
--##         name(nameEnInRng).type
--##             = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeBool)));
--##         name(nameEnToEn).type
--##             = mkPolyType(starToStar,fn(aVar,fn(typeInt,aVar)));
--##         name(nameEnFrEn).type
--##             = mkPolyType(starToStar,fn(aVar,typeInt));
--##         name(nameEnFrom).type
--##             = mkPolyType(starToStar,fn(aVar,listof));
--##         name(nameEnFrTo).type
--##             = name(nameEnFrTh).type
--##             = mkPolyType(starToStar,fn(aVar,fn(aVar,listof)));
--## 
--## #if EVAL_INSTANCES
--##         addEvalInst(0,typeArrow,2,NIL); /* Add Eval instances for builtins */
--##         addEvalInst(0,typeList,1,NIL);
--##         addEvalInst(0,typeUnit,0,NIL);
--## #endif
--##         for (i=2; i<=NUM_DTUPLES; i++) {/* Add derived instances of tuples */
--## #if EVAL_INSTANCES
--##             addEvalInst(0,mkTuple(i),i,NIL);
--## #endif
--##             addTupInst(classEq,i);
--##             addTupInst(classOrd,i);
--##             addTupInst(classShow,i);
--##             addTupInst(classRead,i);
--##             addTupInst(classIx,i);
--##         }
--##     }
--## }
--## 
--## 
--## static Void linkPreludeCM() {           /* Hook to cfuns and mfuns in      */
--##     if (isNull(nameFalse)) {            /* prelude when first loaded       */
--##         nameFalse   = findName(findText("False"));
--##         nameTrue    = findName(findText("True"));
--##         nameJust    = findName(findText("Just"));
--##         nameNothing = findName(findText("Nothing"));
--##         nameLeft    = findName(findText("Left"));
--##         nameRight   = findName(findText("Right"));
--##         nameLT      = findName(findText("LT"));
--##         nameEQ      = findName(findText("EQ"));
--##         nameGT      = findName(findText("GT"));
--##         if (isNull(nameFalse) || isNull(nameTrue)    ||
--##             isNull(nameJust)  || isNull(nameNothing) ||
--##             isNull(nameLeft)  || isNull(nameRight)   ||
--##             isNull(nameLT)    || isNull(nameEQ)      || isNull(nameGT)) {
--##             ERRMSG(0) "Prelude does not define standard constructors"
--##             EEND;
--##         }
--## 
--##         nameFromInt     = findName(findText("fromInt"));
--##         nameFromInteger = findName(findText("fromInteger"));
--##         nameFromDouble  = findName(findText("fromDouble"));
--##         nameEq          = findName(findText("=="));
--##         nameCompare     = findName(findText("compare"));
--##         nameLe          = findName(findText("<="));
--##         nameGt          = findName(findText(">"));
--##         nameShowsPrec   = findName(findText("showsPrec"));
--##         nameReadsPrec   = findName(findText("readsPrec"));
--##         nameIndex       = findName(findText("index"));
--##         nameInRange     = findName(findText("inRange"));
--##         nameRange       = findName(findText("range"));
--##         nameMult        = findName(findText("*"));
--##         namePlus        = findName(findText("+"));
--##         nameMinBnd      = findName(findText("minBound"));
--##         nameMaxBnd      = findName(findText("maxBound"));
--## #if EVAL_INSTANCES
--##         nameStrict      = findName(findText("strict"));
--##         nameSeq         = findName(findText("seq"));
--## #endif
--##         nameReturn      = findName(findText("return"));
--##         nameBind        = findName(findText(">>="));
--##         nameMFail       = findName(findText("fail"));
--##         if (isNull(nameFromInt)   || isNull(nameFromDouble)  ||
--##             isNull(nameEq)        || isNull(nameCompare)     ||
--##             isNull(nameLe)        || isNull(nameGt)          ||
--##             isNull(nameShowsPrec) || isNull(nameReadsPrec)   ||
--##             isNull(nameIndex)     || isNull(nameInRange)     ||
--##             isNull(nameRange)     || isNull(nameMult)        ||
--##             isNull(namePlus)      || isNull(nameFromInteger) ||
--##             isNull(nameMinBnd)    || isNull(nameMaxBnd)      ||
--## #if EVAL_INSTANCES
--##             isNull(nameStrict)    || isNull(nameSeq)         ||
--## #endif
--##             isNull(nameReturn)    || isNull(nameBind)        ||
--##             isNull(nameMFail)) {
--##             ERRMSG(0) "Prelude does not define standard members"
--##             EEND;
--##         }
--##     }
--## }
--## 
#endif
