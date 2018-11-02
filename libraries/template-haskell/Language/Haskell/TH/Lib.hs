-- |
-- Language.Haskell.TH.Lib contains lots of useful helper functions for
-- generating and manipulating Template Haskell terms

-- Note: this module mostly re-exports functions from
-- Language.Haskell.TH.Lib.Internal, but if a change occurs to Template
-- Haskell which requires breaking the API offered in this module, we opt to
-- copy the old definition here, and make the changes in
-- Language.Haskell.TH.Lib.Internal. This way, we can retain backwards
-- compatibility while still allowing GHC to make changes as it needs.

module Language.Haskell.TH.Lib (
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

    -- * Library functions
    -- ** Abbreviations
        InfoQ, ExpQ, TExpQ, DecQ, DecsQ, ConQ, TypeQ, KindQ, TyVarBndrQ,
        TyLitQ, CxtQ, PredQ, DerivClauseQ, MatchQ, ClauseQ, BodyQ, GuardQ,
        StmtQ, RangeQ, SourceStrictnessQ, SourceUnpackednessQ, BangQ,
        BangTypeQ, VarBangTypeQ, StrictTypeQ, VarStrictTypeQ, FieldExpQ, PatQ,
        FieldPatQ, RuleBndrQ, TySynEqnQ, PatSynDirQ, PatSynArgsQ,
        FamilyResultSigQ, DerivStrategyQ,

    -- ** Constructors lifted to 'Q'
    -- *** Literals
        intPrimL, wordPrimL, floatPrimL, doublePrimL, integerL, rationalL,
        charL, stringL, stringPrimL, charPrimL,
    -- *** Patterns
        litP, varP, tupP, unboxedTupP, unboxedSumP, conP, uInfixP, parensP,
        infixP, tildeP, bangP, asP, wildP, recP,
        listP, sigP, viewP,
        fieldPat,

    -- *** Pattern Guards
        normalB, guardedB, normalG, normalGE, patG, patGE, match, clause,

    -- *** Expressions
        dyn, varE, unboundVarE, labelE,  conE, litE, appE, appTypeE, uInfixE, parensE,
        staticE, infixE, infixApp, sectionL, sectionR,
        lamE, lam1E, lamCaseE, tupE, unboxedTupE, unboxedSumE, condE, multiIfE,
        letE, caseE, appsE, listE, sigE, recConE, recUpdE, stringE, fieldExp,
    -- **** Ranges
    fromE, fromThenE, fromToE, fromThenToE,

    -- ***** Ranges with more indirection
    arithSeqE,
    fromR, fromThenR, fromToR, fromThenToR,
    -- **** Statements
    doE, compE,
    bindS, letS, noBindS, parS,

    -- *** Types
        forallT, varT, conT, appT, arrowT, infixT, uInfixT, parensT, equalityT,
        listT, tupleT, unboxedTupleT, unboxedSumT, sigT, litT, wildCardT,
        promotedT, promotedTupleT, promotedNilT, promotedConsT,
    -- **** Type literals
    numTyLit, strTyLit,
    -- **** Strictness
    noSourceUnpackedness, sourceNoUnpack, sourceUnpack,
    noSourceStrictness, sourceLazy, sourceStrict,
    isStrict, notStrict, unpacked,
    bang, bangType, varBangType, strictType, varStrictType,
    -- **** Class Contexts
    cxt, classP, equalP,
    -- **** Constructors
    normalC, recC, infixC, forallC, gadtC, recGadtC,

    -- *** Kinds
    varK, conK, tupleK, arrowK, listK, appK, starK, constraintK,

    -- *** Type variable binders
    plainTV, kindedTV,

    -- *** Roles
    nominalR, representationalR, phantomR, inferR,

    -- *** Top Level Declarations
    -- **** Data
    valD, funD, tySynD, dataD, newtypeD,
    derivClause, DerivClause(..),
    stockStrategy, anyclassStrategy, newtypeStrategy,
    viaStrategy, DerivStrategy(..),
    -- **** Class
    classD, instanceD, instanceWithOverlapD, Overlap(..),
    sigD, standaloneDerivD, standaloneDerivWithStrategyD, defaultSigD,

    -- **** Role annotations
    roleAnnotD,
    -- **** Type Family / Data Family
    dataFamilyD, openTypeFamilyD, closedTypeFamilyD, dataInstD,
    newtypeInstD, tySynInstD,
    tySynEqn, injectivityAnn, noSig, kindSig, tyVarSig,

    -- **** Fixity
    infixLD, infixRD, infixND,

    -- **** Foreign Function Interface (FFI)
    cCall, stdCall, cApi, prim, javaScript,
    unsafe, safe, interruptible, forImpD,

    -- **** Functional dependencies
    funDep,

    -- **** Pragmas
    ruleVar, typedRuleVar,
    valueAnnotation, typeAnnotation, moduleAnnotation,
    pragInlD, pragSpecD, pragSpecInlD, pragSpecInstD, pragRuleD, pragAnnD,
    pragLineD, pragCompleteD,

    -- **** Pattern Synonyms
    patSynD, patSynSigD, unidir, implBidir, explBidir, prefixPatSyn,
    infixPatSyn, recordPatSyn,

    -- ** Reify
    thisModule

   ) where

import Language.Haskell.TH.Lib.Internal hiding
  ( tySynD
  , dataD
  , newtypeD
  , classD
  , dataInstD
  , newtypeInstD
  , dataFamilyD
  , openTypeFamilyD
  , closedTypeFamilyD
  , forallC

  , forallT
  , sigT

  , plainTV
  , kindedTV
  , starK
  , constraintK

  , noSig
  , kindSig
  , tyVarSig

  , derivClause
  , standaloneDerivWithStrategyD

  , Role
  , InjectivityAnn
  )
import Language.Haskell.TH.Syntax

import Control.Monad (liftM2)
import Prelude

-- All definitions below represent the "old" API, since their definitions are
-- different in Language.Haskell.TH.Lib.Internal. Please think carefully before
-- deciding to change the APIs of the functions below, as they represent the
-- public API (as opposed to the Internal module, which has no API promises.)

-------------------------------------------------------------------------------
-- *   Dec

tySynD :: Name -> [TyVarBndr] -> TypeQ -> DecQ
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySynD tc tvs rhs1) }

dataD :: CxtQ -> Name -> [TyVarBndr] -> Maybe Kind -> [ConQ] -> [DerivClauseQ]
      -> DecQ
dataD ctxt tc tvs ksig cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    derivs1 <- sequence derivs
    return (DataD ctxt1 tc tvs ksig cons1 derivs1)

newtypeD :: CxtQ -> Name -> [TyVarBndr] -> Maybe Kind -> ConQ -> [DerivClauseQ]
         -> DecQ
newtypeD ctxt tc tvs ksig con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    derivs1 <- sequence derivs
    return (NewtypeD ctxt1 tc tvs ksig con1 derivs1)

classD :: CxtQ -> Name -> [TyVarBndr] -> [FunDep] -> [DecQ] -> DecQ
classD ctxt cls tvs fds decs =
  do
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ ClassD ctxt1 cls tvs fds decs1

dataInstD :: CxtQ -> Name -> [TypeQ] -> Maybe Kind -> [ConQ] -> [DerivClauseQ]
          -> DecQ
dataInstD ctxt tc tys ksig cons derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    cons1 <- sequence cons
    derivs1 <- sequence derivs
    return (DataInstD ctxt1 tc tys1 ksig cons1 derivs1)

newtypeInstD :: CxtQ -> Name -> [TypeQ] -> Maybe Kind -> ConQ -> [DerivClauseQ]
             -> DecQ
newtypeInstD ctxt tc tys ksig con derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    con1  <- con
    derivs1 <- sequence derivs
    return (NewtypeInstD ctxt1 tc tys1 ksig con1 derivs1)

dataFamilyD :: Name -> [TyVarBndr] -> Maybe Kind -> DecQ
dataFamilyD tc tvs kind
    = return $ DataFamilyD tc tvs kind

openTypeFamilyD :: Name -> [TyVarBndr] -> FamilyResultSig
                -> Maybe InjectivityAnn -> DecQ
openTypeFamilyD tc tvs res inj
    = return $ OpenTypeFamilyD (TypeFamilyHead tc tvs res inj)

closedTypeFamilyD :: Name -> [TyVarBndr] -> FamilyResultSig
                  -> Maybe InjectivityAnn -> [TySynEqnQ] -> DecQ
closedTypeFamilyD tc tvs result injectivity eqns =
  do eqns1 <- sequence eqns
     return (ClosedTypeFamilyD (TypeFamilyHead tc tvs result injectivity) eqns1)

forallC :: [TyVarBndr] -> CxtQ -> ConQ -> ConQ
forallC ns ctxt con = liftM2 (ForallC ns) ctxt con

-------------------------------------------------------------------------------
-- *   Type

forallT :: [TyVarBndr] -> CxtQ -> TypeQ -> TypeQ
forallT tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallT tvars ctxt1 ty1

sigT :: TypeQ -> Kind -> TypeQ
sigT t k
  = do
      t' <- t
      return $ SigT t' k

-------------------------------------------------------------------------------
-- *   Kind

plainTV :: Name -> TyVarBndr
plainTV = PlainTV

kindedTV :: Name -> Kind -> TyVarBndr
kindedTV = KindedTV

starK :: Kind
starK = StarT

constraintK :: Kind
constraintK = ConstraintT

-------------------------------------------------------------------------------
-- *   Type family result

noSig :: FamilyResultSig
noSig = NoSig

kindSig :: Kind -> FamilyResultSig
kindSig = KindSig

tyVarSig :: TyVarBndr -> FamilyResultSig
tyVarSig = TyVarSig

-------------------------------------------------------------------------------
-- * Top Level Declarations

derivClause :: Maybe DerivStrategy -> [PredQ] -> DerivClauseQ
derivClause mds p = do
  p' <- cxt p
  return $ DerivClause mds p'

standaloneDerivWithStrategyD :: Maybe DerivStrategy -> CxtQ -> TypeQ -> DecQ
standaloneDerivWithStrategyD mds ctxt ty = do
  ctxt' <- ctxt
  ty'   <- ty
  return $ StandaloneDerivD mds ctxt' ty'
