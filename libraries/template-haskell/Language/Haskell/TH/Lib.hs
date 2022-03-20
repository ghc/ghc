{-# LANGUAGE Safe #-}

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
        InfoQ, ExpQ, TExpQ, CodeQ, DecQ, DecsQ, ConQ, TypeQ, KindQ,
        TyLitQ, CxtQ, PredQ, DerivClauseQ, MatchQ, ClauseQ, BodyQ, GuardQ,
        StmtQ, RangeQ, SourceStrictnessQ, SourceUnpackednessQ, BangQ,
        BangTypeQ, VarBangTypeQ, StrictTypeQ, VarStrictTypeQ, FieldExpQ, PatQ,
        FieldPatQ, RuleBndrQ, TySynEqnQ, PatSynDirQ, PatSynArgsQ,
        FamilyResultSigQ, DerivStrategyQ,
        TyVarBndrUnit, TyVarBndrSpec,

    -- ** Constructors lifted to 'Q'
    -- *** Literals
        intPrimL, wordPrimL, floatPrimL, doublePrimL, integerL, rationalL,
        charL, stringL, stringPrimL, charPrimL, bytesPrimL, mkBytes,
    -- *** Patterns
        litP, varP, tupP, unboxedTupP, unboxedSumP, conP, uInfixP, parensP,
        infixP, tildeP, bangP, asP, wildP, recP,
        listP, sigP, viewP,
        fieldPat,

    -- *** Pattern Guards
        normalB, guardedB, normalG, normalGE, patG, patGE, match, clause,

    -- *** Expressions
        dyn, varE, unboundVarE, labelE, implicitParamVarE, conE, litE, staticE,
        appE, appTypeE, uInfixE, parensE, infixE, infixApp, sectionL, sectionR,
        lamE, lam1E, lamCaseE, lamCasesE, tupE, unboxedTupE, unboxedSumE, condE,
        multiIfE, letE, caseE, appsE, listE, sigE, recConE, recUpdE, stringE,
        fieldExp, getFieldE, projectionE,
    -- **** Ranges
    fromE, fromThenE, fromToE, fromThenToE,

    -- ***** Ranges with more indirection
    arithSeqE,
    fromR, fromThenR, fromToR, fromThenToR,
    -- **** Statements
    doE, mdoE, compE,
    bindS, letS, noBindS, parS, recS,

    -- *** Types
        forallT, forallVisT, varT, conT, appT, appKindT, arrowT, mulArrowT,
        infixT, uInfixT, promotedInfixT, promotedUInfixT,
        parensT, equalityT, listT, tupleT, unboxedTupleT, unboxedSumT,
        sigT, litT, wildCardT, promotedT, promotedTupleT, promotedNilT,
        promotedConsT, implicitParamT,
    -- **** Type literals
    numTyLit, strTyLit, charTyLit,
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
    plainInvisTV, kindedInvisTV,
    specifiedSpec, inferredSpec,

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
    sigD, kiSigD, standaloneDerivD, standaloneDerivWithStrategyD, defaultSigD,

    -- **** Role annotations
    roleAnnotD,
    -- **** Type Family / Data Family
    dataFamilyD, openTypeFamilyD, closedTypeFamilyD, dataInstD,
    newtypeInstD, tySynInstD,
    tySynEqn, injectivityAnn, noSig, kindSig, tyVarSig,

    -- **** Fixity
    infixLD, infixRD, infixND,

    -- **** Default declaration
    defaultD,

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

    -- **** Implicit Parameters
    implicitParamBindD,

    -- ** Reify
    thisModule,

    -- ** Documentation
    withDecDoc, withDecsDoc, funD_doc, dataD_doc, newtypeD_doc, dataInstD_doc,
    newtypeInstD_doc, patSynD_doc

   ) where

import Language.Haskell.TH.Lib.Internal hiding
  ( tySynD
  , dataD
  , newtypeD
  , classD
  , pragRuleD
  , dataInstD
  , newtypeInstD
  , dataFamilyD
  , openTypeFamilyD
  , closedTypeFamilyD
  , tySynEqn
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

  , doE
  , mdoE
  , tupE
  , unboxedTupE

  , conP

  , Role
  , InjectivityAnn
  )
import qualified Language.Haskell.TH.Lib.Internal as Internal
import Language.Haskell.TH.Syntax

import Control.Applicative ( liftA2 )
import Foreign.ForeignPtr
import Data.Word
import Prelude

-- All definitions below represent the "old" API, since their definitions are
-- different in Language.Haskell.TH.Lib.Internal. Please think carefully before
-- deciding to change the APIs of the functions below, as they represent the
-- public API (as opposed to the Internal module, which has no API promises.)

-------------------------------------------------------------------------------
-- *   Dec

tySynD :: Quote m => Name -> [TyVarBndr ()] -> m Type -> m Dec
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySynD tc tvs rhs1) }

dataD :: Quote m => m Cxt -> Name -> [TyVarBndr ()] -> Maybe Kind -> [m Con] -> [m DerivClause]
      -> m Dec
dataD ctxt tc tvs ksig cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequenceA cons
    derivs1 <- sequenceA derivs
    return (DataD ctxt1 tc tvs ksig cons1 derivs1)

newtypeD :: Quote m => m Cxt -> Name -> [TyVarBndr ()] -> Maybe Kind -> m Con -> [m DerivClause]
         -> m Dec
newtypeD ctxt tc tvs ksig con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    derivs1 <- sequenceA derivs
    return (NewtypeD ctxt1 tc tvs ksig con1 derivs1)

classD :: Quote m => m Cxt -> Name -> [TyVarBndr ()] -> [FunDep] -> [m Dec] -> m Dec
classD ctxt cls tvs fds decs =
  do
    decs1 <- sequenceA decs
    ctxt1 <- ctxt
    return $ ClassD ctxt1 cls tvs fds decs1

pragRuleD :: Quote m => String -> [m RuleBndr] -> m Exp -> m Exp -> Phases -> m Dec
pragRuleD n bndrs lhs rhs phases
  = do
      bndrs1 <- sequenceA bndrs
      lhs1   <- lhs
      rhs1   <- rhs
      return $ PragmaD $ RuleP n Nothing bndrs1 lhs1 rhs1 phases

dataInstD :: Quote m => m Cxt -> Name -> [m Type] -> Maybe Kind -> [m Con] -> [m DerivClause]
          -> m Dec
dataInstD ctxt tc tys ksig cons derivs =
  do
    ctxt1 <- ctxt
    ty1 <- foldl appT (conT tc) tys
    cons1 <- sequenceA cons
    derivs1 <- sequenceA derivs
    return (DataInstD ctxt1 Nothing ty1 ksig cons1 derivs1)

newtypeInstD :: Quote m => m Cxt -> Name -> [m Type] -> Maybe Kind -> m Con -> [m DerivClause]
             -> m Dec
newtypeInstD ctxt tc tys ksig con derivs =
  do
    ctxt1 <- ctxt
    ty1 <- foldl appT (conT tc) tys
    con1  <- con
    derivs1 <- sequenceA derivs
    return (NewtypeInstD ctxt1 Nothing ty1 ksig con1 derivs1)

dataFamilyD :: Quote m => Name -> [TyVarBndr ()] -> Maybe Kind -> m Dec
dataFamilyD tc tvs kind
    = pure $ DataFamilyD tc tvs kind

openTypeFamilyD :: Quote m => Name -> [TyVarBndr ()] -> FamilyResultSig
                -> Maybe InjectivityAnn -> m Dec
openTypeFamilyD tc tvs res inj
    = pure $ OpenTypeFamilyD (TypeFamilyHead tc tvs res inj)

closedTypeFamilyD :: Quote m => Name -> [TyVarBndr ()] -> FamilyResultSig
                  -> Maybe InjectivityAnn -> [m TySynEqn] -> m Dec
closedTypeFamilyD tc tvs result injectivity eqns =
  do eqns1 <- sequenceA eqns
     return (ClosedTypeFamilyD (TypeFamilyHead tc tvs result injectivity) eqns1)

tySynEqn :: Quote m => (Maybe [TyVarBndr ()]) -> m Type -> m Type -> m TySynEqn
tySynEqn tvs lhs rhs =
  do
    lhs1 <- lhs
    rhs1 <- rhs
    return (TySynEqn tvs lhs1 rhs1)

forallC :: Quote m => [TyVarBndr Specificity] -> m Cxt -> m Con -> m Con
forallC ns ctxt con = liftA2 (ForallC ns) ctxt con

-------------------------------------------------------------------------------
-- *   Type

forallT :: Quote m => [TyVarBndr Specificity] -> m Cxt -> m Type -> m Type
forallT tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallT tvars ctxt1 ty1

sigT :: Quote m => m Type -> Kind -> m Type
sigT t k
  = do
      t' <- t
      return $ SigT t' k

-------------------------------------------------------------------------------
-- *   Kind

plainTV :: Name -> TyVarBndr ()
plainTV n = PlainTV n ()

kindedTV :: Name -> Kind -> TyVarBndr ()
kindedTV n k = KindedTV n () k

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

tyVarSig :: TyVarBndr () -> FamilyResultSig
tyVarSig = TyVarSig

-------------------------------------------------------------------------------
-- * Top Level Declarations

derivClause :: Quote m => Maybe DerivStrategy -> [m Pred] -> m DerivClause
derivClause mds p = do
  p' <- cxt p
  return $ DerivClause mds p'

standaloneDerivWithStrategyD :: Quote m => Maybe DerivStrategy -> m Cxt -> m Type -> m Dec
standaloneDerivWithStrategyD mds ctxt ty = do
  ctxt' <- ctxt
  ty'   <- ty
  return $ StandaloneDerivD mds ctxt' ty'

-------------------------------------------------------------------------------
-- * Bytes literals

-- | Create a Bytes datatype representing raw bytes to be embedded into the
-- program/library binary.
--
-- @since 2.16.0.0
mkBytes
   :: ForeignPtr Word8 -- ^ Pointer to the data
   -> Word             -- ^ Offset from the pointer
   -> Word             -- ^ Number of bytes
   -> Bytes
mkBytes = Bytes

-------------------------------------------------------------------------------
-- * Tuple expressions

tupE :: Quote m => [m Exp] -> m Exp
tupE es = do { es1 <- sequenceA es; return (TupE $ map Just es1)}

unboxedTupE :: Quote m => [m Exp] -> m Exp
unboxedTupE es = do { es1 <- sequenceA es; return (UnboxedTupE $ map Just es1)}

-------------------------------------------------------------------------------
-- * Do expressions

doE :: Quote m => [m Stmt] -> m Exp
doE = Internal.doE Nothing

mdoE :: Quote m => [m Stmt] -> m Exp
mdoE = Internal.mdoE Nothing

-------------------------------------------------------------------------------
-- * Patterns

conP :: Quote m => Name -> [m Pat] -> m Pat
conP n xs = Internal.conP n [] xs
