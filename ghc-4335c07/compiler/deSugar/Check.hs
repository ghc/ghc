{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Pattern Matching Coverage Checking.
-}

{-# LANGUAGE CPP, GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Check (
        -- Checking and printing
        checkSingle, checkMatches, isAnyPmCheckEnabled,

        -- See Note [Type and Term Equality Propagation]
        genCaseTmCs1, genCaseTmCs2,

        -- Pattern-match-specific type operations
        pmIsClosedType, pmTopNormaliseType_maybe
    ) where

#include "HsVersions.h"

import GhcPrelude

import TmOracle
import Unify( tcMatchTy )
import BasicTypes
import DynFlags
import HsSyn
import TcHsSyn
import Id
import ConLike
import Name
import FamInstEnv
import TysPrim (tYPETyCon)
import TysWiredIn
import TyCon
import SrcLoc
import Util
import Outputable
import FastString
import DataCon
import HscTypes (CompleteMatch(..))

import DsMonad
import TcSimplify    (tcCheckSatisfiability)
import TcType        (toTcType, isStringTy, isIntTy, isWordTy)
import Bag
import ErrUtils
import Var           (EvVar)
import TyCoRep
import Type
import UniqSupply
import DsGRHSs       (isTrueLHsExpr)
import Maybes        ( expectJust )

import Data.List     (find)
import Data.Maybe    (isJust, fromMaybe)
import Control.Monad (forM, when, forM_)
import Coercion
import TcEvidence
import IOEnv
import qualified Data.Semigroup as Semi

import ListT (ListT(..), fold, select)

{-
This module checks pattern matches for:
\begin{enumerate}
  \item Equations that are redundant
  \item Equations with inaccessible right-hand-side
  \item Exhaustiveness
\end{enumerate}

The algorithm is based on the paper:

  "GADTs Meet Their Match:
     Pattern-matching Warnings That Account for GADTs, Guards, and Laziness"

    http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf

%************************************************************************
%*                                                                      *
                     Pattern Match Check Types
%*                                                                      *
%************************************************************************
-}

-- We use the non-determinism monad to apply the algorithm to several
-- possible sets of constructors. Users can specify complete sets of
-- constructors by using COMPLETE pragmas.
-- The algorithm only picks out constructor
-- sets deep in the bowels which makes a simpler `mapM` more difficult to
-- implement. The non-determinism is only used in one place, see the ConVar
-- case in `pmCheckHd`.

type PmM a = ListT DsM a

liftD :: DsM a -> PmM a
liftD m = ListT $ \sk fk -> m >>= \a -> sk a fk

-- Pick the first match complete covered match or otherwise the "best" match.
-- The best match is the one with the least uncovered clauses, ties broken
-- by the number of inaccessible clauses followed by number of redundant
-- clauses.
--
-- This is specified in the
-- "Disambiguating between multiple ``COMPLETE`` pragmas" section of the
-- users' guide. If you update the implementation of this function, make sure
-- to update that section of the users' guide as well.
getResult :: PmM PmResult -> DsM PmResult
getResult ls
  = do { res <- fold ls goM (pure Nothing)
       ; case res of
            Nothing -> panic "getResult is empty"
            Just a  -> return a }
  where
    goM :: PmResult -> DsM (Maybe PmResult) -> DsM (Maybe PmResult)
    goM mpm dpm = do { pmr <- dpm
                     ; return $ Just $ go pmr mpm }

    -- Careful not to force unecessary results
    go :: Maybe PmResult -> PmResult -> PmResult
    go Nothing rs = rs
    go (Just old@(PmResult prov rs (UncoveredPatterns us) is)) new
      | null us && null rs && null is = old
      | otherwise =
        let PmResult prov' rs' (UncoveredPatterns us') is' = new
        in case compareLength us us'
                `mappend` (compareLength is is')
                `mappend` (compareLength rs rs')
                `mappend` (compare prov prov') of
              GT  -> new
              EQ  -> new
              LT  -> old
    go (Just (PmResult _ _ (TypeOfUncovered _) _)) _new
      = panic "getResult: No inhabitation candidates"

data PatTy = PAT | VA -- Used only as a kind, to index PmPat

-- The *arity* of a PatVec [p1,..,pn] is
-- the number of p1..pn that are not Guards

data PmPat :: PatTy -> * where
  PmCon  :: { pm_con_con     :: ConLike
            , pm_con_arg_tys :: [Type]
            , pm_con_tvs     :: [TyVar]
            , pm_con_dicts   :: [EvVar]
            , pm_con_args    :: [PmPat t] } -> PmPat t
            -- For PmCon arguments' meaning see @ConPatOut@ in hsSyn/HsPat.hs
  PmVar  :: { pm_var_id   :: Id } -> PmPat t
  PmLit  :: { pm_lit_lit  :: PmLit } -> PmPat t -- See Note [Literals in PmPat]
  PmNLit :: { pm_lit_id   :: Id
            , pm_lit_not  :: [PmLit] } -> PmPat 'VA
  PmGrd  :: { pm_grd_pv   :: PatVec
            , pm_grd_expr :: PmExpr  } -> PmPat 'PAT

-- data T a where
--     MkT :: forall p q. (Eq p, Ord q) => p -> q -> T [p]
-- or  MkT :: forall p q r. (Eq p, Ord q, [p] ~ r) => p -> q -> T r

type Pattern = PmPat 'PAT -- ^ Patterns
type ValAbs  = PmPat 'VA  -- ^ Value Abstractions

type PatVec = [Pattern]             -- ^ Pattern Vectors
data ValVec = ValVec [ValAbs] Delta -- ^ Value Vector Abstractions

-- | Term and type constraints to accompany each value vector abstraction.
-- For efficiency, we store the term oracle state instead of the term
-- constraints. TODO: Do the same for the type constraints?
data Delta = MkDelta { delta_ty_cs :: Bag EvVar
                     , delta_tm_cs :: TmState }

type ValSetAbs = [ValVec]  -- ^ Value Set Abstractions
type Uncovered = ValSetAbs

-- Instead of keeping the whole sets in memory, we keep a boolean for both the
-- covered and the divergent set (we store the uncovered set though, since we
-- want to print it). For both the covered and the divergent we have:
--
--   True <=> The set is non-empty
--
-- hence:
--  C = True             ==> Useful clause (no warning)
--  C = False, D = True  ==> Clause with inaccessible RHS
--  C = False, D = False ==> Redundant clause

data Covered = Covered | NotCovered
  deriving Show

instance Outputable Covered where
  ppr (Covered) = text "Covered"
  ppr (NotCovered) = text "NotCovered"

-- Like the or monoid for booleans
-- Covered = True, Uncovered = False
instance Semi.Semigroup Covered where
  Covered <> _ = Covered
  _ <> Covered = Covered
  NotCovered <> NotCovered = NotCovered

instance Monoid Covered where
  mempty = NotCovered
  mappend = (Semi.<>)

data Diverged = Diverged | NotDiverged
  deriving Show

instance Outputable Diverged where
  ppr Diverged = text "Diverged"
  ppr NotDiverged = text "NotDiverged"

instance Semi.Semigroup Diverged where
  Diverged <> _ = Diverged
  _ <> Diverged = Diverged
  NotDiverged <> NotDiverged = NotDiverged

instance Monoid Diverged where
  mempty = NotDiverged
  mappend = (Semi.<>)

-- | When we learned that a given match group is complete
data Provenance =
                  FromBuiltin -- ^  From the original definition of the type
                              --    constructor.
                | FromComplete -- ^ From a user-provided @COMPLETE@ pragma
  deriving (Show, Eq, Ord)

instance Outputable Provenance where
  ppr  = text . show

instance Semi.Semigroup Provenance where
  FromComplete <> _ = FromComplete
  _ <> FromComplete = FromComplete
  _ <> _ = FromBuiltin

instance Monoid Provenance where
  mempty = FromBuiltin
  mappend = (Semi.<>)

data PartialResult = PartialResult {
                        presultProvenance :: Provenance
                         -- keep track of provenance because we don't want
                         -- to warn about redundant matches if the result
                         -- is contaminated with a COMPLETE pragma
                      , presultCovered :: Covered
                      , presultUncovered :: Uncovered
                      , presultDivergent :: Diverged }

instance Outputable PartialResult where
  ppr (PartialResult prov c vsa d)
           = text "PartialResult" <+> ppr prov <+> ppr c
                                  <+> ppr d <+> ppr vsa


instance Semi.Semigroup PartialResult where
  (PartialResult prov1 cs1 vsa1 ds1)
    <> (PartialResult prov2 cs2 vsa2 ds2)
      = PartialResult (prov1 Semi.<> prov2)
                      (cs1 Semi.<> cs2)
                      (vsa1 Semi.<> vsa2)
                      (ds1 Semi.<> ds2)


instance Monoid PartialResult where
  mempty = PartialResult mempty mempty [] mempty
  mappend = (Semi.<>)

-- newtype ChoiceOf a = ChoiceOf [a]

-- | Pattern check result
--
-- * Redundant clauses
-- * Not-covered clauses (or their type, if no pattern is available)
-- * Clauses with inaccessible RHS
--
-- More details about the classification of clauses into useful, redundant
-- and with inaccessible right hand side can be found here:
--
--     https://ghc.haskell.org/trac/ghc/wiki/PatternMatchCheck
--
data PmResult =
  PmResult {
      pmresultProvenance   :: Provenance
    , pmresultRedundant    :: [Located [LPat GhcTc]]
    , pmresultUncovered    :: UncoveredCandidates
    , pmresultInaccessible :: [Located [LPat GhcTc]] }

-- | Either a list of patterns that are not covered, or their type, in case we
-- have no patterns at hand. Not having patterns at hand can arise when
-- handling EmptyCase expressions, in two cases:
--
-- * The type of the scrutinee is a trivially inhabited type (like Int or Char)
-- * The type of the scrutinee cannot be reduced to WHNF.
--
-- In both these cases we have no inhabitation candidates for the type at hand,
-- but we don't want to issue just a wildcard as missing. Instead, we print a
-- type annotated wildcard, so that the user knows what kind of patterns is
-- expected (e.g. (_ :: Int), or (_ :: F Int), where F Int does not reduce).
data UncoveredCandidates = UncoveredPatterns Uncovered
                         | TypeOfUncovered Type

-- | The empty pattern check result
emptyPmResult :: PmResult
emptyPmResult = PmResult FromBuiltin [] (UncoveredPatterns []) []

-- | Non-exhaustive empty case with unknown/trivial inhabitants
uncoveredWithTy :: Type -> PmResult
uncoveredWithTy ty = PmResult FromBuiltin [] (TypeOfUncovered ty) []

{-
%************************************************************************
%*                                                                      *
       Entry points to the checker: checkSingle and checkMatches
%*                                                                      *
%************************************************************************
-}

-- | Check a single pattern binding (let)
checkSingle :: DynFlags -> DsMatchContext -> Id -> Pat GhcTc -> DsM ()
checkSingle dflags ctxt@(DsMatchContext _ locn) var p = do
  tracePmD "checkSingle" (vcat [ppr ctxt, ppr var, ppr p])
  mb_pm_res <- tryM (getResult (checkSingle' locn var p))
  case mb_pm_res of
    Left  _   -> warnPmIters dflags ctxt
    Right res -> dsPmWarn dflags ctxt res

-- | Check a single pattern binding (let)
checkSingle' :: SrcSpan -> Id -> Pat GhcTc -> PmM PmResult
checkSingle' locn var p = do
  liftD resetPmIterDs -- set the iter-no to zero
  fam_insts <- liftD dsGetFamInstEnvs
  clause    <- liftD $ translatePat fam_insts p
  missing   <- mkInitialUncovered [var]
  tracePm "checkSingle: missing" (vcat (map pprValVecDebug missing))
                                 -- no guards
  PartialResult prov cs us ds <- runMany (pmcheckI clause []) missing
  let us' = UncoveredPatterns us
  return $ case (cs,ds) of
    (Covered,  _    )         -> PmResult prov [] us' [] -- useful
    (NotCovered, NotDiverged) -> PmResult prov m  us' [] -- redundant
    (NotCovered, Diverged )   -> PmResult prov [] us' m  -- inaccessible rhs
  where m = [L locn [L locn p]]

-- | Check a matchgroup (case, functions, etc.)
checkMatches :: DynFlags -> DsMatchContext
             -> [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> DsM ()
checkMatches dflags ctxt vars matches = do
  tracePmD "checkMatches" (hang (vcat [ppr ctxt
                               , ppr vars
                               , text "Matches:"])
                               2
                               (vcat (map ppr matches)))
  mb_pm_res <- tryM $ getResult $ case matches of
    -- Check EmptyCase separately
    -- See Note [Checking EmptyCase Expressions]
    [] | [var] <- vars -> checkEmptyCase' var
    _normal_match      -> checkMatches' vars matches
  case mb_pm_res of
    Left  _   -> warnPmIters dflags ctxt
    Right res -> dsPmWarn dflags ctxt res

-- | Check a matchgroup (case, functions, etc.). To be called on a non-empty
-- list of matches. For empty case expressions, use checkEmptyCase' instead.
checkMatches' :: [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> PmM PmResult
checkMatches' vars matches
  | null matches = panic "checkMatches': EmptyCase"
  | otherwise = do
      liftD resetPmIterDs -- set the iter-no to zero
      missing    <- mkInitialUncovered vars
      tracePm "checkMatches: missing" (vcat (map pprValVecDebug missing))
      (prov, rs,us,ds) <- go matches missing
      return $ PmResult {
                   pmresultProvenance   = prov
                 , pmresultRedundant    = map hsLMatchToLPats rs
                 , pmresultUncovered    = UncoveredPatterns us
                 , pmresultInaccessible = map hsLMatchToLPats ds }
  where
    go :: [LMatch GhcTc (LHsExpr GhcTc)] -> Uncovered
       -> PmM (Provenance
              , [LMatch GhcTc (LHsExpr GhcTc)]
              , Uncovered
              , [LMatch GhcTc (LHsExpr GhcTc)])
    go []     missing = return (mempty, [], missing, [])
    go (m:ms) missing = do
      tracePm "checMatches': go" (ppr m $$ ppr missing)
      fam_insts          <- liftD dsGetFamInstEnvs
      (clause, guards)   <- liftD $ translateMatch fam_insts m
      r@(PartialResult prov cs missing' ds)
        <- runMany (pmcheckI clause guards) missing
      tracePm "checMatches': go: res" (ppr r)
      (ms_prov, rs, final_u, is)  <- go ms missing'
      let final_prov = prov `mappend` ms_prov
      return $ case (cs, ds) of
        -- useful
        (Covered,  _    )        -> (final_prov,  rs, final_u,   is)
        -- redundant
        (NotCovered, NotDiverged) -> (final_prov, m:rs, final_u,is)
        -- inaccessible
        (NotCovered, Diverged )   -> (final_prov,  rs, final_u, m:is)

    hsLMatchToLPats :: LMatch id body -> Located [LPat id]
    hsLMatchToLPats (L l (Match { m_pats = pats })) = L l pats

-- | Check an empty case expression. Since there are no clauses to process, we
--   only compute the uncovered set. See Note [Checking EmptyCase Expressions]
--   for details.
checkEmptyCase' :: Id -> PmM PmResult
checkEmptyCase' var = do
  tm_css <- map toComplex . bagToList <$> liftD getTmCsDs
  case tmOracle initialTmState tm_css of
    Just tm_state -> do
      ty_css        <- liftD getDictsDs
      fam_insts     <- liftD dsGetFamInstEnvs
      mb_candidates <- inhabitationCandidates fam_insts (idType var)
      case mb_candidates of
        -- Inhabitation checking failed / the type is trivially inhabited
        Left ty -> return (uncoveredWithTy ty)

        -- A list of inhabitant candidates is available: Check for each
        -- one for the satisfiability of the constraints it gives rise to.
        Right candidates -> do
          missing_m <- flip concatMapM candidates $ \(va,tm_ct,ty_cs) -> do
            let all_ty_cs = unionBags ty_cs ty_css
            sat_ty <- tyOracle all_ty_cs
            return $ case (sat_ty, tmOracle tm_state (tm_ct:tm_css)) of
              (True, Just tm_state') -> [(va, all_ty_cs, tm_state')]
              _non_sat               -> []
          let mkValVec (va,all_ty_cs,tm_state')
                = ValVec [va] (MkDelta all_ty_cs tm_state')
              uncovered = UncoveredPatterns (map mkValVec missing_m)
          return $ if null missing_m
            then emptyPmResult
            else PmResult FromBuiltin [] uncovered []
    Nothing -> return emptyPmResult

-- | Returns 'True' if the argument 'Type' is a fully saturated application of
-- a closed type constructor.
--
-- Closed type constructors are those with a fixed right hand side, as
-- opposed to e.g. associated types. These are of particular interest for
-- pattern-match coverage checking, because GHC can exhaustively consider all
-- possible forms that values of a closed type can take on.
--
-- Note that this function is intended to be used to check types of value-level
-- patterns, so as a consequence, the 'Type' supplied as an argument to this
-- function should be of kind @Type@.
pmIsClosedType :: Type -> Bool
pmIsClosedType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args)
             | is_algebraic_like tc && not (isFamilyTyCon tc)
             -> ASSERT2( ty_args `lengthIs` tyConArity tc, ppr ty ) True
      _other -> False
  where
    -- This returns True for TyCons which /act like/ algebraic types.
    -- (See "Type#type_classification" for what an algebraic type is.)
    --
    -- This is qualified with \"like\" because of a particular special
    -- case: TYPE (the underlyind kind behind Type, among others). TYPE
    -- is conceptually a datatype (and thus algebraic), but in practice it is
    -- a primitive builtin type, so we must check for it specially.
    --
    -- NB: it makes sense to think of TYPE as a closed type in a value-level,
    -- pattern-matching context. However, at the kind level, TYPE is certainly
    -- not closed! Since this function is specifically tailored towards pattern
    -- matching, however, it's OK to label TYPE as closed.
    is_algebraic_like :: TyCon -> Bool
    is_algebraic_like tc = isAlgTyCon tc || tc == tYPETyCon

pmTopNormaliseType_maybe :: FamInstEnvs -> Type -> Maybe (Type, [DataCon], Type)
-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
--
-- Behaves exactly like `topNormaliseType_maybe`, but instead of returning a
-- coercion, it returns useful information for issuing pattern matching
-- warnings. See Note [Type normalisation for EmptyCase] for details.
pmTopNormaliseType_maybe env typ
  = do ((ty_f,tm_f), ty) <- topNormaliseTypeX stepper comb typ
       return (eq_src_ty ty (typ : ty_f [ty]), tm_f [], ty)
  where
    -- Find the first type in the sequence of rewrites that is a data type,
    -- newtype, or a data family application (not the representation tycon!).
    -- This is the one that is equal (in source Haskell) to the initial type.
    -- If none is found in the list, then all of them are type family
    -- applications, so we simply return the last one, which is the *simplest*.
    eq_src_ty :: Type -> [Type] -> Type
    eq_src_ty ty tys = maybe ty id (find is_closed_or_data_family tys)

    is_closed_or_data_family :: Type -> Bool
    is_closed_or_data_family ty = pmIsClosedType ty || isDataFamilyAppType ty

    -- For efficiency, represent both lists as difference lists.
    -- comb performs the concatenation, for both lists.
    comb (tyf1, tmf1) (tyf2, tmf2) = (tyf1 . tyf2, tmf1 . tmf2)

    stepper = newTypeStepper `composeSteppers` tyFamStepper

    -- A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
    -- a loop. If it would fall into a loop, it produces 'NS_Abort'.
    newTypeStepper :: NormaliseStepper ([Type] -> [Type],[DataCon] -> [DataCon])
    newTypeStepper rec_nts tc tys
      | Just (ty', _co) <- instNewTyCon_maybe tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> let tyf = ((TyConApp tc tys):)
                               tmf = ((tyConSingleDataCon tc):)
                           in  NS_Step rec_nts' ty' (tyf, tmf)
          Nothing       -> NS_Abort
      | otherwise
      = NS_Done

    tyFamStepper :: NormaliseStepper ([Type] -> [Type], [DataCon] -> [DataCon])
    tyFamStepper rec_nts tc tys  -- Try to step a type/data family
      = let (_args_co, ntys) = normaliseTcArgs env Representational tc tys in
          -- NB: It's OK to use normaliseTcArgs here instead of
          -- normalise_tc_args (which takes the LiftingContext described
          -- in Note [Normalising types]) because the reduceTyFamApp below
          -- works only at top level. We'll never recur in this function
          -- after reducing the kind of a bound tyvar.

        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (_co, rhs) -> NS_Step rec_nts rhs ((rhs:), id)
          _               -> NS_Done

{- Note [Type normalisation for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EmptyCase is an exception for pattern matching, since it is strict. This means
that it boils down to checking whether the type of the scrutinee is inhabited.
Function pmTopNormaliseType_maybe gets rid of the outermost type function/data
family redex and newtypes, in search of an algebraic type constructor, which is
easier to check for inhabitation.

It returns 3 results instead of one, because there are 2 subtle points:
1. Newtypes are isomorphic to the underlying type in core but not in the source
   language,
2. The representational data family tycon is used internally but should not be
   shown to the user

Hence, if pmTopNormaliseType_maybe env ty = Just (src_ty, dcs, core_ty), then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of data constructors "skipped", every time we normalise a
      newtype to it's core representation, we keep track of the source data
      constructor.
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType_maybe env ty = Just (src_ty, dcs, core_ty)
      implies
        topNormaliseType_maybe env ty = Just (co, core_ty)
      for some coercion co.

To see how all cases come into play, consider the following example:

  data family T a :: *
  data instance T Int = T1 | T2 Bool
  -- Which gives rise to FC:
  --   data T a
  --   data R:TInt = T1 | T2 Bool
  --   axiom ax_ti : T Int ~R R:TInt

  newtype G1 = MkG1 (T Int)
  newtype G2 = MkG2 G1

  type instance F Int  = F Char
  type instance F Char = G2

In this case pmTopNormaliseType_maybe env (F Int) results in

  Just (G2, [MkG2,MkG1], R:TInt)

Which means that in source Haskell:
  - G2 is equivalent to F Int (in contrast, G1 isn't).
  - if (x : R:TInt) then (MkG2 (MkG1 x) : F Int).
-}

-- | Generate all inhabitation candidates for a given type. The result is
-- either (Left ty), if the type cannot be reduced to a closed algebraic type
-- (or if it's one trivially inhabited, like Int), or (Right candidates), if it
-- can. In this case, the candidates are the signature of the tycon, each one
-- accompanied by the term- and type- constraints it gives rise to.
-- See also Note [Checking EmptyCase Expressions]
inhabitationCandidates :: FamInstEnvs -> Type
                       -> PmM (Either Type [(ValAbs, ComplexEq, Bag EvVar)])
inhabitationCandidates fam_insts ty
  = case pmTopNormaliseType_maybe fam_insts ty of
      Just (src_ty, dcs, core_ty) -> alts_to_check src_ty core_ty dcs
      Nothing                     -> alts_to_check ty     ty      []
  where
    -- All these types are trivially inhabited
    trivially_inhabited = [ charTyCon, doubleTyCon, floatTyCon
                          , intTyCon, wordTyCon, word8TyCon ]

    -- Note: At the moment we leave all the typing and constraint fields of
    -- PmCon empty, since we know that they are not gonna be used. Is the
    -- right-thing-to-do to actually create them, even if they are never used?
    build_tm :: ValAbs -> [DataCon] -> ValAbs
    build_tm = foldr (\dc e -> PmCon (RealDataCon dc) [] [] [] [e])

    -- Inhabitation candidates, using the result of pmTopNormaliseType_maybe
    alts_to_check :: Type -> Type -> [DataCon]
                  -> PmM (Either Type [(ValAbs, ComplexEq, Bag EvVar)])
    alts_to_check src_ty core_ty dcs = case splitTyConApp_maybe core_ty of
      Just (tc, _)
        | tc `elem` trivially_inhabited -> case dcs of
            []    -> return (Left src_ty)
            (_:_) -> do var <- liftD $ mkPmId (toTcType core_ty)
                        let va = build_tm (PmVar var) dcs
                        return $ Right [(va, mkIdEq var, emptyBag)]

        | pmIsClosedType core_ty -> liftD $ do
            var  <- mkPmId (toTcType core_ty) -- it would be wrong to unify x
            alts <- mapM (mkOneConFull var . RealDataCon) (tyConDataCons tc)
            return $ Right [(build_tm va dcs, eq, cs) | (va, eq, cs) <- alts]
      -- For other types conservatively assume that they are inhabited.
      _other -> return (Left src_ty)

{- Note [Checking EmptyCase Expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Empty case expressions are strict on the scrutinee. That is, `case x of {}`
will force argument `x`. Hence, `checkMatches` is not sufficient for checking
empty cases, because it assumes that the match is not strict (which is true
for all other cases, apart from EmptyCase). This gave rise to #10746. Instead,
we do the following:

1. We normalise the outermost type family redex, data family redex or newtype,
   using pmTopNormaliseType_maybe (in types/FamInstEnv.hs). This computes 3
   things:
   (a) A normalised type src_ty, which is equal to the type of the scrutinee in
       source Haskell (does not normalise newtypes or data families)
   (b) The actual normalised type core_ty, which coincides with the result
       topNormaliseType_maybe. This type is not necessarily equal to the input
       type in source Haskell. And this is precicely the reason we compute (a)
       and (c): the reasoning happens with the underlying types, but both the
       patterns and types we print should respect newtypes and also show the
       family type constructors and not the representation constructors.

   (c) A list of all newtype data constructors dcs, each one corresponding to a
       newtype rewrite performed in (b).

   For an example see also Note [Type normalisation for EmptyCase]
   in types/FamInstEnv.hs.

2. Function checkEmptyCase' performs the check:
   - If core_ty is not an algebraic type, then we cannot check for
     inhabitation, so we emit (_ :: src_ty) as missing, conservatively assuming
     that the type is inhabited.
   - If core_ty is an algebraic type, then we unfold the scrutinee to all
     possible constructor patterns, using inhabitationCandidates, and then
     check each one for constraint satisfiability, same as we for normal
     pattern match checking.

%************************************************************************
%*                                                                      *
              Transform source syntax to *our* syntax
%*                                                                      *
%************************************************************************
-}

-- -----------------------------------------------------------------------
-- * Utilities

nullaryConPattern :: ConLike -> Pattern
-- Nullary data constructor and nullary type constructor
nullaryConPattern con =
  PmCon { pm_con_con = con, pm_con_arg_tys = []
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = [] }
{-# INLINE nullaryConPattern #-}

truePattern :: Pattern
truePattern = nullaryConPattern (RealDataCon trueDataCon)
{-# INLINE truePattern #-}

-- | A fake guard pattern (True <- _) used to represent cases we cannot handle
fake_pat :: Pattern
fake_pat = PmGrd { pm_grd_pv   = [truePattern]
                 , pm_grd_expr = PmExprOther EWildPat }
{-# INLINE fake_pat #-}

-- | Check whether a guard pattern is generated by the checker (unhandled)
isFakeGuard :: [Pattern] -> PmExpr -> Bool
isFakeGuard [PmCon { pm_con_con = RealDataCon c }] (PmExprOther EWildPat)
  | c == trueDataCon = True
  | otherwise        = False
isFakeGuard _pats _e = False

-- | Generate a `canFail` pattern vector of a specific type
mkCanFailPmPat :: Type -> DsM PatVec
mkCanFailPmPat ty = do
  var <- mkPmVar ty
  return [var, fake_pat]

vanillaConPattern :: ConLike -> [Type] -> PatVec -> Pattern
-- ADT constructor pattern => no existentials, no local constraints
vanillaConPattern con arg_tys args =
  PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = args }
{-# INLINE vanillaConPattern #-}

-- | Create an empty list pattern of a given type
nilPattern :: Type -> Pattern
nilPattern ty =
  PmCon { pm_con_con = RealDataCon nilDataCon, pm_con_arg_tys = [ty]
        , pm_con_tvs = [], pm_con_dicts = []
        , pm_con_args = [] }
{-# INLINE nilPattern #-}

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = RealDataCon consDataCon
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = [], pm_con_dicts = []
                               , pm_con_args = xs++ys }]
{-# INLINE mkListPatVec #-}

-- | Create a (non-overloaded) literal pattern
mkLitPattern :: HsLit GhcTc -> Pattern
mkLitPattern lit = PmLit { pm_lit_lit = PmSLit lit }
{-# INLINE mkLitPattern #-}

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into of (PmPat Id)

translatePat :: FamInstEnvs -> Pat GhcTc -> DsM PatVec
translatePat fam_insts pat = case pat of
  WildPat ty  -> mkPmVars [ty]
  VarPat  id  -> return [PmVar (unLoc id)]
  ParPat p    -> translatePat fam_insts (unLoc p)
  LazyPat _   -> mkPmVars [hsPatType pat] -- like a variable

  -- ignore strictness annotations for now
  BangPat p   -> translatePat fam_insts (unLoc p)

  AsPat lid p -> do
     -- Note [Translating As Patterns]
    ps <- translatePat fam_insts (unLoc p)
    let [e] = map vaToPmExpr (coercePatVec ps)
        g   = PmGrd [PmVar (unLoc lid)] e
    return (ps ++ [g])

  SigPatOut p _ty -> translatePat fam_insts (unLoc p)

  -- See Note [Translate CoPats]
  CoPat wrapper p ty
    | isIdHsWrapper wrapper                   -> translatePat fam_insts p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts p
    | otherwise -> do
        ps      <- translatePat fam_insts p
        (xp,xe) <- mkPmId2Forms ty
        let g = mkGuard ps (mkHsWrap wrapper (unLoc xe))
        return [xp,g]

  -- (n + k)  ===>   x (True <- x >= k) (n <- x-k)
  NPlusKPat (L _ _n) _k1 _k2 _ge _minus ty -> mkCanFailPmPat ty

  -- (fun -> pat)   ===>   x (pat <- fun x)
  ViewPat lexpr lpat arg_ty -> do
    ps <- translatePat fam_insts (unLoc lpat)
    -- See Note [Guards and Approximation]
    case all cantFailPattern ps of
      True  -> do
        (xp,xe) <- mkPmId2Forms arg_ty
        let g = mkGuard ps (HsApp lexpr xe)
        return [xp,g]
      False -> mkCanFailPmPat arg_ty

  -- list
  ListPat ps ty Nothing -> do
    foldr (mkListPatVec ty) [nilPattern ty]
      <$> translatePatVec fam_insts (map unLoc ps)

  -- overloaded list
  ListPat lpats elem_ty (Just (pat_ty, _to_list))
    | Just e_ty <- splitListTyConApp_maybe pat_ty
    , (_, norm_elem_ty) <- normaliseType fam_insts Nominal elem_ty
         -- elem_ty is frequently something like
         -- `Item [Int]`, but we prefer `Int`
    , norm_elem_ty `eqType` e_ty ->
        -- We have to ensure that the element types are exactly the same.
        -- Otherwise, one may give an instance IsList [Int] (more specific than
        -- the default IsList [a]) with a different implementation for `toList'
        translatePat fam_insts (ListPat lpats e_ty Nothing)
      -- See Note [Guards and Approximation]
    | otherwise -> mkCanFailPmPat pat_ty

  ConPatOut { pat_con     = L _ con
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_dicts   = dicts
            , pat_args    = ps } -> do
    groups <- allCompleteMatches con arg_tys
    case groups of
      [] -> mkCanFailPmPat (conLikeResTy con arg_tys)
      _  -> do
        args <- translateConPatVec fam_insts arg_tys ex_tvs con ps
        return [PmCon { pm_con_con     = con
                      , pm_con_arg_tys = arg_tys
                      , pm_con_tvs     = ex_tvs
                      , pm_con_dicts   = dicts
                      , pm_con_args    = args }]

  NPat (L _ ol) mb_neg _eq ty -> translateNPat fam_insts ol mb_neg ty

  LitPat lit
      -- If it is a string then convert it to a list of characters
    | HsString src s <- lit ->
        foldr (mkListPatVec charTy) [nilPattern charTy] <$>
          translatePatVec fam_insts (map (LitPat . HsChar src) (unpackFS s))
    | otherwise -> return [mkLitPattern lit]

  PArrPat ps ty -> do
    tidy_ps <- translatePatVec fam_insts (map unLoc ps)
    let fake_con = RealDataCon (parrFakeCon (length ps))
    return [vanillaConPattern fake_con [ty] (concat tidy_ps)]

  TuplePat ps boxity tys -> do
    tidy_ps <- translatePatVec fam_insts (map unLoc ps)
    let tuple_con = RealDataCon (tupleDataCon boxity (length ps))
    return [vanillaConPattern tuple_con tys (concat tidy_ps)]

  SumPat p alt arity ty -> do
    tidy_p <- translatePat fam_insts (unLoc p)
    let sum_con = RealDataCon (sumDataCon alt arity)
    return [vanillaConPattern sum_con ty tidy_p]

  -- --------------------------------------------------------------------------
  -- Not supposed to happen
  ConPatIn  {} -> panic "Check.translatePat: ConPatIn"
  SplicePat {} -> panic "Check.translatePat: SplicePat"
  SigPatIn  {} -> panic "Check.translatePat: SigPatIn"

-- | Translate an overloaded literal (see `tidyNPat' in deSugar/MatchLit.hs)
translateNPat :: FamInstEnvs
              -> HsOverLit GhcTc -> Maybe (SyntaxExpr GhcTc) -> Type
              -> DsM PatVec
translateNPat fam_insts (OverLit val False _ ty) mb_neg outer_ty
  | not type_change, isStringTy ty, HsIsString src s <- val, Nothing <- mb_neg
  = translatePat fam_insts (LitPat (HsString src s))
  | not type_change, isIntTy    ty, HsIntegral i <- val
  = translatePat fam_insts
                 (LitPat $ case mb_neg of
                             Nothing -> HsInt def i
                             Just _  -> HsInt def (negateIntegralLit i))
  | not type_change, isWordTy   ty, HsIntegral i <- val
  = translatePat fam_insts
                 (LitPat $ case mb_neg of
                             Nothing -> HsWordPrim (il_text i) (il_value i)
                             Just _  -> let ni = negateIntegralLit i in
                                        HsWordPrim (il_text ni) (il_value ni))
  where
    type_change = not (outer_ty `eqType` ty)

translateNPat _ ol mb_neg _
  = return [PmLit { pm_lit_lit = PmOLit (isJust mb_neg) ol }]

-- | Translate a list of patterns (Note: each pattern is translated
-- to a pattern vector but we do not concatenate the results).
translatePatVec :: FamInstEnvs -> [Pat GhcTc] -> DsM [PatVec]
translatePatVec fam_insts pats = mapM (translatePat fam_insts) pats

-- | Translate a constructor pattern
translateConPatVec :: FamInstEnvs -> [Type] -> [TyVar]
                   -> ConLike -> HsConPatDetails GhcTc -> DsM PatVec
translateConPatVec fam_insts _univ_tys _ex_tvs _ (PrefixCon ps)
  = concat <$> translatePatVec fam_insts (map unLoc ps)
translateConPatVec fam_insts _univ_tys _ex_tvs _ (InfixCon p1 p2)
  = concat <$> translatePatVec fam_insts (map unLoc [p1,p2])
translateConPatVec fam_insts  univ_tys  ex_tvs c (RecCon (HsRecFields fs _))
    -- Nothing matched. Make up some fresh term variables
  | null fs        = mkPmVars arg_tys
    -- The data constructor was not defined using record syntax. For the
    -- pattern to be in record syntax it should be empty (e.g. Just {}).
    -- So just like the previous case.
  | null orig_lbls = ASSERT(null matched_lbls) mkPmVars arg_tys
    -- Some of the fields appear, in the original order (there may be holes).
    -- Generate a simple constructor pattern and make up fresh variables for
    -- the rest of the fields
  | matched_lbls `subsetOf` orig_lbls
  = ASSERT(orig_lbls `equalLength` arg_tys)
      let translateOne (lbl, ty) = case lookup lbl matched_pats of
            Just p  -> translatePat fam_insts p
            Nothing -> mkPmVars [ty]
      in  concatMapM translateOne (zip orig_lbls arg_tys)
    -- The fields that appear are not in the correct order. Make up fresh
    -- variables for all fields and add guards after matching, to force the
    -- evaluation in the correct order.
  | otherwise = do
      arg_var_pats    <- mkPmVars arg_tys
      translated_pats <- forM matched_pats $ \(x,pat) -> do
        pvec <- translatePat fam_insts pat
        return (x, pvec)

      let zipped = zip orig_lbls [ x | PmVar x <- arg_var_pats ]
          guards = map (\(name,pvec) -> case lookup name zipped of
                            Just x  -> PmGrd pvec (PmExprVar (idName x))
                            Nothing -> panic "translateConPatVec: lookup")
                       translated_pats

      return (arg_var_pats ++ guards)
  where
    -- The actual argument types (instantiated)
    arg_tys = conLikeInstOrigArgTys c (univ_tys ++ mkTyVarTys ex_tvs)

    -- Some label information
    orig_lbls    = map flSelector $ conLikeFieldLabels c
    matched_pats = [ (getName (unLoc (hsRecFieldId x)), unLoc (hsRecFieldArg x))
                   | L _ x <- fs]
    matched_lbls = [ name | (name, _pat) <- matched_pats ]

    subsetOf :: Eq a => [a] -> [a] -> Bool
    subsetOf []     _  = True
    subsetOf (_:_)  [] = False
    subsetOf (x:xs) (y:ys)
      | x == y    = subsetOf    xs  ys
      | otherwise = subsetOf (x:xs) ys

-- Translate a single match
translateMatch :: FamInstEnvs -> LMatch GhcTc (LHsExpr GhcTc)
               -> DsM (PatVec,[PatVec])
translateMatch fam_insts (L _ (Match { m_pats = lpats, m_grhss = grhss })) = do
  pats'   <- concat <$> translatePatVec fam_insts pats
  guards' <- mapM (translateGuards fam_insts) guards
  return (pats', guards')
  where
    extractGuards :: LGRHS GhcTc (LHsExpr GhcTc) -> [GuardStmt GhcTc]
    extractGuards (L _ (GRHS gs _)) = map unLoc gs

    pats   = map unLoc lpats
    guards = map extractGuards (grhssGRHSs grhss)

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to PmPats (Pattern)

-- | Translate a list of guard statements to a pattern vector
translateGuards :: FamInstEnvs -> [GuardStmt GhcTc] -> DsM PatVec
translateGuards fam_insts guards = do
  all_guards <- concat <$> mapM (translateGuard fam_insts) guards
  return (replace_unhandled all_guards)
  -- It should have been (return all_guards) but it is too expressive.
  -- Since the term oracle does not handle all constraints we generate,
  -- we (hackily) replace all constraints the oracle cannot handle with a
  -- single one (we need to know if there is a possibility of falure).
  -- See Note [Guards and Approximation] for all guard-related approximations
  -- we implement.
  where
    replace_unhandled :: PatVec -> PatVec
    replace_unhandled gv
      | any_unhandled gv = fake_pat : [ p | p <- gv, shouldKeep p ]
      | otherwise        = gv

    any_unhandled :: PatVec -> Bool
    any_unhandled gv = any (not . shouldKeep) gv

    shouldKeep :: Pattern -> Bool
    shouldKeep p
      | PmVar {} <- p      = True
      | PmCon {} <- p      = singleConstructor (pm_con_con p)
                             && all shouldKeep (pm_con_args p)
    shouldKeep (PmGrd pv e)
      | all shouldKeep pv  = True
      | isNotPmExprOther e = True  -- expensive but we want it
    shouldKeep _other_pat  = False -- let the rest..

-- | Check whether a pattern can fail to match
cantFailPattern :: Pattern -> Bool
cantFailPattern p
  | PmVar {} <- p = True
  | PmCon {} <- p = singleConstructor (pm_con_con p)
                    && all cantFailPattern (pm_con_args p)
cantFailPattern (PmGrd pv _e)
                  = all cantFailPattern pv
cantFailPattern _ = False

-- | Translate a guard statement to Pattern
translateGuard :: FamInstEnvs -> GuardStmt GhcTc -> DsM PatVec
translateGuard fam_insts guard = case guard of
  BodyStmt   e _ _ _ -> translateBoolGuard e
  LetStmt      binds -> translateLet (unLoc binds)
  BindStmt p e _ _ _ -> translateBind fam_insts p e
  LastStmt        {} -> panic "translateGuard LastStmt"
  ParStmt         {} -> panic "translateGuard ParStmt"
  TransStmt       {} -> panic "translateGuard TransStmt"
  RecStmt         {} -> panic "translateGuard RecStmt"
  ApplicativeStmt {} -> panic "translateGuard ApplicativeLastStmt"

-- | Translate let-bindings
translateLet :: HsLocalBinds GhcTc -> DsM PatVec
translateLet _binds = return []

-- | Translate a pattern guard
translateBind :: FamInstEnvs -> LPat GhcTc -> LHsExpr GhcTc -> DsM PatVec
translateBind fam_insts (L _ p) e = do
  ps <- translatePat fam_insts p
  return [mkGuard ps (unLoc e)]

-- | Translate a boolean guard
translateBoolGuard :: LHsExpr GhcTc -> DsM PatVec
translateBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- PatVec for efficiency
  | otherwise = return [mkGuard [truePattern] (unLoc e)]

{- Note [Guards and Approximation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if the algorithm is really expressive, the term oracle we use is not.
Hence, several features are not translated *properly* but we approximate.
The list includes:

1. View Patterns
----------------
A view pattern @(f -> p)@ should be translated to @x (p <- f x)@. The term
oracle does not handle function applications so we know that the generated
constraints will not be handled at the end. Hence, we distinguish between two
cases:
  a) Pattern @p@ cannot fail. Then this is just a binding and we do the *right
     thing*.
  b) Pattern @p@ can fail. This means that when checking the guard, we will
     generate several cases, with no useful information. E.g.:

       h (f -> [a,b]) = ...
       h x ([a,b] <- f x) = ...

       uncovered set = { [x |> { False ~ (f x ~ [])            }]
                       , [x |> { False ~ (f x ~ (t1:[]))       }]
                       , [x |> { False ~ (f x ~ (t1:t2:t3:t4)) }] }

     So we have two problems:
       1) Since we do not print the constraints in the general case (they may
          be too many), the warning will look like this:

            Pattern match(es) are non-exhaustive
            In an equation for `h':
                Patterns not matched:
                    _
                    _
                    _
          Which is not short and not more useful than a single underscore.
       2) The size of the uncovered set increases a lot, without gaining more
          expressivity in our warnings.

     Hence, in this case, we replace the guard @([a,b] <- f x)@ with a *dummy*
     @fake_pat@: @True <- _@. That is, we record that there is a possibility
     of failure but we minimize it to a True/False. This generates a single
     warning and much smaller uncovered sets.

2. Overloaded Lists
-------------------
An overloaded list @[...]@ should be translated to @x ([...] <- toList x)@. The
problem is exactly like above, as its solution. For future reference, the code
below is the *right thing to do*:

   ListPat lpats elem_ty (Just (pat_ty, to_list))
     otherwise -> do
       (xp, xe) <- mkPmId2Forms pat_ty
       ps       <- translatePatVec (map unLoc lpats)
       let pats = foldr (mkListPatVec elem_ty) [nilPattern elem_ty] ps
           g    = mkGuard pats (HsApp (noLoc to_list) xe)
       return [xp,g]

3. Overloaded Literals
----------------------
The case with literals is a bit different. a literal @l@ should be translated
to @x (True <- x == from l)@. Since we want to have better warnings for
overloaded literals as it is a very common feature, we treat them differently.
They are mainly covered in Note [Undecidable Equality on Overloaded Literals]
in PmExpr.

4. N+K Patterns & Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An n+k pattern (n+k) should be translated to @x (True <- x >= k) (n <- x-k)@.
Since the only pattern of the three that causes failure is guard @(n <- x-k)@,
and has two possible outcomes. Hence, there is no benefit in using a dummy and
we implement the proper thing. Pattern synonyms are simply not implemented yet.
Hence, to be conservative, we generate a dummy pattern, assuming that the
pattern can fail.

5. Actual Guards
----------------
During translation, boolean guards and pattern guards are translated properly.
Let bindings though are omitted by function @translateLet@. Since they are lazy
bindings, we do not actually want to generate a (strict) equality (like we do
in the pattern bind case). Hence, we safely drop them.

Additionally, top-level guard translation (performed by @translateGuards@)
replaces guards that cannot be reasoned about (like the ones we described in
1-4) with a single @fake_pat@ to record the possibility of failure to match.

Note [Translate CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns `CoPat`
efficiently, which gave rise to #11276. The original approach translated
`CoPat`s:

    pat |> co    ===>    x (pat <- (e |> co))

Instead, we now check whether the coercion is a hole or if it is just refl, in
which case we can drop it. Unfortunately, data families generate useful
coercions so guards are still generated in these cases and checking data
families is not really efficient.

%************************************************************************
%*                                                                      *
                 Utilities for Pattern Match Checking
%*                                                                      *
%************************************************************************
-}

-- ----------------------------------------------------------------------------
-- * Basic utilities

-- | Get the type out of a PmPat. For guard patterns (ps <- e) we use the type
-- of the first (or the single -WHEREVER IT IS- valid to use?) pattern
pmPatType :: PmPat p -> Type
pmPatType (PmCon { pm_con_con = con, pm_con_arg_tys = tys })
  = conLikeResTy con tys
pmPatType (PmVar  { pm_var_id  = x }) = idType x
pmPatType (PmLit  { pm_lit_lit = l }) = pmLitType l
pmPatType (PmNLit { pm_lit_id  = x }) = idType x
pmPatType (PmGrd  { pm_grd_pv  = pv })
  = ASSERT(patVecArity pv == 1) (pmPatType p)
  where Just p = find ((==1) . patternArity) pv

-- | Generate a value abstraction for a given constructor (generate
-- fresh variables of the appropriate type for arguments)
mkOneConFull :: Id -> ConLike -> DsM (ValAbs, ComplexEq, Bag EvVar)
--  *  x :: T tys, where T is an algebraic data type
--     NB: in the case of a data family, T is the *representation* TyCon
--     e.g.   data instance T (a,b) = T1 a b
--       leads to
--            data TPair a b = T1 a b  -- The "representation" type
--       It is TPair, not T, that is given to mkOneConFull
--
--  * 'con' K is a constructor of data type T
--
-- After instantiating the universal tyvars of K we get
--          K tys :: forall bs. Q => s1 .. sn -> T tys
--
-- Results: ValAbs:          K (y1::s1) .. (yn::sn)
--          ComplexEq:       x ~ K y1..yn
--          [EvVar]:         Q
mkOneConFull x con = do
  let res_ty  = idType x
      (univ_tvs, ex_tvs, eq_spec, thetas, _req_theta , arg_tys, con_res_ty)
        = conLikeFullSig con
      tc_args = tyConAppArgs res_ty
      subst1  = case con of
                  RealDataCon {} -> zipTvSubst univ_tvs tc_args
                  PatSynCon {}   -> expectJust "mkOneConFull" (tcMatchTy con_res_ty res_ty)
                                    -- See Note [Pattern synonym result type] in PatSyn

  (subst, ex_tvs') <- cloneTyVarBndrs subst1 ex_tvs <$> getUniqueSupplyM

  -- Fresh term variables (VAs) as arguments to the constructor
  arguments <-  mapM mkPmVar (substTys subst arg_tys)
  -- All constraints bound by the constructor (alpha-renamed)
  let theta_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  evvars <- mapM (nameType "pm") theta_cs
  let con_abs  = PmCon { pm_con_con     = con
                       , pm_con_arg_tys = tc_args
                       , pm_con_tvs     = ex_tvs'
                       , pm_con_dicts   = evvars
                       , pm_con_args    = arguments }
  return (con_abs, (PmExprVar (idName x), vaToPmExpr con_abs), listToBag evvars)

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

-- | Create a guard pattern
mkGuard :: PatVec -> HsExpr GhcTc -> Pattern
mkGuard pv e
  | all cantFailPattern pv = PmGrd pv expr
  | PmExprOther {} <- expr = fake_pat
  | otherwise              = PmGrd pv expr
  where
    expr = hsExprToPmExpr e

-- | Create a term equality of the form: `(False ~ (x ~ lit))`
mkNegEq :: Id -> PmLit -> ComplexEq
mkNegEq x l = (falsePmExpr, PmExprVar (idName x) `PmExprEq` PmExprLit l)
{-# INLINE mkNegEq #-}

-- | Create a term equality of the form: `(x ~ lit)`
mkPosEq :: Id -> PmLit -> ComplexEq
mkPosEq x l = (PmExprVar (idName x), PmExprLit l)
{-# INLINE mkPosEq #-}

-- | Create a term equality of the form: `(x ~ x)`
-- (always discharged by the term oracle)
mkIdEq :: Id -> ComplexEq
mkIdEq x = (PmExprVar name, PmExprVar name)
  where name = idName x
{-# INLINE mkIdEq #-}

-- | Generate a variable pattern of a given type
mkPmVar :: Type -> DsM (PmPat p)
mkPmVar ty = PmVar <$> mkPmId ty
{-# INLINE mkPmVar #-}

-- | Generate many variable patterns, given a list of types
mkPmVars :: [Type] -> DsM PatVec
mkPmVars tys = mapM mkPmVar tys
{-# INLINE mkPmVars #-}

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> DsM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "$pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalId name ty)

-- | Generate a fresh term variable of a given and return it in two forms:
-- * A variable pattern
-- * A variable expression
mkPmId2Forms :: Type -> DsM (Pattern, LHsExpr GhcTc)
mkPmId2Forms ty = do
  x <- mkPmId ty
  return (PmVar x, noLoc (HsVar (noLoc x)))

-- ----------------------------------------------------------------------------
-- * Converting between Value Abstractions, Patterns and PmExpr

-- | Convert a value abstraction an expression
vaToPmExpr :: ValAbs -> PmExpr
vaToPmExpr (PmCon  { pm_con_con = c, pm_con_args = ps })
  = PmExprCon c (map vaToPmExpr ps)
vaToPmExpr (PmVar  { pm_var_id  = x }) = PmExprVar (idName x)
vaToPmExpr (PmLit  { pm_lit_lit = l }) = PmExprLit l
vaToPmExpr (PmNLit { pm_lit_id  = x }) = PmExprVar (idName x)

-- | Convert a pattern vector to a list of value abstractions by dropping the
-- guards (See Note [Translating As Patterns])
coercePatVec :: PatVec -> [ValAbs]
coercePatVec pv = concatMap coercePmPat pv

-- | Convert a pattern to a list of value abstractions (will be either an empty
-- list if the pattern is a guard pattern, or a singleton list in all other
-- cases) by dropping the guards (See Note [Translating As Patterns])
coercePmPat :: Pattern -> [ValAbs]
coercePmPat (PmVar { pm_var_id  = x }) = [PmVar { pm_var_id  = x }]
coercePmPat (PmLit { pm_lit_lit = l }) = [PmLit { pm_lit_lit = l }]
coercePmPat (PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
                   , pm_con_tvs = tvs, pm_con_dicts = dicts
                   , pm_con_args = args })
  = [PmCon { pm_con_con  = con, pm_con_arg_tys = arg_tys
           , pm_con_tvs  = tvs, pm_con_dicts = dicts
           , pm_con_args = coercePatVec args }]
coercePmPat (PmGrd {}) = [] -- drop the guards

-- | Check whether a data constructor is the only way to construct
-- a data type.
singleConstructor :: ConLike -> Bool
singleConstructor (RealDataCon dc) =
  case tyConDataCons (dataConTyCon dc) of
    [_] -> True
    _   -> False
singleConstructor _ = False

-- | For a given conlike, finds all the sets of patterns which could
-- be relevant to that conlike by consulting the result type.
--
-- These come from two places.
--  1. From data constructors defined with the result type constructor.
--  2. From `COMPLETE` pragmas which have the same type as the result
--     type constructor.
allCompleteMatches :: ConLike -> [Type] -> DsM [(Provenance, [ConLike])]
allCompleteMatches cl tys = do
  let fam = case cl of
           RealDataCon dc ->
            [(FromBuiltin, map RealDataCon (tyConDataCons (dataConTyCon dc)))]
           PatSynCon _    -> []

  pragmas <- case splitTyConApp_maybe (conLikeResTy cl tys) of
              Just (tc, _) -> dsGetCompleteMatches tc
              Nothing -> return []
  let fams cm = fmap (FromComplete,) $
                mapM dsLookupConLike (completeMatchConLikes cm)
  from_pragma <- mapM fams pragmas

  let final_groups = fam ++ from_pragma
  tracePmD "allCompleteMatches" (ppr final_groups)
  return final_groups

-- -----------------------------------------------------------------------
-- * Types and constraints

newEvVar :: Name -> Type -> EvVar
newEvVar name ty = mkLocalId name (toTcType ty)

nameType :: String -> Type -> DsM EvVar
nameType name ty = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit (name++"_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (newEvVar idname ty)

{-
%************************************************************************
%*                                                                      *
                              The type oracle
%*                                                                      *
%************************************************************************
-}

-- | Check whether a set of type constraints is satisfiable.
tyOracle :: Bag EvVar -> PmM Bool
tyOracle evs
  = liftD $
    do { ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability evs
       ; case res of
            Just sat -> return sat
            Nothing  -> pprPanic "tyOracle" (vcat $ pprErrMsgBagWithLoc errs) }

{-
%************************************************************************
%*                                                                      *
                             Sanity Checks
%*                                                                      *
%************************************************************************
-}

-- | The arity of a pattern/pattern vector is the
-- number of top-level patterns that are not guards
type PmArity = Int

-- | Compute the arity of a pattern vector
patVecArity :: PatVec -> PmArity
patVecArity = sum . map patternArity

-- | Compute the arity of a pattern
patternArity :: Pattern -> PmArity
patternArity (PmGrd {}) = 0
patternArity _other_pat = 1

{-
%************************************************************************
%*                                                                      *
            Heart of the algorithm: Function pmcheck
%*                                                                      *
%************************************************************************

Main functions are:

* mkInitialUncovered :: [Id] -> PmM Uncovered

  Generates the initial uncovered set. Term and type constraints in scope
  are checked, if they are inconsistent, the set is empty, otherwise, the
  set contains only a vector of variables with the constraints in scope.

* pmcheck :: PatVec -> [PatVec] -> ValVec -> PmM PartialResult

  Checks redundancy, coverage and inaccessibility, using auxilary functions
  `pmcheckGuards` and `pmcheckHd`. Mainly handles the guard case which is
  common in all three checks (see paper) and calls `pmcheckGuards` when the
  whole clause is checked, or `pmcheckHd` when the pattern vector does not
  start with a guard.

* pmcheckGuards :: [PatVec] -> ValVec -> PmM PartialResult

  Processes the guards.

* pmcheckHd :: Pattern -> PatVec -> [PatVec]
          -> ValAbs -> ValVec -> PmM PartialResult

  Worker: This function implements functions `covered`, `uncovered` and
  `divergent` from the paper at once. Slightly different from the paper because
  it does not even produce the covered and uncovered sets. Since we only care
  about whether a clause covers SOMETHING or if it may forces ANY argument, we
  only store a boolean in both cases, for efficiency.
-}

-- | Lift a pattern matching action from a single value vector abstration to a
-- value set abstraction, but calling it on every vector and the combining the
-- results.
runMany :: (ValVec -> PmM PartialResult) -> (Uncovered -> PmM PartialResult)
runMany _ [] = return mempty
runMany pm (m:ms) = mappend <$> pm m <*> runMany pm ms

-- | Generate the initial uncovered set. It initializes the
-- delta with all term and type constraints in scope.
mkInitialUncovered :: [Id] -> PmM Uncovered
mkInitialUncovered vars = do
  ty_cs  <- liftD getDictsDs
  tm_cs  <- map toComplex . bagToList <$> liftD getTmCsDs
  sat_ty <- tyOracle ty_cs
  let initTyCs = if sat_ty then ty_cs else emptyBag
      initTmState = fromMaybe initialTmState (tmOracle initialTmState tm_cs)
      patterns  = map PmVar vars
    -- If any of the term/type constraints are non
    -- satisfiable then return with the initialTmState. See #12957
  return [ValVec patterns (MkDelta initTyCs initTmState)]

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheck`
pmcheckI :: PatVec -> [PatVec] -> ValVec -> PmM PartialResult
pmcheckI ps guards vva = do
  n <- liftD incrCheckPmIterDs
  tracePm "pmCheck" (ppr n <> colon <+> pprPatVec ps
                        $$ hang (text "guards:") 2 (vcat (map pprPatVec guards))
                        $$ pprValVecDebug vva)
  res <- pmcheck ps guards vva
  tracePm "pmCheckResult:" (ppr res)
  return res
{-# INLINE pmcheckI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckGuards`
pmcheckGuardsI :: [PatVec] -> ValVec -> PmM PartialResult
pmcheckGuardsI gvs vva = liftD incrCheckPmIterDs >> pmcheckGuards gvs vva
{-# INLINE pmcheckGuardsI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckHd`
pmcheckHdI :: Pattern -> PatVec -> [PatVec] -> ValAbs -> ValVec
           -> PmM PartialResult
pmcheckHdI p ps guards va vva = do
  n <- liftD incrCheckPmIterDs
  tracePm "pmCheckHdI" (ppr n <> colon <+> pprPmPatDebug p
                        $$ pprPatVec ps
                        $$ hang (text "guards:") 2 (vcat (map pprPatVec guards))
                        $$ pprPmPatDebug va
                        $$ pprValVecDebug vva)

  res <- pmcheckHd p ps guards va vva
  tracePm "pmCheckHdI: res" (ppr res)
  return res
{-# INLINE pmcheckHdI #-}

-- | Matching function: Check simultaneously a clause (takes separately the
-- patterns and the list of guards) for exhaustiveness, redundancy and
-- inaccessibility.
pmcheck :: PatVec -> [PatVec] -> ValVec -> PmM PartialResult
pmcheck [] guards vva@(ValVec [] _)
  | null guards = return $ mempty { presultCovered = Covered }
  | otherwise   = pmcheckGuardsI guards vva

-- Guard
pmcheck (p@(PmGrd pv e) : ps) guards vva@(ValVec vas delta)
    -- short-circuit if the guard pattern is useless.
    -- we just have two possible outcomes: fail here or match and recurse
    -- none of the two contains any useful information about the failure
    -- though. So just have these two cases but do not do all the boilerplate
  | isFakeGuard pv e = forces . mkCons vva <$> pmcheckI ps guards vva
  | otherwise = do
      y <- liftD $ mkPmId (pmPatType p)
      let tm_state = extendSubst y e (delta_tm_cs delta)
          delta'   = delta { delta_tm_cs = tm_state }
      utail <$> pmcheckI (pv ++ ps) guards (ValVec (PmVar y : vas) delta')

pmcheck [] _ (ValVec (_:_) _) = panic "pmcheck: nil-cons"
pmcheck (_:_) _ (ValVec [] _) = panic "pmcheck: cons-nil"

pmcheck (p:ps) guards (ValVec (va:vva) delta)
  = pmcheckHdI p ps guards va (ValVec vva delta)

-- | Check the list of guards
pmcheckGuards :: [PatVec] -> ValVec -> PmM PartialResult
pmcheckGuards []       vva = return (usimple [vva])
pmcheckGuards (gv:gvs) vva = do
  (PartialResult prov1 cs vsa ds) <- pmcheckI gv [] vva
  (PartialResult prov2 css vsas dss) <- runMany (pmcheckGuardsI gvs) vsa
  return $ PartialResult (prov1 `mappend` prov2)
                         (cs `mappend` css)
                         vsas
                         (ds `mappend` dss)

-- | Worker function: Implements all cases described in the paper for all three
-- functions (`covered`, `uncovered` and `divergent`) apart from the `Guard`
-- cases which are handled by `pmcheck`
pmcheckHd :: Pattern -> PatVec -> [PatVec] -> ValAbs -> ValVec
          -> PmM PartialResult

-- Var
pmcheckHd (PmVar x) ps guards va (ValVec vva delta)
  | Just tm_state <- solveOneEq (delta_tm_cs delta)
                                (PmExprVar (idName x), vaToPmExpr va)
  = ucon va <$> pmcheckI ps guards (ValVec vva (delta {delta_tm_cs = tm_state}))
  | otherwise = return mempty

-- ConCon
pmcheckHd ( p@(PmCon {pm_con_con = c1, pm_con_args = args1})) ps guards
          (va@(PmCon {pm_con_con = c2, pm_con_args = args2})) (ValVec vva delta)
  | c1 /= c2  =
    return (usimple [ValVec (va:vva) delta])
  | otherwise = kcon c1 (pm_con_arg_tys p) (pm_con_tvs p) (pm_con_dicts p)
                <$> pmcheckI (args1 ++ ps) guards (ValVec (args2 ++ vva) delta)

-- LitLit
pmcheckHd (PmLit l1) ps guards (va@(PmLit l2)) vva =
  case eqPmLit l1 l2 of
    True  -> ucon va <$> pmcheckI ps guards vva
    False -> return $ ucon va (usimple [vva])

-- ConVar
pmcheckHd (p@(PmCon { pm_con_con = con, pm_con_arg_tys = tys }))
          ps guards
          (PmVar x) (ValVec vva delta) = do
  (prov, complete_match) <- select =<< liftD (allCompleteMatches con tys)

  cons_cs <- mapM (liftD . mkOneConFull x) complete_match

  inst_vsa <- flip concatMapM cons_cs $ \(va, tm_ct, ty_cs) -> do
    let ty_state = ty_cs `unionBags` delta_ty_cs delta -- not actually a state
    sat_ty <- if isEmptyBag ty_cs then return True
                                  else tyOracle ty_state
    return $ case (sat_ty, solveOneEq (delta_tm_cs delta) tm_ct) of
      (True, Just tm_state) -> [ValVec (va:vva) (MkDelta ty_state tm_state)]
      _ty_or_tm_failed      -> []

  set_provenance prov .
    force_if (canDiverge (idName x) (delta_tm_cs delta)) <$>
      runMany (pmcheckI (p:ps) guards) inst_vsa

-- LitVar
pmcheckHd (p@(PmLit l)) ps guards (PmVar x) (ValVec vva delta)
  = force_if (canDiverge (idName x) (delta_tm_cs delta)) <$>
      mkUnion non_matched <$>
        case solveOneEq (delta_tm_cs delta) (mkPosEq x l) of
          Just tm_state -> pmcheckHdI p ps guards (PmLit l) $
                             ValVec vva (delta {delta_tm_cs = tm_state})
          Nothing       -> return mempty
  where
    us | Just tm_state <- solveOneEq (delta_tm_cs delta) (mkNegEq x l)
       = [ValVec (PmNLit x [l] : vva) (delta { delta_tm_cs = tm_state })]
       | otherwise = []

    non_matched = usimple us

-- LitNLit
pmcheckHd (p@(PmLit l)) ps guards
          (PmNLit { pm_lit_id = x, pm_lit_not = lits }) (ValVec vva delta)
  | all (not . eqPmLit l) lits
  , Just tm_state <- solveOneEq (delta_tm_cs delta) (mkPosEq x l)
    -- Both guards check the same so it would be sufficient to have only
    -- the second one. Nevertheless, it is much cheaper to check whether
    -- the literal is in the list so we check it first, to avoid calling
    -- the term oracle (`solveOneEq`) if possible
  = mkUnion non_matched <$>
      pmcheckHdI p ps guards (PmLit l)
                (ValVec vva (delta { delta_tm_cs = tm_state }))
  | otherwise = return non_matched
  where
    us | Just tm_state <- solveOneEq (delta_tm_cs delta) (mkNegEq x l)
       = [ValVec (PmNLit x (l:lits) : vva) (delta { delta_tm_cs = tm_state })]
       | otherwise = []

    non_matched = usimple us

-- ----------------------------------------------------------------------------
-- The following three can happen only in cases like #322 where constructors
-- and overloaded literals appear in the same match. The general strategy is
-- to replace the literal (positive/negative) by a variable and recurse. The
-- fact that the variable is equal to the literal is recorded in `delta` so
-- no information is lost

-- LitCon
pmcheckHd (PmLit l) ps guards (va@(PmCon {})) (ValVec vva delta)
  = do y <- liftD $ mkPmId (pmPatType va)
       let tm_state = extendSubst y (PmExprLit l) (delta_tm_cs delta)
           delta'   = delta { delta_tm_cs = tm_state }
       pmcheckHdI (PmVar y) ps guards va (ValVec vva delta')

-- ConLit
pmcheckHd (p@(PmCon {})) ps guards (PmLit l) (ValVec vva delta)
  = do y <- liftD $ mkPmId (pmPatType p)
       let tm_state = extendSubst y (PmExprLit l) (delta_tm_cs delta)
           delta'   = delta { delta_tm_cs = tm_state }
       pmcheckHdI p ps guards (PmVar y) (ValVec vva delta')

-- ConNLit
pmcheckHd (p@(PmCon {})) ps guards (PmNLit { pm_lit_id = x }) vva
  = pmcheckHdI p ps guards (PmVar x) vva

-- Impossible: handled by pmcheck
pmcheckHd (PmGrd {}) _ _ _ _ = panic "pmcheckHd: Guard"

-- ----------------------------------------------------------------------------
-- * Utilities for main checking

updateVsa :: (ValSetAbs -> ValSetAbs) -> (PartialResult -> PartialResult)
updateVsa f p@(PartialResult { presultUncovered = old })
  = p { presultUncovered = f old }


-- | Initialise with default values for covering and divergent information.
usimple :: ValSetAbs -> PartialResult
usimple vsa = mempty { presultUncovered = vsa }

-- | Take the tail of all value vector abstractions in the uncovered set
utail :: PartialResult -> PartialResult
utail = updateVsa upd
  where upd vsa = [ ValVec vva delta | ValVec (_:vva) delta <- vsa ]

-- | Prepend a value abstraction to all value vector abstractions in the
-- uncovered set
ucon :: ValAbs -> PartialResult -> PartialResult
ucon va = updateVsa upd
  where
    upd vsa = [ ValVec (va:vva) delta | ValVec vva delta <- vsa ]

-- | Given a data constructor of arity `a` and an uncovered set containing
-- value vector abstractions of length `(a+n)`, pass the first `n` value
-- abstractions to the constructor (Hence, the resulting value vector
-- abstractions will have length `n+1`)
kcon :: ConLike -> [Type] -> [TyVar] -> [EvVar]
     -> PartialResult -> PartialResult
kcon con arg_tys ex_tvs dicts
  = let n = conLikeArity con
        upd vsa =
          [ ValVec (va:vva) delta
          | ValVec vva' delta <- vsa
          , let (args, vva) = splitAt n vva'
          , let va = PmCon { pm_con_con     = con
                            , pm_con_arg_tys = arg_tys
                            , pm_con_tvs     = ex_tvs
                            , pm_con_dicts   = dicts
                            , pm_con_args    = args } ]
    in updateVsa upd

-- | Get the union of two covered, uncovered and divergent value set
-- abstractions. Since the covered and divergent sets are represented by a
-- boolean, union means computing the logical or (at least one of the two is
-- non-empty).

mkUnion :: PartialResult -> PartialResult -> PartialResult
mkUnion = mappend

-- | Add a value vector abstraction to a value set abstraction (uncovered).
mkCons :: ValVec -> PartialResult -> PartialResult
mkCons vva = updateVsa (vva:)

-- | Set the divergent set to not empty
forces :: PartialResult -> PartialResult
forces pres = pres { presultDivergent = Diverged }

-- | Set the divergent set to non-empty if the flag is `True`
force_if :: Bool -> PartialResult -> PartialResult
force_if True  pres = forces pres
force_if False pres = pres

set_provenance :: Provenance -> PartialResult -> PartialResult
set_provenance prov pr = pr { presultProvenance = prov }

-- ----------------------------------------------------------------------------
-- * Propagation of term constraints inwards when checking nested matches

{- Note [Type and Term Equality Propagation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a match it would be great to have all type and term information
available so we can get more precise results. For this reason we have functions
`addDictsDs' and `addTmCsDs' in PmMonad that store in the environment type and
term constraints (respectively) as we go deeper.

The type constraints we propagate inwards are collected by `collectEvVarsPats'
in HsPat.hs. This handles bug #4139 ( see example
  https://ghc.haskell.org/trac/ghc/attachment/ticket/4139/GADTbug.hs )
where this is needed.

For term equalities we do less, we just generate equalities for HsCase. For
example we accurately give 2 redundancy warnings for the marked cases:

f :: [a] -> Bool
f x = case x of

  []    -> case x of        -- brings (x ~ []) in scope
             []    -> True
             (_:_) -> False -- can't happen

  (_:_) -> case x of        -- brings (x ~ (_:_)) in scope
             (_:_) -> True
             []    -> False -- can't happen

Functions `genCaseTmCs1' and `genCaseTmCs2' are responsible for generating
these constraints.
-}

-- | Generate equalities when checking a case expression:
--     case x of { p1 -> e1; ... pn -> en }
-- When we go deeper to check e.g. e1 we record two equalities:
-- (x ~ y), where y is the initial uncovered when checking (p1; .. ; pn)
-- and (x ~ p1).
genCaseTmCs2 :: Maybe (LHsExpr GhcTc) -- Scrutinee
             -> [Pat GhcTc]           -- LHS       (should have length 1)
             -> [Id]                  -- MatchVars (should have length 1)
             -> DsM (Bag SimpleEq)
genCaseTmCs2 Nothing _ _ = return emptyBag
genCaseTmCs2 (Just scr) [p] [var] = do
  fam_insts <- dsGetFamInstEnvs
  [e] <- map vaToPmExpr . coercePatVec <$> translatePat fam_insts p
  let scr_e = lhsExprToPmExpr scr
  return $ listToBag [(var, e), (var, scr_e)]
genCaseTmCs2 _ _ _ = panic "genCaseTmCs2: HsCase"

-- | Generate a simple equality when checking a case expression:
--     case x of { matches }
-- When checking matches we record that (x ~ y) where y is the initial
-- uncovered. All matches will have to satisfy this equality.
genCaseTmCs1 :: Maybe (LHsExpr GhcTc) -> [Id] -> Bag SimpleEq
genCaseTmCs1 Nothing     _    = emptyBag
genCaseTmCs1 (Just scr) [var] = unitBag (var, lhsExprToPmExpr scr)
genCaseTmCs1 _ _              = panic "genCaseTmCs1: HsCase"

{- Note [Literals in PmPat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating a literal to a variable accompanied with a guard, we
treat them like constructor patterns. The following example from
"./libraries/base/GHC/IO/Encoding.hs" shows why:

mkTextEncoding' :: CodingFailureMode -> String -> IO TextEncoding
mkTextEncoding' cfm enc = case [toUpper c | c <- enc, c /= '-'] of
    "UTF8"    -> return $ UTF8.mkUTF8 cfm
    "UTF16"   -> return $ UTF16.mkUTF16 cfm
    "UTF16LE" -> return $ UTF16.mkUTF16le cfm
    ...

Each clause gets translated to a list of variables with an equal number of
guards. For every guard we generate two cases (equals True/equals False) which
means that we generate 2^n cases to feed the oracle with, where n is the sum of
the length of all strings that appear in the patterns. For this particular
example this means over 2^40 cases. Instead, by representing them like with
constructor we get the following:
  1. We exploit the common prefix with our representation of VSAs
  2. We prune immediately non-reachable cases
     (e.g. False == (x == "U"), True == (x == "U"))

Note [Translating As Patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating x@p as:  x (p <- x)
we instead translate it as:     p (x <- coercePattern p)
for performance reasons. For example:

  f x@True  = 1
  f y@False = 2

Gives the following with the first translation:

  x |> {x == False, x == y, y == True}

If we use the second translation we get an empty set, independently of the
oracle. Since the pattern `p' may contain guard patterns though, it cannot be
used as an expression. That's why we call `coercePatVec' to drop the guard and
`vaToPmExpr' to transform the value abstraction to an expression in the
guard pattern (value abstractions are a subset of expressions). We keep the
guards in the first pattern `p' though.


%************************************************************************
%*                                                                      *
      Pretty printing of exhaustiveness/redundancy check warnings
%*                                                                      *
%************************************************************************
-}

-- | Check whether any part of pattern match checking is enabled (does not
-- matter whether it is the redundancy check or the exhaustiveness check).
isAnyPmCheckEnabled :: DynFlags -> DsMatchContext -> Bool
isAnyPmCheckEnabled dflags (DsMatchContext kind _loc)
  = wopt Opt_WarnOverlappingPatterns dflags || exhaustive dflags kind

instance Outputable ValVec where
  ppr (ValVec vva delta)
    = let (residual_eqs, subst) = wrapUpTmState (delta_tm_cs delta)
          vector                = substInValAbs subst vva
      in  ppr_uncovered (vector, residual_eqs)

-- | Apply a term substitution to a value vector abstraction. All VAs are
-- transformed to PmExpr (used only before pretty printing).
substInValAbs :: PmVarEnv -> [ValAbs] -> [PmExpr]
substInValAbs subst = map (exprDeepLookup subst . vaToPmExpr)

-- | Wrap up the term oracle's state once solving is complete. Drop any
-- information about unhandled constraints (involving HsExprs) and flatten
-- (height 1) the substitution.
wrapUpTmState :: TmState -> ([ComplexEq], PmVarEnv)
wrapUpTmState (residual, (_, subst)) = (residual, flattenPmVarEnv subst)

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant && onlyBuiltin
          exists_i = flag_i && notNull inaccessible && onlyBuiltin && not is_rec_upd
          exists_u = flag_u && (case uncovered of
                                  TypeOfUncovered   _ -> True
                                  UncoveredPatterns u -> notNull u)

      when exists_r $ forM_ redundant $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))
      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        case uncovered of
          TypeOfUncovered ty           -> warnEmptyCase ty
          UncoveredPatterns candidates -> pprEqns candidates
  where
    PmResult
      { pmresultProvenance = prov
      , pmresultRedundant = redundant
      , pmresultUncovered = uncovered
      , pmresultInaccessible = inaccessible } = pm_result

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind
    flag_u_reason = maybe NoReason Reason (exhaustiveWarningFlag kind)

    is_rec_upd = case kind of { RecUpd -> True; _ -> False }
       -- See Note [Inaccessible warnings for record updates]

    onlyBuiltin = prov == FromBuiltin

    maxPatterns = maxUncoveredPatterns dflags

    -- Print a single clause (for redundant/with-inaccessible-rhs)
    pprEqn q txt = pp_context True ctx (text txt) $ \f -> ppr_eqn f kind q

    -- Print several clauses (for uncovered clauses)
    pprEqns qs = pp_context False ctx (text "are non-exhaustive") $ \_ ->
      case qs of -- See #11245
           [ValVec [] _]
                    -> text "Guards do not cover entire pattern space"
           _missing -> let us = map ppr qs
                       in  hang (text "Patterns not matched:") 4
                                (vcat (take maxPatterns us)
                                 $$ dots maxPatterns us)

    -- Print a type-annotated wildcard (for non-exhaustive `EmptyCase`s for
    -- which we only know the type and have no inhabitants at hand)
    warnEmptyCase ty = pp_context False ctx (text "are non-exhaustive") $ \_ ->
      hang (text "Patterns not matched:") 4 (underscore <+> dcolon <+> ppr ty)

{- Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #12957)
  data T a where
    T1 :: { x :: Int } -> T Bool
    T2 :: { x :: Int } -> T a
    T3 :: T a

  f :: T Char -> T a
  f r = r { x = 3 }

The desugarer will (conservatively generate a case for T1 even though
it's impossible:
  f r = case r of
          T1 x -> T1 3   -- Inaccessible branch
          T2 x -> T2 3
          _    -> error "Missing"

We don't want to warn about the inaccessible branch because the programmer
didn't put it there!  So we filter out the warning here.
-}

-- | Issue a warning when the predefined number of iterations is exceeded
-- for the pattern match checker
warnPmIters :: DynFlags -> DsMatchContext -> DsM ()
warnPmIters dflags (DsMatchContext kind loc)
  = when (flag_i || flag_u) $ do
      iters <- maxPmCheckIterations <$> getDynFlags
      putSrcSpanDs loc (warnDs NoReason (msg iters))
  where
    ctxt   = pprMatchContext kind
    msg is = fsep [ text "Pattern match checker exceeded"
                  , parens (ppr is), text "iterations in", ctxt <> dot
                  , text "(Use -fmax-pmcheck-iterations=n"
                  , text "to set the maximun number of iterations to n)" ]

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind

dots :: Int -> [a] -> SDoc
dots maxPatterns qs
    | qs `lengthExceeds` maxPatterns = text "..."
    | otherwise                      = empty

-- | Check whether the exhaustiveness checker should run (exhaustiveness only)
exhaustive :: DynFlags -> HsMatchContext id -> Bool
exhaustive  dflags = maybe False (`wopt` dflags) . exhaustiveWarningFlag

-- | Denotes whether an exhaustiveness check is supported, and if so,
-- via which 'WarningFlag' it's controlled.
-- Returns 'Nothing' if check is not supported.
exhaustiveWarningFlag :: HsMatchContext id -> Maybe WarningFlag
exhaustiveWarningFlag (FunRhs {})   = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag CaseAlt       = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag IfAlt         = Nothing
exhaustiveWarningFlag LambdaExpr    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag PatBindRhs    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag ProcExpr      = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag RecUpd        = Just Opt_WarnIncompletePatternsRecUpd
exhaustiveWarningFlag ThPatSplice   = Nothing
exhaustiveWarningFlag PatSyn        = Nothing
exhaustiveWarningFlag ThPatQuote    = Nothing
exhaustiveWarningFlag (StmtCtxt {}) = Nothing -- Don't warn about incomplete patterns
                                       -- in list comprehensions, pattern guards
                                       -- etc. They are often *supposed* to be
                                       -- incomplete

-- True <==> singular
pp_context :: Bool -> DsMatchContext -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pp_context singular (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [text txt <+> msg,
          sep [ text "In" <+> ppr_match <> char ':'
              , nest 4 (rest_of_msg_fun pref)]]
  where
    txt | singular  = "Pattern match"
        | otherwise = "Pattern match(es)"

    (ppr_match, pref)
        = case kind of
             FunRhs { mc_fun = L _ fun }
                  -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _    -> (pprMatchContext kind, \ pp -> pp)

ppr_pats :: HsMatchContext Name -> [Pat GhcTc] -> SDoc
ppr_pats kind pats
  = sep [sep (map ppr pats), matchSeparator kind, text "..."]

ppr_eqn :: (SDoc -> SDoc) -> HsMatchContext Name -> [LPat GhcTc] -> SDoc
ppr_eqn prefixF kind eqn = prefixF (ppr_pats kind (map unLoc eqn))

ppr_constraint :: (SDoc,[PmLit]) -> SDoc
ppr_constraint (var, lits) = var <+> text "is not one of"
                                 <+> braces (pprWithCommas ppr lits)

ppr_uncovered :: ([PmExpr], [ComplexEq]) -> SDoc
ppr_uncovered (expr_vec, complex)
  | null cs   = fsep vec -- there are no literal constraints
  | otherwise = hang (fsep vec) 4 $
                  text "where" <+> vcat (map ppr_constraint cs)
  where
    sdoc_vec = mapM pprPmExprWithParens expr_vec
    (vec,cs) = runPmPprM sdoc_vec (filterComplex complex)

{- Note [Representation of Term Equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the paper, term constraints always take the form (x ~ e). Of course, a more
general constraint of the form (e1 ~ e1) can always be transformed to an
equivalent set of the former constraints, by introducing a fresh, intermediate
variable: { y ~ e1, y ~ e1 }. Yet, implementing this representation gave rise
to #11160 (incredibly bad performance for literal pattern matching). Two are
the main sources of this problem (the actual problem is how these two interact
with each other):

1. Pattern matching on literals generates twice as many constraints as needed.
   Consider the following (tests/ghci/should_run/ghcirun004):

    foo :: Int -> Int
    foo 1    = 0
    ...
    foo 5000 = 4999

   The covered and uncovered set *should* look like:
     U0 = { x |> {} }

     C1  = { 1  |> { x ~ 1 } }
     U1  = { x  |> { False ~ (x ~ 1) } }
     ...
     C10 = { 10 |> { False ~ (x ~ 1), .., False ~ (x ~ 9), x ~ 10 } }
     U10 = { x  |> { False ~ (x ~ 1), .., False ~ (x ~ 9), False ~ (x ~ 10) } }
     ...

     If we replace { False ~ (x ~ 1) } with { y ~ False, y ~ (x ~ 1) }
     we get twice as many constraints. Also note that half of them are just the
     substitution [x |-> False].

2. The term oracle (`tmOracle` in deSugar/TmOracle) uses equalities of the form
   (x ~ e) as substitutions [x |-> e]. More specifically, function
   `extendSubstAndSolve` applies such substitutions in the residual constraints
   and partitions them in the affected and non-affected ones, which are the new
   worklist. Essentially, this gives quadradic behaviour on the number of the
   residual constraints. (This would not be the case if the term oracle used
   mutable variables but, since we use it to handle disjunctions on value set
   abstractions (`Union` case), we chose a pure, incremental interface).

Now the problem becomes apparent (e.g. for clause 300):
  * Set U300 contains 300 substituting constraints [y_i |-> False] and 300
    constraints that we know that will not reduce (stay in the worklist).
  * To check for consistency, we apply the substituting constraints ONE BY ONE
    (since `tmOracle` is called incrementally, it does not have all of them
    available at once). Hence, we go through the (non-progressing) constraints
    over and over, achieving over-quadradic behaviour.

If instead we allow constraints of the form (e ~ e),
  * All uncovered sets Ui contain no substituting constraints and i
    non-progressing constraints of the form (False ~ (x ~ lit)) so the oracle
    behaves linearly.
  * All covered sets Ci contain exactly (i-1) non-progressing constraints and
    a single substituting constraint. So the term oracle goes through the
    constraints only once.

The performance improvement becomes even more important when more arguments are
involved.
-}

-- Debugging Infrastructre

tracePm :: String -> SDoc -> PmM ()
tracePm herald doc = liftD $ tracePmD herald doc


tracePmD :: String -> SDoc -> DsM ()
tracePmD herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace (text herald $$ (nest 2 doc))


pprPmPatDebug :: PmPat a -> SDoc
pprPmPatDebug (PmCon cc _arg_tys _con_tvs _con_dicts con_args)
  = hsep [text "PmCon", ppr cc, hsep (map pprPmPatDebug con_args)]
pprPmPatDebug (PmVar vid) = text "PmVar" <+> ppr vid
pprPmPatDebug (PmLit li)  = text "PmLit" <+> ppr li
pprPmPatDebug (PmNLit i nl) = text "PmNLit" <+> ppr i <+> ppr nl
pprPmPatDebug (PmGrd pv ge) = text "PmGrd" <+> hsep (map pprPmPatDebug pv)
                                           <+> ppr ge

pprPatVec :: PatVec -> SDoc
pprPatVec ps = hang (text "Pattern:") 2
                (brackets $ sep
                  $ punctuate (comma <> char '\n') (map pprPmPatDebug ps))

pprValAbs :: [ValAbs] -> SDoc
pprValAbs ps = hang (text "ValAbs:") 2
                (brackets $ sep
                  $ punctuate (comma) (map pprPmPatDebug ps))

pprValVecDebug :: ValVec -> SDoc
pprValVecDebug (ValVec vas _d) = text "ValVec" <+>
                                  parens (pprValAbs vas)
