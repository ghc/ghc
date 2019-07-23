{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Pattern Matching Coverage Checking.
-}

{-# LANGUAGE CPP            #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE LambdaCase     #-}

module Check (
        -- Checking and printing
        checkSingle, checkMatches, checkGuardMatches, isAnyPmCheckEnabled,

        -- See Note [Type and Term Equality Propagation]
        genCaseTmCs1, genCaseTmCs2
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr
import TmOracle
import PmPpr
import Unify( tcMatchTy )
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
import PatSyn
import HscTypes (CompleteMatch(..))
import BasicTypes (Boxity(..), Satisfiability(..))

import DsMonad
import TcSimplify    (tcCheckSatisfiability)
import Bag
import ErrUtils
import Var           (EvVar)
import TyCoRep
import Type
import UniqSupply
import DsUtils       (isTrueLHsExpr)
import Maybes        (expectJust, mapMaybe, isJust, fromMaybe)
import qualified GHC.LanguageExtensions as LangExt

import Data.List     (find)
import Data.List.NonEmpty (toList)
import Data.Void     (Void)
import Control.Monad (forM, when, forM_, filterM)
import Coercion
import TcEvidence
import TcSimplify    (tcNormalise)
import IOEnv
import qualified Data.Semigroup as Semi

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

type PmM = DsM

data PmPat where
  -- | For the arguments' meaning see 'HsPat.ConPatOut'.
  PmCon  :: { pm_con_con     :: ConLike
            , pm_con_arg_tys :: [Type]
            , pm_con_tvs     :: [TyVar]
            , pm_con_args    :: [PmPat] } -> PmPat
  PmVar  :: { pm_var_id   :: Id } -> PmPat
  -- | See Note [Literals in PmPat]
  PmLit  :: { pm_lit_lit  :: PmLit } -> PmPat
  PmGrd  :: { pm_grd_pv   :: PatVec -- ^ Always has 'patVecArity' 1.
            , pm_grd_expr :: PmExpr } -> PmPat
  -- | A fake guard pattern (True <- _) used to represent cases we cannot handle.
  PmFake :: PmPat

-- | Should not be user-facing.
instance Outputable PmPat where
  ppr (PmCon cc _arg_tys _con_tvs con_args)
    = cparen (notNull con_args) (hsep [ppr cc, hsep (map ppr con_args)])
  ppr (PmVar vid) = ppr vid
  ppr (PmLit li) = ppr li
  ppr (PmGrd pv ge) = hsep (map ppr pv) <+> text "<-" <+> ppr ge
  ppr PmFake = text "<PmFake>"

-- data T a where
--     MkT :: forall p q. (Eq p, Ord q) => p -> q -> T [p]
-- or  MkT :: forall p q r. (Eq p, Ord q, [p] ~ r) => p -> q -> T r

type ValAbs  = Id -- ^ Value Abstractions

-- | Pattern Vectors. The *arity* of a PatVec [p1,..,pn] is
-- the number of p1..pn that are not Guards. See 'patternArity'.
type PatVec = [PmPat]
data ValVec = ValVec [ValAbs] Delta -- ^ Value Vector Abstractions

-- | Term and type constraints to accompany each value vector abstraction.
-- For efficiency, we store the term oracle state instead of the term
-- constraints. TODO: Do the same for the type constraints?
data Delta = MkDelta { delta_ty_cs :: Bag EvVar  -- Type oracle; things like a~Int
                     , delta_tm_cs :: TmState }  -- Term oracle; things like x~Nothing

type ValSetAbs = [ValVec]  -- ^ Value Set Abstractions
type Uncovered = ValSetAbs

-- | Should be user-facing. See 'pprValVecSubstituted' for a function intended
-- to produce user-facing output.
instance Outputable ValVec where
  ppr (ValVec vva delta) = ppr_vec vva <+> text "|>" <+> ppr_delta delta
    where
      ppr_vec = brackets . fsep . punctuate comma . map ppr
      ppr_delta _d = hcat [
          -- intentionally formatted this way enable the dev to comment in only
          -- the info she needs
          ppr (delta_tm_cs delta),
          ppr (delta_ty_cs delta)
        ]

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
  ppr = text . show

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
  ppr = text . show

instance Semi.Semigroup Diverged where
  Diverged <> _ = Diverged
  _ <> Diverged = Diverged
  NotDiverged <> NotDiverged = NotDiverged

instance Monoid Diverged where
  mempty = NotDiverged
  mappend = (Semi.<>)

data PartialResult = PartialResult {
                        presultCovered :: Covered
                      , presultUncovered :: Uncovered
                      , presultDivergent :: Diverged }

instance Outputable PartialResult where
  ppr (PartialResult c vsa d)
    = hang (text "PartialResult" <+> ppr c <+> ppr d) 2  (ppr_vsa vsa)
    where
      ppr_vsa = braces . fsep . punctuate comma . map ppr

instance Semi.Semigroup PartialResult where
  (PartialResult cs1 vsa1 ds1)
    <> (PartialResult cs2 vsa2 ds2)
      = PartialResult (cs1 Semi.<> cs2)
                      (vsa1 Semi.<> vsa2)
                      (ds1 Semi.<> ds2)


instance Monoid PartialResult where
  mempty = PartialResult mempty [] mempty
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
--     https://gitlab.haskell.org/ghc/ghc/wikis/pattern-match-check
--
data PmResult =
  PmResult {
      pmresultRedundant    :: [Located [LPat GhcTc]]
    , pmresultUncovered    :: UncoveredCandidates
    , pmresultInaccessible :: [Located [LPat GhcTc]] }

instance Outputable PmResult where
  ppr pmr = hang (text "PmResult") 2 $ vcat
    [ text "pmresultRedundant" <+> ppr (pmresultRedundant pmr)
    , text "pmresultUncovered" <+> ppr (pmresultUncovered pmr)
    , text "pmresultInaccessible" <+> ppr (pmresultInaccessible pmr)
    ]

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

instance Outputable UncoveredCandidates where
  ppr (UncoveredPatterns uc) = text "UnPat" <+> ppr uc
  ppr (TypeOfUncovered ty)   = text "UnTy" <+> ppr ty

-- | The empty pattern check result
emptyPmResult :: PmResult
emptyPmResult = PmResult [] (UncoveredPatterns []) []

-- | Non-exhaustive empty case with unknown/trivial inhabitants
uncoveredWithTy :: Type -> PmResult
uncoveredWithTy ty = PmResult [] (TypeOfUncovered ty) []

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
  tracePm "checkSingle" (vcat [ppr ctxt, ppr var, ppr p])
  mb_pm_res <- tryM (checkSingle' locn var p)
  case mb_pm_res of
    Left  _   -> warnPmIters dflags ctxt
    Right res -> dsPmWarn dflags ctxt res

-- | Check a single pattern binding (let)
checkSingle' :: SrcSpan -> Id -> Pat GhcTc -> PmM PmResult
checkSingle' locn var p = do
  resetPmIterDs -- set the iter-no to zero
  fam_insts <- dsGetFamInstEnvs
  clause    <- translatePat fam_insts p
  missing   <- mkInitialUncovered [var]
  tracePm "checkSingle': missing" (vcat (map ppr missing))
                                  -- no guards
  PartialResult cs us ds <- runMany (pmcheckI clause []) missing
  let us' = UncoveredPatterns us
  return $ case (cs,ds) of
    (Covered,  _    )         -> PmResult [] us' [] -- useful
    (NotCovered, NotDiverged) -> PmResult m  us' [] -- redundant
    (NotCovered, Diverged )   -> PmResult [] us' m  -- inaccessible rhs
  where m = [cL locn [cL locn p]]

-- | Exhaustive for guard matches, is used for guards in pattern bindings and
-- in @MultiIf@ expressions.
checkGuardMatches :: HsMatchContext Name          -- Match context
                  -> GRHSs GhcTc (LHsExpr GhcTc)  -- Guarded RHSs
                  -> PmM ()
checkGuardMatches hs_ctx guards@(GRHSs _ grhss _) = do
    dflags <- getDynFlags
    let combinedLoc = foldl1 combineSrcSpans (map getLoc grhss)
        dsMatchContext = DsMatchContext hs_ctx combinedLoc
        match = cL combinedLoc $
                  Match { m_ext = noExtField
                        , m_ctxt = hs_ctx
                        , m_pats = []
                        , m_grhss = guards }
    checkMatches dflags dsMatchContext [] [match]
checkGuardMatches _ (XGRHSs nec) = noExtCon nec

-- | Check a matchgroup (case, functions, etc.)
checkMatches :: DynFlags -> DsMatchContext
             -> [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> PmM ()
checkMatches dflags ctxt vars matches = do
  tracePm "checkMatches" (hang (vcat [ppr ctxt
                               , ppr vars
                               , text "Matches:"])
                               2
                               (vcat (map ppr matches)))
  mb_pm_res <- tryM $ case matches of
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
      resetPmIterDs -- set the iter-no to zero
      missing    <- mkInitialUncovered vars
      tracePm "checkMatches': missing" (vcat (map ppr missing))
      (rs,us,ds) <- go matches missing
      return $ PmResult {
                   pmresultRedundant    = map hsLMatchToLPats rs
                 , pmresultUncovered    = UncoveredPatterns us
                 , pmresultInaccessible = map hsLMatchToLPats ds }
  where
    go :: [LMatch GhcTc (LHsExpr GhcTc)] -> Uncovered
       -> PmM ( [LMatch GhcTc (LHsExpr GhcTc)]
              , Uncovered
              , [LMatch GhcTc (LHsExpr GhcTc)])
    go []     missing = return ([], missing, [])
    go (m:ms) missing = do
      tracePm "checkMatches': go" (ppr m $$ ppr missing)
      fam_insts          <- dsGetFamInstEnvs
      (clause, guards)   <- translateMatch fam_insts m
      r@(PartialResult cs missing' ds)
        <- runMany (pmcheckI clause guards) missing
      tracePm "checkMatches': go: res" (ppr r)
      (rs, final_u, is)  <- go ms missing'
      return $ case (cs, ds) of
        -- useful
        (Covered,  _    )        -> (rs, final_u,   is)
        -- redundant
        (NotCovered, NotDiverged) -> (m:rs, final_u,is)
        -- inaccessible
        (NotCovered, Diverged )   -> (rs, final_u, m:is)

    hsLMatchToLPats :: LMatch id body -> Located [LPat id]
    hsLMatchToLPats (dL->L l (Match { m_pats = pats })) = cL l pats
    hsLMatchToLPats _                                   = panic "checkMatches'"

-- | Check an empty case expression. Since there are no clauses to process, we
--   only compute the uncovered set. See Note [Checking EmptyCase Expressions]
--   for details.
checkEmptyCase' :: Id -> PmM PmResult
checkEmptyCase' var = do
  tm_ty_css     <- pmInitialTmTyCs
  mb_candidates <- inhabitationCandidates (delta_ty_cs tm_ty_css) (idType var)
  case mb_candidates of
    -- Inhabitation checking failed / the type is trivially inhabited
    Left ty -> return (uncoveredWithTy ty)

    -- A list of inhabitant candidates is available: Check for each
    -- one for the satisfiability of the constraints it gives rise to.
    Right (_, va, candidates) -> do
      missing_m <- flip mapMaybeM candidates $
          \InhabitationCandidate{ ic_tm_cs = tm_cs
                                , ic_ty_cs = ty_cs
                                , ic_strict_arg_tys = strict_arg_tys } -> do
        mb_sat <- pmIsSatisfiable tm_ty_css tm_cs ty_cs strict_arg_tys
        pure $ fmap (ValVec [va]) mb_sat
      return $ if null missing_m
        then emptyPmResult
        else PmResult [] (UncoveredPatterns missing_m) []

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

pmTopNormaliseType_maybe :: FamInstEnvs -> Bag EvVar -> Type
                         -> PmM (Maybe (Type, [DataCon], Type))
-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
--
-- Behaves exactly like `topNormaliseType_maybe`, but instead of returning a
-- coercion, it returns useful information for issuing pattern matching
-- warnings. See Note [Type normalisation for EmptyCase] for details.
--
-- NB: Normalisation can potentially change kinds, if the head of the type
-- is a type family with a variable result kind. I (Richard E) can't think
-- of a way to cause trouble here, though.
pmTopNormaliseType_maybe env ty_cs typ
  = do (_, mb_typ') <- initTcDsForSolver $ tcNormalise ty_cs typ
         -- Before proceeding, we chuck typ into the constraint solver, in case
         -- solving for given equalities may reduce typ some. See
         -- "Wrinkle: local equalities" in
         -- Note [Type normalisation for EmptyCase].
       pure $ do typ' <- mb_typ'
                 ((ty_f,tm_f), ty) <- topNormaliseTypeX stepper comb typ'
                 -- We need to do topNormaliseTypeX in addition to tcNormalise,
                 -- since topNormaliseX looks through newtypes, which
                 -- tcNormalise does not do.
                 Just (eq_src_ty ty (typ' : ty_f [ty]), tm_f [], ty)
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
      = let (_args_co, ntys, _res_co) = normaliseTcArgs env Representational tc tys in
          -- NB: It's OK to use normaliseTcArgs here instead of
          -- normalise_tc_args (which takes the LiftingContext described
          -- in Note [Normalising types]) because the reduceTyFamApp below
          -- works only at top level. We'll never recur in this function
          -- after reducing the kind of a bound tyvar.

        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (_co, rhs) -> NS_Step rec_nts rhs ((rhs:), id)
          _               -> NS_Done

-- | Determine suitable constraints to use at the beginning of pattern-match
-- coverage checking by consulting the sets of term and type constraints
-- currently in scope. If one of these sets of constraints is unsatisfiable,
-- use an empty set in its place. (See
-- @Note [Recovering from unsatisfiable pattern-matching constraints]@
-- for why this is done.)
pmInitialTmTyCs :: PmM Delta
pmInitialTmTyCs = do
  ty_cs  <- getDictsDs
  tm_cs  <- bagToList <$> getTmCsDs
  sat_ty <- tyOracle ty_cs
  let initTyCs = if sat_ty then ty_cs else emptyBag
      initTmState = fromMaybe initialTmState (tmOracle initialTmState tm_cs)
  pure $ MkDelta initTyCs initTmState

{-
Note [Recovering from unsatisfiable pattern-matching constraints]
~~~~~~~~~~~~~~~~
Consider the following code (see #12957 and #15450):

  f :: Int ~ Bool => ()
  f = case True of { False -> () }

We want to warn that the pattern-matching in `f` is non-exhaustive. But GHC
used not to do this; in fact, it would warn that the match was /redundant/!
This is because the constraint (Int ~ Bool) in `f` is unsatisfiable, and the
coverage checker deems any matches with unsatifiable constraint sets to be
unreachable.

We decide to better than this. When beginning coverage checking, we first
check if the constraints in scope are unsatisfiable, and if so, we start
afresh with an empty set of constraints. This way, we'll get the warnings
that we expect.
-}

-- | Given a conlike's term constraints, type constraints, and strict argument
-- types, check if they are satisfiable.
-- (In other words, this is the ⊢_Sat oracle judgment from the GADTs Meet
-- Their Match paper.)
--
-- For the purposes of efficiency, this takes as separate arguments the
-- ambient term and type constraints (which are known beforehand to be
-- satisfiable), as well as the new term and type constraints (which may not
-- be satisfiable). This lets us implement two mini-optimizations:
--
-- * If there are no new type constraints, then don't bother initializing
--   the type oracle, since it's redundant to do so.
-- * Since the new term constraint is a separate argument, we only need to
--   execute one iteration of the term oracle (instead of traversing the
--   entire set of term constraints).
--
-- Taking strict argument types into account is something which was not
-- discussed in GADTs Meet Their Match. For an explanation of what role they
-- serve, see @Note [Extensions to GADTs Meet Their Match]@.
pmIsSatisfiable
  :: Delta       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TmVarCt -- ^ The new term constraints.
  -> Bag EvVar   -- ^ The new type constraints.
  -> [Type]      -- ^ The strict argument types.
  -> PmM (Maybe Delta)
                 -- ^ @'Just' delta@ if the constraints (@delta@) are
                 -- satisfiable, and each strict argument type is inhabitable.
                 -- 'Nothing' otherwise.
pmIsSatisfiable amb_cs new_tm_c new_ty_cs strict_arg_tys = do
  mb_sat <- tmTyCsAreSatisfiable amb_cs new_tm_c new_ty_cs
  case mb_sat of
    Nothing -> pure Nothing
    Just delta -> do
      -- We know that the term and type constraints are inhabitable, so now
      -- check if each strict argument type is inhabitable.
      all_non_void <- checkAllNonVoid initRecTc delta strict_arg_tys
      pure $ if all_non_void -- Check if each strict argument type
                             -- is inhabitable
                then Just delta
                else Nothing

-- | Like 'pmIsSatisfiable', but only checks if term and type constraints are
-- satisfiable, and doesn't bother checking anything related to strict argument
-- types.
tmTyCsAreSatisfiable
  :: Delta       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TmVarCt -- ^ The new term constraint.
  -> Bag EvVar   -- ^ The new type constraints.
  -> PmM (Maybe Delta)
       -- ^ @'Just' delta@ if the constraints (@delta@) are
       -- satisfiable. 'Nothing' otherwise.
tmTyCsAreSatisfiable
    (MkDelta{ delta_tm_cs = amb_tm_cs, delta_ty_cs = amb_ty_cs })
    new_tm_cs new_ty_cs = do
  let ty_cs = new_ty_cs `unionBags` amb_ty_cs
  sat_ty <- if isEmptyBag new_ty_cs
               then pure True
               else tyOracle ty_cs
  pure $ case (sat_ty, tmOracle amb_tm_cs new_tm_cs) of
           (True, Just term_cs) -> Just $ MkDelta ty_cs term_cs
           _unsat               -> Nothing

ensureInhabited :: Delta -> Id -> PmM (Satisfiability Delta Void)
-- Given (x :: ty) and Delta (what we know about x),
--   figure out if anything can match 'x'.
-- We return a new Delta, logically equivalent to the old Delta,
--   but perhaps with some work done on it
-- Monadic because we may need to look up the possible
--   COMPLETE sets of a data type, and so we can run the
--   type oracle
ensureInhabited delta x = find_one_in_each_set delta
  where
    ty = idType x -- TODO: normalize?
    nm = idName x

    find_one_in_each_set delta = do
      suggestPossibleConLikes (delta_tm_cs delta) nm ty >>= \case
        Unsatisfiable              -> pure Unsatisfiable -- -XEmptyDataDecls
        PossiblySatisfiable tm_cs' -> pure (PossiblySatisfiable delta{ delta_tm_cs = tm_cs' })
        Satisfiable tm_cs' cons ->
          -- Run these candidates through the type oracle
          go delta{ delta_tm_cs = tm_cs' } (toList cons)
          where
            go delta []         = pure (PossiblySatisfiable delta)
            go delta (con:cons) = mkOneSatisfiableConFull delta x con >>= \case
              Just _  -> go delta cons -- success
              Nothing -> do
                -- Nope, try again, term oracle!
                tryAddRefutableAltCon (delta_tm_cs delta) nm ty (PmAltConLike con) >>= \case
                  Nothing -> pure Unsatisfiable -- term oracle says we are out of options
                  Just ts -> find_one_in_each_set (delta{ delta_tm_cs = ts })

-- | Implements two performance optimizations, as described in the
-- \"Strict argument type constraints\" section of
-- @Note [Extensions to GADTs Meet Their Match]@.
checkAllNonVoid :: RecTcChecker -> Delta -> [Type] -> PmM Bool
checkAllNonVoid rec_ts amb_cs strict_arg_tys = do
  fam_insts <- dsGetFamInstEnvs
  let definitely_inhabited =
        definitelyInhabitedType fam_insts (delta_ty_cs amb_cs)
  tys_to_check <- filterOutM definitely_inhabited strict_arg_tys
  let rec_max_bound | tys_to_check `lengthExceeds` 1
                    = 1
                    | otherwise
                    = defaultRecTcMaxBound
      rec_ts' = setRecTcMaxBound rec_max_bound rec_ts
  allM (nonVoid rec_ts' amb_cs) tys_to_check

-- | Checks if a strict argument type of a conlike is inhabitable by a
-- terminating value (i.e, an 'InhabitationCandidate').
-- See @Note [Extensions to GADTs Meet Their Match]@.
nonVoid
  :: RecTcChecker -- ^ The per-'TyCon' recursion depth limit.
  -> Delta        -- ^ The ambient term/type constraints (known to be
                  --   satisfiable).
  -> Type         -- ^ The strict argument type.
  -> PmM Bool     -- ^ 'True' if the strict argument type might be inhabited by
                  --   a terminating value (i.e., an 'InhabitationCandidate').
                  --   'False' if it is definitely uninhabitable by anything
                  --   (except bottom).
nonVoid rec_ts amb_cs strict_arg_ty = do
  mb_cands <- inhabitationCandidates (delta_ty_cs amb_cs) strict_arg_ty
  case mb_cands of
    Right (tc, _, cands)
      |  Just rec_ts' <- checkRecTc rec_ts tc
      -> anyM (cand_is_inhabitable rec_ts' amb_cs) cands
           -- A strict argument type is inhabitable by a terminating value if
           -- at least one InhabitationCandidate is inhabitable.
    _ -> pure True
           -- Either the type is trivially inhabited or we have exceeded the
           -- recursion depth for some TyCon (so bail out and conservatively
           -- claim the type is inhabited).
  where
    -- Checks if an InhabitationCandidate for a strict argument type:
    --
    -- (1) Has satisfiable term and type constraints.
    -- (2) Has 'nonVoid' strict argument types (we bail out of this
    --     check if recursion is detected).
    --
    -- See Note [Extensions to GADTs Meet Their Match]
    cand_is_inhabitable :: RecTcChecker -> Delta
                        -> InhabitationCandidate -> PmM Bool
    cand_is_inhabitable rec_ts amb_cs
      (InhabitationCandidate{ ic_tm_cs          = new_tm_cs
                            , ic_ty_cs          = new_ty_cs
                            , ic_strict_arg_tys = new_strict_arg_tys }) = do
        mb_sat <- tmTyCsAreSatisfiable amb_cs new_tm_cs new_ty_cs
        case mb_sat of
          Nothing -> pure False
          Just new_delta -> do
            checkAllNonVoid rec_ts new_delta new_strict_arg_tys

-- | @'definitelyInhabitedType' ty@ returns 'True' if @ty@ has at least one
-- constructor @C@ such that:
--
-- 1. @C@ has no equality constraints.
-- 2. @C@ has no strict argument types.
--
-- See the \"Strict argument type constraints\" section of
-- @Note [Extensions to GADTs Meet Their Match]@.
definitelyInhabitedType :: FamInstEnvs -> Bag EvVar -> Type -> PmM Bool
definitelyInhabitedType env ty_cs ty = do
  mb_res <- pmTopNormaliseType_maybe env ty_cs ty
  pure $ case mb_res of
           Just (_, cons, _) -> any meets_criteria cons
           Nothing           -> False
  where
    meets_criteria :: DataCon -> Bool
    meets_criteria con =
      null (dataConEqSpec con) && -- (1)
      null (dataConImplBangs con) -- (2)

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

Hence, if pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty),
then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of data constructors "skipped", every time we normalise a
      newtype to its core representation, we keep track of the source data
      constructor.
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty)
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

In this case pmTopNormaliseType_maybe env ty_cs (F Int) results in

  Just (G2, [MkG2,MkG1], R:TInt)

Which means that in source Haskell:
  - G2 is equivalent to F Int (in contrast, G1 isn't).
  - if (x : R:TInt) then (MkG2 (MkG1 x) : F Int).

-----
-- Wrinkle: Local equalities
-----

Given the following type family:

  type family F a
  type instance F Int = Void

Should the following program (from #14813) be considered exhaustive?

  f :: (i ~ Int) => F i -> a
  f x = case x of {}

You might think "of course, since `x` is obviously of type Void". But the
idType of `x` is technically F i, not Void, so if we pass F i to
inhabitationCandidates, we'll mistakenly conclude that `f` is non-exhaustive.
In order to avoid this pitfall, we need to normalise the type passed to
pmTopNormaliseType_maybe, using the constraint solver to solve for any local
equalities (such as i ~ Int) that may be in scope.
-}

-- | Generate all 'InhabitationCandidate's for a given type. The result is
-- either @'Left' ty@, if the type cannot be reduced to a closed algebraic type
-- (or if it's one trivially inhabited, like 'Int'), or @'Right' candidates@,
-- if it can. In this case, the candidates are the signature of the tycon, each
-- one accompanied by the term- and type- constraints it gives rise to.
-- See also Note [Checking EmptyCase Expressions]
inhabitationCandidates :: Bag EvVar -> Type
                       -> PmM (Either Type (TyCon, ValAbs, [InhabitationCandidate]))
inhabitationCandidates ty_cs ty = do
  fam_insts   <- dsGetFamInstEnvs
  mb_norm_res <- pmTopNormaliseType_maybe fam_insts ty_cs ty
  case mb_norm_res of
    Just (src_ty, dcs, core_ty) -> alts_to_check src_ty core_ty dcs
    Nothing                     -> alts_to_check ty     ty      []
  where
    -- All these types are trivially inhabited
    trivially_inhabited = [ charTyCon, doubleTyCon, floatTyCon
                          , intTyCon, wordTyCon, word8TyCon ]

    -- Note: At the moment we leave all the typing and constraint fields of
    -- PmCon empty, since we know that they are not gonna be used. Is the
    -- right-thing-to-do to actually create them, even if they are never used?
    build_tm :: PmExpr -> [DataCon] -> PmExpr
    build_tm = foldr (\dc e -> PmExprCon (PmAltConLike (RealDataCon dc)) [e])

    -- Inhabitation candidates, using the result of pmTopNormaliseType_maybe
    alts_to_check :: Type -> Type -> [DataCon]
                  -> PmM (Either Type (TyCon, ValAbs, [InhabitationCandidate]))
    alts_to_check src_ty core_ty dcs = case splitTyConApp_maybe core_ty of
      Just (tc, _)
        |  tc `elem` trivially_inhabited
        -> case dcs of
             []    -> return (Left src_ty)
             (_:_) -> do inner <- mkPmId core_ty
                         let expr = build_tm (idToPmExpr inner) dcs
                         outer <- mkPmId src_ty
                         return $ Right (tc, outer, [InhabitationCandidate
                           { ic_tm_cs = unitBag (TVC outer expr)
                           , ic_ty_cs = emptyBag, ic_strict_arg_tys = [] }])

        |  pmIsClosedType core_ty && not (isAbstractTyCon tc)
           -- Don't consider abstract tycons since we don't know what their
           -- constructors are, which makes the results of coverage checking
           -- them extremely misleading.
        -> do
             inner <- mkPmId core_ty -- it would be wrong to unify x
             alts <- mapM (fmap fst . mkOneConFull inner . RealDataCon) (tyConDataCons tc)
             outer <- mkPmId src_ty
             let new_tm_ct = TVC outer (build_tm (idToPmExpr inner) dcs)
             let wrap_dcs alt = alt{ ic_tm_cs = new_tm_ct `consBag` ic_tm_cs alt}
             return $ Right (tc, outer, map wrap_dcs alts)
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

nullaryConPattern :: ConLike -> PmPat
-- Nullary data constructor and nullary type constructor
nullaryConPattern con =
  PmCon { pm_con_con = con, pm_con_arg_tys = []
        , pm_con_tvs = [], pm_con_args = [] }
{-# INLINE nullaryConPattern #-}

truePattern :: PmPat
truePattern = nullaryConPattern (RealDataCon trueDataCon)
{-# INLINE truePattern #-}

-- | Generate a `canFail` pattern vector of a specific type
mkCanFailPmPat :: Type -> PmM PatVec
mkCanFailPmPat ty = do
  var <- mkPmVar ty
  return [var, PmFake]

vanillaConPattern :: ConLike -> [Type] -> PatVec -> PmPat
-- ADT constructor pattern => no existentials, no local constraints
vanillaConPattern con arg_tys args =
  PmCon { pm_con_con = con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_args = args }
{-# INLINE vanillaConPattern #-}

-- | Create an empty list pattern of a given type
nilPattern :: Type -> PmPat
nilPattern ty =
  PmCon { pm_con_con = RealDataCon nilDataCon, pm_con_arg_tys = [ty]
        , pm_con_tvs = [], pm_con_args = [] }
{-# INLINE nilPattern #-}

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = RealDataCon consDataCon
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = []
                               , pm_con_args = xs++ys }]
{-# INLINE mkListPatVec #-}

-- | Create a (non-overloaded) literal pattern
mkLitPattern :: HsLit GhcTc -> PmPat
mkLitPattern lit = PmLit { pm_lit_lit = PmSLit lit }
{-# INLINE mkLitPattern #-}

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into of (PmPat Id)

translatePat :: FamInstEnvs -> Pat GhcTc -> PmM PatVec
translatePat fam_insts pat = case pat of
  WildPat  ty  -> mkPmVars [ty]
  VarPat _ id  -> return [PmVar (unLoc id)]
  ParPat _ p   -> translatePat fam_insts (unLoc p)
  LazyPat _ _  -> mkPmVars [hsPatType pat] -- like a variable

  -- ignore strictness annotations for now
  BangPat _ p  -> translatePat fam_insts (unLoc p)

  AsPat _ lid p -> do
     -- Note [Translating As Patterns]
    ps <- translatePat fam_insts (unLoc p)
    let [e] = patVecToPmExprs ps
        g   = PmGrd [PmVar (unLoc lid)] e
    return (ps ++ [g])

  SigPat _ p _ty -> translatePat fam_insts (unLoc p)

  -- See Note [Translate CoPats]
  CoPat _ wrapper p ty
    | isIdHsWrapper wrapper                   -> translatePat fam_insts p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts p
    | otherwise -> do
        ps      <- translatePat fam_insts p
        (xp,xe) <- mkPmId2Forms ty
        g <- mkGuard ps (mkHsWrap wrapper (unLoc xe))
        return [xp,g]

  -- (n + k)  ===>   x (True <- x >= k) (n <- x-k)
  NPlusKPat ty (dL->L _ _n) _k1 _k2 _ge _minus -> mkCanFailPmPat ty

  -- (fun -> pat)   ===>   x (pat <- fun x)
  ViewPat arg_ty lexpr lpat -> do
    ps <- translatePat fam_insts (unLoc lpat)
    -- See Note [Guards and Approximation]
    res <- allM cantFailPattern ps
    case res of
      True  -> do
        (xp,xe) <- mkPmId2Forms arg_ty
        g <- mkGuard ps (HsApp noExtField lexpr xe)
        return [xp,g]
      False -> mkCanFailPmPat arg_ty

  -- list
  ListPat (ListPatTc ty Nothing) ps -> do
    foldr (mkListPatVec ty) [nilPattern ty]
      <$> translatePatVec fam_insts (map unLoc ps)

  -- overloaded list
  ListPat (ListPatTc _elem_ty (Just (pat_ty, _to_list))) lpats -> do
    dflags <- getDynFlags
    if xopt LangExt.RebindableSyntax dflags
       then mkCanFailPmPat pat_ty
       else case splitListTyConApp_maybe pat_ty of
              Just e_ty -> translatePat fam_insts
                                        (ListPat (ListPatTc e_ty Nothing) lpats)
              Nothing   -> mkCanFailPmPat pat_ty
    -- (a) In the presence of RebindableSyntax, we don't know anything about
    --     `toList`, we should treat `ListPat` as any other view pattern.
    --
    -- (b) In the absence of RebindableSyntax,
    --     - If the pat_ty is `[a]`, then we treat the overloaded list pattern
    --       as ordinary list pattern. Although we can give an instance
    --       `IsList [Int]` (more specific than the default `IsList [a]`), in
    --       practice, we almost never do that. We assume the `_to_list` is
    --       the `toList` from `instance IsList [a]`.
    --
    --     - Otherwise, we treat the `ListPat` as ordinary view pattern.
    --
    -- See #14547, especially comment#9 and comment#10.
    --
    -- Here we construct CanFailPmPat directly, rather can construct a view
    -- pattern and do further translation as an optimization, for the reason,
    -- see Note [Guards and Approximation].

  ConPatOut { pat_con     = (dL->L _ con)
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_args    = ps } -> do
    let ty = conLikeResTy con arg_tys
    groups <- allCompleteMatches ty
    case groups of
      [] -> mkCanFailPmPat ty
      _  -> do
        args <- translateConPatVec fam_insts arg_tys ex_tvs con ps
        return [PmCon { pm_con_con     = con
                      , pm_con_arg_tys = arg_tys
                      , pm_con_tvs     = ex_tvs
                      , pm_con_args    = args }]

  -- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
  NPat ty (dL->L _ olit) mb_neg _
    | Just lit <- hsOverLitAsHsLit olit (isJust mb_neg) ty
    -> translatePat fam_insts (LitPat noExtField lit)
    | otherwise
    -> return [PmLit { pm_lit_lit = PmOLit (isJust mb_neg) olit }]

  -- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
  LitPat _ lit
    | HsString src s <- lit ->
        foldr (mkListPatVec charTy) [nilPattern charTy] <$>
          translatePatVec fam_insts
            (map (LitPat noExtField . HsChar src) (unpackFS s))
    | otherwise -> return [mkLitPattern lit]

  TuplePat tys ps boxity -> do
    tidy_ps <- translatePatVec fam_insts (map unLoc ps)
    let tuple_con = RealDataCon (tupleDataCon boxity (length ps))
        tys' = case boxity of
                 Boxed -> tys
                 -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
                 Unboxed -> map getRuntimeRep tys ++ tys
    return [vanillaConPattern tuple_con tys' (concat tidy_ps)]

  SumPat ty p alt arity -> do
    tidy_p <- translatePat fam_insts (unLoc p)
    let sum_con = RealDataCon (sumDataCon alt arity)
    -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    return [vanillaConPattern sum_con (map getRuntimeRep ty ++ ty) tidy_p]

  -- --------------------------------------------------------------------------
  -- Not supposed to happen
  ConPatIn  {} -> panic "Check.translatePat: ConPatIn"
  SplicePat {} -> panic "Check.translatePat: SplicePat"
  XPat      {} -> panic "Check.translatePat: XPat"

-- | Translate a list of patterns (Note: each pattern is translated
-- to a pattern vector but we do not concatenate the results).
translatePatVec :: FamInstEnvs -> [Pat GhcTc] -> PmM [PatVec]
translatePatVec fam_insts pats = mapM (translatePat fam_insts) pats

-- | Translate a constructor pattern
translateConPatVec :: FamInstEnvs -> [Type] -> [TyVar]
                   -> ConLike -> HsConPatDetails GhcTc -> PmM PatVec
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
                            Just x  -> PmGrd pvec (idToPmExpr x)
                            Nothing -> panic "translateConPatVec: lookup")
                       translated_pats

      return (arg_var_pats ++ guards)
  where
    -- The actual argument types (instantiated)
    arg_tys = conLikeInstOrigArgTys c (univ_tys ++ mkTyVarTys ex_tvs)

    -- Some label information
    orig_lbls    = map flSelector $ conLikeFieldLabels c
    matched_pats = [ (getName (unLoc (hsRecFieldId x)), unLoc (hsRecFieldArg x))
                   | (dL->L _ x) <- fs]
    matched_lbls = [ name | (name, _pat) <- matched_pats ]

    subsetOf :: Eq a => [a] -> [a] -> Bool
    subsetOf []     _  = True
    subsetOf (_:_)  [] = False
    subsetOf (x:xs) (y:ys)
      | x == y    = subsetOf    xs  ys
      | otherwise = subsetOf (x:xs) ys

-- Translate a single match
translateMatch :: FamInstEnvs -> LMatch GhcTc (LHsExpr GhcTc)
               -> PmM (PatVec,[PatVec])
translateMatch fam_insts (dL->L _ (Match { m_pats = lpats, m_grhss = grhss })) =
  do
  pats'   <- concat <$> translatePatVec fam_insts pats
  guards' <- mapM (translateGuards fam_insts) guards
  -- tracePm "translateMatch" (vcat [ppr pats, ppr pats', ppr guards, ppr guards'])
  return (pats', guards')
  where
    extractGuards :: LGRHS GhcTc (LHsExpr GhcTc) -> [GuardStmt GhcTc]
    extractGuards (dL->L _ (GRHS _ gs _)) = map unLoc gs
    extractGuards _                       = panic "translateMatch"

    pats   = map unLoc lpats
    guards = map extractGuards (grhssGRHSs grhss)
translateMatch _ _ = panic "translateMatch"

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to PmPats (Pattern)

-- | Translate a list of guard statements to a pattern vector
translateGuards :: FamInstEnvs -> [GuardStmt GhcTc] -> PmM PatVec
translateGuards fam_insts guards = do
  all_guards <- concat <$> mapM (translateGuard fam_insts) guards
  let
    shouldKeep :: PmPat -> PmM Bool
    shouldKeep p
      | PmVar {} <- p = pure True
      | PmCon {} <- p = (&&)
                          <$> singleMatchConstructor (pm_con_con p) (pm_con_arg_tys p)
                          <*> allM shouldKeep (pm_con_args p)
    shouldKeep (PmGrd pv e)
      | isNotPmExprOther e = pure True  -- expensive but we want it
      | otherwise          = allM shouldKeep pv
    shouldKeep _other_pat  = pure False -- let the rest..

  all_handled <- allM shouldKeep all_guards
  -- It should have been @pure all_guards@ but it is too expressive.
  -- Since the term oracle does not handle all constraints we generate,
  -- we (hackily) replace all constraints the oracle cannot handle with a
  -- single one (we need to know if there is a possibility of failure).
  -- See Note [Guards and Approximation] for all guard-related approximations
  -- we implement.
  if all_handled
    then pure all_guards
    else do
      kept <- filterM shouldKeep all_guards
      pure (PmFake : kept)

-- | Check whether a pattern can fail to match
cantFailPattern :: PmPat -> PmM Bool
cantFailPattern PmVar {}      = pure True
cantFailPattern PmCon { pm_con_con = c, pm_con_arg_tys = tys, pm_con_args = ps}
  = (&&) <$> singleMatchConstructor c tys <*> allM cantFailPattern ps
cantFailPattern (PmGrd pv _e) = allM cantFailPattern pv
cantFailPattern _             = pure False

-- | Translate a guard statement to Pattern
translateGuard :: FamInstEnvs -> GuardStmt GhcTc -> PmM PatVec
translateGuard fam_insts guard = case guard of
  BodyStmt _   e _ _ -> translateBoolGuard e
  LetStmt  _   binds -> translateLet (unLoc binds)
  BindStmt _ p e _ _ -> translateBind fam_insts p e
  LastStmt        {} -> panic "translateGuard LastStmt"
  ParStmt         {} -> panic "translateGuard ParStmt"
  TransStmt       {} -> panic "translateGuard TransStmt"
  RecStmt         {} -> panic "translateGuard RecStmt"
  ApplicativeStmt {} -> panic "translateGuard ApplicativeLastStmt"
  XStmtLR nec        -> noExtCon nec

-- | Translate let-bindings
translateLet :: HsLocalBinds GhcTc -> PmM PatVec
translateLet _binds = return []

-- | Translate a pattern guard
translateBind :: FamInstEnvs -> LPat GhcTc -> LHsExpr GhcTc -> PmM PatVec
translateBind fam_insts (dL->L _ p) e = do
  ps <- translatePat fam_insts p
  g <- mkGuard ps (unLoc e)
  return [g]

-- | Translate a boolean guard
translateBoolGuard :: LHsExpr GhcTc -> PmM PatVec
translateBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- PatVec for efficiency
  | otherwise = (:[]) <$> mkGuard [truePattern] (unLoc e)

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
     @PmFake@: @True <- _@. That is, we record that there is a possibility
     of failure but we minimize it to a True/False. This generates a single
     warning and much smaller uncovered sets.

2. Overloaded Lists
-------------------
An overloaded list @[...]@ should be translated to @x ([...] <- toList x)@. The
problem is exactly like above, as its solution. For future reference, the code
below is the *right thing to do*:

   ListPat (ListPatTc elem_ty (Just (pat_ty, _to_list))) lpats
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
They are mainly covered in Note [Undecidable Equality for PmAltCons] and
Note [Translate Overloaded Literals for Exhaustiveness Checking] in PmExpr.

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
1-4) with a single @PmFake@ to record the possibility of failure to match.

Note [Translate CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns `CoPat`
efficiently, which gave rise to #11276. The original approach translated
`CoPat`s:

    pat |> co    ===>    x (pat <- (e |> co))

Why did we do this seemingly unnecessary expansion in the first place?
The reason is that the type of @pat |> co@ (which is the type of the value
abstraction we match against) might be different than that of @pat@. Data
instances such as @Sing (a :: Bool)@ are a good example of this: If we would
just drop the coercion, we'd get a type error when matching @pat@ against its
value abstraction, with the result being that pmIsSatisfiable decides that every
possible data constructor fitting @pat@ is rejected as uninhabitated, leading to
a lot of false warnings.

But we can check whether the coercion is a hole or if it is just refl, in
which case we can drop it.

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
pmPatType :: PmPat -> Type
pmPatType (PmCon { pm_con_con = con, pm_con_arg_tys = tys })
  = conLikeResTy con tys
pmPatType (PmVar  { pm_var_id  = x }) = idType x
pmPatType (PmLit  { pm_lit_lit = l }) = pmLitType l
pmPatType (PmGrd  { pm_grd_pv  = pv })
  = ASSERT(patVecArity pv == 1) (pmPatType p)
  where Just p = find ((==1) . patternArity) pv
pmPatType PmFake = pmPatType truePattern

-- | Information about a conlike that is relevant to coverage checking.
-- It is called an \"inhabitation candidate\" since it is a value which may
-- possibly inhabit some type, but only if its term constraints ('ic_tm_cs')
-- and type constraints ('ic_ty_cs') are permitting, and if all of its strict
-- argument types ('ic_strict_arg_tys') are inhabitable.
-- See @Note [Extensions to GADTs Meet Their Match]@.
data InhabitationCandidate =
  InhabitationCandidate
  { ic_tm_cs          :: Bag TmVarCt
  , ic_ty_cs          :: Bag EvVar
  , ic_strict_arg_tys :: [Type]
  }

{-
Note [Extensions to GADTs Meet Their Match]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GADTs Meet Their Match paper presents the formalism that GHC's coverage
checker adheres to. Since the paper's publication, there have been some
additional features added to the coverage checker which are not described in
the paper. This Note serves as a reference for these new features.

* Handling of uninhabited fields like `!Void`.
  See Note [Strict argument type constraints]
* Efficient handling of literal splitting, large enumerations and accurate
  redundancy warnings for `COMPLETE` groups through the term oracle.
  See Note [Refutable shapes] in TmOracle.

Note [Strict argument type constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the ConVar case of clause processing, each conlike K traditionally
generates two different forms of constraints:

* A term constraint (e.g., x ~ K y1 ... yn)
* Type constraints from the conlike's context (e.g., if K has type
  forall bs. Q => s1 .. sn -> T tys, then Q would be its type constraints)

As it turns out, these alone are not enough to detect a certain class of
unreachable code. Consider the following example (adapted from #15305):

  data K = K1 | K2 !Void

  f :: K -> ()
  f K1 = ()

Even though `f` doesn't match on `K2`, `f` is exhaustive in its patterns. Why?
Because it's impossible to construct a terminating value of type `K` using the
`K2` constructor, and thus it's impossible for `f` to ever successfully match
on `K2`.

The reason is because `K2`'s field of type `Void` is //strict//. Because there
are no terminating values of type `Void`, any attempt to construct something
using `K2` will immediately loop infinitely or throw an exception due to the
strictness annotation. (If the field were not strict, then `f` could match on,
say, `K2 undefined` or `K2 (let x = x in x)`.)

Since neither the term nor type constraints mentioned above take strict
argument types into account, we make use of the `nonVoid` function to
determine whether a strict type is inhabitable by a terminating value or not.

`nonVoid ty` returns True when either:
1. `ty` has at least one InhabitationCandidate for which both its term and type
   constraints are satifiable, and `nonVoid` returns `True` for all of the
   strict argument types in that InhabitationCandidate.
2. We're unsure if it's inhabited by a terminating value.

`nonVoid ty` returns False when `ty` is definitely uninhabited by anything
(except bottom). Some examples:

* `nonVoid Void` returns False, since Void has no InhabitationCandidates.
  (This is what lets us discard the `K2` constructor in the earlier example.)
* `nonVoid (Int :~: Int)` returns True, since it has an InhabitationCandidate
  (through the Refl constructor), and its term constraint (x ~ Refl) and
  type constraint (Int ~ Int) are satisfiable.
* `nonVoid (Int :~: Bool)` returns False. Although it has an
  InhabitationCandidate (by way of Refl), its type constraint (Int ~ Bool) is
  not satisfiable.
* Given the following definition of `MyVoid`:

    data MyVoid = MkMyVoid !Void

  `nonVoid MyVoid` returns False. The InhabitationCandidate for the MkMyVoid
  constructor contains Void as a strict argument type, and since `nonVoid Void`
  returns False, that InhabitationCandidate is discarded, leaving no others.

* Performance considerations

We must be careful when recursively calling `nonVoid` on the strict argument
types of an InhabitationCandidate, because doing so naïvely can cause GHC to
fall into an infinite loop. Consider the following example:

  data Abyss = MkAbyss !Abyss

  stareIntoTheAbyss :: Abyss -> a
  stareIntoTheAbyss x = case x of {}

In principle, stareIntoTheAbyss is exhaustive, since there is no way to
construct a terminating value using MkAbyss. However, both the term and type
constraints for MkAbyss are satisfiable, so the only way one could determine
that MkAbyss is unreachable is to check if `nonVoid Abyss` returns False.
There is only one InhabitationCandidate for Abyss—MkAbyss—and both its term
and type constraints are satisfiable, so we'd need to check if `nonVoid Abyss`
returns False... and now we've entered an infinite loop!

To avoid this sort of conundrum, `nonVoid` uses a simple test to detect the
presence of recursive types (through `checkRecTc`), and if recursion is
detected, we bail out and conservatively assume that the type is inhabited by
some terminating value. This avoids infinite loops at the expense of making
the coverage checker incomplete with respect to functions like
stareIntoTheAbyss above. Then again, the same problem occurs with recursive
newtypes, like in the following code:

  newtype Chasm = MkChasm Chasm

  gazeIntoTheChasm :: Chasm -> a
  gazeIntoTheChasm x = case x of {} -- Erroneously warned as non-exhaustive

So this limitation is somewhat understandable.

Note that even with this recursion detection, there is still a possibility that
`nonVoid` can run in exponential time. Consider the following data type:

  data T = MkT !T !T !T

If we call `nonVoid` on each of its fields, that will require us to once again
check if `MkT` is inhabitable in each of those three fields, which in turn will
require us to check if `MkT` is inhabitable again... As you can see, the
branching factor adds up quickly, and if the recursion depth limit is, say,
100, then `nonVoid T` will effectively take forever.

To mitigate this, we check the branching factor every time we are about to call
`nonVoid` on a list of strict argument types. If the branching factor exceeds 1
(i.e., if there is potential for exponential runtime), then we limit the
maximum recursion depth to 1 to mitigate the problem. If the branching factor
is exactly 1 (i.e., we have a linear chain instead of a tree), then it's okay
to stick with a larger maximum recursion depth.

Another microoptimization applies to data types like this one:

  data S a = ![a] !T

Even though there is a strict field of type [a], it's quite silly to call
nonVoid on it, since it's "obvious" that it is inhabitable. To make this
intuition formal, we say that a type is definitely inhabitable (DI) if:

  * It has at least one constructor C such that:
    1. C has no equality constraints (since they might be unsatisfiable)
    2. C has no strict argument types (since they might be uninhabitable)

It's relatively cheap to check if a type is DI, so before we call `nonVoid`
on a list of strict argument types, we filter out all of the DI ones.
-}

instance Outputable InhabitationCandidate where
  ppr (InhabitationCandidate { ic_tm_cs = tm_ct
                             , ic_ty_cs = ty_cs
                             , ic_strict_arg_tys = strict_arg_tys }) =
    text "InhabitationCandidate" <+>
      vcat [ text "ic_tm_cs          =" <+> ppr tm_ct
           , text "ic_ty_cs          =" <+> ppr ty_cs
           , text "ic_strict_arg_tys =" <+> ppr strict_arg_tys ]

-- | Generate an 'InhabitationCandidate' for a given 'ConLike'.
-- Generate fresh variables of the appropriate type for arguments and return
-- them alongside.
mkOneConFull :: Id -> ConLike -> PmM (InhabitationCandidate, [ValAbs])
--  *  x :: T tys, where T is an algebraic data type
--     NB: in the case of a data family, T is the *representation* TyCon
--     e.g.   data instance T (a,b) = T1 a b
--       leads to
--            data TPair a b = T1 a b  -- The "representation" type
--       It is TPair, not T, that is given to mkOneConFull
--
--  * 'con' K is a conlike of data type T
--
-- After instantiating the universal tyvars of K we get
--          K tys :: forall bs. Q => s1 .. sn -> T tys
--
-- Suppose y1 is a strict field. Then we get
-- Results: ic_tm_cs:          x ~ K (y1::s1) .. (yn::sn)
--          ic_ty_cs:          Q
--          ic_strict_arg_tys: [s1]
--          [y1,..,yn]
mkOneConFull x con = do
  let res_ty  = idType x
      (univ_tvs, _ex_tvs, eq_spec, thetas, _req_theta , arg_tys, con_res_ty)
        = conLikeFullSig con
      arg_is_banged = map isBanged $ conLikeImplBangs con
      -- tyConAppArgs crashes for T11336(b), so use splitAppTy instead. Should
      -- be fine after type-checking.
      (_, tc_args) = splitAppTys res_ty
  let subst  = case con of
                  RealDataCon {} -> zipTvSubst univ_tvs tc_args
                  -- The expectJust is always satisfied as long as we filter
                  -- with 'isValidCompleteMatch' in 'allCompleteMatches'
                  PatSynCon {}   -> expectJust "mkOneConFull" (tcMatchTy con_res_ty res_ty)
                                    -- See Note [Pattern synonym result type] in PatSyn

  let arg_tys' = substTys subst arg_tys
  -- Fresh term variables (VAs) as arguments to the constructor
  vars <- mapM mkPmId arg_tys'
  let args = map idToPmExpr vars
  -- All constraints bound by the constructor (alpha-renamed)
  let theta_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  evvars <- mapM (nameType "pm") theta_cs
  let expr = PmExprCon (PmAltConLike con) args
      strict_arg_tys = filterByList arg_is_banged arg_tys'
  let ic = InhabitationCandidate
           { ic_tm_cs          = unitBag (TVC x expr)
           , ic_ty_cs          = listToBag evvars
           , ic_strict_arg_tys = strict_arg_tys
           }
  return (ic, vars)

-- | Invoke 'mkOneConFull' and immediately check whether the resulting
-- 'InhabitationCandidate' @ic@ with arguments @arg_vars@ is inhabited by
-- consulting 'pmIsSatisfiable'. Return @Just (new_delta, ic, arg_vars)@ if it
-- is.
mkOneSatisfiableConFull :: Delta -> Id -> ConLike -> PmM (Maybe (Delta, InhabitationCandidate, [ValAbs]))
mkOneSatisfiableConFull delta x con = do
  -- mkOneConFull doesn't cope with type families, so we have to normalise
  -- x's result type first and introduce an auxiliary binding.
  fam_insts <- dsGetFamInstEnvs
  mb_res_ty <- pmTopNormaliseType_maybe fam_insts (delta_ty_cs delta) (idType x)
  let res_ty = fromMaybe (idType x) (fstOf3 <$> mb_res_ty)
  (y, delta') <- mkIdCoercion x res_ty delta
  tracePm "coercing" (ppr x $$ ppr (idType x) $$ ppr y $$ ppr res_ty)
  (ic, arg_vars) <- mkOneConFull y con
  tracePm "mkOneSatisfiableConFull" (ppr x <+> ppr y $$ ppr ic $$ ppr (delta_tm_cs delta'))
  mb_delta <- pmIsSatisfiable delta' (ic_tm_cs ic) (ic_ty_cs ic) (ic_strict_arg_tys ic)
  pure ((,ic,arg_vars) <$> mb_delta)

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

-- | Introduce a new 'Id' that has the given type and is in the same equivalence
-- class as the argument.
mkIdCoercion :: Id -> Type -> Delta -> PmM (Id, Delta)
mkIdCoercion x ty delta
  | eqType (idType x) ty = pure (x, delta) -- no need to introduce anything new
  | otherwise = do
      y <- mkPmId ty
      let e = idToPmExpr x
      pure (y, delta { delta_tm_cs = extendSubst y e (delta_tm_cs delta) })

-- | Create a guard pattern
mkGuard :: PatVec -> HsExpr GhcTc -> PmM PmPat
mkGuard pv e = do
  res <- allM cantFailPattern pv
  let expr = hsExprToPmExpr e
  tracePm "mkGuard" (vcat [ppr pv, ppr e, ppr res, ppr expr])
  if | res                    -> pure (PmGrd pv expr)
     | PmExprOther {} <- expr -> pure PmFake
     | otherwise              -> pure (PmGrd pv expr)

-- | Generate a variable pattern of a given type
mkPmVar :: Type -> PmM PmPat
mkPmVar ty = PmVar <$> mkPmId ty

-- | Generate many variable patterns, given a list of types
mkPmVars :: [Type] -> PmM PatVec
mkPmVars tys = mapM mkPmVar tys

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> PmM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "$pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalId name ty)

-- | Generate a fresh term variable of a given and return it in two forms:
-- * A variable pattern
-- * A variable expression
mkPmId2Forms :: Type -> PmM (PmPat, LHsExpr GhcTc)
mkPmId2Forms ty = do
  x <- mkPmId ty
  return (PmVar x, noLoc (HsVar noExtField (noLoc x)))

-- ----------------------------------------------------------------------------
-- * Converting between Value Abstractions, Patterns and PmExpr

-- | Convert an 'Id' (or 'ValAbs') into an expression
idToPmExpr :: Id -> PmExpr
idToPmExpr = PmExprVar . idName

-- | Convert a pattern vector to a list of 'PmExpr's by dropping the guards
-- (See Note [Translating As Patterns])
patVecToPmExprs :: PatVec -> [PmExpr]
patVecToPmExprs = mapMaybe pmPatToPmExpr

-- | Convert a pattern to a 'PmExpr' (will be either 'Nothing' if the pattern is
-- a guard pattern, or 'Just' an expression in all other cases) by dropping the
-- guards (See Note [Translating As Patterns])
pmPatToPmExpr :: PmPat -> Maybe PmExpr
pmPatToPmExpr (PmVar { pm_var_id  = x }) = Just (idToPmExpr x)
pmPatToPmExpr (PmLit { pm_lit_lit = l }) = Just (PmExprCon (PmAltLit l) [])
pmPatToPmExpr (PmCon { pm_con_con = con, pm_con_args = args })
  = Just (PmExprCon (PmAltConLike con) (patVecToPmExprs args))
pmPatToPmExpr _ = Nothing -- drop the guards

-- | Check whether a 'ConLike' has the /single match/ property, i.e. whether
-- it is the only possible match in the given context. See also
-- 'allCompleteMatches' and Note [Single match constructors].
singleMatchConstructor :: ConLike -> [Type] -> PmM Bool
singleMatchConstructor cl tys =
  any isSingleton <$> allCompleteMatches (conLikeResTy cl tys)

{-
Note [Single match constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When translating pattern guards for consumption by the checker, we desugar
every pattern guard that might fail ('cantFailPattern') to 'PmFake'
(True <- _). Which patterns can't fail? Exactly those that only match on
'singleMatchConstructor's.

Here are a few examples:
  * @f a | (a, b) <- foo a = 42@: Product constructors are generally
    single match. This extends to single constructors of GADTs like 'Refl'.
  * If @f | Id <- id () = 42@, where @pattern Id = ()@ and 'Id' is part of a
    singleton `COMPLETE` set, then 'Id' has the single match property.

In effect, we can just enumerate 'allCompleteMatches' and check if the conlike
occurs as a singleton set.
There's the chance that 'Id' is part of multiple `COMPLETE` sets. That's
irrelevant; If the user specified a singleton set, it is single-match.

Note that this doesn't really take into account incoming type constraints;
It might be obvious from type context that a particular GADT constructor has
the single-match property. We currently don't (can't) check this in the
translation step. See #15753 for why this yields surprising results.
-}

-- | For a given type, finds all the COMPLETE sets of conlikes that inhabit it.
--
-- These come from two places.
--  1. From data constructors defined with the result type constructor.
--  2. From `COMPLETE` pragmas which have the same type as the result
--     type constructor. Note that we only use `COMPLETE` pragmas
--     *all* of whose pattern types match. See #14135
allCompleteMatches :: Type -> DsM [[ConLike]]
allCompleteMatches ty = case splitTyConApp_maybe ty of
  Nothing -> pure [] -- NB: We don't know any COMPLETE set, as opposed to [[]]
  Just (tc, _) -> do
    let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc
    let maybe_to_list = maybe [] (:[])
    let rdcs = maybe_to_list mb_rdcs
    pragmas <- dsGetCompleteMatches tc
    let fams = mapM dsLookupConLike . completeMatchConLikes
    pscs <- mapM fams pragmas
    let candidates = rdcs ++ pscs
    -- Check that all the pattern synonym return types in a `COMPLETE`
    -- pragma subsume the type we're matching.
    -- See Note [Filtering out non-matching COMPLETE sets]
    pure (filter (isValidCompleteMatch ty) candidates)
      where
        isValidCompleteMatch :: Type -> [ConLike] -> Bool
        isValidCompleteMatch ty = all p
          where
            p (RealDataCon _) = True
            p (PatSynCon ps)  = isJust (tcMatchTy (projResTy (patSynSig ps)) ty)
            projResTy (_, _, _, _, _, res_ty) = res_ty

{-
Note [Filtering out non-matching COMPLETE sets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, conlikes in a COMPLETE set are simply grouped by the
type constructor heading the return type. This is nice and simple, but it does
mean that there are scenarios when a COMPLETE set might be incompatible with
the type of a scrutinee. For instance, consider (from #14135):

  data Foo a = Foo1 a | Foo2 a

  pattern MyFoo2 :: Int -> Foo Int
  pattern MyFoo2 i = Foo2 i

  {-# COMPLETE Foo1, MyFoo2 #-}

  f :: Foo a -> a
  f (Foo1 x) = x

`f` has an incomplete pattern-match, so when choosing which constructors to
report as unmatched in a warning, GHC must choose between the original set of
data constructors {Foo1, Foo2} and the COMPLETE set {Foo1, MyFoo2}. But observe
that GHC shouldn't even consider the COMPLETE set as a possibility: the return
type of MyFoo2, Foo Int, does not match the type of the scrutinee, Foo a, since
there's no substitution `s` such that s(Foo Int) = Foo a.

To ensure that GHC doesn't pick this COMPLETE set, it checks each pattern
synonym constructor's return type matches the type of the scrutinee, and if one
doesn't, then we remove the whole COMPLETE set from consideration.

One might wonder why GHC only checks /pattern synonym/ constructors, and not
/data/ constructors as well. The reason is because that the type of a
GADT constructor very well may not match the type of a scrutinee, and that's
OK. Consider this example (from #14059):

  data SBool (z :: Bool) where
    SFalse :: SBool False
    STrue  :: SBool True

  pattern STooGoodToBeTrue :: forall (z :: Bool). ()
                           => z ~ True
                           => SBool z
  pattern STooGoodToBeTrue = STrue
  {-# COMPLETE SFalse, STooGoodToBeTrue #-}

  wobble :: SBool z -> Bool
  wobble STooGoodToBeTrue = True

In the incomplete pattern match for `wobble`, we /do/ want to warn that SFalse
should be matched against, even though its type, SBool False, does not match
the scrutinee type, SBool z.
-}

-- -----------------------------------------------------------------------
-- * Types and constraints

newEvVar :: Name -> Type -> EvVar
newEvVar name ty = mkLocalId name ty

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
  = do { ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability evs
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
patternArity :: PmPat -> PmArity
patternArity (PmGrd {}) = 0
patternArity PmFake     = 0
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
  delta <- pmInitialTmTyCs
  return [ValVec vars delta]

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheck`
pmcheckI :: PatVec -> [PatVec] -> ValVec -> PmM PartialResult
pmcheckI ps guards vva = do
  n <- incrCheckPmIterDs
  tracePm "pmCheck" (ppr n <> colon
                        $$ hang (text "patterns:") 2 (ppr ps)
                        $$ hang (text "guards:") 2 (ppr guards)
                        $$ ppr vva)
  res <- pmcheck ps guards vva
  tracePm "pmCheckResult:" (ppr res)
  return res
{-# INLINE pmcheckI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckGuards`
pmcheckGuardsI :: [PatVec] -> ValVec -> PmM PartialResult
pmcheckGuardsI gvs vva = incrCheckPmIterDs >> pmcheckGuards gvs vva
{-# INLINE pmcheckGuardsI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckHd`
pmcheckHdI :: PmPat -> PatVec -> [PatVec] -> ValAbs -> ValVec
           -> PmM PartialResult
pmcheckHdI p ps guards va vva = do
  n <- incrCheckPmIterDs
  tracePm "pmCheckHdI" (ppr n <> colon <+> ppr p
                        $$ hang (text "patterns:") 2 (ppr ps)
                        $$ hang (text "guards:") 2 (ppr guards)
                        $$ ppr va
                        $$ ppr vva)

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
pmcheck (PmFake : ps) guards vva =
  -- short-circuit if the guard pattern is useless.
  -- we just have two possible outcomes: fail here or match and recurse
  -- none of the two contains any useful information about the failure
  -- though. So just have these two cases but do not do all the boilerplate
  forces . mkCons vva <$> pmcheckI ps guards vva
pmcheck (p : ps) guards (ValVec vas delta)
  | PmGrd { pm_grd_pv = pv, pm_grd_expr = e } <- p
  = do
      tracePm "PmGrd: pmPatType" (hcat [ppr p, ppr (pmPatType p)])
      y <- mkPmId (pmPatType p)
      let tm_state = extendSubst y e (delta_tm_cs delta)
          delta'   = delta { delta_tm_cs = tm_state }
      pr <- pmcheckI (pv ++ ps) guards (ValVec (y : vas) delta')
      pure (utail pr)

pmcheck [] _ (ValVec (_:_) _) = panic "pmcheck: nil-cons"
pmcheck (_:_) _ (ValVec [] _) = panic "pmcheck: cons-nil"

pmcheck (p:ps) guards (ValVec (va:vva) delta)
  = pmcheckHdI p ps guards va (ValVec vva delta)

-- | Check the list of guards
pmcheckGuards :: [PatVec] -> ValVec -> PmM PartialResult
pmcheckGuards []       vva = return (usimple [vva])
pmcheckGuards (gv:gvs) vva = do
  (PartialResult cs vsa ds) <- pmcheckI gv [] vva
  (PartialResult css vsas dss) <- runMany (pmcheckGuardsI gvs) vsa
  return $ PartialResult (cs `mappend` css)
                         vsas
                         (ds `mappend` dss)

-- | Worker function: Implements all cases described in the paper for all three
-- functions (`covered`, `uncovered` and `divergent`) apart from the `Guard`
-- cases which are handled by `pmcheck`
pmcheckHd :: PmPat -> PatVec -> [PatVec] -> ValAbs -> ValVec
          -> PmM PartialResult

-- Var
pmcheckHd (PmVar x) ps guards y (ValVec vva delta) =
  case solveOneEq (delta_tm_cs delta) (TVC x (idToPmExpr y)) of
    Nothing       -> return mempty
    Just tm_state -> ucon y <$> pmcheckI ps guards (ValVec vva delta')
      where
        delta' = delta { delta_tm_cs = tm_state }

-- ConVar
pmcheckHd p@PmCon{} ps guards x vva@(ValVec vas delta) = do
  -- Split the value vector into two value vectors: One representing the current
  -- constructor, the other representing everything but the current constructor
  -- (and the already known impossible constructors).
  let con = pm_con_con p
  let args = pm_con_args p

  -- For the value vector of the current constructor, we directly recurse into
  -- checking the the current case, so we get back a PartialResult
  pr_pos <- mkOneSatisfiableConFull delta x con >>= \case
    Nothing -> pure mempty
    Just (delta', _ic, arg_vas) -> do
      tracePm "matched at all" (ppr (delta_tm_cs delta'))
      pr <- pmcheckI (args ++ ps) guards (ValVec (arg_vas ++ vas) delta')
      kcon (PmAltConLike con) (pmPatType p) pr

  -- The var is forced regardless of whether @con@ was satisfiable
  let pr_pos' = forceIfCanDiverge x (delta_tm_cs delta) pr_pos
  pr_neg <- mkUnmatched x (PmAltConLike con) vva
  tracePm "ConVar" (vcat [ppr p, ppr x, ppr pr_pos', ppr pr_neg])

  -- Combine both into a single PartialResult
  pure (mkUnion pr_pos' pr_neg)

-- LitVar
pmcheckHd p@(PmLit l) ps guards x vva@(ValVec vas delta) = do
  pr_pos <- case solveOneEq (delta_tm_cs delta) (TVC x (mkPmExprLit l)) of
    Nothing -> pure mempty
    Just tms -> do
      tracePm "matched at all" (ppr (delta_tm_cs delta))
      let vva'= ValVec vas (delta { delta_tm_cs = tms })
      pmcheckI ps guards vva' >>= kcon (PmAltLit l) (pmPatType p)

  -- The var is forced regardless of whether @l@ was satisfiable.
  -- Although for literals I can't think of a scenario where the var is not
  -- forced yet but the above oracle call leads to a contradiction...
  let pr_pos' = forceIfCanDiverge x (delta_tm_cs delta) pr_pos
  pr_neg <- mkUnmatched x (PmAltLit l) vva
  pure (mkUnion pr_pos' pr_neg)

-- Impossible: handled by pmcheck
pmcheckHd PmFake     _ _ _ _ = panic "pmcheckHd: Fake"
pmcheckHd (PmGrd {}) _ _ _ _ = panic "pmcheckHd: Guard"

{-
Note [Coverage checking and existential tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's implementation of the pattern-match coverage algorithm (as described in
the GADTs Meet Their Match paper) must take some care to emit enough type
constraints when handling data constructors with exisentially quantified type
variables. To better explain what the challenge is, consider a constructor K
of the form:

  K @e_1 ... @e_m ev_1 ... ev_v ty_1 ... ty_n :: T u_1 ... u_p

Where:

* e_1, ..., e_m are the existentially bound type variables.
* ev_1, ..., ev_v are evidence variables, which may inhabit a dictionary type
  (e.g., Eq) or an equality constraint (e.g., e_1 ~ Int).
* ty_1, ..., ty_n are the types of K's fields.
* T u_1 ... u_p is the return type, where T is the data type constructor, and
  u_1, ..., u_p are the universally quantified type variables.

In the ConVar case, the coverage algorithm will have in hand the constructor
K as well as a pattern variable (pv :: T PV_1 ... PV_p), where PV_1, ..., PV_p
are some types that instantiate u_1, ... u_p. The idea is that we should
substitute PV_1 for u_1, ..., and PV_p for u_p when forming a PmCon (the
mkOneConFull function accomplishes this) and then hand this PmCon off to the
ConCon case.

The presence of existentially quantified type variables adds a significant
wrinkle. We always grab e_1, ..., e_m from the definition of K to begin with,
but we don't want them to appear in the final PmCon, because then
calling (mkOneConFull K) for other pattern variables might reuse the same
existential tyvars, which is certainly wrong.

Previously, GHC's solution to this wrinkle was to always create fresh names
for the existential tyvars and put them into the PmCon. This works well for
many cases, but it can break down if you nest GADT pattern matches in just
the right way. For instance, consider the following program:

    data App f a where
      App :: f a -> App f (Maybe a)

    data Ty a where
      TBool :: Ty Bool
      TInt  :: Ty Int

    data T f a where
      C :: T Ty (Maybe Bool)

    foo :: T f a -> App f a -> ()
    foo C (App TBool) = ()

foo is a total program, but with the previous approach to handling existential
tyvars, GHC would mark foo's patterns as non-exhaustive.

When foo is desugared to Core, it looks roughly like so:

    foo @f @a (C co1 _co2) (App @a1 _co3 (TBool |> co1)) = ()

(Where `a1` is an existential tyvar.)

That, in turn, is processed by the coverage checker to become:

    foo @f @a (C co1 _co2) (App @a1 _co3 (pmvar123 :: f a1))
      | TBool <- pmvar123 |> co1
      = ()

Note that the type of pmvar123 is `f a1`—this will be important later.

Now, we proceed with coverage-checking as usual. When we come to the
ConVar case for App, we create a fresh variable `a2` to represent its
existential tyvar. At this point, we have the equality constraints
`(a ~ Maybe a2, a ~ Maybe Bool, f ~ Ty)` in scope.

However, when we check the guard, it will use the type of pmvar123, which is
`f a1`. Thus, when considering if pmvar123 can match the constructor TInt,
it will generate the constraint `a1 ~ Int`. This means our final set of
equality constraints would be:

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int

Which is satisfiable! Freshening the existential tyvar `a` to `a2` doomed us,
because GHC is unable to relate `a2` to `a1`, which really should be the same
tyvar.

Luckily, we can avoid this pitfall. Recall that the ConVar case was where we
generated a PmCon with too-fresh existentials. But after ConVar, we have the
ConCon case, which considers whether each constructor of a particular data type
can be matched on in a particular spot.

In the case of App, when we get to the ConCon case, we will compare our
original App PmCon (from the source program) to the App PmCon created from the
ConVar case. In the former PmCon, we have `a1` in hand, which is exactly the
existential tyvar we want! Thus, we can force `a1` to be the same as `a2` here
by emitting an additional `a1 ~ a2` constraint. Now our final set of equality
constraints will be:

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int
    a1 ~ a2

Which is unsatisfiable, as we desired, since we now have that
Int ~ a1 ~ a2 ~ Bool.

In general, App might have more than one constructor, in which case we
couldn't reuse the existential tyvar for App for a different constructor. This
means that we can only use this trick in ConCon when the constructors are the
same. But this is fine, since this is the only scenario where this situation
arises in the first place!
-}

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

-- | Given a 'PmAltCon' of arity `a` and an uncovered set containing
-- value vector abstractions of length `(a+n)`, pass the first `n` value
-- abstractions to the constructor (Hence, the resulting value vector
-- abstractions will have length `n+1`)
kcon :: PmAltCon -> Type -> PartialResult -> PmM PartialResult
kcon con ty pr = do
  x <- mkPmId ty
  let n = pmAltConArity con
      upd vsa =
        [ ValVec (x:vva) delta{ delta_tm_cs = ts' }
        | ValVec vva' delta <- vsa
        , let (args, vva) = splitAt n vva'
        , let e = PmExprCon con (map idToPmExpr args)
        , let Just ts' = solveOneEq (delta_tm_cs delta) (TVC x e)
        ]
  pure (updateVsa upd pr)

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

-- | Set the divergent set to non-empty if the variable has not been forced yet
forceIfCanDiverge :: Id -> TmState -> PartialResult -> PartialResult
forceIfCanDiverge x tms
  | canDiverge (idName x) tms = forces
  | otherwise                 = id

mkUnmatched :: Id -> PmAltCon -> ValVec -> PmM PartialResult
mkUnmatched x nalt (ValVec vva delta) =
  -- See Note [Refutable shapes] in TmOracle
  tryAddRefutableAltCon (delta_tm_cs delta) (idName x) (idType x) nalt >>= \case
    Nothing -> pure mempty
    Just tms -> do
      -- We need to normalise here to catch uninhabitable vars that would
      -- otherwise
      --  * hide redundant pattern match warnings
      --  * turn redundant pattern match warnings into inaccessible RHS
      ensureInhabited (delta {delta_tm_cs = tms}) x >>= \case
        Unsatisfiable              -> pure mempty
        PossiblySatisfiable delta' -> pure (usimple [ValVec (x:vva) delta'])
#if __GLASGOW_HASKELL__ < 808
        -- GHC before 8.8 will say that this match is needed, while GHC 8.8
        -- will correctly flag it as redundant.
        Satisfiable _ _            -> panic "mkUnmatched"
#endif

-- ----------------------------------------------------------------------------
-- * Propagation of term constraints inwards when checking nested matches

{- Note [Type and Term Equality Propagation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a match it would be great to have all type and term information
available so we can get more precise results. For this reason we have functions
`addDictsDs' and `addTmCsDs' in DsMonad that store in the environment type and
term constraints (respectively) as we go deeper.

The type constraints we propagate inwards are collected by `collectEvVarsPats'
in HsPat.hs. This handles bug #4139 ( see example
  https://gitlab.haskell.org/ghc/ghc/snippets/672 )
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
             -> DsM (Bag TmVarCt)
genCaseTmCs2 Nothing _ _ = return emptyBag
genCaseTmCs2 (Just scr) [p] [var] = do
  fam_insts <- dsGetFamInstEnvs
  [e] <- patVecToPmExprs <$> translatePat fam_insts p
  let scr_e = lhsExprToPmExpr scr
  return $ listToBag [(TVC var e), (TVC var scr_e)]
genCaseTmCs2 _ _ _ = panic "genCaseTmCs2: HsCase"

-- | Generate a simple equality when checking a case expression:
--     case x of { matches }
-- When checking matches we record that (x ~ y) where y is the initial
-- uncovered. All matches will have to satisfy this equality.
genCaseTmCs1 :: Maybe (LHsExpr GhcTc) -> [Id] -> Bag TmVarCt
genCaseTmCs1 Nothing     _    = emptyBag
genCaseTmCs1 (Just scr) [var] = unitBag (TVC var (lhsExprToPmExpr scr))
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
we instead translate it as:     p (x <- pmPatToPmExpr p)
for performance reasons. For example:

  f x@True  = 1
  f y@False = 2

Gives the following with the first translation:

  x |> {x == False, x == y, y == True}

If we use the second translation we get an empty set, independently of the
oracle. Since the pattern `p' may contain guard patterns though, it cannot be
used as an expression. That's why we call `patVecToPmExprs' to drop the guard
and transform it to an expression in the guard pattern. We keep the guards in
the first pattern `p' though.


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

pprValVecSubstituted :: ValVec -> SDoc
pprValVecSubstituted (ValVec vva delta) = pprUncovered (vector, tm_cs)
  where
    tm_cs           = delta_tm_cs delta
    vector          = substInValAbs tm_cs vva

-- | Deeply lookup a value vector abstraction. All VAs are
-- transformed to PmExpr (used only before pretty printing).
substInValAbs :: TmState -> [ValAbs] -> [PmExpr]
substInValAbs ts = map (exprDeepLookup ts . idToPmExpr)

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible && not is_rec_upd
          exists_u = flag_u && (case uncovered of
                                  TypeOfUncovered   _ -> True
                                  UncoveredPatterns u -> notNull u)

      when exists_r $ forM_ redundant $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))
      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        case uncovered of
          TypeOfUncovered ty           -> warnEmptyCase ty
          UncoveredPatterns candidates -> pprEqns candidates
  where
    PmResult
      { pmresultRedundant = redundant
      , pmresultUncovered = uncovered
      , pmresultInaccessible = inaccessible } = pm_result

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind
    flag_u_reason = maybe NoReason Reason (exhaustiveWarningFlag kind)

    is_rec_upd = case kind of { RecUpd -> True; _ -> False }
       -- See Note [Inaccessible warnings for record updates]

    maxPatterns = maxUncoveredPatterns dflags

    -- Print a single clause (for redundant/with-inaccessible-rhs)
    pprEqn q txt = pprContext True ctx (text txt) $ \f ->
      f (pprPats kind (map unLoc q))

    -- Print several clauses (for uncovered clauses)
    pprEqns qs = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      case qs of -- See #11245
           [ValVec [] _]
                    -> text "Guards do not cover entire pattern space"
           _missing -> let us = map pprValVecSubstituted qs
                       in  hang (text "Patterns not matched:") 4
                                (vcat (take maxPatterns us)
                                 $$ dots maxPatterns us)

    -- Print a type-annotated wildcard (for non-exhaustive `EmptyCase`s for
    -- which we only know the type and have no inhabitants at hand)
    warnEmptyCase ty = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      hang (text "Patterns not matched:") 4 (underscore <+> dcolon <+> ppr ty)

{- Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12957)
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
                  , text "to set the maximum number of iterations to n)" ]

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
exhaustiveWarningFlag IfAlt         = Just Opt_WarnIncompletePatterns
exhaustiveWarningFlag LambdaExpr    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag PatBindRhs    = Just Opt_WarnIncompleteUniPatterns
exhaustiveWarningFlag PatBindGuards = Just Opt_WarnIncompletePatterns
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
pprContext :: Bool -> DsMatchContext -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pprContext singular (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [text txt <+> msg,
          sep [ text "In" <+> ppr_match <> char ':'
              , nest 4 (rest_of_msg_fun pref)]]
  where
    txt | singular  = "Pattern match"
        | otherwise = "Pattern match(es)"

    (ppr_match, pref)
        = case kind of
             FunRhs { mc_fun = (dL->L _ fun) }
                  -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _    -> (pprMatchContext kind, \ pp -> pp)

pprPats :: HsMatchContext Name -> [Pat GhcTc] -> SDoc
pprPats kind pats
  = sep [sep (map ppr pats), matchSeparator kind, text "..."]

-- Debugging Infrastructre

tracePm :: String -> SDoc -> PmM ()
tracePm herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace (text herald $$ (nest 2 doc))
