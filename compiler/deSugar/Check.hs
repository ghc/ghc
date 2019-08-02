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
import PmOracle
import PmPpr
import CoreUtils (exprType)
import CoreSyn (Expr(Var))
import Unify( tcMatchTy )
import DynFlags
import HsSyn
import TcHsSyn
import Id
import ConLike
import Name
import FamInst
import TysWiredIn
import TyCon
import SrcLoc
import Util
import Outputable
import FastString (FastString, unpackFS)
import DataCon
import PatSyn
import HscTypes (CompleteMatch(..))
import BasicTypes (Boxity(..), SourceText)

import {-# SOURCE #-} DsExpr (dsExpr)
import DsMonad
import Bag
import TyCoRep
import Type
import DsUtils       (isTrueLHsExpr)
import Maybes        (mapMaybe, isJust, expectJust)
import qualified GHC.LanguageExtensions as LangExt

import Data.List     (find)
import Control.Monad (forM, when, forM_)
import Coercion
import TcEvidence
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

data PmPat where
  -- | For the arguments' meaning see 'HsPat.ConPatOut'.
  -- See also Note [Literals in PmPat].
  PmCon  :: { pm_con_con     :: PmAltCon
            , pm_con_arg_tys :: [Type]
            , pm_con_tvs     :: [TyVar]
            , pm_con_args    :: [PmPat] } -> PmPat
  PmVar  :: { pm_var_id   :: Id } -> PmPat
  PmGrd  :: { pm_grd_pv   :: PatVec -- ^ Always has 'patVecArity' 1.
            , pm_grd_expr :: PmExpr } -> PmPat
  -- | A fake guard pattern (True <- _) used to represent cases we cannot handle.
  PmFake :: PmPat

-- | Should not be user-facing.
instance Outputable PmPat where
  ppr (PmCon alt _arg_tys _con_tvs con_args)
    = cparen (notNull con_args) (hsep [ppr alt, hsep (map ppr con_args)])
  ppr (PmVar vid) = ppr vid
  ppr (PmGrd pv ge) = hsep (map ppr pv) <+> text "<-" <+> ppr ge
  ppr PmFake = text "<PmFake>"

-- data T a where
--     MkT :: forall p q. (Eq p, Ord q) => p -> q -> T [p]
-- or  MkT :: forall p q r. (Eq p, Ord q, [p] ~ r) => p -> q -> T r

-- | Pattern Vectors. The *arity* of a PatVec [p1,..,pn] is
-- the number of p1..pn that are not Guards. See 'patternArity'.
type PatVec = [PmPat]
type ValVec = [Id] -- ^ Value Vector Abstractions

-- | Each 'Delta' is proof (i.e., a model of the fact) that some values are not
-- covered by a pattern match. E.g. @f Nothing = <rhs>@ might be given an
-- uncovered set @[x :-> Just y]@ or @[x /= Nothing]@, where @x@ is the variable
-- matching against @f@'s first argument.
type Uncovered = [Delta]

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

-- | A triple <C,U,D> of covered, uncovered, and divergent sets.
data PartialResult = PartialResult {
                        presultCovered :: Covered
                      , presultUncovered :: Uncovered
                      , presultDivergent :: Diverged }

emptyPartialResult :: PartialResult
emptyPartialResult = PartialResult { presultUncovered = mempty
                                   , presultCovered   = mempty
                                   , presultDivergent = mempty }

combinePartialResults :: PartialResult -> PartialResult -> PartialResult
combinePartialResults (PartialResult cs1 vsa1 ds1) (PartialResult cs2 vsa2 ds2)
  = PartialResult (cs1 Semi.<> cs2)
                  (vsa1 Semi.<> vsa2)
                  (ds1 Semi.<> ds2)

instance Outputable PartialResult where
  ppr (PartialResult c vsa d)
    = hang (text "PartialResult" <+> ppr c <+> ppr d) 2  (ppr_vsa vsa)
    where
      ppr_vsa = braces . fsep . punctuate comma . map ppr

instance Semi.Semigroup PartialResult where
  (<>) = combinePartialResults

instance Monoid PartialResult where
  mempty = emptyPartialResult
  mappend = combinePartialResults

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
data UncoveredCandidates = UncoveredPatterns [([PmExpr], Delta)]
                         | TypeOfUncovered Type

instance Outputable UncoveredCandidates where
  ppr (UncoveredPatterns vva) = text "UnPat" <+> ppr vva
  ppr (TypeOfUncovered ty)   = text "UnTy" <+> ppr ty

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
  missing   <- pmInitialTmTyCs
  tracePm "checkSingle': missing" (ppr missing)
  PartialResult cs us ds <- pmcheckI clause [] [var] missing
  dflags <- getDynFlags
  us' <- getUncoveredPatterns [var] (maxUncoveredPatterns dflags) us
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
    -- See Note [Checking EmptyCase Expressions] in PmOracle
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
      missing    <- pmInitialTmTyCs
      tracePm "checkMatches': missing" (ppr missing)
      (rs,us,ds) <- go matches [missing]
      dflags <- getDynFlags
      us' <- getUncoveredPatterns vars (maxUncoveredPatterns dflags) us
      return $ PmResult {
                   pmresultRedundant    = map hsLMatchToLPats rs
                 , pmresultUncovered    = us'
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
        <- runMany (pmcheckI clause guards vars) missing
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
--   in "PmOracle" for details.
checkEmptyCase' :: Id -> PmM PmResult
checkEmptyCase' x = do
  delta         <- pmInitialTmTyCs
  us <- inhabitants delta (idType x) >>= \case
    -- Inhabitation checking failed / the type is trivially inhabited
    Left ty            -> pure (TypeOfUncovered ty)
    -- A list of oracle states for the different satisfiable constructors is
    -- available. Turn this into a value set abstraction.
    Right (va, deltas) -> pure (UncoveredPatterns (map ([PmExprVar va],) deltas))
  pure (PmResult [] us [])

getUncoveredPatterns :: [Id] -> Int -> [Delta] -> PmM UncoveredCandidates
getUncoveredPatterns _    0 _              = pure (UncoveredPatterns [])
getUncoveredPatterns _    _ []             = pure (UncoveredPatterns [])
getUncoveredPatterns vars n (delta:deltas) = do
  front <- provideEvidenceForEquation vars n delta
  UncoveredPatterns back <- getUncoveredPatterns vars (n - length front) deltas
  pure (UncoveredPatterns (map (, delta) front ++ back))

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
  PmCon { pm_con_con = (PmAltConLike con), pm_con_arg_tys = []
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
  PmCon { pm_con_con = PmAltConLike con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_args = args }
{-# INLINE vanillaConPattern #-}

-- | Create an empty list pattern of a given type
nilPattern :: Type -> PmPat
nilPattern ty =
  PmCon { pm_con_con = PmAltConLike (RealDataCon nilDataCon)
        , pm_con_arg_tys = [ty], pm_con_tvs = [], pm_con_args = [] }
{-# INLINE nilPattern #-}

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = PmAltConLike (RealDataCon consDataCon)
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = []
                               , pm_con_args = xs++ys }]
{-# INLINE mkListPatVec #-}

-- | Create a (non-overloaded) literal pattern
mkSLitPattern :: HsLit GhcTc -> PmPat
mkSLitPattern lit = mkPmLitPattern (PmSLit lit)
{-# INLINE mkSLitPattern #-}

-- | Create a (non-overloaded) literal pattern
mkPmLitPattern :: PmLit -> PmPat
mkPmLitPattern lit = PmCon { pm_con_con = PmAltLit lit
                           , pm_con_arg_tys = []
                           , pm_con_tvs = []
                           , pm_con_args = [] }
{-# INLINE mkPmLitPattern #-}

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
        return [PmCon { pm_con_con     = PmAltConLike con
                      , pm_con_arg_tys = arg_tys
                      , pm_con_tvs     = ex_tvs
                      , pm_con_args    = args }]

  -- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
  NPat ty (dL->L _ olit) mb_neg _
    | Just lit <- hsOverLitAsHsLit olit (isJust mb_neg) ty
    -> translatePat fam_insts (LitPat noExtField lit)
    | otherwise
    -> return [mkPmLitPattern (PmOLit (isJust mb_neg) olit)]

  -- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
  LitPat _ lit
    | HsString src s <- lit ->
        foldr (mkListPatVec charTy) [nilPattern charTy] <$>
          translatePatVec fam_insts
            (map (LitPat noExtField . HsChar src) (unpackFS s))
    | otherwise -> return [mkSLitPattern lit]

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
                            Just x  -> PmGrd pvec (PmExprVar x)
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
translateGuards fam_insts guards =
  concat <$> mapM (translateGuard fam_insts) guards

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

lhsExprToPmExpr :: LHsExpr GhcTc -> PmM PmExpr
lhsExprToPmExpr (dL->L _ e) = hsExprToPmExpr e

hsExprToPmExpr :: HsExpr GhcTc -> PmM PmExpr

-- Translating HsConLikeOut to a flexible meta variable is misleading.
-- For an example why, consider `consAreRigid` in
-- `testsuite/tests/pmcheck/should_compile/PmExprVars.hs`.
-- hsExprToPmExpr (HsConLikeOut _ c) = PmExprVar (conLikeName c)

-- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
hsExprToPmExpr (HsOverLit _ olit)
  | Just lit <- hsOverLitAsHsLit olit False (overLitType olit)
  = hsExprToPmExpr (HsLit noExtField lit)
  | otherwise
  = pure $ PmExprCon (PmAltLit (PmOLit False olit)) []
hsExprToPmExpr (HsLit     _ lit)
  | HsString src s <- lit
  = pure $ stringExprToList src s
  | otherwise = pure $ PmExprCon (PmAltLit (PmSLit lit)) []

hsExprToPmExpr e@(NegApp _ (dL->L _ neg_expr) _) =
  hsExprToPmExpr neg_expr >>= \case
    PmExprCon (PmAltLit (PmOLit False olit)) _ ->
      -- NB: DON'T simply @(NegApp (NegApp olit))@ as @x@. when extension
      -- @RebindableSyntax@ enabled, (-(-x)) may not equals to x.
      pure $ PmExprCon (PmAltLit (PmOLit True olit)) []
    _ -> mkFreshPmExprVarRepresenting e

hsExprToPmExpr (HsPar _ (dL->L _ e)) = hsExprToPmExpr e

hsExprToPmExpr e@(ExplicitTuple _ ps boxity)
  | all tupArgPresent ps = mkPmExprData tuple_con <$> mapMaybeM tuple_arg ps
  | otherwise            = mkFreshPmExprVarRepresenting e
  where
    tuple_con = tupleDataCon boxity (length ps)
    tuple_arg (dL->L _ (Present _ e)) = Just <$> lhsExprToPmExpr e
    tuple_arg _                       = pure Nothing

hsExprToPmExpr e@(ExplicitList _  mb_ol elems)
  | Nothing <- mb_ol = foldr cons nil <$> traverse lhsExprToPmExpr elems
  | otherwise        = mkFreshPmExprVarRepresenting e {- overloaded list: No PmExprApp -}
  where
    cons x xs = mkPmExprData consDataCon [x,xs]
    nil       = mkPmExprData nilDataCon  []

hsExprToPmExpr (RecordCon   _ c binds) = do
  con  <- dsLookupDataCon (idName (unLoc c))
  args <- mapM lhsExprToPmExpr (hsRecFieldsArgs binds)
  return (mkPmExprData con args)

hsExprToPmExpr (HsTick           _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsBinTick      _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsTickPragma _ _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsSCC          _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsCoreAnn      _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySig    _ e _) = lhsExprToPmExpr e
hsExprToPmExpr (HsWrap           _ w e)
  -- A dictionary application spoils e and we have no choice but to return an
  -- PmExprOther. Same thing for other stuff that can't erased in the
  -- compilation process. Otherwise this bites in
  -- teststuite/tests/pmcheck/should_compile/PmExprVars.hs.
  | isErasableHsWrapper w = hsExprToPmExpr e
hsExprToPmExpr e = mkFreshPmExprVarRepresenting e -- the rest are not handled by the oracle

stringExprToList :: SourceText -> FastString -> PmExpr
stringExprToList src s = foldr cons nil (map charToPmExpr (unpackFS s))
  where
    cons x xs      = mkPmExprData consDataCon [x,xs]
    nil            = mkPmExprData nilDataCon  []
    charToPmExpr c = PmExprCon (PmAltLit (PmSLit (HsChar src c))) []

mkFreshPmExprVarRepresenting :: HsExpr GhcTc -> PmM PmExpr
mkFreshPmExprVarRepresenting hs_expr = do
  -- We currently have now mechanism of proving equivalence of two HsExprs, so
  -- generate a fresh representative each time. This is morally wrong, but
  -- is the conservative thing to do.
  -- We have to go through Core because we need the type. Might be useful later
  -- on, since we have infrastructure for deciding alpha equivalence of Core
  -- terms.
  core_expr <- dsExpr hs_expr
  case core_expr of
    Var x
      -- See the HsConLikeOut case above
      | not (isDataConWorkId x) -> pure (PmExprVar x)
    _                           -> PmExprVar <$> mkPmId (exprType core_expr)


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

    pat |> co    ===>    x (pat <- (x |> co))

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
  = pmAltConType con tys
pmPatType (PmVar  { pm_var_id  = x }) = idType x
pmPatType (PmGrd  { pm_grd_pv  = pv })
  = ASSERT(patVecArity pv == 1) (pmPatType p)
  where Just p = find ((==1) . patternArity) pv
pmPatType PmFake = pmPatType truePattern

{-
Note [Extensions to GADTs Meet Their Match]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GADTs Meet Their Match paper presents the formalism that GHC's coverage
checker adheres to. Since the paper's publication, there have been some
additional features added to the coverage checker which are not described in
the paper. This Note serves as a reference for these new features.

* Value abstractions are severely simplified to the point where they are just
  variables. The information about the PmExpr shape of a variable is encoded in
  the oracle state 'Delta' instead.
* Handling of uninhabited fields like `!Void`.
  See Note [Strict argument type constraints] in PmOracle
* Efficient handling of literal splitting, large enumerations and accurate
  redundancy warnings for `COMPLETE` groups through the oracle.
  See Note [Refutable shapes] in PmOracle.
-}

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

-- | Create a guard pattern
mkGuard :: PatVec -> HsExpr GhcTc -> PmM PmPat
mkGuard pv e = do
  res <- allM cantFailPattern pv
  expr <- hsExprToPmExpr e
  tracePm "mkGuard" (vcat [ppr pv, ppr e, ppr res, ppr expr])
  pure (PmGrd pv expr)

-- | Generate a variable pattern of a given type
mkPmVar :: Type -> PmM PmPat
mkPmVar ty = PmVar <$> mkPmId ty

-- | Generate many variable patterns, given a list of types
mkPmVars :: [Type] -> PmM PatVec
mkPmVars tys = mapM mkPmVar tys

-- | Generate a fresh term variable of a given and return it in two forms:
-- * A variable pattern
-- * A variable expression
mkPmId2Forms :: Type -> PmM (PmPat, LHsExpr GhcTc)
mkPmId2Forms ty = do
  x <- mkPmId ty
  return (PmVar x, noLoc (HsVar noExtField (noLoc x)))

-- ----------------------------------------------------------------------------
-- * Converting between Value Abstractions, Patterns and PmExpr

-- | Convert a pattern vector to a list of 'PmExpr's by dropping the guards
-- (See Note [Translating As Patterns])
patVecToPmExprs :: PatVec -> [PmExpr]
patVecToPmExprs = mapMaybe pmPatToPmExpr

-- | Convert a pattern to a 'PmExpr' (will be either 'Nothing' if the pattern is
-- a guard pattern, or 'Just' an expression in all other cases) by dropping the
-- guards (See Note [Translating As Patterns])
pmPatToPmExpr :: PmPat -> Maybe PmExpr
pmPatToPmExpr (PmVar { pm_var_id  = x }) = Just (PmExprVar x)
pmPatToPmExpr (PmCon { pm_con_con = con, pm_con_args = args })
  = Just (PmExprCon con (patVecToPmExprs args))
pmPatToPmExpr _ = Nothing -- drop the guards

-- | Check whether a 'PmAltCon' has the /single match/ property, i.e. whether
-- it is the only possible match in the given context. See also
-- 'allCompleteMatches' and Note [Single match constructors].
singleMatchConstructor :: PmAltCon -> [Type] -> PmM Bool
singleMatchConstructor PmAltLit{}        _   = pure False
singleMatchConstructor (PmAltConLike cl) tys =
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
-- Note that for a data family instance, this must be the *representation* type.
-- e.g. data instance T (a,b) = T1 a b
--   leads to
--      data TPair a b = T1 a b  -- The "representation" type
--   It is TPair a b, not T (a, b), that is given to allCompleteMatches
--
-- These come from two places.
--  1. From data constructors defined with the result type constructor.
--  2. From `COMPLETE` pragmas which have the same type as the result
--     type constructor. Note that we only use `COMPLETE` pragmas
--     *all* of whose pattern types match. See #14135
allCompleteMatches :: Type -> DsM [[ConLike]]
allCompleteMatches ty = case splitTyConApp_maybe ty of
  Nothing -> pure [] -- NB: We don't know any COMPLETE set, as opposed to [[]]
  Just (tc, tc_args) -> do
    -- Look into the representation type of a data family instance, too.
    env <- dsGetFamInstEnvs
    let (tc', _tc_args', _co) = tcLookupDataFamInst env tc tc_args
    let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc'
    let maybe_to_list = maybe [] (:[])
    let rdcs = maybe_to_list mb_rdcs
    -- NB: tc, because COMPLETE sets are associated with the parent data family
    -- TyCon
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

* pmcheck :: PatVec -> [PatVec] -> ValVec -> Delta -> PmM PartialResult

  This function implements functions `covered`, `uncovered` and
  `divergent` from the paper at once. Calls out to the auxilary function
  `pmcheckGuards` for handling (possibly multiple) guarded RHSs when the whole
  clause is checked. Slightly different from the paper because it does not even
  produce the covered and uncovered sets. Since we only care about whether a
  clause covers SOMETHING or if it may forces ANY argument, we only store a
  boolean in both cases, for efficiency.

* pmcheckGuards :: [PatVec] -> ValVec -> Delta -> PmM PartialResult

  Processes the guards.
-}

-- | Lift a pattern matching action from a single value vector abstration to a
-- value set abstraction, but calling it on every vector and combining the
-- results.
runMany :: (Delta -> PmM PartialResult) -> (Uncovered -> PmM PartialResult)
runMany _  []     = return emptyPartialResult
runMany pm (m:ms) = combinePartialResults <$> pm m <*> runMany pm ms

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheck`
pmcheckI :: PatVec -> [PatVec] -> ValVec -> Delta -> PmM PartialResult
pmcheckI ps guards vva delta = do
  n <- incrCheckPmIterDs
  tracePm "pmCheck" (ppr n <> colon
                        $$ hang (text "patterns:") 2 (ppr ps)
                        $$ hang (text "guards:") 2 (ppr guards)
                        $$ ppr vva
                        $$ ppr delta)
  res <- pmcheck ps guards vva delta
  tracePm "pmCheckResult:" (ppr res)
  return res
{-# INLINE pmcheckI #-}

-- | Increase the counter for elapsed algorithm iterations, check that the
-- limit is not exceeded and call `pmcheckGuards`
pmcheckGuardsI :: [PatVec] -> Delta -> PmM PartialResult
pmcheckGuardsI gvs delta = incrCheckPmIterDs >> pmcheckGuards gvs delta
{-# INLINE pmcheckGuardsI #-}

-- | Check the list of mutually exclusive guards
pmcheckGuards :: [PatVec] -> Delta -> PmM PartialResult
pmcheckGuards []       delta = return (usimple delta)
pmcheckGuards (gv:gvs) delta = do
  (PartialResult cs unc ds) <- pmcheckI gv [] [] delta
  (PartialResult css uncs dss) <- runMany (pmcheckGuardsI gvs) unc
  return $ PartialResult (cs `mappend` css)
                         uncs
                         (ds `mappend` dss)

-- | Matching function: Check simultaneously a clause (takes separately the
-- patterns and the list of guards) for exhaustiveness, redundancy and
-- inaccessibility.
pmcheck
  :: PatVec   -- ^ Patterns of the clause
  -> [PatVec] -- ^ (Possibly multiple) guards of the clause
  -> ValVec   -- ^ The value vector abstraction to match against
  -> Delta    -- ^ Oracle state giving meaning to the identifiers in the ValVec
  -> PmM PartialResult
pmcheck [] guards [] delta
  | null guards = return $ mempty { presultCovered = Covered }
  | otherwise   = pmcheckGuardsI guards delta

-- Guard
pmcheck (PmFake : ps) guards vva delta =
  -- short-circuit if the guard pattern is useless.
  -- we just have two possible outcomes: fail here or match and recurse
  -- none of the two contains any useful information about the failure
  -- though. So just have these two cases but do not do all the boilerplate
  forces . mkCons delta <$> pmcheckI ps guards vva delta
pmcheck (p@PmGrd { pm_grd_pv = pv, pm_grd_expr = e } : ps) guards vva delta = do
  tracePm "PmGrd: pmPatType" (vcat [ppr p, ppr (pmPatType p)])
  y <- mkPmId (pmPatType p)
  let delta' = expectJust "y was fresh" $ solveVar delta y e
  pmcheckI (pv ++ ps) guards (y : vva) delta'

-- Var: Add x :-> y to the oracle and recurse
pmcheck (PmVar x : ps) guards (y : vva) delta = pmcheckI ps guards vva delta'
  where
    delta' = expectJust "x is fresh" $ solveVar delta x (PmExprVar y)

-- ConVar
pmcheck (p@PmCon{ pm_con_con = con, pm_con_args = args, pm_con_tvs = ex_tvs } : ps)
        guards (x : vva) delta = do
  -- E.g   f (K p q) = <rhs>
  --       <next equation>
  -- Split the value vector into two value vectors:
  --    * one for <rhs>, binding x to (K p q)
  --    * one for <next equation>, recording that x is /not/ (K _ _)

  -- Stuff for <rhs>
  pr_pos <- refineToAltCon delta x con ex_tvs >>= \case
    Nothing -> pure mempty
    Just (delta', arg_vas) ->
      pmcheckI (args ++ ps) guards (arg_vas ++ vva) delta'

  -- Stuff for <next equation>
  -- The var is forced regardless of whether @con@ was satisfiable
  let pr_pos' = forceIfCanDiverge delta x pr_pos
  pr_neg <- tryAddRefutableAltCon delta x con >>= \case
    Nothing     -> pure mempty
    Just delta' -> pure (usimple delta')

  tracePm "ConVar" (vcat [ppr p, ppr x, ppr pr_pos', ppr pr_neg])

  -- Combine both into a single PartialResult
  pure (mkUnion pr_pos' pr_neg)

pmcheck [] _ (_:_) _ = panic "pmcheck: nil-cons"
pmcheck (_:_) _ [] _ = panic "pmcheck: cons-nil"

-- ----------------------------------------------------------------------------
-- * Utilities for main checking

updateUncovered :: (Uncovered -> Uncovered) -> (PartialResult -> PartialResult)
updateUncovered f p@(PartialResult { presultUncovered = old })
  = p { presultUncovered = f old }


-- | Initialise with default values for covering and divergent information and
-- a singleton uncovered set.
usimple :: Delta -> PartialResult
usimple delta = mempty { presultUncovered = [delta] }

-- | Get the union of two covered, uncovered and divergent value set
-- abstractions. Since the covered and divergent sets are represented by a
-- boolean, union means computing the logical or (at least one of the two is
-- non-empty).

mkUnion :: PartialResult -> PartialResult -> PartialResult
mkUnion = mappend

-- | Add a model to the uncovered set.
mkCons :: Delta -> PartialResult -> PartialResult
mkCons model = updateUncovered (model:)

-- | Set the divergent set to not empty
forces :: PartialResult -> PartialResult
forces pres = pres { presultDivergent = Diverged }

-- | Set the divergent set to non-empty if the variable has not been forced yet
forceIfCanDiverge :: Delta -> Id -> PartialResult -> PartialResult
forceIfCanDiverge delta x
  | canDiverge delta x = forces
  | otherwise          = id

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
  scr_e <- lhsExprToPmExpr scr
  return $ listToBag [(TVC var e), (TVC var scr_e)]
genCaseTmCs2 _ _ _ = panic "genCaseTmCs2: HsCase"

-- | Generate a simple equality when checking a case expression:
--     case x of { matches }
-- When checking matches we record that (x ~ y) where y is the initial
-- uncovered. All matches will have to satisfy this equality.
genCaseTmCs1 :: Maybe (LHsExpr GhcTc) -> [Id] -> DsM (Bag TmVarCt)
genCaseTmCs1 Nothing     _    = return emptyBag
genCaseTmCs1 (Just scr) [var] = do
  scr_e <- lhsExprToPmExpr scr
  return $ unitBag (TVC var scr_e)
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

pprValVecSubstituted :: [PmExpr] -> Delta -> SDoc
pprValVecSubstituted vector delta = pprUncovered (vector, delta)

-- | Deeply lookup a value vector abstraction. All VAs are
-- transformed to PmExpr (used only before pretty printing).
substInValVec :: Delta -> ValVec -> [PmExpr]
substInValVec delta = map (exprDeepLookup delta . PmExprVar)

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible && not is_rec_upd
          exists_u = flag_u && (case uncovered of
                                  TypeOfUncovered   _   -> True
                                  UncoveredPatterns unc -> notNull unc)

      when exists_r $ forM_ redundant $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))
      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        case uncovered of
          TypeOfUncovered ty    -> warnEmptyCase ty
          UncoveredPatterns unc -> pprEqns unc
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
    pprEqns unc = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      case unc of -- See #11245
           ([],_):_ -> text "Guards do not cover entire pattern space"
           _        -> let us = map (uncurry pprValVecSubstituted) unc
                       in  hang (text "Patterns not matched:") 4
                             (vcat (take maxPatterns us) $$ dots maxPatterns us)

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
