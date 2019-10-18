{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Pattern Matching Coverage Checking.
-}

{-# LANGUAGE CPP            #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE LambdaCase     #-}

module GHC.HsToCore.PmCheck (
        -- Checking and printing
        checkSingle, checkMatches, checkGuardMatches,
        needToRunPmCheck, isMatchContextPmChecked,

        -- See Note [Type and Term Equality Propagation]
        addTyCsDs, addScrutTmCs, addPatTmCs
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.HsToCore.PmCheck.Types
import GHC.HsToCore.PmCheck.Oracle
import GHC.HsToCore.PmCheck.Ppr
import BasicTypes (Origin, isGenerated)
import CoreSyn (CoreExpr, Expr(Var,App))
import FastString (unpackFS, lengthFS)
import DynFlags
import GHC.Hs
import TcHsSyn
import Id
import ConLike
import Name
import FamInst
import TysWiredIn
import SrcLoc
import Util
import Outputable
import DataCon
import TyCon
import Var (EvVar)
import Coercion
import TcEvidence
import {-# SOURCE #-} DsExpr (dsExpr, dsLExpr, dsSyntaxExpr)
import {-# SOURCE #-} DsBinds (dsHsWrapper)
import DsUtils (selectMatchVar)
import MatchLit (dsLit, dsOverLit)
import DsMonad
import Bag
import TyCoRep
import Type
import DsUtils       (isTrueLHsExpr)
import Maybes
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad (when, forM_, zipWithM)
import Data.List (elemIndex)
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

-- | A very simple language for pattern guards. Let bindings, bang patterns,
-- and matching variables against flat constructor patterns.
data PmGrd
  = -- | @PmCon x K tvs dicts args@ corresponds to a
    -- @K tvs dicts args <- x@ guard. The @tvs@ and @args@ are bound in this
    -- construct, the @x@ is just a use.
    -- For the arguments' meaning see 'GHC.Hs.Pat.ConPatOut'.
    PmCon {
      pm_id          :: !Id,
      pm_con_con     :: !PmAltCon,
      pm_con_tvs     :: ![TyVar],
      pm_con_dicts   :: ![EvVar],
      pm_con_args    :: ![Id]
    }

    -- | @PmBang x@ corresponds to a @seq x True@ guard.
  | PmBang {
      pm_id          :: !Id
    }

    -- | @PmLet x expr@ corresponds to a @let x = expr@ guard. This actually
    -- /binds/ @x@.
  | PmLet {
      pm_id       :: !Id,
      pm_let_expr :: !CoreExpr
    }

-- | Should not be user-facing.
instance Outputable PmGrd where
  ppr (PmCon x alt _con_tvs _con_dicts con_args)
    = hsep [ppr alt, hsep (map ppr con_args), text "<-", ppr x]
  ppr (PmBang x) = char '!' <> ppr x
  ppr (PmLet x expr) = hsep [text "let", ppr x, text "=", ppr expr]

type GrdVec = [PmGrd]

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

data Precision = Approximate | Precise
  deriving (Eq, Show)

instance Outputable Precision where
  ppr = text . show

instance Semi.Semigroup Precision where
  Approximate <> _ = Approximate
  _ <> Approximate = Approximate
  Precise <> Precise = Precise

instance Monoid Precision where
  mempty = Precise
  mappend = (Semi.<>)

-- | A triple <C,U,D> of covered, uncovered, and divergent sets.
--
-- Also stores a flag 'presultApprox' denoting whether we ran into the
-- 'maxPmCheckModels' limit for the purpose of hints in warning messages to
-- maybe increase the limit.
data PartialResult = PartialResult {
                        presultCovered   :: Covered
                      , presultUncovered :: Uncovered
                      , presultDivergent :: Diverged
                      , presultApprox    :: Precision }

emptyPartialResult :: PartialResult
emptyPartialResult = PartialResult { presultUncovered = mempty
                                   , presultCovered   = mempty
                                   , presultDivergent = mempty
                                   , presultApprox    = mempty }

combinePartialResults :: PartialResult -> PartialResult -> PartialResult
combinePartialResults (PartialResult cs1 vsa1 ds1 ap1) (PartialResult cs2 vsa2 ds2 ap2)
  = PartialResult (cs1 Semi.<> cs2)
                  (vsa1 Semi.<> vsa2)
                  (ds1 Semi.<> ds2)
                  (ap1 Semi.<> ap2) -- the result is approximate if either is

instance Outputable PartialResult where
  ppr (PartialResult c unc d pc)
    = hang (text "PartialResult" <+> ppr c <+> ppr d <+> ppr pc) 2 (ppr_unc unc)
    where
      ppr_unc = braces . fsep . punctuate comma . map ppr

instance Semi.Semigroup PartialResult where
  (<>) = combinePartialResults

instance Monoid PartialResult where
  mempty = emptyPartialResult
  mappend = (Semi.<>)

-- | Pattern check result
--
-- * Redundant clauses
-- * Not-covered clauses (or their type, if no pattern is available)
-- * Clauses with inaccessible RHS
-- * A flag saying whether we ran into the 'maxPmCheckModels' limit for the
--   purpose of suggesting to crank it up in the warning message
--
-- More details about the classification of clauses into useful, redundant
-- and with inaccessible right hand side can be found here:
--
--     https://gitlab.haskell.org/ghc/ghc/wikis/pattern-match-check
--
data PmResult =
  PmResult {
      pmresultRedundant    :: [Located [LPat GhcTc]]
    , pmresultUncovered    :: [Delta]
    , pmresultInaccessible :: [Located [LPat GhcTc]]
    , pmresultApproximate  :: Precision }

instance Outputable PmResult where
  ppr pmr = hang (text "PmResult") 2 $ vcat
    [ text "pmresultRedundant" <+> ppr (pmresultRedundant pmr)
    , text "pmresultUncovered" <+> ppr (pmresultUncovered pmr)
    , text "pmresultInaccessible" <+> ppr (pmresultInaccessible pmr)
    , text "pmresultApproximate" <+> ppr (pmresultApproximate pmr)
    ]

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
  res <- checkSingle' locn var p
  dsPmWarn dflags ctxt [var] res

-- | Check a single pattern binding (let)
checkSingle' :: SrcSpan -> Id -> Pat GhcTc -> DsM PmResult
checkSingle' locn var p = do
  fam_insts <- dsGetFamInstEnvs
  grds      <- translatePat fam_insts var p
  missing   <- getPmDelta
  tracePm "checkSingle': missing" (ppr missing)
  PartialResult cs us ds pc <- pmCheck grds [] 1 missing
  dflags <- getDynFlags
  us' <- getNFirstUncovered [var] (maxUncoveredPatterns dflags + 1) us
  let plain = PmResult { pmresultRedundant    = []
                       , pmresultUncovered    = us'
                       , pmresultInaccessible = []
                       , pmresultApproximate  = pc }
  return $ case (cs,ds) of
    (Covered   , _          ) -> plain                              -- useful
    (NotCovered, NotDiverged) -> plain { pmresultRedundant = m    } -- redundant
    (NotCovered, Diverged   ) -> plain { pmresultInaccessible = m } -- inaccessible rhs
  where m = [cL locn [cL locn p]]

-- | Exhaustive for guard matches, is used for guards in pattern bindings and
-- in @MultiIf@ expressions.
checkGuardMatches :: HsMatchContext Name          -- Match context
                  -> GRHSs GhcTc (LHsExpr GhcTc)  -- Guarded RHSs
                  -> DsM ()
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
             -> [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> DsM ()
checkMatches dflags ctxt vars matches = do
  tracePm "checkMatches" (hang (vcat [ppr ctxt
                               , ppr vars
                               , text "Matches:"])
                               2
                               (vcat (map ppr matches)))
  res <- checkMatches' vars matches
  dsPmWarn dflags ctxt vars res

-- | Check a matchgroup (case, functions, etc.).
checkMatches' :: [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> DsM PmResult
checkMatches' vars matches = do
  init_delta <- getPmDelta
  missing <- case matches of
    -- This must be an -XEmptyCase. See Note [Checking EmptyCase]
    [] | [var] <- vars -> maybeToList <$> addTmCt init_delta (TmVarNonVoid var)
    _                  -> pure [init_delta]
  tracePm "checkMatches': missing" (ppr missing)
  (rs,us,ds,pc) <- go matches missing
  dflags <- getDynFlags
  us' <- getNFirstUncovered vars (maxUncoveredPatterns dflags + 1) us
  return $ PmResult {
                pmresultRedundant    = map hsLMatchToLPats rs
              , pmresultUncovered    = us'
              , pmresultInaccessible = map hsLMatchToLPats ds
              , pmresultApproximate  = pc }
  where
    go :: [LMatch GhcTc (LHsExpr GhcTc)] -> Uncovered
       -> DsM ( [LMatch GhcTc (LHsExpr GhcTc)]
              , Uncovered
              , [LMatch GhcTc (LHsExpr GhcTc)]
              , Precision)
    go []     missing = return ([], missing, [], Precise)
    go (m:ms) missing = do
      tracePm "checkMatches': go" (ppr m)
      dflags             <- getDynFlags
      fam_insts          <- dsGetFamInstEnvs
      (clause, guards)   <- translateMatch fam_insts vars m
      let limit                     = maxPmCheckModels dflags
          n_siblings                = length missing
          throttled_check delta     =
            snd <$> throttle limit (pmCheck clause guards) n_siblings delta

      r@(PartialResult cs missing' ds pc1) <- runMany throttled_check missing

      tracePm "checkMatches': go: res" (ppr r)
      (rs, final_u, is, pc2)  <- go ms missing'
      return $ case (cs, ds) of
        -- useful
        (Covered,  _    )        -> (rs, final_u,    is, pc1 Semi.<> pc2)
        -- redundant
        (NotCovered, NotDiverged) -> (m:rs, final_u, is, pc1 Semi.<> pc2)
        -- inaccessible
        (NotCovered, Diverged )   -> (rs, final_u, m:is, pc1 Semi.<> pc2)

    hsLMatchToLPats :: LMatch id body -> Located [LPat id]
    hsLMatchToLPats (dL->L l (Match { m_pats = pats })) = cL l pats
    hsLMatchToLPats _                                   = panic "checkMatches'"

getNFirstUncovered :: [Id] -> Int -> [Delta] -> DsM [Delta]
getNFirstUncovered _    0 _              = pure []
getNFirstUncovered _    _ []             = pure []
getNFirstUncovered vars n (delta:deltas) = do
  front <- provideEvidence vars n delta
  back <- getNFirstUncovered vars (n - length front) deltas
  pure (front ++ back)

{- Note [Checking EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-XEmptyCase is useful for matching on empty data types like 'Void'. For example,
the following is a complete match:

    f :: Void -> ()
    f x = case x of {}

Really, -XEmptyCase is the only way to write a program that at the same time is
safe (@f _ = error "boom"@ is not because of ⊥), doesn't trigger a warning
(@f !_ = error "inaccessible" has inaccessible RHS) and doesn't turn an
exception into divergence (@f x = f x@).

Semantically, unlike every other case expression, -XEmptyCase is strict in its
match var x, which rules out ⊥ as an inhabitant. So we add x /~ ⊥ to the
initial Delta and check if there are any values left to match on.
-}

{-
%************************************************************************
%*                                                                      *
              Transform source syntax to *our* syntax
%*                                                                      *
%************************************************************************
-}

-- -----------------------------------------------------------------------
-- * Utilities

-- | Smart constructor that eliminates trivial lets
mkPmLetVar :: Id -> Id -> GrdVec
mkPmLetVar x y | x == y = []
mkPmLetVar x y          = [PmLet x (Var y)]

-- | ADT constructor pattern => no existentials, no local constraints
vanillaConGrd :: Id -> DataCon -> [Id] -> PmGrd
vanillaConGrd scrut con arg_ids =
  PmCon { pm_id = scrut, pm_con_con = PmAltConLike (RealDataCon con)
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = arg_ids }

-- | Creates a 'GrdVec' refining a match var of list type to a list,
-- where list fields are matched against the incoming tagged 'GrdVec's.
-- For example:
--   @mkListGrds "a" "[(x, True <- x),(y, !y)]"@
-- to
--   @"[(x:b) <- a, True <- x, (y:c) <- b, seq y True, [] <- c]"@
-- where b,c are freshly allocated in @mkListGrds@ and a is the match variable.
mkListGrds :: Id -> [(Id, GrdVec)] -> DsM GrdVec
-- See Note [Order of guards matter] for why we need to intertwine guards
-- on list elements.
mkListGrds a []                  = pure [vanillaConGrd a nilDataCon []]
mkListGrds a ((x, head_grds):xs) = do
  b <- mkPmId (idType a)
  tail_grds <- mkListGrds b xs
  pure $ vanillaConGrd a consDataCon [x, b] : head_grds ++ tail_grds

-- | Create a 'GrdVec' refining a match variable to a 'PmLit'.
mkPmLitGrds :: Id -> PmLit -> DsM GrdVec
mkPmLitGrds x (PmLit _ (PmLitString s)) = do
  -- We translate String literals to list literals for better overlap reasoning.
  -- It's a little unfortunate we do this here rather than in
  -- 'GHC.HsToCore.PmCheck.Oracle.trySolve' and 'GHC.HsToCore.PmCheck.Oracle.addRefutableAltCon', but it's so much
  -- simpler here.
  -- See Note [Representation of Strings in TmState] in GHC.HsToCore.PmCheck.Oracle
  vars <- traverse mkPmId (take (lengthFS s) (repeat charTy))
  let mk_char_lit y c = mkPmLitGrds y (PmLit charTy (PmLitChar c))
  char_grdss <- zipWithM mk_char_lit vars (unpackFS s)
  mkListGrds x (zip vars char_grdss)
mkPmLitGrds x lit = do
  let grd = PmCon { pm_id = x
                  , pm_con_con = PmAltLit lit
                  , pm_con_tvs = []
                  , pm_con_dicts = []
                  , pm_con_args = [] }
  pure [grd]

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into GrdVec

-- | @translatePat _ x pat@ transforms @pat@ into a 'GrdVec', where
-- the variable representing the match is @x@.
translatePat :: FamInstEnvs -> Id -> Pat GhcTc -> DsM GrdVec
translatePat fam_insts x pat = case pat of
  WildPat  _ty -> pure []
  VarPat _ y   -> pure (mkPmLetVar (unLoc y) x)
  ParPat _ p   -> translateLPat fam_insts x p
  LazyPat _ _  -> pure [] -- like a wildcard
  BangPat _ p  ->
    -- Add the bang in front of the list, because it will happen before any
    -- nested stuff.
    (PmBang x :) <$> translateLPat fam_insts x p

  -- (x@pat)   ==>   Translate pat with x as match var and handle impedance
  --                 mismatch with incoming match var
  AsPat _ (dL->L _ y) p -> (mkPmLetVar y x ++) <$> translateLPat fam_insts y p

  SigPat _ p _ty -> translateLPat fam_insts x p

  -- See Note [Translate CoPats]
  -- Generally the translation is
  -- pat |> co   ===>   let y = x |> co, pat <- y  where y is a match var of pat
  CoPat _ wrapper p _ty
    | isIdHsWrapper wrapper                   -> translatePat fam_insts x p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts x p
    | otherwise -> do
        (y, grds) <- translatePatV fam_insts p
        wrap_rhs_y <- dsHsWrapper wrapper
        pure (PmLet y (wrap_rhs_y (Var x)) : grds)

  -- (n + k)  ===>   let b = x >= k, True <- b, let n = x-k
  NPlusKPat _pat_ty (dL->L _ n) k1 k2 ge minus -> do
    b <- mkPmId boolTy
    let grd_b = vanillaConGrd b trueDataCon []
    [ke1, ke2] <- traverse dsOverLit [unLoc k1, k2]
    rhs_b <- dsSyntaxExpr ge    [Var x, ke1]
    rhs_n <- dsSyntaxExpr minus [Var x, ke2]
    pure [PmLet b rhs_b, grd_b, PmLet n rhs_n]

  -- (fun -> pat)   ===>   let y = fun x, pat <- y where y is a match var of pat
  ViewPat _arg_ty lexpr pat -> do
    (y, grds) <- translateLPatV fam_insts pat
    fun <- dsLExpr lexpr
    pure $ PmLet y (App fun (Var x)) : grds

  -- list
  ListPat (ListPatTc _elem_ty Nothing) ps ->
    translateListPat fam_insts x ps

  -- overloaded list
  ListPat (ListPatTc elem_ty (Just (pat_ty, to_list))) pats -> do
    dflags <- getDynFlags
    case splitListTyConApp_maybe pat_ty of
      Just _e_ty
        | not (xopt LangExt.RebindableSyntax dflags)
        -- Just translate it as a regular ListPat
        -> translateListPat fam_insts x pats
      _ -> do
        y <- mkPmId (mkListTy elem_ty)
        grds <- translateListPat fam_insts y pats
        rhs_y <- dsSyntaxExpr to_list [Var x]
        pure $ PmLet y rhs_y : grds

    -- (a) In the presence of RebindableSyntax, we don't know anything about
    --     `toList`, we should treat `ListPat` as any other view pattern.
    --
    -- (b) In the absence of RebindableSyntax,
    --     - If the pat_ty is `[a]`, then we treat the overloaded list pattern
    --       as ordinary list pattern. Although we can give an instance
    --       `IsList [Int]` (more specific than the default `IsList [a]`), in
    --       practice, we almost never do that. We assume the `to_list` is
    --       the `toList` from `instance IsList [a]`.
    --
    --     - Otherwise, we treat the `ListPat` as ordinary view pattern.
    --
    -- See #14547, especially comment#9 and comment#10.

  ConPatOut { pat_con     = (dL->L _ con)
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_dicts   = dicts
            , pat_args    = ps } -> do
    translateConPatOut fam_insts x con arg_tys ex_tvs dicts ps

  NPat ty (dL->L _ olit) mb_neg _ -> do
    -- See Note [Literal short cut] in MatchLit.hs
    -- We inline the Literal short cut for @ty@ here, because @ty@ is more
    -- precise than the field of OverLitTc, which is all that dsOverLit (which
    -- normally does the literal short cut) can look at. Also @ty@ matches the
    -- type of the scrutinee, so info on both pattern and scrutinee (for which
    -- short cutting in dsOverLit works properly) is overloaded iff either is.
    dflags <- getDynFlags
    core_expr <- case olit of
      OverLit{ ol_val = val, ol_ext = OverLitTc rebindable _ }
        | not rebindable
        , Just expr <- shortCutLit dflags val ty
        -> dsExpr expr
      _ -> dsOverLit olit
    let lit  = expectJust "failed to detect OverLit" (coreExprAsPmLit core_expr)
    let lit' = case mb_neg of
          Just _  -> expectJust "failed to negate lit" (negatePmLit lit)
          Nothing -> lit
    mkPmLitGrds x lit'

  LitPat _ lit -> do
    core_expr <- dsLit (convertLit lit)
    let lit = expectJust "failed to detect Lit" (coreExprAsPmLit core_expr)
    mkPmLitGrds x lit

  TuplePat _tys pats boxity -> do
    (vars, grdss) <- mapAndUnzipM (translateLPatV fam_insts) pats
    let tuple_con = tupleDataCon boxity (length vars)
    pure $ vanillaConGrd x tuple_con vars : concat grdss

  SumPat _ty p alt arity -> do
    (y, grds) <- translateLPatV fam_insts p
    let sum_con = sumDataCon alt arity
    -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    pure $ vanillaConGrd x sum_con [y] : grds

  -- --------------------------------------------------------------------------
  -- Not supposed to happen
  ConPatIn  {} -> panic "Check.translatePat: ConPatIn"
  SplicePat {} -> panic "Check.translatePat: SplicePat"
  XPat      n  -> noExtCon n

-- | 'translatePat', but also select and return a new match var.
translatePatV :: FamInstEnvs -> Pat GhcTc -> DsM (Id, GrdVec)
translatePatV fam_insts pat = do
  x <- selectMatchVar pat
  grds <- translatePat fam_insts x pat
  pure (x, grds)

translateLPat :: FamInstEnvs -> Id -> LPat GhcTc -> DsM GrdVec
translateLPat fam_insts x = translatePat fam_insts x . unLoc

-- | 'translateLPat', but also select and return a new match var.
translateLPatV :: FamInstEnvs -> LPat GhcTc -> DsM (Id, GrdVec)
translateLPatV fam_insts = translatePatV fam_insts . unLoc

-- | @translateListPat _ x [p1, ..., pn]@ is basically
--   @translateConPatOut _ x $(mkListConPatOuts [p1, ..., pn]>@ without ever
-- constructing the 'ConPatOut's.
translateListPat :: FamInstEnvs -> Id -> [LPat GhcTc] -> DsM GrdVec
translateListPat fam_insts x pats = do
  vars_and_grdss <- traverse (translateLPatV fam_insts) pats
  mkListGrds x vars_and_grdss

-- | Translate a constructor pattern
translateConPatOut :: FamInstEnvs -> Id -> ConLike -> [Type] -> [TyVar]
                   -> [EvVar] -> HsConPatDetails GhcTc -> DsM GrdVec
translateConPatOut fam_insts x con univ_tys ex_tvs dicts = \case
    PrefixCon ps                 -> go_field_pats (zip [0..] ps)
    InfixCon  p1 p2              -> go_field_pats (zip [0..] [p1,p2])
    RecCon    (HsRecFields fs _) -> go_field_pats (rec_field_ps fs)
  where
    -- The actual argument types (instantiated)
    arg_tys     = conLikeInstOrigArgTys con (univ_tys ++ mkTyVarTys ex_tvs)

    -- Extract record field patterns tagged by field index from a list of
    -- LHsRecField
    rec_field_ps fs = map (tagged_pat . unLoc) fs
      where
        tagged_pat f = (lbl_to_index (getName (hsRecFieldId f)), hsRecFieldArg f)
        -- Unfortunately the label info is empty when the DataCon wasn't defined
        -- with record field labels, hence we translate to field index.
        orig_lbls        = map flSelector $ conLikeFieldLabels con
        lbl_to_index lbl = expectJust "lbl_to_index" $ elemIndex lbl orig_lbls

    go_field_pats tagged_pats = do
      -- The fields that appear might not be in the correct order. So first
      -- do a PmCon match, then force according to field strictness and then
      -- force evaluation of the field patterns in the order given by
      -- the first field of @tagged_pats@.
      -- See Note [Field match order for RecCon]

      -- Translate the mentioned field patterns. We're doing this first to get
      -- the Ids for pm_con_args.
      let trans_pat (n, pat) = do
            (var, pvec) <- translateLPatV fam_insts pat
            pure ((n, var), pvec)
      (tagged_vars, arg_grdss) <- mapAndUnzipM trans_pat tagged_pats

      let get_pat_id n ty = case lookup n tagged_vars of
            Just var -> pure var
            Nothing  -> mkPmId ty

      -- 1. the constructor pattern match itself
      arg_ids <- zipWithM get_pat_id [0..] arg_tys
      let con_grd = PmCon x (PmAltConLike con) ex_tvs dicts arg_ids

      -- 2. bang strict fields
      let arg_is_banged = map isBanged $ conLikeImplBangs con
          bang_grds     = map PmBang   $ filterByList arg_is_banged arg_ids

      -- 3. guards from field selector patterns
      let arg_grds = concat arg_grdss

      -- tracePm "ConPatOut" (ppr x $$ ppr con $$ ppr arg_ids)
      --
      -- Store the guards in exactly that order
      --      1.         2.           3.
      pure (con_grd : bang_grds ++ arg_grds)

-- Translate a single match
translateMatch :: FamInstEnvs -> [Id] -> LMatch GhcTc (LHsExpr GhcTc)
               -> DsM (GrdVec, [GrdVec])
translateMatch fam_insts vars (dL->L _ (Match { m_pats = pats, m_grhss = grhss }))
  = do
      pats'   <- concat <$> zipWithM (translateLPat fam_insts) vars pats
      guards' <- mapM (translateGuards fam_insts) guards
      -- tracePm "translateMatch" (vcat [ppr pats, ppr pats', ppr guards, ppr guards'])
      return (pats', guards')
      where
        extractGuards :: LGRHS GhcTc (LHsExpr GhcTc) -> [GuardStmt GhcTc]
        extractGuards (dL->L _ (GRHS _ gs _)) = map unLoc gs
        extractGuards _                       = panic "translateMatch"

        guards = map extractGuards (grhssGRHSs grhss)
translateMatch _ _ _ = panic "translateMatch"

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to simpler PmGrds

-- | Translate a list of guard statements to a 'GrdVec'
translateGuards :: FamInstEnvs -> [GuardStmt GhcTc] -> DsM GrdVec
translateGuards fam_insts guards =
  concat <$> mapM (translateGuard fam_insts) guards

-- | Translate a guard statement to a 'GrdVec'
translateGuard :: FamInstEnvs -> GuardStmt GhcTc -> DsM GrdVec
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
translateLet :: HsLocalBinds GhcTc -> DsM GrdVec
translateLet _binds = return []

-- | Translate a pattern guard
--   @pat <- e ==>  let x = e;  <guards for pat <- x>@
translateBind :: FamInstEnvs -> LPat GhcTc -> LHsExpr GhcTc -> DsM GrdVec
translateBind fam_insts p e = dsLExpr e >>= \case
  Var y
    | Nothing <- isDataConId_maybe y
    -- RHS is a variable, so that will allow us to omit the let
    -> translateLPat fam_insts y p
  rhs -> do
    (x, grds) <- translateLPatV fam_insts p
    pure (PmLet x rhs : grds)

-- | Translate a boolean guard
--   @e ==>  let x = e; True <- x@
translateBoolGuard :: LHsExpr GhcTc -> DsM GrdVec
translateBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- GrdVec for efficiency
  | otherwise = dsLExpr e >>= \case
      Var y
        | Nothing <- isDataConId_maybe y
        -- Omit the let by matching on y
        -> pure [vanillaConGrd y trueDataCon []]
      rhs -> do
        x <- mkPmId boolTy
        pure $ [PmLet x rhs, vanillaConGrd x trueDataCon []]

{- Note [Field match order for RecCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The order for RecCon field patterns actually determines evaluation order of
the pattern match. For example:

  data T = T { a :: !Bool, b :: Char, c :: Int }
  f :: T -> ()
  f T{ c = 42, b = 'b' } = ()

Then
  * @f (T (error "a") (error "b") (error "c"))@ errors out with "a" because of
    the strict field.
  * @f (T True        (error "b") (error "c"))@ errors out with "c" because it
    is mentioned frist in the pattern match.

This means we can't just desugar the pattern match to the PatVec
@[T !_ 'b' 42]@. Instead we have to generate variable matches that have
strictness according to the field declarations and afterwards force them in the
right order. As a result, we get the PatVec @[T !_ b c, 42 <- c, 'b' <- b]@.

Of course, when the labels occur in the order they are defined, we can just use
the simpler desugaring.

Note [Order of guards matters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Similar to Note [Field match order for RecCon], the order in which the guards
for a pattern match appear matter. Consider a situation similar to T5117:

  f (0:_)  = ()
  f (0:[]) = ()

The latter clause is clearly redundant. Yet if we translate the second clause as

  [x:xs' <- xs, [] <- xs', 0 <- x]

We will say that the second clause only has an inaccessible RHS. That's because
we force the tail of the list before comparing its head! So the correct
translation would have been

  [x:xs' <- xs, 0 <- x, [] <- xs']

And we have to take in the guards on list cells into @mkListGrds@.

Note [Countering exponential blowup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Precise pattern match exhaustiveness checking is necessarily exponential in
the size of some input programs. We implement a counter-measure in the form of
the -fmax-pmcheck-models flag, limiting the number of Deltas we check against
each pattern by a constant.

How do we do that? Consider

  f True True = ()
  f True True = ()

And imagine we set our limit to 1 for the sake of the example. The first clause
will be checked against the initial Delta, {}. Doing so will produce an
Uncovered set of size 2, containing the models {x/~True} and {x~True,y/~True}.
Also we find the first clause to cover the model {x~True,y~True}.

But the Uncovered set we get out of the match is too huge! We somehow have to
ensure not to make things worse as they are already, so we continue checking
with a singleton Uncovered set of the initial Delta {}. Why is this
sound (wrt. notion of the GADTs Meet their Match paper)? Well, it basically
amounts to forgetting that we matched against the first clause. The values
represented by {} are a superset of those represented by its two refinements
{x/~True} and {x~True,y/~True}.

This forgetfulness becomes very apparent in the example above: By continuing
with {} we don't detect the second clause as redundant, as it again covers the
same non-empty subset of {}. So we don't flag everything as redundant anymore,
but still will never flag something as redundant that isn't.

For exhaustivity, the converse applies: We will report @f@ as non-exhaustive
and report @f _ _@ as missing, which is a superset of the actual missing
matches. But soundness means we will never fail to report a missing match.

This mechanism is implemented in the higher-order function 'throttle'.

Note [Combinatorial explosion in guards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function with many clauses and deeply nested guards like in #11195 tend to
overwhelm the checker because they lead to exponential splitting behavior.
See the comments on #11195 on refinement trees. Every guard refines the
disjunction of Deltas by another split. This no different than the ConVar case,
but in stark contrast we mostly don't get any useful information out of that
split! Hence splitting k-fold just means having k-fold more work. The problem
exacerbates for larger k, because it gets even more unlikely that we can handle
all of the arising Deltas better than just continue working on the original
Delta.

We simply apply the same mechanism as in Note [Countering exponential blowup].
But we don't want to forget about actually useful info from pattern match
clauses just because we had one clause with many guards. So we set the limit for
guards much lower.

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

{-
Note [Extensions to GADTs Meet Their Match]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GADTs Meet Their Match paper presents the formalism that GHC's coverage
checker adheres to. Since the paper's publication, there have been some
additional features added to the coverage checker which are not described in
the paper. This Note serves as a reference for these new features.

* Value abstractions are severely simplified to the point where they are just
  variables. The information about the shape of a variable is encoded in
  the oracle state 'Delta' instead.
* Handling of uninhabited fields like `!Void`.
  See Note [Strict argument type constraints] in GHC.HsToCore.PmCheck.Oracle.
* Efficient handling of literal splitting, large enumerations and accurate
  redundancy warnings for `COMPLETE` groups through the oracle.

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

SG: Another angle at this is that the implied constraints when we instantiate
universal type variables in the return type of a GADT will lead to *provided*
thetas, whereas when we instantiate the return type of a pattern synonym that
corresponds to a *required* theta. See Note [Pattern synonym result type] in
PatSyn. Note how isValidCompleteMatches will successfully filter out

    pattern Just42 :: Maybe Int
    pattern Just42 = Just 42

But fail to filter out the equivalent

    pattern Just'42 :: (a ~ Int) => Maybe a
    pattern Just'42 = Just 42

Which seems fine as far as tcMatchTy is concerned, but it raises a few eye
brows.
-}

{-
%************************************************************************
%*                                                                      *
            Heart of the algorithm: Function pmCheck
%*                                                                      *
%************************************************************************

Main functions are:

* pmCheck :: PatVec -> [PatVec] -> ValVec -> Delta -> DsM PartialResult

  This function implements functions `covered`, `uncovered` and
  `divergent` from the paper at once. Calls out to the auxilary function
  `pmCheckGuards` for handling (possibly multiple) guarded RHSs when the whole
  clause is checked. Slightly different from the paper because it does not even
  produce the covered and uncovered sets. Since we only care about whether a
  clause covers SOMETHING or if it may forces ANY argument, we only store a
  boolean in both cases, for efficiency.

* pmCheckGuards :: [PatVec] -> ValVec -> Delta -> DsM PartialResult

  Processes the guards.
-}

-- | @throttle limit f n delta@ executes the pattern match action @f@ but
-- replaces the 'Uncovered' set by @[delta]@ if not doing so would lead to
-- too many Deltas to check.
--
-- See Note [Countering exponential blowup] and
-- Note [Combinatorial explosion in guards]
--
-- How many is "too many"? @throttle@ assumes that the pattern match action
-- will be executed against @n@ similar other Deltas, its "siblings". Now, by
-- observing the branching factor (i.e. the number of children) of executing
-- the action, we can estimate how many Deltas there would be in the next
-- generation. If we find that this number exceeds @limit@, we do
-- "birth control": We simply don't allow a branching factor of more than 1.
-- Otherwise we just return the singleton set of the original @delta@.
-- This amounts to forgetting about the refined facts we got from running the
-- action.
throttle :: Int -> (Int -> Delta -> DsM PartialResult) -> Int -> Delta -> DsM (Int, PartialResult)
throttle limit f n_siblings delta = do
  res <- f n_siblings delta
  let n_own_children = length (presultUncovered res)
  let n_next_gen = n_siblings * n_own_children
  -- Birth control!
  if n_next_gen <= limit || n_own_children <= 1
    then pure (n_next_gen, res)
    else pure (n_siblings, res { presultUncovered = [delta], presultApprox = Approximate })

-- | Map a pattern matching action processing a single 'Delta' over a
-- 'Uncovered' set and return the combined 'PartialResult's.
runMany :: (Delta -> DsM PartialResult) -> Uncovered -> DsM PartialResult
runMany f unc = mconcat <$> traverse f unc

-- | Print diagnostic info and actually call 'pmCheck''.
pmCheck :: GrdVec -> [GrdVec] -> Int -> Delta -> DsM PartialResult
pmCheck ps guards n delta = do
  tracePm "pmCheck {" $ vcat [ ppr n <> colon
                           , hang (text "patterns:") 2 (ppr ps)
                           , hang (text "guards:") 2 (ppr guards)
                           , ppr delta ]
  res <- pmCheck' ps guards n delta
  tracePm "}:" (ppr res) -- braces are easier to match by tooling
  return res

-- | Lifts 'pmCheck' over a 'DsM (Maybe Delta)'.
pmCheckM :: GrdVec -> [GrdVec] -> Int -> DsM (Maybe Delta) -> DsM PartialResult
pmCheckM ps guards n m_mb_delta = m_mb_delta >>= \case
  Nothing    -> pure mempty
  Just delta -> pmCheck ps guards n delta

-- | Check the list of mutually exclusive guards
pmCheckGuards :: [GrdVec] -> Int -> Delta -> DsM PartialResult
pmCheckGuards []       _ delta = return (usimple delta)
pmCheckGuards (gv:gvs) n delta = do
  dflags <- getDynFlags
  let limit = maxPmCheckModels dflags `div` 5
  (n', PartialResult cs unc ds pc) <- throttle limit (pmCheck gv []) n delta
  (PartialResult css uncs dss pcs) <- runMany (pmCheckGuards gvs n') unc
  return $ PartialResult (cs `mappend` css)
                         uncs
                         (ds `mappend` dss)
                         (pc `mappend` pcs)

-- | Matching function: Check simultaneously a clause (takes separately the
-- patterns and the list of guards) for exhaustiveness, redundancy and
-- inaccessibility.
pmCheck'
  :: GrdVec   -- ^ Patterns of the clause
  -> [GrdVec] -- ^ (Possibly multiple) guards of the clause
  -> Int      -- ^ Estimate on the number of similar 'Delta's to handle.
              --   See 6. in Note [Countering exponential blowup]
  -> Delta    -- ^ Oracle state giving meaning to the identifiers in the ValVec
  -> DsM PartialResult
pmCheck' [] guards n delta
  | null guards = return $ mempty { presultCovered = Covered }
  | otherwise   = pmCheckGuards guards n delta

-- let x = e: Add x ~ e to the oracle
pmCheck' (PmLet { pm_id = x, pm_let_expr = e } : ps) guards n delta = do
  tracePm "PmLet" (vcat [ppr x, ppr e])
  -- x is fresh because it's bound by the let
  delta' <- expectJust "x is fresh" <$> addVarCoreCt delta x e
  pmCheck ps guards n delta'

-- Bang x: Add x /~ _|_ to the oracle
pmCheck' (PmBang x : ps) guards n delta = do
  tracePm "PmBang" (ppr x)
  pr <- pmCheckM ps guards n (addTmCt delta (TmVarNonVoid x))
  pure (forceIfCanDiverge delta x pr)

-- Con: Add x ~ K ys to the Covered set and x /~ K to the Uncovered set
pmCheck' (p : ps) guards n delta
  | PmCon{ pm_id = x, pm_con_con = con, pm_con_args = args
         , pm_con_dicts = dicts } <- p = do
  -- E.g   f (K p q) = <rhs>
  --       <next equation>
  -- Split delta into two refinements:
  --    * one for <rhs>, binding x to (K p q)
  --    * one for <next equation>, recording that x is /not/ (K _ _)

  -- Stuff for <rhs>
  pr_pos <- pmCheckM ps guards n (addPmConCts delta x con dicts args)

  -- The var is forced regardless of whether @con@ was satisfiable
  -- See Note [Divergence of Newtype matches]
  let pr_pos' = addConMatchStrictness delta x con pr_pos

  -- Stuff for <next equation>
  pr_neg <- addRefutableAltCon delta x con >>= \case
    Nothing     -> pure mempty
    Just delta' -> pure (usimple delta')

  tracePm "PmCon" (vcat [ppr p, ppr x, ppr pr_pos', ppr pr_neg])

  -- Combine both into a single PartialResult
  let pr = mkUnion pr_pos' pr_neg
  pure pr

addPmConCts :: Delta -> Id -> PmAltCon -> [EvVar] -> [Id] -> DsM (Maybe Delta)
addPmConCts delta x con dicts fields = runMaybeT $ do
  delta_ty    <- MaybeT $ addTypeEvidence delta (listToBag dicts)
  delta_tm_ty <- MaybeT $ addTmCt delta_ty (TmVarCon x con fields)
  pure delta_tm_ty

-- ----------------------------------------------------------------------------
-- * Utilities for main checking

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

-- | Set the divergent set to not empty
forces :: PartialResult -> PartialResult
forces pres = pres { presultDivergent = Diverged }

-- | Set the divergent set to non-empty if the variable has not been forced yet
forceIfCanDiverge :: Delta -> Id -> PartialResult -> PartialResult
forceIfCanDiverge delta x
  | canDiverge delta x = forces
  | otherwise          = id

-- | 'forceIfCanDiverge' if the 'PmAltCon' was not a Newtype.
-- See Note [Divergence of Newtype matches].
addConMatchStrictness :: Delta -> Id -> PmAltCon -> PartialResult -> PartialResult
addConMatchStrictness _     _ (PmAltConLike (RealDataCon dc)) res
  | isNewTyCon (dataConTyCon dc) = res
addConMatchStrictness delta x _ res = forceIfCanDiverge delta x res

-- ----------------------------------------------------------------------------
-- * Propagation of term constraints inwards when checking nested matches

{- Note [Type and Term Equality Propagation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a match it would be great to have all type and term information
available so we can get more precise results. For this reason we have functions
`addDictsDs' and `addTmVarCsDs' in DsMonad that store in the environment type and
term constraints (respectively) as we go deeper.

The type constraints we propagate inwards are collected by `collectEvVarsPats'
in GHC.Hs.Pat. This handles bug #4139 ( see example
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

Functions `addScrutTmCs' and `addPatTmCs' are responsible for generating
these constraints.
-}

locallyExtendPmDelta :: (Delta -> DsM (Maybe Delta)) -> DsM a -> DsM a
locallyExtendPmDelta ext k = getPmDelta >>= ext >>= \case
  -- If adding a constraint would lead to a contradiction, don't add it.
  -- See @Note [Recovering from unsatisfiable pattern-matching constraints]@
  -- for why this is done.
  Nothing     -> k
  Just delta' -> updPmDelta delta' k

-- | Add in-scope type constraints
addTyCsDs :: Bag EvVar -> DsM a -> DsM a
addTyCsDs ev_vars =
  locallyExtendPmDelta (\delta -> addTypeEvidence delta ev_vars)

-- | Add equalities for the scrutinee to the local 'DsM' environment when
-- checking a case expression:
--     case e of x { matches }
-- When checking matches we record that (x ~ e) where x is the initial
-- uncovered. All matches will have to satisfy this equality.
addScrutTmCs :: Maybe (LHsExpr GhcTc) -> [Id] -> DsM a -> DsM a
addScrutTmCs Nothing    _   k = k
addScrutTmCs (Just scr) [x] k = do
  scr_e <- dsLExpr scr
  locallyExtendPmDelta (\delta -> addVarCoreCt delta x scr_e) k
addScrutTmCs _   _   _ = panic "addScrutTmCs: HsCase with more than one case binder"

-- | Add equalities to the local 'DsM' environment when checking the RHS of a
-- case expression:
--     case e of x { p1 -> e1; ... pn -> en }
-- When we go deeper to check e.g. e1 we record (x ~ p1).
addPatTmCs :: [Pat GhcTc]           -- LHS       (should have length 1)
           -> [Id]                  -- MatchVars (should have length 1)
           -> DsM a
           -> DsM a
-- Computes an approximation of the Covered set for p1 (which pmCheck currently
-- discards).
addPatTmCs ps xs k = do
  fam_insts <- dsGetFamInstEnvs
  grds <- concat <$> zipWithM (translatePat fam_insts) xs ps
  locallyExtendPmDelta (\delta -> computeCovered grds delta) k

-- | A dead simple version of 'pmCheck' that only computes the Covered set.
-- So it only cares about collecting positive info.
-- We use it to collect info from a pattern when we check its RHS.
-- See 'addPatTmCs'.
computeCovered :: GrdVec -> Delta -> DsM (Maybe Delta)
-- The duplication with 'pmCheck' is really unfortunate, but it's simpler than
-- separating out the common cases with 'pmCheck', because that would make the
-- ConVar case harder to understand.
computeCovered [] delta = pure (Just delta)
computeCovered (PmLet { pm_id = x, pm_let_expr = e } : ps) delta = do
  delta' <- expectJust "x is fresh" <$> addVarCoreCt delta x e
  computeCovered ps delta'
computeCovered (PmBang{} : ps) delta = do
  computeCovered ps delta
computeCovered (p : ps) delta
  | PmCon{ pm_id = x, pm_con_con = con, pm_con_args = args
         , pm_con_dicts = dicts } <- p
  = addPmConCts delta x con dicts args >>= \case
      Nothing     -> pure Nothing
      Just delta' -> computeCovered ps delta'

{-
%************************************************************************
%*                                                                      *
      Pretty printing of exhaustiveness/redundancy check warnings
%*                                                                      *
%************************************************************************
-}

-- | Check whether any part of pattern match checking is enabled for this
-- 'HsMatchContext' (does not matter whether it is the redundancy check or the
-- exhaustiveness check).
isMatchContextPmChecked :: DynFlags -> Origin -> HsMatchContext id -> Bool
isMatchContextPmChecked dflags origin kind
  | isGenerated origin
  = False
  | otherwise
  = wopt Opt_WarnOverlappingPatterns dflags || exhaustive dflags kind

-- | Return True when any of the pattern match warnings ('allPmCheckWarnings')
-- are enabled, in which case we need to run the pattern match checker.
needToRunPmCheck :: DynFlags -> Origin -> Bool
needToRunPmCheck dflags origin
  | isGenerated origin
  = False
  | otherwise
  = notNull (filter (`wopt` dflags) allPmCheckWarnings)

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> [Id] -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) vars pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible && not is_rec_upd
          exists_u = flag_u && notNull uncovered
          approx   = precision == Approximate

      when (approx && (exists_u || exists_i)) $
        putSrcSpanDs loc (warnDs NoReason approx_msg)

      when exists_r $ forM_ redundant $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(dL->L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))
      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        pprEqns vars uncovered
  where
    PmResult
      { pmresultRedundant = redundant
      , pmresultUncovered = uncovered
      , pmresultInaccessible = inaccessible
      , pmresultApproximate = precision } = pm_result

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
    pprEqns vars deltas = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      case vars of -- See #11245
           [] -> text "Guards do not cover entire pattern space"
           _  -> let us = map (\delta -> pprUncovered delta vars) deltas
                 in  hang (text "Patterns not matched:") 4
                       (vcat (take maxPatterns us) $$ dots maxPatterns us)

    approx_msg = vcat
      [ hang
          (text "Pattern match checker ran into -fmax-pmcheck-models="
            <> int (maxPmCheckModels dflags)
            <> text " limit, so")
          2
          (  bullet <+> text "Redundant clauses might not be reported at all"
          $$ bullet <+> text "Redundant clauses might be reported as inaccessible"
          $$ bullet <+> text "Patterns reported as unmatched might actually be matched")
      , text "Increase the limit or resolve the warnings to suppress this message." ]

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

dots :: Int -> [a] -> SDoc
dots maxPatterns qs
    | qs `lengthExceeds` maxPatterns = text "..."
    | otherwise                      = empty

-- | All warning flags that need to run the pattern match checker.
allPmCheckWarnings :: [WarningFlag]
allPmCheckWarnings =
  [ Opt_WarnIncompletePatterns
  , Opt_WarnIncompleteUniPatterns
  , Opt_WarnIncompletePatternsRecUpd
  , Opt_WarnOverlappingPatterns
  ]

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
