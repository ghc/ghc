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
import CoreSyn (CoreExpr, Expr(Var))
import CoreUtils (exprType)
import FastString (unpackFS)
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
import PatSyn (patSynArity)
import BasicTypes (Boxity(..))
import Var (EvVar)
import Coercion
import TcEvidence
import {-# SOURCE #-} DsExpr (dsExpr, dsLExpr, dsSyntaxExpr)
import MatchLit (dsLit, dsOverLit)
import DsMonad
import Bag
import TyCoRep
import Type
import DsUtils       (isTrueLHsExpr)
import Maybes
import qualified GHC.LanguageExtensions as LangExt

import Data.List     (find, isSubsequenceOf)
import Control.Monad (forM, when, forM_, zipWithM)
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

data PmPat
  = -- | For the arguments' meaning see 'HsPat.ConPatOut'.
    PmCon {
      pm_con_con     :: PmAltCon,
      pm_con_arg_tys :: [Type],
      pm_con_tvs     :: [TyVar],
      pm_con_dicts   :: [EvVar],
      pm_con_args    :: [PmPat]
    }

    -- | Possibly strict variable pattern match
  | PmVar {
      _pm_var_bang :: HsImplBang,
      pm_var_id    :: Id
    }

    -- | @PmGrd pat expr@ matches @expr@ against @pat@,
    --   binding the variables in @pat@
  | PmGrd {
      pm_grd_pv   :: PatVec,
      -- ^ Always has 'patVecArity' 1.
      pm_grd_expr :: CoreExpr
    }

-- | Should not be user-facing.
instance Outputable PmPat where
  ppr (PmCon alt _arg_tys _con_tvs _con_dicts con_args)
    = cparen (notNull con_args) (hsep [ppr alt, hsep (map ppr con_args)])
  ppr (PmVar bang vid) = (if isBanged bang then char '!' else empty) <> ppr vid
  ppr (PmGrd pv ge) = hsep (map ppr pv) <+> text "<-" <+> ppr ge

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
    , pmresultUncovered    :: UncoveredCandidates
    , pmresultInaccessible :: [Located [LPat GhcTc]]
    , pmresultApproximate  :: Precision }

instance Outputable PmResult where
  ppr pmr = hang (text "PmResult") 2 $ vcat
    [ text "pmresultRedundant" <+> ppr (pmresultRedundant pmr)
    , text "pmresultUncovered" <+> ppr (pmresultUncovered pmr)
    , text "pmresultInaccessible" <+> ppr (pmresultInaccessible pmr)
    , text "pmresultApproximate" <+> ppr (pmresultApproximate pmr)
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
data UncoveredCandidates = UncoveredPatterns [Id] [Delta]
                         | TypeOfUncovered Type

instance Outputable UncoveredCandidates where
  ppr (UncoveredPatterns vva deltas) = text "UnPat" <+> ppr vva $$ ppr deltas
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
  res <- checkSingle' locn var p
  dsPmWarn dflags ctxt res

-- | Check a single pattern binding (let)
checkSingle' :: SrcSpan -> Id -> Pat GhcTc -> DsM PmResult
checkSingle' locn var p = do
  fam_insts <- dsGetFamInstEnvs
  clause    <- translatePat fam_insts p
  missing   <- getPmDelta
  tracePm "checkSingle': missing" (ppr missing)
  PartialResult cs us ds pc <- pmcheckI clause [] [var] 1 missing
  dflags <- getDynFlags
  us' <- getNFirstUncovered [var] (maxUncoveredPatterns dflags + 1) us
  let uc = UncoveredPatterns [var] us'
  return $ case (cs,ds) of
    (Covered,  _    )         -> PmResult [] uc [] pc -- useful
    (NotCovered, NotDiverged) -> PmResult m  uc [] pc -- redundant
    (NotCovered, Diverged )   -> PmResult [] uc m  pc -- inaccessible rhs
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
  res <- case matches of
    -- Check EmptyCase separately
    -- See Note [Checking EmptyCase Expressions] in GHC.HsToCore.PmCheck.Oracle
    [] | [var] <- vars -> checkEmptyCase' var
    _normal_match      -> checkMatches' vars matches
  dsPmWarn dflags ctxt res

-- | Check a matchgroup (case, functions, etc.). To be called on a non-empty
-- list of matches. For empty case expressions, use checkEmptyCase' instead.
checkMatches' :: [Id] -> [LMatch GhcTc (LHsExpr GhcTc)] -> DsM PmResult
checkMatches' vars matches
  | null matches = panic "checkMatches': EmptyCase"
  | otherwise = do
      missing    <- getPmDelta
      tracePm "checkMatches': missing" (ppr missing)
      (rs,us,ds,pc) <- go matches [missing]
      dflags <- getDynFlags
      us' <- getNFirstUncovered vars (maxUncoveredPatterns dflags + 1) us
      let up = UncoveredPatterns vars us'
      return $ PmResult {
                   pmresultRedundant    = map hsLMatchToLPats rs
                 , pmresultUncovered    = up
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
      (clause, guards)   <- translateMatch fam_insts m
      let limit           = maxPmCheckModels dflags
          n_siblings      = length missing
          throttled_check delta =
            snd <$> throttle limit (pmcheckI clause guards vars) n_siblings delta

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

-- | Check an empty case expression. Since there are no clauses to process, we
--   only compute the uncovered set. See Note [Checking EmptyCase Expressions]
--   in "GHC.HsToCore.PmCheck.Oracle" for details.
checkEmptyCase' :: Id -> DsM PmResult
checkEmptyCase' x = do
  delta         <- getPmDelta
  us <- inhabitants delta (idType x) >>= \case
    -- Inhabitation checking failed / the type is trivially inhabited
    Left ty            -> pure (TypeOfUncovered ty)
    -- A list of oracle states for the different satisfiable constructors is
    -- available. Turn this into a value set abstraction.
    Right (va, deltas) -> pure (UncoveredPatterns [va] deltas)
  pure (PmResult [] us [] Precise)

getNFirstUncovered :: [Id] -> Int -> [Delta] -> DsM [Delta]
getNFirstUncovered _    0 _              = pure []
getNFirstUncovered _    _ []             = pure []
getNFirstUncovered vars n (delta:deltas) = do
  front <- provideEvidenceForEquation vars n delta
  back <- getNFirstUncovered vars (n - length front) deltas
  pure (front ++ back)

{-
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
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = [] }
{-# INLINE nullaryConPattern #-}

truePattern :: PmPat
truePattern = nullaryConPattern (RealDataCon trueDataCon)
{-# INLINE truePattern #-}

vanillaConPattern :: ConLike -> [Type] -> PatVec -> PmPat
-- ADT constructor pattern => no existentials, no local constraints
vanillaConPattern con arg_tys args =
  PmCon { pm_con_con = PmAltConLike con, pm_con_arg_tys = arg_tys
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = args }
{-# INLINE vanillaConPattern #-}

-- | Create an empty list pattern of a given type
nilPattern :: Type -> PmPat
nilPattern ty =
  PmCon { pm_con_con = PmAltConLike (RealDataCon nilDataCon)
        , pm_con_arg_tys = [ty], pm_con_tvs = [], pm_con_dicts = []
        , pm_con_args = [] }
{-# INLINE nilPattern #-}

mkListPatVec :: Type -> PatVec -> PatVec -> PatVec
mkListPatVec ty xs ys = [PmCon { pm_con_con = PmAltConLike (RealDataCon consDataCon)
                               , pm_con_arg_tys = [ty]
                               , pm_con_tvs = []
                               , pm_con_dicts = []
                               , pm_con_args = xs++ys }]
{-# INLINE mkListPatVec #-}

-- | Create a literal pattern
mkPmLitPattern :: PmLit -> PatVec
mkPmLitPattern lit@(PmLit _ val)
  -- We translate String literals to list literals for better overlap reasoning.
  -- It's a little unfortunate we do this here rather than in
  -- 'GHC.HsToCore.PmCheck.Oracle.trySolve' and 'GHC.HsToCore.PmCheck.Oracle.addRefutableAltCon', but it's so much
  -- simpler here.
  -- See Note [Representation of Strings in TmState] in GHC.HsToCore.PmCheck.Oracle
  | PmLitString s <- val
  , let mk_char_lit c = mkPmLitPattern (PmLit charTy (PmLitChar c))
  = foldr (\c p -> mkListPatVec charTy (mk_char_lit c) p)
          [nilPattern charTy]
          (unpackFS s)
  | otherwise
  = [PmCon { pm_con_con = PmAltLit lit
           , pm_con_arg_tys = []
           , pm_con_tvs = []
           , pm_con_dicts = []
           , pm_con_args = [] }]
{-# INLINE mkPmLitPattern #-}

-- -----------------------------------------------------------------------
-- * Transform (Pat Id) into [PmPat]
-- The arity of the [PmPat] is always 1, but it may be a combination
-- of a vanilla pattern and a guard pattern.
-- Example: view pattern  (f y -> Just x)
--          becomes       [PmVar z, PmGrd [PmPat (Just x), f y]]
--          where z is fresh

translatePat :: FamInstEnvs -> Pat GhcTc -> DsM PatVec
translatePat fam_insts pat = case pat of
  WildPat  ty  -> mkPmVars [ty]
  VarPat _ id  -> return [PmVar HsLazy (unLoc id)]
  ParPat _ p   -> translatePat fam_insts (unLoc p)
  LazyPat _ _  -> mkPmVars [hsPatType pat] -- like a variable
  BangPat _ p  -> addBangs [HsStrict] <$> translatePat fam_insts (unLoc p)

  -- (x@pat)   ===>   x (pat <- x)
  AsPat _ (dL->L _ x) p -> do
    pat <- translatePat fam_insts (unLoc p)
    pure [PmVar HsLazy x, PmGrd pat (Var x)]

  SigPat _ p _ty -> translatePat fam_insts (unLoc p)

  -- See Note [Translate CoPats]
  CoPat _ wrapper p ty
    | isIdHsWrapper wrapper                   -> translatePat fam_insts p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts p
    | otherwise -> do
        ps <- translatePat fam_insts p
        (xp,xe) <- mkPmId2Forms ty
        g <- mkGuard ps (mkHsWrap wrapper (unLoc xe))
        pure [xp,g]

  -- (n + k)  ===>   x (True <- x >= k) (n <- x-k)
  NPlusKPat pat_ty (dL->L _ n) k1 k2 ge minus -> do
    (xp, xe) <- mkPmId2Forms pat_ty
    let ke1 = HsOverLit noExtField (unLoc k1)
        ke2 = HsOverLit noExtField k2
    g1 <- mkGuardSyntaxExpr [truePattern]    ge    [unLoc xe, ke1]
    g2 <- mkGuardSyntaxExpr [PmVar HsLazy n] minus [ke2]
    return [xp, g1, g2]

  -- (fun -> pat)   ===>   x (pat <- fun x)
  ViewPat arg_ty lexpr lpat -> do
    ps <- translatePat fam_insts (unLoc lpat)
    (xp,xe) <- mkPmId2Forms arg_ty
    g <- mkGuard ps (HsApp noExtField lexpr xe)
    return [xp, g]

  -- list
  ListPat (ListPatTc ty Nothing) ps -> do
    pv <- translatePatVec fam_insts (map unLoc ps)
    return (foldr (mkListPatVec ty) [nilPattern ty] pv)

  -- overloaded list
  ListPat (ListPatTc elem_ty (Just (pat_ty, to_list))) lpats -> do
    dflags <- getDynFlags
    case splitListTyConApp_maybe pat_ty of
      Just e_ty
        | not (xopt LangExt.RebindableSyntax dflags)
        -- Just translate it as a regular ListPat
        -> translatePat fam_insts (ListPat (ListPatTc e_ty Nothing) lpats)
      _ -> do
        ps       <- translatePatVec fam_insts (map unLoc lpats)
        (xp, xe) <- mkPmId2Forms pat_ty
        let pats = foldr (mkListPatVec elem_ty) [nilPattern elem_ty] ps
        g <- mkGuardSyntaxExpr pats to_list [unLoc xe]
        return [xp,g]

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
    -- see Note [Countering exponential blowup].

  ConPatOut { pat_con     = (dL->L _ con)
            , pat_arg_tys = arg_tys
            , pat_tvs     = ex_tvs
            , pat_dicts   = dicts
            , pat_args    = ps } -> do
    args <- translateConPatVec fam_insts arg_tys ex_tvs con ps
    return [PmCon { pm_con_con     = PmAltConLike con
                  , pm_con_arg_tys = arg_tys
                  , pm_con_tvs     = ex_tvs
                  , pm_con_dicts   = dicts
                  , pm_con_args    = args }]

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
    return (mkPmLitPattern lit')

  LitPat _ lit -> do
    core_expr <- dsLit (convertLit lit)
    let lit = expectJust "failed to detect Lit" (coreExprAsPmLit core_expr)
    return (mkPmLitPattern lit)

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
translatePatVec :: FamInstEnvs -> [Pat GhcTc] -> DsM [PatVec]
translatePatVec fam_insts pats = mapM (translatePat fam_insts) pats

-- | Translate a constructor pattern
translateConPatVec :: FamInstEnvs -> [Type] -> [TyVar]
                   -> ConLike -> HsConPatDetails GhcTc
                   -> DsM PatVec
translateConPatVec fam_insts _univ_tys _ex_tvs c (PrefixCon ps)
  = addFieldBangs c . concat <$> translatePatVec fam_insts (map unLoc ps)
translateConPatVec fam_insts _univ_tys _ex_tvs c (InfixCon p1 p2)
  = addFieldBangs c . concat <$> translatePatVec fam_insts (map unLoc [p1,p2])
translateConPatVec fam_insts  univ_tys  ex_tvs c (RecCon (HsRecFields fs _))
    -- Nothing matched. Make up some fresh term variables
  | null fs        = addFieldBangs c <$> mkPmVars arg_tys
    -- The data constructor was not defined using record syntax. For the
    -- pattern to be in record syntax it should be empty (e.g. Just {}).
    -- So just like the previous case.
  | null orig_lbls = ASSERT(null matched_lbls) addFieldBangs c <$> mkPmVars arg_tys
    -- Some of the fields appear, in the original order (there may be holes).
    -- Generate a simple constructor pattern and make up fresh variables for
    -- the rest of the fields
  | matched_lbls `isSubsequenceOf` orig_lbls
  = ASSERT(orig_lbls `equalLength` arg_tys)
      let translateOne lbl ty = case lookup lbl matched_pats of
            Just p  -> translatePat fam_insts p
            Nothing -> mkPmVars [ty]
      in  addFieldBangs c . concat <$> zipWithM translateOne orig_lbls arg_tys
    -- The fields that appear are not in the correct order. Make up fresh
    -- variables for all fields and add guards after matching, to force the
    -- evaluation in the correct order.
    -- See Note [Field match order for RecCon]
  | otherwise = do
      arg_var_pats    <- addFieldBangs c <$> mkPmVars arg_tys
      translated_pats <- forM matched_pats $ \(x,pat) -> do
        pvec <- translatePat fam_insts pat
        return (x, pvec)

      let zipped = zip orig_lbls [ x | PmVar _ x <- arg_var_pats ]
          guards = map (\(name,pvec) -> case lookup name zipped of
                            Just x  -> PmGrd pvec (Var x)
                            Nothing -> panic "translateConPatVec: lookup")
                       translated_pats

      return (arg_var_pats ++ guards)
  where
    -- The actual argument types (instantiated), with strictness marks
    arg_tys     = conLikeInstOrigArgTys c (univ_tys ++ mkTyVarTys ex_tvs)

    -- Some label information
    orig_lbls    = map flSelector $ conLikeFieldLabels c
    matched_pats = [ (getName (unLoc (hsRecFieldId x)), unLoc (hsRecFieldArg x))
                   | (dL->L _ x) <- fs]
    matched_lbls = [ name | (name, _pat) <- matched_pats ]

-- Translate a single match
translateMatch :: FamInstEnvs -> LMatch GhcTc (LHsExpr GhcTc)
               -> DsM (PatVec, [PatVec])
translateMatch fam_insts (dL->L _ (Match { m_pats = lpats, m_grhss = grhss }))
  = do
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
translateGuards :: FamInstEnvs -> [GuardStmt GhcTc] -> DsM PatVec
translateGuards fam_insts guards =
  concat <$> mapM (translateGuard fam_insts) guards

-- | Translate a guard statement to Pattern
translateGuard :: FamInstEnvs -> GuardStmt GhcTc -> DsM PatVec
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
translateLet :: HsLocalBinds GhcTc -> DsM PatVec
translateLet _binds = return []

-- | Translate a pattern guard
translateBind :: FamInstEnvs -> LPat GhcTc -> LHsExpr GhcTc -> DsM PatVec
translateBind fam_insts (dL->L _ p) e = do
  ps <- translatePat fam_insts p
  g <- mkGuard ps (unLoc e)
  return [g]

-- | Translate a boolean guard
translateBoolGuard :: LHsExpr GhcTc -> DsM PatVec
translateBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- PatVec for efficiency
  | otherwise = (:[]) <$> mkGuard [truePattern] (unLoc e)

-- | Adds bangs to 'PmVar's in the 'PatVec' if the corresponding field of the
-- 'ConLike' definition had one.
addFieldBangs :: ConLike -> PatVec -> PatVec
addFieldBangs c ps = addBangs bangs ps
  where
    bangs = case c of
      RealDataCon dc -> dataConImplBangs dc
      PatSynCon ps   -> take (patSynArity ps) (repeat HsLazy)

-- | Basically zip the bangs with the 'PatVec', with a few caveats:
--
--    * Skip 'PmGrd's, because they don't match anything. Each bangs corresponds
--      to a pattern arity 1 pattern.
--    * A bang doesn't affect a 'PmCon' because it's already strict, so we just
--      discharge it.
--    * Add the bang to the 'PmVar'.
--
-- Example: @addBangs [HsStrict, HsStrict] [x, 0 <- e, I# 42, True <- p 2]@
--          evaluates to @[!x, 0 <- e, I# 42, True <- p 2]@, so only the first
--          pattern changes from lazy to strict.
addBangs :: [HsImplBang] -> PatVec -> PatVec
addBangs (bang:bangs) (PmVar _ x:ps) = PmVar bang x : addBangs bangs ps
addBangs bangs        (p@PmGrd{}:ps) = p            : addBangs bangs ps
addBangs (_   :bangs) (p@PmCon{}:ps) = p            : addBangs bangs ps
addBangs []           []             = []
addBangs _            _              = panic "addBangs"


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

-- | Get the type out of a PmPat. For guard patterns (ps <- e) we use the type
-- of the first (or the single -WHEREVER IT IS- valid to use?) pattern
pmPatType :: PmPat -> Type
pmPatType (PmCon { pm_con_con = con, pm_con_arg_tys = tys })
  = pmAltConType con tys
pmPatType (PmVar  { pm_var_id  = x }) = idType x
pmPatType (PmGrd  { pm_grd_pv  = pv })
  = ASSERT(patVecArity pv == 1) (pmPatType p)
  where Just p = find ((==1) . patternArity) pv

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
-}

-- ----------------------------------------------------------------------------
-- * More smart constructors and fresh variable generation

-- | Create a guard pattern
mkGuard :: PatVec -> HsExpr GhcTc -> DsM PmPat
mkGuard pv e = PmGrd pv <$> dsExpr e

mkGuardSyntaxExpr :: PatVec -> SyntaxExpr GhcTc -> [HsExpr GhcTc] -> DsM PmPat
mkGuardSyntaxExpr pv f args = do
  core_args <- traverse dsExpr args
  PmGrd pv <$> dsSyntaxExpr f core_args

-- | Generate a lazy variable pattern of a given type
mkPmVar :: Type -> DsM PmPat
mkPmVar ty = PmVar HsLazy <$> mkPmId ty

-- | Generate many lazy variable patterns, given a list of types
mkPmVars :: [Type] -> DsM PatVec
mkPmVars tys = mapM mkPmVar tys

-- | Generate a fresh term variable of a given and return it in two forms:
-- * A variable pattern
-- * A variable expression
mkPmId2Forms :: Type -> DsM (PmPat, LHsExpr GhcTc)
mkPmId2Forms ty = do
  x <- mkPmId ty
  return (PmVar HsLazy x, noLoc (HsVar noExtField (noLoc x)))

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
patternArity _other_pat = 1

{-
%************************************************************************
%*                                                                      *
            Heart of the algorithm: Function pmcheck
%*                                                                      *
%************************************************************************

Main functions are:

* pmcheck :: PatVec -> [PatVec] -> ValVec -> Delta -> DsM PartialResult

  This function implements functions `covered`, `uncovered` and
  `divergent` from the paper at once. Calls out to the auxilary function
  `pmcheckGuards` for handling (possibly multiple) guarded RHSs when the whole
  clause is checked. Slightly different from the paper because it does not even
  produce the covered and uncovered sets. Since we only care about whether a
  clause covers SOMETHING or if it may forces ANY argument, we only store a
  boolean in both cases, for efficiency.

* pmcheckGuards :: [PatVec] -> ValVec -> Delta -> DsM PartialResult

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

-- | Print diagnostic info and actually call 'pmcheck'.
pmcheckI :: PatVec -> [PatVec] -> ValVec -> Int -> Delta -> DsM PartialResult
pmcheckI ps guards vva n delta = do
  tracePm "pmCheck {" $ vcat [ ppr n <> colon
                           , hang (text "patterns:") 2 (ppr ps)
                           , hang (text "guards:") 2 (ppr guards)
                           , ppr vva
                           , ppr delta ]
  res <- pmcheck ps guards vva n delta
  tracePm "}:" (ppr res) -- braces are easier to match by tooling
  return res
{-# INLINE pmcheckI #-}

-- | Check the list of mutually exclusive guards
pmcheckGuards :: [PatVec] -> Int -> Delta -> DsM PartialResult
pmcheckGuards []       _ delta = return (usimple delta)
pmcheckGuards (gv:gvs) n delta = do
  dflags <- getDynFlags
  let limit = maxPmCheckModels dflags `div` 5
  (n', PartialResult cs unc ds pc) <- throttle limit (pmcheckI gv [] []) n delta
  (PartialResult css uncs dss pcs) <- runMany (pmcheckGuards gvs n') unc
  return $ PartialResult (cs `mappend` css)
                         uncs
                         (ds `mappend` dss)
                         (pc `mappend` pcs)

-- | Matching function: Check simultaneously a clause (takes separately the
-- patterns and the list of guards) for exhaustiveness, redundancy and
-- inaccessibility.
pmcheck
  :: PatVec   -- ^ Patterns of the clause
  -> [PatVec] -- ^ (Possibly multiple) guards of the clause
  -> ValVec   -- ^ The value vector abstraction to match against
  -> Int      -- ^ Estimate on the number of similar 'Delta's to handle.
              --   See 6. in Note [Countering exponential blowup]
  -> Delta    -- ^ Oracle state giving meaning to the identifiers in the ValVec
  -> DsM PartialResult
pmcheck [] guards [] n delta
  | null guards = return $ mempty { presultCovered = Covered }
  | otherwise   = pmcheckGuards guards n delta

-- Guard
pmcheck (p@PmGrd { pm_grd_pv = pv, pm_grd_expr = e } : ps) guards vva n delta = do
  tracePm "PmGrd: pmPatType" (vcat [ppr p, ppr (pmPatType p)])
  x <- mkPmId (exprType e)
  delta' <- expectJust "x is fresh" <$> addVarCoreCt delta x e
  pmcheckI (pv ++ ps) guards (x : vva) n delta'

-- Var: Add x :-> y to the oracle and recurse
pmcheck (PmVar bang x : ps) guards (y : vva) n delta = do
  delta_lzy <- expectJust "x is fresh" <$> addTmCt delta (TmVarVar x y)
  if isBanged bang
    then do
      pr <- addTmCt delta_lzy (TmVarNonVoid x) >>= \case
              Nothing        -> pure mempty
              Just delta_str -> pmcheckI ps guards vva n delta_str
      pure (forceIfCanDiverge delta x pr)
    else pmcheckI ps guards vva n delta_lzy

-- ConVar
pmcheck (p : ps) guards (x : vva) n delta
  | PmCon{ pm_con_con = con, pm_con_args = args, pm_con_dicts = dicts } <- p = do
  -- E.g   f (K p q) = <rhs>
  --       <next equation>
  -- Split the value vector into two value vectors:
  --    * one for <rhs>, binding x to (K p q)
  --    * one for <next equation>, recording that x is /not/ (K _ _)

  -- Stuff for <rhs>
  pr_pos <- addPmConCts delta x con dicts args >>= \case
    Nothing -> pure mempty
    Just (delta', arg_vas) ->
      pmcheckI (args ++ ps) guards (arg_vas ++ vva) n delta'

  -- Stuff for <next equation>
  -- The var is forced regardless of whether @con@ was satisfiable
  let pr_pos' = forceIfCanDiverge delta x pr_pos
  pr_neg <- addRefutableAltCon delta x con >>= \case
    Nothing     -> pure mempty
    Just delta' -> pure (usimple delta')

  tracePm "ConVar" (vcat [ppr p, ppr x, ppr pr_pos', ppr pr_neg])

  -- Combine both into a single PartialResult
  let pr = mkUnion pr_pos' pr_neg
  pure pr

pmcheck [] _ (_:_) _ _ = panic "pmcheck: nil-cons"
pmcheck (_:_) _ [] _ _ = panic "pmcheck: cons-nil"

addPmConCts :: Delta -> Id -> PmAltCon -> [EvVar] -> PatVec -> DsM (Maybe (Delta, ValVec))
addPmConCts delta x con dicts field_pats = do
  -- mk_id will re-use the variable name if possible. The x ~ x is easily
  -- discharged by the oracle at no overhead (see 'GHC.HsToCore.PmCheck.Oracle.addVarVarCt').
  let mk_id (PmVar _ x) = pure (Just x)
      mk_id p@PmCon{} = Just <$> mkPmId (pmPatType p)
      mk_id PmGrd{}   = pure Nothing -- PmGrds have arity 0, so just forget about them
  field_vas <- catMaybes <$> traverse mk_id field_pats
  runMaybeT $ do
    delta_ty    <- MaybeT $ addTypeEvidence delta (listToBag dicts)
    delta_tm_ty <- MaybeT $ addTmCt delta_ty (TmVarCon x con field_vas)
    -- strictness on fields is unleashed when we match
    pure (delta_tm_ty, field_vas)

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
-- Computes an approximation of the Covered set for p1 (which pmcheck currently
-- discards).
addPatTmCs ps xs k = do
  fam_insts <- dsGetFamInstEnvs
  pv <- concat <$> translatePatVec fam_insts ps
  locallyExtendPmDelta (\delta -> computeCovered pv xs delta) k

-- | A dead simple version of 'pmcheck' that only computes the Covered set.
-- So it only cares about collecting positive info.
-- We use it to collect info from a pattern when we check its RHS.
-- See 'addPatTmCs'.
computeCovered :: PatVec -> ValVec -> Delta -> DsM (Maybe Delta)
-- The duplication with 'pmcheck' is really unfortunate, but it's simpler than
-- separating out the common cases with 'pmcheck', because that would make the
-- ConVar case harder to understand.
computeCovered [] [] delta = pure (Just delta)
computeCovered (PmGrd { pm_grd_pv = pv, pm_grd_expr = e } : ps) vva delta = do
  x <- mkPmId (exprType e)
  delta' <- expectJust "x is fresh" <$> addVarCoreCt delta x e
  computeCovered (pv ++ ps) (x : vva) delta'
computeCovered (PmVar _ x : ps) (y : vva) delta = do
  delta' <- expectJust "x is fresh" <$> addTmCt delta (TmVarVar x y)
  computeCovered ps vva delta'
computeCovered (p : ps) (x : vva) delta
  | PmCon{ pm_con_con = con, pm_con_args = args, pm_con_dicts = dicts } <- p
  = addPmConCts delta x con dicts args >>= \case
      Nothing -> pure Nothing
      Just (delta', arg_vas) ->
        computeCovered (args ++ ps) (arg_vas ++ vva) delta'
computeCovered ps vs _delta = pprPanic "computeCovered" (ppr ps $$ ppr vs)

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
dsPmWarn :: DynFlags -> DsMatchContext -> PmResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) pm_result
  = when (flag_i || flag_u) $ do
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible && not is_rec_upd
          exists_u = flag_u && (case uncovered of
                                  TypeOfUncovered   _     -> True
                                  UncoveredPatterns _ unc -> notNull unc)
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
        case uncovered of
          TypeOfUncovered ty    -> warnEmptyCase ty
          UncoveredPatterns vars unc -> pprEqns vars unc
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

    -- Print a type-annotated wildcard (for non-exhaustive `EmptyCase`s for
    -- which we only know the type and have no inhabitants at hand)
    warnEmptyCase ty = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      hang (text "Patterns not matched:") 4 (underscore <+> dcolon <+> ppr ty)

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
