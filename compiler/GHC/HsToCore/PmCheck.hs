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
import TcType (evVarPred)
import {-# SOURCE #-} DsExpr (dsExpr, dsLExpr, dsSyntaxExpr)
import {-# SOURCE #-} DsBinds (dsHsWrapper)
import DsUtils (selectMatchVar)
import MatchLit (dsLit, dsOverLit)
import DsMonad
import Bag
import OrdList
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

    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/gadtpm-acm.pdf

%************************************************************************
%*                                                                      *
                     Pattern Match Check Types
%*                                                                      *
%************************************************************************
-}

-- | A very simple language for pattern guards. Let bindings, bang patterns,
-- and matching variables against flat constructor patterns.
data PmGrd
  = -- | @PmCon x K dicts args@ corresponds to a @K dicts args <- x@ guard.
    -- The @args@ are bound in this construct, the @x@ is just a use.
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
  ppr (PmCon x alt _tvs _con_dicts con_args)
    = hsep [ppr alt, hsep (map ppr con_args), text "<-", ppr x]
  ppr (PmBang x) = char '!' <> ppr x
  ppr (PmLet x expr) = hsep [text "let", ppr x, text "=", ppr expr]

type GrdVec = [PmGrd]

data Precision = Approximate | Precise
  deriving (Eq, Show)

instance Outputable Precision where
  ppr = text . show

instance Semi.Semigroup Precision where
  Precise <> Precise = Precise
  _       <> _       = Approximate

instance Monoid Precision where
  mempty = Precise
  mappend = (Semi.<>)

-- | Means by which we identify a RHS for later pretty-printing in a warning
-- message. 'SDoc' for the equation to show, 'Located' for the location.
type RhsInfo = Located SDoc

-- | A representation of the desugaring to 'PmGrd's of all clauses of a
-- function definition/pattern match/etc.
data GrdTree
  = Rhs !RhsInfo
  | Guard !PmGrd !GrdTree
  -- ^ @Guard grd t@ will try to match @grd@ and on success continue to match
  -- @t@. Falls through if either match fails. Models left-to-right semantics
  -- of pattern matching.
  | Sequence !GrdTree !GrdTree
  -- ^ @Sequence l r@ first matches against @l@, and then matches all
  -- fallen-through values against @r@. Models top-to-bottom semantics of
  -- pattern matching.
  | Empty
  -- ^ A @GrdTree@ that always fails. Most useful for
  -- Note [Checking EmptyCase]. A neutral element to 'Sequence'.

-- | The digest of 'checkGrdTree', representing the annotated pattern-match
-- tree. 'redundantAndInaccessibleRhss' can figure out redundant and proper
-- inaccessible RHSs from this.
data AnnotatedTree
  = AccessibleRhs !RhsInfo
  -- ^ A RHS deemed accessible.
  | InaccessibleRhs !RhsInfo
  -- ^ A RHS deemed inaccessible; no value could possibly reach it.
  | MayDiverge !AnnotatedTree
  -- ^ Asserts that the tree may force diverging values, so not all of its
  -- clauses can be redundant.
  | SequenceAnn !AnnotatedTree !AnnotatedTree
  -- ^ Mirrors 'Sequence' for preserving the skeleton of a 'GrdTree's.
  | EmptyAnn
  -- ^ Mirrors 'Empty' for preserving the skeleton of a 'GrdTree's.

pprRhsInfo :: RhsInfo -> SDoc
pprRhsInfo (L (RealSrcSpan rss) _) = ppr (srcSpanStartLine rss)
pprRhsInfo (L s _)                 = ppr s

instance Outputable GrdTree where
  ppr (Rhs info)      = text "->" <+> pprRhsInfo info
  -- Format guards as "| True <- x, let x = 42, !z"
  ppr g@Guard{} = fsep (prefix (map ppr grds)) <+> ppr t
    where
      (t, grds)                  = collect_grds g
      collect_grds (Guard grd t) = (grd :) <$> collect_grds t
      collect_grds t             = (t, [])
      prefix []                  = []
      prefix (s:sdocs)           = char '|' <+> s : map (comma <+>) sdocs
  -- Format nested Sequences in blocks "{ grds1; grds2; ... }"
  ppr t@Sequence{}    = braces (space <> fsep (punctuate semi (collect_seqs t)) <> space)
    where
      collect_seqs (Sequence l r) = collect_seqs l ++ collect_seqs r
      collect_seqs t              = [ppr t]
  ppr Empty          = text "<empty case>"

instance Outputable AnnotatedTree where
  ppr (AccessibleRhs info)   = pprRhsInfo info
  ppr (InaccessibleRhs info) = text "inaccessible" <+> pprRhsInfo info
  ppr (MayDiverge t)         = text "div" <+> ppr t
    -- Format nested Sequences in blocks "{ grds1; grds2; ... }"
  ppr t@SequenceAnn{}        = braces (space <> fsep (punctuate semi (collect_seqs t)) <> space)
    where
      collect_seqs (SequenceAnn l r) = collect_seqs l ++ collect_seqs r
      collect_seqs t                 = [ppr t]
  ppr EmptyAnn               = text "<empty case>"

newtype Deltas = MkDeltas (Bag Delta)

instance Outputable Deltas where
  ppr (MkDeltas deltas) = ppr deltas

instance Semigroup Deltas where
  MkDeltas l <> MkDeltas r = MkDeltas (l `unionBags` r)

liftDeltasM :: Monad m => (Delta -> m (Maybe Delta)) -> Deltas -> m Deltas
liftDeltasM f (MkDeltas ds) = MkDeltas . catBagMaybes <$> (traverse f ds)

-- | Lift 'addPmCts' over 'Deltas'.
addPmCtsDeltas :: Deltas -> PmCts -> DsM Deltas
addPmCtsDeltas deltas cts = liftDeltasM (\d -> addPmCts d cts) deltas

-- | 'addPmCtsDeltas' a single 'PmCt'.
addPmCtDeltas :: Deltas -> PmCt -> DsM Deltas
addPmCtDeltas deltas ct = addPmCtsDeltas deltas (unitBag ct)

-- | Test if any of the 'Delta's is inhabited. Currently this is pure, because
-- we preserve the invariant that there are no uninhabited 'Delta's. But that
-- could change in the future, for example by implementing this function in
-- terms of @notNull <$> provideEvidence 1 ds@.
isInhabited :: Deltas -> DsM Bool
isInhabited (MkDeltas ds) = pure (not (null ds))

-- | Pattern-match check result
data CheckResult
  = CheckResult
  { cr_clauses :: !AnnotatedTree
  -- ^ Captures redundancy info for each clause in the original program.
  --   (for -Woverlapping-patterns)
  , cr_uncov   :: !Deltas
  -- ^ The set of uncovered values falling out at the bottom.
  --   (for -Wincomplete-patterns)
  , cr_approx  :: !Precision
  -- ^ A flag saying whether we ran into the 'maxPmCheckModels' limit for the
  --   purpose of suggesting to crank it up in the warning message
  }

instance Outputable CheckResult where
  ppr (CheckResult c unc pc)
    = text "CheckResult" <+> ppr_precision pc <+> braces (fsep
        [ field "clauses" c <> comma
        , field "uncov" unc])
    where
      ppr_precision Precise     = empty
      ppr_precision Approximate = text "(Approximate)"
      field name value = text name <+> equals <+> ppr value

{-
%************************************************************************
%*                                                                      *
       Entry points to the checker: checkSingle and checkMatches
%*                                                                      *
%************************************************************************
-}

-- | Check a single pattern binding (let) for exhaustiveness.
checkSingle :: DynFlags -> DsMatchContext -> Id -> Pat GhcTc -> DsM ()
checkSingle dflags ctxt@(DsMatchContext kind locn) var p = do
  tracePm "checkSingle" (vcat [ppr ctxt, ppr var, ppr p])
  -- We only ever need to run this in a context where we need exhaustivity
  -- warnings (so not in pattern guards or comprehensions, for example, because
  -- they are perfectly fine to fail).
  -- Omitting checking this flag emits redundancy warnings twice in obscure
  -- cases like #17646.
  when (exhaustive dflags kind) $ do
    missing   <- MkDeltas . unitBag <$> getPmDelta
    tracePm "checkSingle: missing" (ppr missing)
    fam_insts <- dsGetFamInstEnvs
    grd_tree  <- mkGrdTreeRhs (L locn $ ppr p) <$> translatePat fam_insts var p
    res <- checkGrdTree grd_tree missing
    dsPmWarn dflags ctxt [var] res

-- | Exhaustive for guard matches, is used for guards in pattern bindings and
-- in @MultiIf@ expressions.
checkGuardMatches :: HsMatchContext GhcRn         -- Match context
                  -> GRHSs GhcTc (LHsExpr GhcTc)  -- Guarded RHSs
                  -> DsM ()
checkGuardMatches hs_ctx guards@(GRHSs _ grhss _) = do
    dflags <- getDynFlags
    let combinedLoc = foldl1 combineSrcSpans (map getLoc grhss)
        dsMatchContext = DsMatchContext hs_ctx combinedLoc
        match = L combinedLoc $
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

  init_deltas <- MkDeltas . unitBag <$> getPmDelta
  missing <- case matches of
    -- This must be an -XEmptyCase. See Note [Checking EmptyCase]
    [] | [var] <- vars -> addPmCtDeltas init_deltas (PmNotBotCt var)
    _                  -> pure init_deltas
  fam_insts <- dsGetFamInstEnvs
  grd_tree  <- mkGrdTreeMany [] <$> mapM (translateMatch fam_insts vars) matches
  res <- checkGrdTree grd_tree missing

  dsPmWarn dflags ctxt vars res

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
-- where @b@ and @c@ are freshly allocated in @mkListGrds@ and @a@ is the match
-- variable.
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
  -- 'GHC.HsToCore.PmCheck.Oracle.trySolve' and
  -- 'GHC.HsToCore.PmCheck.Oracle.addRefutableAltCon', but it's so much simpler
  -- here. See Note [Representation of Strings in TmState] in
  -- GHC.HsToCore.PmCheck.Oracle
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
  AsPat _ (L _ y) p -> (mkPmLetVar y x ++) <$> translateLPat fam_insts y p

  SigPat _ p _ty -> translateLPat fam_insts x p

  -- See Note [Translate CoPats]
  -- Generally the translation is
  -- pat |> co   ===>   let y = x |> co, pat <- y  where y is a match var of pat
  XPat (CoPat wrapper p _ty)
    | isIdHsWrapper wrapper                   -> translatePat fam_insts x p
    | WpCast co <-  wrapper, isReflexiveCo co -> translatePat fam_insts x p
    | otherwise -> do
        (y, grds) <- translatePatV fam_insts p
        wrap_rhs_y <- dsHsWrapper wrapper
        pure (PmLet y (wrap_rhs_y (Var x)) : grds)

  -- (n + k)  ===>   let b = x >= k, True <- b, let n = x-k
  NPlusKPat _pat_ty (L _ n) k1 k2 ge minus -> do
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

  ConPat { pat_con     = L _ con
         , pat_args    = ps
         , pat_con_ext = ConPatTc
           { pat_arg_tys = arg_tys
           , pat_tvs     = ex_tvs
           , pat_dicts   = dicts
           }
         } -> do
    translateConPatOut fam_insts x con arg_tys ex_tvs dicts ps

  NPat ty (L _ olit) mb_neg _ -> do
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
  SplicePat {} -> panic "Check.translatePat: SplicePat"

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

mkGrdTreeRhs :: Located SDoc -> GrdVec -> GrdTree
mkGrdTreeRhs sdoc = foldr Guard (Rhs sdoc)

mkGrdTreeMany :: GrdVec -> [GrdTree] -> GrdTree
mkGrdTreeMany _    []    = Empty
mkGrdTreeMany grds trees = foldr Guard (foldr1 Sequence trees) grds

-- Translate a single match
translateMatch :: FamInstEnvs -> [Id] -> LMatch GhcTc (LHsExpr GhcTc)
               -> DsM GrdTree
translateMatch fam_insts vars (L match_loc (Match { m_pats = pats, m_grhss = grhss })) = do
  pats'   <- concat <$> zipWithM (translateLPat fam_insts) vars pats
  grhss' <- mapM (translateLGRHS fam_insts match_loc pats) (grhssGRHSs grhss)
  -- tracePm "translateMatch" (vcat [ppr pats, ppr pats', ppr grhss, ppr grhss'])
  return (mkGrdTreeMany pats' grhss')
translateMatch _ _ (L _ (XMatch _)) = panic "translateMatch"

-- -----------------------------------------------------------------------
-- * Transform source guards (GuardStmt Id) to simpler PmGrds

-- | Translate a guarded right-hand side to a single 'GrdTree'
translateLGRHS :: FamInstEnvs -> SrcSpan -> [LPat GhcTc] -> LGRHS GhcTc (LHsExpr GhcTc) -> DsM GrdTree
translateLGRHS fam_insts match_loc pats (L _loc (GRHS _ gs _)) =
  -- _loc apparently points to the match separator that comes after the guards..
  mkGrdTreeRhs loc_sdoc . concat <$> mapM (translateGuard fam_insts . unLoc) gs
    where
      loc_sdoc
        | null gs   = L match_loc (sep (map ppr pats))
        | otherwise = L grd_loc   (sep (map ppr pats) <+> vbar <+> interpp'SP gs)
      L grd_loc _ = head gs
translateLGRHS _ _ _ (L _ (XGRHS _)) = panic "translateLGRHS"

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

This mechanism is implemented in 'throttle'.

Guards are an extreme example in this regard, with #11195 being a particularly
dreadful example: Since their RHS are often pretty much unique, we split on a
variable (the one representing the RHS) that doesn't occur anywhere else in the
program, so we don't actually get useful information out of that split!

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
            Heart of the algorithm: checkGrdTree
%*                                                                      *
%************************************************************************
-}

-- | @throttle limit old new@ returns @old@ if the number of 'Delta's in @new@
-- is exceeding the given @limit@ and the @old@ number of 'Delta's.
-- See Note [Countering exponential blowup].
throttle :: Int -> Deltas -> Deltas -> (Precision, Deltas)
throttle limit old@(MkDeltas old_ds) new@(MkDeltas new_ds)
  --- | pprTrace "PmCheck:throttle" (ppr (length old_ds) <+> ppr (length new_ds) <+> ppr limit) False = undefined
  | length new_ds > max limit (length old_ds) = (Approximate, old)
  | otherwise                                 = (Precise,     new)

-- | Matching on a newtype doesn't force anything.
-- See Note [Divergence of Newtype matches] in Oracle.
conMatchForces :: PmAltCon -> Bool
conMatchForces (PmAltConLike (RealDataCon dc))
  | isNewTyCon (dataConTyCon dc) = False
conMatchForces _                 = True

-- | Makes sure that we only wrap a single 'MayDiverge' around an
-- 'AnnotatedTree', purely for esthetic reasons.
mayDiverge :: AnnotatedTree -> AnnotatedTree
mayDiverge a@(MayDiverge _) = a
mayDiverge a                = MayDiverge a

-- | Computes two things:
--
--   * The set of uncovered values not matched by any of the clauses of the
--     'GrdTree'. Note that 'PmCon' guards are the only way in which values
--     fall through from one 'Many' branch to the next.
--   * An 'AnnotatedTree' that contains divergence and inaccessibility info
--     for all clauses. Will be fed to 'redundantAndInaccessibleRhss' for
--     presenting redundant and proper innaccessible RHSs to the user.
checkGrdTree' :: GrdTree -> Deltas -> DsM CheckResult
-- RHS: Check that it covers something and wrap Inaccessible if not
checkGrdTree' (Rhs sdoc) deltas = do
  is_covered <- isInhabited deltas
  let clauses = if is_covered then AccessibleRhs sdoc else InaccessibleRhs sdoc
  pure CheckResult
    { cr_clauses = clauses
    , cr_uncov   = MkDeltas emptyBag
    , cr_approx  = Precise }
-- let x = e: Refine with x ~ e
checkGrdTree' (Guard (PmLet x e) tree) deltas = do
  deltas' <- addPmCtDeltas deltas (PmCoreCt x e)
  checkGrdTree' tree deltas'
-- Bang x: Diverge on x ~ ⊥, refine with x /~ ⊥
checkGrdTree' (Guard (PmBang x) tree) deltas = do
  has_diverged <- addPmCtDeltas deltas (PmBotCt x) >>= isInhabited
  deltas' <- addPmCtDeltas deltas (PmNotBotCt x)
  res <- checkGrdTree' tree deltas'
  pure res{ cr_clauses = applyWhen has_diverged mayDiverge (cr_clauses res) }
-- Con: Diverge on x ~ ⊥, fall through on x /~ K and refine with x ~ K ys
--      and type info
checkGrdTree' (Guard (PmCon x con tvs dicts args) tree) deltas = do
  has_diverged <-
    if conMatchForces con
      then addPmCtDeltas deltas (PmBotCt x) >>= isInhabited
      else pure False
  unc_this <- addPmCtDeltas deltas (PmNotConCt x con)
  deltas' <- addPmCtsDeltas deltas $
    listToBag (PmTyCt . evVarPred <$> dicts) `snocBag` PmConCt x con tvs args
  CheckResult tree' unc_inner prec <- checkGrdTree' tree deltas'
  limit <- maxPmCheckModels <$> getDynFlags
  let (prec', unc') = throttle limit deltas (unc_this Semi.<> unc_inner)
  pure CheckResult
    { cr_clauses = applyWhen has_diverged mayDiverge tree'
    , cr_uncov = unc'
    , cr_approx = prec Semi.<> prec' }
-- Sequence: Thread residual uncovered sets from equation to equation
checkGrdTree' (Sequence l r) unc_0 = do
  CheckResult l' unc_1 prec_l <- checkGrdTree' l unc_0
  CheckResult r' unc_2 prec_r <- checkGrdTree' r unc_1
  pure CheckResult
    { cr_clauses = SequenceAnn l' r'
    , cr_uncov = unc_2
    , cr_approx = prec_l Semi.<> prec_r }
-- Empty: Fall through for all values
checkGrdTree' Empty unc = do
  pure CheckResult
    { cr_clauses = EmptyAnn
    , cr_uncov = unc
    , cr_approx = Precise }

-- | Print diagnostic info and actually call 'checkGrdTree''.
checkGrdTree :: GrdTree -> Deltas -> DsM CheckResult
checkGrdTree guards deltas = do
  tracePm "checkGrdTree {" $ vcat [ ppr guards
                                  , ppr deltas ]
  res <- checkGrdTree' guards deltas
  tracePm "}:" (ppr res) -- braces are easier to match by tooling
  return res

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
  locallyExtendPmDelta (\delta -> addPmCts delta (PmTyCt . evVarPred <$> ev_vars))

-- | Add equalities for the scrutinee to the local 'DsM' environment when
-- checking a case expression:
--     case e of x { matches }
-- When checking matches we record that (x ~ e) where x is the initial
-- uncovered. All matches will have to satisfy this equality.
addScrutTmCs :: Maybe (LHsExpr GhcTc) -> [Id] -> DsM a -> DsM a
addScrutTmCs Nothing    _   k = k
addScrutTmCs (Just scr) [x] k = do
  scr_e <- dsLExpr scr
  locallyExtendPmDelta (\delta -> addPmCts delta (unitBag (PmCoreCt x scr_e))) k
addScrutTmCs _   _   _ = panic "addScrutTmCs: HsCase with more than one case binder"

addPmConCts :: Delta -> Id -> PmAltCon -> [TyVar] -> [EvVar] -> [Id] -> DsM (Maybe Delta)
addPmConCts delta x con tvs dicts fields = runMaybeT $ do
  delta_ty    <- MaybeT $ addPmCts delta (listToBag (PmTyCt . evVarPred <$> dicts))
  delta_tm_ty <- MaybeT $ addPmCts delta_ty (unitBag (PmConCt x con tvs fields))
  pure delta_tm_ty

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
  delta' <- expectJust "x is fresh" <$> addPmCts delta (unitBag (PmCoreCt x e))
  computeCovered ps delta'
computeCovered (PmBang{} : ps) delta = do
  computeCovered ps delta
computeCovered (p : ps) delta
  | PmCon{ pm_id = x, pm_con_con = con, pm_con_tvs = tvs, pm_con_args = args
         , pm_con_dicts = dicts } <- p
  = addPmConCts delta x con tvs dicts args >>= \case
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

redundantAndInaccessibleRhss :: AnnotatedTree -> ([RhsInfo], [RhsInfo])
redundantAndInaccessibleRhss tree = (fromOL ol_red, fromOL ol_inacc)
  where
    (_ol_acc, ol_inacc, ol_red) = go tree
    -- | Collects RHSs which are
    --    1. accessible
    --    2. proper inaccessible (so we can't delete them)
    --    3. hypothetically redundant (so not only inaccessible RHS, but we can
    --       even safely delete the equation without altering semantics)
    -- See Note [Determining inaccessible clauses]
    go :: AnnotatedTree -> (OrdList RhsInfo, OrdList RhsInfo, OrdList RhsInfo)
    go (AccessibleRhs info)   = (unitOL info, nilOL, nilOL)
    go (InaccessibleRhs info) = (nilOL,       nilOL, unitOL info) -- presumably redundant
    go (MayDiverge t)         = case go t of
      -- See Note [Determining inaccessible clauses]
      (acc, inacc, red)
        | isNilOL acc && isNilOL inacc -> (nilOL, red, nilOL)
      res                              -> res
    go (SequenceAnn l r)      = go l Semi.<> go r
    go EmptyAnn               = (nilOL,       nilOL, nilOL)

{- Note [Determining inaccessible clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f _  True = ()
  f () True = ()
  f _  _    = ()
Is f's second clause redundant? The perhaps surprising answer is, no, it isn't!
@f (error "boom") False@ will force the error with clause 2, but will return
() if it was deleted, so clearly not redundant. Yet for now combination of
arguments we can ever reach clause 2's RHS, so we say it has inaccessible RHS
(as opposed to being completely redundant).

We detect an inaccessible RHS simply by pretending it's redundant, until we see
that it's part of a sub-tree in the pattern match that forces some argument
(which corresponds to wrapping the 'AnnotatedTree' in 'MayDiverge'). Then we
turn all supposedly redundant RHSs into inaccessible ones.

But as it turns out (@g@ from #17465) this is too conservative:
  g () | False = ()
       | otherwise = ()
g's first clause has an inaccessible RHS, but it's also safe to delete. So it's
redundant, really! But by just turning all redundant child clauses into
inaccessible ones, we report the first clause as inaccessible.

Clearly, it is enough if we say that we only degrade if *not all* of the child
clauses are redundant. As long as there is at least one clause which we announce
not to be redundant, the guard prefix responsible for the 'MayDiverge' will
survive. Hence we check for that in 'redundantAndInaccessibleRhss'.
-}

-- | Issue all the warnings (coverage, exhaustiveness, inaccessibility)
dsPmWarn :: DynFlags -> DsMatchContext -> [Id] -> CheckResult -> DsM ()
dsPmWarn dflags ctx@(DsMatchContext kind loc) vars result
  = when (flag_i || flag_u) $ do
      unc_examples <- getNFirstUncovered vars (maxPatterns + 1) uncovered
      let exists_r = flag_i && notNull redundant
          exists_i = flag_i && notNull inaccessible && not is_rec_upd
          exists_u = flag_u && notNull unc_examples
          approx   = precision == Approximate

      when (approx && (exists_u || exists_i)) $
        putSrcSpanDs loc (warnDs NoReason approx_msg)

      when exists_r $ forM_ redundant $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))

      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        pprEqns vars unc_examples
  where
    CheckResult
      { cr_clauses = clauses
      , cr_uncov   = uncovered
      , cr_approx  = precision } = result
    (redundant, inaccessible) = redundantAndInaccessibleRhss clauses

    flag_i = wopt Opt_WarnOverlappingPatterns dflags
    flag_u = exhaustive dflags kind
    flag_u_reason = maybe NoReason Reason (exhaustiveWarningFlag kind)

    is_rec_upd = case kind of { RecUpd -> True; _ -> False }
       -- See Note [Inaccessible warnings for record updates]

    maxPatterns = maxUncoveredPatterns dflags

    -- Print a single clause (for redundant/with-inaccessible-rhs)
    pprEqn q txt = pprContext True ctx (text txt) $ \f ->
      f (q <+> matchSeparator kind <+> text "...")

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

getNFirstUncovered :: [Id] -> Int -> Deltas -> DsM [Delta]
getNFirstUncovered vars n (MkDeltas deltas) = go n (bagToList deltas)
  where
    go 0 _              = pure []
    go _ []             = pure []
    go n (delta:deltas) = do
      front <- provideEvidence vars n delta
      back <- go (n - length front) deltas
      pure (front ++ back)

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
             FunRhs { mc_fun = L _ fun }
                  -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _    -> (pprMatchContext kind, \ pp -> pp)
