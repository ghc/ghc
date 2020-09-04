{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE FlexibleInstances          #-}

-- | This module coverage checks pattern matches. It finds
--
--     * Uncovered patterns, certifying non-exhaustivity
--     * Redundant equations
--     * Equations with an inaccessible right-hand-side
--
-- The algorithm is based on the paper
-- [Lower Your Guards: A Compositional Pattern-Match Coverage Checker"](https://dl.acm.org/doi/abs/10.1145/3408989)
--
-- There is an overview Figure 2 in there that's probably helpful.
-- Here is an overview of how it's implemented, which follows the structure of
-- the entry points such as 'covCheckMatches':
--
--  1. Desugar source syntax (like 'LMatch') to guard tree variants (like
--     'GrdMatch'), with one of the desugaring functions (like 'desugarMatch').
--     Follows Section 3.1 in the paper.
--  2. Coverage check guard trees (with a function like 'checkMatch') to get a
--     'CheckResult', containing
--       a. The set of uncovered values, 'cr_uncov'
--       b. And an annotated tree variant (like 'AnnMatch') that captures
--          redundancy and inaccessibility information as 'RedSets' annotations
--     Basically the UA function from Section 5.1. The Normalised Refinement
--     Types 'Nablas' are maintained in "GHC.HsToCore.PmCheck.Oracle".
--  3. Collect redundancy information into a 'CIRB' with a function such
--     as 'cirbsMatch'. Follows the R function from Figure 6 of the paper.
--  4. Format and report uncovered patterns and redundant equations ('CIRB')
--     with 'formatReportWarnings'. Basically job of the G function, plus proper
--     pretty printing of the warnings (Section 5.4 of the paper).
--  5. Return 'Nablas' reaching syntactic sub-components for
--     Note [Long-distance information]. Collected by functions such as
--     'ldiMatch'. See Section 4.1 of the paper.
module GHC.HsToCore.PmCheck (
        -- Checking and printing
        covCheckPatBind, covCheckMatches, covCheckGRHSs,
        isMatchContextPmChecked,

        -- See Note [Long-distance information]
        addTyCs, addCoreScrutTmCs, addHsScrutTmCs
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.HsToCore.PmCheck.Types
import GHC.HsToCore.PmCheck.Oracle
import GHC.HsToCore.PmCheck.Ppr
import GHC.Types.Basic (Origin(..), isGenerated)
import GHC.Core (CoreExpr, Expr(Var,App))
import GHC.Data.FastString (unpackFS, lengthFS)
import GHC.Driver.Session
import GHC.Hs
import GHC.Tc.Utils.Zonk (shortCutLit)
import GHC.Types.Id
import GHC.Core.ConLike
import GHC.Types.Name
import GHC.Builtin.Types
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.DataCon
import GHC.Types.Var (EvVar)
import GHC.Core.Coercion
import GHC.Tc.Types.Evidence (HsWrapper(..), isIdHsWrapper)
import GHC.Tc.Utils.TcType (evVarPred)
import {-# SOURCE #-} GHC.HsToCore.Expr (dsExpr, dsLExpr, dsSyntaxExpr)
import {-# SOURCE #-} GHC.HsToCore.Binds (dsHsWrapper)
import GHC.HsToCore.Utils (selectMatchVar)
import GHC.HsToCore.Match.Literal (dsLit, dsOverLit)
import GHC.HsToCore.Monad
import GHC.Data.Bag
import GHC.Data.IOEnv (unsafeInterleaveM)
import GHC.Data.OrdList
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.HsToCore.Utils       (isTrueLHsExpr)
import GHC.Data.Maybe
import qualified GHC.LanguageExtensions as LangExt
import GHC.Utils.Monad (concatMapM, mapMaybeM)

import Control.Monad (when, forM_, zipWithM)
import Data.List (elemIndex)
import qualified Data.Semigroup as Semi
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Coerce

--
-- * Exported entry points to the checker
--

-- | A non-empty delta that is initialised from the ambient refinement type
-- capturing long-distance information, or the trivially habitable 'Nablas' if
-- the former is uninhabited.
-- See Note [Recovering from unsatisfiable pattern-matching constraints].
getLdiNablas :: DsM Nablas
getLdiNablas = do
  nablas <- getPmNablas
  isInhabited nablas >>= \case
    True  -> pure nablas
    False -> pure initNablas

-- | Check a pattern binding (let, where) for exhaustiveness.
covCheckPatBind :: DsMatchContext -> Id -> Pat GhcTc -> DsM ()
-- See Note [covCheckPatBind only checks PatBindRhs]
covCheckPatBind ctxt@(DsMatchContext PatBindRhs loc) var p = do
  missing   <- getLdiNablas
  pat_bind <- desugarPatBind loc var p
  tracePm "covCheckPatBind {" (vcat [ppr ctxt, ppr var, ppr p, ppr pat_bind, ppr missing])
  result <- unCA (checkPatBind pat_bind) missing
  tracePm "}: " (ppr (cr_uncov result))
  formatReportWarnings cirbsPatBind ctxt [var] result
covCheckPatBind _ _ _ = pure ()

-- | Exhaustive for guard matches, is used for guards in pattern bindings and
-- in @MultiIf@ expressions. Returns the 'Nablas' covered by the RHSs.
covCheckGRHSs
  :: HsMatchContext GhcRn         -- ^ Match context, for warning messages
  -> GRHSs GhcTc (LHsExpr GhcTc)  -- ^ The GRHSs to check
  -> DsM (NonEmpty Nablas)        -- ^ Covered 'Nablas' for each RHS, for long
                                  --   distance info
covCheckGRHSs hs_ctxt guards@(GRHSs _ grhss _) = do
  let combined_loc = foldl1 combineSrcSpans (map getLoc grhss)
      ctxt = DsMatchContext hs_ctxt combined_loc
  matches <- desugarGRHSs combined_loc empty guards
  missing   <- getLdiNablas
  tracePm "covCheckGRHSs" (hang (vcat [ppr ctxt
                                , text "Guards:"])
                                2
                                (pprGRHSs hs_ctxt guards $$ ppr missing))
  result <- unCA (checkGRHSs matches) missing
  tracePm "}: " (ppr (cr_uncov result))
  formatReportWarnings cirbsGRHSs ctxt [] result
  return (ldiGRHS <$> cr_ret result)

-- | Check a list of syntactic 'Match'es (part of case, functions, etc.), each
-- with a 'Pat' and one or more 'GRHSs':
--
-- @
--   f x y | x == y    = 1   -- match on x and y with two guarded RHSs
--         | otherwise = 2
--   f _ _             = 3   -- clause with a single, un-guarded RHS
-- @
--
-- Returns one non-empty 'Nablas' for 1.) each pattern of a 'Match' and 2.)
-- each of a 'Match'es 'GRHS' for Note [Long-distance information].
--
-- Special case: When there are /no matches/, then the functionassumes it
-- checks and @-XEmptyCase@ with only a single match variable.
-- See Note [Checking EmptyCase].
covCheckMatches
  :: DsMatchContext                  -- ^ Match context, for warnings messages
  -> [Id]                            -- ^ Match variables, i.e. x and y above
  -> [LMatch GhcTc (LHsExpr GhcTc)]  -- ^ List of matches
  -> DsM [(Nablas, NonEmpty Nablas)] -- ^ One covered 'Nablas' per Match and
                                     --   GRHS, for long distance info.
covCheckMatches ctxt vars matches = do
  -- We have to force @missing@ before printing out the trace message,
  -- otherwise we get interleaved output from the solver. This function
  -- should be strict in @missing@ anyway!
  !missing   <- getLdiNablas
  tracePm "covCheckMatches {" $
          hang (vcat [ppr ctxt, ppr vars, text "Matches:"])
               2
               (vcat (map ppr matches) $$ ppr missing)
  case NE.nonEmpty matches of
    Nothing -> do
      -- This must be an -XEmptyCase. See Note [Checking EmptyCase]
      let var = only vars
      empty_case <- desugarEmptyCase var
      result <- unCA (checkEmptyCase empty_case) missing
      tracePm "}: " (ppr (cr_uncov result))
      formatReportWarnings cirbsEmptyCase ctxt vars result
      return []
    Just matches -> do
      matches <- desugarMatches vars matches
      result <- unCA (checkMatchGroup matches) missing
      tracePm "}: " (ppr (cr_uncov result))
      formatReportWarnings cirbsMatchGroup ctxt vars result
      return (NE.toList (ldiMatchGroup (cr_ret result)))

{- Note [covCheckPatBind only checks PatBindRhs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@covCheckPatBind@'s sole purpose is to check vanilla pattern bindings, like
@x :: Int; Just x = e@, which is in a @PatBindRhs@ context.
But its caller is also called for individual pattern guards in a @StmtCtxt@.
For example, both pattern guards in @f x y | True <- x, False <- y = ...@ will
go through this function. It makes no sense to do coverage checking there:
  * Pattern guards may well fail. Fall-through is not an unrecoverable panic,
    but rather behavior the programmer expects, so inexhaustivity should not be
    reported.
  * Redundancy is already reported for the whole GRHS via one of the other
    exported coverage checking functions. Also reporting individual redundant
    guards is... redundant. See #17646.
Note that we can't just omit checking of @StmtCtxt@ altogether (by adjusting
'isMatchContextPmChecked'), because that affects the other checking functions,
too.

Note [Checking EmptyCase]
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
match var x, which rules out ⊥ as an inhabitant. So we add x ≁ ⊥ to the
initial Nabla and check if there are any values left to match on.
-}

--
-- * Guard language
--

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
    -- If the extra 'SrcInfo' is present, the bang guard came from a source
    -- bang pattern, in which case we might want to report it as redundant.
    -- See Note [Dead bang patterns].
  | PmBang {
      pm_id   :: !Id,
      _pm_loc :: !(Maybe SrcInfo)
    }

    -- | @PmLet x expr@ corresponds to a @let x = expr@ guard. This actually
    -- /binds/ @x@.
  | PmLet {
      pm_id        :: !Id,
      _pm_let_expr :: !CoreExpr
    }

-- | Should not be user-facing.
instance Outputable PmGrd where
  ppr (PmCon x alt _tvs _con_dicts con_args)
    = hsep [ppr alt, hsep (map ppr con_args), text "<-", ppr x]
  ppr (PmBang x _loc) = char '!' <> ppr x
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

--
-- * Guard tree language
--

-- | Means by which we identify a source construct for later pretty-printing in
-- a warning message. 'SDoc' for the equation to show, 'Located' for the
-- location.
type SrcInfo = Located SDoc

-- | Redundancy sets, used to determine redundancy of RHSs and bang patterns
-- (later digested into a 'CIRB').
data RedSets
  = RedSets
  { rs_cov :: !Nablas
  -- ^ The /Covered/ set; the set of values reaching a particular program
  -- point.
  , rs_div :: !Nablas
  -- ^ The /Diverging/ set; empty if no match can lead to divergence.
  --   If it wasn't empty, we have to turn redundancy warnings into
  --   inaccessibility warnings for any subclauses.
  , rs_bangs :: !(OrdList (Nablas, SrcInfo))
  -- ^ If any of the 'Nablas' is empty, the corresponding 'SrcInfo' pin-points
  -- a bang pattern in source that is redundant. See Note [Dead bang patterns].
  }

-- The following two type synonyms instantiate our tree structures to guard
-- trees and annotated trees, respectively, by giving the types to attach as
-- payload.

-- | Used as tree payload pre-checking. The LYG guards to check.
type Pre = [PmGrd]

-- | Used as tree payload post-checking. The redundancy info we elaborated.
type Post = RedSets

-- | A guard tree denoting 'MatchGroup'.
newtype PmMatchGroup p = PmMatchGroup (NonEmpty (PmMatch p))

-- | A guard tree denoting 'Match': A payload describing the pats and a bunch of
-- GRHS.
data PmMatch p = PmMatch { pm_pats :: !p, pm_grhss :: !(NonEmpty (PmGRHS p)) }

-- | A guard tree denoting 'GRHS': A payload describing the grds and a 'SrcInfo'
-- useful for printing out in warnings messages.
data PmGRHS p = PmGRHS { pg_grds :: !p, pg_rhs :: !SrcInfo }

-- | A guard tree denoting an -XEmptyCase.
newtype PmEmptyCase = PmEmptyCase { pe_var :: Id }

-- | A guard tree denoting a pattern binding.
newtype PmPatBind p =
  -- just reuse GrdGRHS and pretend its @SrcInfo@ is info on the /pattern/,
  -- rather than on the pattern bindings.
  PmPatBind (PmGRHS p)

emptyRedSets :: RedSets
-- Semigroup instance would be misleading!
emptyRedSets = RedSets mempty mempty mempty

pprSrcInfo :: SrcInfo -> SDoc
pprSrcInfo (L (RealSrcSpan rss _) _) = ppr (srcSpanStartLine rss)
pprSrcInfo (L s _)                   = ppr s

-- | Format LYG guards as @| True <- x, let x = 42, !z@
pprLygGuards :: [PmGrd] -> SDoc
pprLygGuards []     = empty
pprLygGuards (g:gs) = fsep (char '|' <+> ppr g : map ((comma <+>) . ppr) gs)

-- | Format a LYG sequence (e.g. 'Match'es of a 'MatchGroup' or 'GRHSs') as
-- @{ <first alt>; ...; <last alt> }@
pprLygSequence :: Outputable a => NonEmpty a -> SDoc
pprLygSequence (NE.toList -> as) =
  braces (space <> fsep (punctuate semi (map ppr as)) <> space)

instance Outputable (PmMatchGroup Pre) where
  ppr (PmMatchGroup matches) = pprLygSequence matches

instance Outputable (PmMatch Pre) where
  ppr (PmMatch { pm_pats = grds, pm_grhss = grhss }) =
    pprLygGuards grds <+> ppr grhss

instance Outputable (PmGRHS Pre) where
  ppr (PmGRHS { pg_grds = grds, pg_rhs = rhs }) =
    pprLygGuards grds <+> text "->" <+> pprSrcInfo rhs

instance Outputable (PmPatBind Pre) where
  ppr (PmPatBind PmGRHS { pg_grds = grds, pg_rhs = bind }) =
    ppr bind <+> pprLygGuards grds <+> text "=" <+> text "..."

instance Outputable PmEmptyCase where
  ppr (PmEmptyCase { pe_var = var }) =
    text "<empty case on " <> ppr var <> text ">"

pprRedSets :: RedSets -> SDoc
-- It's useful to change this definition for different verbosity levels in
-- printf-debugging
pprRedSets RedSets { rs_cov = _cov, rs_div = _div, rs_bangs = _bangs }
  = empty

instance Outputable (PmMatchGroup Post) where
  ppr (PmMatchGroup matches) = pprLygSequence matches

instance Outputable (PmMatch Post) where
  ppr (PmMatch { pm_pats = red, pm_grhss = grhss }) =
    pprRedSets red <+> ppr grhss

instance Outputable (PmGRHS Post) where
  ppr (PmGRHS { pg_grds = red, pg_rhs = rhs }) =
    pprRedSets red <+> text "->" <+> pprSrcInfo rhs

{- Note [Dead bang patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f :: Bool -> Int
  f True = 1
  f !x   = 2

Whenever we fall through to the second equation, we will already have evaluated
the argument. Thus, the bang pattern serves no purpose and should be warned
about. We call this kind of bang patterns "dead". Dead bangs are the ones
that under no circumstances can force a thunk that wasn't already forced.
Dead bangs are a form of redundant bangs; see below.

We can detect dead bang patterns by checking whether @x ~ ⊥@ is satisfiable
where the PmBang appears in 'checkGrd'. If not, then clearly the bang is
dead. Such a dead bang is then indicated in the annotated pattern-match tree by
a 'RedundantSrcBang' wrapping. In 'redundantAndInaccessibles', we collect
all dead bangs to warn about.

Note that we don't want to warn for a dead bang that appears on a redundant
clause. That is because in that case, we recommend to delete the clause wholly,
including its leading pattern match.

Dead bang patterns are redundant. But there are bang patterns which are
redundant that aren't dead, for example

  f !() = 0

the bang still forces the match variable, before we attempt to match on (). But
it is redundant with the forcing done by the () match. We currently don't
detect redundant bangs that aren't dead.
-}

--
-- * Desugaring source syntax to guard trees
--

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
--   @"[(x:b) <- a, True <- x, (y:c) <- b, !y, [] <- c]"@
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
  -- We desugar String literals to list literals for better overlap reasoning.
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

-- | @desugarPat _ x pat@ transforms @pat@ into a 'GrdVec', where
-- the variable representing the match is @x@.
desugarPat :: Id -> Pat GhcTc -> DsM GrdVec
desugarPat x pat = case pat of
  WildPat  _ty -> pure []
  VarPat _ y   -> pure (mkPmLetVar (unLoc y) x)
  ParPat _ p   -> desugarLPat x p
  LazyPat _ _  -> pure [] -- like a wildcard
  BangPat _ p@(L l p') ->
    -- Add the bang in front of the list, because it will happen before any
    -- nested stuff.
    (PmBang x pm_loc :) <$> desugarLPat x p
      where pm_loc = Just (L l (ppr p'))

  -- (x@pat)   ==>   Desugar pat with x as match var and handle impedance
  --                 mismatch with incoming match var
  AsPat _ (L _ y) p -> (mkPmLetVar y x ++) <$> desugarLPat y p

  SigPat _ p _ty -> desugarLPat x p

  -- See Note [Desugar CoPats]
  -- Generally the translation is
  -- pat |> co   ===>   let y = x |> co, pat <- y  where y is a match var of pat
  XPat (CoPat wrapper p _ty)
    | isIdHsWrapper wrapper                   -> desugarPat x p
    | WpCast co <-  wrapper, isReflexiveCo co -> desugarPat x p
    | otherwise -> do
        (y, grds) <- desugarPatV p
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
    (y, grds) <- desugarLPatV pat
    fun <- dsLExpr lexpr
    pure $ PmLet y (App fun (Var x)) : grds

  -- list
  ListPat (ListPatTc _elem_ty Nothing) ps ->
    desugarListPat x ps

  -- overloaded list
  ListPat (ListPatTc elem_ty (Just (pat_ty, to_list))) pats -> do
    dflags <- getDynFlags
    case splitListTyConApp_maybe pat_ty of
      Just _e_ty
        | not (xopt LangExt.RebindableSyntax dflags)
        -- Just desugar it as a regular ListPat
        -> desugarListPat x pats
      _ -> do
        y <- mkPmId (mkListTy elem_ty)
        grds <- desugarListPat y pats
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
           { cpt_arg_tys = arg_tys
           , cpt_tvs     = ex_tvs
           , cpt_dicts   = dicts
           }
         } -> do
    desugarConPatOut x con arg_tys ex_tvs dicts ps

  NPat ty (L _ olit) mb_neg _ -> do
    -- See Note [Literal short cut] in "GHC.HsToCore.Match.Literal"
    -- We inline the Literal short cut for @ty@ here, because @ty@ is more
    -- precise than the field of OverLitTc, which is all that dsOverLit (which
    -- normally does the literal short cut) can look at. Also @ty@ matches the
    -- type of the scrutinee, so info on both pattern and scrutinee (for which
    -- short cutting in dsOverLit works properly) is overloaded iff either is.
    dflags <- getDynFlags
    let platform = targetPlatform dflags
    core_expr <- case olit of
      OverLit{ ol_val = val, ol_ext = OverLitTc rebindable _ }
        | not rebindable
        , Just expr <- shortCutLit platform val ty
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
    (vars, grdss) <- mapAndUnzipM desugarLPatV pats
    let tuple_con = tupleDataCon boxity (length vars)
    pure $ vanillaConGrd x tuple_con vars : concat grdss

  SumPat _ty p alt arity -> do
    (y, grds) <- desugarLPatV p
    let sum_con = sumDataCon alt arity
    -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    pure $ vanillaConGrd x sum_con [y] : grds

  SplicePat {} -> panic "Check.desugarPat: SplicePat"

-- | 'desugarPat', but also select and return a new match var.
desugarPatV :: Pat GhcTc -> DsM (Id, GrdVec)
desugarPatV pat = do
  x <- selectMatchVar Many pat
  grds <- desugarPat x pat
  pure (x, grds)

desugarLPat :: Id -> LPat GhcTc -> DsM GrdVec
desugarLPat x = desugarPat x . unLoc

-- | 'desugarLPat', but also select and return a new match var.
desugarLPatV :: LPat GhcTc -> DsM (Id, GrdVec)
desugarLPatV = desugarPatV . unLoc

-- | @desugarListPat _ x [p1, ..., pn]@ is basically
--   @desugarConPatOut _ x $(mkListConPatOuts [p1, ..., pn]>@ without ever
-- constructing the 'ConPatOut's.
desugarListPat :: Id -> [LPat GhcTc] -> DsM GrdVec
desugarListPat x pats = do
  vars_and_grdss <- traverse desugarLPatV pats
  mkListGrds x vars_and_grdss

-- | Desugar a constructor pattern
desugarConPatOut :: Id -> ConLike -> [Type] -> [TyVar]
                 -> [EvVar] -> HsConPatDetails GhcTc -> DsM GrdVec
desugarConPatOut x con univ_tys ex_tvs dicts = \case
    PrefixCon ps                 -> go_field_pats (zip [0..] ps)
    InfixCon  p1 p2              -> go_field_pats (zip [0..] [p1,p2])
    RecCon    (HsRecFields fs _) -> go_field_pats (rec_field_ps fs)
  where
    -- The actual argument types (instantiated)
    arg_tys     = map scaledThing $ conLikeInstOrigArgTys con (univ_tys ++ mkTyVarTys ex_tvs)

    -- Extract record field patterns tagged by field index from a list of
    -- LHsRecField
    rec_field_ps fs = map (tagged_pat . unLoc) fs
      where
        tagged_pat f = (lbl_to_index (getName (hsRecFieldId f)), hsRecFieldArg f)
        -- Unfortunately the label info is empty when the DataCon wasn't defined
        -- with record field labels, hence we desugar to field index.
        orig_lbls        = map flSelector $ conLikeFieldLabels con
        lbl_to_index lbl = expectJust "lbl_to_index" $ elemIndex lbl orig_lbls

    go_field_pats tagged_pats = do
      -- The fields that appear might not be in the correct order. So
      --   1. Do the PmCon match
      --   2. Then pattern match on the fields in the order given by the first
      --      field of @tagged_pats@.
      -- See Note [Field match order for RecCon]

      -- Desugar the mentioned field patterns. We're doing this first to get
      -- the Ids for pm_con_args and bring them in order afterwards.
      let trans_pat (n, pat) = do
            (var, pvec) <- desugarLPatV pat
            pure ((n, var), pvec)
      (tagged_vars, arg_grdss) <- mapAndUnzipM trans_pat tagged_pats

      let get_pat_id n ty = case lookup n tagged_vars of
            Just var -> pure var
            Nothing  -> mkPmId ty

      -- 1. the constructor pattern match itself
      arg_ids <- zipWithM get_pat_id [0..] arg_tys
      let con_grd = PmCon x (PmAltConLike con) ex_tvs dicts arg_ids

      -- 2. guards from field selector patterns
      let arg_grds = concat arg_grdss

      -- tracePm "ConPatOut" (ppr x $$ ppr con $$ ppr arg_ids)
      pure (con_grd : arg_grds)

desugarPatBind :: SrcSpan -> Id -> Pat GhcTc -> DsM (PmPatBind Pre)
-- See 'GrdPatBind' for how this simply repurposes GrdGRHS.
desugarPatBind loc var pat =
  PmPatBind . flip PmGRHS (L loc (ppr pat)) <$> desugarPat var pat

desugarEmptyCase :: Id -> DsM PmEmptyCase
desugarEmptyCase var = pure PmEmptyCase { pe_var = var }

-- | Desugar the non-empty 'Match'es of a 'MatchGroup'.
desugarMatches :: [Id] -> NonEmpty (LMatch GhcTc (LHsExpr GhcTc))
               -> DsM (PmMatchGroup Pre)
desugarMatches vars matches =
  PmMatchGroup <$> traverse (desugarMatch vars) matches

-- Desugar a single match
desugarMatch :: [Id] -> LMatch GhcTc (LHsExpr GhcTc) -> DsM (PmMatch Pre)
desugarMatch vars (L match_loc (Match { m_pats = pats, m_grhss = grhss })) = do
  pats'  <- concat <$> zipWithM desugarLPat vars pats
  grhss' <- desugarGRHSs match_loc (sep (map ppr pats)) grhss
  -- tracePm "desugarMatch" (vcat [ppr pats, ppr pats', ppr grhss'])
  return PmMatch { pm_pats = pats', pm_grhss = grhss' }

desugarGRHSs :: SrcSpan -> SDoc -> GRHSs GhcTc (LHsExpr GhcTc) -> DsM (NonEmpty (PmGRHS Pre))
desugarGRHSs match_loc pp_pats grhss
  = traverse (desugarLGRHS match_loc pp_pats)
  . expectJust "desugarGRHSs"
  . NE.nonEmpty
  $ grhssGRHSs grhss

-- | Desugar a guarded right-hand side to a single 'GrdTree'
desugarLGRHS :: SrcSpan -> SDoc -> LGRHS GhcTc (LHsExpr GhcTc) -> DsM (PmGRHS Pre)
desugarLGRHS match_loc pp_pats (L _loc (GRHS _ gs _)) = do
  -- _loc points to the match separator (ie =, ->) that comes after the guards.
  -- Hence we have to pass in the match_loc, which we use in case that the RHS
  -- is unguarded.
  -- pp_pats is the space-separated pattern of the current Match this
  -- GRHS belongs to, so the @A B x@ part in @A B x | 0 <- x@.
  let rhs_info = case gs of
        []              -> L match_loc pp_pats
        (L grd_loc _):_ -> L grd_loc   (pp_pats <+> vbar <+> interpp'SP gs)
  grds <- concatMapM (desugarGuard . unLoc) gs
  pure PmGRHS { pg_grds = grds, pg_rhs = rhs_info }

-- | Desugar a guard statement to a 'GrdVec'
desugarGuard :: GuardStmt GhcTc -> DsM GrdVec
desugarGuard guard = case guard of
  BodyStmt _   e _ _ -> desugarBoolGuard e
  LetStmt  _   binds -> desugarLet (unLoc binds)
  BindStmt _ p e     -> desugarBind p e
  LastStmt        {} -> panic "desugarGuard LastStmt"
  ParStmt         {} -> panic "desugarGuard ParStmt"
  TransStmt       {} -> panic "desugarGuard TransStmt"
  RecStmt         {} -> panic "desugarGuard RecStmt"
  ApplicativeStmt {} -> panic "desugarGuard ApplicativeLastStmt"

-- | Desugar let-bindings
desugarLet :: HsLocalBinds GhcTc -> DsM GrdVec
desugarLet _binds = return []

-- | Desugar a pattern guard
--   @pat <- e ==>  let x = e;  <guards for pat <- x>@
desugarBind :: LPat GhcTc -> LHsExpr GhcTc -> DsM GrdVec
desugarBind p e = dsLExpr e >>= \case
  Var y
    | Nothing <- isDataConId_maybe y
    -- RHS is a variable, so that will allow us to omit the let
    -> desugarLPat y p
  rhs -> do
    (x, grds) <- desugarLPatV p
    pure (PmLet x rhs : grds)

-- | Desugar a boolean guard
--   @e ==>  let x = e; True <- x@
desugarBoolGuard :: LHsExpr GhcTc -> DsM GrdVec
desugarBoolGuard e
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
        pure [PmLet x rhs, vanillaConGrd x trueDataCon []]

{- Note [Field match order for RecCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The order for RecCon field patterns actually determines evaluation order of
the pattern match. For example:

  data T = T { a :: Char, b :: Int }
  f :: T -> ()
  f T{ b = 42, a = 'a' } = ()

Then @f (T (error "a") (error "b"))@ errors out with "b" because it is mentioned
first in the pattern match.

This means we can't just desugar the pattern match to
@[T a b <- x, 'a' <- a, 42 <- b]@. Instead we have to force them in the
right order: @[T a b <- x, 42 <- b, 'a' <- a]@.

Note [Strict fields and fields of unlifted type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do strict fields play into Note [Field match order for RecCon]? Answer:
They don't. Desugaring is entirely unconcerned by strict fields; the forcing
happens *before* pattern matching. But for each strict (or more generally,
unlifted) field @s@ we have to add @s ≁ ⊥@ constraints when we check the PmCon
guard in 'checkGrd'. Strict fields are devoid of ⊥ by construction, there's
nothing that a bang pattern would act on. Example from #18341:

  data T = MkT !Int
  f :: T -> ()
  f (MkT  _) | False = () -- inaccessible
  f (MkT !_) | False = () -- redundant, not only inaccessible!
  f _                = ()

The second clause desugars to @MkT n <- x, !n@. When coverage checked, the
'PmCon' @MkT n <- x@ refines the set of values that reach the bang pattern with
the constraints @x ~ MkT n, n ≁ ⊥@ (this list is computed by 'pmConCts').
Checking the 'PmBang' @!n@ will then try to add the constraint @n ~ ⊥@ to this
set to get the diverging set, which is found to be empty. Hence the whole
clause is detected as redundant, as expected.

Note [Order of guards matters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Similar to Note [Field match order for RecCon], the order in which the guards
for a pattern match appear matter. Consider a situation similar to T5117:

  f (0:_)  = ()
  f (0:[]) = ()

The latter clause is clearly redundant. Yet if we desugar the second clause as

  [x:xs' <- xs, [] <- xs', 0 <- x]

We will say that the second clause only has an inaccessible RHS. That's because
we force the tail of the list before comparing its head! So the correct
translation would have been

  [x:xs' <- xs, 0 <- x, [] <- xs']

And we have to take in the guards on list cells into @mkListGrds@.

Note [Desugar CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns
`CoPat` efficiently, which gave rise to #11276. The original approach
desugared `CoPat`s:

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
-}

--
-- * Coverage checking guard trees into annotated trees
--

-- | Pattern-match coverage check result
data CheckResult a
  = CheckResult
  { cr_ret :: !a
  -- ^ A hole for redundancy info and covered sets.
  , cr_uncov   :: !Nablas
  -- ^ The set of uncovered values falling out at the bottom.
  --   (for -Wincomplete-patterns, but also important state for the algorithm)
  , cr_approx  :: !Precision
  -- ^ A flag saying whether we ran into the 'maxPmCheckModels' limit for the
  -- purpose of suggesting to crank it up in the warning message. Writer state.
  } deriving Functor

instance Outputable a => Outputable (CheckResult a) where
  ppr (CheckResult c unc pc)
    = text "CheckResult" <+> ppr_precision pc <+> braces (fsep
        [ field "ret" c <> comma
        , field "uncov" unc])
    where
      ppr_precision Precise     = empty
      ppr_precision Approximate = text "(Approximate)"
      field name value = text name <+> equals <+> ppr value

-- | Lift 'addPmCts' over 'Nablas'.
addPmCtsNablas :: Nablas -> PmCts -> DsM Nablas
addPmCtsNablas nablas cts = liftNablasM (\d -> addPmCts d cts) nablas

-- | 'addPmCtsNablas' for a single 'PmCt'.
addPmCtNablas :: Nablas -> PmCt -> DsM Nablas
addPmCtNablas nablas ct = addPmCtsNablas nablas (unitBag ct)

-- | Test if any of the 'Nabla's is inhabited. Currently this is pure, because
-- we preserve the invariant that there are no uninhabited 'Nabla's. But that
-- could change in the future, for example by implementing this function in
-- terms of @notNull <$> provideEvidence 1 ds@.
isInhabited :: Nablas -> DsM Bool
isInhabited (MkNablas ds) = pure (not (null ds))

-- | Coverage checking action. Can be composed 'leftToRight' or 'topToBottom'.
newtype CheckAction a = CA { unCA :: Nablas -> DsM (CheckResult a) }
  deriving Functor

-- | Composes 'CheckAction's top-to-bottom:
-- If a value falls through the resulting action, then it must fall through the
-- first action and then through the second action.
-- If a value matches the resulting action, then it either matches the
-- first action or matches the second action.
-- Basically the semantics of the LYG branching construct.
topToBottom :: (top -> bot -> ret)
            -> CheckAction top
            -> CheckAction bot
            -> CheckAction ret
topToBottom f (CA top) (CA bot) = CA $ \inc -> do
  t <- top inc
  b <- bot (cr_uncov t)
  pure CheckResult { cr_ret = f (cr_ret t) (cr_ret b)
                   , cr_uncov = cr_uncov b
                   , cr_approx = cr_approx t Semi.<> cr_approx b }


-- | Composes 'CheckAction's left-to-right:
-- If a value falls through the resulting action, then it either falls through the
-- first action or through the second action.
-- If a value matches the resulting action, then it must match the first action
-- and then match the second action.
-- Basically the semantics of the LYG guard construct.
leftToRight :: (RedSets -> right -> ret)
            -> CheckAction RedSets
            -> CheckAction right
            -> CheckAction ret
leftToRight f (CA left) (CA right) = CA $ \inc -> do
  l <- left inc
  r <- right (rs_cov (cr_ret l))
  limit <- maxPmCheckModels <$> getDynFlags
  let uncov = cr_uncov l Semi.<> cr_uncov r
  -- See Note [Countering exponential blowup]
  let (prec', uncov') = throttle limit inc uncov
  pure CheckResult { cr_ret = f (cr_ret l) (cr_ret r)
                   , cr_uncov = uncov'
                   , cr_approx = prec' Semi.<> cr_approx l Semi.<> cr_approx r }

-- | @throttle limit old new@ returns @old@ if the number of 'Nabla's in @new@
-- is exceeding the given @limit@ and the @old@ number of 'Nabla's.
-- See Note [Countering exponential blowup].
throttle :: Int -> Nablas -> Nablas -> (Precision, Nablas)
throttle limit old@(MkNablas old_ds) new@(MkNablas new_ds)
  --- | pprTrace "PmCheck:throttle" (ppr (length old_ds) <+> ppr (length new_ds) <+> ppr limit) False = undefined
  | length new_ds > max limit (length old_ds) = (Approximate, old)
  | otherwise                                 = (Precise,     new)

-- | The 'PmCts' arising from a successful  'PmCon' match @T gammas as ys <- x@.
-- These include
--
--   * @gammas@: Constraints arising from the bound evidence vars
--   * @y ≁ ⊥@ constraints for each unlifted field (including strict fields)
--     @y@ in @ys@
--   * The constructor constraint itself: @x ~ T as ys@.
--
-- See Note [Strict fields and fields of unlifted type].
pmConCts :: Id -> PmAltCon -> [TyVar] -> [EvVar] -> [Id] -> PmCts
pmConCts x con tvs dicts args = gammas `unionBags` unlifted `snocBag` con_ct
  where
    gammas   = listToBag $ map (PmTyCt . evVarPred) dicts
    con_ct   = PmConCt x con tvs args
    unlifted = listToBag [ PmNotBotCt arg
                         | (arg, bang) <-
                             zipEqual "pmConCts" args (pmAltConImplBangs con)
                         , isBanged bang || isUnliftedType (idType arg)
                         ]

checkSequence :: (grdtree -> CheckAction anntree) -> NonEmpty grdtree -> CheckAction (NonEmpty anntree)
-- The implementation is pretty similar to
-- @traverse1 :: Apply f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)@
checkSequence act (t :| [])       = (:| []) <$> act t
checkSequence act (t1 :| (t2:ts)) =
  topToBottom (NE.<|) (act t1) (checkSequence act (t2:|ts))

checkGrd :: PmGrd -> CheckAction RedSets
checkGrd grd = CA $ \inc -> case grd of
  -- let x = e: Refine with x ~ e
  PmLet x e -> do
    matched <- addPmCtNablas inc (PmCoreCt x e)
    -- tracePm "check:Let" (ppr x <+> char '=' <+> ppr e)
    pure CheckResult { cr_ret = emptyRedSets { rs_cov = matched }
                     , cr_uncov = mempty
                     , cr_approx = Precise }
  -- Bang x _: Diverge on x ~ ⊥, refine with x ≁ ⊥
  PmBang x mb_info -> do
    div <- addPmCtNablas inc (PmBotCt x)
    matched <- addPmCtNablas inc (PmNotBotCt x)
    -- See Note [Dead bang patterns]
    -- mb_info = Just info <==> PmBang originates from bang pattern in source
    let bangs | Just info <- mb_info = unitOL (div, info)
              | otherwise            = NilOL
    -- tracePm "check:Bang" (ppr x <+> ppr div)
    pure CheckResult { cr_ret = RedSets { rs_cov = matched, rs_div = div, rs_bangs = bangs }
                     , cr_uncov = mempty
                     , cr_approx = Precise }
  -- Con: Fall through on x ≁ K and refine with x ~ K ys and type info
  PmCon x con tvs dicts args -> do
    !div <- if isPmAltConMatchStrict con
      then addPmCtNablas inc (PmBotCt x)
      else pure mempty
    let con_cts = pmConCts x con tvs dicts args
    !matched <- addPmCtsNablas inc con_cts
    !uncov   <- addPmCtNablas  inc (PmNotConCt x con)
    -- tracePm "checkGrd:Con" (ppr inc $$ ppr grd $$ ppr con_cts $$ ppr matched)
    pure CheckResult { cr_ret = emptyRedSets { rs_cov = matched, rs_div = div }
                     , cr_uncov = uncov
                     , cr_approx = Precise }

checkGrds :: [PmGrd] -> CheckAction RedSets
checkGrds [] = CA $ \inc ->
  pure CheckResult { cr_ret = emptyRedSets { rs_cov = inc }
                   , cr_uncov = mempty
                   , cr_approx = Precise }
checkGrds (g:grds) = leftToRight merge (checkGrd g) (checkGrds grds)
  where
    merge ri_g ri_grds = -- This operation would /not/ form a Semigroup!
      RedSets { rs_cov   = rs_cov ri_grds
              , rs_div   = rs_div ri_g   Semi.<> rs_div ri_grds
              , rs_bangs = rs_bangs ri_g Semi.<> rs_bangs ri_grds }

checkMatchGroup :: PmMatchGroup Pre -> CheckAction (PmMatchGroup Post)
checkMatchGroup (PmMatchGroup matches) =
  PmMatchGroup <$> checkSequence checkMatch matches

checkMatch :: PmMatch Pre -> CheckAction (PmMatch Post)
checkMatch (PmMatch { pm_pats = grds, pm_grhss = grhss }) =
  leftToRight PmMatch (checkGrds grds) (checkGRHSs grhss)

checkGRHSs :: NonEmpty (PmGRHS Pre) -> CheckAction (NonEmpty (PmGRHS Post))
checkGRHSs = checkSequence checkGRHS

checkGRHS :: PmGRHS Pre -> CheckAction (PmGRHS Post)
checkGRHS (PmGRHS { pg_grds = grds, pg_rhs = rhs_info }) =
  flip PmGRHS rhs_info <$> checkGrds grds

checkEmptyCase :: PmEmptyCase -> CheckAction PmEmptyCase
checkEmptyCase pe@(PmEmptyCase { pe_var = var }) = CA $ \inc -> do
  unc <- addPmCtNablas inc (PmNotBotCt var)
  pure CheckResult { cr_ret = pe, cr_uncov = unc, cr_approx = mempty }

checkPatBind :: (PmPatBind Pre) -> CheckAction (PmPatBind Post)
checkPatBind = coerce checkGRHS

{- Note [Countering exponential blowup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Precise pattern match exhaustiveness checking is necessarily exponential in
the size of some input programs. We implement a counter-measure in the form of
the -fmax-pmcheck-models flag, limiting the number of Nablas we check against
each pattern by a constant.

How do we do that? Consider

  f True True = ()
  f True True = ()

And imagine we set our limit to 1 for the sake of the example. The first clause
will be checked against the initial Nabla, {}. Doing so will produce an
Uncovered set of size 2, containing the models {x≁True} and {x~True,y≁True}.
Also we find the first clause to cover the model {x~True,y~True}.

But the Uncovered set we get out of the match is too huge! We somehow have to
ensure not to make things worse as they are already, so we continue checking
with a singleton Uncovered set of the initial Nabla {}. Why is this
sound (wrt. the notion in GADTs Meet Their Match)? Well, it basically amounts
to forgetting that we matched against the first clause. The values represented
by {} are a superset of those represented by its two refinements {x≁True} and
{x~True,y≁True}.

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
-}

--
-- * Collecting long-distance information
--

ldiMatchGroup :: PmMatchGroup Post -> NonEmpty (Nablas, NonEmpty Nablas)
ldiMatchGroup (PmMatchGroup matches) = ldiMatch <$> matches

ldiMatch :: PmMatch Post -> (Nablas, NonEmpty Nablas)
ldiMatch (PmMatch { pm_pats = red, pm_grhss = grhss }) =
  (rs_cov red, ldiGRHS <$> grhss)

ldiGRHS :: PmGRHS Post -> Nablas
ldiGRHS (PmGRHS { pg_grds = red }) = rs_cov red

--
-- * Collecting redundancy information
--

-- | The result of redundancy checking:
--    * RHSs classified as /C/overed, /I/naccessible and /R/edundant
--    * And redundant /B/ang patterns. See Note [Dead bang patterns].
data CIRB
  = CIRB
  { cirb_cov   :: !(OrdList SrcInfo) -- ^ Covered clauses
  , cirb_inacc :: !(OrdList SrcInfo) -- ^ Inaccessible clauses
  , cirb_red   :: !(OrdList SrcInfo) -- ^ Redundant clauses
  , cirb_bangs :: !(OrdList SrcInfo) -- ^ Redundant bang patterns
  }

instance Semigroup CIRB where
  CIRB a b c d <> CIRB e f g h = CIRB (a <> e) (b <> f) (c <> g) (d <> h)
    where (<>) = (Semi.<>)

instance Monoid CIRB where
  mempty = CIRB mempty mempty mempty mempty

markAllRedundant :: CIRB -> CIRB
markAllRedundant CIRB { cirb_cov = cov, cirb_inacc = inacc, cirb_red = red } =
  mempty { cirb_red = cov Semi.<> inacc Semi.<> red }

-- See Note [Determining inaccessible clauses]
ensureOneNotRedundant :: CIRB -> CIRB
ensureOneNotRedundant ci = case ci of
  CIRB { cirb_cov = NilOL, cirb_inacc = NilOL, cirb_red = ConsOL r rs }
    -> ci { cirb_inacc = unitOL r, cirb_red = rs }
  _ -> ci

-- | Only adds the redundant bangs to the @CIRB@ if there is at least one
-- non-redundant 'SrcInfo'. There is no point in remembering a redundant bang
-- if the whole match is redundant!
addRedundantBangs :: OrdList SrcInfo -> CIRB -> CIRB
addRedundantBangs _red_bangs cirb@CIRB { cirb_cov = NilOL, cirb_inacc = NilOL } =
  cirb
addRedundantBangs red_bangs  cirb =
  cirb { cirb_bangs = cirb_bangs cirb Semi.<> red_bangs }

-- | Checks the 'Nablas' in a 'RedSets' for inhabitants and returns
--    1. Whether the Covered set was inhabited
--    2. Whether the Diverging set was inhabited
--    3. All source bangs whose 'Nablas' were empty, which means they are
--       redundant.
testRedSets :: RedSets -> DsM (Bool, Bool, OrdList SrcInfo)
testRedSets RedSets { rs_cov = cov, rs_div = div, rs_bangs = bangs } = do
  is_covered  <- isInhabited cov
  may_diverge <- isInhabited div
  red_bangs   <- flip mapMaybeM (fromOL bangs) $ \(nablas, bang) -> do
    isInhabited nablas >>= \case
      True  -> pure Nothing
      False -> pure (Just bang)
  pure (is_covered, may_diverge, toOL red_bangs)

cirbsMatchGroup :: PmMatchGroup Post -> DsM CIRB
cirbsMatchGroup (PmMatchGroup matches) =
  Semi.sconcat <$> traverse cirbsMatch matches

cirbsMatch :: PmMatch Post -> DsM CIRB
cirbsMatch PmMatch { pm_pats = red, pm_grhss = grhss } = do
  (is_covered, may_diverge, red_bangs) <- testRedSets red
  cirb <- cirbsGRHSs grhss
  pure $ addRedundantBangs red_bangs
       -- See Note [Determining inaccessible clauses]
       $ applyWhen may_diverge ensureOneNotRedundant
       $ applyWhen (not is_covered) markAllRedundant
       $ cirb

cirbsGRHSs :: NonEmpty (PmGRHS Post) -> DsM CIRB
cirbsGRHSs grhss = Semi.sconcat <$> traverse cirbsGRHS grhss

cirbsGRHS :: PmGRHS Post -> DsM CIRB
cirbsGRHS PmGRHS { pg_grds = red, pg_rhs = info } = do
  (is_covered, may_diverge, red_bangs) <- testRedSets red
  let cirb | is_covered  = mempty { cirb_cov   = unitOL info }
           | may_diverge = mempty { cirb_inacc = unitOL info }
           | otherwise   = mempty { cirb_red   = unitOL info }
  pure (addRedundantBangs red_bangs cirb)

cirbsEmptyCase :: PmEmptyCase -> DsM CIRB
cirbsEmptyCase _ = pure mempty

cirbsPatBind :: PmPatBind Post -> DsM CIRB
cirbsPatBind = coerce cirbsGRHS

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
-}

--
-- * Formatting and reporting warnings
--

-- | Given a function that collects 'CIRB's, this function will emit warnings
-- for a 'CheckResult'.
formatReportWarnings :: (ann -> DsM CIRB) -> DsMatchContext -> [Id] -> CheckResult ann -> DsM ()
formatReportWarnings collect ctx vars cr@CheckResult { cr_ret = ann } = do
  cov_info <- collect ann
  dflags <- getDynFlags
  reportWarnings dflags ctx vars cr{cr_ret=cov_info}

-- | Issue all the warnings
-- (redundancy, inaccessibility, exhaustiveness, redundant bangs).
reportWarnings :: DynFlags -> DsMatchContext -> [Id] -> CheckResult CIRB -> DsM ()
reportWarnings dflags ctx@(DsMatchContext kind loc) vars
  CheckResult { cr_ret    = CIRB { cirb_inacc = inaccessible_rhss
                                 , cirb_red   = redundant_rhss
                                 , cirb_bangs = redundant_bangs }
              , cr_uncov  = uncovered
              , cr_approx = precision }
  = when (flag_i || flag_u || flag_b) $ do
      unc_examples <- getNFirstUncovered vars (maxPatterns + 1) uncovered
      let exists_r = flag_i && notNull redundant_rhss
          exists_i = flag_i && notNull inaccessible_rhss
          exists_u = flag_u && notNull unc_examples
          exists_b = flag_b && notNull redundant_bangs
          approx   = precision == Approximate

      when (approx && (exists_u || exists_i)) $
        putSrcSpanDs loc (warnDs NoReason approx_msg)

      when exists_b $ forM_ redundant_bangs $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnRedundantBangPatterns)
                               (pprEqn q "has redundant bang"))

      when exists_r $ forM_ redundant_rhss $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "is redundant"))
      when exists_i $ forM_ inaccessible_rhss $ \(L l q) -> do
        putSrcSpanDs l (warnDs (Reason Opt_WarnOverlappingPatterns)
                               (pprEqn q "has inaccessible right hand side"))

      when exists_u $ putSrcSpanDs loc $ warnDs flag_u_reason $
        pprEqns vars unc_examples
  where
    flag_i = overlapping dflags kind
    flag_u = exhaustive dflags kind
    flag_b = redundant_bang dflags
    flag_u_reason = maybe NoReason Reason (exhaustiveWarningFlag kind)

    maxPatterns = maxUncoveredPatterns dflags

    -- Print a single clause (for redundant/with-inaccessible-rhs)
    pprEqn q txt = pprContext True ctx (text txt) $ \f ->
      f (q <+> matchSeparator kind <+> text "...")

    -- Print several clauses (for uncovered clauses)
    pprEqns vars nablas = pprContext False ctx (text "are non-exhaustive") $ \_ ->
      case vars of -- See #11245
           [] -> text "Guards do not cover entire pattern space"
           _  -> let us = map (\nabla -> pprUncovered nabla vars) nablas
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

getNFirstUncovered :: [Id] -> Int -> Nablas -> DsM [Nabla]
getNFirstUncovered vars n (MkNablas nablas) = go n (bagToList nablas)
  where
    go 0 _              = pure []
    go _ []             = pure []
    go n (nabla:nablas) = do
      front <- provideEvidence vars n nabla
      back <- go (n - length front) nablas
      pure (front ++ back)

dots :: Int -> [a] -> SDoc
dots maxPatterns qs
    | qs `lengthExceeds` maxPatterns = text "..."
    | otherwise                      = empty

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

--
-- * Utilities
--

-- | All warning flags that need to run the pattern match checker.
allPmCheckWarnings :: [WarningFlag]
allPmCheckWarnings =
  [ Opt_WarnIncompletePatterns
  , Opt_WarnIncompleteUniPatterns
  , Opt_WarnIncompletePatternsRecUpd
  , Opt_WarnOverlappingPatterns
  ]

-- | Check whether the redundancy checker should run (redundancy only)
overlapping :: DynFlags -> HsMatchContext id -> Bool
-- See Note [Inaccessible warnings for record updates]
overlapping _      RecUpd = False
overlapping dflags _      = wopt Opt_WarnOverlappingPatterns dflags

-- | Check whether the exhaustiveness checker should run (exhaustiveness only)
exhaustive :: DynFlags -> HsMatchContext id -> Bool
exhaustive  dflags = maybe False (`wopt` dflags) . exhaustiveWarningFlag

-- | Check whether unnecessary bangs should be warned about
redundant_bang :: DynFlags -> Bool
redundant_bang dflags = wopt Opt_WarnRedundantBangPatterns dflags

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
-- Don't warn about incomplete patterns in list comprehensions, pattern guards
-- etc. They are often *supposed* to be incomplete
exhaustiveWarningFlag (StmtCtxt {}) = Nothing

-- | Check whether any part of pattern match checking is enabled for this
-- 'HsMatchContext' (does not matter whether it is the redundancy check or the
-- exhaustiveness check).
isMatchContextPmChecked :: DynFlags -> Origin -> HsMatchContext id -> Bool
isMatchContextPmChecked dflags origin kind
  | isGenerated origin
  = False
  | otherwise
  = overlapping dflags kind || exhaustive dflags kind

-- | Return True when any of the pattern match warnings ('allPmCheckWarnings')
-- are enabled, in which case we need to run the pattern match checker.
needToRunPmCheck :: DynFlags -> Origin -> Bool
needToRunPmCheck dflags origin
  | isGenerated origin
  = False
  | otherwise
  = notNull (filter (`wopt` dflags) allPmCheckWarnings)

{- Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12957)
  data T a where
    T1 :: { x :: Int } -> T Bool
    T2 :: { x :: Int } -> T a
    T3 :: T a

  f :: T Char -> T a
  f r = r { x = 3 }

The desugarer will conservatively generate a case for T1 even though
it's impossible:
  f r = case r of
          T1 x -> T1 3   -- Inaccessible branch
          T2 x -> T2 3
          _    -> error "Missing"

We don't want to warn about the inaccessible branch because the programmer
didn't put it there!  So we filter out the warning here.

The same can happen for long distance term constraints instead of type
constraints (#17783):

  data T = A { x :: Int } | B { x :: Int }
  f r@A{} = r { x = 3 }
  f _     = B 0

Here, the long distance info from the FunRhs match (@r ~ A x@) will make the
clause matching on @B@ of the desugaring to @case@ redundant. It's generated
code that we don't want to warn about.
-}

--
-- * Long-distance information
--

-- | Locally update 'dsl_nablas' with the given action, but defer evaluation
-- with 'unsafeInterleaveM' in order not to do unnecessary work.
locallyExtendPmNablas :: (Nablas -> DsM Nablas) -> DsM a -> DsM a
locallyExtendPmNablas ext k = do
  nablas <- getLdiNablas
  nablas' <- unsafeInterleaveM $ ext nablas
  updPmNablas nablas' k

-- | Add in-scope type constraints if the coverage checker might run and then
-- run the given action.
addTyCs :: Origin -> Bag EvVar -> DsM a -> DsM a
addTyCs origin ev_vars m = do
  dflags <- getDynFlags
  applyWhen (needToRunPmCheck dflags origin)
            (locallyExtendPmNablas (\nablas -> addPmCtsNablas nablas (PmTyCt . evVarPred <$> ev_vars)))
            m

-- | Add equalities for the 'CoreExpr' scrutinee to the local 'DsM' environment
-- when checking a case expression:
--     case e of x { matches }
-- When checking matches we record that (x ~ e) where x is the initial
-- uncovered. All matches will have to satisfy this equality.
addCoreScrutTmCs :: Maybe CoreExpr -> [Id] -> DsM a -> DsM a
addCoreScrutTmCs Nothing    _   k = k
addCoreScrutTmCs (Just scr) [x] k =
  flip locallyExtendPmNablas k $ \nablas ->
    addPmCtsNablas nablas (unitBag (PmCoreCt x scr))
addCoreScrutTmCs _   _   _ = panic "addCoreScrutTmCs: scrutinee, but more than one match id"

-- | 'addCoreScrutTmCs', but desugars the 'LHsExpr' first.
addHsScrutTmCs :: Maybe (LHsExpr GhcTc) -> [Id] -> DsM a -> DsM a
addHsScrutTmCs Nothing    _    k = k
addHsScrutTmCs (Just scr) vars k = do
  scr_e <- dsLExpr scr
  addCoreScrutTmCs (Just scr_e) vars k

{- Note [Long-distance information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data Color = R | G | B
  f :: Color -> Int
  f R = …
  f c = … (case c of
          G -> True
          B -> False) …

Humans can make the "long-distance connection" between the outer pattern match
and the nested case pattern match to see that the inner pattern match is
exhaustive: @c@ can't be @R@ anymore because it was matched in the first clause
of @f@.

To achieve similar reasoning in the coverage checker, we keep track of the set
of values that can reach a particular program point (often loosely referred to
as "Covered set") in 'GHC.HsToCore.Monad.dsl_nablas'.
We fill that set with Covered Nablas returned by the exported checking
functions, which the call sites put into place with
'GHC.HsToCore.Monad.updPmNablas'.
Call sites also extend this set with facts from type-constraint dictionaries,
case scrutinees, etc. with the exported functions 'addTyCs', 'addCoreScrutTmCs'
and 'addHsScrutTmCs'.

Note [Recovering from unsatisfiable pattern-matching constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following code (see #12957 and #15450):

  f :: Int ~ Bool => ()
  f = case True of { False -> () }

We want to warn that the pattern-matching in `f` is non-exhaustive. But GHC
used not to do this; in fact, it would warn that the match was /redundant/!
This is because the constraint (Int ~ Bool) in `f` is unsatisfiable, and the
coverage checker deems any matches with unsatisfiable constraint sets to be
unreachable.

We make sure to always start from an inhabited 'Nablas' by calling
'getLdiNablas', which falls back to the trivially inhabited 'Nablas' if the
long-distance info returned by 'GHC.HsToCore.Monad.getPmNablas' is empty.
-}
