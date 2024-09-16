
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

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
-- the entry points such as 'pmcMatches':
--
--  1. Desugar source syntax (like 'LMatch') to guard tree variants (like
--     'GrdMatch'), with one of the desugaring functions (like 'desugarMatch').
--     See "GHC.HsToCore.Pmc.Desugar".
--     Follows Section 3.1 in the paper.
--  2. Coverage check guard trees (with a function like 'checkMatch') to get a
--     'CheckResult'. See "GHC.HsToCore.Pmc.Check".
--     The normalised refinement types 'Nabla' are tested for inhabitants by
--     "GHC.HsToCore.Pmc.Solver".
--  3. Collect redundancy information into a 'CIRB' with a function such
--     as 'cirbsMatch'. Follows the R function from Figure 6 of the paper.
--  4. Format and report uncovered patterns and redundant equations ('CIRB')
--     with 'formatReportWarnings'. Basically job of the G function, plus proper
--     pretty printing of the warnings (Section 5.4 of the paper).
--  5. Return 'Nablas' reaching syntactic sub-components for
--     Note [Long-distance information]. Collected by functions such as
--     'ldiMatch'. See Section 4.1 of the paper.
module GHC.HsToCore.Pmc (
        -- Checking and printing
        pmcPatBind, pmcMatches, pmcGRHSs, pmcRecSel,
        isMatchContextPmChecked, isMatchContextPmChecked_SinglePat,

        -- See Note [Long-distance information]
        addTyCs, addCoreScrutTmCs, addHsScrutTmCs, getLdiNablas,
        getNFirstUncovered
    ) where

import GHC.Prelude

import GHC.HsToCore.Errors.Types
import GHC.HsToCore.Pmc.Types
import GHC.HsToCore.Pmc.Utils
import GHC.HsToCore.Pmc.Desugar
import GHC.HsToCore.Pmc.Check
import GHC.HsToCore.Pmc.Solver
import GHC.Types.Basic (Origin(..), isDoExpansionGenerated)
import GHC.Core
import GHC.Driver.DynFlags
import GHC.Hs
import GHC.Types.Id
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Var (EvVar, Var (..))
import GHC.Types.Id.Info
import GHC.Tc.Utils.TcType (evVarPred)
import {-# SOURCE #-} GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad
import GHC.Data.Bag
import GHC.Data.OrdList

import Control.Monad (when, unless, forM_)
import qualified Data.Semigroup as Semi
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Coerce
import GHC.Tc.Utils.Monad

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

-- | We need to call the Hs desugarer to get the Core of a let-binding or where
-- clause. We don't want to run the coverage checker when doing so! Efficiency
-- is one concern, but also a lack of properly set up long-distance information
-- might trigger warnings that we normally wouldn't emit.
noCheckDs :: DsM a -> DsM a
noCheckDs = updTopFlags (\dflags -> foldl' wopt_unset dflags allPmCheckWarnings)

-- | Check a pattern binding (let, where) for exhaustiveness.
pmcPatBind :: DsMatchContext -> Id -> Pat GhcTc -> DsM Nablas
pmcPatBind ctxt@(DsMatchContext match_ctxt loc) var p
  = mb_discard_warnings $ do
      !missing <- getLdiNablas
      pat_bind <- noCheckDs $ desugarPatBind loc var p
      tracePm "pmcPatBind {" (vcat [ppr ctxt, ppr var, ppr p, ppr pat_bind, ppr missing])
      result <- unCA (checkPatBind pat_bind) missing
      let ldi = ldiGRHS $ ( \ pb -> case pb of PmPatBind grhs -> grhs) $ cr_ret result
      tracePm "pmcPatBind }: " $
        vcat [ text "cr_uncov:" <+> ppr (cr_uncov result)
             , text "ldi:" <+> ppr ldi ]
      formatReportWarnings ReportPatBind ctxt [var] result
      return ldi
  where
    -- See Note [pmcPatBind doesn't warn on pattern guards]
    mb_discard_warnings
      = if want_pmc match_ctxt
        then id
        else discardWarningsDs
    want_pmc PatBindRhs = True
    want_pmc LazyPatCtx = True
    want_pmc (StmtCtxt stmt_ctxt) =
      case stmt_ctxt of
        PatGuard {} -> False
        _           -> True
    want_pmc _ = False

-- | Exhaustive for guard matches, is used for guards in pattern bindings and
-- in @MultiIf@ expressions. Returns the 'Nablas' covered by the RHSs.
pmcGRHSs
  :: HsMatchContextRn             -- ^ Match context, for warning messages
  -> GRHSs GhcTc (LHsExpr GhcTc)  -- ^ The GRHSs to check
  -> DsM (NonEmpty Nablas)        -- ^ Covered 'Nablas' for each RHS, for long
                                  --   distance info
pmcGRHSs hs_ctxt guards@(GRHSs _ grhss _) = do
  let combined_loc = foldl1 combineSrcSpans (map getLocA grhss)
      ctxt = DsMatchContext hs_ctxt combined_loc
  !missing <- getLdiNablas
  matches  <- noCheckDs $ desugarGRHSs combined_loc empty guards
  tracePm "pmcGRHSs" (hang (vcat [ppr ctxt
                                , text "Guards:"])
                                2
                                (pprGRHSs hs_ctxt guards $$ ppr missing))
  result <- unCA (checkGRHSs matches) missing
  tracePm "}: " (ppr (cr_uncov result))
  formatReportWarnings ReportGRHSs ctxt [] result
  return (ldiGRHSs (cr_ret result))

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
-- Special case: When there are /no matches/, then the function assumes it
-- checks an @-XEmptyCase@ with only a single match variable.
-- See Note [Checking EmptyCase].
pmcMatches
  :: Origin
  -> DsMatchContext                  -- ^ Match context, for warnings messages
  -> [Id]                            -- ^ Match variables, i.e. x and y above
  -> [LMatch GhcTc (LHsExpr GhcTc)]  -- ^ List of matches
  -> DsM [(Nablas, NonEmpty Nablas)] -- ^ One covered 'Nablas' per Match and
                                     --   GRHS, for long distance info.
pmcMatches origin ctxt vars matches = {-# SCC "pmcMatches" #-} do
  -- We have to force @missing@ before printing out the trace message,
  -- otherwise we get interleaved output from the solver. This function
  -- should be strict in @missing@ anyway!
  !missing <- getLdiNablas
  tracePm "pmcMatches {" $
          hang (vcat [ppr origin, ppr ctxt, ppr vars, text "Matches:"])
               2
               ((ppr matches) $$ (text "missing:" <+> ppr missing))
  case NE.nonEmpty matches of
    Nothing -> do
      -- This must be an -XEmptyCase. See Note [Checking EmptyCase]
      let var = only vars
      empty_case <- noCheckDs $ desugarEmptyCase var
      result <- unCA (checkEmptyCase empty_case) missing
      tracePm "}: " (ppr (cr_uncov result))
      formatReportWarnings ReportEmptyCase ctxt vars result
      return []
    Just matches -> do
      matches <- {-# SCC "desugarMatches" #-}
                 noCheckDs $ desugarMatches vars matches
      tracePm "desugared matches" (ppr matches)
      result  <- {-# SCC "checkMatchGroup" #-}
                 unCA (checkMatchGroup matches) missing
      tracePm "}: " (ppr (cr_uncov result))
      unless (isDoExpansionGenerated origin) -- Do expansion generated code shouldn't emit overlapping warnings
        ({-# SCC "formatReportWarnings" #-}
        formatReportWarnings ReportMatchGroup ctxt vars result)
      return (NE.toList (ldiMatchGroup (cr_ret result)))

{-
Note [Detecting incomplete record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A record selector occurrence is incomplete iff. it could fail due to
being applied to a data type constructor not present for this record field.

e.g.
  data T = T1 | T2 {x :: Int}
  d = x someComputation -- `d` may fail

There are 4 parts to detecting and warning about
incomplete record selectors to consider:

  - Computing which constructors a general application of a record field will succeed on,
    and which ones it will fail on. This is stored in the `sel_cons` field of
    `IdDetails` datatype, which is a part of an `Id` and calculated when renaming a
    record selector in `mkOneRecordSelector`

  - Emitting a warning whenever a `HasField` constraint is solved.
    This is checked in `matchHasField` and emitted only for when
    the constraint is resolved with an implicit instance rather than a
    custom one (since otherwise the warning will be emitted in
      the custom implementation anyways)

    e.g.
      g :: HasField "x" t Int => t -> Int
      g = getField @"x"

      f :: T -> Int
      f = g -- warning will be emitted here

  - Emitting a warning for a general occurrence of the record selector
    This is done during the renaming of a `HsRecSel` expression in `dsExpr`
    and simply pulls the information about incompleteness from the `Id`

    e.g.
      l :: T -> Int
      l a = x a -- warning will be emitted here

  - Emitting a warning for a record selector `sel` applied to a variable `y`.
    In that case we want to use the long-distance information from the
    pattern match checker to rule out impossible constructors
    (See Note [Long-distance information]). We first add constraints to
    the long-distance `Nablas` that `y` cannot be one of the constructors that
    contain `sel` (function `checkRecSel` in GHC.HsToCore.Pmc.Check). If the
    `Nablas` are still inhabited, we emit a warning with the inhabiting constructors
    as examples of where `sel` may fail.

    e.g.
      z :: T -> Int
      z T1 = 0
      z a = x a -- warning will not be emitted here since `a` can only be `T2`
-}

pmcRecSel :: Id       -- ^ Id of the selector
          -> CoreExpr -- ^ Core expression of the argument to the selector
          -> DsM ()
pmcRecSel sel_id arg
  | RecSelId{ sel_cons = (cons_w_field, _ : _) } <- idDetails sel_id = do
      !missing <- getLdiNablas

      tracePm "pmcRecSel {" (ppr sel_id)
      CheckResult{ cr_ret = PmRecSel{ pr_arg_var = arg_id }, cr_uncov = uncov_nablas }
        <- unCA (checkRecSel (PmRecSel () arg cons_w_field)) missing
      tracePm "}: " $ ppr uncov_nablas

      inhabited <- isInhabited uncov_nablas
      when inhabited $ warn_incomplete arg_id uncov_nablas
        where
          sel_name = varName sel_id
          warn_incomplete arg_id uncov_nablas = do
            dflags <- getDynFlags
            let maxConstructors = maxUncoveredPatterns dflags
            unc_examples <- getNFirstUncovered MinimalCover [arg_id] (maxConstructors + 1) uncov_nablas
            let cons = [con | unc_example <- unc_examples
                      , Just (PACA (PmAltConLike con) _ _) <- [lookupSolution unc_example arg_id]]
                not_full_examples = length cons == (maxConstructors + 1)
                cons' = take maxConstructors cons
            diagnosticDs $ DsIncompleteRecordSelector sel_name cons' not_full_examples

pmcRecSel _ _ = return ()

{- Note [pmcPatBind doesn't warn on pattern guards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@pmcPatBind@'s main purpose is to check vanilla pattern bindings, like
@x :: Int; Just x = e@, which is in a @PatBindRhs@ context.
But its caller is also called for individual pattern guards in a @StmtCtxt@.
For example, both pattern guards in @f x y | True <- x, False <- y = ...@ will
go through this function. It makes no sense to report pattern match warnings
for these pattern guards:

  * Pattern guards may well fail. Fall-through is not an unrecoverable panic,
    but rather behavior the programmer expects, so inexhaustivity should not be
    reported.

  * Redundancy is already reported for the whole GRHS via one of the other
    exported coverage checking functions. Also, reporting individual redundant
    guards is... redundant. See #17646.

However, we should not skip pattern-match checking altogether, as it may reveal
important long-distance information. One example is described in
Note [Long-distance information in do notation] in GHC.HsToCore.Expr.

Instead, we simply discard warnings when in pattern-guards, by using the function
discardWarningsDs.
-}

--
-- * Collecting long-distance information
--

ldiMatchGroup :: PmMatchGroup Post -> NonEmpty (Nablas, NonEmpty Nablas)
ldiMatchGroup (PmMatchGroup matches) = ldiMatch <$> matches

ldiMatch :: PmMatch Post -> (Nablas, NonEmpty Nablas)
ldiMatch (PmMatch { pm_pats = red, pm_grhss = grhss }) =
  (rs_cov red, ldiGRHSs grhss)

ldiGRHSs :: PmGRHSs Post -> NonEmpty Nablas
ldiGRHSs (PmGRHSs { pgs_grhss = grhss }) = ldiGRHS <$> grhss

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
  red_bangs   <- flip mapMaybeM (fromOL bangs) $ \(nablas, bang) ->
    isInhabited nablas >>= \case
      True  -> pure Nothing
      False -> pure (Just bang)
  pure (is_covered, may_diverge, toOL red_bangs)

cirbsMatchGroup :: PmMatchGroup Post -> DsM CIRB
cirbsMatchGroup (PmMatchGroup matches) =
  Semi.sconcat <$> traverse cirbsMatch matches

cirbsMatch :: PmMatch Post -> DsM CIRB
cirbsMatch PmMatch { pm_pats = red, pm_grhss = grhss } = do
  (_is_covered, may_diverge, red_bangs) <- testRedSets red
  -- Don't look at is_covered: If it is True, all children are redundant anyway,
  -- unless there is a 'considerAccessible', which may break that rule
  -- intentionally. See Note [considerAccessible] in "GHC.HsToCore.Pmc.Check".
  cirb <- cirbsGRHSs grhss
  pure $ addRedundantBangs red_bangs
       -- See Note [Determining inaccessible clauses]
       $ applyWhen may_diverge ensureOneNotRedundant
       $ cirb

cirbsGRHSs :: PmGRHSs Post -> DsM CIRB
cirbsGRHSs (PmGRHSs { pgs_grhss = grhss }) = Semi.sconcat <$> traverse cirbsGRHS grhss

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

-- | A datatype to accommodate the different call sites of
-- 'formatReportWarnings'. Used for extracting 'CIRB's from a concrete 'ann'
-- through 'collectInMode'. Since this is only possible for a couple of
-- well-known 'ann's, this is a GADT.
data FormatReportWarningsMode ann where
  ReportPatBind :: FormatReportWarningsMode (PmPatBind Post)
  ReportGRHSs   :: FormatReportWarningsMode (PmGRHSs Post)
  ReportMatchGroup:: FormatReportWarningsMode (PmMatchGroup Post)
  ReportEmptyCase:: FormatReportWarningsMode PmEmptyCase

deriving instance Eq (FormatReportWarningsMode ann)

-- | A function collecting 'CIRB's for each of the different
-- 'FormatReportWarningsMode's.
collectInMode :: FormatReportWarningsMode ann -> ann -> DsM CIRB
collectInMode ReportPatBind    = cirbsPatBind
collectInMode ReportGRHSs      = cirbsGRHSs
collectInMode ReportMatchGroup = cirbsMatchGroup
collectInMode ReportEmptyCase  = cirbsEmptyCase

-- | Given a 'FormatReportWarningsMode', this function will emit warnings
-- for a 'CheckResult'.
formatReportWarnings :: FormatReportWarningsMode ann -> DsMatchContext -> [Id] -> CheckResult ann -> DsM ()
formatReportWarnings report_mode ctx vars cr@CheckResult { cr_ret = ann } = do
  cov_info <- collectInMode report_mode ann
  dflags <- getDynFlags
  reportWarnings dflags report_mode ctx vars cr{cr_ret=cov_info}

-- | Issue all the warnings
-- (redundancy, inaccessibility, exhaustiveness, redundant bangs).
reportWarnings :: DynFlags -> FormatReportWarningsMode ann -> DsMatchContext -> [Id] -> CheckResult CIRB -> DsM ()
reportWarnings dflags report_mode (DsMatchContext kind loc) vars
  CheckResult { cr_ret    = CIRB { cirb_inacc = inaccessible_rhss
                                 , cirb_red   = redundant_rhss
                                 , cirb_bangs = redundant_bangs }
              , cr_uncov  = uncovered
              , cr_approx = precision }
  = when (flag_i || flag_u || flag_b) $ do
      unc_examples <- getNFirstUncovered gen_mode vars (maxPatterns + 1) uncovered
      let exists_r = flag_i && notNull redundant_rhss
          exists_i = flag_i && notNull inaccessible_rhss
          exists_u = flag_u && notNull unc_examples
          exists_b = flag_b && notNull redundant_bangs
          approx   = precision == Approximate

      when (approx && (exists_u || exists_i)) $
        putSrcSpanDs loc (diagnosticDs (DsMaxPmCheckModelsReached (maxPmCheckModels dflags)))

      when exists_b $ forM_ redundant_bangs $ \(SrcInfo (L l q)) ->
        putSrcSpanDs l (diagnosticDs (DsRedundantBangPatterns kind q))

      when exists_r $ forM_ redundant_rhss $ \(SrcInfo (L l q)) ->
        putSrcSpanDs l (diagnosticDs (DsOverlappingPatterns kind q))
      when exists_i $ forM_ inaccessible_rhss $ \(SrcInfo (L l q)) ->
        putSrcSpanDs l (diagnosticDs (DsInaccessibleRhs kind q))

      when exists_u $
        putSrcSpanDs loc (diagnosticDs (DsNonExhaustivePatterns kind check_type maxPatterns vars unc_examples))
  where
    flag_i = overlapping dflags kind
    flag_u = exhaustive dflags kind
    flag_b = redundantBang dflags
    check_type = ExhaustivityCheckType (exhaustiveWarningFlag kind)
    gen_mode = case report_mode of -- See Note [Case split inhabiting patterns]
      ReportEmptyCase -> CaseSplitTopLevel
      _               -> MinimalCover

    maxPatterns = maxUncoveredPatterns dflags

getNFirstUncovered :: GenerateInhabitingPatternsMode -> [Id] -> Int -> Nablas -> DsM [Nabla]
getNFirstUncovered mode vars n (MkNablas nablas) = go n (bagToList nablas)
  where
    go 0 _              = pure []
    go _ []             = pure []
    go n (nabla:nablas) = do
      front <- generateInhabitingPatterns mode vars n nabla
      back <- go (n - length front) nablas
      pure (front ++ back)

--
-- * Adding external long-distance information
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
            (locallyExtendPmNablas $ \nablas ->
              addPhiCtsNablas nablas (PhiTyCt . evVarPred <$> ev_vars))
            m

-- | Add equalities for the 'CoreExpr' scrutinees to the local 'DsM' environment,
-- e.g. when checking a case expression:
--     case e of x { matches }
-- When checking matches we record that (x ~ e) where x is the initial
-- uncovered. All matches will have to satisfy this equality.
-- This is also used for the Arrows \cases command, where these equalities have
-- to be added for multiple scrutinees rather than just one.
addCoreScrutTmCs :: [CoreExpr] -> [Id] -> DsM a -> DsM a
addCoreScrutTmCs []         _      k = k
addCoreScrutTmCs (scr:scrs) (x:xs) k =
  flip locallyExtendPmNablas (addCoreScrutTmCs scrs xs k) $ \nablas ->
    addPhiCtsNablas nablas (unitBag (PhiCoreCt x scr))
addCoreScrutTmCs _   _   _ = panic "addCoreScrutTmCs: numbers of scrutinees and match ids differ"

-- | 'addCoreScrutTmCs', but desugars the 'LHsExpr's first.
addHsScrutTmCs :: [LHsExpr GhcTc] -> [Id] -> DsM a -> DsM a
addHsScrutTmCs scrs vars k = do
  scr_es <- traverse dsLExpr scrs
  addCoreScrutTmCs scr_es vars k

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
