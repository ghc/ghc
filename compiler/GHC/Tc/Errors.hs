
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module GHC.Tc.Errors(
       reportUnsolved, reportAllUnsolved, warnAllUnsolved,
       warnDefaulting,

       -- * GHC API helper functions
       solverReportMsg_ExpectedActuals,
       solverReportInfo_ExpectedActuals
  ) where

import GHC.Prelude

import GHC.Driver.Env (hsc_units)
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Config.Diagnostic

import GHC.Rename.Unbound

import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Env( tcInitTidyEnv )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify ( checkTyVarEq )
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.EvTerm
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Instantiate
import {-# SOURCE #-} GHC.Tc.Errors.Hole ( findValidHoleFits, getHoleFitDispConfig, pprHoleFit )

import GHC.Types.Name
import GHC.Types.Name.Reader ( lookupGRE_Name, GlobalRdrEnv, mkRdrUnqual
                             , emptyLocalRdrEnv, lookupGlobalRdrEnv , lookupLocalRdrOcc )
import GHC.Types.Id
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Error
import qualified GHC.Types.Unique.Map as UM

--import GHC.Rename.Unbound ( unknownNameSuggestions, WhatLooking(..) )
import GHC.Unit.Module
import qualified GHC.LanguageExtensions as LangExt

import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCo.Ppr  ( pprTyVars
                           )
import GHC.Core.InstEnv
import GHC.Core.TyCon
import GHC.Core.DataCon

import GHC.Utils.Error  (diagReasonSeverity,  pprLocMsgEnvelope )
import GHC.Utils.Misc
import GHC.Utils.Outputable as O
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.FV ( fvVarList, unionFV )

import GHC.Data.Bag
import GHC.Data.List.SetOps ( equivClasses, nubOrdBy )
import GHC.Data.Maybe
import qualified GHC.Data.Strict as Strict

import Control.Monad    ( unless, when, foldM, forM_ )
import Data.Foldable    ( toList )
import Data.Functor     ( (<&>) )
import Data.Function    ( on )
import Data.List        ( partition, sort )
import Data.List.NonEmpty ( NonEmpty(..), (<|) )
import qualified Data.List.NonEmpty as NE ( map, reverse )
import Data.List        ( sortBy )
import Data.Ord         ( comparing )
import qualified Data.Semigroup as S

{-
************************************************************************
*                                                                      *
\section{Errors and contexts}
*                                                                      *
************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

Note [Deferring coercion errors to runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:

  module Main where

  a :: Int
  a = 'a'

  main = print "b"

Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.

Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in GHC.Tc.Utils.Unify, we normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into

  co :: Int ~ Char
  co = ...

  a :: Int
  a = 'a' `cast` co

The constraint solver would realize that `co` is an insoluble constraint, and
emit an error with `reportUnsolved`. But we can also replace the right-hand side
of `co` with `error "Deferred type error: Int ~ Char"`. This allows the program
to compile, and it will run fine unless we evaluate `a`. This is what
`deferErrorsToRuntime` does.

It does this by keeping track of which errors correspond to which coercion
in GHC.Tc.Errors. GHC.Tc.Errors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.
-}

-- | Report unsolved goals as errors or warnings. We may also turn some into
-- deferred run-time errors if `-fdefer-type-errors` is on.
reportUnsolved :: WantedConstraints -> TcM (Bag EvBind)
reportUnsolved wanted
  = do { binds_var <- newTcEvBinds
       ; defer_errors <- goptM Opt_DeferTypeErrors
       ; let type_errors | not defer_errors = ErrorWithoutFlag
                         | otherwise        = WarningWithFlag Opt_WarnDeferredTypeErrors

       ; defer_holes <- goptM Opt_DeferTypedHoles
       ; let expr_holes | not defer_holes = ErrorWithoutFlag
                        | otherwise       = WarningWithFlag Opt_WarnTypedHoles

       ; partial_sigs      <- xoptM LangExt.PartialTypeSignatures
       ; let type_holes | not partial_sigs
                        = ErrorWithoutFlag
                        | otherwise
                        = WarningWithFlag Opt_WarnPartialTypeSignatures

       ; defer_out_of_scope <- goptM Opt_DeferOutOfScopeVariables
       ; let out_of_scope_holes | not defer_out_of_scope
                                = ErrorWithoutFlag
                                | otherwise
                                = WarningWithFlag Opt_WarnDeferredOutOfScopeVariables

       ; report_unsolved type_errors expr_holes
                         type_holes out_of_scope_holes
                         binds_var wanted

       ; ev_binds <- getTcEvBindsMap binds_var
       ; return (evBindMapBinds ev_binds)}

-- | Report *all* unsolved goals as errors, even if -fdefer-type-errors is on
-- However, do not make any evidence bindings, because we don't
-- have any convenient place to put them.
-- NB: Type-level holes are OK, because there are no bindings.
-- See Note [Deferring coercion errors to runtime]
-- Used by solveEqualities for kind equalities
--      (see Note [Fail fast on kind errors] in "GHC.Tc.Solver")
reportAllUnsolved :: WantedConstraints -> TcM ()
reportAllUnsolved wanted
  = do { ev_binds <- newNoTcEvBinds

       ; partial_sigs      <- xoptM LangExt.PartialTypeSignatures
       ; let type_holes | not partial_sigs  = ErrorWithoutFlag
                        | otherwise         = WarningWithFlag Opt_WarnPartialTypeSignatures

       ; report_unsolved ErrorWithoutFlag
                         ErrorWithoutFlag type_holes ErrorWithoutFlag
                         ev_binds wanted }

-- | Report all unsolved goals as warnings (but without deferring any errors to
-- run-time). See Note [Safe Haskell Overlapping Instances Implementation] in
-- "GHC.Tc.Solver"
warnAllUnsolved :: WantedConstraints -> TcM ()
warnAllUnsolved wanted
  = do { ev_binds <- newTcEvBinds
       ; report_unsolved WarningWithoutFlag
                         WarningWithoutFlag
                         WarningWithoutFlag
                         WarningWithoutFlag
                         ev_binds wanted }

-- | Report unsolved goals as errors or warnings.
report_unsolved :: DiagnosticReason -- Deferred type errors
                -> DiagnosticReason -- Expression holes
                -> DiagnosticReason -- Type holes
                -> DiagnosticReason -- Out of scope holes
                -> EvBindsVar        -- cec_binds
                -> WantedConstraints -> TcM ()
report_unsolved type_errors expr_holes
    type_holes out_of_scope_holes binds_var wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do { traceTc "reportUnsolved {" $
         vcat [ text "type errors:" <+> ppr type_errors
              , text "expr holes:" <+> ppr expr_holes
              , text "type holes:" <+> ppr type_holes
              , text "scope holes:" <+> ppr out_of_scope_holes ]
       ; traceTc "reportUnsolved (before zonking and tidying)" (ppr wanted)

       ; wanted <- zonkWC wanted   -- Zonk to reveal all information

       ; let tidy_env = tidyFreeTyCoVars emptyTidyEnv free_tvs
             free_tvs = filterOut isCoVar $
                        tyCoVarsOfWCList wanted
                        -- tyCoVarsOfWC returns free coercion *holes*, even though
                        -- they are "bound" by other wanted constraints. They in
                        -- turn may mention variables bound further in, which makes
                        -- no sense. Really we should not return those holes at all;
                        -- for now we just filter them out.

       ; traceTc "reportUnsolved (after zonking):" $
         vcat [ text "Free tyvars:" <+> pprTyVars free_tvs
              , text "Tidy env:" <+> ppr tidy_env
              , text "Wanted:" <+> ppr wanted ]

       ; warn_redundant <- woptM Opt_WarnRedundantConstraints
       ; exp_syns <- goptM Opt_PrintExpandedSynonyms
       ; let err_ctxt = CEC { cec_encl  = []
                            , cec_tidy  = tidy_env
                            , cec_defer_type_errors = type_errors
                            , cec_expr_holes = expr_holes
                            , cec_type_holes = type_holes
                            , cec_out_of_scope_holes = out_of_scope_holes
                            , cec_suppress = insolubleWC wanted
                                 -- See Note [Suppressing error messages]
                                 -- Suppress low-priority errors if there
                                 -- are insoluble errors anywhere;
                                 -- See #15539 and c.f. setting ic_status
                                 -- in GHC.Tc.Solver.setImplicationStatus
                            , cec_warn_redundant = warn_redundant
                            , cec_expand_syns = exp_syns
                            , cec_binds    = binds_var }

       ; tc_lvl <- getTcLevel
       ; reportWanteds err_ctxt tc_lvl wanted
       ; traceTc "reportUnsolved }" empty }

--------------------------------------------
--      Internal functions
--------------------------------------------

-- | Make a report from a single 'TcSolverReportMsg'.
important :: SolverReportErrCtxt -> TcSolverReportMsg -> SolverReport
important ctxt doc = mempty { sr_important_msgs = [SolverReportWithCtxt ctxt doc] }

mk_relevant_bindings :: RelevantBindings -> SolverReport
mk_relevant_bindings binds = mempty { sr_supplementary = [SupplementaryBindings binds] }

mk_report_hints :: [GhcHint] -> SolverReport
mk_report_hints hints = mempty { sr_hints = hints }

-- | Returns True <=> the SolverReportErrCtxt indicates that something is deferred
deferringAnyBindings :: SolverReportErrCtxt -> Bool
  -- Don't check cec_type_holes, as these don't cause bindings to be deferred
deferringAnyBindings (CEC { cec_defer_type_errors  = ErrorWithoutFlag
                          , cec_expr_holes         = ErrorWithoutFlag
                          , cec_out_of_scope_holes = ErrorWithoutFlag }) = False
deferringAnyBindings _                                                   = True

maybeSwitchOffDefer :: EvBindsVar -> SolverReportErrCtxt -> SolverReportErrCtxt
-- Switch off defer-type-errors inside CoEvBindsVar
-- See Note [Failing equalities with no evidence bindings]
maybeSwitchOffDefer evb ctxt
 | CoEvBindsVar{} <- evb
 = ctxt { cec_defer_type_errors  = ErrorWithoutFlag
        , cec_expr_holes         = ErrorWithoutFlag
        , cec_out_of_scope_holes = ErrorWithoutFlag }
 | otherwise
 = ctxt

{- Note [Failing equalities with no evidence bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we go inside an implication that has no term evidence
(e.g. unifying under a forall), we can't defer type errors.  You could
imagine using the /enclosing/ bindings (in cec_binds), but that may
not have enough stuff in scope for the bindings to be well typed.  So
we just switch off deferred type errors altogether.  See #14605.

This is done by maybeSwitchOffDefer.  It's also useful in one other
place: see Note [Wrapping failing kind equalities] in GHC.Tc.Solver.

Note [Suppressing error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cec_suppress flag says "don't report any errors".  Instead, just create
evidence bindings (as usual).  It's used when more important errors have occurred.

Specifically (see reportWanteds)
  * If there are insoluble Givens, then we are in unreachable code and all bets
    are off.  So don't report any further errors.
  * If there are any insolubles (eg Int~Bool), here or in a nested implication,
    then suppress errors from the simple constraints here.  Sometimes the
    simple-constraint errors are a knock-on effect of the insolubles.

This suppression behaviour is controlled by the Bool flag in
ReportErrorSpec, as used in reportWanteds.

But we need to take care: flags can turn errors into warnings, and we
don't want those warnings to suppress subsequent errors (including
suppressing the essential addTcEvBind for them: #15152). So in
tryReporter we use askNoErrs to see if any error messages were
/actually/ produced; if not, we don't switch on suppression.

A consequence is that warnings never suppress warnings, so turning an
error into a warning may allow subsequent warnings to appear that were
previously suppressed.   (e.g. partial-sigs/should_fail/T14584)
-}

reportImplic :: SolverReportErrCtxt -> Implication -> TcM ()
reportImplic ctxt implic@(Implic { ic_skols  = tvs
                                 , ic_given  = given
                                 , ic_wanted = wanted, ic_binds = evb
                                 , ic_status = status, ic_info = info
                                 , ic_env    = tcl_env
                                 , ic_tclvl  = tc_lvl })
  | BracketSkol <- info
  , not insoluble
  = return ()        -- For Template Haskell brackets report only
                     -- definite errors. The whole thing will be re-checked
                     -- later when we plug it in, and meanwhile there may
                     -- certainly be un-satisfied constraints

  | otherwise
  = do { traceTc "reportImplic" $ vcat
           [ text "tidy env:"   <+> ppr (cec_tidy ctxt)
           , text "skols:     " <+> pprTyVars tvs
           , text "tidy skols:" <+> pprTyVars tvs' ]

       ; when bad_telescope $ reportBadTelescope ctxt tcl_env info tvs
               -- Do /not/ use the tidied tvs because then are in the
               -- wrong order, so tidying will rename things wrongly
       ; reportWanteds ctxt' tc_lvl wanted
       ; when (cec_warn_redundant ctxt) $
         warnRedundantConstraints ctxt' tcl_env info' dead_givens }
  where
    insoluble    = isInsolubleStatus status
    (env1, tvs') = tidyVarBndrs (cec_tidy ctxt) $
                   scopedSort tvs
        -- scopedSort: the ic_skols may not be in dependency order
        -- (see Note [Skolems in an implication] in GHC.Tc.Types.Constraint)
        -- but tidying goes wrong on out-of-order constraints;
        -- so we sort them here before tidying
    info'   = tidySkolemInfoAnon env1 info
    implic' = implic { ic_skols = tvs'
                     , ic_given = map (tidyEvVar env1) given
                     , ic_info  = info' }

    ctxt1 = maybeSwitchOffDefer evb ctxt
    ctxt' = ctxt1 { cec_tidy     = env1
                  , cec_encl     = implic' : cec_encl ctxt

                  , cec_suppress = insoluble || cec_suppress ctxt
                        -- Suppress inessential errors if there
                        -- are insolubles anywhere in the
                        -- tree rooted here, or we've come across
                        -- a suppress-worthy constraint higher up (#11541)

                  , cec_binds    = evb }

    dead_givens = case status of
                    IC_Solved { ics_dead = dead } -> dead
                    _                             -> []

    bad_telescope = case status of
              IC_BadTelescope -> True
              _               -> False

warnRedundantConstraints :: SolverReportErrCtxt -> TcLclEnv -> SkolemInfoAnon -> [EvVar] -> TcM ()
-- See Note [Tracking redundant constraints] in GHC.Tc.Solver
warnRedundantConstraints ctxt env info ev_vars
 | null redundant_evs
 = return ()

 | SigSkol user_ctxt _ _ <- info
 = restoreLclEnv env $  -- We want to add "In the type signature for f"
                        -- to the error context, which is a bit tiresome
   setSrcSpan (redundantConstraintsSpan user_ctxt) $
   report_redundant_msg True

 | otherwise  -- But for InstSkol there already *is* a surrounding
              -- "In the instance declaration for Eq [a]" context
              -- and we don't want to say it twice. Seems a bit ad-hoc
 = report_redundant_msg False
 where
   report_redundant_msg :: Bool -- ^ whether to add "In ..." to the diagnostic
                        -> TcRn ()
   report_redundant_msg show_info
     = do { lcl_env <- getLclEnv
          ; msg <-
              mkErrorReport
                lcl_env
                (TcRnRedundantConstraints redundant_evs (info, show_info))
                (Just ctxt)
                []
          ; reportDiagnostic msg }

   redundant_evs =
       filterOut is_type_error $
       case info of -- See Note [Redundant constraints in instance decls]
         InstSkol -> filterOut (improving . idType) ev_vars
         _        -> ev_vars

   -- See #15232
   is_type_error = isJust . userTypeError_maybe . idType

   improving pred -- (transSuperClasses p) does not include p
     = any isImprovementPred (pred : transSuperClasses pred)

reportBadTelescope :: SolverReportErrCtxt -> TcLclEnv -> SkolemInfoAnon -> [TcTyVar] -> TcM ()
reportBadTelescope ctxt env (ForAllSkol telescope) skols
  = do { msg <- mkErrorReport
                  env
                  (TcRnSolverReport [report] ErrorWithoutFlag noHints)
                  (Just ctxt)
                  []
       ; reportDiagnostic msg }
  where
    report = SolverReportWithCtxt ctxt $ BadTelescope telescope skols

reportBadTelescope _ _ skol_info skols
  = pprPanic "reportBadTelescope" (ppr skol_info $$ ppr skols)

{- Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations, we don't report unused givens if
they can give rise to improvement.  Example (#10100):
    class Add a b ab | a b -> ab, a ab -> b
    instance Add Zero b b
    instance Add a b ab => Add (Succ a) b (Succ ab)
The context (Add a b ab) for the instance is clearly unused in terms
of evidence, since the dictionary has no fields.  But it is still
needed!  With the context, a wanted constraint
   Add (Succ Zero) beta (Succ Zero)
we will reduce to (Add Zero beta Zero), and thence we get beta := Zero.
But without the context we won't find beta := Zero.

This only matters in instance declarations..
-}

-- | Should we completely ignore this constraint in error reporting?
-- It *must* be the case that any constraint for which this returns True
-- somehow causes an error to be reported elsewhere.
-- See Note [Constraints to ignore].
ignoreConstraint :: Ct -> Bool
ignoreConstraint ct
  | AssocFamPatOrigin <- ctOrigin ct
  = True
  | otherwise
  = False

-- | Makes an error item from a constraint, calculating whether or not
-- the item should be suppressed. See Note [Wanteds rewrite Wanteds]
-- in GHC.Tc.Types.Constraint. Returns Nothing if we should just ignore
-- a constraint. See Note [Constraints to ignore].
mkErrorItem :: Ct -> TcM (Maybe ErrorItem)
mkErrorItem ct
  | ignoreConstraint ct
  = do { traceTc "Ignoring constraint:" (ppr ct)
       ; return Nothing }   -- See Note [Constraints to ignore]

  | otherwise
  = do { let loc = ctLoc ct
             flav = ctFlavour ct

       ; (suppress, m_evdest) <- case ctEvidence ct of
           CtGiven {} -> return (False, Nothing)
           CtWanted { ctev_rewriters = rewriters, ctev_dest = dest }
             -> do { supp <- anyUnfilledCoercionHoles rewriters
                   ; return (supp, Just dest) }

       ; let m_reason = case ct of CIrredCan { cc_reason = reason } -> Just reason
                                   _                                -> Nothing

       ; return $ Just $ EI { ei_pred     = ctPred ct
                            , ei_evdest   = m_evdest
                            , ei_flavour  = flav
                            , ei_loc      = loc
                            , ei_m_reason = m_reason
                            , ei_suppress = suppress }}

----------------------------------------------------------------
reportWanteds :: SolverReportErrCtxt -> TcLevel -> WantedConstraints -> TcM ()
reportWanteds ctxt tc_lvl wc@(WC { wc_simple = simples, wc_impl = implics
                                 , wc_holes = holes })
  | isEmptyWC wc = traceTc "reportWanteds empty WC" empty
  | otherwise
  = do { tidy_items <- mapMaybeM mkErrorItem tidy_cts
       ; traceTc "reportWanteds 1" (vcat [ text "Simples =" <+> ppr simples
                                         , text "Suppress =" <+> ppr (cec_suppress ctxt)
                                         , text "tidy_cts   =" <+> ppr tidy_cts
                                         , text "tidy_items =" <+> ppr tidy_items
                                         , text "tidy_holes =" <+> ppr tidy_holes ])

       -- This check makes sure that we aren't suppressing the only error that will
       -- actually stop compilation
       ; massert $
           null simples ||                  -- no errors to report here
           any ignoreConstraint simples ||  -- one error is ignorable (is reported elsewhere)
           not (all ei_suppress tidy_items) -- not all error are suppressed

         -- First, deal with any out-of-scope errors:
       ; let (out_of_scope, other_holes) = partition isOutOfScopeHole tidy_holes
               -- don't suppress out-of-scope errors
             ctxt_for_scope_errs = ctxt { cec_suppress = False }
       ; (_, no_out_of_scope) <- askNoErrs $
                                 reportHoles tidy_items ctxt_for_scope_errs out_of_scope

         -- Next, deal with things that are utterly wrong
         -- Like Int ~ Bool (incl nullary TyCons)
         -- or  Int ~ t a   (AppTy on one side)
         -- These /ones/ are not suppressed by the incoming context
         -- (but will be by out-of-scope errors)
       ; let ctxt_for_insols = ctxt { cec_suppress = not no_out_of_scope }
       ; reportHoles tidy_items ctxt_for_insols other_holes
          -- holes never suppress

          -- See Note [Suppressing confusing errors]
       ; let (suppressed_items, items0) = partition suppress tidy_items
       ; traceTc "reportWanteds suppressed:" (ppr suppressed_items)
       ; (ctxt1, items1) <- tryReporters ctxt_for_insols report1 items0

         -- Now all the other constraints.  We suppress errors here if
         -- any of the first batch failed, or if the enclosing context
         -- says to suppress
       ; let ctxt2 = ctxt1 { cec_suppress = cec_suppress ctxt || cec_suppress ctxt1 }
       ; (ctxt3, leftovers) <- tryReporters ctxt2 report2 items1
       ; massertPpr (null leftovers)
           (text "The following unsolved Wanted constraints \
                 \have not been reported to the user:"
           $$ ppr leftovers)

       ; mapBagM_ (reportImplic ctxt2) implics
            -- NB ctxt2: don't suppress inner insolubles if there's only a
            -- wanted insoluble here; but do suppress inner insolubles
            -- if there's a *given* insoluble here (= inaccessible code)

            -- Only now, if there are no errors, do we report suppressed ones
            -- See Note [Suppressing confusing errors]
            -- We don't need to update the context further because of the
            -- whenNoErrs guard
       ; whenNoErrs $
         do { (_, more_leftovers) <- tryReporters ctxt3 report3 suppressed_items
            ; massertPpr (null more_leftovers) (ppr more_leftovers) } }
 where
    env        = cec_tidy ctxt
    tidy_cts   = bagToList (mapBag (tidyCt env)   simples)
    tidy_holes = bagToList (mapBag (tidyHole env) holes)

      -- See Note [Suppressing confusing errors]
    suppress :: ErrorItem -> Bool
    suppress item
      | Wanted <- ei_flavour item
      = is_ww_fundep_item item
      | otherwise
      = False

    -- report1: ones that should *not* be suppressed by
    --          an insoluble somewhere else in the tree
    -- It's crucial that anything that is considered insoluble
    -- (see GHC.Tc.Utils.insolublWantedCt) is caught here, otherwise
    -- we might suppress its error message, and proceed on past
    -- type checking to get a Lint error later
    report1 = [ ("custom_error", is_user_type_error, True,  mkUserTypeErrorReporter)

              , given_eq_spec
              , ("insoluble2",   utterly_wrong,  True, mkGroupReporter mkEqErr)
              , ("skolem eq1",   very_wrong,     True, mkSkolReporter)
              , ("skolem eq2",   skolem_eq,      True, mkSkolReporter)
              , ("non-tv eq",    non_tv_eq,      True, mkSkolReporter)

                  -- The only remaining equalities are alpha ~ ty,
                  -- where alpha is untouchable; and representational equalities
                  -- Prefer homogeneous equalities over hetero, because the
                  -- former might be holding up the latter.
                  -- See Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical
              , ("Homo eqs",      is_homo_equality,  True,  mkGroupReporter mkEqErr)
              , ("Other eqs",     is_equality,       True,  mkGroupReporter mkEqErr)
              ]

    -- report2: we suppress these if there are insolubles elsewhere in the tree
    report2 = [ ("Implicit params", is_ip,           False, mkGroupReporter mkIPErr)
              , ("Irreds",          is_irred,        False, mkGroupReporter mkIrredErr)
              , ("FixedRuntimeRep", is_FRR,          False, mkGroupReporter mkFRRErr)
              , ("Dicts",           is_dict,         False, mkGroupReporter mkDictErr) ]

    -- report3: suppressed errors should be reported as categorized by either report1
    -- or report2. Keep this in sync with the suppress function above
    report3 = [ ("wanted/wanted fundeps", is_ww_fundep, True, mkGroupReporter mkEqErr)
              ]

    -- rigid_nom_eq, rigid_nom_tv_eq,
    is_dict, is_equality, is_ip, is_FRR, is_irred :: ErrorItem -> Pred -> Bool

    is_given_eq item pred
       | Given <- ei_flavour item
       , EqPred {} <- pred = True
       | otherwise         = False
       -- I think all given residuals are equalities

    -- Things like (Int ~N Bool)
    utterly_wrong _ (EqPred NomEq ty1 ty2) = isRigidTy ty1 && isRigidTy ty2
    utterly_wrong _ _                      = False

    -- Things like (a ~N Int)
    very_wrong _ (EqPred NomEq ty1 ty2) = isSkolemTy tc_lvl ty1 && isRigidTy ty2
    very_wrong _ _                      = False

    -- Things like (a ~N b) or (a  ~N  F Bool)
    skolem_eq _ (EqPred NomEq ty1 _) = isSkolemTy tc_lvl ty1
    skolem_eq _ _                    = False

    -- Things like (F a  ~N  Int)
    non_tv_eq _ (EqPred NomEq ty1 _) = not (isTyVarTy ty1)
    non_tv_eq _ _                    = False

    is_user_type_error item _ = isUserTypeError (errorItemPred item)

    is_homo_equality _ (EqPred _ ty1 ty2) = tcTypeKind ty1 `tcEqType` tcTypeKind ty2
    is_homo_equality _ _                  = False

    is_equality _ (EqPred {}) = True
    is_equality _ _           = False

    is_dict _ (ClassPred {}) = True
    is_dict _ _              = False

    is_ip _ (ClassPred cls _) = isIPClass cls
    is_ip _ _                 = False

    is_FRR item (SpecialPred ConcretePrimPred _)
      | FixedRuntimeRepOrigin {} <- errorItemOrigin item
      = True
    is_FRR _ _
      = False

    is_irred _ (IrredPred {}) = True
    is_irred _ _              = False

     -- See situation (1) of Note [Suppressing confusing errors]
    is_ww_fundep item _ = is_ww_fundep_item item
    is_ww_fundep_item = isWantedWantedFunDepOrigin . errorItemOrigin

    given_eq_spec  -- See Note [Given errors]
      | has_gadt_match_here
      = ("insoluble1a", is_given_eq, True,  mkGivenErrorReporter)
      | otherwise
      = ("insoluble1b", is_given_eq, False, ignoreErrorReporter)
          -- False means don't suppress subsequent errors
          -- Reason: we don't report all given errors
          --         (see mkGivenErrorReporter), and we should only suppress
          --         subsequent errors if we actually report this one!
          --         #13446 is an example

    -- See Note [Given errors]
    has_gadt_match_here = has_gadt_match (cec_encl ctxt)
    has_gadt_match [] = False
    has_gadt_match (implic : implics)
      | PatSkol {} <- ic_info implic
      , ic_given_eqs implic /= NoGivenEqs
      , ic_warn_inaccessible implic
          -- Don't bother doing this if -Winaccessible-code isn't enabled.
          -- See Note [Avoid -Winaccessible-code when deriving] in GHC.Tc.TyCl.Instance.
      = True
      | otherwise
      = has_gadt_match implics

---------------
isSkolemTy :: TcLevel -> Type -> Bool
-- The type is a skolem tyvar
isSkolemTy tc_lvl ty
  | Just tv <- getTyVar_maybe ty
  =  isSkolemTyVar tv
  || (isTyVarTyVar tv && isTouchableMetaTyVar tc_lvl tv)
     -- The last case is for touchable TyVarTvs
     -- we postpone untouchables to a latter test (too obscure)

  | otherwise
  = False

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isTypeFamilyTyCon tc -> Just tc
                      _ -> Nothing

{- Note [Suppressing confusing errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Certain errors we might encounter are potentially confusing to users.
If there are any other errors to report, at all, we want to suppress these.

Which errors (only 1 case right now):

1) Errors which arise from the interaction of two Wanted fun-dep constraints.
   Example:

     class C a b | a -> b where
       op :: a -> b -> b

     foo _ = op True Nothing

     bar _ = op False []

   Here, we could infer
     foo :: C Bool (Maybe a) => p -> Maybe a
     bar :: C Bool [a]       => p -> [a]

   (The unused arguments suppress the monomorphism restriction.) The problem
   is that these types can't both be correct, as they violate the functional
   dependency. Yet reporting an error here is awkward: we must
   non-deterministically choose either foo or bar to reject. We thus want
   to report this problem only when there is nothing else to report.
   See typecheck/should_fail/T13506 for an example of when to suppress
   the error. The case above is actually accepted, because foo and bar
   are checked separately, and thus the two fundep constraints never
   encounter each other. It is test case typecheck/should_compile/FunDepOrigin1.

   This case applies only when both fundeps are *Wanted* fundeps; when
   both are givens, the error represents unreachable code. For
   a Given/Wanted case, see #9612.

Mechanism:

We use the `suppress` function within reportWanteds to filter out these two
cases, then report all other errors. Lastly, we return to these suppressed
ones and report them only if there have been no errors so far.

Note [Constraints to ignore]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some constraints are meant only to aid the solver by unification; a failure
to solve them is not necessarily an error to report to the user. It is critical
that compilation is aborted elsewhere if there are any ignored constraints here;
they will remain unfilled, and might have been used to rewrite another constraint.

Currently, the constraints to ignore are:

1) Constraints generated in order to unify associated type instance parameters
   with class parameters. Here are two illustrative examples:

     class C (a :: k) where
       type F (b :: k)

     instance C True where
       type F a = Int

     instance C Left where
       type F (Left :: a -> Either a b) = Bool

   In the first instance, we want to infer that `a` has type Bool. So we emit
   a constraint unifying kappa (the guessed type of `a`) with Bool. All is well.

   In the second instance, we process the associated type instance only
   after fixing the quantified type variables of the class instance. We thus
   have skolems a1 and b1 such that the class instance is for (Left :: a1 -> Either a1 b1).
   Unifying a1 and b1 with a and b in the type instance will fail, but harmlessly so.
   checkConsistentFamInst checks for this, and will fail if anything has gone
   awry. Really the equality constraints emitted are just meant as an aid, not
   a requirement. This is test case T13972.

   We detect this case by looking for an origin of AssocFamPatOrigin; constraints
   with this origin are dropped entirely during error message reporting.

   If there is any trouble, checkValidFamInst bleats, aborting compilation.

-}



--------------------------------------------
--      Reporters
--------------------------------------------

type Reporter
  = SolverReportErrCtxt -> [ErrorItem] -> TcM ()
type ReporterSpec
  = ( String                      -- Name
    , ErrorItem -> Pred -> Bool  -- Pick these ones
    , Bool                        -- True <=> suppress subsequent reporters
    , Reporter)                   -- The reporter itself

mkSkolReporter :: Reporter
-- Suppress duplicates with either the same LHS, or same location
-- Pre-condition: all items are equalities
mkSkolReporter ctxt items
  = mapM_ (reportGroup mkEqErr ctxt) (group items)
  where
     group [] = []
     group (item:items) = (item : yeses) : group noes
        where
          (yeses, noes) = partition (group_with item) items

     group_with item1 item2
       | EQ <- cmp_loc item1 item2 = True
       | eq_lhs_type   item1 item2 = True
       | otherwise                 = False

reportHoles :: [ErrorItem]  -- other (tidied) constraints
            -> SolverReportErrCtxt -> [Hole] -> TcM ()
reportHoles tidy_items ctxt holes
  = do
      diag_opts <- initDiagOpts <$> getDynFlags
      let severity = diagReasonSeverity diag_opts (cec_type_holes ctxt)
          holes'   = filter (keepThisHole severity) holes
      -- Zonk and tidy all the TcLclEnvs before calling `mkHoleError`
      -- because otherwise types will be zonked and tidied many times over.
      (tidy_env', lcl_name_cache) <- zonkTidyTcLclEnvs (cec_tidy ctxt) (map (ctl_env . hole_loc) holes')
      let ctxt' = ctxt { cec_tidy = tidy_env' }
      forM_ holes' $ \hole -> do { msg <- mkHoleError lcl_name_cache tidy_items ctxt' hole
                                 ; reportDiagnostic msg }

keepThisHole :: Severity -> Hole -> Bool
-- See Note [Skip type holes rapidly]
keepThisHole sev hole
  = case hole_sort hole of
       ExprHole {}    -> True
       TypeHole       -> keep_type_hole
       ConstraintHole -> keep_type_hole
  where
    keep_type_hole = case sev of
                         SevIgnore -> False
                         _         -> True

-- | zonkTidyTcLclEnvs takes a bunch of 'TcLclEnv's, each from a Hole.
-- It returns a ('Name' :-> 'Type') mapping which gives the zonked, tidied
-- type for each Id in any of the binder stacks in the  'TcLclEnv's.
-- Since there is a huge overlap between these stacks, is is much,
-- much faster to do them all at once, avoiding duplication.
zonkTidyTcLclEnvs :: TidyEnv -> [TcLclEnv] -> TcM (TidyEnv, NameEnv Type)
zonkTidyTcLclEnvs tidy_env lcls = foldM go (tidy_env, emptyNameEnv) (concatMap tcl_bndrs lcls)
  where
    go envs tc_bndr = case tc_bndr of
          TcTvBndr {} -> return envs
          TcIdBndr id _top_lvl -> go_one (idName id) (idType id) envs
          TcIdBndr_ExpType name et _top_lvl ->
            do { mb_ty <- readExpType_maybe et
                   -- et really should be filled in by now. But there's a chance
                   -- it hasn't, if, say, we're reporting a kind error en route to
                   -- checking a term. See test indexed-types/should_fail/T8129
                   -- Or we are reporting errors from the ambiguity check on
                   -- a local type signature
               ; case mb_ty of
                   Just ty -> go_one name ty envs
                   Nothing -> return envs
               }
    go_one name ty (tidy_env, name_env) = do
            if name `elemNameEnv` name_env
              then return (tidy_env, name_env)
              else do
                (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env ty
                return (tidy_env',  extendNameEnv name_env name tidy_ty)

{- Note [Skip type holes rapidly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have module with a /lot/ of partial type signatures, and we
compile it while suppressing partial-type-signature warnings.  Then
we don't want to spend ages constructing error messages and lists of
relevant bindings that we never display! This happened in #14766, in
which partial type signatures in a Happy-generated parser cause a huge
increase in compile time.

The function ignoreThisHole short-circuits the error/warning generation
machinery, in cases where it is definitely going to be a no-op.
-}

mkUserTypeErrorReporter :: Reporter
mkUserTypeErrorReporter ctxt
  = mapM_ $ \item -> do { let err = important ctxt $ mkUserTypeError item
                        ; maybeReportError ctxt [item] err
                        ; addDeferredBinding ctxt err item }

mkUserTypeError :: ErrorItem -> TcSolverReportMsg
mkUserTypeError item =
  case getUserTypeErrorMsg (errorItemPred item) of
    Just msg -> UserTypeError msg
    Nothing  -> pprPanic "mkUserTypeError" (ppr item)

mkGivenErrorReporter :: Reporter
-- See Note [Given errors]
mkGivenErrorReporter ctxt items
  = do { (ctxt, relevant_binds, item) <- relevantBindings True ctxt item
       ; let (implic:_) = cec_encl ctxt
                 -- Always non-empty when mkGivenErrorReporter is called
             loc'  = setCtLocEnv (ei_loc item) (ic_env implic)
             item' = item { ei_loc = loc' }
                   -- For given constraints we overwrite the env (and hence src-loc)
                   -- with one from the immediately-enclosing implication.
                   -- See Note [Inaccessible code]

       ; (eq_err_msgs, _hints) <- mkEqErr_help ctxt item' ty1 ty2
       -- The hints wouldn't help in this situation, so we discard them.
       ; let supplementary = [ SupplementaryBindings relevant_binds ]
             msg = TcRnInaccessibleCode implic (NE.reverse . NE.map (SolverReportWithCtxt ctxt) $ eq_err_msgs)
       ; msg <- mkErrorReport (ctLocEnv loc') msg (Just ctxt) supplementary
       ; reportDiagnostic msg }
  where
    (item : _ )  = items    -- Never empty
    (ty1, ty2)   = getEqPredTys (errorItemPred item)

ignoreErrorReporter :: Reporter
-- Discard Given errors that don't come from
-- a pattern match; maybe we should warn instead?
ignoreErrorReporter ctxt items
  = do { traceTc "mkGivenErrorReporter no" (ppr items $$ ppr (cec_encl ctxt))
       ; return () }


{- Note [Given errors]
~~~~~~~~~~~~~~~~~~~~~~
Given constraints represent things for which we have (or will have)
evidence, so they aren't errors.  But if a Given constraint is
insoluble, this code is inaccessible, and we might want to at least
warn about that.  A classic case is

   data T a where
     T1 :: T Int
     T2 :: T a
     T3 :: T Bool

   f :: T Int -> Bool
   f T1 = ...
   f T2 = ...
   f T3 = ...  -- We want to report this case as inaccessible

We'd like to point out that the T3 match is inaccessible. It
will have a Given constraint [G] Int ~ Bool.

But we don't want to report ALL insoluble Given constraints.  See Trac
#12466 for a long discussion.  For example, if we aren't careful
we'll complain about
   f :: ((Int ~ Bool) => a -> a) -> Int
which arguably is OK.  It's more debatable for
   g :: (Int ~ Bool) => Int -> Int
but it's tricky to distinguish these cases so we don't report
either.

The bottom line is this: has_gadt_match looks for an enclosing
pattern match which binds some equality constraints.  If we
find one, we report the insoluble Given.
-}

mkGroupReporter :: (SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport)
                             -- Make error message for a group
                -> Reporter  -- Deal with lots of constraints
-- Group together errors from same location,
-- and report only the first (to avoid a cascade)
mkGroupReporter mk_err ctxt items
  = mapM_ (reportGroup mk_err ctxt . toList) (equivClasses cmp_loc items)

eq_lhs_type :: ErrorItem -> ErrorItem -> Bool
eq_lhs_type item1 item2
  = case (classifyPredType (errorItemPred item1), classifyPredType (errorItemPred item2)) of
       (EqPred eq_rel1 ty1 _, EqPred eq_rel2 ty2 _) ->
         (eq_rel1 == eq_rel2) && (ty1 `eqType` ty2)
       _ -> pprPanic "mkSkolReporter" (ppr item1 $$ ppr item2)

cmp_loc :: ErrorItem -> ErrorItem -> Ordering
cmp_loc item1 item2 = get item1 `compare` get item2
  where
    get ei = realSrcSpanStart (ctLocSpan (errorItemCtLoc ei))
             -- Reduce duplication by reporting only one error from each
             -- /starting/ location even if the end location differs

reportGroup :: (SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport) -> Reporter
reportGroup mk_err ctxt items
  = do { err <- mk_err ctxt items
       ; traceTc "About to maybeReportErr" $
         vcat [ text "Constraint:"             <+> ppr items
              , text "cec_suppress ="          <+> ppr (cec_suppress ctxt)
              , text "cec_defer_type_errors =" <+> ppr (cec_defer_type_errors ctxt) ]
       ; maybeReportError ctxt items err
           -- But see Note [Always warn with -fdefer-type-errors]
       ; traceTc "reportGroup" (ppr items)
       ; mapM_ (addDeferredBinding ctxt err) items }
           -- Add deferred bindings for all
           -- Redundant if we are going to abort compilation,
           -- but that's hard to know for sure, and if we don't
           -- abort, we need bindings for all (e.g. #12156)

-- See Note [No deferring for multiplicity errors]
nonDeferrableOrigin :: CtOrigin -> Bool
nonDeferrableOrigin NonLinearPatternOrigin     = True
nonDeferrableOrigin (UsageEnvironmentOf {})    = True
nonDeferrableOrigin (FixedRuntimeRepOrigin {}) = True
nonDeferrableOrigin _                          = False

maybeReportError :: SolverReportErrCtxt
                 -> [ErrorItem]     -- items covered by the Report
                 -> SolverReport -> TcM ()
maybeReportError ctxt items@(item1:_) (SolverReport { sr_important_msgs = important
                                                    , sr_supplementary = supp
                                                    , sr_hints = hints })
  = unless (cec_suppress ctxt  -- Some worse error has occurred, so suppress this diagnostic
         || all ei_suppress items) $
                           -- if they're all to be suppressed, report nothing
                           -- if at least one is not suppressed, do report:
                           -- the function that generates the error message
                           -- should look for an unsuppressed error item
    do let reason | any (nonDeferrableOrigin . errorItemOrigin) items = ErrorWithoutFlag
                  | otherwise                                         = cec_defer_type_errors ctxt
                  -- See Note [No deferring for multiplicity errors]
           diag = TcRnSolverReport important reason hints
       msg <- mkErrorReport (ctLocEnv (errorItemCtLoc item1)) diag (Just ctxt) supp
       reportDiagnostic msg
maybeReportError _ _ _ = panic "maybeReportError"

addDeferredBinding :: SolverReportErrCtxt -> SolverReport -> ErrorItem -> TcM ()
-- See Note [Deferring coercion errors to runtime]
addDeferredBinding ctxt err (EI { ei_evdest = Just dest, ei_pred = item_ty
                                , ei_loc = loc })
     -- if evdest is Just, then the constraint was from a wanted
  | deferringAnyBindings ctxt
  = do { err_tm <- mkErrorTerm ctxt loc item_ty err
       ; let ev_binds_var = cec_binds ctxt

       ; case dest of
           EvVarDest evar
             -> addTcEvBind ev_binds_var $ mkWantedEvBind evar err_tm
           HoleDest hole
             -> do { -- See Note [Deferred errors for coercion holes]
                     let co_var = coHoleCoVar hole
                   ; addTcEvBind ev_binds_var $ mkWantedEvBind co_var err_tm
                   ; fillCoercionHole hole (mkTcCoVarCo co_var) }}
addDeferredBinding _ _ _ = return ()    -- Do not set any evidence for Given

mkErrorTerm :: SolverReportErrCtxt -> CtLoc -> Type  -- of the error term
            -> SolverReport -> TcM EvTerm
mkErrorTerm ctxt ct_loc ty (SolverReport { sr_important_msgs = important, sr_supplementary = supp })
  = do { msg <- mkErrorReport
                  (ctLocEnv ct_loc)
                  (TcRnSolverReport important ErrorWithoutFlag noHints) (Just ctxt) supp
         -- This will be reported at runtime, so we always want "error:" in the report, never "warning:"
       ; dflags <- getDynFlags
       ; let err_msg = pprLocMsgEnvelope msg
             err_str = showSDoc dflags $
                       err_msg $$ text "(deferred type error)"

       ; return $ evDelayedError ty err_str }

tryReporters :: SolverReportErrCtxt -> [ReporterSpec] -> [ErrorItem] -> TcM (SolverReportErrCtxt, [ErrorItem])
-- Use the first reporter in the list whose predicate says True
tryReporters ctxt reporters items
  = do { let (vis_items, invis_items)
               = partition (isVisibleOrigin . errorItemOrigin) items
       ; traceTc "tryReporters {" (ppr vis_items $$ ppr invis_items)
       ; (ctxt', items') <- go ctxt reporters vis_items invis_items
       ; traceTc "tryReporters }" (ppr items')
       ; return (ctxt', items') }
  where
    go ctxt [] vis_items invis_items
      = return (ctxt, vis_items ++ invis_items)

    go ctxt (r : rs) vis_items invis_items
       -- always look at *visible* Origins before invisible ones
       -- this is the whole point of isVisibleOrigin
      = do { (ctxt', vis_items') <- tryReporter ctxt r vis_items
           ; (ctxt'', invis_items') <- tryReporter ctxt' r invis_items
           ; go ctxt'' rs vis_items' invis_items' }
                -- Carry on with the rest, because we must make
                -- deferred bindings for them if we have -fdefer-type-errors
                -- But suppress their error messages

tryReporter :: SolverReportErrCtxt -> ReporterSpec -> [ErrorItem] -> TcM (SolverReportErrCtxt, [ErrorItem])
tryReporter ctxt (str, keep_me,  suppress_after, reporter) items
  | null yeses
  = return (ctxt, items)
  | otherwise
  = do { traceTc "tryReporter{ " (text str <+> ppr yeses)
       ; (_, no_errs) <- askNoErrs (reporter ctxt yeses)
       ; let suppress_now   = not no_errs && suppress_after
                            -- See Note [Suppressing error messages]
             ctxt' = ctxt { cec_suppress = suppress_now || cec_suppress ctxt }
       ; traceTc "tryReporter end }" (text str <+> ppr (cec_suppress ctxt) <+> ppr suppress_after)
       ; return (ctxt', nos) }
  where
    (yeses, nos) = partition keep items
    keep item = keep_me item (classifyPredType (errorItemPred item))

-- | Wrap an input 'TcRnMessage' with additional contextual information,
-- such as relevant bindings or valid hole fits.
mkErrorReport :: TcLclEnv
              -> TcRnMessage
                  -- ^ The main payload of the message.
              -> Maybe SolverReportErrCtxt
                  -- ^ The context to add, after the main diagnostic
                  -- but before the supplementary information.
                  -- Nothing <=> don't add any context.
              -> [SolverReportSupplementary]
                  -- ^ Supplementary information, to be added at the end of the message.
              -> TcM (MsgEnvelope TcRnMessage)
mkErrorReport tcl_env msg mb_ctxt supplementary
  = do { mb_context <- traverse (\ ctxt -> mkErrInfo (cec_tidy ctxt) (tcl_ctxt tcl_env)) mb_ctxt
       ; unit_state <- hsc_units <$> getTopEnv
       ; hfdc <- getHoleFitDispConfig
       ; let
           err_info =
             ErrInfo
               (fromMaybe empty mb_context)
               (vcat $ map (pprSolverReportSupplementary hfdc) supplementary)
       ; mkTcRnMessage
           (RealSrcSpan (tcl_loc tcl_env) Strict.Nothing)
           (TcRnMessageWithInfo unit_state $ TcRnMessageDetailed err_info msg) }

-- | Pretty-print supplementary information, to add to an error report.
pprSolverReportSupplementary :: HoleFitDispConfig -> SolverReportSupplementary -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprSolverReportSupplementary hfdc = \case
  SupplementaryBindings binds -> pprRelevantBindings binds
  SupplementaryHoleFits fits  -> pprValidHoleFits hfdc fits
  SupplementaryCts      cts   -> pprConstraintsInclude cts

-- | Display a collection of valid hole fits.
pprValidHoleFits :: HoleFitDispConfig -> ValidHoleFits -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprValidHoleFits hfdc (ValidHoleFits (Fits fits discarded_fits) (Fits refs discarded_refs))
  = fits_msg $$ refs_msg

  where
    fits_msg, refs_msg, fits_discard_msg, refs_discard_msg :: SDoc
    fits_msg = ppUnless (null fits) $
                    hang (text "Valid hole fits include") 2 $
                    vcat (map (pprHoleFit hfdc) fits)
                      $$ ppWhen  discarded_fits fits_discard_msg
    refs_msg = ppUnless (null refs) $
                  hang (text "Valid refinement hole fits include") 2 $
                  vcat (map (pprHoleFit hfdc) refs)
                    $$ ppWhen discarded_refs refs_discard_msg
    fits_discard_msg =
      text "(Some hole fits suppressed;" <+>
      text "use -fmax-valid-hole-fits=N" <+>
      text "or -fno-max-valid-hole-fits)"
    refs_discard_msg =
      text "(Some refinement hole fits suppressed;" <+>
      text "use -fmax-refinement-hole-fits=N" <+>
      text "or -fno-max-refinement-hole-fits)"

-- | Add a "Constraints include..." message.
--
-- See Note [Constraints include ...]
pprConstraintsInclude :: [(PredType, RealSrcSpan)] -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprConstraintsInclude cts
  = ppUnless (null cts) $
     hang (text "Constraints include")
        2 (vcat $ map pprConstraint cts)
  where
    pprConstraint (constraint, loc) =
      ppr constraint <+> nest 2 (parens (text "from" <+> ppr loc))

{- Note [Always warn with -fdefer-type-errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -fdefer-type-errors is on we warn about *all* type errors, even
if cec_suppress is on.  This can lead to a lot more warnings than you
would get errors without -fdefer-type-errors, but if we suppress any of
them you might get a runtime error that wasn't warned about at compile
time.

To be consistent, we should also report multiple warnings from a single
location in mkGroupReporter, when -fdefer-type-errors is on.  But that
is perhaps a bit *over*-consistent!

With #10283, you can now opt out of deferred type error warnings.

Note [No deferring for multiplicity errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [Wrapper returned from tcSubMult] in GHC.Tc.Utils.Unify,
linear types do not support casts and any nontrivial coercion will raise
an error during desugaring.

This means that even if we defer a multiplicity mismatch during typechecking,
the desugarer will refuse to compile anyway. Worse: the error raised
by the desugarer would shadow the type mismatch warnings (#20083).
As a solution, we refuse to defer submultiplicity constraints. Test: T20083.

To determine whether a constraint arose from a submultiplicity check, we
look at the CtOrigin. All calls to tcSubMult use one of two origins,
UsageEnvironmentOf and NonLinearPatternOrigin. Those origins are not
used outside of linear types.

In the future, we should compile 'WpMultCoercion' to a runtime error with
-fdefer-type-errors, but the current implementation does not always
place the wrapper in the right place and the resulting program can fail Lint.
This plan is tracked in #20083.

Note [Deferred errors for coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we need to defer a type error where the destination for the evidence
is a coercion hole. We can't just put the error in the hole, because we can't
make an erroneous coercion. (Remember that coercions are erased for runtime.)
Instead, we invent a new EvVar, bind it to an error and then make a coercion
from that EvVar, filling the hole with that coercion. Because coercions'
types are unlifted, the error is guaranteed to be hit before we get to the
coercion.

************************************************************************
*                                                                      *
                Irreducible predicate errors
*                                                                      *
************************************************************************
-}

mkIrredErr :: SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport
mkIrredErr ctxt items
  = do { (ctxt, binds_msg, item1) <- relevantBindings True ctxt item1
       ; let msg = important ctxt $
                   CouldNotDeduce (getUserGivens ctxt) (item1 :| others) Nothing
       ; return $ msg `mappend` mk_relevant_bindings binds_msg }
  where
    (item1:others) = final_items

    filtered_items = filter (not . ei_suppress) items
    final_items | null filtered_items = items
                    -- they're all suppressed; must report *something*
                    -- NB: even though reportWanteds asserts that not
                    -- all items are suppressed, it's possible all the
                    -- irreducibles are suppressed, and so this function
                    -- might get all suppressed items
                | otherwise           = filtered_items

{- Note [Constructing Hole Errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whether or not 'mkHoleError' returns an error is not influenced by cec_suppress. In other terms,
these "hole" errors are /not/ suppressed by cec_suppress. We want to see them!

There are two cases to consider:

1. For out-of-scope variables we always report an error, unless -fdefer-out-of-scope-variables is on,
   in which case the messages are discarded. See also #12170 and #12406. If deferring, report a warning
   only if -Wout-of-scope-variables is on.

2. For the general case, when -XPartialTypeSignatures is on, warnings (instead of errors) are generated
   for holes in partial type signatures, unless -Wpartial-type-signatures is not on, in which case
   the messages are discarded. If deferring, report a warning only if -Wtyped-holes is on.

The above can be summarised into the following table:

| Hole Type    | Active Flags                                             | Outcome          |
|--------------|----------------------------------------------------------|------------------|
| out-of-scope | None                                                     | Error            |
| out-of-scope | -fdefer-out-of-scope-variables, -Wout-of-scope-variables | Warning          |
| out-of-scope | -fdefer-out-of-scope-variables                           | Ignore (discard) |
| type         | None                                                     | Error            |
| type         | -XPartialTypeSignatures, -Wpartial-type-signatures       | Warning          |
| type         | -XPartialTypeSignatures                                  | Ignore (discard) |
| expression   | None                                                     | Error            |
| expression   | -Wdefer-typed-holes, -Wtyped-holes                       | Warning          |
| expression   | -Wdefer-typed-holes                                      | Ignore (discard) |

See also 'reportUnsolved'.

-}

----------------
-- | Constructs a new hole error, unless this is deferred. See Note [Constructing Hole Errors].
mkHoleError :: NameEnv Type -> [ErrorItem] -> SolverReportErrCtxt -> Hole -> TcM (MsgEnvelope TcRnMessage)
mkHoleError _ _tidy_simples ctxt hole@(Hole { hole_occ = occ, hole_loc = ct_loc })
  | isOutOfScopeHole hole
  = do { dflags  <- getDynFlags
       ; rdr_env <- getGlobalRdrEnv
       ; imp_info <- getImports
       ; curr_mod <- getModule
       ; hpt <- getHpt
       ; let (imp_errs, hints)
                = unknownNameSuggestions WL_Anything
                    dflags hpt curr_mod rdr_env
                    (tcl_rdr lcl_env) imp_info (mkRdrUnqual occ)
             errs   = [SolverReportWithCtxt ctxt (ReportHoleError hole $ OutOfScopeHole imp_errs)]
             report = SolverReport errs [] hints

       ; maybeAddDeferredBindings ctxt hole report
       ; mkErrorReport lcl_env (TcRnSolverReport errs (cec_out_of_scope_holes ctxt) hints) Nothing []
          -- Pass the value 'Nothing' for the context, as it's generally not helpful
          -- to include the context here.
       }
  where
    lcl_env = ctLocEnv ct_loc

 -- general case: not an out-of-scope error
mkHoleError lcl_name_cache tidy_simples ctxt
  hole@(Hole { hole_ty = hole_ty
             , hole_sort = sort
             , hole_loc = ct_loc })
  = do { rel_binds
           <- relevant_bindings False lcl_env lcl_name_cache (tyCoVarsOfType hole_ty)
               -- The 'False' means "don't filter the bindings"; see Trac #8191

       ; show_hole_constraints <- goptM Opt_ShowHoleConstraints
       ; let relevant_cts
               | ExprHole _ <- sort, show_hole_constraints
               = givenConstraints ctxt
               | otherwise
               = []

       ; show_valid_hole_fits <- goptM Opt_ShowValidHoleFits
       ; (ctxt, hole_fits) <- if show_valid_hole_fits
                              then validHoleFits ctxt tidy_simples hole
                              else return (ctxt, noValidHoleFits)
       ; (grouped_skvs, other_tvs) <- zonkAndGroupSkolTvs hole_ty
       ; let reason | ExprHole _ <- sort = cec_expr_holes ctxt
                    | otherwise          = cec_type_holes ctxt
             errs = [SolverReportWithCtxt ctxt $ ReportHoleError hole $ HoleError sort other_tvs grouped_skvs]
             supp = [ SupplementaryBindings rel_binds
                    , SupplementaryCts      relevant_cts
                    , SupplementaryHoleFits hole_fits ]

       ; maybeAddDeferredBindings ctxt hole (SolverReport errs supp [])

       ; mkErrorReport lcl_env (TcRnSolverReport errs reason noHints) (Just ctxt) supp
       }

  where
    lcl_env = ctLocEnv ct_loc

-- | For all the skolem type variables in a type, zonk the skolem info and group together
-- all the type variables with the same origin.
zonkAndGroupSkolTvs :: Type -> TcM ([(SkolemInfoAnon, [TcTyVar])], [TcTyVar])
zonkAndGroupSkolTvs hole_ty = do
  zonked_info <- mapM (\(sk, tv) -> (,) <$> (zonkSkolemInfoAnon . getSkolemInfo $ sk) <*> pure (fst <$> tv)) skolem_list
  return (zonked_info, other_tvs)
  where
    tvs = tyCoVarsOfTypeList hole_ty
    (skol_tvs, other_tvs) = partition (\tv -> isTcTyVar tv && isSkolemTyVar tv) tvs

    group_skolems :: UM.UniqMap SkolemInfo ([(TcTyVar, Int)])
    group_skolems = bagToList <$> UM.listToUniqMap_C unionBags [(skolemSkolInfo tv, unitBag (tv, n)) | tv <- skol_tvs | n <- [0..]]

    skolem_list = sortBy (comparing (sort . map snd . snd)) (UM.nonDetEltsUniqMap group_skolems)

{- Note [Adding deferred bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When working with typed holes we have to deal with the case where
we want holes to be reported as warnings to users during compile time but
as errors during runtime. Therefore, we have to call 'maybeAddDeferredBindings'
so that the correct 'Severity' can be computed out of that later on.

-}


-- | Adds deferred bindings (as errors).
-- See Note [Adding deferred bindings].
maybeAddDeferredBindings :: SolverReportErrCtxt
                         -> Hole
                         -> SolverReport
                         -> TcM ()
maybeAddDeferredBindings ctxt hole report = do
  case hole_sort hole of
    ExprHole (HER ref ref_ty _) -> do
      -- Only add bindings for holes in expressions
      -- not for holes in partial type signatures
      -- cf. addDeferredBinding
      when (deferringAnyBindings ctxt) $ do
        err_tm <- mkErrorTerm ctxt (hole_loc hole) ref_ty report
          -- NB: ref_ty, not hole_ty. hole_ty might be rewritten.
          -- See Note [Holes] in GHC.Tc.Types.Constraint
        writeMutVar ref err_tm
    _ -> pure ()

-- We unwrap the SolverReportErrCtxt here, to avoid introducing a loop in module
-- imports
validHoleFits :: SolverReportErrCtxt    -- ^ The context we're in, i.e. the
                                        -- implications and the tidy environment
              -> [ErrorItem]      -- ^ Unsolved simple constraints
              -> Hole             -- ^ The hole
              -> TcM (SolverReportErrCtxt, ValidHoleFits)
                -- ^ We return the new context
                -- with a possibly updated
                -- tidy environment, and
                -- the valid hole fits.
validHoleFits ctxt@(CEC { cec_encl = implics
                        , cec_tidy = lcl_env}) simps hole
  = do { (tidy_env, fits) <- findValidHoleFits lcl_env implics (map mk_wanted simps) hole
       ; return (ctxt {cec_tidy = tidy_env}, fits) }
  where
    mk_wanted :: ErrorItem -> CtEvidence
    mk_wanted (EI { ei_pred = pred, ei_evdest = Just dest, ei_loc = loc })
         = CtWanted { ctev_pred      = pred
                    , ctev_dest      = dest
                    , ctev_loc       = loc
                    , ctev_rewriters = emptyRewriterSet }
    mk_wanted item = pprPanic "validHoleFits no evdest" (ppr item)

-- See Note [Constraints include ...]
givenConstraints :: SolverReportErrCtxt -> [(Type, RealSrcSpan)]
givenConstraints ctxt
  = do { implic@Implic{ ic_given = given } <- cec_encl ctxt
       ; constraint <- given
       ; return (varType constraint, tcl_loc (ic_env implic)) }

----------------

mkIPErr :: SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport
-- What would happen if an item is suppressed because of
-- Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint? Very unclear
-- what's best. Let's not worry about this.
mkIPErr ctxt items
  = do { (ctxt, binds_msg, item1) <- relevantBindings True ctxt item1
       ; let msg = important ctxt $ UnboundImplicitParams (item1 :| others)
       ; return $ msg `mappend` mk_relevant_bindings binds_msg }
  where
    item1:others = items

----------------

-- | Create a user-facing error message for unsolved @'Concrete#' ki@
-- Wanted constraints arising from representation-polymorphism checks.
--
-- See Note [Reporting representation-polymorphism errors] in GHC.Tc.Types.Origin.
mkFRRErr :: SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport
mkFRRErr ctxt items
  = do { -- Zonking/tidying.
       ; origs <-
           -- Zonk/tidy the 'CtOrigin's.
           zonkTidyOrigins (cec_tidy ctxt) (map errorItemOrigin items)
             <&>
           -- Then remove duplicates: only retain one 'CtOrigin' per representation-polymorphic type.
           (nubOrdBy (nonDetCmpType `on` (snd . frr_orig_and_type)) . snd)
        -- Obtain all the errors we want to report (constraints with FixedRuntimeRep origin),
        -- with the corresponding types:
        --   ty1 :: TYPE rep1, ty2 :: TYPE rep2, ...
       ; let origs_and_tys = map frr_orig_and_type origs

       ; return $ important ctxt $ FixedRuntimeRepError origs_and_tys }
  where

    frr_orig_and_type :: CtOrigin -> (FRROrigin, Type)
    frr_orig_and_type (FixedRuntimeRepOrigin ty frr_orig) = (frr_orig, ty)
    frr_orig_and_type orig
      = pprPanic "mkFRRErr: not a FixedRuntimeRep origin"
          (text "origin =" <+> ppr orig)

{-
Note [Constraints include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'givenConstraintsMsg' returns the "Constraints include ..." message enabled by
-fshow-hole-constraints. For example, the following hole:

    foo :: (Eq a, Show a) => a -> String
    foo x = _

would generate the message:

    Constraints include
      Eq a (from foo.hs:1:1-36)
      Show a (from foo.hs:1:1-36)

Constraints are displayed in order from innermost (closest to the hole) to
outermost. There's currently no filtering or elimination of duplicates.

************************************************************************
*                                                                      *
                Equality errors
*                                                                      *
************************************************************************

Note [Inaccessible code]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a where
     T1 :: T a
     T2 :: T Bool

   f :: (a ~ Int) => T a -> Int
   f T1 = 3
   f T2 = 4   -- Unreachable code

Here the second equation is unreachable. The original constraint
(a~Int) from the signature gets rewritten by the pattern-match to
(Bool~Int), so the danger is that we report the error as coming from
the *signature* (#7293).  So, for Given errors we replace the
env (and hence src-loc) on its CtLoc with that from the immediately
enclosing implication.

Note [Error messages for untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#9109)
  data G a where { GBool :: G Bool }
  foo x = case x of GBool -> True

Here we can't solve (t ~ Bool), where t is the untouchable result
meta-var 't', because of the (a ~ Bool) from the pattern match.
So we infer the type
   f :: forall a t. G a -> t
making the meta-var 't' into a skolem.  So when we come to report
the unsolved (t ~ Bool), t won't look like an untouchable meta-var
any more.  So we don't assert that it is.
-}

-- Don't have multiple equality errors from the same location
-- E.g.   (Int,Bool) ~ (Bool,Int)   one error will do!
mkEqErr :: SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport
mkEqErr ctxt items
  | item:_ <- filter (not . ei_suppress) items
  = mkEqErr1 ctxt item

  | item:_ <- items  -- they're all suppressed. still need an error message
                     -- for -fdefer-type-errors though
  = mkEqErr1 ctxt item

  | otherwise
  = panic "mkEqErr"  -- guaranteed to have at least one item

mkEqErr1 :: SolverReportErrCtxt -> ErrorItem -> TcM SolverReport
mkEqErr1 ctxt item   -- Wanted only
                     -- givens handled in mkGivenErrorReporter
  = do { (ctxt, binds_msg, item) <- relevantBindings True ctxt item
       ; rdr_env <- getGlobalRdrEnv
       ; fam_envs <- tcGetFamInstEnvs
       ; let mb_coercible_msg = case errorItemEqRel item of
               NomEq  -> Nothing
               ReprEq -> ReportCoercibleMsg <$> mkCoercibleExplanation rdr_env fam_envs ty1 ty2
       ; traceTc "mkEqErr1" (ppr item $$ pprCtOrigin (errorItemOrigin item))
       ; (last_msg :| prev_msgs, hints) <- mkEqErr_help ctxt item ty1 ty2
       ; let
           report = foldMap (important ctxt) (reverse prev_msgs)
                  `mappend` (important ctxt $ mkTcReportWithInfo last_msg $ maybeToList mb_coercible_msg)
                  `mappend` (mk_relevant_bindings binds_msg)
                  `mappend` (mk_report_hints hints)
       ; return report }
  where
    (ty1, ty2) = getEqPredTys (errorItemPred item)

-- | This function tries to reconstruct why a "Coercible ty1 ty2" constraint
-- is left over.
mkCoercibleExplanation :: GlobalRdrEnv -> FamInstEnvs
                       -> TcType -> TcType -> Maybe CoercibleMsg
mkCoercibleExplanation rdr_env fam_envs ty1 ty2
  | Just (tc, tys) <- tcSplitTyConApp_maybe ty1
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = Just msg
  | Just (tc, tys) <- splitTyConApp_maybe ty2
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = Just msg
  | Just (s1, _) <- tcSplitAppTy_maybe ty1
  , Just (s2, _) <- tcSplitAppTy_maybe ty2
  , s1 `eqType` s2
  , has_unknown_roles s1
  = Just $ UnknownRoles s1
  | otherwise
  = Nothing
  where
    coercible_msg_for_tycon tc
        | isAbstractTyCon tc
        = Just $ TyConIsAbstract tc
        | isNewTyCon tc
        , [data_con] <- tyConDataCons tc
        , let dc_name = dataConName data_con
        , isNothing (lookupGRE_Name rdr_env dc_name)
        = Just $ OutOfScopeNewtypeConstructor tc data_con
        | otherwise = Nothing

    has_unknown_roles ty
      | Just (tc, tys) <- tcSplitTyConApp_maybe ty
      = tys `lengthAtLeast` tyConArity tc  -- oversaturated tycon
      | Just (s, _) <- tcSplitAppTy_maybe ty
      = has_unknown_roles s
      | isTyVarTy ty
      = True
      | otherwise
      = False

-- | Accumulated messages in reverse order.
type AccReportMsgs = NonEmpty TcSolverReportMsg

mkEqErr_help :: SolverReportErrCtxt
             -> ErrorItem
             -> TcType -> TcType -> TcM (AccReportMsgs, [GhcHint])
mkEqErr_help ctxt item ty1 ty2
  | Just (tv1, _) <- tcGetCastedTyVar_maybe ty1
  = mkTyVarEqErr ctxt item tv1 ty2
  | Just (tv2, _) <- tcGetCastedTyVar_maybe ty2
  = mkTyVarEqErr ctxt item tv2 ty1
  | otherwise
  = return (reportEqErr ctxt item ty1 ty2 :| [], [])

reportEqErr :: SolverReportErrCtxt
            -> ErrorItem
            -> TcType -> TcType -> TcSolverReportMsg
reportEqErr ctxt item ty1 ty2
  = mkTcReportWithInfo mismatch eqInfos
  where
    mismatch = misMatchOrCND False ctxt item ty1 ty2
    eqInfos  = eqInfoMsgs ty1 ty2

mkTyVarEqErr :: SolverReportErrCtxt -> ErrorItem
             -> TcTyVar -> TcType -> TcM (AccReportMsgs, [GhcHint])
-- tv1 and ty2 are already tidied
mkTyVarEqErr ctxt item tv1 ty2
  = do { traceTc "mkTyVarEqErr" (ppr item $$ ppr tv1 $$ ppr ty2)
       ; mkTyVarEqErr' ctxt item tv1 ty2 }

mkTyVarEqErr' :: SolverReportErrCtxt -> ErrorItem
              -> TcTyVar -> TcType -> TcM (AccReportMsgs, [GhcHint])
mkTyVarEqErr' ctxt item tv1 ty2
     -- impredicativity is a simple error to understand; try it first
  | check_eq_result `cterHasProblem` cteImpredicative = do
  tyvar_eq_info <- extraTyVarEqInfo tv1 ty2
  let
      poly_msg = CannotUnifyWithPolytype item tv1 ty2
      poly_msg_with_info
        | isSkolemTyVar tv1
        = mkTcReportWithInfo poly_msg tyvar_eq_info
        | otherwise
        = poly_msg
      -- Unlike the other reports, this discards the old 'report_important'
       -- instead of augmenting it.  This is because the details are not likely
       -- to be helpful since this is just an unimplemented feature.
  return (poly_msg_with_info <| headline_msg :| [], [])

  | isSkolemTyVar tv1  -- ty2 won't be a meta-tyvar; we would have
                       -- swapped in Solver.Canonical.canEqTyVarHomo
    || isTyVarTyVar tv1 && not (isTyVarTy ty2)
    || errorItemEqRel item == ReprEq
     -- The cases below don't really apply to ReprEq (except occurs check)
  = do
    tv_extra     <- extraTyVarEqInfo tv1 ty2
    return (mkTcReportWithInfo headline_msg tv_extra :| [], add_sig)

  | cterHasOccursCheck check_eq_result
    -- We report an "occurs check" even for  a ~ F t a, where F is a type
    -- function; it's not insoluble (because in principle F could reduce)
    -- but we have certainly been unable to solve it
  = let extras2 = eqInfoMsgs ty1 ty2

        interesting_tyvars = filter (not . noFreeVarsOfType . tyVarKind) $
                             filter isTyVar $
                             fvVarList $
                             tyCoFVsOfType ty1 `unionFV` tyCoFVsOfType ty2

        extras3 = case interesting_tyvars of
          [] -> []
          (tv : tvs) -> [OccursCheckInterestingTyVars (tv :| tvs)]

    in return (mkTcReportWithInfo headline_msg (extras2 ++ extras3) :| [], [])

    -- This is wrinkle (4) in Note [Equalities with incompatible kinds] in
    -- GHC.Tc.Solver.Canonical
  | hasCoercionHoleTy ty2
  = return (mkBlockedEqErr item :| [], [])

  -- If the immediately-enclosing implication has 'tv' a skolem, and
  -- we know by now its an InferSkol kind of skolem, then presumably
  -- it started life as a TyVarTv, else it'd have been unified, given
  -- that there's no occurs-check or forall problem
  | (implic:_) <- cec_encl ctxt
  , Implic { ic_skols = skols } <- implic
  , tv1 `elem` skols
  = do
    tv_extra     <- extraTyVarEqInfo tv1 ty2
    return (mkTcReportWithInfo mismatch_msg tv_extra :| [], [])

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_skols = skols } <- implic
  , let esc_skols = filter (`elemVarSet` (tyCoVarsOfType ty2)) skols
  , not (null esc_skols)
  = return (SkolemEscape item implic esc_skols :| [mismatch_msg], [])

  -- Nastiest case: attempt to unify an untouchable variable
  -- So tv is a meta tyvar (or started that way before we
  -- generalised it).  So presumably it is an *untouchable*
  -- meta tyvar or a TyVarTv, else it'd have been unified
  -- See Note [Error messages for untouchables]
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_tclvl = lvl } <- implic
  = assertPpr (not (isTouchableMetaTyVar lvl tv1))
              (ppr tv1 $$ ppr lvl) $ do -- See Note [Error messages for untouchables]
    let tclvl_extra = UntouchableVariable tv1 implic
    tv_extra     <- extraTyVarEqInfo tv1 ty2
    return (mkTcReportWithInfo tclvl_extra tv_extra :| [mismatch_msg], add_sig)

  | otherwise
  = return (reportEqErr ctxt item (mkTyVarTy tv1) ty2 :| [], [])
        -- This *can* happen (#6123)
        -- Consider an ambiguous top-level constraint (a ~ F a)
        -- Not an occurs check, because F is a type function.
  where
    headline_msg = misMatchOrCND insoluble_occurs_check ctxt item ty1 ty2
    mismatch_msg = mkMismatchMsg item ty1 ty2
    add_sig      = maybeToList $ suggestAddSig ctxt ty1 ty2

    ty1 = mkTyVarTy tv1

    check_eq_result = case ei_m_reason item of
      Just (NonCanonicalReason result) -> result
      _ -> checkTyVarEq tv1 ty2
        -- in T2627b, we report an error for F (F a0) ~ a0. Note that the type
        -- variable is on the right, so we don't get useful info for the CIrredCan,
        -- and have to compute the result of checkTyVarEq here.

    insoluble_occurs_check = check_eq_result `cterHasProblem` cteInsolubleOccurs

eqInfoMsgs :: TcType -> TcType -> [TcSolverReportInfo]
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
eqInfoMsgs ty1 ty2
  = catMaybes [tyfun_msg, ambig_msg]
  where
    mb_fun1 = isTyFun_maybe ty1
    mb_fun2 = isTyFun_maybe ty2

      -- if a type isn't headed by a type function, then any ambiguous
      -- variables need not be reported as such. e.g.: F a ~ t0 -> t0, where a is a skolem
    ambig_tkvs1 = maybe mempty (\_ -> ambigTkvsOfTy ty1) mb_fun1
    ambig_tkvs2 = maybe mempty (\_ -> ambigTkvsOfTy ty2) mb_fun2

    ambig_tkvs@(ambig_kvs, ambig_tvs) = ambig_tkvs1 S.<> ambig_tkvs2

    ambig_msg | isJust mb_fun1 || isJust mb_fun2
              , not (null ambig_kvs && null ambig_tvs)
              = Just $ Ambiguity False ambig_tkvs
              | otherwise
              = Nothing

    tyfun_msg | Just tc1 <- mb_fun1
              , Just tc2 <- mb_fun2
              , tc1 == tc2
              , not (isInjectiveTyCon tc1 Nominal)
              = Just $ NonInjectiveTyFam tc1
              | otherwise
              = Nothing

misMatchOrCND :: Bool -> SolverReportErrCtxt -> ErrorItem
              -> TcType -> TcType -> TcSolverReportMsg
-- If oriented then ty1 is actual, ty2 is expected
misMatchOrCND insoluble_occurs_check ctxt item ty1 ty2
  | insoluble_occurs_check  -- See Note [Insoluble occurs check]
    || (isRigidTy ty1 && isRigidTy ty2)
    || (ei_flavour item == Given)
    || null givens
  = -- If the equality is unconditionally insoluble
    -- or there is no context, don't report the context
    mkMismatchMsg item ty1 ty2

  | otherwise
  = CouldNotDeduce givens (item :| []) (Just $ CND_Extra level ty1 ty2)

  where
    level   = ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel
    givens  = [ given | given <- getUserGivens ctxt, ic_given_eqs given /= NoGivenEqs ]
              -- Keep only UserGivens that have some equalities.
              -- See Note [Suppress redundant givens during error reporting]

-- These are for the "blocked" equalities, as described in TcCanonical
-- Note [Equalities with incompatible kinds], wrinkle (2). There should
-- always be another unsolved wanted around, which will ordinarily suppress
-- this message. But this can still be printed out with -fdefer-type-errors
-- (sigh), so we must produce a message.
mkBlockedEqErr :: ErrorItem -> TcSolverReportMsg
mkBlockedEqErr item = BlockedEquality item

{-
Note [Suppress redundant givens during error reporting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When GHC is unable to solve a constraint and prints out an error message, it
will print out what given constraints are in scope to provide some context to
the programmer. But we shouldn't print out /every/ given, since some of them
are not terribly helpful to diagnose type errors. Consider this example:

  foo :: Int :~: Int -> a :~: b -> a :~: c
  foo Refl Refl = Refl

When reporting that GHC can't solve (a ~ c), there are two givens in scope:
(Int ~ Int) and (a ~ b). But (Int ~ Int) is trivially soluble (i.e.,
redundant), so it's not terribly useful to report it in an error message.
To accomplish this, we discard any Implications that do not bind any
equalities by filtering the `givens` selected in `misMatchOrCND` (based on
the `ic_given_eqs` field of the Implication). Note that we discard givens
that have no equalities whatsoever, but we want to keep ones with only *local*
equalities, as these may be helpful to the user in understanding what went
wrong.

But this is not enough to avoid all redundant givens! Consider this example,
from #15361:

  goo :: forall (a :: Type) (b :: Type) (c :: Type).
         a :~~: b -> a :~~: c
  goo HRefl = HRefl

Matching on HRefl brings the /single/ given (* ~ *, a ~ b) into scope.
The (* ~ *) part arises due the kinds of (:~~:) being unified. More
importantly, (* ~ *) is redundant, so we'd like not to report it. However,
the Implication (* ~ *, a ~ b) /does/ bind an equality (as reported by its
ic_given_eqs field), so the test above will keep it wholesale.

To refine this given, we apply mkMinimalBySCs on it to extract just the (a ~ b)
part. This works because mkMinimalBySCs eliminates reflexive equalities in
addition to superclasses (see Note [Remove redundant provided dicts]
in GHC.Tc.TyCl.PatSyn).
-}

extraTyVarEqInfo :: TcTyVar -> TcType -> TcM [TcSolverReportInfo]
-- Add on extra info about skolem constants
-- NB: The types themselves are already tidied
extraTyVarEqInfo tv1 ty2
  = (:) <$> extraTyVarInfo tv1 <*> ty_extra ty2
  where
    ty_extra ty = case tcGetCastedTyVar_maybe ty of
                    Just (tv, _) -> (:[]) <$> extraTyVarInfo tv
                    Nothing      -> return []

extraTyVarInfo :: TcTyVar -> TcM TcSolverReportInfo
extraTyVarInfo tv = assertPpr (isTyVar tv) (ppr tv) $
  case tcTyVarDetails tv of
    SkolemTv skol_info lvl overlaps -> do
      new_skol_info <- zonkSkolemInfo skol_info
      return $ TyVarInfo (mkTcTyVar (tyVarName tv) (tyVarKind tv) (SkolemTv new_skol_info lvl overlaps))
    _ -> return $ TyVarInfo tv


suggestAddSig :: SolverReportErrCtxt -> TcType -> TcType -> Maybe GhcHint
-- See Note [Suggest adding a type signature]
suggestAddSig ctxt ty1 _ty2
  | bndr : bndrs <- inferred_bndrs
  = Just $ SuggestAddTypeSignatures $ NamedBindings (bndr :| bndrs)
  | otherwise
  = Nothing
  where
    inferred_bndrs =
      case tcGetTyVar_maybe ty1 of
        Just tv | isSkolemTyVar tv -> find (cec_encl ctxt) False tv
        _                          -> []

    -- 'find' returns the binders of an InferSkol for 'tv',
    -- provided there is an intervening implication with
    -- ic_given_eqs /= NoGivenEqs (i.e. a GADT match)
    find [] _ _ = []
    find (implic:implics) seen_eqs tv
       | tv `elem` ic_skols implic
       , InferSkol prs <- ic_info implic
       , seen_eqs
       = map fst prs
       | otherwise
       = find implics (seen_eqs || ic_given_eqs implic /= NoGivenEqs) tv

--------------------
mkMismatchMsg :: ErrorItem -> Type -> Type -> TcSolverReportMsg
mkMismatchMsg item ty1 ty2 =
  case orig of
    TypeEqOrigin { uo_actual, uo_expected, uo_thing = mb_thing } ->
      mkTcReportWithInfo
        (TypeEqMismatch
          { teq_mismatch_ppr_explicit_kinds = ppr_explicit_kinds
          , teq_mismatch_item = item
          , teq_mismatch_ty1  = ty1
          , teq_mismatch_ty2  = ty2
          , teq_mismatch_actual   = uo_actual
          , teq_mismatch_expected = uo_expected
          , teq_mismatch_what     = mb_thing})
        extras
    KindEqOrigin cty1 cty2 sub_o mb_sub_t_or_k ->
      mkTcReportWithInfo (Mismatch False item ty1 ty2)
        (WhenMatching cty1 cty2 sub_o mb_sub_t_or_k : extras)
    _ ->
      mkTcReportWithInfo
        (Mismatch False item ty1 ty2)
        extras
  where
    orig = errorItemOrigin item
    extras = sameOccExtras ty2 ty1
    ppr_explicit_kinds = shouldPprWithExplicitKinds ty1 ty2 orig

-- | Whether to print explicit kinds (with @-fprint-explicit-kinds@)
-- in an 'SDoc' when a type mismatch occurs to due invisible kind arguments.
--
-- This function first checks to see if the 'CtOrigin' argument is a
-- 'TypeEqOrigin'. If so, it first checks whether the equality is a visible
-- equality; if it's not, definitely print the kinds. Even if the equality is
-- a visible equality, check the expected/actual types to see if the types
-- have equal visible components. If the 'CtOrigin' is
-- not a 'TypeEqOrigin', fall back on the actual mismatched types themselves.
shouldPprWithExplicitKinds :: Type -> Type -> CtOrigin -> Bool
shouldPprWithExplicitKinds _ty1 _ty2 (TypeEqOrigin { uo_actual = act
                                                   , uo_expected = exp
                                                   , uo_visible = vis })
  | not vis   = True                  -- See tests T15870, T16204c
  | otherwise = tcEqTypeVis act exp   -- See tests T9171, T9144.
shouldPprWithExplicitKinds ty1 ty2 _ct
  = tcEqTypeVis ty1 ty2

{- Note [Insoluble occurs check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] a ~ [a],  [W] a ~ [a] (#13674).  The Given is insoluble
so we don't use it for rewriting.  The Wanted is also insoluble, and
we don't solve it from the Given.  It's very confusing to say
    Cannot solve a ~ [a] from given constraints a ~ [a]

And indeed even thinking about the Givens is silly; [W] a ~ [a] is
just as insoluble as Int ~ Bool.

Conclusion: if there's an insoluble occurs check (cteInsolubleOccurs)
then report it directly, not in the "cannot deduce X from Y" form.
This is done in misMatchOrCND (via the insoluble_occurs_check arg)

(NB: there are potentially-soluble ones, like (a ~ F a b), and we don't
want to be as draconian with them.)
-}

sameOccExtras :: TcType -> TcType -> [TcSolverReportInfo]
-- See Note [Disambiguating (X ~ X) errors]
sameOccExtras ty1 ty2
  | Just (tc1, _) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, _) <- tcSplitTyConApp_maybe ty2
  , let n1 = tyConName tc1
        n2 = tyConName tc2
        same_occ = nameOccName n1                   == nameOccName n2
        same_pkg = moduleUnit (nameModule n1) == moduleUnit (nameModule n2)
  , n1 /= n2   -- Different Names
  , same_occ   -- but same OccName
  = [SameOcc same_pkg n1 n2]
  | otherwise
  = []

{- Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The OutsideIn algorithm rejects GADT programs that don't have a principal
type, and indeed some that do.  Example:
   data T a where
     MkT :: Int -> T Int

   f (MkT n) = n

Does this have type f :: T a -> a, or f :: T a -> Int?
The error that shows up tends to be an attempt to unify an
untouchable type variable.  So suggestAddSig sees if the offending
type variable is bound by an *inferred* signature, and suggests
adding a declared signature instead.

More specifically, we suggest adding a type sig if we have p ~ ty, and
p is a skolem bound by an InferSkol.  Those skolems were created from
unification variables in simplifyInfer.  Why didn't we unify?  It must
have been because of an intervening GADT or existential, making it
untouchable. Either way, a type signature would help.  For GADTs, it
might make it typeable; for existentials the attempt to write a
signature will fail -- or at least will produce a better error message
next time

This initially came up in #8968, concerning pattern synonyms.

Note [Disambiguating (X ~ X) errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #8278

Note [Reporting occurs-check errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (a ~ [a]), if 'a' is a rigid type variable bound by a user-supplied
type signature, then the best thing is to report that we can't unify
a with [a], because a is a skolem variable.  That avoids the confusing
"occur-check" error message.

But nowadays when inferring the type of a function with no type signature,
even if there are errors inside, we still generalise its signature and
carry on. For example
   f x = x:x
Here we will infer something like
   f :: forall a. a -> [a]
with a deferred error of (a ~ [a]).  So in the deferred unsolved constraint
'a' is now a skolem, but not one bound by the programmer in the context!
Here we really should report an occurs check.

So isUserSkolem distinguishes the two.

Note [Non-injective type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very confusing to get a message like
     Couldn't match expected type `Depend s'
            against inferred type `Depend s1'
so mkTyFunInfoMsg adds:
       NB: `Depend' is type function, and hence may not be injective

Warn of loopy local equalities that were dropped.


************************************************************************
*                                                                      *
                 Type-class errors
*                                                                      *
************************************************************************
-}

mkDictErr :: HasDebugCallStack => SolverReportErrCtxt -> [ErrorItem] -> TcM SolverReport
mkDictErr ctxt orig_items
  = assert (not (null items)) $
    do { inst_envs <- tcGetInstEnvs
       ; let min_items = elim_superclasses items
             lookups = map (lookup_cls_inst inst_envs) min_items
             (no_inst_items, overlap_items) = partition is_no_inst lookups

       -- Report definite no-instance errors,
       -- or (iff there are none) overlap errors
       -- But we report only one of them (hence 'head') because they all
       -- have the same source-location origin, to try avoid a cascade
       -- of error from one location
       ; err <- mk_dict_err ctxt (head (no_inst_items ++ overlap_items))
       ; return $ important ctxt err }
  where
    filtered_items = filter (not . ei_suppress) orig_items
    items | null filtered_items = orig_items  -- all suppressed, but must report
                                              -- something for -fdefer-type-errors
          | otherwise           = filtered_items  -- common case

    no_givens = null (getUserGivens ctxt)

    is_no_inst (item, (matches, unifiers, _))
      =  no_givens
      && null matches
      && (nullUnifiers unifiers || all (not . isAmbiguousTyVar) (tyCoVarsOfTypeList (errorItemPred item)))

    lookup_cls_inst inst_envs item
      = (item, lookupInstEnv True inst_envs clas tys)
      where
        (clas, tys) = getClassPredTys (errorItemPred item)


    -- When simplifying [W] Ord (Set a), we need
    --    [W] Eq a, [W] Ord a
    -- but we really only want to report the latter
    elim_superclasses items = mkMinimalBySCs errorItemPred items

-- Note [mk_dict_err]
-- ~~~~~~~~~~~~~~~~~~~
-- Different dictionary error messages are reported depending on the number of
-- matches and unifiers:
--
--   - No matches, regardless of unifiers: report "No instance for ...".
--   - Two or more matches, regardless of unifiers: report "Overlapping instances for ...",
--     and show the matching and unifying instances.
--   - One match, one or more unifiers: report "Overlapping instances for", show the
--     matching and unifying instances, and say "The choice depends on the instantion of ...,
--     and the result of evaluating ...".
mk_dict_err :: HasCallStack => SolverReportErrCtxt -> (ErrorItem, ClsInstLookupResult)
            -> TcM TcSolverReportMsg
-- Report an overlap error if this class constraint results
-- from an overlap (returning Left clas), otherwise return (Right pred)
mk_dict_err ctxt (item, (matches, unifiers, unsafe_overlapped))
  | null matches  -- No matches but perhaps several unifiers
  = do { (_, rel_binds, item) <- relevantBindings True ctxt item
       ; candidate_insts <- get_candidate_instances
       ; (imp_errs, field_suggestions) <- record_field_suggestions
       ; return (cannot_resolve_msg item candidate_insts rel_binds imp_errs field_suggestions) }

  | null unsafe_overlapped   -- Some matches => overlap errors
  = return $ overlap_msg

  | otherwise
  = return $ safe_haskell_msg
  where
    orig          = errorItemOrigin item
    pred          = errorItemPred item
    (clas, tys)   = getClassPredTys pred
    ispecs        = [ispec | (ispec, _) <- matches]
    unsafe_ispecs = [ispec | (ispec, _) <- unsafe_overlapped]

    get_candidate_instances :: TcM [ClsInst]
    -- See Note [Report candidate instances]
    get_candidate_instances
      | [ty] <- tys   -- Only try for single-parameter classes
      = do { instEnvs <- tcGetInstEnvs
           ; return (filter (is_candidate_inst ty)
                            (classInstances instEnvs clas)) }
      | otherwise = return []

    is_candidate_inst ty inst -- See Note [Report candidate instances]
      | [other_ty] <- is_tys inst
      , Just (tc1, _) <- tcSplitTyConApp_maybe ty
      , Just (tc2, _) <- tcSplitTyConApp_maybe other_ty
      = let n1 = tyConName tc1
            n2 = tyConName tc2
            different_names = n1 /= n2
            same_occ_names = nameOccName n1 == nameOccName n2
        in different_names && same_occ_names
      | otherwise = False

    -- See Note [Out-of-scope fields with -XOverloadedRecordDot]
    record_field_suggestions :: TcM ([ImportError], [GhcHint])
    record_field_suggestions = flip (maybe $ return ([], noHints)) record_field $ \name ->
       do { glb_env <- getGlobalRdrEnv
          ; lcl_env <- getLocalRdrEnv
          ; if occ_name_in_scope glb_env lcl_env name
            then return ([], noHints)
            else do { dflags   <- getDynFlags
                    ; imp_info <- getImports
                    ; curr_mod <- getModule
                    ; hpt      <- getHpt
                    ; return (unknownNameSuggestions WL_RecField dflags hpt curr_mod
                        glb_env emptyLocalRdrEnv imp_info (mkRdrUnqual name)) } }

    occ_name_in_scope glb_env lcl_env occ_name = not $
      null (lookupGlobalRdrEnv glb_env occ_name) &&
      isNothing (lookupLocalRdrOcc lcl_env occ_name)

    record_field = case orig of
      HasFieldOrigin name -> Just (mkVarOccFS name)
      _                   -> Nothing

    cannot_resolve_msg :: ErrorItem -> [ClsInst] -> RelevantBindings
                       -> [ImportError] -> [GhcHint] -> TcSolverReportMsg
    cannot_resolve_msg item candidate_insts binds imp_errs field_suggestions
      = CannotResolveInstance item (getPotentialUnifiers unifiers) candidate_insts imp_errs field_suggestions binds

    -- Overlap errors.
    overlap_msg, safe_haskell_msg :: TcSolverReportMsg
    -- Normal overlap error
    overlap_msg
      = assert (not (null matches)) $ OverlappingInstances item ispecs (getPotentialUnifiers unifiers)

    -- Overlap error because of Safe Haskell (first
    -- match should be the most specific match)
    safe_haskell_msg
     = assert (matches `lengthIs` 1 && not (null unsafe_ispecs)) $
       UnsafeOverlap item ispecs unsafe_ispecs

{- Note [Report candidate instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved (Num Int), where `Int` is not the Prelude Int,
but comes from some other module, then it may be helpful to point out
that there are some similarly named instances elsewhere.  So we get
something like
    No instance for (Num Int) arising from the literal ‘3’
    There are instances for similar types:
      instance Num GHC.Types.Int -- Defined in ‘GHC.Num’
Discussion in #9611.

Note [Highlighting ambiguous type variables]
~-------------------------------------------
When we encounter ambiguous type variables (i.e. type variables
that remain metavariables after type inference), we need a few more
conditions before we can reason that *ambiguity* prevents constraints
from being solved:
  - We can't have any givens, as encountering a typeclass error
    with given constraints just means we couldn't deduce
    a solution satisfying those constraints and as such couldn't
    bind the type variable to a known type.
  - If we don't have any unifiers, we don't even have potential
    instances from which an ambiguity could arise.
  - Lastly, I don't want to mess with error reporting for
    unknown runtime types so we just fall back to the old message there.
Once these conditions are satisfied, we can safely say that ambiguity prevents
the constraint from being solved.

Note [Out-of-scope fields with -XOverloadedRecordDot]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XOverloadedRecordDot, when a field isn't in scope, the error that appears
is produces here, and it says
    No instance for (GHC.Record.HasField "<fieldname>" ...).

Additionally, though, we want to suggest similar field names that are in scope
or could be in scope with different import lists.

However, we can still get an error about a missing HasField instance when a
field is in scope (if the types are wrong), and so it's important that we don't
suggest similar names here if the record field is in scope, either qualified or
unqualified, since qualification doesn't matter for -XOverloadedRecordDot.

Example:

    import Data.Monoid (Alt(..))

    foo = undefined.getAll

results in

     No instance for (GHC.Records.HasField "getAll" r0 a0)
        arising from selecting the field ‘getAll’
      Perhaps you meant ‘getAlt’ (imported from Data.Monoid)
      Perhaps you want to add ‘getAll’ to the import list
      in the import of ‘Data.Monoid’
-}

{-
Note [Kind arguments in error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (#9171)

    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’

The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.

To mitigate this, GHC attempts to enable the -fprint-explicit-kinds flag
whenever any error message arises due to a kind mismatch. This means that
the above error message would instead be displayed as:

    Couldn't match expected type
                  ‘GetParam @* @k2 @* Base (GetParam @* @* @k2 Base Int)’
                with actual type
                  ‘GetParam @* @k20 @* Base (GetParam @* @* @k20 Base Int)’

Which makes it clearer that the culprit is the mismatch between `k2` and `k20`.
-}

-----------------------
-- relevantBindings looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.
--
-- We always remove closed top-level bindings, though,
-- since they are never relevant (cf #8233)

relevantBindings :: Bool  -- True <=> filter by tyvar; False <=> no filtering
                          -- See #8191
                 -> SolverReportErrCtxt -> ErrorItem
                 -> TcM (SolverReportErrCtxt, RelevantBindings, ErrorItem)
-- Also returns the zonked and tidied CtOrigin of the constraint
relevantBindings want_filtering ctxt item
  = do { traceTc "relevantBindings" (ppr item)
       ; (env1, tidy_orig) <- zonkTidyOrigin (cec_tidy ctxt) (ctLocOrigin loc)

             -- For *kind* errors, report the relevant bindings of the
             -- enclosing *type* equality, because that's more useful for the programmer
       ; let extra_tvs = case tidy_orig of
                             KindEqOrigin t1 t2 _ _ -> tyCoVarsOfTypes [t1,t2]
                             _                      -> emptyVarSet
             ct_fvs = tyCoVarsOfType (errorItemPred item) `unionVarSet` extra_tvs

             -- Put a zonked, tidied CtOrigin into the ErrorItem
             loc'   = setCtLocOrigin loc tidy_orig
             item'  = item { ei_loc = loc' }

       ; (env2, lcl_name_cache) <- zonkTidyTcLclEnvs env1 [lcl_env]

       ; relev_bds <- relevant_bindings want_filtering lcl_env lcl_name_cache ct_fvs
       ; let ctxt'  = ctxt { cec_tidy = env2 }
       ; return (ctxt', relev_bds, item') }
  where
    loc     = errorItemCtLoc item
    lcl_env = ctLocEnv loc

-- slightly more general version, to work also with holes
relevant_bindings :: Bool
                  -> TcLclEnv
                  -> NameEnv Type -- Cache of already zonked and tidied types
                  -> TyCoVarSet
                  -> TcM RelevantBindings
relevant_bindings want_filtering lcl_env lcl_name_env ct_tvs
  = do { dflags <- getDynFlags
       ; traceTc "relevant_bindings" $
           vcat [ ppr ct_tvs
                , pprWithCommas id [ ppr id <+> dcolon <+> ppr (idType id)
                                   | TcIdBndr id _ <- tcl_bndrs lcl_env ]
                , pprWithCommas id
                    [ ppr id | TcIdBndr_ExpType id _ _ <- tcl_bndrs lcl_env ] ]

       ; go dflags (maxRelevantBinds dflags)
                    emptyVarSet (RelevantBindings [] False)
                    (removeBindingShadowing $ tcl_bndrs lcl_env)
         -- tcl_bndrs has the innermost bindings first,
         -- which are probably the most relevant ones
  }
  where
    run_out :: Maybe Int -> Bool
    run_out Nothing = False
    run_out (Just n) = n <= 0

    dec_max :: Maybe Int -> Maybe Int
    dec_max = fmap (\n -> n - 1)


    go :: DynFlags -> Maybe Int -> TcTyVarSet
       -> RelevantBindings
       -> [TcBinder]
       -> TcM RelevantBindings
    go _ _ _ (RelevantBindings bds discards) []
      = return $ RelevantBindings (reverse bds) discards
    go dflags n_left tvs_seen rels@(RelevantBindings bds discards) (tc_bndr : tc_bndrs)
      = case tc_bndr of
          TcTvBndr {} -> discard_it
          TcIdBndr id top_lvl -> go2 (idName id) top_lvl
          TcIdBndr_ExpType name et top_lvl ->
            do { mb_ty <- readExpType_maybe et
                   -- et really should be filled in by now. But there's a chance
                   -- it hasn't, if, say, we're reporting a kind error en route to
                   -- checking a term. See test indexed-types/should_fail/T8129
                   -- Or we are reporting errors from the ambiguity check on
                   -- a local type signature
               ; case mb_ty of
                   Just _ty -> go2 name top_lvl
                   Nothing -> discard_it  -- No info; discard
               }
      where
        discard_it = go dflags n_left tvs_seen rels tc_bndrs
        go2 id_name top_lvl
          = do { let tidy_ty = case lookupNameEnv lcl_name_env id_name of
                                  Just tty -> tty
                                  Nothing -> pprPanic "relevant_bindings" (ppr id_name)
               ; traceTc "relevantBindings 1" (ppr id_name <+> dcolon <+> ppr tidy_ty)
               ; let id_tvs = tyCoVarsOfType tidy_ty
                     bd = (id_name, tidy_ty)
                     new_seen = tvs_seen `unionVarSet` id_tvs

               ; if (want_filtering && not (hasPprDebug dflags)
                                    && id_tvs `disjointVarSet` ct_tvs)
                          -- We want to filter out this binding anyway
                          -- so discard it silently
                 then discard_it

                 else if isTopLevel top_lvl && not (isNothing n_left)
                          -- It's a top-level binding and we have not specified
                          -- -fno-max-relevant-bindings, so discard it silently
                 then discard_it

                 else if run_out n_left && id_tvs `subVarSet` tvs_seen
                          -- We've run out of n_left fuel and this binding only
                          -- mentions already-seen type variables, so discard it
                 then go dflags n_left tvs_seen (RelevantBindings bds True) -- Record that we have now discarded something
                         tc_bndrs

                          -- Keep this binding, decrement fuel
                 else go dflags (dec_max n_left) new_seen
                         (RelevantBindings (bd:bds) discards) tc_bndrs }

-----------------------
warnDefaulting :: TcTyVar -> [Ct] -> Type -> TcM ()
warnDefaulting _ [] _
  = panic "warnDefaulting: empty Wanteds"
warnDefaulting the_tv wanteds@(ct:_) default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
            -- don't want to report all the superclass constraints, which
            -- add unhelpful clutter
       ; let filtered = filter (not . isWantedSuperclassOrigin . ctOrigin) wanteds
             tidy_env = tidyFreeTyCoVars env0 $
                        tyCoVarsOfCtsList (listToBag filtered)
             tidy_wanteds = map (tidyCt tidy_env) filtered
             tidy_tv = lookupVarEnv (snd tidy_env) the_tv
             diag = TcRnWarnDefaulting tidy_wanteds tidy_tv default_ty
             loc = ctLoc ct
       ; setCtLocM loc $ diagnosticTc warn_default diag }

{-
Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in GHC.Runtime.Heap.Inspect.zonkRTTIType.
-}

{-**********************************************************************
*                                                                      *
                      GHC API helper functions
*                                                                      *
**********************************************************************-}

-- | If the 'TcSolverReportMsg' is a type mismatch between
-- an actual and an expected type, return the actual and expected types
-- (in that order).
--
-- Prefer using this over manually inspecting the 'TcSolverReportMsg' datatype
-- if you just want this information, as the datatype itself is subject to change
-- across GHC versions.
solverReportMsg_ExpectedActuals :: TcSolverReportMsg -> [(Type, Type)]
solverReportMsg_ExpectedActuals
  = \case
    TcReportWithInfo msg infos ->
      solverReportMsg_ExpectedActuals msg
      ++ (solverReportInfo_ExpectedActuals =<< toList infos)
    Mismatch { mismatch_ty1 = exp, mismatch_ty2 = act } ->
      [(exp, act)]
    KindMismatch { kmismatch_expected = exp, kmismatch_actual = act } ->
      [(exp, act)]
    TypeEqMismatch { teq_mismatch_expected = exp, teq_mismatch_actual = act } ->
      [(exp,act)]
    _ -> []

-- | Retrieves all @"expected"/"actual"@ messages from a 'TcSolverReportInfo'.
--
-- Prefer using this over inspecting the 'TcSolverReportInfo' datatype if
-- you just need this information, as the datatype itself is subject to change
-- across GHC versions.
solverReportInfo_ExpectedActuals :: TcSolverReportInfo -> [(Type, Type)]
solverReportInfo_ExpectedActuals
  = \case
    ExpectedActual { ea_expected = exp, ea_actual = act } ->
      [(exp, act)]
    ExpectedActualAfterTySynExpansion
      { ea_expanded_expected = exp, ea_expanded_actual = act } ->
      [(exp, act)]
    _ -> []
