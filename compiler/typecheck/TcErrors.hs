{-# LANGUAGE CPP, ScopedTypeVariables #-}

module TcErrors(
       reportUnsolved, reportAllUnsolved, warnAllUnsolved,
       warnDefaulting,

       solverDepthErrorTcS
  ) where

#include "HsVersions.h"

import TcRnTypes
import TcRnMonad
import TcMType
import TcUnify( occCheckForErrors, OccCheckResult(..) )
import TcType
import RnUnbound ( unknownNameSuggestions )
import Type
import TyCoRep
import Kind
import Unify            ( tcMatchTys )
import Module
import FamInst
import FamInstEnv       ( flattenTys )
import Inst
import InstEnv
import TyCon
import Class
import DataCon
import TcEvidence
import HsExpr  ( UnboundVar(..) )
import HsBinds ( PatSynBind(..) )
import Name
import RdrName ( lookupGlobalRdrEnv, lookupGRE_Name, GlobalRdrEnv
               , mkRdrUnqual, isLocalGRE, greSrcSpan, pprNameProvenance
               , GlobalRdrElt (..), globalRdrEnvElts )
import PrelNames ( typeableClassName, hasKey, liftedRepDataConKey )
import Id
import Var
import VarSet
import VarEnv
import NameSet
import Bag
import ErrUtils         ( ErrMsg, errDoc, pprLocErrMsg )
import BasicTypes
import ConLike          ( ConLike(..), conLikeWrapId_maybe )
import Util
import HscTypes (HscEnv, lookupTypeHscEnv, TypeEnv, lookupTypeEnv )
import NameEnv (lookupNameEnv)
import FastString
import Outputable
import SrcLoc
import DynFlags
import ListSetOps       ( equivClasses )
import Maybes
import Pair
import qualified GHC.LanguageExtensions as LangExt
import FV ( fvVarList, unionFV )

import Control.Monad    ( when )
import Data.List        ( partition, mapAccumL, nub, sortBy, unfoldr )
import qualified Data.Set as Set

#if __GLASGOW_HASKELL__ > 710
import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup
#endif


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
we run into a type mismatch in TcUnify, we normally just emit an error. But it
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
in TcErrors. TcErrors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.
-}

-- | Report unsolved goals as errors or warnings. We may also turn some into
-- deferred run-time errors if `-fdefer-type-errors` is on.
reportUnsolved :: WantedConstraints -> TcM (Bag EvBind)
reportUnsolved wanted
  = do { binds_var <- newTcEvBinds
       ; defer_errors <- goptM Opt_DeferTypeErrors
       ; warn_errors <- woptM Opt_WarnDeferredTypeErrors -- implement #10283
       ; let type_errors | not defer_errors = TypeError
                         | warn_errors      = TypeWarn
                         | otherwise        = TypeDefer

       ; defer_holes <- goptM Opt_DeferTypedHoles
       ; warn_holes  <- woptM Opt_WarnTypedHoles
       ; let expr_holes | not defer_holes = HoleError
                        | warn_holes      = HoleWarn
                        | otherwise       = HoleDefer

       ; partial_sigs      <- xoptM LangExt.PartialTypeSignatures
       ; warn_partial_sigs <- woptM Opt_WarnPartialTypeSignatures
       ; let type_holes | not partial_sigs  = HoleError
                        | warn_partial_sigs = HoleWarn
                        | otherwise         = HoleDefer

       ; defer_out_of_scope <- goptM Opt_DeferOutOfScopeVariables
       ; warn_out_of_scope <- woptM Opt_WarnDeferredOutOfScopeVariables
       ; let out_of_scope_holes | not defer_out_of_scope = HoleError
                                | warn_out_of_scope      = HoleWarn
                                | otherwise              = HoleDefer

       ; report_unsolved binds_var False type_errors expr_holes
          type_holes out_of_scope_holes wanted

       ; ev_binds <- getTcEvBindsMap binds_var
       ; return (evBindMapBinds ev_binds)}

-- | Report *all* unsolved goals as errors, even if -fdefer-type-errors is on
-- However, do not make any evidence bindings, because we don't
-- have any convenient place to put them.
-- See Note [Deferring coercion errors to runtime]
-- Used by solveEqualities for kind equalities
--      (see Note [Fail fast on kind errors] in TcSimplify]
-- and for simplifyDefault.
reportAllUnsolved :: WantedConstraints -> TcM ()
reportAllUnsolved wanted
  = do { ev_binds <- newTcEvBinds
       ; report_unsolved ev_binds False TypeError
                         HoleError HoleError HoleError wanted }

-- | Report all unsolved goals as warnings (but without deferring any errors to
-- run-time). See Note [Safe Haskell Overlapping Instances Implementation] in
-- TcSimplify
warnAllUnsolved :: WantedConstraints -> TcM ()
warnAllUnsolved wanted
  = do { ev_binds <- newTcEvBinds
       ; report_unsolved ev_binds True TypeWarn
                         HoleWarn HoleWarn HoleWarn wanted }

-- | Report unsolved goals as errors or warnings.
report_unsolved :: EvBindsVar        -- cec_binds
                -> Bool              -- Errors as warnings
                -> TypeErrorChoice   -- Deferred type errors
                -> HoleChoice        -- Expression holes
                -> HoleChoice        -- Type holes
                -> HoleChoice        -- Out of scope holes
                -> WantedConstraints -> TcM ()
report_unsolved mb_binds_var err_as_warn type_errors expr_holes
    type_holes out_of_scope_holes wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do { traceTc "reportUnsolved (before zonking and tidying)" (ppr wanted)

       ; wanted <- zonkWC wanted   -- Zonk to reveal all information
       ; env0 <- tcInitTidyEnv
            -- If we are deferring we are going to need /all/ evidence around,
            -- including the evidence produced by unflattening (zonkWC)
       ; let tidy_env = tidyFreeTyCoVars env0 free_tvs
             free_tvs = tyCoVarsOfWCList wanted

       ; traceTc "reportUnsolved (after zonking):" $
         vcat [ text "Free tyvars:" <+> pprTyVars free_tvs
              , text "Wanted:" <+> ppr wanted ]

       ; warn_redundant <- woptM Opt_WarnRedundantConstraints
       ; let err_ctxt = CEC { cec_encl  = []
                            , cec_tidy  = tidy_env
                            , cec_defer_type_errors = type_errors
                            , cec_errors_as_warns = err_as_warn
                            , cec_expr_holes = expr_holes
                            , cec_type_holes = type_holes
                            , cec_out_of_scope_holes = out_of_scope_holes
                            , cec_suppress = False -- See Note [Suppressing error messages]
                            , cec_warn_redundant = warn_redundant
                            , cec_binds    = mb_binds_var }

       ; tc_lvl <- getTcLevel
       ; reportWanteds err_ctxt tc_lvl wanted }

--------------------------------------------
--      Internal functions
--------------------------------------------

-- | An error Report collects messages categorised by their importance.
-- See Note [Error report] for details.
data Report
  = Report { report_important :: [SDoc]
           , report_relevant_bindings :: [SDoc]
           , report_valid_substitutions :: [SDoc]
           }

instance Outputable Report where   -- Debugging only
  ppr (Report { report_important = imp
              , report_relevant_bindings = rel
              , report_valid_substitutions = val })
    = vcat [ text "important:" <+> vcat imp
           , text "relevant:"  <+> vcat rel
           , text "valid:"  <+> vcat val ]

{- Note [Error report]
The idea is that error msgs are divided into three parts: the main msg, the
context block (\"In the second argument of ...\"), and the relevant bindings
block, which are displayed in that order, with a mark to divide them.  The
idea is that the main msg ('report_important') varies depending on the error
in question, but context and relevant bindings are always the same, which
should simplify visual parsing.

The context is added when the the Report is passed off to 'mkErrorReport'.
Unfortunately, unlike the context, the relevant bindings are added in
multiple places so they have to be in the Report.
-}

#if __GLASGOW_HASKELL__ > 710
instance Semigroup Report where
    Report a1 b1 c1 <> Report a2 b2 c2 = Report (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)
#endif

instance Monoid Report where
    mempty = Report [] [] []
    mappend (Report a1 b1 c1) (Report a2 b2 c2)
      = Report (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

-- | Put a doc into the important msgs block.
important :: SDoc -> Report
important doc = mempty { report_important = [doc] }

-- | Put a doc into the relevant bindings block.
relevant_bindings :: SDoc -> Report
relevant_bindings doc = mempty { report_relevant_bindings = [doc] }

-- | Put a doc into the valid substitutions block.
valid_substitutions :: SDoc -> Report
valid_substitutions docs = mempty { report_valid_substitutions = [docs] }

data TypeErrorChoice   -- What to do for type errors found by the type checker
  = TypeError     -- A type error aborts compilation with an error message
  | TypeWarn      -- A type error is deferred to runtime, plus a compile-time warning
  | TypeDefer     -- A type error is deferred to runtime; no error or warning at compile time

data HoleChoice
  = HoleError     -- A hole is a compile-time error
  | HoleWarn      -- Defer to runtime, emit a compile-time warning
  | HoleDefer     -- Defer to runtime, no warning

instance Outputable HoleChoice where
  ppr HoleError = text "HoleError"
  ppr HoleWarn  = text "HoleWarn"
  ppr HoleDefer = text "HoleDefer"

instance Outputable TypeErrorChoice  where
  ppr TypeError = text "TypeError"
  ppr TypeWarn  = text "TypeWarn"
  ppr TypeDefer = text "TypeDefer"

data ReportErrCtxt
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                                       --   (innermost first)
                                       -- ic_skols and givens are tidied, rest are not
          , cec_tidy  :: TidyEnv

          , cec_binds :: EvBindsVar    -- Make some errors (depending on cec_defer)
                                       -- into warnings, and emit evidence bindings
                                       -- into 'cec_binds' for unsolved constraints

          , cec_errors_as_warns :: Bool   -- Turn all errors into warnings
                                          -- (except for Holes, which are
                                          -- controlled by cec_type_holes and
                                          -- cec_expr_holes)
          , cec_defer_type_errors :: TypeErrorChoice -- Defer type errors until runtime

          -- cec_expr_holes is a union of:
          --   cec_type_holes - a set of typed holes: '_', '_a', '_foo'
          --   cec_out_of_scope_holes - a set of variables which are
          --                            out of scope: 'x', 'y', 'bar'
          , cec_expr_holes :: HoleChoice           -- Holes in expressions
          , cec_type_holes :: HoleChoice           -- Holes in types
          , cec_out_of_scope_holes :: HoleChoice   -- Out of scope holes

          , cec_warn_redundant :: Bool    -- True <=> -Wredundant-constraints

          , cec_suppress :: Bool    -- True <=> More important errors have occurred,
                                    --          so create bindings if need be, but
                                    --          don't issue any more errors/warnings
                                    -- See Note [Suppressing error messages]
      }

instance Outputable ReportErrCtxt where
  ppr (CEC { cec_binds              = bvar
           , cec_errors_as_warns    = ew
           , cec_defer_type_errors  = dte
           , cec_expr_holes         = eh
           , cec_type_holes         = th
           , cec_out_of_scope_holes = osh
           , cec_warn_redundant     = wr
           , cec_suppress           = sup })
    = text "CEC" <+> braces (vcat
         [ text "cec_binds"              <+> equals <+> ppr bvar
         , text "cec_errors_as_warns"    <+> equals <+> ppr ew
         , text "cec_defer_type_errors"  <+> equals <+> ppr dte
         , text "cec_expr_holes"         <+> equals <+> ppr eh
         , text "cec_type_holes"         <+> equals <+> ppr th
         , text "cec_out_of_scope_holes" <+> equals <+> ppr osh
         , text "cec_warn_redundant"     <+> equals <+> ppr wr
         , text "cec_suppress"           <+> equals <+> ppr sup ])

{-
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
-}

reportImplic :: ReportErrCtxt -> Implication -> TcM ()
reportImplic ctxt implic@(Implic { ic_skols = tvs, ic_given = given
                                 , ic_wanted = wanted, ic_binds = evb
                                 , ic_status = status, ic_info = info
                                 , ic_env = tcl_env, ic_tclvl = tc_lvl })
  | BracketSkol <- info
  , not insoluble
  = return ()        -- For Template Haskell brackets report only
                     -- definite errors. The whole thing will be re-checked
                     -- later when we plug it in, and meanwhile there may
                     -- certainly be un-satisfied constraints

  | otherwise
  = do { traceTc "reportImplic" (ppr implic')
       ; reportWanteds ctxt' tc_lvl wanted
       ; when (cec_warn_redundant ctxt) $
         warnRedundantConstraints ctxt' tcl_env info' dead_givens }
  where
    insoluble    = isInsolubleStatus status
    (env1, tvs') = mapAccumL tidyTyCoVarBndr (cec_tidy ctxt) tvs
    info'        = tidySkolemInfo env1 info
    implic' = implic { ic_skols = tvs'
                     , ic_given = map (tidyEvVar env1) given
                     , ic_info  = info' }
    ctxt' = ctxt { cec_tidy     = env1
                 , cec_encl     = implic' : cec_encl ctxt

                 , cec_suppress = insoluble || cec_suppress ctxt
                      -- Suppress inessential errors if there
                      -- are are insolubles anywhere in the
                      -- tree rooted here, or we've come across
                      -- a suppress-worthy constraint higher up (Trac #11541)

                 , cec_binds    = evb }

    dead_givens = case status of
                    IC_Solved { ics_dead = dead } -> dead
                    _                             -> []

warnRedundantConstraints :: ReportErrCtxt -> TcLclEnv -> SkolemInfo -> [EvVar] -> TcM ()
-- See Note [Tracking redundant constraints] in TcSimplify
warnRedundantConstraints ctxt env info ev_vars
 | null redundant_evs
 = return ()

 | SigSkol {} <- info
 = setLclEnv env $  -- We want to add "In the type signature for f"
                    -- to the error context, which is a bit tiresome
   addErrCtxt (text "In" <+> ppr info) $
   do { env <- getLclEnv
      ; msg <- mkErrorReport ctxt env (important doc)
      ; reportWarning (Reason Opt_WarnRedundantConstraints) msg }

 | otherwise  -- But for InstSkol there already *is* a surrounding
              -- "In the instance declaration for Eq [a]" context
              -- and we don't want to say it twice. Seems a bit ad-hoc
 = do { msg <- mkErrorReport ctxt env (important doc)
      ; reportWarning (Reason Opt_WarnRedundantConstraints) msg }
 where
   doc = text "Redundant constraint" <> plural redundant_evs <> colon
         <+> pprEvVarTheta redundant_evs

   redundant_evs = case info of -- See Note [Redundant constraints in instance decls]
                     InstSkol -> filterOut improving ev_vars
                     _        -> ev_vars

   improving ev_var = any isImprovementPred $
                      transSuperClasses (idType ev_var)

{- Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations, we don't report unused givens if
they can give rise to improvement.  Example (Trac #10100):
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

reportWanteds :: ReportErrCtxt -> TcLevel -> WantedConstraints -> TcM ()
reportWanteds ctxt tc_lvl (WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  = do { traceTc "reportWanteds" (vcat [ text "Simples =" <+> ppr simples
                                       , text "Insols =" <+> ppr insols
                                       , text "Suppress =" <+> ppr (cec_suppress ctxt)])
       ; let tidy_cts = bagToList (mapBag (tidyCt env) (insols `unionBags` simples))

         -- First deal with things that are utterly wrong
         -- Like Int ~ Bool (incl nullary TyCons)
         -- or  Int ~ t a   (AppTy on one side)
         -- These /ones/ are not suppressed by the incoming context
       ; let ctxt_for_insols = ctxt { cec_suppress = False }
       ; (ctxt1, cts1) <- tryReporters ctxt_for_insols report1 tidy_cts

         -- Now all the other constraints.  We suppress errors here if
         -- any of the first batch failed, or if the enclosing context
         -- says to suppress
       ; let ctxt2 = ctxt { cec_suppress = cec_suppress ctxt || cec_suppress ctxt1 }
       ; (_, leftovers) <- tryReporters ctxt2 report2 cts1
       ; MASSERT2( null leftovers, ppr leftovers )

            -- All the Derived ones have been filtered out of simples
            -- by the constraint solver. This is ok; we don't want
            -- to report unsolved Derived goals as errors
            -- See Note [Do not report derived but soluble errors]

     ; mapBagM_ (reportImplic ctxt2) implics }
            -- NB ctxt1: don't suppress inner insolubles if there's only a
            -- wanted insoluble here; but do suppress inner insolubles
            -- if there's a *given* insoluble here (= inaccessible code)
 where
    env = cec_tidy ctxt

    -- report1: ones that should *not* be suppresed by
    --          an insoluble somewhere else in the tree
    -- It's crucial that anything that is considered insoluble
    -- (see TcRnTypes.trulyInsoluble) is caught here, otherwise
    -- we might suppress its error message, and proceed on past
    -- type checking to get a Lint error later
    report1 = [ ("custom_error", is_user_type_error,True, mkUserTypeErrorReporter)
              , given_eq_spec
              , ("insoluble2 ty", utterly_wrong_ty, True, mkGroupReporter mkEqErr)
              , ("insoluble2_ki", utterly_wrong,    True, mkGroupReporter mkEqErr)
              , ("skolem eq1",    very_wrong,       True, mkSkolReporter)
              , ("skolem eq2",    skolem_eq,        True, mkSkolReporter)
              , ("non-tv eq",     non_tv_eq,        True, mkSkolReporter)
              , ("Out of scope",  is_out_of_scope,  True, mkHoleReporter)
              , ("Holes",         is_hole,          False, mkHoleReporter)

                  -- The only remaining equalities are alpha ~ ty,
                  -- where alpha is untouchable; and representational equalities
                  -- Prefer homogeneous equalities over hetero, because the
                  -- former might be holding up the latter.
                  -- See Note [Equalities with incompatible kinds] in TcCanonical
              , ("Homo eqs",      is_homo_equality, True,  mkGroupReporter mkEqErr)
              , ("Other eqs",     is_equality,      False, mkGroupReporter mkEqErr) ]

    -- report2: we suppress these if there are insolubles elsewhere in the tree
    report2 = [ ("Implicit params", is_ip,           False, mkGroupReporter mkIPErr)
              , ("Irreds",          is_irred,        False, mkGroupReporter mkIrredErr)
              , ("Dicts",           is_dict,         False, mkGroupReporter mkDictErr) ]

    -- rigid_nom_eq, rigid_nom_tv_eq,
    is_hole, is_dict,
      is_equality, is_ip, is_irred :: Ct -> PredTree -> Bool

    is_given_eq ct pred
       | EqPred {} <- pred = arisesFromGivens ct
       | otherwise         = False
       -- I think all given residuals are equalities

    -- Things like (Int ~N Bool)
    utterly_wrong _ (EqPred NomEq ty1 ty2) = isRigidTy ty1 && isRigidTy ty2
    utterly_wrong _ _                      = False

     -- Like utterly_wrong, but suppress derived kind equalities
    utterly_wrong_ty ct pred
      = utterly_wrong ct pred && case ctOrigin ct of
                                   KindEqOrigin {} -> False
                                   _               -> True

    -- Things like (a ~N Int)
    very_wrong _ (EqPred NomEq ty1 ty2) = isSkolemTy tc_lvl ty1 && isRigidTy ty2
    very_wrong _ _                      = False

    -- Things like (a ~N b) or (a  ~N  F Bool)
    skolem_eq _ (EqPred NomEq ty1 _) = isSkolemTy tc_lvl ty1
    skolem_eq _ _                    = False

    -- Things like (F a  ~N  Int)
    non_tv_eq _ (EqPred NomEq ty1 _) = not (isTyVarTy ty1)
    non_tv_eq _ _                    = False

    is_out_of_scope ct _ = isOutOfScopeCt ct
    is_hole         ct _ = isHoleCt ct

    is_user_type_error ct _ = isUserTypeErrorCt ct

    is_homo_equality _ (EqPred _ ty1 ty2) = typeKind ty1 `tcEqType` typeKind ty2
    is_homo_equality _ _                  = False

    is_equality _ (EqPred {}) = True
    is_equality _ _           = False

    is_dict _ (ClassPred {}) = True
    is_dict _ _              = False

    is_ip _ (ClassPred cls _) = isIPClass cls
    is_ip _ _                 = False

    is_irred _ (IrredPred {}) = True
    is_irred _ _              = False

    given_eq_spec = case find_gadt_match (cec_encl ctxt) of
       Just imp -> ("insoluble1a", is_given_eq, True,  mkGivenErrorReporter imp)
       Nothing  -> ("insoluble1b", is_given_eq, False, ignoreErrorReporter)
                  -- False means don't suppress subsequent errors
                  -- Reason: we don't report all given errors
                  --         (see mkGivenErrorReporter), and we should only suppress
                  --         subsequent errors if we actually report this one!
                  --         Trac #13446 is an example

    find_gadt_match [] = Nothing
    find_gadt_match (implic : implics)
      | PatSkol {} <- ic_info implic
      , not (ic_no_eqs implic)
      = Just implic
      | otherwise
      = find_gadt_match implics

---------------
isSkolemTy :: TcLevel -> Type -> Bool
-- The type is a skolem tyvar
isSkolemTy tc_lvl ty
  | Just tv <- getTyVar_maybe ty
  =  isSkolemTyVar tv
  || (isSigTyVar tv && isTouchableMetaTyVar tc_lvl tv)
     -- The last case is for touchable SigTvs
     -- we postpone untouchables to a latter test (too obscure)

  | otherwise
  = False

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isTypeFamilyTyCon tc -> Just tc
                      _ -> Nothing

--------------------------------------------
--      Reporters
--------------------------------------------

type Reporter
  = ReportErrCtxt -> [Ct] -> TcM ()
type ReporterSpec
  = ( String                     -- Name
    , Ct -> PredTree -> Bool     -- Pick these ones
    , Bool                       -- True <=> suppress subsequent reporters
    , Reporter)                  -- The reporter itself

mkSkolReporter :: Reporter
-- Suppress duplicates with either the same LHS, or same location
mkSkolReporter ctxt cts
  = mapM_ (reportGroup mkEqErr ctxt) (group cts)
  where
     group [] = []
     group (ct:cts) = (ct : yeses) : group noes
        where
          (yeses, noes) = partition (group_with ct) cts

     group_with ct1 ct2
       | EQ <- cmp_loc ct1 ct2 = True
       | eq_lhs_type   ct1 ct2 = True
       | otherwise             = False

mkHoleReporter :: Reporter
-- Reports errors one at a time
mkHoleReporter ctxt
  = mapM_ $ \ct -> do { err <- mkHoleError ctxt ct
                      ; maybeReportHoleError ctxt ct err
                      ; maybeAddDeferredHoleBinding ctxt err ct }

mkUserTypeErrorReporter :: Reporter
mkUserTypeErrorReporter ctxt
  = mapM_ $ \ct -> do { err <- mkUserTypeError ctxt ct
                      ; maybeReportError ctxt err
                      ; addDeferredBinding ctxt err ct }

mkUserTypeError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkUserTypeError ctxt ct = mkErrorMsgFromCt ctxt ct
                        $ important
                        $ pprUserTypeErrorTy
                        $ case getUserTypeErrorMsg ct of
                            Just msg -> msg
                            Nothing  -> pprPanic "mkUserTypeError" (ppr ct)


mkGivenErrorReporter :: Implication -> Reporter
-- See Note [Given errors]
mkGivenErrorReporter implic ctxt cts
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; dflags <- getDynFlags
       ; let ct' = setCtLoc ct (setCtLocEnv (ctLoc ct) (ic_env implic))
                   -- For given constraints we overwrite the env (and hence src-loc)
                  -- with one from the implication.  See Note [Inaccessible code]

             inaccessible_msg = hang (text "Inaccessible code in")
                                   2 (ppr (ic_info implic))
             report = important inaccessible_msg `mappend`
                      relevant_bindings binds_msg

       ; err <- mkEqErr_help dflags ctxt report ct'
                             Nothing ty1 ty2

       ; traceTc "mkGivenErrorReporter" (ppr ct)
       ; maybeReportError ctxt err }
  where
    (ct : _ )  = cts    -- Never empty
    (ty1, ty2) = getEqPredTys (ctPred ct)

ignoreErrorReporter :: Reporter
-- Discard Given errors that don't come from
-- a pattern match; maybe we should warn instead?
ignoreErrorReporter ctxt cts
  = do { traceTc "mkGivenErrorReporter no" (ppr cts $$ ppr (cec_encl ctxt))
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
#12466 for a long discussion on.  For example, if we aren't careful
we'll complain about
   f :: ((Int ~ Bool) => a -> a) -> Int
which arguably is OK.  It's more debatable for
   g :: (Int ~ Bool) => Int -> Int
but it's tricky to distinguish these cases to we don't report
either.

The bottom line is this: find_gadt_match looks for an encosing
pattern match which binds some equality constraints.  If we
find one, we report the insoluble Given.
-}

mkGroupReporter :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg)
                             -- Make error message for a group
                -> Reporter  -- Deal with lots of constraints
-- Group together errors from same location,
-- and report only the first (to avoid a cascade)
mkGroupReporter mk_err ctxt cts
  = mapM_ (reportGroup mk_err ctxt) (equivClasses cmp_loc cts)

eq_lhs_type :: Ct -> Ct -> Bool
eq_lhs_type ct1 ct2
  = case (classifyPredType (ctPred ct1), classifyPredType (ctPred ct2)) of
       (EqPred eq_rel1 ty1 _, EqPred eq_rel2 ty2 _) ->
         (eq_rel1 == eq_rel2) && (ty1 `eqType` ty2)
       _ -> pprPanic "mkSkolReporter" (ppr ct1 $$ ppr ct2)

cmp_loc :: Ct -> Ct -> Ordering
cmp_loc ct1 ct2 = ctLocSpan (ctLoc ct1) `compare` ctLocSpan (ctLoc ct2)

reportGroup :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg) -> ReportErrCtxt
            -> [Ct] -> TcM ()
reportGroup mk_err ctxt cts =
  case partition isMonadFailInstanceMissing cts of
        -- Only warn about missing MonadFail constraint when
        -- there are no other missing constraints!
        (monadFailCts, []) ->
            do { err <- mk_err ctxt monadFailCts
               ; reportWarning (Reason Opt_WarnMissingMonadFailInstances) err }

        (_, cts') -> do { err <- mk_err ctxt cts'
                        ; maybeReportError ctxt err
                            -- But see Note [Always warn with -fdefer-type-errors]
                        ; traceTc "reportGroup" (ppr cts')
                        ; mapM_ (addDeferredBinding ctxt err) cts' }
                            -- Add deferred bindings for all
                            -- Redundant if we are going to abort compilation,
                            -- but that's hard to know for sure, and if we don't
                            -- abort, we need bindings for all (e.g. Trac #12156)
  where
    isMonadFailInstanceMissing ct =
        case ctLocOrigin (ctLoc ct) of
            FailablePattern _pat -> True
            _otherwise           -> False

maybeReportHoleError :: ReportErrCtxt -> Ct -> ErrMsg -> TcM ()
maybeReportHoleError ctxt ct err
  -- When -XPartialTypeSignatures is on, warnings (instead of errors) are
  -- generated for holes in partial type signatures.
  -- Unless -fwarn_partial_type_signatures is not on,
  -- in which case the messages are discarded.
  | isTypeHoleCt ct
  = -- For partial type signatures, generate warnings only, and do that
    -- only if -fwarn_partial_type_signatures is on
    case cec_type_holes ctxt of
       HoleError -> reportError err
       HoleWarn  -> reportWarning (Reason Opt_WarnPartialTypeSignatures) err
       HoleDefer -> return ()

  -- Always report an error for out-of-scope variables
  -- Unless -fdefer-out-of-scope-variables is on,
  -- in which case the messages are discarded.
  -- See Trac #12170, #12406
  | isOutOfScopeCt ct
  = -- If deferring, report a warning only if -Wout-of-scope-variables is on
    case cec_out_of_scope_holes ctxt of
      HoleError -> reportError err
      HoleWarn  ->
        reportWarning (Reason Opt_WarnDeferredOutOfScopeVariables) err
      HoleDefer -> return ()

  -- Otherwise this is a typed hole in an expression,
  -- but not for an out-of-scope variable
  | otherwise
  = -- If deferring, report a warning only if -Wtyped-holes is on
    case cec_expr_holes ctxt of
       HoleError -> reportError err
       HoleWarn  -> reportWarning (Reason Opt_WarnTypedHoles) err
       HoleDefer -> return ()

maybeReportError :: ReportErrCtxt -> ErrMsg -> TcM ()
-- Report the error and/or make a deferred binding for it
maybeReportError ctxt err
  | cec_suppress ctxt    -- Some worse error has occurred;
  = return ()            -- so suppress this error/warning

  | cec_errors_as_warns ctxt
  = reportWarning NoReason err

  | otherwise
  = case cec_defer_type_errors ctxt of
      TypeDefer -> return ()
      TypeWarn  -> reportWarning (Reason Opt_WarnDeferredTypeErrors) err
      TypeError -> reportError err

addDeferredBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
-- See Note [Deferring coercion errors to runtime]
addDeferredBinding ctxt err ct
  | CtWanted { ctev_pred = pred, ctev_dest = dest } <- ctEvidence ct
    -- Only add deferred bindings for Wanted constraints
  = do { dflags <- getDynFlags
       ; let err_msg = pprLocErrMsg err
             err_fs  = mkFastString $ showSDoc dflags $
                       err_msg $$ text "(deferred type error)"
             err_tm  = EvDelayedError pred err_fs
             ev_binds_var = cec_binds ctxt

       ; case dest of
           EvVarDest evar
             -> addTcEvBind ev_binds_var $ mkWantedEvBind evar err_tm
           HoleDest hole
             -> do { -- See Note [Deferred errors for coercion holes]
                     evar <- newEvVar pred
                   ; addTcEvBind ev_binds_var $ mkWantedEvBind evar err_tm
                   ; fillCoercionHole hole (mkTcCoVarCo evar) }}

  | otherwise   -- Do not set any evidence for Given/Derived
  = return ()

maybeAddDeferredHoleBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
maybeAddDeferredHoleBinding ctxt err ct
  | isExprHoleCt ct
  = addDeferredBinding ctxt err ct  -- Only add bindings for holes in expressions
  | otherwise                       -- not for holes in partial type signatures
  = return ()

tryReporters :: ReportErrCtxt -> [ReporterSpec] -> [Ct] -> TcM (ReportErrCtxt, [Ct])
-- Use the first reporter in the list whose predicate says True
tryReporters ctxt reporters cts
  = do { traceTc "tryReporters {" (ppr cts)
       ; (ctxt', cts') <- go ctxt reporters cts
       ; traceTc "tryReporters }" (ppr cts')
       ; return (ctxt', cts') }
  where
    go ctxt [] cts
      = return (ctxt, cts)

    go ctxt (r : rs) cts
      = do { (ctxt', cts') <- tryReporter ctxt r cts
           ; go ctxt' rs cts' }
                -- Carry on with the rest, because we must make
                -- deferred bindings for them if we have -fdefer-type-errors
                -- But suppress their error messages

tryReporter :: ReportErrCtxt -> ReporterSpec -> [Ct] -> TcM (ReportErrCtxt, [Ct])
tryReporter ctxt (str, keep_me,  suppress_after, reporter) cts
  | null yeses = return (ctxt, cts)
  | otherwise  = do { traceTc "tryReporter{ " (text str <+> ppr yeses)
                    ; reporter ctxt yeses
                    ; let ctxt' = ctxt { cec_suppress = suppress_after || cec_suppress ctxt }
                    ; traceTc "tryReporter end }" (text str <+> ppr (cec_suppress ctxt) <+> ppr suppress_after)
                    ; return (ctxt', nos) }
  where
    (yeses, nos) = partition (\ct -> keep_me ct (classifyPredType (ctPred ct))) cts


pprArising :: CtOrigin -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq, KindEq, Given
pprArising (TypeEqOrigin {}) = empty
pprArising (KindEqOrigin {}) = empty
pprArising (GivenOrigin {})  = empty
pprArising orig              = pprCtOrigin orig

-- Add the "arising from..." part to a message about bunch of dicts
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = hang msg 2 (pprArising orig)

pprWithArising :: [Ct] -> (CtLoc, SDoc)
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
-- Works for Wanted/Derived only
pprWithArising []
  = panic "pprWithArising"
pprWithArising (ct:cts)
  | null cts
  = (loc, addArising (ctLocOrigin loc)
                     (pprTheta [ctPred ct]))
  | otherwise
  = (loc, vcat (map ppr_one (ct:cts)))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprCtLoc (ctLoc ct'))

mkErrorMsgFromCt :: ReportErrCtxt -> Ct -> Report -> TcM ErrMsg
mkErrorMsgFromCt ctxt ct report
  = mkErrorReport ctxt (ctLocEnv (ctLoc ct)) report

mkErrorReport :: ReportErrCtxt -> TcLclEnv -> Report -> TcM ErrMsg
mkErrorReport ctxt tcl_env (Report important relevant_bindings valid_subs)
  = do { context <- mkErrInfo (cec_tidy ctxt) (tcl_ctxt tcl_env)
       ; mkErrDocAt (RealSrcSpan (tcl_loc tcl_env))
            (errDoc important [context] (relevant_bindings ++ valid_subs))
       }

type UserGiven = Implication

getUserGivens :: ReportErrCtxt -> [UserGiven]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = implics}) = getUserGivensFromImplics implics

getUserGivensFromImplics :: [Implication] -> [UserGiven]
getUserGivensFromImplics implics
  = reverse (filterOut (null . ic_given) implics)

{-
Note [Always warn with -fdefer-type-errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -fdefer-type-errors is on we warn about *all* type errors, even
if cec_suppress is on.  This can lead to a lot more warnings than you
would get errors without -fdefer-type-errors, but if we suppress any of
them you might get a runtime error that wasn't warned about at compile
time.

This is an easy design choice to change; just flip the order of the
first two equations for maybeReportError

To be consistent, we should also report multiple warnings from a single
location in mkGroupReporter, when -fdefer-type-errors is on.  But that
is perhaps a bit *over*-consistent! Again, an easy choice to change.

With #10283, you can now opt out of deferred type error warnings.

Note [Deferred errors for coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we need to defer a type error where the destination for the evidence
is a coercion hole. We can't just put the error in the hole, because we can't
make an erroneous coercion. (Remember that coercions are erased for runtime.)
Instead, we invent a new EvVar, bind it to an error and then make a coercion
from that EvVar, filling the hole with that coercion. Because coercions'
types are unlifted, the error is guaranteed to be hit before we get to the
coercion.

Note [Do not report derived but soluble errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wc_simples include Derived constraints that have not been solved, but are
not insoluble (in that case they'd be in wc_insols).  We do not want to report
these as errors:

* Superclass constraints. If we have an unsolved [W] Ord a, we'll also have
  an unsolved [D] Eq a, and we do not want to report that; it's just noise.

* Functional dependencies.  For givens, consider
      class C a b | a -> b
      data T a where
         MkT :: C a d => [d] -> T a
      f :: C a b => T a -> F Int
      f (MkT xs) = length xs
  Then we get a [D] b~d.  But there *is* a legitimate call to
  f, namely   f (MkT [True]) :: T Bool, in which b=d.  So we should
  not reject the program.

  For wanteds, something similar
      data T a where
        MkT :: C Int b => a -> b -> T a
      g :: C Int c => c -> ()
      f :: T a -> ()
      f (MkT x y) = g x
  Here we get [G] C Int b, [W] C Int a, hence [D] a~b.
  But again f (MkT True True) is a legitimate call.

(We leave the Deriveds in wc_simple until reportErrors, so that we don't lose
derived superclasses between iterations of the solver.)

For functional dependencies, here is a real example,
stripped off from libraries/utf8-string/Codec/Binary/UTF8/Generic.hs

  class C a b | a -> b
  g :: C a b => a -> b -> ()
  f :: C a b => a -> b -> ()
  f xa xb =
      let loop = g xa
      in loop xb

We will first try to infer a type for loop, and we will succeed:
    C a b' => b' -> ()
Subsequently, we will type check (loop xb) and all is good. But,
recall that we have to solve a final implication constraint:
    C a b => (C a b' => .... cts from body of loop .... ))
And now we have a problem as we will generate an equality b ~ b' and fail to
solve it.


************************************************************************
*                                                                      *
                Irreducible predicate errors
*                                                                      *
************************************************************************
-}

mkIrredErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIrredErr ctxt cts
  = do { (ctxt, binds_msg, ct1) <- relevantBindings True ctxt ct1
       ; let orig = ctOrigin ct1
             msg  = couldNotDeduce (getUserGivens ctxt) (map ctPred cts, orig)
       ; mkErrorMsgFromCt ctxt ct1 $
            important msg `mappend` relevant_bindings binds_msg }
  where
    (ct1:_) = cts

----------------
mkHoleError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkHoleError _ctxt ct@(CHoleCan { cc_hole = ExprHole (OutOfScope occ rdr_env0) })
  -- Out-of-scope variables, like 'a', where 'a' isn't bound; suggest possible
  -- in-scope variables in the message, and note inaccessible exact matches
  = do { dflags   <- getDynFlags
       ; imp_info <- getImports
       ; let suggs_msg = unknownNameSuggestions dflags rdr_env0
                                                (tcl_rdr lcl_env) imp_info rdr
       ; rdr_env     <- getGlobalRdrEnv
       ; splice_locs <- getTopLevelSpliceLocs
       ; let match_msgs = mk_match_msgs rdr_env splice_locs
       ; mkErrDocAt (RealSrcSpan err_loc) $
                    errDoc [out_of_scope_msg] [] (match_msgs ++ [suggs_msg]) }

  where
    rdr         = mkRdrUnqual occ
    ct_loc      = ctLoc ct
    lcl_env     = ctLocEnv ct_loc
    err_loc     = tcl_loc lcl_env
    hole_ty     = ctEvPred (ctEvidence ct)
    boring_type = isTyVarTy hole_ty

    out_of_scope_msg -- Print v :: ty only if the type has structure
      | boring_type = hang herald 2 (ppr occ)
      | otherwise   = hang herald 2 (pp_with_type occ hole_ty)

    herald | isDataOcc occ = text "Data constructor not in scope:"
           | otherwise     = text "Variable not in scope:"

    -- Indicate if the out-of-scope variable exactly (and unambiguously) matches
    -- a top-level binding in a later inter-splice group; see Note [OutOfScope
    -- exact matches]
    mk_match_msgs rdr_env splice_locs
      = let gres = filter isLocalGRE (lookupGlobalRdrEnv rdr_env occ)
        in case gres of
             [gre]
               |  RealSrcSpan bind_loc <- greSrcSpan gre
                  -- Find splice between the unbound variable and the match; use
                  -- lookupLE, not lookupLT, since match could be in the splice
               ,  Just th_loc <- Set.lookupLE bind_loc splice_locs
               ,  err_loc < th_loc
               -> [mk_bind_scope_msg bind_loc th_loc]
             _ -> []

    mk_bind_scope_msg bind_loc th_loc
      | is_th_bind
      = hang (quotes (ppr occ) <+> parens (text "splice on" <+> th_rng))
           2 (text "is not in scope before line" <+> int th_start_ln)
      | otherwise
      = hang (quotes (ppr occ) <+> bind_rng <+> text "is not in scope")
           2 (text "before the splice on" <+> th_rng)
      where
        bind_rng = parens (text "line" <+> int bind_ln)
        th_rng
          | th_start_ln == th_end_ln = single
          | otherwise                = multi
        single = text "line"  <+> int th_start_ln
        multi  = text "lines" <+> int th_start_ln <> text "-" <> int th_end_ln
        bind_ln     = srcSpanStartLine bind_loc
        th_start_ln = srcSpanStartLine th_loc
        th_end_ln   = srcSpanEndLine   th_loc
        is_th_bind = th_loc `containsSpan` bind_loc

mkHoleError ctxt ct@(CHoleCan { cc_hole = hole })
  -- Explicit holes, like "_" or "_f"
  = do { (ctxt, binds_msg, ct) <- relevantBindings False ctxt ct
               -- The 'False' means "don't filter the bindings"; see Trac #8191

       ; show_hole_constraints <- goptM Opt_ShowHoleConstraints
       ; let constraints_msg
               | isExprHoleCt ct, show_hole_constraints
                  = givenConstraintsMsg ctxt
               | otherwise = empty

       ; sub_msg <-  validSubstitutions ct
       ; mkErrorMsgFromCt ctxt ct $
            important hole_msg `mappend`
            relevant_bindings (binds_msg $$ constraints_msg) `mappend`
            valid_substitutions sub_msg}

  where
    occ     = holeOcc hole
    hole_ty = ctEvPred (ctEvidence ct)
    tyvars  = tyCoVarsOfTypeList hole_ty

    hole_msg = case hole of
      ExprHole {} -> vcat [ hang (text "Found hole:")
                               2 (pp_with_type occ hole_ty)
                          , tyvars_msg, expr_hole_hint ]
      TypeHole {} -> vcat [ hang (text "Found type wildcard" <+>
                                  quotes (ppr occ))
                               2 (text "standing for" <+>
                                  quotes (pprType hole_ty))
                          , tyvars_msg, type_hole_hint ]

    tyvars_msg = ppUnless (null tyvars) $
                 text "Where:" <+> vcat (map loc_msg tyvars)

    type_hole_hint
         | HoleError <- cec_type_holes ctxt
         = text "To use the inferred type, enable PartialTypeSignatures"
         | otherwise
         = empty

    expr_hole_hint                       -- Give hint for, say,   f x = _x
         | lengthFS (occNameFS occ) > 1  -- Don't give this hint for plain "_"
         = text "Or perhaps" <+> quotes (ppr occ)
           <+> text "is mis-spelled, or not in scope"
         | otherwise
         = empty

    loc_msg tv
       | isTyVar tv
       = case tcTyVarDetails tv of
           MetaTv {} -> quotes (ppr tv) <+> text "is an ambiguous type variable"
           _         -> extraTyVarInfo ctxt tv
       | otherwise
       = sdocWithDynFlags $ \dflags ->
         if gopt Opt_PrintExplicitCoercions dflags
         then quotes (ppr tv) <+> text "is a coercion variable"
         else empty

mkHoleError _ ct = pprPanic "mkHoleError" (ppr ct)


-- See Note [Valid substitutions include ...]
validSubstitutions :: Ct -> TcM SDoc
validSubstitutions ct | isExprHoleCt ct =
  do { top_env <- getTopEnv
     ; rdr_env <- getGlobalRdrEnv
     ; gbl_env <- tcg_type_env <$> getGblEnv
     ; lcl_env <- getLclTypeEnv
     ; dflags <- getDynFlags
     ; (discards, substitutions) <-
        go (gbl_env, lcl_env, top_env) (maxValidSubstitutions dflags)
         $ localsFirst $ globalRdrEnvElts rdr_env
     ; return $ ppUnless (null substitutions) $
                 hang (text "Valid substitutions include")
                  2 (vcat (map (ppr_sub rdr_env) substitutions)
                    $$ ppWhen discards subsDiscardMsg) }
  where
    hole_ty :: TcPredType
    hole_ty = ctEvPred (ctEvidence ct)

    hole_env = ctLocEnv $ ctEvLoc $ ctEvidence ct

    localsFirst :: [GlobalRdrElt] -> [GlobalRdrElt]
    localsFirst elts = lcl ++ gbl
      where (lcl, gbl) = partition gre_lcl elts

    getBndrOcc :: TcIdBinder -> OccName
    getBndrOcc (TcIdBndr id _) = occName $ getName id
    getBndrOcc (TcIdBndr_ExpType name _ _) = occName $ getName name

    relBindSet =  mkOccSet $ map getBndrOcc $ tcl_bndrs hole_env

    shouldBeSkipped :: GlobalRdrElt -> Bool
    shouldBeSkipped el = (occName $ gre_name el) `elemOccSet` relBindSet

    ppr_sub :: GlobalRdrEnv -> Id -> SDoc
    ppr_sub rdr_env id = case lookupGRE_Name rdr_env (idName id) of
        Just elt -> sep [ idAndTy, nest 2 (parens $ pprNameProvenance elt)]
        _ -> idAndTy
      where name = idName id
            ty = varType id
            idAndTy = (pprPrefixOcc name <+> dcolon <+> pprType ty)

    tyToId :: TyThing -> Maybe Id
    tyToId (AnId i) = Just i
    tyToId (AConLike c) = conLikeWrapId_maybe c
    tyToId _ = Nothing

    tcTyToId :: TcTyThing -> Maybe Id
    tcTyToId (AGlobal id) = tyToId id
    tcTyToId (ATcId id _) = Just id
    tcTyToId _ = Nothing

    substituteable :: Id -> Bool
    substituteable = tcEqType hole_ty . varType

    lookupTopId :: HscEnv -> Name -> IO (Maybe Id)
    lookupTopId env name =
        maybe Nothing tyToId <$> lookupTypeHscEnv env name

    lookupGblId :: TypeEnv -> Name -> Maybe Id
    lookupGblId env name = maybe Nothing tyToId $ lookupTypeEnv env name

    lookupLclId :: TcTypeEnv -> Name -> Maybe Id
    lookupLclId env name = maybe Nothing tcTyToId $ lookupNameEnv env name

    go ::  (TypeEnv, TcTypeEnv, HscEnv) -> Maybe Int -> [GlobalRdrElt]
       -> TcM (Bool, [Id])
    go = go_ []

    go_ ::  [Id] -> (TypeEnv, TcTypeEnv, HscEnv) -> Maybe Int -> [GlobalRdrElt]
         -> TcM (Bool, [Id])
    go_ subs _ _ [] = return (False, reverse subs)
    go_ subs _ (Just 0) _ = return (True, reverse subs)
    go_ subs envs@(gbl,lcl,top) maxleft (el:elts) =
       if shouldBeSkipped el then discard_it
         else do { maybeId <- liftIO lookupId
                 ; case maybeId of
                     Just id | substituteable id ->
                       go_ (id:subs) envs ((\n -> n - 1) <$> maxleft) elts
                     _ -> discard_it }
      where name = gre_name el
            discard_it = go_ subs envs maxleft elts
            getTopId = lookupTopId top name
            gbl_id = lookupGblId gbl name
            lcl_id = lookupLclId lcl name
            lookupId = if (isJust lcl_id) then return lcl_id
                       else if (isJust gbl_id) then return gbl_id else getTopId


validSubstitutions _ = return empty


-- See Note [Constraints include ...]
givenConstraintsMsg :: ReportErrCtxt -> SDoc
givenConstraintsMsg ctxt =
    let constraints :: [(Type, RealSrcSpan)]
        constraints =
          do { Implic{ ic_given = given, ic_env = env } <- cec_encl ctxt
             ; constraint <- given
             ; return (varType constraint, tcl_loc env) }

        pprConstraint (constraint, loc) =
          ppr constraint <+> nest 2 (parens (text "from" <+> ppr loc))

    in ppUnless (null constraints) $
         hang (text "Constraints include")
            2 (vcat $ map pprConstraint constraints)

pp_with_type :: OccName -> Type -> SDoc
pp_with_type occ ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType ty)

----------------
mkIPErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIPErr ctxt cts
  = do { (ctxt, binds_msg, ct1) <- relevantBindings True ctxt ct1
       ; let orig    = ctOrigin ct1
             preds   = map ctPred cts
             givens  = getUserGivens ctxt
             msg | null givens
                 = addArising orig $
                   sep [ text "Unbound implicit parameter" <> plural cts
                       , nest 2 (pprParendTheta preds) ]
                 | otherwise
                 = couldNotDeduce givens (preds, orig)

       ; mkErrorMsgFromCt ctxt ct1 $
            important msg `mappend` relevant_bindings binds_msg }
  where
    (ct1:_) = cts

{-
Note [Valid substitutions include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`validSubstitutions` returns the "Valid substitutions include ..." message.
For example, look at the following definitions in a file called test.hs:

    ps :: String -> IO ()
    ps = putStrLn

    ps2 :: a -> IO ()
    ps2 _ = putStrLn "hello, world"

    main :: IO ()
    main = _ "hello, world"

The hole in `main` would generate the message:

    Valid substitutions include
      ps :: String -> IO () ((defined at test.hs:2:1)
      putStrLn :: String -> IO ()
        (imported from ‘Prelude’ at test.hs:1:1
         (and originally defined in ‘System.IO’))
      putStr :: String -> IO ()
        (imported from ‘Prelude’ at test.hs:1:1
         (and originally defined in ‘System.IO’))

Valid substitutions are found by checking names in scope.

Currently the implementation only looks at exact type matches, as given by
`tcEqType`, so we DO NOT report `ps2` as a valid substitution in the example,
even though it fits in the hole. To determine that `ps2` fits in the hole,
we would need to check ids for subsumption, i.e. that the type of the hole is
a subtype of the id. This can be done using `tcSubType` from `TcUnify` and
`tcCheckSatisfiability` in `TcSimplify`.  Unfortunately, `TcSimplify` uses
`TcErrors` to report errors found during constraint checking, so checking for
subsumption in holes would involve shuffling some code around in `TcSimplify`,
to make a non-error reporting constraint satisfiability checker which could
then be used for checking whether a given id satisfies the constraints imposed
by the hole.

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


Note [OutOfScope exact matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When constructing an out-of-scope error message, we not only generate a list of
possible in-scope alternatives but also search for an exact, unambiguous match
in a later inter-splice group.  If we find such a match, we report its presence
(and indirectly, its scope) in the message.  For example, if a module A contains
the following declarations,

   foo :: Int
   foo = x

   $(return [])  -- Empty top-level splice

   x :: Int
   x = 23

we will issue an error similar to

   A.hs:6:7: error:
       • Variable not in scope: x :: Int
       • ‘x’ (line 11) is not in scope before the splice on line 8

By providing information about the match, we hope to clarify why declaring a
variable after a top-level splice but using it before the splice generates an
out-of-scope error (a situation which is often confusing to Haskell newcomers).

Note that if we find multiple exact matches to the out-of-scope variable
(hereafter referred to as x), we report nothing.  Such matches can only be
duplicate record fields, as the presence of any other duplicate top-level
declarations would have already halted compilation.  But if these record fields
are declared in a later inter-splice group, then so too are their corresponding
types.  Thus, these types must not occur in the inter-splice group containing x
(any unknown types would have already been reported), and so the matches to the
record fields are most likely coincidental.

One oddity of the exact match portion of the error message is that we specify
where the match to x is NOT in scope.  Why not simply state where the match IS
in scope?  It most cases, this would be just as easy and perhaps a little
clearer for the user.  But now consider the following example:

    {-# LANGUAGE TemplateHaskell #-}

    module A where

    import Language.Haskell.TH
    import Language.Haskell.TH.Syntax

    foo = x

    $(do -------------------------------------------------
        ds <- [d| ok1 = x
                |]
        addTopDecls ds
        return [])

    bar = $(do
            ds <- [d| x = 23
                      ok2 = x
                    |]
            addTopDecls ds
            litE $ stringL "hello")

    $(return []) -----------------------------------------

    ok3 = x

Here, x is out-of-scope in the declaration of foo, and so we report

    A.hs:8:7: error:
        • Variable not in scope: x
        • ‘x’ (line 16) is not in scope before the splice on lines 10-14

If we instead reported where x IS in scope, we would have to state that it is in
scope after the second top-level splice as well as among all the top-level
declarations added by both calls to addTopDecls.  But doing so would not only
add complexity to the code but also overwhelm the user with unneeded
information.

The logic which determines where x is not in scope is straightforward: it simply
finds the last top-level splice which occurs after x but before (or at) the
match to x (assuming such a splice exists).  In most cases, the check that the
splice occurs after x acts only as a sanity check.  For example, when the match
to x is a non-TH top-level declaration and a splice S occurs before the match,
then x must precede S; otherwise, it would be in scope.  But when dealing with
addTopDecls, this check serves a practical purpose.  Consider the following
declarations:

    $(do
        ds <- [d| ok = x
                  x = 23
                |]
        addTopDecls ds
        return [])

    foo = x

In this case, x is not in scope in the declaration for foo.  Since x occurs
AFTER the splice containing the match, the logic does not find any splices after
x but before or at its match, and so we report nothing about x's scope.  If we
had not checked whether x occurs before the splice, we would have instead
reported that x is not in scope before the splice.  While correct, such an error
message is more likely to confuse than to enlighten.
-}

{-
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
the *signature* (Trac #7293).  So, for Given errors we replace the
env (and hence src-loc) on its CtLoc with that from the immediately
enclosing implication.

Note [Error messages for untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #9109)
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
mkEqErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkEqErr ctxt (ct:_) = mkEqErr1 ctxt ct
mkEqErr _ [] = panic "mkEqErr"

mkEqErr1 :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkEqErr1 ctxt ct   -- Wanted or derived;
                   -- givens handled in mkGivenErrorReporter
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; rdr_env <- getGlobalRdrEnv
       ; fam_envs <- tcGetFamInstEnvs
       ; exp_syns <- goptM Opt_PrintExpandedSynonyms
       ; let (keep_going, is_oriented, wanted_msg)
                           = mk_wanted_extra (ctLoc ct) exp_syns
             coercible_msg = case ctEqRel ct of
               NomEq  -> empty
               ReprEq -> mkCoercibleExplanation rdr_env fam_envs ty1 ty2
       ; dflags <- getDynFlags
       ; traceTc "mkEqErr1" (ppr ct $$ pprCtOrigin (ctOrigin ct) $$ ppr keep_going)
       ; let report = mconcat [important wanted_msg, important coercible_msg,
                               relevant_bindings binds_msg]
       ; if keep_going
         then mkEqErr_help dflags ctxt report ct is_oriented ty1 ty2
         else mkErrorMsgFromCt ctxt ct report }
  where
    (ty1, ty2) = getEqPredTys (ctPred ct)

       -- If the types in the error message are the same as the types
       -- we are unifying, don't add the extra expected/actual message
    mk_wanted_extra :: CtLoc -> Bool -> (Bool, Maybe SwapFlag, SDoc)
    mk_wanted_extra loc expandSyns
      = case ctLocOrigin loc of
          orig@TypeEqOrigin {} -> mkExpectedActualMsg ty1 ty2 orig
                                                      t_or_k expandSyns
            where
              t_or_k = ctLocTypeOrKind_maybe loc

          KindEqOrigin cty1 mb_cty2 sub_o sub_t_or_k
            -> (True, Nothing, msg1 $$ msg2)
            where
              sub_what = case sub_t_or_k of Just KindLevel -> text "kinds"
                                            _              -> text "types"
              msg1 = sdocWithDynFlags $ \dflags ->
                     case mb_cty2 of
                       Just cty2
                         |  gopt Opt_PrintExplicitCoercions dflags
                         || not (cty1 `pickyEqType` cty2)
                         -> hang (text "When matching" <+> sub_what)
                               2 (vcat [ ppr cty1 <+> dcolon <+>
                                         ppr (typeKind cty1)
                                       , ppr cty2 <+> dcolon <+>
                                         ppr (typeKind cty2) ])
                       _ -> text "When matching the kind of" <+> quotes (ppr cty1)
              msg2 = case sub_o of
                       TypeEqOrigin {}
                         | Just cty2 <- mb_cty2 ->
                         thdOf3 (mkExpectedActualMsg cty1 cty2 sub_o sub_t_or_k
                                                     expandSyns)
                       _ -> empty
          _ -> (True, Nothing, empty)

-- | This function tries to reconstruct why a "Coercible ty1 ty2" constraint
-- is left over.
mkCoercibleExplanation :: GlobalRdrEnv -> FamInstEnvs
                       -> TcType -> TcType -> SDoc
mkCoercibleExplanation rdr_env fam_envs ty1 ty2
  | Just (tc, tys) <- tcSplitTyConApp_maybe ty1
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (tc, tys) <- splitTyConApp_maybe ty2
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (s1, _) <- tcSplitAppTy_maybe ty1
  , Just (s2, _) <- tcSplitAppTy_maybe ty2
  , s1 `eqType` s2
  , has_unknown_roles s1
  = hang (text "NB: We cannot know what roles the parameters to" <+>
          quotes (ppr s1) <+> text "have;")
       2 (text "we must assume that the role is nominal")
  | otherwise
  = empty
  where
    coercible_msg_for_tycon tc
        | isAbstractTyCon tc
        = Just $ hsep [ text "NB: The type constructor"
                      , quotes (pprSourceTyCon tc)
                      , text "is abstract" ]
        | isNewTyCon tc
        , [data_con] <- tyConDataCons tc
        , let dc_name = dataConName data_con
        , isNothing (lookupGRE_Name rdr_env dc_name)
        = Just $ hang (text "The data constructor" <+> quotes (ppr dc_name))
                    2 (sep [ text "of newtype" <+> quotes (pprSourceTyCon tc)
                           , text "is not in scope" ])
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

{-
-- | Make a listing of role signatures for all the parameterised tycons
-- used in the provided types


-- SLPJ Jun 15: I could not convince myself that these hints were really
-- useful.  Maybe they are, but I think we need more work to make them
-- actually helpful.
mkRoleSigs :: Type -> Type -> SDoc
mkRoleSigs ty1 ty2
  = ppUnless (null role_sigs) $
    hang (text "Relevant role signatures:")
       2 (vcat role_sigs)
  where
    tcs = nameEnvElts $ tyConsOfType ty1 `plusNameEnv` tyConsOfType ty2
    role_sigs = mapMaybe ppr_role_sig tcs

    ppr_role_sig tc
      | null roles  -- if there are no parameters, don't bother printing
      = Nothing
      | isBuiltInSyntax (tyConName tc)  -- don't print roles for (->), etc.
      = Nothing
      | otherwise
      = Just $ hsep $ [text "type role", ppr tc] ++ map ppr roles
      where
        roles = tyConRoles tc
-}

mkEqErr_help :: DynFlags -> ReportErrCtxt -> Report
             -> Ct
             -> Maybe SwapFlag   -- Nothing <=> not sure
             -> TcType -> TcType -> TcM ErrMsg
mkEqErr_help dflags ctxt report ct oriented ty1 ty2
  | Just (tv1, co1) <- tcGetCastedTyVar_maybe ty1
  = mkTyVarEqErr dflags ctxt report ct oriented tv1 co1 ty2
  | Just (tv2, co2) <- tcGetCastedTyVar_maybe ty2
  = mkTyVarEqErr dflags ctxt report ct swapped  tv2 co2 ty1
  | otherwise
  = reportEqErr ctxt report ct oriented ty1 ty2
  where
    swapped = fmap flipSwap oriented

reportEqErr :: ReportErrCtxt -> Report
            -> Ct
            -> Maybe SwapFlag   -- Nothing <=> not sure
            -> TcType -> TcType -> TcM ErrMsg
reportEqErr ctxt report ct oriented ty1 ty2
  = mkErrorMsgFromCt ctxt ct (mconcat [misMatch, report, eqInfo])
  where misMatch = important $ misMatchOrCND ctxt ct oriented ty1 ty2
        eqInfo = important $ mkEqInfoMsg ct ty1 ty2

mkTyVarEqErr, mkTyVarEqErr'
  :: DynFlags -> ReportErrCtxt -> Report -> Ct
             -> Maybe SwapFlag -> TcTyVar -> TcCoercionN -> TcType -> TcM ErrMsg
-- tv1 and ty2 are already tidied
mkTyVarEqErr dflags ctxt report ct oriented tv1 co1 ty2
  = do { traceTc "mkTyVarEqErr" (ppr ct $$ ppr tv1 $$ ppr co1 $$ ppr ty2)
       ; mkTyVarEqErr' dflags ctxt report ct oriented tv1 co1 ty2 }

mkTyVarEqErr' dflags ctxt report ct oriented tv1 co1 ty2
  | not insoluble_occurs_check   -- See Note [Occurs check wins]
  , isUserSkolem ctxt tv1   -- ty2 won't be a meta-tyvar, or else the thing would
                            -- be oriented the other way round;
                            -- see TcCanonical.canEqTyVarTyVar
    || isSigTyVar tv1 && not (isTyVarTy ty2)
    || ctEqRel ct == ReprEq
     -- the cases below don't really apply to ReprEq (except occurs check)
  = mkErrorMsgFromCt ctxt ct $ mconcat
        [ important $ misMatchOrCND ctxt ct oriented ty1 ty2
        , important $ extraTyVarEqInfo ctxt tv1 ty2
        , report
        ]

  | OC_Occurs <- occ_check_expand
    -- We report an "occurs check" even for  a ~ F t a, where F is a type
    -- function; it's not insoluble (because in principle F could reduce)
    -- but we have certainly been unable to solve it
    -- See Note [Occurs check error] in TcCanonical
  = do { let main_msg = addArising (ctOrigin ct) $
                        hang (text "Occurs check: cannot construct the infinite" <+> what <> colon)
                              2 (sep [ppr ty1, char '~', ppr ty2])

             extra2 = important $ mkEqInfoMsg ct ty1 ty2

             interesting_tyvars = filter (not . noFreeVarsOfType . tyVarKind) $
                                  filter isTyVar $
                                  fvVarList $
                                  tyCoFVsOfType ty1 `unionFV` tyCoFVsOfType ty2
             extra3 = relevant_bindings $
                      ppWhen (not (null interesting_tyvars)) $
                      hang (text "Type variable kinds:") 2 $
                      vcat (map (tyvar_binding . tidyTyVarOcc (cec_tidy ctxt))
                                interesting_tyvars)

             tyvar_binding tv = ppr tv <+> dcolon <+> ppr (tyVarKind tv)
       ; mkErrorMsgFromCt ctxt ct $
         mconcat [important main_msg, extra2, extra3, report] }

  | OC_Bad <- occ_check_expand
  = do { let msg = vcat [ text "Cannot instantiate unification variable"
                          <+> quotes (ppr tv1)
                        , hang (text "with a" <+> what <+> text "involving foralls:") 2 (ppr ty2)
                        , nest 2 (text "GHC doesn't yet support impredicative polymorphism") ]
       -- Unlike the other reports, this discards the old 'report_important'
       -- instead of augmenting it.  This is because the details are not likely
       -- to be helpful since this is just an unimplemented feature.
       ; mkErrorMsgFromCt ctxt ct $ report { report_important = [msg] } }

   -- check for heterogeneous equality next; see Note [Equalities with incompatible kinds]
   -- in TcCanonical
  | not (k1 `tcEqType` k2)
  = do { let main_msg = addArising (ctOrigin ct) $
                        vcat [ hang (text "Kind mismatch: cannot unify" <+>
                                     parens (ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1)) <+>
                                     text "with")
                                  2 (sep [ppr ty2, dcolon, ppr k2])
                             , text "Their kinds differ." ]
             cast_msg
               | isTcReflexiveCo co1 = empty
               | otherwise           = text "NB:" <+> ppr tv1 <+>
                                       text "was casted to have kind" <+>
                                       quotes (ppr k1)

       ; mkErrorMsgFromCt ctxt ct (mconcat [important main_msg, important cast_msg, report]) }

  -- If the immediately-enclosing implication has 'tv' a skolem, and
  -- we know by now its an InferSkol kind of skolem, then presumably
  -- it started life as a SigTv, else it'd have been unified, given
  -- that there's no occurs-check or forall problem
  | (implic:_) <- cec_encl ctxt
  , Implic { ic_skols = skols } <- implic
  , tv1 `elem` skols
  = mkErrorMsgFromCt ctxt ct $ mconcat
        [ important $ misMatchMsg ct oriented ty1 ty2
        , important $ extraTyVarEqInfo ctxt tv1 ty2
        , report
        ]

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_skols = skols, ic_info = skol_info } <- implic
  , let esc_skols = filter (`elemVarSet` (tyCoVarsOfType ty2)) skols
  , not (null esc_skols)
  = do { let msg = important $ misMatchMsg ct oriented ty1 ty2
             esc_doc = sep [ text "because" <+> what <+> text "variable" <> plural esc_skols
                             <+> pprQuotedList esc_skols
                           , text "would escape" <+>
                             if isSingleton esc_skols then text "its scope"
                                                      else text "their scope" ]
             tv_extra = important $
                        vcat [ nest 2 $ esc_doc
                             , sep [ (if isSingleton esc_skols
                                      then text "This (rigid, skolem)" <+>
                                           what <+> text "variable is"
                                      else text "These (rigid, skolem)" <+>
                                           what <+> text "variables are")
                               <+> text "bound by"
                             , nest 2 $ ppr skol_info
                             , nest 2 $ text "at" <+> ppr (tcl_loc env) ] ]
       ; mkErrorMsgFromCt ctxt ct (mconcat [msg, tv_extra, report]) }

  -- Nastiest case: attempt to unify an untouchable variable
  -- So tv is a meta tyvar (or started that way before we
  -- generalised it).  So presumably it is an *untouchable*
  -- meta tyvar or a SigTv, else it'd have been unified
  -- See Note [Error messages for untouchables]
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_given = given
           , ic_tclvl = lvl, ic_info = skol_info } <- implic
  = ASSERT2( not (isTouchableMetaTyVar lvl tv1)
           , ppr tv1 $$ ppr lvl )  -- See Note [Error messages for untouchables]
    do { let msg = important $ misMatchMsg ct oriented ty1 ty2
             tclvl_extra = important $
                  nest 2 $
                  sep [ quotes (ppr tv1) <+> text "is untouchable"
                      , nest 2 $ text "inside the constraints:" <+> pprEvVarTheta given
                      , nest 2 $ text "bound by" <+> ppr skol_info
                      , nest 2 $ text "at" <+> ppr (tcl_loc env) ]
             tv_extra = important $ extraTyVarEqInfo ctxt tv1 ty2
             add_sig  = important $ suggestAddSig ctxt ty1 ty2
       ; mkErrorMsgFromCt ctxt ct $ mconcat
            [msg, tclvl_extra, tv_extra, add_sig, report] }

  | otherwise
  = reportEqErr ctxt report ct oriented (mkTyVarTy tv1) ty2
        -- This *can* happen (Trac #6123, and test T2627b)
        -- Consider an ambiguous top-level constraint (a ~ F a)
        -- Not an occurs check, because F is a type function.
  where
    Pair _ k1 = tcCoercionKind co1
    k2        = typeKind ty2

    ty1 = mkTyVarTy tv1
    occ_check_expand       = occCheckForErrors dflags tv1 ty2
    insoluble_occurs_check = isInsolubleOccursCheck (ctEqRel ct) tv1 ty2

    what = case ctLocTypeOrKind_maybe (ctLoc ct) of
      Just KindLevel -> text "kind"
      _              -> text "type"

mkEqInfoMsg :: Ct -> TcType -> TcType -> SDoc
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
--        (c) warning about -fprint-explicit-kinds if that might be helpful
mkEqInfoMsg ct ty1 ty2
  = tyfun_msg $$ ambig_msg $$ invis_msg
  where
    mb_fun1 = isTyFun_maybe ty1
    mb_fun2 = isTyFun_maybe ty2

    ambig_msg | isJust mb_fun1 || isJust mb_fun2
              = snd (mkAmbigMsg False ct)
              | otherwise = empty

    -- better to check the exp/act types in the CtOrigin than the actual
    -- mismatched types for suggestion about -fprint-explicit-kinds
    (act_ty, exp_ty) = case ctOrigin ct of
      TypeEqOrigin { uo_actual = act
                   , uo_expected = exp } -> (act, exp)
      _                                  -> (ty1, ty2)

    invis_msg | Just vis <- tcEqTypeVis act_ty exp_ty
              , not vis
              = ppSuggestExplicitKinds
              | otherwise
              = empty

    tyfun_msg | Just tc1 <- mb_fun1
              , Just tc2 <- mb_fun2
              , tc1 == tc2
              = text "NB:" <+> quotes (ppr tc1)
                <+> text "is a type function, and may not be injective"
              | otherwise = empty

isUserSkolem :: ReportErrCtxt -> TcTyVar -> Bool
-- See Note [Reporting occurs-check errors]
isUserSkolem ctxt tv
  = isSkolemTyVar tv && any is_user_skol_tv (cec_encl ctxt)
  where
    is_user_skol_tv (Implic { ic_skols = sks, ic_info = skol_info })
      = tv `elem` sks && is_user_skol_info skol_info

    is_user_skol_info (InferSkol {}) = False
    is_user_skol_info _ = True

misMatchOrCND :: ReportErrCtxt -> Ct
              -> Maybe SwapFlag -> TcType -> TcType -> SDoc
-- If oriented then ty1 is actual, ty2 is expected
misMatchOrCND ctxt ct oriented ty1 ty2
  | null givens ||
    (isRigidTy ty1 && isRigidTy ty2) ||
    isGivenCt ct
       -- If the equality is unconditionally insoluble
       -- or there is no context, don't report the context
  = misMatchMsg ct oriented ty1 ty2
  | otherwise
  = couldNotDeduce givens ([eq_pred], orig)
  where
    ev      = ctEvidence ct
    eq_pred = ctEvPred ev
    orig    = ctEvOrigin ev
    givens  = [ given | given <- getUserGivens ctxt, not (ic_no_eqs given)]
              -- Keep only UserGivens that have some equalities

couldNotDeduce :: [UserGiven] -> (ThetaType, CtOrigin) -> SDoc
couldNotDeduce givens (wanteds, orig)
  = vcat [ addArising orig (text "Could not deduce:" <+> pprTheta wanteds)
         , vcat (pp_givens givens)]

pp_givens :: [UserGiven] -> [SDoc]
pp_givens givens
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (text "from the context:") g
                 : map (ppr_given (text "or from:")) gs
    where
       ppr_given herald (Implic { ic_given = gs, ic_info = skol_info
                                , ic_env = env })
           = hang (herald <+> pprEvVarTheta gs)
                2 (sep [ text "bound by" <+> ppr skol_info
                       , text "at" <+> ppr (tcl_loc env) ])

extraTyVarEqInfo :: ReportErrCtxt -> TcTyVar -> TcType -> SDoc
-- Add on extra info about skolem constants
-- NB: The types themselves are already tidied
extraTyVarEqInfo ctxt tv1 ty2
  = extraTyVarInfo ctxt tv1 $$ ty_extra ty2
  where
    ty_extra ty = case tcGetTyVar_maybe ty of
                    Just tv -> extraTyVarInfo ctxt tv
                    Nothing -> empty

extraTyVarInfo :: ReportErrCtxt -> TcTyVar -> SDoc
extraTyVarInfo ctxt tv
  = ASSERT2( isTyVar tv, ppr tv )
    case tcTyVarDetails tv of
          SkolemTv {}   -> pprSkol implics tv
          RuntimeUnk {} -> pp_tv <+> text "is an interactive-debugger skolem"
          MetaTv {}     -> empty
  where
    implics = cec_encl ctxt
    pp_tv = quotes (ppr tv)

suggestAddSig :: ReportErrCtxt -> TcType -> TcType -> SDoc
-- See Note [Suggest adding a type signature]
suggestAddSig ctxt ty1 ty2
  | null inferred_bndrs
  = empty
  | [bndr] <- inferred_bndrs
  = text "Possible fix: add a type signature for" <+> quotes (ppr bndr)
  | otherwise
  = text "Possible fix: add type signatures for some or all of" <+> (ppr inferred_bndrs)
  where
    inferred_bndrs = nub (get_inf ty1 ++ get_inf ty2)
    get_inf ty | Just tv <- tcGetTyVar_maybe ty
               , isSkolemTyVar tv
               , InferSkol prs <- ic_info (getSkolemInfo (cec_encl ctxt) tv)
               = map fst prs
               | otherwise
               = []

--------------------
misMatchMsg :: Ct -> Maybe SwapFlag -> TcType -> TcType -> SDoc
-- Types are already tidy
-- If oriented then ty1 is actual, ty2 is expected
misMatchMsg ct oriented ty1 ty2
  | Just NotSwapped <- oriented
  = misMatchMsg ct (Just IsSwapped) ty2 ty1

  -- These next two cases are when we're about to report, e.g., that
  -- 'LiftedRep doesn't match 'VoidRep. Much better just to say
  -- lifted vs. unlifted
  | Just (tc1, []) <- splitTyConApp_maybe ty1
  , tc1 `hasKey` liftedRepDataConKey
  = lifted_vs_unlifted

  | Just (tc2, []) <- splitTyConApp_maybe ty2
  , tc2 `hasKey` liftedRepDataConKey
  = lifted_vs_unlifted

  | otherwise  -- So now we have Nothing or (Just IsSwapped)
               -- For some reason we treat Nothing like IsSwapped
  = addArising orig $
    sep [ text herald1 <+> quotes (ppr ty1)
        , nest padding $
          text herald2 <+> quotes (ppr ty2)
        , sameOccExtra ty2 ty1 ]
  where
    herald1 = conc [ "Couldn't match"
                   , if is_repr     then "representation of" else ""
                   , if is_oriented then "expected"          else ""
                   , what ]
    herald2 = conc [ "with"
                   , if is_repr     then "that of"           else ""
                   , if is_oriented then ("actual " ++ what) else "" ]
    padding = length herald1 - length herald2

    is_repr = case ctEqRel ct of { ReprEq -> True; NomEq -> False }
    is_oriented = isJust oriented

    orig = ctOrigin ct
    what = case ctLocTypeOrKind_maybe (ctLoc ct) of
      Just KindLevel -> "kind"
      _              -> "type"

    conc :: [String] -> String
    conc = foldr1 add_space

    add_space :: String -> String -> String
    add_space s1 s2 | null s1   = s2
                    | null s2   = s1
                    | otherwise = s1 ++ (' ' : s2)

    lifted_vs_unlifted
      = addArising orig $
        text "Couldn't match a lifted type with an unlifted type"

mkExpectedActualMsg :: Type -> Type -> CtOrigin -> Maybe TypeOrKind -> Bool
                    -> (Bool, Maybe SwapFlag, SDoc)
-- NotSwapped means (actual, expected), IsSwapped is the reverse
-- First return val is whether or not to print a herald above this msg
mkExpectedActualMsg ty1 ty2 (TypeEqOrigin { uo_actual = act
                                          , uo_expected = exp
                                          , uo_thing = maybe_thing })
                    m_level printExpanded
  | KindLevel <- level, occurs_check_error       = (True, Nothing, empty)
  | isUnliftedTypeKind act, isLiftedTypeKind exp = (False, Nothing, msg2)
  | isLiftedTypeKind act, isUnliftedTypeKind exp = (False, Nothing, msg3)
  | isLiftedTypeKind exp && not (isConstraintKind exp)
                                                 = (False, Nothing, msg4)
  | Just msg <- num_args_msg                     = (False, Nothing, msg $$ msg1)
  | KindLevel <- level, Just th <- maybe_thing   = (False, Nothing, msg5 th)
  | act `pickyEqType` ty1, exp `pickyEqType` ty2 = (True, Just NotSwapped, empty)
  | exp `pickyEqType` ty1, act `pickyEqType` ty2 = (True, Just IsSwapped, empty)
  | otherwise                                    = (True, Nothing, msg1)
  where
    level = m_level `orElse` TypeLevel

    occurs_check_error
      | Just act_tv <- tcGetTyVar_maybe act
      , act_tv `elemVarSet` tyCoVarsOfType exp
      = True
      | Just exp_tv <- tcGetTyVar_maybe exp
      , exp_tv `elemVarSet` tyCoVarsOfType act
      = True
      | otherwise
      = False

    sort = case level of
      TypeLevel -> text "type"
      KindLevel -> text "kind"

    msg1 = case level of
      KindLevel
        | Just th <- maybe_thing
        -> msg5 th

      _ | not (act `pickyEqType` exp)
        -> vcat [ text "Expected" <+> sort <> colon <+> ppr exp
                , text "  Actual" <+> sort <> colon <+> ppr act
                , if printExpanded then expandedTys else empty ]

        | otherwise
        -> empty

    thing_msg = case maybe_thing of
                  Just thing -> \_ -> quotes (ppr thing) <+> text "is"
                  Nothing    -> \vowel -> text "got a" <>
                                          if vowel then char 'n' else empty
    msg2 = sep [ text "Expecting a lifted type, but"
               , thing_msg True, text "unlifted" ]
    msg3 = sep [ text "Expecting an unlifted type, but"
               , thing_msg False, text "lifted" ]
    msg4 = maybe_num_args_msg $$
           sep [ text "Expected a type, but"
               , maybe (text "found something with kind")
                       (\thing -> quotes (ppr thing) <+> text "has kind")
                       maybe_thing
               , quotes (ppr act) ]

    msg5 th = hang (text "Expected" <+> kind_desc <> comma)
                 2 (text "but" <+> quotes (ppr th) <+> text "has kind" <+>
                    quotes (ppr act))
      where
        kind_desc | isConstraintKind exp = text "a constraint"
                  | otherwise            = text "kind" <+> quotes (ppr exp)

    num_args_msg = case level of
      TypeLevel -> Nothing
      KindLevel
        -> let n_act = count_args act
               n_exp = count_args exp in
           case n_act - n_exp of
             n | n /= 0
               , Just thing <- maybe_thing
               , case errorThingNumArgs_maybe thing of
                   Nothing           -> n > 0
                   Just num_act_args -> num_act_args >= -n
                     -- don't report to strip off args that aren't there
               -> Just $ text "Expecting" <+> speakN (abs n) <+>
                         more_or_fewer <+> quotes (ppr thing)
               where
                 more_or_fewer
                  | n < 0     = text "fewer arguments to"
                  | n == 1    = text "more argument to"
                  | otherwise = text "more arguments to"  -- n > 1
             _ -> Nothing

    maybe_num_args_msg = case num_args_msg of
      Nothing -> empty
      Just m  -> m

    count_args ty = count isVisibleBinder $ fst $ splitPiTys ty

    expandedTys =
      ppUnless (expTy1 `pickyEqType` exp && expTy2 `pickyEqType` act) $ vcat
        [ text "Type synonyms expanded:"
        , text "Expected type:" <+> ppr expTy1
        , text "  Actual type:" <+> ppr expTy2
        ]

    (expTy1, expTy2) = expandSynonymsToMatch exp act

mkExpectedActualMsg _ _ _ _ _ = panic "mkExpectedAcutalMsg"

{- Note [Insoluble occurs check wins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] a ~ [a],  [W] a ~ [a] (Trac #13674).  The Given is insoluble
so we don't use it for rewriting.  The Wanted is also insoluble, and
we don't solve it from the Given.  It's very confusing to say
    Cannot solve a ~ [a] from given constraints a ~ [a]

And indeed even thinking about the Givens is silly; [W] a ~ [a] is
just as insoluble as Int ~ Bool.

Conclusion: if there's an insoluble occurs check (isInsolubleOccursCheck)
then report it first.

(NB: there are potentially-soluble ones, like (a ~ F a b), and we don't
want to be as draconian with them.)

Note [Expanding type synonyms to make types similar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In type error messages, if -fprint-expanded-types is used, we want to expand
type synonyms to make expected and found types as similar as possible, but we
shouldn't expand types too much to make type messages even more verbose and
harder to understand. The whole point here is to make the difference in expected
and found types clearer.

`expandSynonymsToMatch` does this, it takes two types, and expands type synonyms
only as much as necessary. Given two types t1 and t2:

  * If they're already same, it just returns the types.

  * If they're in form `C1 t1_1 .. t1_n` and `C2 t2_1 .. t2_m` (C1 and C2 are
    type constructors), it expands C1 and C2 if they're different type synonyms.
    Then it recursively does the same thing on expanded types. If C1 and C2 are
    same, then it applies the same procedure to arguments of C1 and arguments of
    C2 to make them as similar as possible.

    Most important thing here is to keep number of synonym expansions at
    minimum. For example, if t1 is `T (T3, T5, Int)` and t2 is `T (T5, T3,
    Bool)` where T5 = T4, T4 = T3, ..., T1 = X, it returns `T (T3, T3, Int)` and
    `T (T3, T3, Bool)`.

  * Otherwise types don't have same shapes and so the difference is clearly
    visible. It doesn't do any expansions and show these types.

Note that we only expand top-layer type synonyms. Only when top-layer
constructors are the same we start expanding inner type synonyms.

Suppose top-layer type synonyms of t1 and t2 can expand N and M times,
respectively. If their type-synonym-expanded forms will meet at some point (i.e.
will have same shapes according to `sameShapes` function), it's possible to find
where they meet in O(N+M) top-layer type synonym expansions and O(min(N,M))
comparisons. We first collect all the top-layer expansions of t1 and t2 in two
lists, then drop the prefix of the longer list so that they have same lengths.
Then we search through both lists in parallel, and return the first pair of
types that have same shapes. Inner types of these two types with same shapes
are then expanded using the same algorithm.

In case they don't meet, we return the last pair of types in the lists, which
has top-layer type synonyms completely expanded. (in this case the inner types
are not expanded at all, as the current form already shows the type error)
-}

-- | Expand type synonyms in given types only enough to make them as similar as
-- possible. Returned types are the same in terms of used type synonyms.
--
-- To expand all synonyms, see 'Type.expandTypeSynonyms'.
--
-- See `ExpandSynsFail` tests in tests testsuite/tests/typecheck/should_fail for
-- some examples of how this should work.
expandSynonymsToMatch :: Type -> Type -> (Type, Type)
expandSynonymsToMatch ty1 ty2 = (ty1_ret, ty2_ret)
  where
    (ty1_ret, ty2_ret) = go ty1 ty2

    -- | Returns (type synonym expanded version of first type,
    --            type synonym expanded version of second type)
    go :: Type -> Type -> (Type, Type)
    go t1 t2
      | t1 `pickyEqType` t2 =
        -- Types are same, nothing to do
        (t1, t2)

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2 =
        -- Type constructors are same. They may be synonyms, but we don't
        -- expand further.
        let (tys1', tys2') =
              unzip (zipWith (\ty1 ty2 -> go ty1 ty2) tys1 tys2)
         in (TyConApp tc1 tys1', TyConApp tc2 tys2')

    go (AppTy t1_1 t1_2) (AppTy t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in (mkAppTy t1_1' t1_2', mkAppTy t2_1' t2_2')

    go (FunTy t1_1 t1_2) (FunTy t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in (mkFunTy t1_1' t1_2', mkFunTy t2_1' t2_2')

    go (ForAllTy b1 t1) (ForAllTy b2 t2) =
      -- NOTE: We may have a bug here, but we just can't reproduce it easily.
      -- See D1016 comments for details and our attempts at producing a test
      -- case. Short version: We probably need RnEnv2 to really get this right.
      let (t1', t2') = go t1 t2
       in (ForAllTy b1 t1', ForAllTy b2 t2')

    go (CastTy ty1 _) ty2 = go ty1 ty2
    go ty1 (CastTy ty2 _) = go ty1 ty2

    go t1 t2 =
      -- See Note [Expanding type synonyms to make types similar] for how this
      -- works
      let
        t1_exp_tys = t1 : tyExpansions t1
        t2_exp_tys = t2 : tyExpansions t2
        t1_exps    = length t1_exp_tys
        t2_exps    = length t2_exp_tys
        dif        = abs (t1_exps - t2_exps)
      in
        followExpansions $
          zipEqual "expandSynonymsToMatch.go"
            (if t1_exps > t2_exps then drop dif t1_exp_tys else t1_exp_tys)
            (if t2_exps > t1_exps then drop dif t2_exp_tys else t2_exp_tys)

    -- | Expand the top layer type synonyms repeatedly, collect expansions in a
    -- list. The list does not include the original type.
    --
    -- Example, if you have:
    --
    --   type T10 = T9
    --   type T9  = T8
    --   ...
    --   type T0  = Int
    --
    -- `tyExpansions T10` returns [T9, T8, T7, ... Int]
    --
    -- This only expands the top layer, so if you have:
    --
    --   type M a = Maybe a
    --
    -- `tyExpansions (M T10)` returns [Maybe T10] (T10 is not expanded)
    tyExpansions :: Type -> [Type]
    tyExpansions = unfoldr (\t -> (\x -> (x, x)) `fmap` tcView t)

    -- | Drop the type pairs until types in a pair look alike (i.e. the outer
    -- constructors are the same).
    followExpansions :: [(Type, Type)] -> (Type, Type)
    followExpansions [] = pprPanic "followExpansions" empty
    followExpansions [(t1, t2)]
      | sameShapes t1 t2 = go t1 t2 -- expand subtrees
      | otherwise        = (t1, t2) -- the difference is already visible
    followExpansions ((t1, t2) : tss)
      -- Traverse subtrees when the outer shapes are the same
      | sameShapes t1 t2 = go t1 t2
      -- Otherwise follow the expansions until they look alike
      | otherwise = followExpansions tss

    sameShapes :: Type -> Type -> Bool
    sameShapes AppTy{}          AppTy{}          = True
    sameShapes (TyConApp tc1 _) (TyConApp tc2 _) = tc1 == tc2
    sameShapes (FunTy {})       (FunTy {})       = True
    sameShapes (ForAllTy {})    (ForAllTy {})    = True
    sameShapes (CastTy ty1 _)   ty2              = sameShapes ty1 ty2
    sameShapes ty1              (CastTy ty2 _)   = sameShapes ty1 ty2
    sameShapes _                _                = False

sameOccExtra :: TcType -> TcType -> SDoc
-- See Note [Disambiguating (X ~ X) errors]
sameOccExtra ty1 ty2
  | Just (tc1, _) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, _) <- tcSplitTyConApp_maybe ty2
  , let n1 = tyConName tc1
        n2 = tyConName tc2
        same_occ = nameOccName n1                   == nameOccName n2
        same_pkg = moduleUnitId (nameModule n1) == moduleUnitId (nameModule n2)
  , n1 /= n2   -- Different Names
  , same_occ   -- but same OccName
  = text "NB:" <+> (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  | otherwise
  = empty
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> text "is defined at")
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ text "is defined in" <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnitId) $
                    nest 4 $ text "in package" <+> quotes (ppr pkg) ])
       where
         pkg = moduleUnitId mod
         mod = nameModule nm
         loc = nameSrcSpan nm

{-
Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

This initially came up in Trac #8968, concerning pattern synonyms.

Note [Disambiguating (X ~ X) errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #8278

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

mkDictErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkDictErr ctxt cts
  = ASSERT( not (null cts) )
    do { inst_envs <- tcGetInstEnvs
       ; let (ct1:_) = cts  -- ct1 just for its location
             min_cts = elim_superclasses cts
             lookups = map (lookup_cls_inst inst_envs) min_cts
             (no_inst_cts, overlap_cts) = partition is_no_inst lookups

       -- Report definite no-instance errors,
       -- or (iff there are none) overlap errors
       -- But we report only one of them (hence 'head') because they all
       -- have the same source-location origin, to try avoid a cascade
       -- of error from one location
       ; (ctxt, err) <- mk_dict_err ctxt (head (no_inst_cts ++ overlap_cts))
       ; mkErrorMsgFromCt ctxt ct1 (important err) }
  where
    no_givens = null (getUserGivens ctxt)

    is_no_inst (ct, (matches, unifiers, _))
      =  no_givens
      && null matches
      && (null unifiers || all (not . isAmbiguousTyVar) (tyCoVarsOfCtList ct))

    lookup_cls_inst inst_envs ct
                -- Note [Flattening in error message generation]
      = (ct, lookupInstEnv True inst_envs clas (flattenTys emptyInScopeSet tys))
      where
        (clas, tys) = getClassPredTys (ctPred ct)


    -- When simplifying [W] Ord (Set a), we need
    --    [W] Eq a, [W] Ord a
    -- but we really only want to report the latter
    elim_superclasses cts
      = filter (\ct -> any (eqType (ctPred ct)) min_preds) cts
      where
        min_preds = mkMinimalBySCs (map ctPred cts)

mk_dict_err :: ReportErrCtxt -> (Ct, ClsInstLookupResult)
            -> TcM (ReportErrCtxt, SDoc)
-- Report an overlap error if this class constraint results
-- from an overlap (returning Left clas), otherwise return (Right pred)
mk_dict_err ctxt@(CEC {cec_encl = implics}) (ct, (matches, unifiers, unsafe_overlapped))
  | null matches  -- No matches but perhaps several unifiers
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; candidate_insts <- get_candidate_instances
       ; return (ctxt, cannot_resolve_msg ct candidate_insts binds_msg) }

  | null unsafe_overlapped   -- Some matches => overlap errors
  = return (ctxt, overlap_msg)

  | otherwise
  = return (ctxt, safe_haskell_msg)
  where
    orig          = ctOrigin ct
    pred          = ctPred ct
    (clas, tys)   = getClassPredTys pred
    ispecs        = [ispec | (ispec, _) <- matches]
    unsafe_ispecs = [ispec | (ispec, _) <- unsafe_overlapped]
    useful_givens = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
         -- useful_givens are the enclosing implications with non-empty givens,
         -- modulo the horrid discardProvCtxtGivens

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

    cannot_resolve_msg :: Ct -> [ClsInst] -> SDoc -> SDoc
    cannot_resolve_msg ct candidate_insts binds_msg
      = vcat [ no_inst_msg
             , nest 2 extra_note
             , vcat (pp_givens useful_givens)
             , mb_patsyn_prov `orElse` empty
             , ppWhen (has_ambig_tvs && not (null unifiers && null useful_givens))
               (vcat [ ppUnless lead_with_ambig ambig_msg, binds_msg, potential_msg ])

             , ppWhen (isNothing mb_patsyn_prov) $
                   -- Don't suggest fixes for the provided context of a pattern
                   -- synonym; the right fix is to bind more in the pattern
               show_fixes (ctxtFixes has_ambig_tvs pred implics
                           ++ drv_fixes)
             , ppWhen (not (null candidate_insts))
               (hang (text "There are instances for similar types:")
                   2 (vcat (map ppr candidate_insts))) ]
                   -- See Note [Report candidate instances]
      where
        orig = ctOrigin ct
        -- See Note [Highlighting ambiguous type variables]
        lead_with_ambig = has_ambig_tvs && not (any isRuntimeUnkSkol ambig_tvs)
                        && not (null unifiers) && null useful_givens

        (has_ambig_tvs, ambig_msg) = mkAmbigMsg lead_with_ambig ct
        ambig_tvs = uncurry (++) (getAmbigTkvs ct)

        no_inst_msg
          | lead_with_ambig
          = ambig_msg <+> pprArising orig
              $$ text "prevents the constraint" <+>  quotes (pprParendType pred)
              <+> text "from being solved."

          | null useful_givens
          = addArising orig $ text "No instance for"
            <+> pprParendType pred

          | otherwise
          = addArising orig $ text "Could not deduce"
            <+> pprParendType pred

        potential_msg
          = ppWhen (not (null unifiers) && want_potential orig) $
            sdocWithDynFlags $ \dflags ->
            getPprStyle $ \sty ->
            pprPotentials dflags sty potential_hdr unifiers

        potential_hdr
          = vcat [ ppWhen lead_with_ambig $
                     text "Probable fix: use a type annotation to specify what"
                     <+> pprQuotedList ambig_tvs <+> text "should be."
                 , text "These potential instance" <> plural unifiers
                   <+> text "exist:"]

        mb_patsyn_prov :: Maybe SDoc
        mb_patsyn_prov
          | not lead_with_ambig
          , ProvCtxtOrigin PSB{ psb_def = L _ pat } <- orig
          = Just (vcat [ text "In other words, a successful match on the pattern"
                       , nest 2 $ ppr pat
                       , text "does not provide the constraint" <+> pprParendType pred ])
          | otherwise = Nothing

    -- Report "potential instances" only when the constraint arises
    -- directly from the user's use of an overloaded function
    want_potential (TypeEqOrigin {}) = False
    want_potential _                 = True

    extra_note | any isFunTy (filterOutInvisibleTypes (classTyCon clas) tys)
               = text "(maybe you haven't applied a function to enough arguments?)"
               | className clas == typeableClassName  -- Avoid mysterious "No instance for (Typeable T)
               , [_,ty] <- tys                        -- Look for (Typeable (k->*) (T k))
               , Just (tc,_) <- tcSplitTyConApp_maybe ty
               , not (isTypeFamilyTyCon tc)
               = hang (text "GHC can't yet do polykinded")
                    2 (text "Typeable" <+>
                       parens (ppr ty <+> dcolon <+> ppr (typeKind ty)))
               | otherwise
               = empty

    drv_fixes = case orig of
                   DerivOrigin      -> [drv_fix]
                   DerivOriginDC {} -> [drv_fix]
                   DerivOriginCoerce {} -> [drv_fix]
                   _                -> []

    drv_fix = hang (text "use a standalone 'deriving instance' declaration,")
                 2 (text "so you can specify the instance context yourself")

    -- Normal overlap error
    overlap_msg
      = ASSERT( not (null matches) )
        vcat [  addArising orig (text "Overlapping instances for"
                                <+> pprType (mkClassPred clas tys))

             ,  ppUnless (null matching_givens) $
                  sep [text "Matching givens (or their superclasses):"
                      , nest 2 (vcat matching_givens)]

             ,  sdocWithDynFlags $ \dflags ->
                getPprStyle $ \sty ->
                pprPotentials dflags sty (text "Matching instances:") $
                ispecs ++ unifiers

             ,  ppWhen (null matching_givens && isSingleton matches && null unifiers) $
                -- Intuitively, some given matched the wanted in their
                -- flattened or rewritten (from given equalities) form
                -- but the matcher can't figure that out because the
                -- constraints are non-flat and non-rewritten so we
                -- simply report back the whole given
                -- context. Accelerate Smart.hs showed this problem.
                  sep [ text "There exists a (perhaps superclass) match:"
                      , nest 2 (vcat (pp_givens useful_givens))]

             ,  ppWhen (isSingleton matches) $
                parens (vcat [ text "The choice depends on the instantiation of" <+>
                                  quotes (pprWithCommas ppr (tyCoVarsOfTypesList tys))
                             , ppWhen (null (matching_givens)) $
                               vcat [ text "To pick the first instance above, use IncoherentInstances"
                                    , text "when compiling the other instance declarations"]
                        ])]

    matching_givens = mapMaybe matchable useful_givens

    matchable (Implic { ic_given = evvars, ic_info = skol_info, ic_env = env })
      = case ev_vars_matching of
             [] -> Nothing
             _  -> Just $ hang (pprTheta ev_vars_matching)
                            2 (sep [ text "bound by" <+> ppr skol_info
                                   , text "at" <+> ppr (tcl_loc env) ])
        where ev_vars_matching = filter ev_var_matches (map evVarPred evvars)
              ev_var_matches ty = case getClassPredTys_maybe ty of
                 Just (clas', tys')
                   | clas' == clas
                   , Just _ <- tcMatchTys tys tys'
                   -> True
                   | otherwise
                   -> any ev_var_matches (immSuperClasses clas' tys')
                 Nothing -> False

    -- Overlap error because of Safe Haskell (first
    -- match should be the most specific match)
    safe_haskell_msg
     = ASSERT( matches `lengthIs` 1 && not (null unsafe_ispecs) )
       vcat [ addArising orig (text "Unsafe overlapping instances for"
                       <+> pprType (mkClassPred clas tys))
            , sep [text "The matching instance is:",
                   nest 2 (pprInstance $ head ispecs)]
            , vcat [ text "It is compiled in a Safe module and as such can only"
                   , text "overlap instances from the same module, however it"
                   , text "overlaps the following instances from different" <+>
                     text "modules:"
                   , nest 2 (vcat [pprInstances $ unsafe_ispecs])
                   ]
            ]


ctxtFixes :: Bool -> PredType -> [Implication] -> [SDoc]
ctxtFixes has_ambig_tvs pred implics
  | not has_ambig_tvs
  , isTyVarClassPred pred
  , (skol:skols) <- usefulContext implics pred
  , let what | null skols
             , SigSkol (PatSynCtxt {}) _ _ <- skol
             = text "\"required\""
             | otherwise
             = empty
  = [sep [ text "add" <+> pprParendType pred
           <+> text "to the" <+> what <+> text "context of"
         , nest 2 $ ppr_skol skol $$
                    vcat [ text "or" <+> ppr_skol skol
                         | skol <- skols ] ] ]
  | otherwise = []
  where
    ppr_skol (PatSkol (RealDataCon dc) _) = text "the data constructor" <+> quotes (ppr dc)
    ppr_skol (PatSkol (PatSynCon ps)   _) = text "the pattern synonym"  <+> quotes (ppr ps)
    ppr_skol skol_info = ppr skol_info

discardProvCtxtGivens :: CtOrigin -> [UserGiven] -> [UserGiven]
discardProvCtxtGivens orig givens  -- See Note [discardProvCtxtGivens]
  | ProvCtxtOrigin (PSB {psb_id = L _ name}) <- orig
  = filterOut (discard name) givens
  | otherwise
  = givens
  where
    discard n (Implic { ic_info = SigSkol (PatSynCtxt n') _ _ }) = n == n'
    discard _ _                                                  = False

usefulContext :: [Implication] -> PredType -> [SkolemInfo]
-- usefulContext picks out the implications whose context
-- the programmer might plausibly augment to solve 'pred'
usefulContext implics pred
  = go implics
  where
    pred_tvs = tyCoVarsOfType pred
    go [] = []
    go (ic : ics)
       | implausible ic = rest
       | otherwise      = ic_info ic : rest
       where
          -- Stop when the context binds a variable free in the predicate
          rest | any (`elemVarSet` pred_tvs) (ic_skols ic) = []
               | otherwise                                 = go ics

    implausible ic
      | null (ic_skols ic)            = True
      | implausible_info (ic_info ic) = True
      | otherwise                     = False

    implausible_info (SigSkol (InfSigCtxt {}) _ _) = True
    implausible_info _                             = False
    -- Do not suggest adding constraints to an *inferred* type signature

{- Note [Report candidate instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved (Num Int), where `Int` is not the Prelude Int,
but comes from some other module, then it may be helpful to point out
that there are some similarly named instances elsewhere.  So we get
something like
    No instance for (Num Int) arising from the literal ‘3’
    There are instances for similar types:
      instance Num GHC.Types.Int -- Defined in ‘GHC.Num’
Discussion in Trac #9611.

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

Note [discardProvCtxtGivens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most situations we call all enclosing implications "useful". There is one
exception, and that is when the constraint that causes the error is from the
"provided" context of a pattern synonym declaration:

  pattern Pat :: (Num a, Eq a) => Show a   => a -> Maybe a
             --  required      => provided => type
  pattern Pat x <- (Just x, 4)

When checking the pattern RHS we must check that it does actually bind all
the claimed "provided" constraints; in this case, does the pattern (Just x, 4)
bind the (Show a) constraint.  Answer: no!

But the implication we generate for this will look like
   forall a. (Num a, Eq a) => [W] Show a
because when checking the pattern we must make the required
constraints available, since they are needed to match the pattern (in
this case the literal '4' needs (Num a, Eq a)).

BUT we don't want to suggest adding (Show a) to the "required" constraints
of the pattern synonym, thus:
  pattern Pat :: (Num a, Eq a, Show a) => Show a => a -> Maybe a
It would then typecheck but it's silly.  We want the /pattern/ to bind
the alleged "provided" constraints, Show a.

So we suppress that Implication in discardProvCtxtGivens.  It's
painfully ad-hoc but the truth is that adding it to the "required"
constraints would work.  Suprressing it solves two problems.  First,
we never tell the user that we could not deduce a "provided"
constraint from the "required" context. Second, we never give a
possible fix that suggests to add a "provided" constraint to the
"required" context.

For example, without this distinction the above code gives a bad error
message (showing both problems):

  error: Could not deduce (Show a) ... from the context: (Eq a)
         ... Possible fix: add (Show a) to the context of
         the signature for pattern synonym `Pat' ...

-}

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ text "Possible fix:"
                        , nest 2 (vcat (f : map (text "or" <+>) fs))]

pprPotentials :: DynFlags -> PprStyle -> SDoc -> [ClsInst] -> SDoc
-- See Note [Displaying potential instances]
pprPotentials dflags sty herald insts
  | null insts
  = empty

  | null show_these
  = hang herald
       2 (vcat [ not_in_scope_msg empty
               , flag_hint ])

  | otherwise
  = hang herald
       2 (vcat [ pprInstances show_these
               , ppWhen (n_in_scope_hidden > 0) $
                 text "...plus"
                   <+> speakNOf n_in_scope_hidden (text "other")
               , not_in_scope_msg (text "...plus")
               , flag_hint ])
  where
    n_show = 3 :: Int
    show_potentials = gopt Opt_PrintPotentialInstances dflags

    (in_scope, not_in_scope) = partition inst_in_scope insts
    sorted = sortBy fuzzyClsInstCmp in_scope
    show_these | show_potentials = sorted
               | otherwise       = take n_show sorted
    n_in_scope_hidden = length sorted - length show_these

       -- "in scope" means that all the type constructors
       -- are lexically in scope; these instances are likely
       -- to be more useful
    inst_in_scope :: ClsInst -> Bool
    inst_in_scope cls_inst = nameSetAll name_in_scope $
                             orphNamesOfTypes (is_tys cls_inst)

    name_in_scope name
      | isBuiltInSyntax name
      = True -- E.g. (->)
      | Just mod <- nameModule_maybe name
      = qual_in_scope (qualName sty mod (nameOccName name))
      | otherwise
      = True

    qual_in_scope :: QualifyName -> Bool
    qual_in_scope NameUnqual    = True
    qual_in_scope (NameQual {}) = True
    qual_in_scope _             = False

    not_in_scope_msg herald
      | null not_in_scope
      = empty
      | otherwise
      = hang (herald <+> speakNOf (length not_in_scope) (text "instance")
                     <+> text "involving out-of-scope types")
           2 (ppWhen show_potentials (pprInstances not_in_scope))

    flag_hint = ppUnless (show_potentials || equalLength show_these insts) $
                text "(use -fprint-potential-instances to see them all)"

{- Note [Displaying potential instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When showing a list of instances for
  - overlapping instances (show ones that match)
  - no such instance (show ones that could match)
we want to give it a bit of structure.  Here's the plan

* Say that an instance is "in scope" if all of the
  type constructors it mentions are lexically in scope.
  These are the ones most likely to be useful to the programmer.

* Show at most n_show in-scope instances,
  and summarise the rest ("plus 3 others")

* Summarise the not-in-scope instances ("plus 4 not in scope")

* Add the flag -fshow-potential-instances which replaces the
  summary with the full list
-}

{-
Note [Flattening in error message generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (C (Maybe (F x))), where F is a type function, and we have
instances
                C (Maybe Int) and C (Maybe a)
Since (F x) might turn into Int, this is an overlap situation, and
indeed (because of flattening) the main solver will have refrained
from solving.  But by the time we get to error message generation, we've
un-flattened the constraint.  So we must *re*-flatten it before looking
up in the instance environment, lest we only report one matching
instance when in fact there are two.

Re-flattening is pretty easy, because we don't need to keep track of
evidence.  We don't re-use the code in TcCanonical because that's in
the TcS monad, and we are in TcM here.

Note [Suggest -fprint-explicit-kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (Trac #9171)
    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’
The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.
This test suggests -fprint-explicit-kinds when all the ambiguous type
variables are kind variables.
-}

mkAmbigMsg :: Bool -- True when message has to be at beginning of sentence
           -> Ct -> (Bool, SDoc)
mkAmbigMsg prepend_msg ct
  | null ambig_kvs && null ambig_tvs = (False, empty)
  | otherwise                        = (True,  msg)
  where
    (ambig_kvs, ambig_tvs) = getAmbigTkvs ct

    msg |  any isRuntimeUnkSkol ambig_kvs  -- See Note [Runtime skolems]
        || any isRuntimeUnkSkol ambig_tvs
        = vcat [ text "Cannot resolve unknown runtime type"
                 <> plural ambig_tvs <+> pprQuotedList ambig_tvs
               , text "Use :print or :force to determine these types"]

        | not (null ambig_tvs)
        = pp_ambig (text "type") ambig_tvs

        | otherwise  -- All ambiguous kind variabes; suggest -fprint-explicit-kinds
                     -- See Note [Suggest -fprint-explicit-kinds]
        = vcat [ pp_ambig (text "kind") ambig_kvs
               , ppSuggestExplicitKinds ]

    pp_ambig what tkvs
      | prepend_msg -- "Ambiguous type variable 't0'"
      = text "Ambiguous" <+> what <+> text "variable"
        <> plural tkvs <+> pprQuotedList tkvs

      | otherwise -- "The type variable 't0' is ambiguous"
      = text "The" <+> what <+> text "variable" <> plural tkvs
        <+> pprQuotedList tkvs <+> is_or_are tkvs <+> text "ambiguous"

    is_or_are [_] = text "is"
    is_or_are _   = text "are"

pprSkol :: [Implication] -> TcTyVar -> SDoc
pprSkol implics tv
  = case skol_info of
      UnkSkol -> quotes (ppr tv) <+> text "is an unknown type variable"
      _       -> ppr_rigid (pprSkolInfo skol_info)
  where
    Implic { ic_info = skol_info } = getSkolemInfo implics tv
    ppr_rigid pp_info
       = hang (quotes (ppr tv) <+> text "is a rigid type variable bound by")
            2 (sep [ pp_info
                   , text "at" <+> ppr (getSrcSpan tv) ])

getAmbigTkvs :: Ct -> ([Var],[Var])
getAmbigTkvs ct
  = partition (`elemVarSet` dep_tkv_set) ambig_tkvs
  where
    tkvs       = tyCoVarsOfCtList ct
    ambig_tkvs = filter isAmbiguousTyVar tkvs
    dep_tkv_set = tyCoVarsOfTypes (map tyVarKind tkvs)

getSkolemInfo :: [Implication] -> TcTyVar -> Implication
-- Get the skolem info for a type variable
-- from the implication constraint that binds it
getSkolemInfo [] tv
  = pprPanic "No skolem info:" (ppr tv)

getSkolemInfo (implic:implics) tv
  | tv `elem` ic_skols implic = implic
  | otherwise                 = getSkolemInfo implics tv

-----------------------
-- relevantBindings looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.
--
-- We always remove closed top-level bindings, though,
-- since they are never relevant (cf Trac #8233)

relevantBindings :: Bool  -- True <=> filter by tyvar; False <=> no filtering
                          -- See Trac #8191
                 -> ReportErrCtxt -> Ct
                 -> TcM (ReportErrCtxt, SDoc, Ct)
-- Also returns the zonked and tidied CtOrigin of the constraint
relevantBindings want_filtering ctxt ct
  = do { dflags <- getDynFlags
       ; (env1, tidy_orig) <- zonkTidyOrigin (cec_tidy ctxt) (ctLocOrigin loc)
       ; let ct_tvs = tyCoVarsOfCt ct `unionVarSet` extra_tvs

             -- For *kind* errors, report the relevant bindings of the
             -- enclosing *type* equality, because that's more useful for the programmer
             extra_tvs = case tidy_orig of
                             KindEqOrigin t1 m_t2 _ _ -> tyCoVarsOfTypes $
                                                         t1 : maybeToList m_t2
                             _                        -> emptyVarSet
       ; traceTc "relevantBindings" $
           vcat [ ppr ct
                , pprCtOrigin (ctLocOrigin loc)
                , ppr ct_tvs
                , pprWithCommas id [ ppr id <+> dcolon <+> ppr (idType id)
                                   | TcIdBndr id _ <- tcl_bndrs lcl_env ]
                , pprWithCommas id
                    [ ppr id | TcIdBndr_ExpType id _ _ <- tcl_bndrs lcl_env ] ]

       ; (tidy_env', docs, discards)
              <- go dflags env1 ct_tvs (maxRelevantBinds dflags)
                    emptyVarSet [] False
                    (remove_shadowing $ tcl_bndrs lcl_env)
         -- tcl_bndrs has the innermost bindings first,
         -- which are probably the most relevant ones

       ; let doc = ppUnless (null docs) $
                   hang (text "Relevant bindings include")
                      2 (vcat docs $$ ppWhen discards discardMsg)

             -- Put a zonked, tidied CtOrigin into the Ct
             loc'  = setCtLocOrigin loc tidy_orig
             ct'   = setCtLoc ct loc'
             ctxt' = ctxt { cec_tidy = tidy_env' }

       ; return (ctxt', doc, ct') }
  where
    ev      = ctEvidence ct
    loc     = ctEvLoc ev
    lcl_env = ctLocEnv loc

    run_out :: Maybe Int -> Bool
    run_out Nothing = False
    run_out (Just n) = n <= 0

    dec_max :: Maybe Int -> Maybe Int
    dec_max = fmap (\n -> n - 1)

    ---- fixes #12177
    ---- builds up a list of bindings whose OccName has not been seen before
    remove_shadowing :: [TcIdBinder] -> [TcIdBinder]
    remove_shadowing bindings = reverse $ fst $ foldl
      (\(bindingAcc, seenNames) binding ->
        if (occName binding) `elemOccSet` seenNames -- if we've seen it
          then (bindingAcc, seenNames)              -- skip it
          else (binding:bindingAcc, extendOccSet seenNames (occName binding)))
      ([], emptyOccSet) bindings

    go :: DynFlags -> TidyEnv -> TcTyVarSet -> Maybe Int -> TcTyVarSet -> [SDoc]
       -> Bool                          -- True <=> some filtered out due to lack of fuel
       -> [TcIdBinder]
       -> TcM (TidyEnv, [SDoc], Bool)   -- The bool says if we filtered any out
                                        -- because of lack of fuel
    go _ tidy_env _ _ _ docs discards []
      = return (tidy_env, reverse docs, discards)
    go dflags tidy_env ct_tvs n_left tvs_seen docs discards (tc_bndr : tc_bndrs)
      = case tc_bndr of
          TcIdBndr id top_lvl -> go2 (idName id) (idType id) top_lvl
          TcIdBndr_ExpType name et top_lvl ->
            do { mb_ty <- readExpType_maybe et
                   -- et really should be filled in by now. But there's a chance
                   -- it hasn't, if, say, we're reporting a kind error en route to
                   -- checking a term. See test indexed-types/should_fail/T8129
                   -- Or we are reporting errors from the ambiguity check on
                   -- a local type signature
               ; case mb_ty of
                   Just ty -> go2 name ty top_lvl
                   Nothing -> discard_it  -- No info; discard
               }
      where
        discard_it = go dflags tidy_env ct_tvs n_left tvs_seen docs
                        discards tc_bndrs
        go2 id_name id_type top_lvl
          = do { (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env id_type
               ; traceTc "relevantBindings 1" (ppr id_name <+> dcolon <+> ppr tidy_ty)
               ; let id_tvs = tyCoVarsOfType tidy_ty
                     doc = sep [ pprPrefixOcc id_name <+> dcolon <+> ppr tidy_ty
                               , nest 2 (parens (text "bound at"
                                    <+> ppr (getSrcLoc id_name)))]
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
                 then go dflags tidy_env ct_tvs n_left tvs_seen docs
                         True      -- Record that we have now discarded something
                         tc_bndrs

                          -- Keep this binding, decrement fuel
                 else go dflags tidy_env' ct_tvs (dec_max n_left) new_seen
                         (doc:docs) discards tc_bndrs }

discardMsg :: SDoc
discardMsg = text "(Some bindings suppressed;" <+>
             text "use -fmax-relevant-binds=N or -fno-max-relevant-binds)"

subsDiscardMsg :: SDoc
subsDiscardMsg =
    text "(Some substitutions suppressed;" <+>
    text "use -fmax-valid-substitutions=N or -fno-max-valid-substitutions)"

-----------------------
warnDefaulting :: [Ct] -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyCoVars env0 $
                        tyCoVarsOfCtsList (listToBag wanteds)
             tidy_wanteds = map (tidyCt tidy_env) wanteds
             (loc, ppr_wanteds) = pprWithArising tidy_wanteds
             warn_msg =
                hang (hsep [ text "Defaulting the following"
                           , text "constraint" <> plural tidy_wanteds
                           , text "to type"
                           , quotes (ppr default_ty) ])
                     2
                     ppr_wanteds
       ; setCtLocM loc $ warnTc (Reason Opt_WarnTypeDefaults) warn_default warn_msg }

{-
Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in RtClosureInspect.zonkRTTIType.

************************************************************************
*                                                                      *
                 Error from the canonicaliser
         These ones are called *during* constraint simplification
*                                                                      *
************************************************************************
-}

solverDepthErrorTcS :: CtLoc -> TcType -> TcM a
solverDepthErrorTcS loc ty
  = setCtLocM loc $
    do { ty <- zonkTcType ty
       ; env0 <- tcInitTidyEnv
       ; let tidy_env     = tidyFreeTyCoVars env0 (tyCoVarsOfTypeList ty)
             tidy_ty      = tidyType tidy_env ty
             msg
               = vcat [ text "Reduction stack overflow; size =" <+> ppr depth
                      , hang (text "When simplifying the following type:")
                           2 (ppr tidy_ty)
                      , note ]
       ; failWithTcM (tidy_env, msg) }
  where
    depth = ctLocDepth loc
    note = vcat
      [ text "Use -freduction-depth=0 to disable this check"
      , text "(any upper bound you could choose might fail unpredictably with"
      , text " minor updates to GHC, so disabling the check is recommended if"
      , text " you're sure that type checking should terminate)" ]
