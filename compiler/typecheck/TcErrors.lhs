\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcErrors( 
       reportUnsolved, reportAllUnsolved,
       warnDefaulting,

       solverDepthErrorTcS
  ) where

#include "HsVersions.h"

import TcCanonical( occurCheckExpand )
import TcRnTypes
import TcRnMonad
import TcMType
import TcType
import TypeRep
import Type
import Kind ( isKind )
import Unify            ( tcMatchTys )
import Inst
import InstEnv
import TyCon
import TcEvidence
import Name
import Id 
import Var
import VarSet
import VarEnv
import Bag
import Maybes
import ErrUtils         ( ErrMsg, makeIntoWarning, pprLocErrMsg )
import BasicTypes 
import Util
import FastString
import Outputable
import SrcLoc
import DynFlags
import Data.List        ( partition, mapAccumL )
\end{code}

%************************************************************************
%*									*
\section{Errors and contexts}
%*									*
%************************************************************************

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
and does not fail if -fwarn-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.

\begin{code}
reportUnsolved :: WantedConstraints -> TcM (Bag EvBind)
reportUnsolved wanted
  = do { binds_var <- newTcEvBinds
       ; defer <- doptM Opt_DeferTypeErrors
       ; report_unsolved (Just binds_var) defer wanted
       ; getTcEvBinds binds_var }

reportAllUnsolved :: WantedConstraints -> TcM ()
-- Report all unsolved goals, even if -fdefer-type-errors is on
-- See Note [Deferring coercion errors to runtime]
reportAllUnsolved wanted 
  = report_unsolved Nothing (panic "reportAllUnsolved") wanted

report_unsolved :: Maybe EvBindsVar  -- cec_binds
                -> Bool              -- cec_defer
                -> WantedConstraints -> TcM ()
-- Important precondition:
-- WantedConstraints are fully zonked and unflattened, that is,
-- zonkWC has already been applied to these constraints.
report_unsolved mb_binds_var defer wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do { traceTc "reportUnsolved (before unflattening)" (ppr wanted)

       ; env0 <- tcInitTidyEnv
                 
            -- If we are deferring we are going to need /all/ evidence around,
            -- including the evidence produced by unflattening (zonkWC)
       ; errs_so_far <- ifErrsM (return True) (return False)
       ; let tidy_env = tidyFreeTyVars env0 free_tvs
             free_tvs = tyVarsOfWC wanted
             err_ctxt = CEC { cec_encl  = []
                            , cec_insol = errs_so_far || insolubleWC wanted
                                          -- Don't report ambiguity errors if
                                          -- there are any other solid errors 
                                          -- to report
                            , cec_tidy  = tidy_env
                            , cec_defer    = defer
                            , cec_suppress = False
                            , cec_binds    = mb_binds_var }

       ; traceTc "reportUnsolved (after unflattening):" $ 
         vcat [ pprTvBndrs (varSetElems free_tvs)
              , ppr wanted ]

       ; reportWanteds err_ctxt wanted }

--------------------------------------------
--      Internal functions
--------------------------------------------

data ReportErrCtxt 
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                	       	       --   (innermost first)
                                       -- ic_skols and givens are tidied, rest are not
          , cec_tidy  :: TidyEnv
          , cec_insol :: Bool       -- True <=> do not report errors involving 
                                    --          ambiguous errors

          , cec_binds :: Maybe EvBindsVar 
                         -- Nothinng <=> Report all errors, including holes; no bindings
                         -- Just ev  <=> make some errors (depending on cec_defer)
                         --              into warnings, and emit evidence bindings
                         --              into 'ev' for unsolved constraints

          , cec_defer :: Bool       -- True <=> -fdefer-type-errors
                                    -- Irrelevant if cec_binds = Nothing
          , cec_suppress :: Bool    -- True <=> More important errors have occurred,
                                    --          so create bindings if need be, but
                                    --          don't issue any more errors/warnings
      }

reportImplic :: ReportErrCtxt -> Implication -> TcM ()
reportImplic ctxt implic@(Implic { ic_skols = tvs, ic_given = given
                                 , ic_wanted = wanted, ic_binds = evb
                                 , ic_insol = insoluble, ic_info = info })
  | BracketSkol <- info
  , not insoluble -- For Template Haskell brackets report only
  = return ()     -- definite errors. The whole thing will be re-checked
                  -- later when we plug it in, and meanwhile there may
                  -- certainly be un-satisfied constraints

  | otherwise
  = reportWanteds ctxt' wanted
  where
    (env1, tvs') = mapAccumL tidyTyVarBndr (cec_tidy ctxt) tvs
    (env2, info') = tidySkolemInfo env1 info
    implic' = implic { ic_skols = tvs'
                     , ic_given = map (tidyEvVar env2) given
                     , ic_info  = info' }
    ctxt' = ctxt { cec_tidy  = env2
                 , cec_encl  = implic' : cec_encl ctxt
                 , cec_binds = case cec_binds ctxt of
                                 Nothing -> Nothing
                                 Just {} -> Just evb }

reportWanteds :: ReportErrCtxt -> WantedConstraints -> TcM ()
reportWanteds ctxt (WC { wc_flat = flats, wc_insol = insols, wc_impl = implics })
  = do { reportFlats ctxt tidy_cts
       ; mapBagM_ (reportImplic ctxt) implics }
  where
    env = cec_tidy ctxt
    tidy_cts = mapBag (tidyCt env) (insols `unionBags` flats)
                  -- All the Derived ones have been filtered out alrady
                  -- by the constraint solver. This is ok; we don't want
                  -- to report unsolved Derived goals as error
                  -- See Note [Do not report derived but soluble errors]

reportFlats :: ReportErrCtxt -> Cts -> TcM ()
reportFlats ctxt flats    -- Here 'flats' includes insolble goals
  = tryReporters 
      [ -- First deal with things that are utterly wrong
        -- Like Int ~ Bool (incl nullary TyCons)
        -- or  Int ~ t a   (AppTy on one side)
        ("Utterly wrong",  utterly_wrong,   mkGroupReporter mkEqErr)
      , ("Holes",          is_hole,         mkUniReporter mkHoleError)

        -- Report equalities of form (a~ty).  They are usually
        -- skolem-equalities, and they cause confusing knock-on 
        -- effects in other errors; see test T4093b.
      , ("Skolem equalities",    skolem_eq,   mkUniReporter mkEqErr1)
      , ("Unambiguous",          unambiguous, reportFlatErrs) ]
      reportAmbigErrs
      ctxt (bagToList flats)
  where
    utterly_wrong, skolem_eq, unambiguous :: Ct -> PredTree -> Bool

    utterly_wrong _ (EqPred ty1 ty2) = isRigid ty1 && isRigid ty2 
    utterly_wrong _ _ = False

    is_hole ct _ = isHoleCt ct

    skolem_eq _ (EqPred ty1 ty2) = isRigidOrSkol ty1 && isRigidOrSkol ty2 
    skolem_eq _ _ = False

    unambiguous ct pred 
      | not (any isAmbiguousTyVar (varSetElems (tyVarsOfCt ct)))
      = True
      | otherwise 
      = case pred of
          EqPred ty1 ty2 -> isNothing (isTyFun_maybe ty1) && isNothing (isTyFun_maybe ty2)
          _              -> False

---------------
isRigid, isRigidOrSkol :: Type -> Bool
isRigid ty 
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isDecomposableTyCon tc
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | isForAllTy ty                           = True
  | otherwise                               = False

isRigidOrSkol ty 
  | Just tv <- getTyVar_maybe ty = isSkolemTyVar tv
  | otherwise                    = isRigid ty

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isSynFamilyTyCon tc -> Just tc
                      _ -> Nothing

-----------------
reportAmbigErrs :: Reporter
reportAmbigErrs ctxt cts
  | cec_insol ctxt = return ()
  | otherwise      = reportFlatErrs ctxt cts
          -- Only report ambiguity if no other errors (at all) happened
          -- See Note [Avoiding spurious errors] in TcSimplify

reportFlatErrs :: Reporter
-- Called once for non-ambigs, once for ambigs
-- Report equality errors, and others only if we've done all 
-- the equalities.  The equality errors are more basic, and
-- can lead to knock on type-class errors
reportFlatErrs
  = tryReporters
      [ ("Equalities", is_equality, mkGroupReporter mkEqErr) ]
      (\ctxt cts -> do { let (dicts, ips, irreds) = go cts [] [] []
                       ; mkGroupReporter mkIPErr    ctxt ips   
                       ; mkGroupReporter mkIrredErr ctxt irreds
                       ; mkGroupReporter mkDictErr  ctxt dicts })
  where
    is_equality _ (EqPred {}) = True
    is_equality _ _           = False

    go [] dicts ips irreds
      = (dicts, ips, irreds)
    go (ct:cts) dicts ips irreds
      | isIPPred (ctPred ct) = go cts dicts (ct:ips) irreds
      | otherwise
      = case classifyPredType (ctPred ct) of
          ClassPred {}  -> go cts (ct:dicts) ips irreds
          IrredPred {}  -> go cts dicts ips (ct:irreds)
          _             -> panic "reportFlatErrs"
    -- TuplePreds should have been expanded away by the constraint
    -- simplifier, so they shouldn't show up at this point
    -- And EqPreds are dealt with by the is_equality test


--------------------------------------------
--      Reporters
--------------------------------------------

type Reporter = ReportErrCtxt -> [Ct] -> TcM ()

mkUniReporter :: (ReportErrCtxt -> Ct -> TcM ErrMsg) -> Reporter
-- Reports errors one at a time
mkUniReporter mk_err ctxt 
  = mapM_ $ \ct -> 
    do { err <- mk_err ctxt ct
       ; maybeReportError ctxt err ct
       ; maybeAddDeferredBinding ctxt err ct }

mkGroupReporter :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg)
                             -- Make error message for a group
                -> Reporter  -- Deal with lots of constraints
-- Group together insts from same location
-- We want to report them together in error messages

mkGroupReporter _ _ [] 
  = return ()
mkGroupReporter mk_err ctxt (ct1 : rest)
  = do { err <- mk_err ctxt cts
       ; maybeReportError ctxt err ct1
       ; mapM_ (maybeAddDeferredBinding ctxt err) (ct1:rest)
               -- Add deferred bindings for all
       ; mkGroupReporter mk_err ctxt others }
  where
   loc               = cc_loc ct1
   cts               = ct1 : friends
   (friends, others) = partition is_friend rest
   is_friend friend  = cc_loc friend `same_loc` loc

   same_loc :: CtLoc -> CtLoc -> Bool
   same_loc l1 l2 = ctLocSpan l1 == ctLocSpan l2

maybeReportError :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
-- Report the error and/or make a deferred binding for it
maybeReportError ctxt err ct
  | cec_suppress ctxt
  = return ()
  | isHoleCt ct || cec_defer ctxt  -- And it's a hole or we have -fdefer-type-errors
  = reportWarning (makeIntoWarning err)
  | otherwise
  = reportError err

maybeAddDeferredBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
-- See Note [Deferring coercion errors to runtime]
maybeAddDeferredBinding ctxt err ct
  | CtWanted { ctev_pred = pred, ctev_evar = ev_id } <- cc_ev ct
    -- Only add deferred bindings for Wanted constraints
  , isHoleCt ct || cec_defer ctxt  -- And it's a hole or we have -fdefer-type-errors
  , Just ev_binds_var <- cec_binds ctxt  -- We hvae somewhere to put the bindings
  = do { dflags <- getDynFlags
       ; let err_msg = pprLocErrMsg err
             err_fs  = mkFastString $ showSDoc dflags $
                       err_msg $$ text "(deferred type error)"

         -- Create the binding
       ; addTcEvBind ev_binds_var ev_id (EvDelayedError pred err_fs) }

  | otherwise   -- Do not set any evidence for Given/Derived
  = return ()   

tryReporters :: [(String, Ct -> PredTree -> Bool, Reporter)] 
             -> Reporter -> Reporter
-- Use the first reporter in the list whose predicate says True
tryReporters reporters deflt ctxt cts
  = do { traceTc "tryReporters {" (ppr cts) 
       ; go ctxt reporters cts
       ; traceTc "tryReporters }" empty }
  where
    go ctxt [] cts = deflt ctxt cts 
    go ctxt ((str, pred, reporter) : rs) cts
      | null yeses  = do { traceTc "tryReporters: no" (text str)
                         ; go ctxt rs cts }
      | otherwise   = do { traceTc "tryReporters: yes" (text str <+> ppr yeses)
                         ; reporter ctxt yeses :: TcM ()
                         ; go (ctxt { cec_suppress = True }) rs nos }
                         -- Carry on with the rest, because we must make
                         -- deferred bindings for them if we have 
                         -- -fdefer-type-errors
      where
       (yeses, nos) = partition keep_me cts
       keep_me ct = pred ct (classifyPredType (ctPred ct))

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
    loc = cc_loc ct
    ppr_one ct = hang (parens (pprType (ctPred ct))) 
                    2 (pprArisingAt (cc_loc ct))

mkErrorMsg :: ReportErrCtxt -> Ct -> SDoc -> TcM ErrMsg
mkErrorMsg ctxt ct msg 
  = do { let tcl_env = ctLocEnv (cc_loc ct)
       ; err_info <- mkErrInfo (cec_tidy ctxt) (tcl_ctxt tcl_env)
       ; mkLongErrAt (tcl_loc tcl_env) msg err_info }

type UserGiven = ([EvVar], SkolemInfo, SrcSpan)

getUserGivens :: ReportErrCtxt -> [UserGiven]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = ctxt})
  = reverse $
    [ (givens, info, tcl_loc env) 
    | Implic {ic_given = givens, ic_env = env, ic_info = info } <- ctxt
    , not (null givens) ]
\end{code}

Note [Do not report derived but soluble errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wc_flats include Derived constraints that have not been solved, but are
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

(We leave the Deriveds in wc_flat until reportErrors, so that we don't lose
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


%************************************************************************
%*                  *
                Irreducible predicate errors
%*                  *
%************************************************************************

\begin{code}
mkIrredErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIrredErr ctxt cts 
  = do { (ctxt, binds_msg) <- relevantBindings ctxt ct1
       ; mkErrorMsg ctxt ct1 (msg $$ binds_msg) }
  where
    (ct1:_) = cts
    orig    = ctLocOrigin (cc_loc ct1)
    givens  = getUserGivens ctxt
    msg = couldNotDeduce givens (map ctPred cts, orig)

----------------
mkHoleError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkHoleError ctxt ct@(CHoleCan {})
  = do { let tyvars = varSetElems (tyVarsOfCt ct)
             tyvars_msg = map loc_msg tyvars
             msg = (text "Found hole" <+> quotes (text "_") 
                    <+> text "with type") <+> pprType (cc_hole_ty ct)
                   $$ (if null tyvars_msg then empty else text "Where:" <+> vcat tyvars_msg)
       ; (ctxt, binds_doc) <- relevantBindings ctxt ct
       ; mkErrorMsg ctxt ct (msg $$ binds_doc) }
  where
    loc_msg tv 
       = case tcTyVarDetails tv of
          SkolemTv {} -> quotes (ppr tv) <+> skol_msg
          MetaTv {}   -> quotes (ppr tv) <+> text "is a free type variable"
          det -> pprTcTyVarDetails det
       where 
          skol_msg = pprSkol (getSkolemInfo (cec_encl ctxt) tv) (getSrcLoc tv)

mkHoleError _ ct = pprPanic "mkHoleError" (ppr ct)

----------------
mkIPErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIPErr ctxt cts
  = do { (ctxt, _, ambig_err) <- mkAmbigMsg ctxt cts
       ; (ctxt, bind_msg) <- relevantBindings ctxt ct1
       ; mkErrorMsg ctxt ct1 (msg $$ ambig_err $$ bind_msg) }
  where
    (ct1:_) = cts
    orig    = ctLocOrigin (cc_loc ct1)
    preds   = map ctPred cts
    givens  = getUserGivens ctxt
    msg | null givens
        = addArising orig $
          sep [ ptext (sLit "Unbound implicit parameter") <> plural cts
              , nest 2 (pprTheta preds) ] 
        | otherwise
        = couldNotDeduce givens (preds, orig)
\end{code}


%************************************************************************
%*									*
                Equality errors
%*									*
%************************************************************************

\begin{code}
mkEqErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
-- Don't have multiple equality errors from the same location
-- E.g.   (Int,Bool) ~ (Bool,Int)   one error will do!
mkEqErr ctxt (ct:_) = mkEqErr1 ctxt ct
mkEqErr _ [] = panic "mkEqErr"

mkEqErr1 :: ReportErrCtxt -> Ct -> TcM ErrMsg
-- Wanted constraints only!
mkEqErr1 ctxt ct
  = do { (ctxt, binds_msg) <- relevantBindings ctxt ct
       ; (ctxt, orig) <- zonkTidyOrigin ctxt orig
       ; if isGiven ev then 
           mkEqErr_help ctxt (inaccessible_msg orig $$ binds_msg) ct False ty1 ty2
         else
           mk_err binds_msg orig }
  where
    ev         = cc_ev ct
    orig       = ctLocOrigin (cc_loc ct)
    (ty1, ty2) = getEqPredTys (ctPred ct)

    inaccessible_msg orig = hang (ptext (sLit "Inaccessible code in"))
                               2 (ppr orig)

       -- If the types in the error message are the same as the types
       -- we are unifying, don't add the extra expected/actual message
    mk_err extra (TypeEqOrigin (UnifyOrigin { uo_actual = act, uo_expected = exp })) 
      | act `pickyEqType` ty1
      , exp `pickyEqType` ty2 = mkEqErr_help ctxt extra  ct True  ty2 ty1
      | exp `pickyEqType` ty1
      , act `pickyEqType` ty2 = mkEqErr_help ctxt extra  ct True  ty1 ty2
      | otherwise             = mkEqErr_help ctxt extra1 ct False ty1 ty2
      where
        extra1 = msg $$ extra
        msg    = mkExpectedActualMsg exp act
    mk_err extra _ = mkEqErr_help ctxt extra ct False ty1 ty2

mkEqErr_help, reportEqErr 
   :: ReportErrCtxt -> SDoc
   -> Ct
   -> Bool     -- True  <=> Types are correct way round;
               --           report "expected ty1, actual ty2"
               -- False <=> Just report a mismatch without orientation
               --           The ReportErrCtxt has expected/actual 
   -> TcType -> TcType -> TcM ErrMsg
mkEqErr_help ctxt extra ct oriented ty1 ty2
  | Just tv1 <- tcGetTyVar_maybe ty1 = mkTyVarEqErr ctxt extra ct oriented tv1 ty2
  | Just tv2 <- tcGetTyVar_maybe ty2 = mkTyVarEqErr ctxt extra ct oriented tv2 ty1
  | otherwise                        = reportEqErr  ctxt extra ct oriented ty1 ty2

reportEqErr ctxt extra1 ct oriented ty1 ty2
  = do { (ctxt', extra2) <- mkEqInfoMsg ctxt ct ty1 ty2
       ; mkErrorMsg ctxt' ct (vcat [ misMatchOrCND ctxt' ct oriented ty1 ty2
                                   , extra2, extra1]) }

mkTyVarEqErr :: ReportErrCtxt -> SDoc -> Ct -> Bool -> TcTyVar -> TcType -> TcM ErrMsg
-- tv1 and ty2 are already tidied
mkTyVarEqErr ctxt extra ct oriented tv1 ty2
  -- Occurs check
  | isNothing (occurCheckExpand tv1 ty2)
  = let occCheckMsg = hang (text "Occurs check: cannot construct the infinite type:") 2
                           (sep [ppr ty1, char '~', ppr ty2])
    in mkErrorMsg ctxt ct (occCheckMsg $$ extra)

  |  isSkolemTyVar tv1 	  -- ty2 won't be a meta-tyvar, or else the thing would
     		   	  -- be oriented the other way round; see TcCanonical.reOrient
  || isSigTyVar tv1 && not (isTyVarTy ty2)
  = mkErrorMsg ctxt ct (vcat [ misMatchOrCND ctxt ct oriented ty1 ty2
                             , extraTyVarInfo ctxt ty1 ty2
                             , extra ])

  -- So tv is a meta tyvar, and presumably it is
  -- an *untouchable* meta tyvar, else it'd have been unified
  | not (k2 `tcIsSubKind` k1)   	 -- Kind error
  = mkErrorMsg ctxt ct $ (kindErrorMsg (mkTyVarTy tv1) ty2 $$ extra)

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_skols = skols, ic_info = skol_info } <- implic
  , let esc_skols = filter (`elemVarSet` (tyVarsOfType ty2)) skols
  , not (null esc_skols)
  = do { let msg = misMatchMsg oriented ty1 ty2
             esc_doc = sep [ ptext (sLit "because type variable") <> plural esc_skols
                             <+> pprQuotedList esc_skols
                           , ptext (sLit "would escape") <+>
                             if isSingleton esc_skols then ptext (sLit "its scope")
                                                      else ptext (sLit "their scope") ]
             tv_extra = vcat [ nest 2 $ esc_doc
                             , sep [ (if isSingleton esc_skols 
                                      then ptext (sLit "This (rigid, skolem) type variable is")
                                      else ptext (sLit "These (rigid, skolem) type variables are"))
                               <+> ptext (sLit "bound by")
                             , nest 2 $ ppr skol_info
                             , nest 2 $ ptext (sLit "at") <+> ppr (tcl_loc env) ] ]
       ; mkErrorMsg ctxt ct (msg $$ tv_extra $$ extra) }

  -- Nastiest case: attempt to unify an untouchable variable
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_env = env, ic_given = given, ic_info = skol_info } <- implic
  = do { let msg = misMatchMsg oriented ty1 ty2
             untch_extra 
                = nest 2 $
                  sep [ quotes (ppr tv1) <+> ptext (sLit "is untouchable")
                      , nest 2 $ ptext (sLit "inside the constraints") <+> pprEvVarTheta given
                      , nest 2 $ ptext (sLit "bound by") <+> ppr skol_info
                      , nest 2 $ ptext (sLit "at") <+> ppr (tcl_loc env) ]
             tv_extra = extraTyVarInfo ctxt ty1 ty2
       ; mkErrorMsg ctxt ct (vcat [msg, untch_extra, tv_extra, extra]) }

  | otherwise
  = reportEqErr ctxt extra ct oriented (mkTyVarTy tv1) ty2
        -- This *can* happen (Trac #6123, and test T2627b)
        -- Consider an ambiguous top-level constraint (a ~ F a)
        -- Not an occurs check, becuase F is a type function.
  where         
    k1 	= tyVarKind tv1
    k2 	= typeKind ty2
    ty1 = mkTyVarTy tv1

mkEqInfoMsg :: ReportErrCtxt -> Ct -> TcType -> TcType -> TcM (ReportErrCtxt, SDoc)
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int    
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
mkEqInfoMsg ctxt ct ty1 ty2
  = do { (ctxt', _, ambig_msg) <- if isJust mb_fun1 || isJust mb_fun2
                                  then mkAmbigMsg ctxt [ct]
                                  else return (ctxt, False, empty)
       ; return (ctxt', tyfun_msg $$ ambig_msg) }
  where
    mb_fun1 = isTyFun_maybe ty1
    mb_fun2 = isTyFun_maybe ty2
    tyfun_msg | Just tc1 <- mb_fun1
              , Just tc2 <- mb_fun2
              , tc1 == tc2 
              = ptext (sLit "NB:") <+> quotes (ppr tc1) 
                <+> ptext (sLit "is a type function, and may not be injective")
              | otherwise = empty

misMatchOrCND :: ReportErrCtxt -> Ct -> Bool -> TcType -> TcType -> SDoc
-- If oriented then ty1 is expected, ty2 is actual
misMatchOrCND ctxt ct oriented ty1 ty2
  | null givens || 
    (isRigid ty1 && isRigid ty2) || 
    isGivenCt ct
       -- If the equality is unconditionally insoluble
       -- or there is no context, don't report the context
  = misMatchMsg oriented ty1 ty2
  | otherwise      
  = couldNotDeduce givens ([mkEqPred ty1 ty2], orig)
  where
    givens = getUserGivens ctxt
    orig   = TypeEqOrigin (UnifyOrigin ty1 ty2)

couldNotDeduce :: [UserGiven] -> (ThetaType, CtOrigin) -> SDoc
couldNotDeduce givens (wanteds, orig)
  = vcat [ addArising orig (ptext (sLit "Could not deduce") <+> pprTheta wanteds)
         , vcat (pp_givens givens)]

pp_givens :: [UserGiven] -> [SDoc]
pp_givens givens 
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (ptext (sLit "from the context")) g
                 : map (ppr_given (ptext (sLit "or from"))) gs
    where 
       ppr_given herald (gs, skol_info, loc)
           = hang (herald <+> pprEvVarTheta gs)
                2 (sep [ ptext (sLit "bound by") <+> ppr skol_info
                       , ptext (sLit "at") <+> ppr loc])

extraTyVarInfo :: ReportErrCtxt -> TcType -> TcType -> SDoc
-- Add on extra info about the types themselves
-- NB: The types themselves are already tidied
extraTyVarInfo ctxt ty1 ty2
  = nest 2 (extra1 $$ extra2)
  where
    extra1 = tyVarExtraInfoMsg (cec_encl ctxt) ty1
    extra2 = tyVarExtraInfoMsg (cec_encl ctxt) ty2

tyVarExtraInfoMsg :: [Implication] -> Type -> SDoc
-- Shows a bit of extra info about skolem constants
tyVarExtraInfoMsg implics ty
  | Just tv <- tcGetTyVar_maybe ty
  , isTcTyVar tv, isSkolemTyVar tv
  , let pp_tv = quotes (ppr tv)
 = case tcTyVarDetails tv of
    SkolemTv {}   -> pp_tv <+> pprSkol (getSkolemInfo implics tv) (getSrcLoc tv)
    FlatSkol {}   -> pp_tv <+> ptext (sLit "is a flattening type variable")
    RuntimeUnk {} -> pp_tv <+> ptext (sLit "is an interactive-debugger skolem")
    MetaTv {}     -> empty

 | otherwise             -- Normal case
 = empty
 
kindErrorMsg :: TcType -> TcType -> SDoc   -- Types are already tidy
kindErrorMsg ty1 ty2
  = vcat [ ptext (sLit "Kind incompatibility when matching types:")
         , nest 2 (vcat [ ppr ty1 <+> dcolon <+> ppr k1
                        , ppr ty2 <+> dcolon <+> ppr k2 ]) ]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

--------------------
misMatchMsg :: Bool -> TcType -> TcType -> SDoc	   -- Types are already tidy
-- If oriented then ty1 is expected, ty2 is actual
misMatchMsg oriented ty1 ty2 
  | oriented
  = sep [ ptext (sLit "Couldn't match expected") <+> what <+> quotes (ppr ty1)
        , nest 12 $   ptext (sLit "with actual") <+> what <+> quotes (ppr ty2) ]
  | otherwise
  = sep [ ptext (sLit "Couldn't match") <+> what <+> quotes (ppr ty1)
        , nest 14 $ ptext (sLit "with") <+> quotes (ppr ty2) ]
  where 
    what | isKind ty1 = ptext (sLit "kind")
         | otherwise  = ptext (sLit "type")

mkExpectedActualMsg :: Type -> Type -> SDoc
mkExpectedActualMsg exp_ty act_ty
  = vcat [ text "Expected type:" <+> ppr exp_ty
         , text "  Actual type:" <+> ppr act_ty ]
\end{code}

Note [Non-injective type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very confusing to get a message like
     Couldn't match expected type `Depend s'
            against inferred type `Depend s1'
so mkTyFunInfoMsg adds:
       NB: `Depend' is type function, and hence may not be injective

Warn of loopy local equalities that were dropped.


%************************************************************************
%*									*
                 Type-class errors
%*									*
%************************************************************************

\begin{code}
mkDictErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkDictErr ctxt cts 
  = ASSERT( not (null cts) )
    do { inst_envs <- tcGetInstEnvs
       ; lookups   <- mapM (lookup_cls_inst inst_envs) cts
       ; let (no_inst_cts, overlap_cts) = partition is_no_inst lookups

       -- Report definite no-instance errors, 
       -- or (iff there are none) overlap errors
       -- But we report only one of them (hence 'head') becuase they all
       -- have the same source-location origin, to try avoid a cascade
       -- of error from one location
       ; (ctxt, err) <- mk_dict_err ctxt (head (no_inst_cts ++ overlap_cts))
       ; mkErrorMsg ctxt ct1 err }
  where
    ct1:_ = cts
    no_givens = null (getUserGivens ctxt)
    is_no_inst (ct, (matches, unifiers, _))
      =  no_givens 
      && null matches 
      && (null unifiers || all (not . isAmbiguousTyVar) (varSetElems (tyVarsOfCt ct)))
           
    lookup_cls_inst inst_envs ct
      = do { tys_flat <- mapM quickFlattenTy tys
                -- Note [Flattening in error message generation]
           ; return (ct, lookupInstEnv inst_envs clas tys_flat) }
      where
        (clas, tys) = getClassPredTys (ctPred ct)

mk_dict_err :: ReportErrCtxt -> (Ct, ClsInstLookupResult)
            -> TcM (ReportErrCtxt, SDoc)
-- Report an overlap error if this class constraint results
-- from an overlap (returning Left clas), otherwise return (Right pred)
mk_dict_err ctxt (ct, (matches, unifiers, safe_haskell)) 
  | null matches  -- No matches but perhaps several unifiers
  = do { (ctxt, is_ambig, ambig_msg) <- mkAmbigMsg ctxt [ct]
       ; (ctxt, binds_msg) <- relevantBindings ctxt ct
       ; return (ctxt, cannot_resolve_msg is_ambig binds_msg ambig_msg) }

  | not safe_haskell   -- Some matches => overlap errors
  = return (ctxt, overlap_msg)

  | otherwise
  = return (ctxt, safe_haskell_msg)
  where
    orig        = ctLocOrigin (cc_loc ct)
    pred        = ctPred ct
    (clas, tys) = getClassPredTys pred
    ispecs      = [ispec | (ispec, _) <- matches]
    givens      = getUserGivens ctxt
    all_tyvars  = all isTyVarTy tys

    cannot_resolve_msg has_ambig_tvs binds_msg ambig_msg
      = vcat [ addArising orig (no_inst_herald <+> pprParendType pred)
             , vcat (pp_givens givens)
             , if has_ambig_tvs && (not (null unifiers) || not (null givens))
               then ambig_msg $$ binds_msg $$ potential_msg
               else empty
             , show_fixes (add_to_ctxt_fixes has_ambig_tvs ++ drv_fixes) ]

    potential_msg
      | null unifiers = empty
      | otherwise 
      = hang (if isSingleton unifiers 
              then ptext (sLit "Note: there is a potential instance available:")
              else ptext (sLit "Note: there are several potential instances:"))
    	   2 (ppr_insts unifiers)

    add_to_ctxt_fixes has_ambig_tvs
      | not has_ambig_tvs && all_tyvars
      , (orig:origs) <- mapCatMaybes get_good_orig (cec_encl ctxt)
      = [sep [ ptext (sLit "add") <+> pprParendType pred
               <+> ptext (sLit "to the context of")
	     , nest 2 $ ppr_skol orig $$ 
                        vcat [ ptext (sLit "or") <+> ppr_skol orig 
                             | orig <- origs ] ] ]
      | otherwise = []

    ppr_skol (PatSkol dc _) = ptext (sLit "the data constructor") <+> quotes (ppr dc)
    ppr_skol skol_info      = ppr skol_info

	-- Do not suggest adding constraints to an *inferred* type signature!
    get_good_orig ic = case ic_info ic of 
                         SigSkol (InfSigCtxt {}) _ -> Nothing
                         origin                    -> Just origin

    no_inst_herald
      | null givens && null matches = ptext (sLit "No instance for")
      | otherwise                   = ptext (sLit "Could not deduce")

    drv_fixes = case orig of
                   DerivOrigin -> [drv_fix]
                   _           -> []

    drv_fix = hang (ptext (sLit "use a standalone 'deriving instance' declaration,"))
                 2 (ptext (sLit "so you can specify the instance context yourself"))

    -- Normal overlap error
    overlap_msg
      = ASSERT( not (null matches) )
        vcat [	addArising orig (ptext (sLit "Overlapping instances for") 
				<+> pprType (mkClassPred clas tys))

             ,  if not (null matching_givens) then 
                  sep [ptext (sLit "Matching givens (or their superclasses):") 
                      , nest 2 (vcat matching_givens)]
                else empty

    	     ,	sep [ptext (sLit "Matching instances:"),
    		     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])]

             ,  if null matching_givens && isSingleton matches && null unifiers then
                -- Intuitively, some given matched the wanted in their
                -- flattened or rewritten (from given equalities) form
                -- but the matcher can't figure that out because the
                -- constraints are non-flat and non-rewritten so we
                -- simply report back the whole given
                -- context. Accelerate Smart.hs showed this problem.
                  sep [ ptext (sLit "There exists a (perhaps superclass) match:") 
                      , nest 2 (vcat (pp_givens givens))]
                else empty 

	     ,	if not (isSingleton matches)
    		then 	-- Two or more matches
		     empty
    		else 	-- One match
		parens (vcat [ptext (sLit "The choice depends on the instantiation of") <+>
	    		         quotes (pprWithCommas ppr (varSetElems (tyVarsOfTypes tys))),
			      if null (matching_givens) then
                                   vcat [ ptext (sLit "To pick the first instance above, use -XIncoherentInstances"),
			                  ptext (sLit "when compiling the other instance declarations")]
                              else empty])]
        where
            ispecs = [ispec | (ispec, _) <- matches]

            givens = getUserGivens ctxt
            matching_givens = mapCatMaybes matchable givens

            matchable (evvars,skol_info,loc) 
              = case ev_vars_matching of
                     [] -> Nothing
                     _  -> Just $ hang (pprTheta ev_vars_matching)
                                    2 (sep [ ptext (sLit "bound by") <+> ppr skol_info
                                           , ptext (sLit "at") <+> ppr loc])
                where ev_vars_matching = filter ev_var_matches (map evVarPred evvars)
                      ev_var_matches ty = case getClassPredTys_maybe ty of
                         Just (clas', tys')
                           | clas' == clas
                           , Just _ <- tcMatchTys (tyVarsOfTypes tys) tys tys'
                           -> True 
                           | otherwise
                           -> any ev_var_matches (immSuperClasses clas' tys')
                         Nothing -> False

    -- Overlap error because of Safe Haskell (first 
    -- match should be the most specific match)
    safe_haskell_msg
      = ASSERT( length matches > 1 )
        vcat [ addArising orig (ptext (sLit "Unsafe overlapping instances for") 
                        <+> pprType (mkClassPred clas tys))
             , sep [ptext (sLit "The matching instance is:"),
                    nest 2 (pprInstance $ head ispecs)]
             , vcat [ ptext $ sLit "It is compiled in a Safe module and as such can only"
                    , ptext $ sLit "overlap instances from the same module, however it"
                    , ptext $ sLit "overlaps the following instances from different modules:"
                    , nest 2 (vcat [pprInstances $ tail ispecs])
                    ]
             ]

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ ptext (sLit "Possible fix:")
                        , nest 2 (vcat (f : map (ptext (sLit "or") <+>) fs))]

ppr_insts :: [ClsInst] -> SDoc
ppr_insts insts
  = pprInstances (take 3 insts) $$ dot_dot_message
  where
    n_extra = length insts - 3
    dot_dot_message 
       | n_extra <= 0 = empty
       | otherwise    = ptext (sLit "...plus") 
                        <+> speakNOf n_extra (ptext (sLit "other"))

----------------------
quickFlattenTy :: TcType -> TcM TcType
-- See Note [Flattening in error message generation]
quickFlattenTy ty | Just ty' <- tcView ty = quickFlattenTy ty'
quickFlattenTy ty@(TyVarTy {})  = return ty
quickFlattenTy ty@(ForAllTy {}) = return ty     -- See
quickFlattenTy ty@(LitTy {})    = return ty
  -- Don't flatten because of the danger or removing a bound variable
quickFlattenTy (AppTy ty1 ty2) = do { fy1 <- quickFlattenTy ty1
                                    ; fy2 <- quickFlattenTy ty2
                                    ; return (AppTy fy1 fy2) }
quickFlattenTy (FunTy ty1 ty2) = do { fy1 <- quickFlattenTy ty1
                                    ; fy2 <- quickFlattenTy ty2
                                    ; return (FunTy fy1 fy2) }
quickFlattenTy (TyConApp tc tys)
    | not (isSynFamilyTyCon tc)
    = do { fys <- mapM quickFlattenTy tys 
         ; return (TyConApp tc fys) }
    | otherwise
    = do { let (funtys,resttys) = splitAt (tyConArity tc) tys
                -- Ignore the arguments of the type family funtys
         ; v <- newMetaTyVar TauTv (typeKind (TyConApp tc funtys))
         ; flat_resttys <- mapM quickFlattenTy resttys
         ; return (foldl AppTy (mkTyVarTy v) flat_resttys) }
\end{code}

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

Note [Quick-flatten polytypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see C (Ix a => blah) or C (forall a. blah) we simply refrain from
flattening any further.  After all, there can be no instance declarations
that match such things.  And flattening under a for-all is problematic
anyway; consider C (forall a. F a)

\begin{code}
mkAmbigMsg :: ReportErrCtxt -> [Ct] 
           -> TcM (ReportErrCtxt, Bool, SDoc)
mkAmbigMsg ctxt cts
  | isEmptyVarSet ambig_tv_set
  = return (ctxt, False, empty)
  | otherwise
  = do { dflags <- getDynFlags
       
       ; prs <- mapSndM zonkTcType $ 
                [ (id, idType id) | TcIdBndr id top_lvl <- ct1_bndrs
                                  , isTopLevel top_lvl ]
       ; let ambig_ids = [id | (id, zonked_ty) <- prs
                             , tyVarsOfType zonked_ty `intersectsVarSet` ambig_tv_set]
       ; return (ctxt, True, mk_msg dflags ambig_ids) }
  where
    ct1:_ = cts
    ct1_bndrs = tcl_bndrs (ctLocEnv (cc_loc ct1))
 
    ambig_tv_set = foldr (unionVarSet . filterVarSet isAmbiguousTyVar . tyVarsOfCt) 
                         emptyVarSet cts
    ambig_tvs = varSetElems ambig_tv_set
    
    is_or_are | isSingleton ambig_tvs = text "is"
              | otherwise             = text "are"
                 
    mk_msg dflags ambig_ids
      | any isRuntimeUnkSkol ambig_tvs  -- See Note [Runtime skolems]
      =  vcat [ ptext (sLit "Cannot resolve unknown runtime type") <> plural ambig_tvs
                   <+> pprQuotedList ambig_tvs
              , ptext (sLit "Use :print or :force to determine these types")]
      | otherwise
      = vcat [ text "The type variable" <> plural ambig_tvs
	          <+> pprQuotedList ambig_tvs
                  <+> is_or_are <+> text "ambiguous"
             , mk_extra_msg dflags ambig_ids ]
  
    mk_extra_msg dflags ambig_ids
      | null ambig_ids
      = ptext (sLit "Possible fix: add a type signature that fixes these type variable(s)")
			-- This happens in things like
			--	f x = show (read "foo")
			-- where monomorphism doesn't play any role
      | otherwise 
      = vcat [ hang (ptext (sLit "Possible cause: the monomorphism restriction applied to:"))
	          2 (pprWithCommas (quotes . ppr) ambig_ids)
             , ptext (sLit "Probable fix:") <+> vcat
     	          [ ptext (sLit "give these definition(s) an explicit type signature")
     	          , if xopt Opt_MonomorphismRestriction dflags
                    then ptext (sLit "or use -XNoMonomorphismRestriction")
                    else empty ]    -- Only suggest adding "-XNoMonomorphismRestriction"
     			            -- if it is not already set!
             ]


pprSkol :: SkolemInfo -> SrcLoc -> SDoc
pprSkol UnkSkol   _ 
  = ptext (sLit "is an unknown type variable")
pprSkol skol_info tv_loc 
  = sep [ ptext (sLit "is a rigid type variable bound by"),
          sep [ppr skol_info, ptext (sLit "at") <+> ppr tv_loc]]

getSkolemInfo :: [Implication] -> TcTyVar -> SkolemInfo
-- Get the skolem info for a type variable 
-- from the implication constraint that binds it
getSkolemInfo [] tv
  = pprPanic "No skolem info:" (ppr tv)

getSkolemInfo (implic:implics) tv
  | tv `elem` ic_skols implic = ic_info implic
  | otherwise                 = getSkolemInfo implics tv

-----------------------
-- relevantBindings looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.

relevantBindings :: ReportErrCtxt -> Ct
                 -> TcM (ReportErrCtxt, SDoc)
relevantBindings ctxt ct
  = do { (tidy_env', docs) <- go (cec_tidy ctxt) (6, emptyVarSet) 
                                 (reverse (tcl_bndrs lcl_env))
         -- The 'reverse' makes us work from outside in
         -- Blargh; maybe have a flag for this "6"

       ; traceTc "relevantBindings" (ppr [id | TcIdBndr id _ <- tcl_bndrs lcl_env])
       ; let doc = hang (ptext (sLit "Relevant bindings include")) 
                      2 (vcat docs)
       ; if null docs 
         then return (ctxt, empty)
         else do { traceTc "rb" doc
                 ; return (ctxt { cec_tidy = tidy_env' }, doc) } } 
  where
    lcl_env = ctLocEnv (cc_loc ct)
    ct_tvs = tyVarsOfCt ct

    go :: TidyEnv -> (Int, TcTyVarSet)
       -> [TcIdBinder] -> TcM (TidyEnv, [SDoc])
    go tidy_env (_,_) []
       = return (tidy_env, [])
    go tidy_env (n_left,tvs_seen) (TcIdBndr id _ : tc_bndrs)
       | n_left <= 0, ct_tvs `subVarSet` tvs_seen
       =   -- We have run out of n_left, and we
           -- already have bindings mentioning all of ct_tvs
         go tidy_env (n_left,tvs_seen) tc_bndrs
       | otherwise
       = do { (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env (idType id)
            ; let id_tvs = tyVarsOfType tidy_ty
                  doc = sep [ ppr id <+> dcolon <+> ppr tidy_ty
		            , nest 2 (parens (ptext (sLit "bound at")
			    	 <+> ppr (getSrcLoc id)))]
            ; if id_tvs `intersectsVarSet` ct_tvs 
              && (n_left > 0 || not (id_tvs `subVarSet` tvs_seen))
                       -- Either we n_left is big enough, 
                       -- or this binding mentions a new type variable
              then do { (env', docs) <- go tidy_env' (n_left - 1, tvs_seen `unionVarSet` id_tvs) tc_bndrs
                      ; return (env', doc:docs) }
	      else go tidy_env (n_left, tvs_seen) tc_bndrs }

-----------------------
warnDefaulting :: Cts -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyVars env0 $
                        tyVarsOfCts wanteds
             tidy_wanteds = mapBag (tidyCt tidy_env) wanteds
             (loc, ppr_wanteds) = pprWithArising (bagToList tidy_wanteds)
             warn_msg  = hang (ptext (sLit "Defaulting the following constraint(s) to type")
                                <+> quotes (ppr default_ty))
                            2 ppr_wanteds
       ; setCtLoc loc $ warnTc warn_default warn_msg }
\end{code}

Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in RtClosureInspect.zonkRTTIType.  

%************************************************************************
%*									*
                 Error from the canonicaliser
	 These ones are called *during* constraint simplification
%*									*
%************************************************************************

\begin{code}
solverDepthErrorTcS :: Ct -> TcM a
solverDepthErrorTcS ct
  = setCtLoc loc $
    do { ct <- zonkCt ct
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyVars env0 (tyVarsOfCt ct)
             tidy_ct  = tidyCt tidy_env ct
       ; failWithTcM (tidy_env, hang msg 2 (ppr tidy_ct)) }
  where
    loc   = cc_loc ct
    depth = ctLocDepth loc
    msg = vcat [ ptext (sLit "Context reduction stack overflow; size =") <+> int depth
               , ptext (sLit "Use -fcontext-stack=N to increase stack size to N") ]
\end{code}

%************************************************************************
%*									*
                 Tidying
%*									*
%************************************************************************

\begin{code}
zonkTidyTcType :: TidyEnv -> TcType -> TcM (TidyEnv, TcType)
zonkTidyTcType env ty = do { ty' <- zonkTcType ty
                           ; return (tidyOpenType env ty') }

zonkTidyOrigin :: ReportErrCtxt -> CtOrigin -> TcM (ReportErrCtxt, CtOrigin)
zonkTidyOrigin ctxt (GivenOrigin skol_info)
  = do { skol_info1 <- zonkSkolemInfo skol_info
       ; let (env1, skol_info2) = tidySkolemInfo (cec_tidy ctxt) skol_info1
       ; return (ctxt { cec_tidy = env1 }, GivenOrigin skol_info2) }
zonkTidyOrigin ctxt (TypeEqOrigin (UnifyOrigin { uo_actual = act, uo_expected = exp }))
  = do { (env1, act') <- zonkTidyTcType (cec_tidy ctxt) act
       ; (env2, exp') <- zonkTidyTcType env1            exp
       ; return ( ctxt { cec_tidy = env2 }
                , TypeEqOrigin (UnifyOrigin { uo_actual = act', uo_expected = exp' })) }
zonkTidyOrigin ctxt orig = return (ctxt, orig)
\end{code}
