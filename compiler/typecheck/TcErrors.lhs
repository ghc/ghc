\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcErrors( 
       reportUnsolved,
       warnDefaulting,
       unifyCtxt,

       flattenForAllErrorTcS,
       solverDepthErrorTcS
  ) where

#include "HsVersions.h"

import TcRnMonad
import TcMType
import TcSMonad
import TcType
import TypeRep
import Type
import Class
import Unify ( tcMatchTys )
import Inst
import InstEnv
import TyCon
import Name
import NameEnv
import Id	( idType )
import Var
import VarSet
import VarEnv
import SrcLoc
import Bag
import BasicTypes ( IPName )
import ListSetOps( equivClasses )
import Maybes( mapCatMaybes )
import Util
import FastString
import Outputable
import DynFlags
import Data.List( partition )
import Control.Monad( when, unless, filterM )
\end{code}

%************************************************************************
%*									*
\section{Errors and contexts}
%*									*
%************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

\begin{code}
reportUnsolved :: WantedConstraints -> TcM ()
reportUnsolved wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do {   -- Zonk to un-flatten any flatten-skols
       ; wanted  <- zonkWC wanted

       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyVars env0 free_tvs
             free_tvs = tyVarsOfWC wanted
             err_ctxt = CEC { cec_encl  = []
                            , cec_insol = insolubleWC wanted
                            , cec_extra = empty
                            , cec_tidy  = tidy_env }
             tidy_wanted = tidyWC tidy_env wanted

       ; traceTc "reportUnsolved" (ppr tidy_wanted)

       ; reportTidyWanteds err_ctxt tidy_wanted }

--------------------------------------------
--      Internal functions
--------------------------------------------

data ReportErrCtxt 
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                	       	       --   (innermost first)
          , cec_tidy  :: TidyEnv
          , cec_extra :: SDoc       -- Add this to each error message
          , cec_insol :: Bool       -- True <=> we are reporting insoluble errors only
                                    --      Main effect: don't say "Cannot deduce..."
                                    --      when reporting equality errors; see misMatchOrCND
      }

reportTidyImplic :: ReportErrCtxt -> Implication -> TcM ()
reportTidyImplic ctxt implic
  | BracketSkol <- ctLocOrigin (ic_loc implic)
  , not insoluble  -- For Template Haskell brackets report only
  = return ()      -- definite errors. The whole thing will be re-checked
                   -- later when we plug it in, and meanwhile there may
                   -- certainly be un-satisfied constraints

  | otherwise
  = reportTidyWanteds ctxt' (ic_wanted implic)
  where
    insoluble = ic_insol implic
    ctxt' = ctxt { cec_encl = implic : cec_encl ctxt
                 , cec_insol = insoluble }

reportTidyWanteds :: ReportErrCtxt -> WantedConstraints -> TcM ()
reportTidyWanteds ctxt (WC { wc_flat = flats, wc_insol = insols, wc_impl = implics })
  | cec_insol ctxt     -- If there are any insolubles, report only them
                       -- because they are unconditionally wrong
                       -- Moreover, if any of the insolubles are givens, stop right there
                       -- ignoring nested errors, because the code is inaccessible
  = do { let (given, other) = partitionBag (isGivenOrSolved . cc_flavor) insols
             insol_implics  = filterBag ic_insol implics
       ; if isEmptyBag given
         then do { mapBagM_ (reportInsoluble ctxt) other
                 ; mapBagM_ (reportTidyImplic ctxt) insol_implics }
         else mapBagM_ (reportInsoluble ctxt) given }

  | otherwise          -- No insoluble ones
  = ASSERT( isEmptyBag insols )
    do { let flat_evs = bagToList $ mapBag to_wev flats
             to_wev ct | Wanted wl <- cc_flavor ct = mkEvVarX (cc_id ct) wl
                       | otherwise = panic "reportTidyWanteds: unsolved is not wanted!"
             (ambigs, non_ambigs) = partition     is_ambiguous flat_evs
       	     (tv_eqs, others)     = partitionWith is_tv_eq     non_ambigs

       ; groupErrs (reportEqErrs ctxt) tv_eqs
       ; when (null tv_eqs) $ groupErrs (reportFlat ctxt) others
       ; mapBagM_ (reportTidyImplic ctxt) implics

       	   -- Only report ambiguity if no other errors (at all) happened
	   -- See Note [Avoiding spurious errors] in TcSimplify
       ; ifErrsM (return ()) $ reportAmbigErrs ctxt ambigs }
  where
	-- Report equalities of form (a~ty) first.  They are usually
	-- skolem-equalities, and they cause confusing knock-on 
	-- effects in other errors; see test T4093b.
    is_tv_eq c | Just (ty1, ty2) <- getEqPredTys_maybe (evVarOfPred c)
               , tcIsTyVarTy ty1 || tcIsTyVarTy ty2
               = Left (c, (ty1, ty2))
               | otherwise
               = Right (c, evVarOfPred c)

	-- Treat it as "ambiguous" if 
	--   (a) it is a class constraint
        --   (b) it constrains only type variables
	--       (else we'd prefer to report it as "no instance for...")
        --   (c) it mentions a (presumably un-filled-in) meta type variable
    is_ambiguous d = isTyVarClassPred pred
                  && any isAmbiguousTyVar (varSetElems (tyVarsOfType pred))
		  where   
                     pred = evVarOfPred d

reportInsoluble :: ReportErrCtxt -> Ct -> TcM ()
-- Precondition: insolubles are always NonCanonicals! 
reportInsoluble ctxt ct
  | ev <- cc_id ct
  , flav <- cc_flavor ct 
  , Just (ty1, ty2) <- getEqPredTys_maybe (evVarPred ev)
  = setCtFlavorLoc flav $
    do { let ctxt2 = ctxt { cec_extra = cec_extra ctxt $$ inaccessible_msg }
       ; reportEqErr ctxt2 ty1 ty2 }
  | otherwise
  = pprPanic "reportInsoluble" (pprEvVarWithType (cc_id ct))
  where
    inaccessible_msg | Given loc GivenOrig <- (cc_flavor ct)
                       -- If a GivenSolved then we should not report inaccessible code
                     = hang (ptext (sLit "Inaccessible code in"))
                          2 (ppr (ctLocOrigin loc))
                     | otherwise = empty

reportFlat :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()
-- The [PredType] are already tidied
reportFlat ctxt flats origin
  = do { unless (null dicts)  $ reportDictErrs   ctxt dicts  origin
       ; unless (null eqs)    $ reportEqErrs     ctxt eqs    origin
       ; unless (null ips)    $ reportIPErrs     ctxt ips    origin
       ; unless (null irreds) $ reportIrredsErrs ctxt irreds origin }
  where
    (dicts, eqs, ips, irreds) = go_many (map classifyPredType flats)

    go_many []     = ([], [], [], [])
    go_many (t:ts) = (as ++ as', bs ++ bs', cs ++ cs', ds ++ ds')
      where (as, bs, cs, ds) = go t
            (as', bs', cs', ds') = go_many ts

    go (ClassPred cls tys) = ([(cls, tys)], [], [], [])
    go (EqPred ty1 ty2)    = ([], [(ty1, ty2)], [], [])
    go (IPPred ip ty)      = ([], [], [(ip, ty)], [])
    go (IrredPred ty)      = ([], [], [], [ty])
    go (TuplePred {})      = panic "reportFlat"
    -- TuplePreds should have been expanded away by the constraint
    -- simplifier, so they shouldn't show up at this point

--------------------------------------------
--      Support code 
--------------------------------------------

groupErrs :: ([a] -> CtOrigin -> TcM ()) -- Deal with one group
	  -> [(WantedEvVar, a)]	                -- Unsolved wanteds
          -> TcM ()
-- Group together insts with the same origin
-- We want to report them together in error messages

groupErrs _ [] 
  = return ()
groupErrs report_err ((wanted, x) : wanteds)
  = do  { setCtLoc the_loc $
          report_err the_xs (ctLocOrigin the_loc)
	; groupErrs report_err others }
  where
   the_loc           = evVarX wanted
   the_key	     = mk_key the_loc
   the_xs            = x:map snd friends
   (friends, others) = partition (is_friend . fst) wanteds
   is_friend friend  = mk_key (evVarX friend) `same_key` the_key

   mk_key :: WantedLoc -> (SrcSpan, CtOrigin)
   mk_key loc = (ctLocSpan loc, ctLocOrigin loc)

   same_key (s1, o1) (s2, o2) = s1==s2 && o1 `same_orig` o2
   same_orig (OccurrenceOf n1) (OccurrenceOf n2) = n1==n2
   same_orig ScOrigin          ScOrigin          = True
   same_orig DerivOrigin       DerivOrigin       = True
   same_orig DefaultOrigin     DefaultOrigin     = True
   same_orig _ _ = False


-- Add the "arising from..." part to a message about bunch of dicts
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = msg $$ nest 2 (pprArising orig)

pprWithArising :: [WantedEvVar] -> (WantedLoc, SDoc)
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
pprWithArising [] 
  = panic "pprWithArising"
pprWithArising [EvVarX ev loc]
  = (loc, hang (pprEvVarTheta [ev]) 2 (pprArising (ctLocOrigin loc)))
pprWithArising ev_vars
  = (first_loc, vcat (map ppr_one ev_vars))
  where
    first_loc = evVarX (head ev_vars)
    ppr_one (EvVarX v loc)
       = hang (parens (pprType (evVarPred v))) 2 (pprArisingAt loc)

addErrorReport :: ReportErrCtxt -> SDoc -> TcM ()
addErrorReport ctxt msg = addErrTcM (cec_tidy ctxt, msg $$ cec_extra ctxt)

getUserGivens :: ReportErrCtxt -> [([EvVar], GivenLoc)]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = ctxt})
  = reverse $
    [ (givens, loc) | Implic {ic_given = givens, ic_loc = loc} <- ctxt
                    , not (null givens) ]
\end{code}

%************************************************************************
%*                  *
                Irreducible predicate errors
%*                  *
%************************************************************************

\begin{code}
reportIrredsErrs :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()
reportIrredsErrs ctxt irreds orig
  = addErrorReport ctxt msg
  where
    givens = getUserGivens ctxt
    msg = couldNotDeduce givens (irreds, orig)
\end{code}


%************************************************************************
%*									*
                Implicit parameter errors
%*									*
%************************************************************************

\begin{code}
reportIPErrs :: ReportErrCtxt -> [(IPName Name, Type)] -> CtOrigin -> TcM ()
reportIPErrs ctxt ips orig
  = addErrorReport ctxt msg
  where
    givens = getUserGivens ctxt
    msg | null givens
        = addArising orig $
          sep [ ptext (sLit "Unbound implicit parameter") <> plural ips
              , nest 2 (pprTheta (map (uncurry mkIPPred) ips)) ] 
        | otherwise
        = couldNotDeduce givens (map (uncurry mkIPPred) ips, orig)
\end{code}


%************************************************************************
%*									*
                Equality errors
%*									*
%************************************************************************

\begin{code}
reportEqErrs :: ReportErrCtxt -> [(Type, Type)] -> CtOrigin -> TcM ()
-- The [PredType] are already tidied
reportEqErrs ctxt eqs orig
  = do { orig' <- zonkTidyOrigin ctxt orig
       ; mapM_ (report_one orig') eqs }
  where
    report_one orig (ty1, ty2)
      = do { let extra = getWantedEqExtra orig ty1 ty2
                 ctxt' = ctxt { cec_extra = extra $$ cec_extra ctxt }
           ; reportEqErr ctxt' ty1 ty2 }

getWantedEqExtra ::  CtOrigin -> TcType -> TcType -> SDoc
getWantedEqExtra (TypeEqOrigin (UnifyOrigin { uo_actual = act, uo_expected = exp }))
                 ty1 ty2
  -- If the types in the error message are the same as the types we are unifying,
  -- don't add the extra expected/actual message
  | act `eqType` ty1 && exp `eqType` ty2 = empty
  | exp `eqType` ty1 && act `eqType` ty2 = empty
  | otherwise                            = mkExpectedActualMsg act exp

getWantedEqExtra orig _ _ = pprArising orig

reportEqErr :: ReportErrCtxt -> TcType -> TcType -> TcM ()
-- ty1 and ty2 are already tidied
reportEqErr ctxt ty1 ty2
  | Just tv1 <- tcGetTyVar_maybe ty1 = reportTyVarEqErr ctxt tv1 ty2
  | Just tv2 <- tcGetTyVar_maybe ty2 = reportTyVarEqErr ctxt tv2 ty1

  | otherwise	-- Neither side is a type variable
    		-- Since the unsolved constraint is canonical, 
		-- it must therefore be of form (F tys ~ ty)
  = addErrorReport ctxt (misMatchOrCND ctxt ty1 ty2 $$ mkTyFunInfoMsg ty1 ty2)


reportTyVarEqErr :: ReportErrCtxt -> TcTyVar -> TcType -> TcM ()
-- tv1 and ty2 are already tidied
reportTyVarEqErr ctxt tv1 ty2
  |  isSkolemTyVar tv1 	  -- ty2 won't be a meta-tyvar, or else the thing would
     		   	  -- be oriented the other way round; see TcCanonical.reOrient
  || isSigTyVar tv1 && not (isTyVarTy ty2)
  = addErrorReport (addExtraInfo ctxt ty1 ty2)
                   (misMatchOrCND ctxt ty1 ty2)

  -- So tv is a meta tyvar, and presumably it is
  -- an *untouchable* meta tyvar, else it'd have been unified
  | not (k2 `isSubKind` k1)   	 -- Kind error
  = addErrorReport ctxt $ (kindErrorMsg (mkTyVarTy tv1) ty2)

  -- Occurs check
  | tv1 `elemVarSet` tyVarsOfType ty2
  = let occCheckMsg = hang (text "Occurs check: cannot construct the infinite type:") 2
                           (sep [ppr ty1, char '=', ppr ty2])
    in addErrorReport ctxt occCheckMsg

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , let esc_skols = varSetElems (tyVarsOfType ty2 `intersectVarSet` ic_skols implic)
        implic_loc = ic_loc implic
  , not (null esc_skols)
  = setCtLoc implic_loc $	-- Override the error message location from the
    	     			-- place the equality arose to the implication site
    do { (env1, env_sigs) <- findGlobals ctxt (unitVarSet tv1)
       ; let msg = misMatchMsg ty1 ty2
             esc_doc = sep [ ptext (sLit "because type variable") <> plural esc_skols
                             <+> pprQuotedList esc_skols
                           , ptext (sLit "would escape") <+>
                             if isSingleton esc_skols then ptext (sLit "its scope")
                                                      else ptext (sLit "their scope") ]
             extra1 = vcat [ nest 2 $ esc_doc
                           , sep [ (if isSingleton esc_skols 
                                    then ptext (sLit "This (rigid, skolem) type variable is")
                                    else ptext (sLit "These (rigid, skolem) type variables are"))
                                   <+> ptext (sLit "bound by")
                                 , nest 2 $ ppr (ctLocOrigin implic_loc) ] ]
       ; addErrTcM (env1, msg $$ extra1 $$ mkEnvSigMsg (ppr tv1) env_sigs) }

  -- Nastiest case: attempt to unify an untouchable variable
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , let implic_loc = ic_loc implic
        given      = ic_given implic
  = setCtLoc (ic_loc implic) $
    do { let msg = misMatchMsg ty1 ty2
             extra = quotes (ppr tv1)
                 <+> sep [ ptext (sLit "is untouchable")
                         , ptext (sLit "inside the constraints") <+> pprEvVarTheta given
                         , ptext (sLit "bound at") <+> ppr (ctLocOrigin implic_loc)]
       ; addErrorReport (addExtraInfo ctxt ty1 ty2) (msg $$ nest 2 extra) }

  | otherwise
  = pprTrace "reportTyVarEqErr" (ppr tv1 $$ ppr ty2 $$ ppr (cec_encl ctxt)) $
    return () 
    	-- I don't think this should happen, and if it does I want to know
	-- Trac #5130 happened because an actual type error was not
	-- reported at all!  So not reporting is pretty dangerous.
	-- 
	-- OLD, OUT OF DATE COMMENT
        -- This can happen, by a recursive decomposition of frozen
        -- occurs check constraints
        -- Example: alpha ~ T Int alpha has frozen.
        --          Then alpha gets unified to T beta gamma
        -- So now we have  T beta gamma ~ T Int (T beta gamma)
        -- Decompose to (beta ~ Int, gamma ~ T beta gamma)
        -- The (gamma ~ T beta gamma) is the occurs check, but
        -- the (beta ~ Int) isn't an error at all.  So return ()
  where         
    k1 	= tyVarKind tv1
    k2 	= typeKind ty2
    ty1 = mkTyVarTy tv1

mkTyFunInfoMsg :: TcType -> TcType -> SDoc
-- See Note [Non-injective type functions]
mkTyFunInfoMsg ty1 ty2
  | Just (tc1,_) <- tcSplitTyConApp_maybe ty1
  , Just (tc2,_) <- tcSplitTyConApp_maybe ty2
  , tc1 == tc2, isSynFamilyTyCon tc1
  = ptext (sLit "NB:") <+> quotes (ppr tc1) 
    <+> ptext (sLit "is a type function") <> (pp_inj tc1)
  | otherwise = empty
  where       
    pp_inj tc | isInjectiveTyCon tc = empty
              | otherwise = ptext (sLit (", and may not be injective"))

misMatchOrCND :: ReportErrCtxt -> TcType -> TcType -> SDoc
misMatchOrCND ctxt ty1 ty2
  | cec_insol ctxt = misMatchMsg ty1 ty2    -- If the equality is unconditionally
                                            -- insoluble, don't report the context
  | null givens    = misMatchMsg ty1 ty2
  | otherwise      = couldNotDeduce givens ([mkEqPred (ty1, ty2)], orig)
  where
    givens = getUserGivens ctxt
    orig   = TypeEqOrigin (UnifyOrigin ty1 ty2)

couldNotDeduce :: [([EvVar], GivenLoc)] -> (ThetaType, CtOrigin) -> SDoc
couldNotDeduce givens (wanteds, orig)
  = vcat [ hang (ptext (sLit "Could not deduce") <+> pprTheta wanteds)
              2 (pprArising orig)
         , vcat (pp_givens givens)]

pp_givens :: [([EvVar], GivenLoc)] -> [SDoc]
pp_givens givens 
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (ptext (sLit "from the context")) g
                 : map (ppr_given (ptext (sLit "or from"))) gs
    where ppr_given herald (gs,loc)
           = hang (herald <+> pprEvVarTheta gs)
                2 (sep [ ptext (sLit "bound by") <+> ppr (ctLocOrigin loc)
                       , ptext (sLit "at") <+> ppr (ctLocSpan loc)])

addExtraInfo :: ReportErrCtxt -> TcType -> TcType -> ReportErrCtxt
-- Add on extra info about the types themselves
-- NB: The types themselves are already tidied
addExtraInfo ctxt ty1 ty2
  = ctxt { cec_extra = nest 2 (extra1 $$ extra2) $$ cec_extra ctxt }
  where
    extra1 = typeExtraInfoMsg (cec_encl ctxt) ty1
    extra2 = typeExtraInfoMsg (cec_encl ctxt) ty2

misMatchMsg :: TcType -> TcType -> SDoc	   -- Types are already tidy
misMatchMsg ty1 ty2 = sep [ ptext (sLit "Couldn't match type") <+> quotes (ppr ty1)
	                  , nest 15 $ ptext (sLit "with") <+> quotes (ppr ty2)]

kindErrorMsg :: TcType -> TcType -> SDoc   -- Types are already tidy
kindErrorMsg ty1 ty2
  = vcat [ ptext (sLit "Kind incompatibility when matching types:")
         , nest 2 (vcat [ ppr ty1 <+> dcolon <+> ppr k1
                        , ppr ty2 <+> dcolon <+> ppr k2 ]) ]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

typeExtraInfoMsg :: [Implication] -> Type -> SDoc
-- Shows a bit of extra info about skolem constants
typeExtraInfoMsg implics ty
  | Just tv <- tcGetTyVar_maybe ty
  , isTcTyVar tv, isSkolemTyVar tv
  , let pp_tv = quotes (ppr tv)
 = case tcTyVarDetails tv of
    SkolemTv {}   -> pp_tv <+> ppr_skol (getSkolemInfo implics tv) (getSrcLoc tv)
    FlatSkol {}   -> pp_tv <+> ptext (sLit "is a flattening type variable")
    RuntimeUnk {} -> pp_tv <+> ptext (sLit "is an interactive-debugger skolem")
    MetaTv {}     -> empty

 | otherwise             -- Normal case
 = empty

 where
   ppr_skol UnkSkol _   = ptext (sLit "is an unknown type variable")  -- Unhelpful
   ppr_skol info    loc = sep [ptext (sLit "is a rigid type variable bound by"),
                               sep [ppr info, ptext (sLit "at") <+> ppr loc]]
 
--------------------
unifyCtxt :: EqOrigin -> TidyEnv -> TcM (TidyEnv, SDoc)
unifyCtxt (UnifyOrigin { uo_actual = act_ty, uo_expected = exp_ty }) tidy_env
  = do  { (env1, act_ty') <- zonkTidyTcType tidy_env act_ty
        ; (env2, exp_ty') <- zonkTidyTcType env1 exp_ty
        ; return (env2, mkExpectedActualMsg act_ty' exp_ty') }

mkExpectedActualMsg :: Type -> Type -> SDoc
mkExpectedActualMsg act_ty exp_ty
  = vcat [ text "Expected type" <> colon <+> ppr exp_ty
         , text "  Actual type" <> colon <+> ppr act_ty ]
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
reportDictErrs :: ReportErrCtxt -> [(Class, [Type])] -> CtOrigin -> TcM ()	
reportDictErrs ctxt wanteds orig
  = do { inst_envs <- tcGetInstEnvs
       ; non_overlaps <- filterM (reportOverlap ctxt inst_envs orig) wanteds
       ; unless (null non_overlaps) $
         addErrorReport ctxt (mk_no_inst_err non_overlaps) }
  where
    mk_no_inst_err :: [(Class, [Type])] -> SDoc
    mk_no_inst_err wanteds
      | null givens     -- Top level
      = vcat [ addArising orig $
               ptext (sLit "No instance") <> plural min_wanteds
                    <+> ptext (sLit "for") <+> pprTheta min_wanteds
             , show_fixes (fixes2 ++ fixes3) ]

      | otherwise
      = vcat [ couldNotDeduce givens (min_wanteds, orig)
             , show_fixes (fixes1 ++ fixes2 ++ fixes3) ]
      where
        givens = getUserGivens ctxt
        min_wanteds = mkMinimalBySCs (map (uncurry mkClassPred) wanteds)

        fixes2 = case instance_dicts of
                   []  -> []
                   [_] -> [sep [ptext (sLit "add an instance declaration for"),
                                pprTheta instance_dicts]]
                   _   -> [sep [ptext (sLit "add instance declarations for"),
                                pprTheta instance_dicts]]
        fixes3 = case orig of
                   DerivOrigin -> [drv_fix]
                   _           -> []

        instance_dicts = filterOut isTyVarClassPred min_wanteds
		-- Insts for which it is worth suggesting an adding an 
		-- instance declaration.  Exclude tyvar dicts.

        drv_fix = vcat [ptext (sLit "use a standalone 'deriving instance' declaration,"),
                        nest 2 $ ptext (sLit "so you can specify the instance context yourself")]

	show_fixes :: [SDoc] -> SDoc
	show_fixes []     = empty
	show_fixes (f:fs) = sep [ptext (sLit "Possible fix:"), 
				 nest 2 (vcat (f : map (ptext (sLit "or") <+>) fs))]

        fixes1 | (orig:origs) <- mapCatMaybes get_good_orig (cec_encl ctxt)
               = [sep [ ptext (sLit "add") <+> pprTheta min_wanteds
                        <+> ptext (sLit "to the context of")
	              , nest 2 $ ppr_skol orig $$ 
                                 vcat [ ptext (sLit "or") <+> ppr_skol orig 
                                      | orig <- origs ]
                 ]    ]
               | otherwise = []

        ppr_skol (PatSkol dc _) = ptext (sLit "the data constructor") <+> quotes (ppr dc)
        ppr_skol skol_info      = ppr skol_info

	-- Do not suggest adding constraints to an *inferred* type signature!
        get_good_orig ic = case ctLocOrigin (ic_loc ic) of 
                             SigSkol (InfSigCtxt {}) _ -> Nothing
                             origin                    -> Just origin

reportOverlap :: ReportErrCtxt -> (InstEnv,InstEnv) -> CtOrigin
              -> (Class, [Type]) -> TcM Bool
-- Report an overlap error if this class constraint results
-- from an overlap (returning Nothing), otherwise return (Just pred)
reportOverlap ctxt inst_envs orig (clas, tys)
  = do { tys_flat <- mapM quickFlattenTy tys
           -- Note [Flattening in error message generation]

       ; case lookupInstEnv inst_envs clas tys_flat of
                ([], _, _) -> return True            -- No match
                res        -> do { addErrorReport ctxt (mk_overlap_msg res)
                                 ; return False } }
  where
    -- Normal overlap error
    mk_overlap_msg (matches, unifiers, False)
      = ASSERT( not (null matches) )
        vcat [	addArising orig (ptext (sLit "Overlapping instances for") 
				<+> pprType (mkClassPred clas tys))
    	     ,	sep [ptext (sLit "Matching instances") <> colon,
    		     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])]

             ,  if not (null matching_givens) then 
                  sep [ptext (sLit "Matching givens (or their superclasses)") <> colon
                      , nest 2 (vcat matching_givens)]
                else empty

             ,  if null matching_givens && isSingleton matches && null unifiers then
                -- Intuitively, some given matched the wanted in their
                -- flattened or rewritten (from given equalities) form
                -- but the matcher can't figure that out because the
                -- constraints are non-flat and non-rewritten so we
                -- simply report back the whole given
                -- context. Accelerate Smart.hs showed this problem.
                  sep [ ptext (sLit "There exists a (perhaps superclass) match") <> colon
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

            matchable (evvars,gloc) 
              = case ev_vars_matching of
                     [] -> Nothing
                     _  -> Just $ hang (pprTheta ev_vars_matching)
                                    2 (sep [ ptext (sLit "bound by") <+> ppr (ctLocOrigin gloc)
                                           , ptext (sLit "at") <+> ppr (ctLocSpan gloc)])
                where ev_vars_matching = filter ev_var_matches (map evVarPred evvars)
                      ev_var_matches ty = case getClassPredTys_maybe ty of
                         Just (clas', tys')
                           | clas' == clas
                           , Just _ <- tcMatchTys (tyVarsOfTypes tys) tys tys'
                           -> True 
                           | otherwise
                           -> any ev_var_matches (immSuperClasses clas' tys')
                         Nothing -> False

    -- Overlap error because of Safe Haskell (first match should be the most
    -- specific match)
    mk_overlap_msg (matches, _unifiers, True)
      = ASSERT( length matches > 1 )
        vcat [ addArising orig (ptext (sLit "Unsafe overlapping instances for") 
                        <+> pprType (mkClassPred clas tys))
             , sep [ptext (sLit "The matching instance is") <> colon,
                    nest 2 (pprInstance $ head ispecs)]
             , vcat [ ptext $ sLit "It is compiled in a Safe module and as such can only"
                    , ptext $ sLit "overlap instances from the same module, however it"
                    , ptext $ sLit "overlaps the following instances from different modules:"
                    , nest 2 (vcat [pprInstances $ tail ispecs])
                    ]
             ]
        where
            ispecs = [ispec | (ispec, _) <- matches]

----------------------
quickFlattenTy :: TcType -> TcM TcType
-- See Note [Flattening in error message generation]
quickFlattenTy ty | Just ty' <- tcView ty = quickFlattenTy ty'
quickFlattenTy ty@(TyVarTy {})  = return ty
quickFlattenTy ty@(ForAllTy {}) = return ty     -- See
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
reportAmbigErrs :: ReportErrCtxt -> [WantedEvVar] -> TcM ()
reportAmbigErrs ctxt ambigs 
-- Divide into groups that share a common set of ambiguous tyvars
  = mapM_ (reportAmbigGroup ctxt) (equivClasses cmp ambigs_w_tvs) 
 where
    ambigs_w_tvs = [ (d, filter isAmbiguousTyVar (varSetElems (tyVarsOfEvVarX d)))
                   | d <- ambigs ]
    cmp (_,tvs1) (_,tvs2) = tvs1 `compare` tvs2


reportAmbigGroup :: ReportErrCtxt -> [(WantedEvVar, [TcTyVar])] -> TcM ()
-- The pairs all have the same [TcTyVar]
reportAmbigGroup ctxt pairs
  = setCtLoc loc $
    do { dflags <- getDOpts
       ; (tidy_env, docs) <- findGlobals ctxt (mkVarSet tvs)
       ; addErrTcM (tidy_env, main_msg $$ mk_msg dflags docs) }
  where
    (wev, tvs) : _ = pairs
    (loc, pp_wanteds) = pprWithArising (map fst pairs)
    main_msg = sep [ text "Ambiguous type variable" <> plural tvs
	             <+> pprQuotedList tvs
                     <+> text "in the constraint" <> plural pairs <> colon
                   , nest 2 pp_wanteds ]

    mk_msg dflags docs 
        | any isRuntimeUnkSkol tvs  -- See Note [Runtime skolems]
        =  vcat [ptext (sLit "Cannot resolve unknown runtime types:") <+>
                   (pprWithCommas ppr tvs),
                 ptext (sLit "Use :print or :force to determine these types")]

        | DerivOrigin <- ctLocOrigin (evVarX wev)
        = ptext (sLit "Probable fix: use a 'standalone deriving' declaration instead")

        | null docs 
        = ptext (sLit "Probable fix: add a type signature that fixes these type variable(s)")
			-- This happens in things like
			--	f x = show (read "foo")
			-- where monomorphism doesn't play any role
        | otherwise
	= vcat [ptext (sLit "Possible cause: the monomorphism restriction applied to the following:"),
		nest 2 (vcat docs),
		mono_fix dflags]

    mono_fix :: DynFlags -> SDoc
    mono_fix dflags
      = ptext (sLit "Probable fix:") <+> vcat
     	[ptext (sLit "give these definition(s) an explicit type signature"),
     	 if xopt Opt_MonomorphismRestriction dflags
         then ptext (sLit "or use -XNoMonomorphismRestriction")
         else empty]	-- Only suggest adding "-XNoMonomorphismRestriction"
     			-- if it is not already set!

getSkolemInfo :: [Implication] -> TcTyVar -> SkolemInfo
getSkolemInfo [] tv
  = WARN( True, ptext (sLit "No skolem info:") <+> ppr tv )
    UnkSkol
getSkolemInfo (implic:implics) tv
  | tv `elemVarSet` ic_skols implic = ctLocOrigin (ic_loc implic)
  | otherwise                       = getSkolemInfo implics tv

-----------------------
-- findGlobals looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.

mkEnvSigMsg :: SDoc -> [SDoc] -> SDoc
mkEnvSigMsg what env_sigs
 | null env_sigs = empty
 | otherwise = vcat [ ptext (sLit "The following variables have types that mention") <+> what
                    , nest 2 (vcat env_sigs) ]

findGlobals :: ReportErrCtxt
            -> TcTyVarSet
            -> TcM (TidyEnv, [SDoc])

findGlobals ctxt tvs 
  = do { lcl_ty_env <- case cec_encl ctxt of 
                        []    -> getLclTypeEnv
                        (i:_) -> return (ic_env i)
       ; go (cec_tidy ctxt) [] (nameEnvElts lcl_ty_env) }
  where
    go tidy_env acc [] = return (tidy_env, acc)
    go tidy_env acc (thing : things) = do
        (tidy_env1, maybe_doc) <- find_thing tidy_env ignore_it thing
	case maybe_doc of
	  Just d  -> go tidy_env1 (d:acc) things
	  Nothing -> go tidy_env1 acc     things

    ignore_it ty = tvs `disjointVarSet` tyVarsOfType ty

-----------------------
find_thing :: TidyEnv -> (TcType -> Bool)
           -> TcTyThing -> TcM (TidyEnv, Maybe SDoc)
find_thing tidy_env ignore_it (ATcId { tct_id = id })
  = do { (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env (idType id)
       ; if ignore_it tidy_ty then
	   return (tidy_env, Nothing)
         else do 
       { let msg = sep [ ppr id <+> dcolon <+> ppr tidy_ty
		       , nest 2 (parens (ptext (sLit "bound at") <+>
			 	   ppr (getSrcLoc id)))]
       ; return (tidy_env', Just msg) } }

find_thing tidy_env ignore_it (ATyVar tv ty)
  = do { (tidy_env1, tidy_ty) <- zonkTidyTcType tidy_env ty
       ; if ignore_it tidy_ty then
	    return (tidy_env, Nothing)
         else do
       { let -- The name tv is scoped, so we don't need to tidy it
            msg = sep [ ptext (sLit "Scoped type variable") <+> quotes (ppr tv) <+> eq_stuff
                      , nest 2 bound_at]

            eq_stuff | Just tv' <- tcGetTyVar_maybe tidy_ty
		     , getOccName tv == getOccName tv' = empty
		     | otherwise = equals <+> ppr tidy_ty
		-- It's ok to use Type.getTyVar_maybe because ty is zonked by now
	    bound_at = parens $ ptext (sLit "bound at:") <+> ppr (getSrcLoc tv)
 
       ; return (tidy_env1, Just msg) } }

find_thing _ _ thing = pprPanic "find_thing" (ppr thing)

warnDefaulting :: [Ct] -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
       ; let wanted_bag = listToBag wanteds
             tidy_env = tidyFreeTyVars env0 $
                        tyVarsOfCts wanted_bag
             tidy_wanteds = mapBag (tidyCt tidy_env) wanted_bag
             (loc, ppr_wanteds) = pprWithArising (map mk_wev (bagToList tidy_wanteds))
             warn_msg  = hang (ptext (sLit "Defaulting the following constraint(s) to type")
                                <+> quotes (ppr default_ty))
                            2 ppr_wanteds
       ; setCtLoc loc $ warnTc warn_default warn_msg }
  where mk_wev :: Ct -> WantedEvVar 
        mk_wev ct 
           | ev <- cc_id ct 
           , Wanted wloc <- cc_flavor ct
           = EvVarX ev wloc -- must return a WantedEvVar 
        mk_wev _ct = panic "warnDefaulting: encountered non-wanted for defaulting"

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
solverDepthErrorTcS :: Int -> [Ct] -> TcS a
solverDepthErrorTcS depth stack
  | null stack	    -- Shouldn't happen unless you say -fcontext-stack=0
  = wrapErrTcS $ failWith msg
  | otherwise
  = wrapErrTcS $ 
    setCtFlavorLoc (cc_flavor top_item) $
    do { ev_vars <- mapM (zonkEvVar . cc_id) stack
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyVars env0 (tyVarsOfEvVars ev_vars)
             tidy_ev_vars = map (tidyEvVar tidy_env) ev_vars
       ; failWithTcM (tidy_env, hang msg 2 (pprEvVars tidy_ev_vars)) }
  where
    top_item = head stack
    msg = vcat [ ptext (sLit "Context reduction stack overflow; size =") <+> int depth
               , ptext (sLit "Use -fcontext-stack=N to increase stack size to N") ]

flattenForAllErrorTcS :: CtFlavor -> TcType -> TcS a
flattenForAllErrorTcS fl ty
  = wrapErrTcS        $ 
    setCtFlavorLoc fl $ 
    do { env0 <- tcInitTidyEnv
       ; let (env1, ty') = tidyOpenType env0 ty 
             msg = sep [ ptext (sLit "Cannot deal with a type function under a forall type:")
                       , ppr ty' ]
       ; failWithTcM (env1, msg) }
\end{code}

%************************************************************************
%*									*
                 Setting the context
%*									*
%************************************************************************

\begin{code}
setCtFlavorLoc :: CtFlavor -> TcM a -> TcM a
setCtFlavorLoc (Wanted  loc)   thing = setCtLoc loc thing
setCtFlavorLoc (Derived loc)   thing = setCtLoc loc thing
setCtFlavorLoc (Given loc _gk) thing = setCtLoc loc thing
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

zonkTidyOrigin :: ReportErrCtxt -> CtOrigin -> TcM CtOrigin
zonkTidyOrigin ctxt (TypeEqOrigin (UnifyOrigin { uo_actual = act, uo_expected = exp }))
  = do { (env1,  act') <- zonkTidyTcType (cec_tidy ctxt) act
       ; (_env2, exp') <- zonkTidyTcType env1            exp
       ; return (TypeEqOrigin (UnifyOrigin { uo_actual = act', uo_expected = exp' })) }
       -- Drop the returned env on the floor; we may conceivably thereby get
       -- inconsistent naming between uses of this function
zonkTidyOrigin _ orig = return orig
\end{code}
