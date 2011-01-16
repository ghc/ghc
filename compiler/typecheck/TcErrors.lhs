\begin{code}
module TcErrors( 
       reportUnsolved, reportUnsolvedDeriv,
       reportUnsolvedWantedEvVars, warnDefaulting, 
       unifyCtxt, typeExtraInfoMsg, 

       flattenForAllErrorTcS,
       solverDepthErrorTcS
  ) where

#include "HsVersions.h"

import TcRnMonad
import TcMType
import TcSMonad
import TcType
import TypeRep

import Inst
import InstEnv

import TyCon
import Name
import NameEnv
import Id	( idType )
import HsExpr	( pprMatchContext )
import Var
import VarSet
import VarEnv
import SrcLoc
import Bag
import ListSetOps( equivClasses )
import Util
import FastString
import Outputable
import DynFlags
import StaticFlags( opt_PprStyle_Debug )
import Data.List( partition )
import Control.Monad( when, unless )
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
reportUnsolved :: (Bag WantedEvVar, Bag Implication) -> Bag FrozenError -> TcM ()
reportUnsolved (unsolved_flats, unsolved_implics) frozen_errors
  | isEmptyBag unsolved && isEmptyBag frozen_errors
  = return ()
  | otherwise
  = do { frozen_errors_zonked <- mapBagM zonk_frozen frozen_errors
       ; let frozen_tvs = tyVarsOfFrozen frozen_errors_zonked 

       ; unsolved <- mapBagM zonkWanted unsolved
		     -- Zonk to un-flatten any flatten-skols
       ; env0 <- tcInitTidyEnv
       ; let tidy_env      = tidyFreeTyVars env0 $ 
                             tyVarsOfWanteds unsolved `unionVarSet` frozen_tvs 

             tidy_unsolved = tidyWanteds tidy_env unsolved
             err_ctxt = CEC { cec_encl = [] 
                            , cec_extra = empty
                            , cec_tidy = tidy_env 
                            } 

       ; traceTc "reportUnsolved" (vcat [
              text "Unsolved constraints =" <+> ppr unsolved,
              text "Frozen errors =" <+> ppr frozen_errors_zonked ])

       ; let tidy_frozen_errors_zonked = tidyFrozen tidy_env frozen_errors_zonked

       ; reportTidyFrozens tidy_env tidy_frozen_errors_zonked 
       ; reportTidyWanteds err_ctxt tidy_unsolved }
  where
    unsolved = Bag.mapBag WcEvVar unsolved_flats `unionBags` 
                 Bag.mapBag WcImplic unsolved_implics

    zonk_frozen (FrozenError frknd fl ty1 ty2)
      = do { ty1z <- zonkTcType ty1 
           ; ty2z <- zonkTcType ty2
           ; return (FrozenError frknd fl ty1z ty2z) }

    tyVarsOfFrozen fr 
      = unionVarSets $ bagToList (mapBag tvs_of_frozen fr)
    tvs_of_frozen (FrozenError _ _ ty1 ty2) = tyVarsOfTypes [ty1,ty2]

    tidyFrozen env fr = mapBag (tidy_frozen env) fr
    tidy_frozen env (FrozenError frknd fl ty1 ty2)
      = FrozenError frknd fl (tidyType env ty1) (tidyType env ty2)

reportTidyFrozens :: TidyEnv -> Bag FrozenError -> TcM ()
reportTidyFrozens tidy_env fr = mapBagM_ (reportTidyFrozen tidy_env) fr 

reportTidyFrozen :: TidyEnv -> FrozenError -> TcM () 
reportTidyFrozen tidy_env err@(FrozenError _ fl _ty1 _ty2)
  = do { let dec_errs = decompFrozenError err
             init_err_ctxt = CEC { cec_encl  = [] 
                                 , cec_extra = empty
                                 , cec_tidy  = tidy_env }
       ; mapM_ (report_dec_err init_err_ctxt) dec_errs }
  where 
    report_dec_err err_ctxt (ty1,ty2)
        -- The only annoying thing here is that in the given case, 
        -- the ``Inaccessible code'' message will be printed once for 
        -- each decomposed equality.
          = do { (tidy_env2,extra2)
                     <- if isGiven fl
                        then return (cec_tidy err_ctxt, inaccessible_msg)
                        else getWantedEqExtra emptyTvSubst (cec_tidy err_ctxt) loc_orig ty1 ty2
               ; let err_ctxt2 = err_ctxt { cec_tidy  = tidy_env2
                                          , cec_extra = cec_extra err_ctxt $$ extra2 }
               ; setCtFlavorLoc fl $ 
                 reportEqErr err_ctxt2 ty1 ty2 
               }

    loc_orig | Wanted loc <- fl    = ctLocOrigin loc
             | Derived loc _ <- fl = ctLocOrigin loc
             | otherwise           = pprPanic "loc_orig" empty 

    inaccessible_msg 
      | Given loc <- fl
      = hang (ptext (sLit "Inaccessible code in")) 2 (mk_what loc)
      | otherwise = pprPanic "inaccessible_msg" empty

    mk_what loc
      = case ctLocOrigin loc of
          PatSkol dc mc -> sep [ ptext (sLit "a pattern with constructor") 
                                   <+> quotes (ppr dc) <> comma
                               , ptext (sLit "in") <+> pprMatchContext mc ]
	  other_skol -> pprSkolInfo other_skol


decompFrozenError :: FrozenError -> [(TcType,TcType)] 
-- Postcondition: will always return a non-empty list
decompFrozenError (FrozenError errk _fl ty1 ty2) 
  | OccCheckError <- errk
  = dec_occ_check ty1 ty2 
  | otherwise 
  = [(ty1,ty2)]
  where dec_occ_check :: TcType -> TcType -> [(TcType,TcType)] 
        -- This error arises from an original: 
        --      a ~ Maybe a
        -- But by now the a has been substituted away, eg: 
        --      Int ~ Maybe Int
        --      Maybe b ~ Maybe (Maybe b)
        dec_occ_check ty1 ty2 
          | tcEqType ty1 ty2 = []
        dec_occ_check ty1@(TyVarTy {}) ty2 = [(ty1,ty2)] 
        dec_occ_check (FunTy s1 t1) (FunTy s2 t2) 
          = let errs1 = dec_occ_check s1 s2 
                errs2 = dec_occ_check t1 t2
            in errs1 ++ errs2 
        dec_occ_check ty1@(TyConApp fn1 tys1) ty2@(TyConApp fn2 tys2) 
          | fn1 == fn2 && length tys1 == length tys2 
          , not (isSynFamilyTyCon fn1)
          = concatMap (\(t1,t2) -> dec_occ_check t1 t2) (zip tys1 tys2)
          | otherwise 
          = [(ty1,ty2)]
        dec_occ_check ty1 ty2 
          | Just (s1,t1) <- tcSplitAppTy_maybe ty1 
          , Just (s2,t2) <- tcSplitAppTy_maybe ty2 
          = let errs1 = dec_occ_check s1 s2 
                errs2 = dec_occ_check t1 t2 
            in errs1 ++ errs2
        dec_occ_check ty1 ty2 = [(ty1,ty2)]

reportUnsolvedWantedEvVars :: Bag WantedEvVar -> TcM ()
reportUnsolvedWantedEvVars wanteds
  | isEmptyBag wanteds 
  = return ()
  | otherwise
  = do { wanteds <- mapBagM zonkWantedEvVar wanteds
       ; env0 <- tcInitTidyEnv
       ; let tidy_env      = tidyFreeTyVars env0 (tyVarsOfWantedEvVars wanteds)
             tidy_unsolved = tidyWantedEvVars tidy_env wanteds
             err_ctxt = CEC { cec_encl  = [] 
                            , cec_extra = empty
                            , cec_tidy  = tidy_env } 
       ; groupErrs (reportFlat err_ctxt) (bagToList tidy_unsolved) }

reportUnsolvedDeriv :: [PredType] -> WantedLoc -> TcM ()
reportUnsolvedDeriv unsolved loc
  | null unsolved
  = return ()
  | otherwise
  = setCtLoc loc $
    do { unsolved <- zonkTcThetaType unsolved
       ; env0 <- tcInitTidyEnv
       ; let tidy_env      = tidyFreeTyVars env0 (tyVarsOfTheta unsolved)
             tidy_unsolved = map (tidyPred tidy_env) unsolved
             err_ctxt = CEC { cec_encl  = [] 
                            , cec_extra = alt_fix
                            , cec_tidy  = tidy_env } 
       ; reportFlat err_ctxt tidy_unsolved (ctLocOrigin loc) }
  where
    alt_fix = vcat [ptext (sLit "Alternatively, use a standalone 'deriving instance' declaration,"),
                    nest 2 $ ptext (sLit "so you can specify the instance context yourself")]

--------------------------------------------
--      Internal functions
--------------------------------------------

data ReportErrCtxt 
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                	       	       --   (innermost first)
          , cec_tidy     :: TidyEnv
          , cec_extra    :: SDoc       -- Add this to each error message
      }

reportTidyImplic :: ReportErrCtxt -> Implication -> TcM ()
reportTidyImplic ctxt implic
  = reportTidyWanteds ctxt' (ic_wanted implic)
  where
    ctxt' = ctxt { cec_encl = implic : cec_encl ctxt }
  
reportTidyWanteds :: ReportErrCtxt -> WantedConstraints -> TcM ()
reportTidyWanteds ctxt unsolved
  = do { let (flats,  implics)    = splitWanteds unsolved
             (ambigs, non_ambigs) = partition is_ambiguous (bagToList flats)
       	     (tv_eqs, others)     = partition is_tv_eq non_ambigs

       ; groupErrs (reportEqErrs ctxt) tv_eqs
       ; when (null tv_eqs) $ groupErrs (reportFlat ctxt) others
       ; traceTc "rtw" (vcat [
              text "unsolved =" <+> ppr unsolved,
              text "tveqs =" <+> ppr tv_eqs,
              text "others =" <+> ppr others,
              text "ambigs =" <+> ppr ambigs ,
              text "implics =" <+> ppr implics])

       ; when (null tv_eqs) $ mapBagM_ (reportTidyImplic ctxt) implics

       	   -- Only report ambiguity if no other errors (at all) happened
	   -- See Note [Avoiding spurious errors] in TcSimplify
       ; ifErrsM (return ()) $ reportAmbigErrs ctxt skols ambigs }
  where
    skols = foldr (unionVarSet . ic_skols) emptyVarSet (cec_encl ctxt)
 
	-- Report equalities of form (a~ty) first.  They are usually
	-- skolem-equalities, and they cause confusing knock-on 
	-- effects in other errors; see test T4093b.
    is_tv_eq c | EqPred ty1 ty2 <- wantedEvVarPred c
               = tcIsTyVarTy ty1 || tcIsTyVarTy ty2
               | otherwise = False

	-- Treat it as "ambiguous" if 
	--   (a) it is a class constraint
        --   (b) it constrains only type variables
	--       (else we'd prefer to report it as "no instance for...")
        --   (c) it mentions type variables that are not skolems
    is_ambiguous d = isTyVarClassPred pred
                  && not (tyVarsOfPred pred `subVarSet` skols)
		  where   
                     pred = wantedEvVarPred d

reportFlat :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()
-- The [PredType] are already tidied
reportFlat ctxt flats origin
  = do { unless (null dicts) $ reportDictErrs ctxt dicts origin
       ; unless (null eqs)   $ reportEqErrs   ctxt eqs   origin
       ; unless (null ips)   $ reportIPErrs   ctxt ips   origin
       ; ASSERT( null others ) return () }
  where
    (dicts, non_dicts) = partition isClassPred flats
    (eqs, non_eqs)     = partition isEqPred    non_dicts
    (ips, others)      = partition isIPPred    non_eqs

--------------------------------------------
--      Support code 
--------------------------------------------

groupErrs :: ([PredType] -> CtOrigin -> TcM ()) -- Deal with one group
	  -> [WantedEvVar]	                -- Unsolved wanteds
          -> TcM ()
-- Group together insts with the same origin
-- We want to report them together in error messages

groupErrs _ [] 
  = return ()
groupErrs report_err (wanted : wanteds)
  = do	{ setCtLoc the_loc $ 
          report_err the_vars (ctLocOrigin the_loc)
	; groupErrs report_err others }
  where
   the_loc           = wantedEvVarLoc wanted
   the_key	     = mk_key the_loc
   the_vars          = map wantedEvVarPred (wanted:friends)
   (friends, others) = partition is_friend wanteds
   is_friend friend  = mk_key (wantedEvVarLoc friend) == the_key

   mk_key :: WantedLoc -> (SrcSpan, String)
   mk_key loc = (ctLocSpan loc, showSDoc (ppr (ctLocOrigin loc)))
	-- It may seem crude to compare the error messages,
	-- but it makes sure that we combine just what the user sees,
	-- and it avoids need equality on InstLocs.

-- Add the "arising from..." part to a message about bunch of dicts
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = msg $$ nest 2 (pprArising orig)

pprWithArising :: [WantedEvVar] -> (WantedLoc, SDoc)
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising froma use of p at q
-- Also return a location for the erroe message
pprWithArising [] 
  = panic "pprWithArising"
pprWithArising [WantedEvVar ev loc] 
  = (loc, pprEvVarTheta [ev] <+> pprArising (ctLocOrigin loc))
pprWithArising ev_vars
  = (first_loc, vcat (map ppr_one ev_vars))
  where
    first_loc = wantedEvVarLoc (head ev_vars)
    ppr_one (WantedEvVar v loc) 
       = parens (pprPred (evVarPred v)) <+> pprArisingAt loc

addErrorReport :: ReportErrCtxt -> SDoc -> TcM ()
addErrorReport ctxt msg = addErrTcM (cec_tidy ctxt, msg $$ cec_extra ctxt)

pprErrCtxtLoc :: ReportErrCtxt -> SDoc
pprErrCtxtLoc ctxt 
  = case map (ctLocOrigin . ic_loc) (cec_encl ctxt) of
       []           -> ptext (sLit "the top level")	-- Should not happen
       (orig:origs) -> ppr_skol orig $$ 
                       vcat [ ptext (sLit "or") <+> ppr_skol orig | orig <- origs ]
  where
    ppr_skol (PatSkol dc _) = ptext (sLit "the data constructor") <+> quotes (ppr dc)
    ppr_skol skol_info      = pprSkolInfo skol_info

getUserGivens :: ReportErrCtxt -> Maybe [EvVar]
-- Just gs => Say "could not deduce ... from gs"
-- Nothing => No interesting givens, say something else
getUserGivens (CEC {cec_encl = ctxt})
  | null user_givens = Nothing
  | otherwise        = Just user_givens
  where 
    givens = foldl (\gs ic -> ic_given ic ++ gs) [] ctxt
    user_givens | opt_PprStyle_Debug = givens
                | otherwise          = filterOut isSilentEvVar givens
       -- In user mode, don't show the "silent" givens, used for
       -- the "self" dictionary and silent superclass arguments for dfuns
\end{code}


%************************************************************************
%*									*
                Implicit parameter errors
%*									*
%************************************************************************

\begin{code}
reportIPErrs :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()
reportIPErrs ctxt ips orig
  = addErrorReport ctxt $ addArising orig msg
  where
    msg | Just givens <- getUserGivens ctxt
        = couldNotDeduce givens ips
        | otherwise
        = sep [ ptext (sLit "Unbound implicit parameter") <> plural ips
              , nest 2 (pprTheta ips) ] 
\end{code}


%************************************************************************
%*									*
                Equality errors
%*									*
%************************************************************************

\begin{code}
reportEqErrs :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()
-- The [PredType] are already tidied
reportEqErrs ctxt eqs orig
  = mapM_ report_one eqs 
  where
    env0 = cec_tidy ctxt
    report_one (EqPred ty1 ty2) 
      = do { (env1, extra) <- getWantedEqExtra emptyTvSubst env0 orig ty1 ty2
            ; let ctxt' = ctxt { cec_tidy = env1
                               , cec_extra = extra $$ cec_extra ctxt }
           ; reportEqErr ctxt' ty1 ty2 }
    report_one pred 
      = pprPanic "reportEqErrs" (ppr pred)    

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
  | not is_meta1
  , Just tv2 <- tcGetTyVar_maybe ty2
  , isMetaTyVar tv2
  = -- sk ~ alpha: swap
    reportTyVarEqErr ctxt tv2 ty1

  | not is_meta1
  = -- sk ~ ty, where ty isn't a meta-tyvar: mis-match
    addErrorReport (addExtraInfo ctxt ty1 ty2)
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
                                 , nest 2 $ pprSkolInfo (ctLocOrigin implic_loc) ] ]
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
                         , ptext (sLit "bound at") <+> pprSkolInfo (ctLocOrigin implic_loc)]
       ; addErrorReport (addExtraInfo ctxt ty1 ty2) (msg $$ nest 2 extra) }

  | otherwise      -- This can happen, by a recursive decomposition of frozen
                   -- occurs check constraints
                   -- Example: alpha ~ T Int alpha has frozen.
                   --          Then alpha gets unified to T beta gamma
                   -- So now we have  T beta gamma ~ T Int (T beta gamma)
                   -- Decompose to (beta ~ Int, gamma ~ T beta gamma)
                   -- The (gamma ~ T beta gamma) is the occurs check, but
                   -- the (beta ~ Int) isn't an error at all.  So return ()
  = return ()

  where         
    is_meta1 = isMetaTyVar tv1
    k1 	     = tyVarKind tv1
    k2 	     = typeKind ty2
    ty1      = mkTyVarTy tv1

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
  = case getUserGivens ctxt of
      Just givens -> couldNotDeduce givens [EqPred ty1 ty2]
      Nothing     -> misMatchMsg ty1 ty2

couldNotDeduce :: [EvVar] -> [PredType] -> SDoc
couldNotDeduce givens wanteds
  = sep [ ptext (sLit "Could not deduce") <+> pprTheta wanteds
        , nest 2 $ ptext (sLit "from the context") 
                     <+> pprEvVarTheta givens]

addExtraInfo :: ReportErrCtxt -> TcType -> TcType -> ReportErrCtxt
-- Add on extra info about the types themselves
-- NB: The types themselves are already tidied
addExtraInfo ctxt ty1 ty2
  = ctxt { cec_tidy  = env2
         , cec_extra = nest 2 (extra1 $$ extra2) $$ cec_extra ctxt }
  where
    (env1, extra1) = typeExtraInfoMsg (cec_tidy ctxt) ty1
    (env2, extra2) = typeExtraInfoMsg env1            ty2

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

typeExtraInfoMsg :: TidyEnv -> Type -> (TidyEnv, SDoc)
-- Shows a bit of extra info about skolem constants
typeExtraInfoMsg env ty 
  | Just tv <- tcGetTyVar_maybe ty
  , isTcTyVar tv
  , isSkolemTyVar tv || isSigTyVar tv
  , not (isUnkSkol tv)
  , let (env1, tv1) = tidySkolemTyVar env tv
  = (env1, pprSkolTvBinding tv1)
  where
typeExtraInfoMsg env _ty = (env, empty)		-- Normal case

--------------------
unifyCtxt :: EqOrigin -> TidyEnv -> TcM (TidyEnv, SDoc)
unifyCtxt (UnifyOrigin { uo_actual = act_ty, uo_expected = exp_ty }) tidy_env
  = do  { act_ty' <- zonkTcType act_ty
        ; exp_ty' <- zonkTcType exp_ty
        ; let (env1, exp_ty'') = tidyOpenType tidy_env exp_ty'
              (env2, act_ty'') = tidyOpenType env1     act_ty'
        ; return (env2, mkExpectedActualMsg act_ty'' exp_ty'') }

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
reportDictErrs :: ReportErrCtxt -> [PredType] -> CtOrigin -> TcM ()	
reportDictErrs ctxt wanteds orig
  = do { inst_envs <- tcGetInstEnvs
       ; non_overlaps <- mapMaybeM (reportOverlap ctxt inst_envs orig) wanteds
       ; unless (null non_overlaps) $
         addErrorReport ctxt (mk_no_inst_err non_overlaps) }
  where
    mk_no_inst_err :: [PredType] -> SDoc
    mk_no_inst_err wanteds
      | Just givens <- getUserGivens ctxt
      = vcat [ addArising orig $ couldNotDeduce givens wanteds
	     , show_fixes (fix1 : fixes2) ]

      | otherwise	-- Top level 
      = vcat [ addArising orig $
	       ptext (sLit "No instance") <> plural wanteds
		    <+> ptext (sLit "for") <+> pprTheta wanteds
	     , show_fixes fixes2 ]

      where
    	fix1 = sep [ ptext (sLit "add") <+> pprTheta wanteds 
                          <+> ptext (sLit "to the context of")
	           , nest 2 $ pprErrCtxtLoc ctxt ]

        fixes2 = case instance_dicts of
                   []  -> []
                   [_] -> [sep [ptext (sLit "add an instance declaration for"),
                                pprTheta instance_dicts]]
                   _   -> [sep [ptext (sLit "add instance declarations for"),
                                pprTheta instance_dicts]]
        instance_dicts = filterOut isTyVarClassPred wanteds
		-- Insts for which it is worth suggesting an adding an 
		-- instance declaration.  Exclude tyvar dicts.

	show_fixes :: [SDoc] -> SDoc
	show_fixes []     = empty
	show_fixes (f:fs) = sep [ptext (sLit "Possible fix:"), 
				 nest 2 (vcat (f : map (ptext (sLit "or") <+>) fs))]

reportOverlap :: ReportErrCtxt -> (InstEnv,InstEnv) -> CtOrigin
              -> PredType -> TcM (Maybe PredType)
-- Report an overlap error if this class constraint results
-- from an overlap (returning Nothing), otherwise return (Just pred)
reportOverlap ctxt inst_envs orig pred@(ClassP clas tys)
  = do { tys_flat <- mapM quickFlattenTy tys
           -- Note [Flattening in error message generation]

       ; case lookupInstEnv inst_envs clas tys_flat of
                ([], _) -> return (Just pred)               -- No match
		-- The case of exactly one match and no unifiers means a
		-- successful lookup.  That can't happen here, because dicts
		-- only end up here if they didn't match in Inst.lookupInst
		([_],[])
		 | debugIsOn -> pprPanic "check_overlap" (ppr pred)
                res          -> do { addErrorReport ctxt (mk_overlap_msg res)
                                   ; return Nothing } }
  where
    mk_overlap_msg (matches, unifiers)
      = ASSERT( not (null matches) )
        vcat [	addArising orig (ptext (sLit "Overlapping instances for") 
				<+> pprPred pred)
    	     ,	sep [ptext (sLit "Matching instances") <> colon,
    		     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])]
	     ,	if not (isSingleton matches)
    		then 	-- Two or more matches
		     empty
    		else 	-- One match, plus some unifiers
		ASSERT( not (null unifiers) )
		parens (vcat [ptext (sLit "The choice depends on the instantiation of") <+>
	    		         quotes (pprWithCommas ppr (varSetElems (tyVarsOfPred pred))),
			      ptext (sLit "To pick the first instance above, use -XIncoherentInstances"),
			      ptext (sLit "when compiling the other instance declarations")])]
      where
    	ispecs = [ispec | (ispec, _) <- matches]

reportOverlap _ _ _ _ = panic "reportOverlap"    -- Not a ClassP

----------------------
quickFlattenTy :: TcType -> TcM TcType
-- See Note [Flattening in error message generation]
quickFlattenTy ty | Just ty' <- tcView ty = quickFlattenTy ty'
quickFlattenTy ty@(TyVarTy {})  = return ty
quickFlattenTy ty@(LiteralTy {})= return ty
quickFlattenTy ty@(ForAllTy {}) = return ty     -- See
quickFlattenTy ty@(PredTy {})   = return ty     -- Note [Quick-flatten polytypes]
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
reportAmbigErrs :: ReportErrCtxt -> TcTyVarSet -> [WantedEvVar] -> TcM ()
reportAmbigErrs ctxt skols ambigs 
-- Divide into groups that share a common set of ambiguous tyvars
  = mapM_ report (equivClasses cmp ambigs_w_tvs)
  where
    ambigs_w_tvs = [ (d, varSetElems (tyVarsOfWantedEvVar d `minusVarSet` skols))
                   | d <- ambigs ]
    cmp (_,tvs1) (_,tvs2) = tvs1 `compare` tvs2

    report :: [(WantedEvVar, [TcTyVar])] -> TcM ()
    report pairs
       = setCtLoc loc $
         do { let main_msg = sep [ text "Ambiguous type variable" <> plural tvs
	         	           <+> pprQuotedList tvs
                                   <+> text "in the constraint" <> plural pairs <> colon
                                 , nest 2 pp_wanteds ]
             ; (tidy_env, mono_msg) <- mkMonomorphismMsg ctxt tvs
            ; addErrTcM (tidy_env, main_msg $$ mono_msg) }
       where
         (_, tvs) : _ = pairs
         (loc, pp_wanteds) = pprWithArising (map fst pairs)

mkMonomorphismMsg :: ReportErrCtxt -> [TcTyVar] -> TcM (TidyEnv, SDoc)
-- There's an error with these Insts; if they have free type variables
-- it's probably caused by the monomorphism restriction. 
-- Try to identify the offending variable
-- ASSUMPTION: the Insts are fully zonked
mkMonomorphismMsg ctxt inst_tvs
  = do	{ dflags <- getDOpts
	; traceTc "Mono" (vcat (map pprSkolTvBinding inst_tvs))
        ; (tidy_env, docs) <- findGlobals ctxt (mkVarSet inst_tvs)
	; return (tidy_env, mk_msg dflags docs) }
  where
    mk_msg _ _ | any isRuntimeUnkSkol inst_tvs  -- See Note [Runtime skolems]
        =  vcat [ptext (sLit "Cannot resolve unknown runtime types:") <+>
                   (pprWithCommas ppr inst_tvs),
                ptext (sLit "Use :print or :force to determine these types")]
    mk_msg _ []   = ptext (sLit "Probable fix: add a type signature that fixes these type variable(s)")
			-- This happens in things like
			--	f x = show (read "foo")
			-- where monomorphism doesn't play any role
    mk_msg dflags docs 
	= vcat [ptext (sLit "Possible cause: the monomorphism restriction applied to the following:"),
		nest 2 (vcat docs),
		monomorphism_fix dflags]

monomorphism_fix :: DynFlags -> SDoc
monomorphism_fix dflags
  = ptext (sLit "Probable fix:") <+> vcat
	[ptext (sLit "give these definition(s) an explicit type signature"),
	 if xopt Opt_MonomorphismRestriction dflags
           then ptext (sLit "or use -XNoMonomorphismRestriction")
           else empty]	-- Only suggest adding "-XNoMonomorphismRestriction"
			-- if it is not already set!


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
  = do { id_ty <- zonkTcType  (idType id)
       ; if ignore_it id_ty then
	   return (tidy_env, Nothing)
         else do 
       { let (tidy_env', tidy_ty) = tidyOpenType tidy_env id_ty
	     msg = sep [ ppr id <+> dcolon <+> ppr tidy_ty
		       , nest 2 (parens (ptext (sLit "bound at") <+>
			 	   ppr (getSrcLoc id)))]
       ; return (tidy_env', Just msg) } }

find_thing tidy_env ignore_it (ATyVar tv ty)
  = do { tv_ty <- zonkTcType ty
       ; if ignore_it tv_ty then
	    return (tidy_env, Nothing)
         else do
       { let -- The name tv is scoped, so we don't need to tidy it
	    (tidy_env1, tidy_ty) = tidyOpenType  tidy_env tv_ty
            msg = sep [ ptext (sLit "Scoped type variable") <+> quotes (ppr tv) <+> eq_stuff
                      , nest 2 bound_at]

	    eq_stuff | Just tv' <- tcGetTyVar_maybe tv_ty 
		     , getOccName tv == getOccName tv' = empty
		     | otherwise = equals <+> ppr tidy_ty
		-- It's ok to use Type.getTyVar_maybe because ty is zonked by now
	    bound_at = parens $ ptext (sLit "bound at:") <+> ppr (getSrcLoc tv)
 
       ; return (tidy_env1, Just msg) } }

find_thing _ _ thing = pprPanic "find_thing" (ppr thing)

warnDefaulting :: [WantedEvVar] -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- doptM Opt_WarnTypeDefaults
       ; setCtLoc loc $ warnTc warn_default warn_msg }
  where
	-- Tidy them first
    warn_msg  = vcat [ ptext (sLit "Defaulting the following constraint(s) to type") <+>
				quotes (ppr default_ty),
		      nest 2 ppr_wanteds ]
    (loc, ppr_wanteds) = pprWithArising wanteds
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

solverDepthErrorTcS :: Int -> [CanonicalCt] -> TcS a
solverDepthErrorTcS depth stack
  | null stack	    -- Shouldn't happen unless you say -fcontext-stack=0
  = wrapErrTcS $ failWith msg
  | otherwise
  = wrapErrTcS $ 
    setCtFlavorLoc (cc_flavor top_item) $
    do { env0 <- tcInitTidyEnv
       ; let ev_vars  = map cc_id stack
             env1     = tidyFreeTyVars env0 free_tvs
             free_tvs = foldr (unionVarSet . tyVarsOfEvVar) emptyVarSet ev_vars
             extra    = pprEvVars (map (tidyEvVar env1) ev_vars)
       ; failWithTcM (env1, hang msg 2 extra) }
  where
    top_item = head stack
    msg = vcat [ ptext (sLit "Context reduction stack overflow; size =") <+> int depth
               , ptext (sLit "Use -fcontext-stack=N to increase stack size to N") ]

flattenForAllErrorTcS :: CtFlavor -> TcType -> Bag CanonicalCt -> TcS a
flattenForAllErrorTcS fl ty _bad_eqs
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
setCtFlavorLoc (Derived loc _) thing = setCtLoc loc thing
setCtFlavorLoc (Given   loc)   thing = setCtLoc loc thing

getWantedEqExtra :: TvSubst -> TidyEnv -> CtOrigin -> TcType -> TcType
                 -> TcM (TidyEnv, SDoc)
getWantedEqExtra subst env0 (TypeEqOrigin item) ty1 ty2
  -- If the types in the error message are the same 
  -- as the types we are unifying (remember to zonk the latter)
  -- don't add the extra expected/actual message
  --
  -- The complication is that the types in the TypeEqOrigin must
  --   (a) be zonked
  --   (b) have any TcS-monad pending equalities applied to them 
  --   	   	(hence the passed-in substitution)
  = do { (env1, act) <- zonkSubstTidy env0 subst (uo_actual item)
       ; (env2, exp) <- zonkSubstTidy env1 subst (uo_expected item)
       ; if (act `tcEqType` ty1 && exp `tcEqType` ty2)
         || (exp `tcEqType` ty1 && act `tcEqType` ty2)
         then	
            return (env0, empty)
         else 
            return (env2, mkExpectedActualMsg act exp) }

getWantedEqExtra _ env0 orig _ _ 
  = return (env0, pprArising orig)

zonkSubstTidy :: TidyEnv -> TvSubst -> TcType -> TcM (TidyEnv, TcType)
-- In general, becore printing a type, we want to
--   a) Zonk it.  Even during constraint simplification this is
--      is important, to un-flatten the flatten skolems in a type
--   b) Substitute any solved unification variables.  This is
--      only important *during* solving, becuase after solving
--      the substitution is expressed in the mutable type variables
--      But during solving there may be constraint (F xi ~ ty)
--      where the substitution has not been applied to the RHS
zonkSubstTidy env subst ty
  = do { ty' <- zonkTcTypeAndSubst subst ty
       ; return (tidyOpenType env ty') }
\end{code}
