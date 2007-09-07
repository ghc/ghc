
\begin{code}
module TcTyFuns (
	tcNormalizeFamInst,

	normaliseGivens, normaliseGivenDicts, 
	normaliseWanteds, normaliseWantedDicts,
	solveWanteds,
	substEqInDictInsts,
	
	addBind 		-- should not be here
  ) where


#include "HsVersions.h"

import HsSyn

import TcRnMonad
import TcEnv
import Inst
import TcType
import TcMType
import Coercion
import TypeRep	( Type(..) )
import TyCon
import Var	( isTcTyVar )
import Type
import Bag
import Outputable
import SrcLoc	( Located(..) )
import Maybes

import Data.List
import Control.Monad (liftM)
\end{code}


%************************************************************************
%*									*
		Normalisation of types
%*									*
%************************************************************************

Unfold a single synonym family instance and yield the witnessing coercion.
Return 'Nothing' if the given type is either not synonym family instance
or is a synonym family instance that has no matching instance declaration.
(Applies only if the type family application is outermost.)

For example, if we have

  :Co:R42T a :: T [a] ~ :R42T a

then 'T [Int]' unfolds to (:R42T Int, :Co:R42T Int).

\begin{code}
tcUnfoldSynFamInst :: Type -> TcM (Maybe (Type, Coercion))
tcUnfoldSynFamInst (TyConApp tycon tys)
  | not (isOpenSynTyCon tycon)     -- unfold *only* _synonym_ family instances
  = return Nothing
  | otherwise
  = do { -- we only use the indexing arguments for matching, 
         -- not the additional ones
       ; maybeFamInst <- tcLookupFamInst tycon idxTys
       ; case maybeFamInst of
           Nothing                -> return Nothing
           Just (rep_tc, rep_tys) -> return $ Just (mkTyConApp rep_tc tys',
		                                    mkTyConApp coe_tc tys')
             where
               tys'   = rep_tys ++ restTys
               coe_tc = expectJust "TcTyFun.tcUnfoldSynFamInst" 
                                   (tyConFamilyCoercion_maybe rep_tc)
       }
    where
        n                = tyConArity tycon
        (idxTys, restTys) = splitAt n tys
tcUnfoldSynFamInst _other = return Nothing
\end{code}

Normalise 'Type's and 'PredType's by unfolding type family applications where
possible (ie, we treat family instances as a TRS).  Also zonk meta variables.

	tcNormalizeFamInst ty = (co, ty')
	then   co : ty ~ ty'

\begin{code}
tcNormalizeFamInst :: Type -> TcM (CoercionI, Type)
tcNormalizeFamInst = tcGenericNormalizeFamInst tcUnfoldSynFamInst

tcNormalizeFamInstPred :: TcPredType -> TcM (CoercionI, TcPredType)
tcNormalizeFamInstPred = tcGenericNormalizeFamInstPred tcUnfoldSynFamInst
\end{code}

Generic normalisation of 'Type's and 'PredType's; ie, walk the type term and
apply the normalisation function gives as the first argument to every TyConApp
and every TyVarTy subterm.

	tcGenericNormalizeFamInst fun ty = (co, ty')
	then   co : ty ~ ty'

This function is (by way of using smart constructors) careful to ensure that
the returned coercion is exactly IdCo (and not some semantically equivalent,
but syntactically different coercion) whenever (ty' `tcEqType` ty).  This
makes it easy for the caller to determine whether the type changed.  BUT
even if we return IdCo, ty' may be *syntactically* different from ty due to
unfolded closed type synonyms (by way of tcCoreView).  In the interest of
good error messages, callers should discard ty' in favour of ty in this case.

\begin{code}
tcGenericNormalizeFamInst :: (TcType -> TcM (Maybe (TcType,Coercion))) 	
                             -- what to do with type functions and tyvars
	                   -> TcType  			-- old type
	                   -> TcM (CoercionI, Type)	-- (coercion, new type)
tcGenericNormalizeFamInst fun ty
  | Just ty' <- tcView ty = tcGenericNormalizeFamInst fun ty' 
tcGenericNormalizeFamInst fun (TyConApp tyCon tys)
  = do	{ (cois, ntys) <- mapAndUnzipM (tcGenericNormalizeFamInst fun) tys
	; let tycon_coi = mkTyConAppCoI tyCon ntys cois
	; maybe_ty_co <- fun (TyConApp tyCon ntys)      -- use normalised args!
	; case maybe_ty_co of
	    -- a matching family instance exists
	    Just (ty', co) ->
	      do { let first_coi = mkTransCoI tycon_coi (ACo co)
		 ; (rest_coi, nty) <- tcGenericNormalizeFamInst fun ty'
		 ; let fix_coi = mkTransCoI first_coi rest_coi
	   	 ; return (fix_coi, nty)
		 }
	    -- no matching family instance exists
	    -- we do not do anything
	    Nothing -> return (tycon_coi, TyConApp tyCon ntys)
	}
tcGenericNormalizeFamInst fun (AppTy ty1 ty2)
  = do	{ (coi1,nty1) <- tcGenericNormalizeFamInst fun ty1
	; (coi2,nty2) <- tcGenericNormalizeFamInst fun ty2
	; return (mkAppTyCoI nty1 coi1 nty2 coi2, AppTy nty1 nty2)
	}
tcGenericNormalizeFamInst fun (FunTy ty1 ty2)
  = do	{ (coi1,nty1) <- tcGenericNormalizeFamInst fun ty1
	; (coi2,nty2) <- tcGenericNormalizeFamInst fun ty2
	; return (mkFunTyCoI nty1 coi1 nty2 coi2, FunTy nty1 nty2)
	}
tcGenericNormalizeFamInst fun (ForAllTy tyvar ty1)
  = do 	{ (coi,nty1) <- tcGenericNormalizeFamInst fun ty1
	; return (mkForAllTyCoI tyvar coi,ForAllTy tyvar nty1)
	}
tcGenericNormalizeFamInst fun (NoteTy note ty1)
  = do	{ (coi,nty1) <- tcGenericNormalizeFamInst fun ty1
	; return (mkNoteTyCoI note coi,NoteTy note nty1)
	}
tcGenericNormalizeFamInst fun ty@(TyVarTy tv)
  | isTcTyVar tv
  = do	{ traceTc (text "tcGenericNormalizeFamInst" <+> ppr ty)
	; res <- lookupTcTyVar tv
	; case res of
	    DoneTv _ -> 
	      do { maybe_ty' <- fun ty
		 ; case maybe_ty' of
		     Nothing	     -> return (IdCo, ty)
		     Just (ty', co1) -> 
                       do { (coi2, ty'') <- tcGenericNormalizeFamInst fun ty'
			  ; return (ACo co1 `mkTransCoI` coi2, ty'') 
			  }
		 }
	    IndirectTv ty' -> tcGenericNormalizeFamInst fun ty' 
	}
  | otherwise
  = return (IdCo, ty)
tcGenericNormalizeFamInst fun (PredTy predty)
  = do 	{ (coi, pred') <- tcGenericNormalizeFamInstPred fun predty
	; return (coi, PredTy pred') }

---------------------------------
tcGenericNormalizeFamInstPred :: (TcType -> TcM (Maybe (TcType,Coercion)))
	                      -> TcPredType
	                      -> TcM (CoercionI, TcPredType)

tcGenericNormalizeFamInstPred fun (ClassP cls tys) 
  = do { (cois, tys')<- mapAndUnzipM (tcGenericNormalizeFamInst fun) tys
       ; return (mkClassPPredCoI cls tys' cois, ClassP cls tys')
       }
tcGenericNormalizeFamInstPred fun (IParam ipn ty) 
  = do { (coi, ty') <- tcGenericNormalizeFamInst fun ty
       ; return $ (mkIParamPredCoI ipn coi, IParam ipn ty')
       }
tcGenericNormalizeFamInstPred fun (EqPred ty1 ty2) 
  = do { (coi1, ty1') <- tcGenericNormalizeFamInst fun ty1
       ; (coi2, ty2') <- tcGenericNormalizeFamInst fun ty2
       ; return (mkEqPredCoI ty1' coi1 ty2' coi2, EqPred ty1' ty2') }
\end{code}


%************************************************************************
%*									*
\section{Normalisation of Given Dictionaries}
%*									*
%************************************************************************

\begin{code}
normaliseGivenDicts, normaliseWantedDicts
	:: [Inst] 		-- given equations
	-> [Inst]		-- dictionaries
	-> TcM ([Inst],TcDictBinds)

normaliseGivenDicts  eqs dicts = normalise_dicts eqs dicts False
normaliseWantedDicts eqs dicts = normalise_dicts eqs dicts True

normalise_dicts
	:: [Inst]	-- given equations
	-> [Inst]	-- dictionaries
	-> Bool		-- True <=> the dicts are wanted 
			-- Fals <=> they are given
	-> TcM ([Inst],TcDictBinds)
normalise_dicts given_eqs dicts is_wanted
  = do	{ traceTc $ text "normaliseGivenDicts <-" <+> ppr dicts <+> 
                    text "with" <+> ppr given_eqs
	; (dicts0, binds0)  <- normaliseInsts is_wanted dicts
	; (dicts1, binds1)  <- substEqInDictInsts given_eqs dicts0
	; let binds01 = binds0 `unionBags` binds1
	; if isEmptyBag binds1
	  then return (dicts1, binds01)
	  else do { (dicts2, binds2) <- normaliseGivenDicts given_eqs dicts1
		  ; return (dicts2, binds01 `unionBags` binds2) } }
\end{code}


%************************************************************************
%*									*
\section{Normalisation of wanteds constraints}
%*									*
%************************************************************************

\begin{code}
normaliseWanteds :: [Inst] -> TcM [Inst]
normaliseWanteds insts 
  = do { traceTc (text "normaliseWanteds <-" <+> ppr insts)
       ; result <- liftM fst $ rewriteToFixedPoint Nothing
		     [ ("(Occurs)",  noChange  $ occursCheckInsts)
		     , ("(ZONK)",    dontRerun $ zonkInsts)
		     , ("(TRIVIAL)", trivialInsts)
		     -- no `swapInsts'; it messes up error messages and should
                     -- not be necessary -=chak
		     , ("(DECOMP)",  decompInsts)
		     , ("(TOP)",     topInsts)
		     , ("(SUBST)",   substInsts)
		     , ("(UNIFY)",   unifyInsts)
                     ] insts
       ; traceTc (text "normaliseWanteds ->" <+> ppr result)
       ; return result
       }
\end{code}


%************************************************************************
%*									*
\section{Normalisation of givens constraints}
%*									*
%************************************************************************

\begin{code}
normaliseGivens :: [Inst] -> TcM ([Inst], TcM ())
normaliseGivens givens
 = do { traceTc (text "normaliseGivens <-" <+> ppr givens)
      ; (result, deSkolem) <- 
          rewriteToFixedPoint (Just ("(SkolemOccurs)", skolemOccurs))
	    [ ("(Occurs)",  noChange  $ occursCheckInsts)
	    , ("(ZONK)",    dontRerun $ zonkInsts)
	    , ("(TRIVIAL)", trivialInsts)
	    , ("(SWAP)",    swapInsts)
	    , ("(DECOMP)",  decompInsts)
	    , ("(TOP)",	    topInsts)
	    , ("(SUBST)",   substInsts)
            ] givens
      ; traceTc (text "normaliseGivens ->" <+> ppr result)
      ; return (result, deSkolem)
      }

-- An explanation of what this does would be helpful! -=chak
skolemOccurs :: PrecondRule
skolemOccurs [] = return ([], return ())
skolemOccurs (inst@(EqInst {}):insts) 
	= do { (insts',actions) <- skolemOccurs insts
	       -- check whether the current inst  co :: ty1 ~ ty2  suffers 
	       -- from the occurs check issue: F ty1 \in ty2
	      ; let occurs = go False ty2
	      ; if occurs
                  then 
	 	      -- if it does generate two new coercions:
		      do { skolem_var <- newMetaTyVar TauTv (typeKind ty1)
			 ; let skolem_ty = TyVarTy skolem_var
		      --    ty1    :: ty1 ~ b
			 ; inst1 <- mkEqInst (EqPred ty1 skolem_ty) (mkGivenCo ty1)
		      --    sym co :: ty2 ~ b
			 ; inst2 <- mkEqInst (EqPred ty2 skolem_ty) (mkGivenCo $ fromACo $ mkSymCoI $ ACo $ fromGivenCo co)
		      -- to replace the old one
		      -- the corresponding action is
		      --    b := ty1
			 ; let action = writeMetaTyVar skolem_var ty1
	     	         ; return (inst1:inst2:insts', action >> actions)
		         }
		  else 
	     	      return (inst:insts', actions)
	     }
	where 
		ty1 = eqInstLeftTy inst
		ty2 = eqInstRightTy inst
		co  = eqInstCoercion inst
		check :: Bool -> TcType -> Bool
		check flag ty 
			= if flag && ty1 `tcEqType` ty
				then True
				else go flag ty		

		go flag (TyConApp con tys)	= or $ map (check (isOpenSynTyCon con || flag)) tys
		go flag (FunTy arg res)	= or $ map (check flag) [arg,res]
		go flag (AppTy fun arg)		= or $ map (check flag) [fun,arg]
		go _    _ 			= False
skolemOccurs _ = panic "TcTyFuns.skolemOccurs: not EqInst"
\end{code}


%************************************************************************
%*									*
\section{Solving of wanted constraints with respect to a given set}
%*									*
%************************************************************************

\begin{code}
solveWanteds :: [Inst]          -- givens
	     -> [Inst] 	        -- wanteds
	     -> TcM [Inst]	-- irreducible wanteds
solveWanteds givens wanteds 
  = do { traceTc $ text "solveWanteds <-" <+> ppr wanteds <+> text "with" <+> 
                   ppr givens
       ; result <- liftM fst $ rewriteToFixedPoint Nothing
                     [ ("(Occurs)",  noChange $ occursCheckInsts)
                     , ("(TRIVIAL)", trivialInsts)
                     , ("(DECOMP)",  decompInsts)
                     , ("(TOP)",     topInsts)
                     , ("(GIVEN)",   givenInsts givens)
                     , ("(UNIFY)",   unifyInsts)
                     ] wanteds
       ; traceTc (text "solveWanteds ->" <+> ppr result)
       ; return result
       }
  where
    -- Use `substInst' with every given on all the wanteds.
    givenInsts :: [Inst] -> [Inst] -> TcM ([Inst],Bool)		 
    givenInsts []     wanteds = return (wanteds,False)
    givenInsts (g:gs) wanteds
      = do { (wanteds1, changed1) <- givenInsts gs wanteds
	   ; (wanteds2, changed2) <- substInst g wanteds1
	   ; return (wanteds2, changed1 || changed2)
	   }
\end{code}


%************************************************************************
%*									*
\section{Normalisation rules and iterative rule application}
%*									*
%************************************************************************

We have four kinds of normalising rewrite rules:

(1) Normalisation rules that rewrite a set of insts and return a flag indicating
    whether any changes occurred during rewriting that necessitate re-running
    the current rule set.

(2) Precondition rules that rewrite a set of insts and return a monadic action
    that reverts the effect of preconditioning.

(3) Idempotent normalisation rules that never require re-running the rule set. 

(4) Checking rule that does not alter the set of insts. 

\begin{code}
type RewriteRule     = [Inst] -> TcM ([Inst], Bool)   -- rewrite, maybe re-run
type PrecondRule     = [Inst] -> TcM ([Inst], TcM ()) -- rewrite, revertable
type IdemRewriteRule = [Inst] -> TcM [Inst]           -- rewrite, don't re-run
type CheckRule       = [Inst] -> TcM ()               -- check

type NamedRule       = (String, RewriteRule)          -- rule with description
type NamedPreRule    = (String, PrecondRule)          -- precond with desc
\end{code}

Templates lifting idempotent and checking rules to full rules (which can be put
into a rule set).

\begin{code}
dontRerun :: IdemRewriteRule -> RewriteRule
dontRerun rule insts = liftM addFalse $ rule insts
  where
    addFalse x = (x, False)

noChange :: CheckRule -> RewriteRule
noChange rule insts = rule insts >> return (insts, False)
\end{code}

The following function applies a set of rewrite rules until a fixed point is
reached; i.e., none of the `RewriteRule's require re-running the rule set.
Optionally, there may be a pre-conditing rule that is applied before any other
rules are applied and before the rule set is re-run.

The result is the set of rewritten (i.e., normalised) insts and, in case of a
pre-conditing rule, a monadic action that reverts the effects of
pre-conditioning - specifically, this is removing introduced skolems.

\begin{code}
rewriteToFixedPoint :: Maybe NamedPreRule   -- optional preconditioning rule
                    -> [NamedRule]          -- rule set
                    -> [Inst]               -- insts to rewrite
                    -> TcM ([Inst], TcM ())
rewriteToFixedPoint precondRule rules insts
  = completeRewrite (return ()) precondRule insts
  where
    completeRewrite :: TcM () -> Maybe NamedPreRule -> [Inst] 
                    -> TcM ([Inst], TcM ())
    completeRewrite dePrecond (Just (precondName, precond)) insts
      = do { (insts', dePrecond') <- precond insts
           ; traceTc $ text precondName <+> ppr insts'
           ; tryRules (dePrecond >> dePrecond') rules insts'
           }
    completeRewrite dePrecond Nothing insts
      = tryRules dePrecond rules insts

    tryRules dePrecond _                    []    = return ([]   , dePrecond)
    tryRules dePrecond []                   insts = return (insts, dePrecond)
    tryRules dePrecond ((name, rule):rules) insts 
      = do { (insts', rerun) <- rule insts
           ; traceTc $ text name <+> ppr insts'
	   ; if rerun then completeRewrite dePrecond precondRule insts'
		      else tryRules dePrecond rules insts'
           }
\end{code}


%************************************************************************
%*									*
\section{Different forms of Inst rewritings}
%*									*
%************************************************************************

Rewrite schemata applied by way of eq_rewrite and friends.

\begin{code}

	-- (Trivial)
	--	g1 : t ~ t
	--		>-->
	--	g1 := t
	--
trivialInsts :: RewriteRule
trivialInsts []
	= return ([],False)
trivialInsts (i@(EqInst {}):is)
	= do { (is',changed)<- trivialInsts is
	     ; if tcEqType ty1 ty2
		  then do { eitherEqInst i 
				(\covar -> writeMetaTyVar covar ty1) 
			        (\_     -> return ())
			  ; return (is',True)
			  }
		  else return (i:is',changed)
	     }
	where
	   ty1 = eqInstLeftTy i
	   ty2 = eqInstRightTy i
trivialInsts _ = panic "TcTyFuns.trivialInsts: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
swapInsts :: RewriteRule
-- All the inputs and outputs are equalities
swapInsts insts 
  = do { (insts', changeds) <- mapAndUnzipM swapInst insts
       ; return (insts', or changeds)
       }

	-- (Swap)
	-- 	g1 : c ~ Fd
	--		>-->
	--	g2 : Fd ~ c
	--	g1 := sym g2
	--
        -- This is not all, is it?  Td ~ c is also rewritten to c ~ Td!
swapInst :: Inst -> TcM (Inst, Bool)
swapInst i@(EqInst {})
	= go ty1 ty2
	where
	      ty1 = eqInstLeftTy i
	      ty2 = eqInstRightTy i
              go ty1 ty2		| Just ty1' <- tcView ty1 
		     			= go ty1' ty2 
              go ty1 ty2		| Just ty2' <- tcView ty2
		     			= go ty1 ty2' 
	      go (TyConApp tyCon _) _	| isOpenSynTyCon tyCon
					= return (i,False)
		-- we should swap!
	      go ty1 ty2@(TyConApp tyCon _) 
					| isOpenSynTyCon tyCon
					= actual_swap ty1 ty2
	      go ty1@(TyConApp _ _) ty2@(TyVarTy _)
					= actual_swap ty1 ty2
	      go _ _			= return (i,False)

	      actual_swap ty1 ty2 = do { wg_co <- eitherEqInst i
						          -- old_co := sym new_co
						          (\old_covar ->
							   do { new_cotv <- newMetaTyVar TauTv (mkCoKind ty2 ty1)
						              ; let new_co = TyVarTy new_cotv
							      ; writeMetaTyVar old_covar (mkCoercion symCoercionTyCon [new_co])
							      ; return $ mkWantedCo new_cotv
							      })
						          -- new_co := sym old_co
					   	          (\old_co -> return $ mkGivenCo $ mkCoercion symCoercionTyCon [old_co])
					     ; new_inst <- mkEqInst (EqPred ty2 ty1) wg_co
					     ; return (new_inst,True)
					     }
swapInst _ = panic "TcTyFuns.swapInst: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompInsts :: RewriteRule
decompInsts insts = do { (insts,bs) <- mapAndUnzipM decompInst insts
		       ; return (concat insts,or bs)
		       }

	-- (Decomp)
	-- 	g1 : T cs ~ T ds
	--		>-->
	--	g21 : c1 ~ d1, ..., g2n : cn ~ dn
	--	g1 := T g2s
	--
	--  Works also for the case where T is actually an application of a 
        --  type family constructor to a set of types, provided the 
        --  applications on both sides of the ~ are identical;
        --  see also Note [OpenSynTyCon app] in TcUnify
	--
decompInst :: Inst -> TcM ([Inst],Bool)
decompInst i@(EqInst {})
  = go ty1 ty2
  where 
    ty1 = eqInstLeftTy i
    ty2 = eqInstRightTy i
    go ty1 ty2		
      | Just ty1' <- tcView ty1 = go ty1' ty2 
      | Just ty2' <- tcView ty2 = go ty1 ty2' 

    go ty1@(TyConApp con1 tys1) ty2@(TyConApp con2 tys2)
      | con1 == con2 && identicalHead
      = do { cos <- eitherEqInst i
                      -- old_co := Con1 cos
                      (\old_covar ->
                        do { cotvs <- zipWithM (\t1 t2 -> 
                                                newMetaTyVar TauTv 
                                                             (mkCoKind t1 t2)) 
                                               tys1 tys2
                           ; let cos = map TyVarTy cotvs
                           ; writeMetaTyVar old_covar (TyConApp con1 cos)
                           ; return $ map mkWantedCo cotvs
                           })
                      -- co_i := Con_i old_co
                      (\old_co -> return $ 
                                    map mkGivenCo $
                                        mkRightCoercions (length tys1) old_co)
           ; insts <- zipWithM mkEqInst (zipWith EqPred tys1 tys2) cos
           ; traceTc (text "decomp identicalHead" <+> ppr insts) 
           ; return (insts, not $ null insts) 
           }
      | con1 /= con2 && not (isOpenSynTyCon con1 || isOpenSynTyCon con2)
        -- not matching data constructors (of any flavour) are bad news
      = do { env0 <- tcInitTidyEnv
           ; let (env1, tidy_ty1) = tidyOpenType env0 ty1
                 (env2, tidy_ty2) = tidyOpenType env1 ty2
                 extra 	          = sep [ppr tidy_ty1, char '~', ppr tidy_ty2]
                 msg 		  = 
                   ptext SLIT("Unsolvable equality constraint:")
           ; failWithTcM (env2, hang msg 2 extra)
           }
      where
        n             = tyConArity con1
        (idxTys1, _)  = splitAt n tys1
        (idxTys2, _)  = splitAt n tys2
        identicalHead = not (isOpenSynTyCon con1) ||
                        idxTys1 `tcEqTypes` idxTys2

    go _ _ = return ([i], False)
decompInst _ = panic "TcTyFuns.decompInst: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
topInsts :: RewriteRule
topInsts insts 
	=  do { (insts,bs) <- mapAndUnzipM topInst insts
	      ; return (insts,or bs)
	      }

	-- (Top)
	-- 	g1 : t ~ s
	--		>--> co1 :: t ~ t' / co2 :: s ~ s'
	--	g2 : t' ~ s'
	--	g1 := co1 * g2 * sym co2
topInst :: Inst -> TcM (Inst,Bool)
topInst i@(EqInst {})
	= do { (coi1,ty1') <- tcNormalizeFamInst ty1
	     ; (coi2,ty2') <- tcNormalizeFamInst ty2
	     ; case (coi1,coi2) of
		(IdCo,IdCo) -> 
		  return (i,False)
		_           -> 
		 do { wg_co <- eitherEqInst i
			         -- old_co = co1 * new_co * sym co2
			         (\old_covar ->
                                  do { new_cotv <- newMetaTyVar TauTv (mkCoKind ty1 ty2)
			             ; let new_co = TyVarTy new_cotv
				     ; let old_coi = coi1 `mkTransCoI` ACo new_co `mkTransCoI` (mkSymCoI coi2)
				     ; writeMetaTyVar old_covar (fromACo old_coi)
			             ; return $ mkWantedCo new_cotv
				     })
				 -- new_co = sym co1 * old_co * co2
			         (\old_co -> return $ mkGivenCo $ fromACo $ mkSymCoI coi1 `mkTransCoI` ACo old_co `mkTransCoI` coi2)	
		    ; new_inst <- mkEqInst (EqPred ty1' ty2') wg_co 
		    ; return (new_inst,True)
		    }
	     }
	where
	      ty1 = eqInstLeftTy i
	      ty2 = eqInstRightTy i
topInst _ = panic "TcTyFuns.topInsts: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
substInsts :: RewriteRule
substInsts insts = substInstsWorker insts []

substInstsWorker :: [Inst] -> [Inst] -> TcM ([Inst],Bool)
substInstsWorker [] acc 
	= return (acc,False)
substInstsWorker (i:is) acc 
	| (TyConApp con _) <- tci_left i, isOpenSynTyCon con
	= do { (is',change) <- substInst i (acc ++ is)
	     ; if change 
		  then return ((i:is'),True)
                  else substInstsWorker is (i:acc)
	     }
	| otherwise
	= substInstsWorker is (i:acc)
		
	-- (Subst)
	--	g : F c ~ t,
	--	forall g1 : s1{F c} ~ s2{F c}
	--		>-->
	--	g2 : s1{t} ~ s2{t}
	--	g1 := s1{g} * g2  * sym s2{g}		<=> 	g2 := sym s1{g} * g1 * s2{g}
substInst :: Inst -> [Inst] -> TcM ([Inst], Bool)
substInst _inst [] 
	= return ([],False)
substInst inst@(EqInst {tci_left = pattern, tci_right = target}) (i@(EqInst {tci_left = ty1, tci_right = ty2}):is)			
	= do { (is',changed) <- substInst inst is
	     ; (coi1,ty1')   <- tcGenericNormalizeFamInst fun ty1
	     ; (coi2,ty2')   <- tcGenericNormalizeFamInst fun ty2
	     ; case (coi1,coi2) of
		(IdCo,IdCo) -> 
		  return (i:is',changed)
		_           -> 
		  do { gw_co <- eitherEqInst i
			          -- old_co := co1 * new_co * sym co2
			          (\old_covar ->
				   do { new_cotv <- newMetaTyVar TauTv (mkCoKind ty1' ty2')
			              ; let new_co = TyVarTy new_cotv
				      ; let old_coi = coi1 `mkTransCoI` ACo new_co `mkTransCoI` (mkSymCoI coi2)
				      ; writeMetaTyVar old_covar (fromACo old_coi)
			              ; return $ mkWantedCo new_cotv
				      })
			          -- new_co := sym co1 * old_co * co2
			          (\old_co -> return $ mkGivenCo $ fromACo $ (mkSymCoI coi1) `mkTransCoI` ACo old_co `mkTransCoI` coi2)
	             ; new_inst <- mkEqInst (EqPred ty1' ty2') gw_co
		     ; return (new_inst:is',True)
		     }
	     }
	where fun ty = return $ if tcEqType pattern ty then Just (target,coercion) else Nothing

	      coercion = eitherEqInst inst TyVarTy id
substInst _ _ = panic "TcTyFuns.substInst: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unifyInsts :: RewriteRule
unifyInsts insts 
	= do { (insts',changeds) <- mapAndUnzipM unifyInst insts
	     ; return (concat insts',or changeds)
	     }

	-- (UnifyMeta)
	--	g : alpha ~ t
	--		>-->
	--	alpha := t
	--	g     := t
	--
	--  TOMDO: you should only do this for certain `meta' type variables
unifyInst :: Inst -> TcM ([Inst], Bool)
unifyInst i@(EqInst {tci_left = ty1, tci_right = ty2})
	| TyVarTy tv1 <- ty1, isMetaTyVar tv1 	= go ty2 tv1
	| TyVarTy tv2 <- ty2, isMetaTyVar tv2	= go ty1 tv2 	
	| otherwise				= return ([i],False) 
	where go ty tv
		= do { let cotv = fromWantedCo "unifyInst" $ eqInstCoercion i
		     ; writeMetaTyVar tv   ty	--	alpha := t
		     ; writeMetaTyVar cotv ty	--	g     := t
	             ; return ([],True)
		     }
unifyInst _ = panic "TcTyFuns.unifyInst: not EqInst"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
occursCheckInsts :: CheckRule
occursCheckInsts insts = mappM_ occursCheckInst insts


	-- (OccursCheck)
	--	t ~ s[T t]
	--		>-->
	--	fail
	--
occursCheckInst :: Inst -> TcM () 
occursCheckInst (EqInst {tci_left = ty1, tci_right = ty2})
	= go ty2 
	where
		check ty = if ty `tcEqType` ty1
			      then occursError 
			      else go ty

		go (TyConApp con tys)	= if isOpenSynTyCon con then return () else mappM_ check tys
		go (FunTy arg res)	= mappM_ check [arg,res]
		go (AppTy fun arg)	= mappM_ check [fun,arg]
		go _			= return ()

		occursError 		= do { env0 <- tcInitTidyEnv
					     ; let (env1, tidy_ty1)  =  tidyOpenType env0 ty1
					           (env2, tidy_ty2)  =  tidyOpenType env1 ty2
					           extra = sep [ppr tidy_ty1, char '~', ppr tidy_ty2]
					     ; failWithTcM (env2, hang msg 2 extra)
					     }
					where msg = ptext SLIT("Occurs check: cannot construct the infinite type")
occursCheckInst _ = panic "TcTyFuns.occursCheckInst: not eqInst"
\end{code}

Normalises a set of dictionaries relative to a set of given equalities (which
are interpreted as rewrite rules).  We only consider given equalities of the
form

  F ts ~ t

where F is a type family.

\begin{code}
substEqInDictInsts :: [Inst]    -- given equalities (used as rewrite rules)
                   -> [Inst]    -- dictinaries to be normalised
                   -> TcM ([Inst], TcDictBinds)
substEqInDictInsts eq_insts insts 
  = do { traceTc (text "substEqInDictInst <-" <+> ppr insts)
       ; result <- foldlM rewriteWithOneEquality (insts, emptyBag) eq_insts
       ; traceTc (text "substEqInDictInst ->" <+> ppr result)
       ; return result
       }
  where
      -- (1) Given equality of form 'F ts ~ t': use for rewriting
    rewriteWithOneEquality (insts, dictBinds)
                           inst@(EqInst {tci_left  = pattern, 
                                         tci_right = target})
      | isOpenSynTyConApp pattern
      = do { (insts', moreDictBinds) <- genericNormaliseInsts True {- wanted -}
                                                              applyThisEq insts
           ; return (insts', dictBinds `unionBags` moreDictBinds)
           }
      where
        applyThisEq = tcGenericNormalizeFamInstPred (return . matchResult)

        -- rewrite in case of an exact match
        matchResult ty | tcEqType pattern ty = Just (target, eqInstType inst)
                       | otherwise           = Nothing

      -- (2) Given equality has the wrong form: ignore
    rewriteWithOneEquality (insts, dictBinds) _not_a_rewrite_rule
      = return (insts, dictBinds)
\end{code}

%************************************************************************
%*									*
	Normalisation of Insts
%*									*
%************************************************************************

Take a bunch of Insts (not EqInsts), and normalise them wrt the top-level
type-function equations, where

	(norm_insts, binds) = normaliseInsts is_wanted insts

If 'is_wanted'
  = True,  (binds + norm_insts) defines insts       (wanteds)
  = False, (binds + insts)      defines norm_insts  (givens)

\begin{code}
normaliseInsts :: Bool   	  		-- True <=> wanted insts
	       -> [Inst] 			-- wanted or given insts 
	       -> TcM ([Inst], TcDictBinds) 	-- normalized insts and bindings
normaliseInsts isWanted insts 
  = genericNormaliseInsts isWanted tcNormalizeFamInstPred insts

genericNormaliseInsts  :: Bool          	    -- True <=> wanted insts
	               -> (TcPredType -> TcM (CoercionI, TcPredType))  
                                                    -- how to normalise
	               -> [Inst]      		    -- wanted or given insts 
	               -> TcM ([Inst], TcDictBinds) -- normalized insts & binds
genericNormaliseInsts isWanted fun insts
  = do { (insts', binds) <- mapAndUnzipM (normaliseOneInst isWanted fun) insts
       ; return (insts', unionManyBags binds)
       }
  where
    normaliseOneInst isWanted fun
	             dict@(Dict {tci_pred = pred,
                                 tci_loc  = loc})
      = do { traceTc (text "genericNormaliseInst 1")
	   ; (coi, pred') <- fun pred
   	   ; traceTc (text "genericNormaliseInst 2")

	   ; case coi of
	       IdCo   -> return (dict, emptyBag)
                         -- don't use pred' in this case; otherwise, we get
                         -- more unfolded closed type synonyms in error messages
	       ACo co -> 
                 do { -- an inst for the new pred
		    ; dict' <- newDictBndr loc pred'
		      -- relate the old inst to the new one
		      -- target_dict = source_dict `cast` st_co
		    ; let (target_dict, source_dict, st_co) 
			    | isWanted  = (dict,  dict', mkSymCoercion co)
			    | otherwise = (dict', dict,  co)
			      -- if isWanted
			      -- 	co :: dict ~ dict'
			      -- 	hence dict = dict' `cast` sym co
			      -- else
			      -- 	co :: dict ~ dict'
			      -- 	hence dict' = dict `cast` co
		          expr      = HsVar $ instToId source_dict
		          cast_expr = HsWrap (WpCo st_co) expr
			  rhs       = L (instLocSpan loc) cast_expr
			  binds     = mkBind target_dict rhs
		      -- return the new inst
		    ; return (dict', binds)
		    }
	   }
	
	-- TOMDO: treat other insts appropriately
    normaliseOneInst _isWanted _fun inst
      = do { inst' <- zonkInst inst
	   ; return (inst', emptyBag)
	   }

addBind :: Bag (LHsBind TcId) -> Inst -> LHsExpr TcId -> Bag (LHsBind TcId)
addBind binds inst rhs = binds `unionBags` mkBind inst rhs

mkBind :: Inst -> LHsExpr TcId -> Bag (LHsBind TcId)
mkBind inst rhs = unitBag (L (instSpan inst) 
			  (VarBind (instToId inst) rhs))
\end{code}
