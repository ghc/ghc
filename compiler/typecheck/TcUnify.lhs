%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Type subsumption and unification

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcUnify (
	-- Full-blown subsumption
  tcSubExp, tcFunResTy, tcGen, 
  checkSigTyVars, checkSigTyVarsWrt, bleatEscapedTvs, sigCtxt, 

	-- Various unifications
  unifyType, unifyTypeList, unifyTheta,
  unifyKind, unifyKinds, unifyFunKind, 
  checkExpectedKind, 
  preSubType, boxyMatchTypes, 

  --------------------------------
  -- Holes
  tcInfer, subFunTys, unBox, refineBox, refineBoxToTau, withBox, 
  boxyUnify, boxyUnifyList, zapToMonotype,
  boxySplitListTy, boxySplitPArrTy, boxySplitTyConApp, boxySplitAppTy,
  wrapFunResCoercion
  ) where

#include "HsVersions.h"

import HsSyn
import TypeRep

import TcMType
import TcSimplify
import TcEnv
import TcTyFuns
import TcIface
import TcRnMonad         -- TcType, amongst others
import TcType
import Type
import Coercion
import TysPrim
import Inst
import TyCon
import TysWiredIn
import Var
import VarSet
import VarEnv
import Name
import ErrUtils
import Maybes
import BasicTypes
import Util
import Outputable
import Unique
\end{code}

%************************************************************************
%*									*
\subsection{'hole' type variables}
%*									*
%************************************************************************

\begin{code}
tcInfer :: (BoxyType -> TcM a) -> TcM (a, TcType)
tcInfer tc_infer
  = do	{ box <- newBoxyTyVar openTypeKind
	; res <- tc_infer (mkTyVarTy box)
	; res_ty <- {- pprTrace "tcInfer" (ppr (mkTyVarTy box)) $ -} readFilledBox box	-- Guaranteed filled-in by now
	; return (res, res_ty) }
\end{code}


%************************************************************************
%*									*
	subFunTys
%*									*
%************************************************************************

\begin{code}
subFunTys :: SDoc  -- Somthing like "The function f has 3 arguments"
		   -- or "The abstraction (\x.e) takes 1 argument"
	  -> Arity		-- Expected # of args
	  -> BoxyRhoType	-- res_ty
	  -> ([BoxySigmaType] -> BoxyRhoType -> TcM a)
	  -> TcM (HsWrapper, a)
-- Attempt to decompse res_ty to have enough top-level arrows to
-- match the number of patterns in the match group
-- 
-- If (subFunTys n_args res_ty thing_inside) = (co_fn, res)
-- and the inner call to thing_inside passes args: [a1,...,an], b
-- then co_fn :: (a1 -> ... -> an -> b) ~ res_ty
--
-- Note that it takes a BoxyRho type, and guarantees to return a BoxyRhoType


{-	Error messages from subFunTys

   The abstraction `\Just 1 -> ...' has two arguments
   but its type `Maybe a -> a' has only one

   The equation(s) for `f' have two arguments
   but its type `Maybe a -> a' has only one

   The section `(f 3)' requires 'f' to take two arguments
   but its type `Int -> Int' has only one

   The function 'f' is applied to two arguments
   but its type `Int -> Int' has only one
-}


subFunTys error_herald n_pats res_ty thing_inside
  = loop n_pats [] res_ty
  where
	-- In 'loop', the parameter 'arg_tys' accumulates 
	-- the arg types so far, in *reverse order*
	-- INVARIANT:   res_ty :: *
    loop n args_so_far res_ty
	| Just res_ty' <- tcView res_ty  = loop n args_so_far res_ty'

    loop n args_so_far res_ty
	| isSigmaTy res_ty 	-- Do this before checking n==0, because we 
				-- guarantee to return a BoxyRhoType, not a BoxySigmaType
	= do { (gen_fn, (co_fn, res)) <- tcGen res_ty emptyVarSet $ \ _ res_ty' ->
					 loop n args_so_far res_ty'
	     ; return (gen_fn <.> co_fn, res) }

    loop 0 args_so_far res_ty 
	= do { res <- thing_inside (reverse args_so_far) res_ty
	     ; return (idHsWrapper, res) }

    loop n args_so_far (FunTy arg_ty res_ty) 
	= do { (co_fn, res) <- loop (n-1) (arg_ty:args_so_far) res_ty
	     ; co_fn' <- wrapFunResCoercion [arg_ty] co_fn
	     ; return (co_fn', res) }

	-- res_ty might have a type variable at the head, such as (a b c),
	-- in which case we must fill in with (->).  Simplest thing to do
	-- is to use boxyUnify, but we catch failure and generate our own
	-- error message on failure
    loop n args_so_far res_ty@(AppTy _ _)
	= do { [arg_ty',res_ty'] <- newBoxyTyVarTys [argTypeKind, openTypeKind]
	     ; (_, mb_coi) <- tryTcErrs $ boxyUnify res_ty (FunTy arg_ty' res_ty')
	     ; if isNothing mb_coi then bale_out args_so_far
	       else do { case expectJust "subFunTys" mb_coi of
				IdCo -> return ()
				ACo co -> traceTc (text "you're dropping a coercion: " <+> ppr co)
		       ; loop n args_so_far (FunTy arg_ty' res_ty') 
		       }
	     }

    loop n args_so_far (TyVarTy tv)
        | isTyConableTyVar tv
	= do { cts <- readMetaTyVar tv 
	     ; case cts of
		 Indirect ty -> loop n args_so_far ty
		 Flexi -> do { (res_ty:arg_tys) <- withMetaTvs tv kinds mk_res_ty
			     ; res <- thing_inside (reverse args_so_far ++ arg_tys) res_ty
			     ; return (idHsWrapper, res) } }
	where
	  mk_res_ty (res_ty' : arg_tys') = mkFunTys arg_tys' res_ty'
	  mk_res_ty [] = panic "TcUnify.mk_res_ty1"
	  kinds = openTypeKind : take n (repeat argTypeKind)
		-- Note argTypeKind: the args can have an unboxed type,
		-- but not an unboxed tuple.

    loop n args_so_far res_ty = bale_out args_so_far

    bale_out args_so_far 
	= do { env0 <- tcInitTidyEnv
	     ; res_ty' <- zonkTcType res_ty
	     ; let (env1, res_ty'') = tidyOpenType env0 res_ty'
	     ; failWithTcM (env1, mk_msg res_ty'' (length args_so_far)) }

    mk_msg res_ty n_actual 
      = error_herald <> comma $$ 
	sep [ptext SLIT("but its type") <+> quotes (pprType res_ty), 
	     if n_actual == 0 then ptext SLIT("has none") 
	     else ptext SLIT("has only") <+> speakN n_actual]
\end{code}

\begin{code}
----------------------
boxySplitTyConApp :: TyCon			-- T :: k1 -> ... -> kn -> *
	          -> BoxyRhoType 		-- Expected type (T a b c)
		  -> TcM ([BoxySigmaType],	-- Element types, a b c
                          CoercionI)
  -- It's used for wired-in tycons, so we call checkWiredInTyCon
  -- Precondition: never called with FunTyCon
  -- Precondition: input type :: *

boxySplitTyConApp tc orig_ty
  = do	{ checkWiredInTyCon tc 
	; loop (tyConArity tc) [] orig_ty }
  where
    loop n_req args_so_far ty 
      | Just ty' <- tcView ty = loop n_req args_so_far ty'

    loop n_req args_so_far ty@(TyConApp tycon args)
      | tc == tycon
      = ASSERT( n_req == length args) 	-- ty::*
	return (args ++ args_so_far, IdCo)

      | isOpenSynTyCon tycon        -- try to normalise type family application
      = do { (coi1, ty') <- tcNormaliseFamInst ty
           ; case coi1 of
	       IdCo   -> defer    -- no progress, but maybe solvable => defer
               ACo _  ->          -- progress: so lets try again
        	 do { (args, coi2) <- loop n_req args_so_far ty'
		    ; return $ (args, coi2 `mkTransCoI` mkSymCoI coi1)
		    }
           }

    loop n_req args_so_far (AppTy fun arg)
      | n_req > 0
      = do { (args, coi) <- loop (n_req - 1) (arg:args_so_far) fun
           ; return (args, mkAppTyCoI fun coi arg IdCo)
           }

    loop n_req args_so_far (TyVarTy tv)
      | isTyConableTyVar tv
      , res_kind `isSubKind` tyVarKind tv
      = do { cts <- readMetaTyVar tv
	   ; case cts of
	       Indirect ty -> loop n_req args_so_far ty
	       Flexi       -> do { arg_tys <- withMetaTvs tv arg_kinds mk_res_ty
				 ; return (arg_tys ++ args_so_far, IdCo) }
	   }
      | otherwise             -- defer as tyvar may be refined by equalities
      = defer
      where
	(arg_kinds, res_kind) = splitKindFunTysN n_req (tyConKind tc)

    loop _ _ _ = boxySplitFailure (mkTyConApp tc (mkTyVarTys (tyConTyVars tc)))
                                  orig_ty

    -- defer splitting by generating an equality constraint
    defer = boxySplitDefer arg_kinds mk_res_ty orig_ty
      where
        (arg_kinds, _) = splitKindFunTys (tyConKind tc)

    -- apply splitted tycon to arguments
    mk_res_ty = mkTyConApp tc

----------------------
boxySplitListTy :: BoxyRhoType -> TcM (BoxySigmaType, CoercionI)
-- Special case for lists
boxySplitListTy exp_ty 
 = do { ([elt_ty], coi) <- boxySplitTyConApp listTyCon exp_ty
      ; return (elt_ty, coi) }

----------------------
boxySplitPArrTy :: BoxyRhoType -> TcM (BoxySigmaType, CoercionI)
-- Special case for parrs
boxySplitPArrTy exp_ty 
  = do { ([elt_ty], coi) <- boxySplitTyConApp parrTyCon exp_ty
       ; return (elt_ty, coi) }

----------------------
boxySplitAppTy :: BoxyRhoType				-- Type to split: m a
	       -> TcM ((BoxySigmaType, BoxySigmaType),	-- Returns m, a
                       CoercionI)
-- If the incoming type is a mutable type variable of kind k, then 
-- boxySplitAppTy returns a new type variable (m: * -> k); note the *.
-- If the incoming type is boxy, then so are the result types; and vice versa

boxySplitAppTy orig_ty
  = loop orig_ty
  where
    loop ty 
      | Just ty' <- tcView ty = loop ty'

    loop ty 
      | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
      = return ((fun_ty, arg_ty), IdCo)

    loop ty@(TyConApp tycon args)
      | isOpenSynTyCon tycon        -- try to normalise type family application
      = do { (coi1, ty') <- tcNormaliseFamInst ty
           ; case coi1 of
	       IdCo   -> defer    -- no progress, but maybe solvable => defer
               ACo co ->          -- progress: so lets try again
        	 do { (args, coi2) <- loop ty'
		    ; return $ (args, coi2 `mkTransCoI` mkSymCoI coi1)
		    }
           }

    loop (TyVarTy tv)
      | isTyConableTyVar tv
      = do { cts <- readMetaTyVar tv
	   ; case cts of
	       Indirect ty -> loop ty
	       Flexi -> do { [fun_ty, arg_ty] <- withMetaTvs tv kinds mk_res_ty
			   ; return ((fun_ty, arg_ty), IdCo) } }
      | otherwise             -- defer as tyvar may be refined by equalities
      = defer
      where
	tv_kind = tyVarKind tv
	kinds = [mkArrowKind liftedTypeKind (defaultKind tv_kind),
					 	-- m :: * -> k
		 liftedTypeKind]		-- arg type :: *
	-- The defaultKind is a bit smelly.  If you remove it,
	-- try compiling	f x = do { x }
	-- and you'll get a kind mis-match.  It smells, but
	-- not enough to lose sleep over.
	
    loop _ = boxySplitFailure (mkAppTy alphaTy betaTy) orig_ty

    -- defer splitting by generating an equality constraint
    defer = do { ([ty1, ty2], coi) <- boxySplitDefer arg_kinds mk_res_ty orig_ty
               ; return ((ty1, ty2), coi)
               }
      where
	orig_kind = typeKind orig_ty
	arg_kinds = [mkArrowKind liftedTypeKind (defaultKind orig_kind),
					 	-- m :: * -> k
		     liftedTypeKind]		-- arg type :: *
 
    -- build type application
    mk_res_ty [fun_ty', arg_ty'] = mkAppTy fun_ty' arg_ty'
    mk_res_ty _other             = panic "TcUnify.mk_res_ty2"

------------------
boxySplitFailure actual_ty expected_ty
  = unifyMisMatch False False actual_ty expected_ty
	-- "outer" is False, so we don't pop the context
	-- which is what we want since we have not pushed one!

------------------
boxySplitDefer :: [Kind]                   -- kinds of required arguments
               -> ([TcType] -> TcTauType)  -- construct lhs from argument tyvars
               -> BoxyRhoType              -- type to split
               -> TcM ([TcType], CoercionI)
boxySplitDefer kinds mkTy orig_ty
  = do { tau_tys <- mapM newFlexiTyVarTy kinds
       ; coi <- defer_unification False False (mkTy tau_tys) orig_ty
       ; return (tau_tys, coi)
       }
\end{code}


--------------------------------
-- withBoxes: the key utility function
--------------------------------

\begin{code}
withMetaTvs :: TcTyVar	-- An unfilled-in, non-skolem, meta type variable
	    -> [Kind] 	-- Make fresh boxes (with the same BoxTv/TauTv setting as tv)
	    -> ([BoxySigmaType] -> BoxySigmaType)
					-- Constructs the type to assign 
					-- to the original var
	    -> TcM [BoxySigmaType]  	-- Return the fresh boxes

-- It's entirely possible for the [kind] to be empty.  
-- For example, when pattern-matching on True, 
-- we call boxySplitTyConApp passing a boolTyCon

-- Invariant: tv is still Flexi

withMetaTvs tv kinds mk_res_ty
  | isBoxyTyVar tv
  = do	{ box_tvs <- mapM (newMetaTyVar BoxTv) kinds
	; let box_tys = mkTyVarTys box_tvs
	; writeMetaTyVar tv (mk_res_ty box_tys)
	; return box_tys }

  | otherwise			-- Non-boxy meta type variable
  = do	{ tau_tys <- mapM newFlexiTyVarTy kinds
	; writeMetaTyVar tv (mk_res_ty tau_tys)	-- Write it *first*
						-- Sure to be a tau-type
	; return tau_tys }

withBox :: Kind -> (BoxySigmaType -> TcM a) -> TcM (a, TcType)
-- Allocate a *boxy* tyvar
withBox kind thing_inside
  = do	{ box_tv <- newMetaTyVar BoxTv kind
	; res <- thing_inside (mkTyVarTy box_tv)
	; ty  <- {- pprTrace "with_box" (ppr (mkTyVarTy box_tv)) $ -} readFilledBox box_tv
  	; return (res, ty) }
\end{code}


%************************************************************************
%*									*
		Approximate boxy matching
%*									*
%************************************************************************

\begin{code}
preSubType :: [TcTyVar]		-- Quantified type variables
	   -> TcTyVarSet	-- Subset of quantified type variables
				--   see Note [Pre-sub boxy]
	    -> TcType		-- The rho-type part; quantified tyvars scopes over this
	    -> BoxySigmaType	-- Matching type from the context
	    -> TcM [TcType]	-- Types to instantiate the tyvars
-- Perform pre-subsumption, and return suitable types
-- to instantiate the quantified type varibles:
--	info from the pre-subsumption, if there is any
--	a boxy type variable otherwise
--
-- Note [Pre-sub boxy]
--   The 'btvs' are a subset of 'qtvs'.  They are the ones we can
--   instantiate to a boxy type variable, because they'll definitely be
--   filled in later.  This isn't always the case; sometimes we have type 
--   variables mentioned in the context of the type, but not the body; 
-- 		  f :: forall a b. C a b => a -> a
--   Then we may land up with an unconstrained 'b', so we want to 
--   instantiate it to a monotype (non-boxy) type variable
--
-- The 'qtvs' that are *neither* fixed by the pre-subsumption, *nor* are in 'btvs',
-- are instantiated to TauTv meta variables.
	
preSubType qtvs btvs qty expected_ty
  = do { tys <- mapM inst_tv qtvs
	; traceTc (text "preSubType" <+> (ppr qtvs $$ ppr btvs $$ ppr qty $$ ppr expected_ty $$ ppr pre_subst $$ ppr tys))
	; return tys }
  where
    pre_subst = boxySubMatchType (mkVarSet qtvs) qty expected_ty
    inst_tv tv	
	| Just boxy_ty <- lookupTyVar pre_subst tv = return boxy_ty
	| tv `elemVarSet` btvs = do { tv' <- tcInstBoxyTyVar tv
				    ; return (mkTyVarTy tv') }
	| otherwise	       = do { tv' <- tcInstTyVar tv
				    ; return (mkTyVarTy tv') }

boxySubMatchType 
	:: TcTyVarSet -> TcType	-- The "template"; the tyvars are skolems
	-> BoxyRhoType		-- Type to match (note a *Rho* type)
	-> TvSubst 		-- Substitution of the [TcTyVar] to BoxySigmaTypes

-- boxySubMatchType implements the Pre-subsumption judgement, in Fig 5 of the paper
-- "Boxy types: inference for higher rank types and impredicativity"

boxySubMatchType tmpl_tvs tmpl_ty boxy_ty
  = go tmpl_tvs tmpl_ty emptyVarSet boxy_ty
  where
    go t_tvs t_ty b_tvs b_ty
	| Just t_ty' <- tcView t_ty = go t_tvs t_ty' b_tvs b_ty
	| Just b_ty' <- tcView b_ty = go t_tvs t_ty b_tvs b_ty'

    go t_tvs (TyVarTy _) b_tvs b_ty = emptyTvSubst	-- Rule S-ANY; no bindings
	-- Rule S-ANY covers (a) type variables and (b) boxy types
	-- in the template.  Both look like a TyVarTy.
	-- See Note [Sub-match] below

    go t_tvs t_ty b_tvs b_ty
	| isSigmaTy t_ty, (tvs, _, t_tau) <- tcSplitSigmaTy t_ty 
	= go (t_tvs `delVarSetList` tvs) t_tau b_tvs b_ty	-- Rule S-SPEC
		-- Under a forall on the left, if there is shadowing, 
		-- do not bind! Hence the delVarSetList.
	| isSigmaTy b_ty, (tvs, _, b_tau) <- tcSplitSigmaTy b_ty 
	= go t_tvs t_ty (extendVarSetList b_tvs tvs) b_tau	-- Rule S-SKOL
		-- Add to the variables we must not bind to
	-- NB: it's *important* to discard the theta part. Otherwise
	-- consider (forall a. Eq a => a -> b) ~<~ (Int -> Int -> Bool)
	-- and end up with a completely bogus binding (b |-> Bool), by lining
	-- up the (Eq a) with the Int, whereas it should be (b |-> (Int->Bool)).  
	-- This pre-subsumption stuff can return too few bindings, but it 
	-- must *never* return bogus info.
							
    go t_tvs (FunTy arg1 res1) b_tvs (FunTy arg2 res2)	-- Rule S-FUN
	= boxy_match t_tvs arg1 b_tvs arg2 (go t_tvs res1 b_tvs res2)
	-- Match the args, and sub-match the results

    go t_tvs t_ty b_tvs b_ty = boxy_match t_tvs t_ty b_tvs b_ty emptyTvSubst
	-- Otherwise defer to boxy matching
	-- This covers TyConApp, AppTy, PredTy
\end{code}

Note [Sub-match]
~~~~~~~~~~~~~~~~
Consider this
	head :: [a] -> a
	|- head xs : <rhobox>
We will do a boxySubMatchType between 	a ~ <rhobox>
But we *don't* want to match [a |-> <rhobox>] because 
    (a) The box should be filled in with a rho-type, but
	   but the returned substitution maps TyVars to boxy
	   *sigma* types
    (b) In any case, the right final answer might be *either*
	   instantiate 'a' with a rho-type or a sigma type
	   head xs : Int   vs   head xs : forall b. b->b
So the matcher MUST NOT make a choice here.   In general, we only
bind a template type variable in boxyMatchType, not in boxySubMatchType.


\begin{code}
boxyMatchTypes 
	:: TcTyVarSet -> [TcType] -- The "template"; the tyvars are skolems
	-> [BoxySigmaType]	  -- Type to match
	-> TvSubst 		  -- Substitution of the [TcTyVar] to BoxySigmaTypes

-- boxyMatchTypes implements the Pre-matching judgement, in Fig 5 of the paper
-- "Boxy types: inference for higher rank types and impredicativity"

-- Find a *boxy* substitution that makes the template look as much 
-- 	like the BoxySigmaType as possible.  
-- It's always ok to return an empty substitution; 
--	anything more is jam on the pudding
-- 
-- NB1: This is a pure, non-monadic function.  
--	It does no unification, and cannot fail
--
-- Precondition: the arg lengths are equal
-- Precondition: none of the template type variables appear anywhere in the [BoxySigmaType]
--
	
------------
boxyMatchTypes tmpl_tvs tmpl_tys boxy_tys
  = ASSERT( length tmpl_tys == length boxy_tys )
    boxy_match_s tmpl_tvs tmpl_tys emptyVarSet boxy_tys emptyTvSubst
	-- ToDo: add error context?

boxy_match_s tmpl_tvs [] boxy_tvs [] subst
  = subst
boxy_match_s tmpl_tvs (t_ty:t_tys) boxy_tvs (b_ty:b_tys) subst
  = boxy_match tmpl_tvs t_ty boxy_tvs b_ty $
    boxy_match_s tmpl_tvs t_tys boxy_tvs b_tys subst
boxy_match_s tmpl_tvs _ boxy_tvs _ subst
  = panic "boxy_match_s"	-- Lengths do not match
    

------------
boxy_match :: TcTyVarSet -> TcType	-- Template
	   -> TcTyVarSet 		-- boxy_tvs: do not bind template tyvars to any of these
	   -> BoxySigmaType		-- Match against this type
	   -> TvSubst
	   -> TvSubst

-- The boxy_tvs argument prevents this match:
--	[a]  forall b. a  ~  forall b. b
-- We don't want to bind the template variable 'a'
-- to the quantified type variable 'b'!

boxy_match tmpl_tvs orig_tmpl_ty boxy_tvs orig_boxy_ty subst
  = go orig_tmpl_ty orig_boxy_ty
  where
    go t_ty b_ty 
	| Just t_ty' <- tcView t_ty = go t_ty' b_ty
	| Just b_ty' <- tcView b_ty = go t_ty b_ty'

    go ty1 ty2		-- C.f. the isSigmaTy case for boxySubMatchType
	| isSigmaTy ty1
	, (tvs1, _, tau1) <- tcSplitSigmaTy ty1
	, (tvs2, _, tau2) <- tcSplitSigmaTy ty2
	, equalLength tvs1 tvs2
	= boxy_match (tmpl_tvs `delVarSetList` tvs1)    tau1 
		     (boxy_tvs `extendVarSetList` tvs2) tau2 subst

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
	| tc1 == tc2 
	, not $ isOpenSynTyCon tc1
	= go_s tys1 tys2

    go (FunTy arg1 res1) (FunTy arg2 res2)
	= go_s [arg1,res1] [arg2,res2]

    go t_ty b_ty
	| Just (s1,t1) <- tcSplitAppTy_maybe t_ty,
	  Just (s2,t2) <- tcSplitAppTy_maybe b_ty,
	  typeKind t2 `isSubKind` typeKind t1	-- Maintain invariant
	= go_s [s1,t1] [s2,t2]

    go (TyVarTy tv) b_ty
 	| tv `elemVarSet` tmpl_tvs	-- Template type variable in the template
	, boxy_tvs `disjointVarSet` tyVarsOfType orig_boxy_ty
	, typeKind b_ty `isSubKind` tyVarKind tv  -- See Note [Matching kinds]
	= extendTvSubst subst tv boxy_ty'
	| otherwise
	= subst				-- Ignore others
	where
	  boxy_ty' = case lookupTyVar subst tv of
			Nothing -> orig_boxy_ty
			Just ty -> ty `boxyLub` orig_boxy_ty

    go _ _ = emptyTvSubst	-- It's important to *fail* by returning the empty substitution
	-- Example:  Tree a ~ Maybe Int
	-- We do not want to bind (a |-> Int) in pre-matching, because that can give very
	-- misleading error messages.  An even more confusing case is
	--	     a -> b ~ Maybe Int
	-- Then we do not want to bind (b |-> Int)!  It's always safe to discard bindings
	-- from this pre-matching phase.

    --------
    go_s tys1 tys2 = boxy_match_s tmpl_tvs tys1 boxy_tvs tys2 subst


boxyLub :: BoxySigmaType -> BoxySigmaType -> BoxySigmaType
-- Combine boxy information from the two types
-- If there is a conflict, return the first
boxyLub orig_ty1 orig_ty2
  = go orig_ty1 orig_ty2
  where
    go (AppTy f1 a1) (AppTy f2 a2) = AppTy (boxyLub f1 f2) (boxyLub a1 a2)
    go (FunTy f1 a1) (FunTy f2 a2) = FunTy (boxyLub f1 f2) (boxyLub a1 a2)
    go (TyConApp tc1 ts1) (TyConApp tc2 ts2) 
      | tc1 == tc2, length ts1 == length ts2
      = TyConApp tc1 (zipWith boxyLub ts1 ts2)

    go (TyVarTy tv1) ty2		-- This is the whole point; 
      | isTcTyVar tv1, isBoxyTyVar tv1 	-- choose ty2 if ty2 is a box
      = orig_ty2	

	-- Look inside type synonyms, but only if the naive version fails
    go ty1 ty2 | Just ty1' <- tcView ty1 = go ty1' ty2
	       | Just ty2' <- tcView ty1 = go ty1 ty2'

    -- For now, we don't look inside ForAlls, PredTys
    go ty1 ty2 = orig_ty1	-- Default
\end{code}

Note [Matching kinds]
~~~~~~~~~~~~~~~~~~~~~
The target type might legitimately not be a sub-kind of template.  
For example, suppose the target is simply a box with an OpenTypeKind, 
and the template is a type variable with LiftedTypeKind.  
Then it's ok (because the target type will later be refined).
We simply don't bind the template type variable.

It might also be that the kind mis-match is an error. For example,
suppose we match the template (a -> Int) against (Int# -> Int),
where the template type variable 'a' has LiftedTypeKind.  This
matching function does not fail; it simply doesn't bind the template.
Later stuff will fail.

%************************************************************************
%*									*
		Subsumption checking
%*									*
%************************************************************************

All the tcSub calls have the form
	
		tcSub expected_ty offered_ty
which checks
		offered_ty <= expected_ty

That is, that a value of type offered_ty is acceptable in
a place expecting a value of type expected_ty.

It returns a coercion function 
	co_fn :: offered_ty ~ expected_ty
which takes an HsExpr of type offered_ty into one of type
expected_ty.

\begin{code}
-----------------
tcSubExp :: BoxySigmaType -> BoxySigmaType -> TcM HsWrapper	-- Locally used only
	-- (tcSub act exp) checks that 
	--	act <= exp
tcSubExp actual_ty expected_ty
  = -- addErrCtxtM (unifyCtxt actual_ty expected_ty) $
    -- Adding the error context here leads to some very confusing error
    -- messages, such as "can't match forall a. a->a with forall a. a->a"
    -- Example is tcfail165: 
    --	    do var <- newEmptyMVar :: IO (MVar (forall a. Show a => a -> String))
    --         putMVar var (show :: forall a. Show a => a -> String)
    -- Here the info does not flow from the 'var' arg of putMVar to its 'show' arg
    -- but after zonking it looks as if it does!
    --
    -- So instead I'm adding the error context when moving from tc_sub to u_tys

    traceTc (text "tcSubExp" <+> ppr actual_ty <+> ppr expected_ty) >>
    tc_sub SubOther actual_ty actual_ty False expected_ty expected_ty

tcFunResTy :: Name -> BoxySigmaType -> BoxySigmaType -> TcM HsWrapper	-- Locally used only
tcFunResTy fun actual_ty expected_ty
  = traceTc (text "tcFunResTy" <+> ppr actual_ty <+> ppr expected_ty) >>
    tc_sub (SubFun fun) actual_ty actual_ty False expected_ty expected_ty
		   
-----------------
data SubCtxt = SubDone 		-- Error-context already pushed
	     | SubFun Name 	-- Context is tcFunResTy
	     | SubOther		-- Context is something else

tc_sub :: SubCtxt		-- How to add an error-context
       -> BoxySigmaType		-- actual_ty, before expanding synonyms
       -> BoxySigmaType		-- 		..and after
       -> InBox			-- True <=> expected_ty is inside a box
       -> BoxySigmaType		-- expected_ty, before
       -> BoxySigmaType		-- 		..and after
       -> TcM HsWrapper
				-- The acual_ty is never inside a box
-- IMPORTANT pre-condition: if the args contain foralls, the bound type 
--			    variables are visible non-monadically
--			    (i.e. tha args are sufficiently zonked)
-- This invariant is needed so that we can "see" the foralls, ad
-- e.g. in the SPEC rule where we just use splitSigmaTy 
	
tc_sub sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
  = traceTc (text "tc_sub" <+> ppr act_ty $$ ppr exp_ty) >>
    tc_sub1 sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
	-- This indirection is just here to make 
	-- it easy to insert a debug trace!

tc_sub1 sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
  | Just exp_ty' <- tcView exp_ty = tc_sub sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty'
tc_sub1 sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
  | Just act_ty' <- tcView act_ty = tc_sub sub_ctxt act_sty act_ty' exp_ib exp_sty exp_ty

-----------------------------------
-- Rule SBOXY, plus other cases when act_ty is a type variable
-- Just defer to boxy matching
-- This rule takes precedence over SKOL!
tc_sub1 sub_ctxt act_sty (TyVarTy tv) exp_ib exp_sty exp_ty
  = do	{ traceTc (text "tc_sub1 - case 1")
	; coi <- addSubCtxt sub_ctxt act_sty exp_sty $
		 uVar True False tv exp_ib exp_sty exp_ty
	; traceTc (case coi of 
			IdCo   -> text "tc_sub1 (Rule SBOXY) IdCo"
			ACo co -> text "tc_sub1 (Rule SBOXY) ACo" <+> ppr co)
	; return $ case coi of
			IdCo   -> idHsWrapper 
			ACo co -> WpCo co
	}

-----------------------------------
-- Skolemisation case (rule SKOL)
-- 	actual_ty:   d:Eq b => b->b
--	expected_ty: forall a. Ord a => a->a
--	co_fn e      /\a. \d2:Ord a. let d = eqFromOrd d2 in e

-- It is essential to do this *before* the specialisation case
-- Example:  f :: (Eq a => a->a) -> ...
--	     g :: Ord b => b->b
-- Consider  f g !

tc_sub1 sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
  | isSigmaTy exp_ty	
  = do { traceTc (text "tc_sub1 - case 2") ;
    if exp_ib then  	-- SKOL does not apply if exp_ty is inside a box
	defer_to_boxy_matching sub_ctxt act_sty act_ty exp_ib exp_sty exp_ty
    else do 
	{ (gen_fn, co_fn) <- tcGen exp_ty act_tvs $ \ _ body_exp_ty ->
			     tc_sub sub_ctxt act_sty act_ty False body_exp_ty body_exp_ty
	; return (gen_fn <.> co_fn) }
    }
  where
    act_tvs = tyVarsOfType act_ty
		-- It's really important to check for escape wrt 
		-- the free vars of both expected_ty *and* actual_ty

-----------------------------------
-- Specialisation case (rule ASPEC):
--	actual_ty:   forall a. Ord a => a->a
--	expected_ty: Int -> Int
--	co_fn e =    e Int dOrdInt

tc_sub1 sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty
-- Implements the new SPEC rule in the Appendix of the paper
-- "Boxy types: inference for higher rank types and impredicativity"
-- (This appendix isn't in the published version.)
-- The idea is to *first* do pre-subsumption, and then full subsumption
-- Example:	forall a. a->a  <=  Int -> (forall b. Int)
--   Pre-subsumpion finds a|->Int, and that works fine, whereas
--   just running full subsumption would fail.
  | isSigmaTy actual_ty
  = do	{ traceTc (text "tc_sub1 - case 3")
	; 	-- Perform pre-subsumption, and instantiate
	  	-- the type with info from the pre-subsumption; 
		-- boxy tyvars if pre-subsumption gives no info
	  let (tyvars, theta, tau) = tcSplitSigmaTy actual_ty
	      tau_tvs = exactTyVarsOfType tau
	; inst_tys <- if exp_ib then	-- Inside a box, do not do clever stuff
			do { tyvars' <- mapM tcInstBoxyTyVar tyvars
			   ; return (mkTyVarTys tyvars') }
		      else 		-- Outside, do clever stuff
			preSubType tyvars tau_tvs tau expected_ty
	; let subst' = zipOpenTvSubst tyvars inst_tys
	      tau'   = substTy subst' tau

		-- Perform a full subsumption check
	; traceTc (text "tc_sub_spec" <+> vcat [ppr actual_ty, 
						ppr tyvars <+> ppr theta <+> ppr tau,
						ppr tau'])
	; co_fn2 <- tc_sub sub_ctxt tau' tau' exp_ib exp_sty expected_ty

		-- Deal with the dictionaries
		-- The origin gives a helpful origin when we have
		-- a function with type f :: Int -> forall a. Num a => ...
		-- This way the (Num a) dictionary gets an OccurrenceOf f origin
	; let orig = case sub_ctxt of
			SubFun n -> OccurrenceOf n
			other    -> InstSigOrigin	-- Unhelpful
	; co_fn1 <- instCall orig inst_tys (substTheta subst' theta)
 	; return (co_fn2 <.> co_fn1) }

-----------------------------------
-- Function case (rule F1)
tc_sub1 sub_ctxt act_sty (FunTy act_arg act_res) exp_ib exp_sty (FunTy exp_arg exp_res)
  = do { traceTc (text "tc_sub1 - case 4")
       ; addSubCtxt sub_ctxt act_sty exp_sty $
                    tc_sub_funs act_arg act_res exp_ib exp_arg exp_res
       }

-- Function case (rule F2)
tc_sub1 sub_ctxt act_sty act_ty@(FunTy act_arg act_res) _ exp_sty (TyVarTy exp_tv)
  | isBoxyTyVar exp_tv
  = addSubCtxt sub_ctxt act_sty exp_sty $
    do	{ traceTc (text "tc_sub1 - case 5")
	; cts <- readMetaTyVar exp_tv
	; case cts of
	    Indirect ty -> tc_sub SubDone act_sty act_ty True exp_sty ty
	    Flexi -> do { [arg_ty,res_ty] <- withMetaTvs exp_tv fun_kinds mk_res_ty
 			      ; tc_sub_funs act_arg act_res True arg_ty res_ty } }
 where
    mk_res_ty [arg_ty', res_ty'] = mkFunTy arg_ty' res_ty'
    mk_res_ty other = panic "TcUnify.mk_res_ty3"
    fun_kinds = [argTypeKind, openTypeKind]

-- Everything else: defer to boxy matching
tc_sub1 sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty@(TyVarTy exp_tv)
  = do { traceTc (text "tc_sub1 - case 6a" <+> ppr [isBoxyTyVar exp_tv, isMetaTyVar exp_tv, isSkolemTyVar exp_tv, isExistentialTyVar exp_tv,isSigTyVar exp_tv] )
       ; defer_to_boxy_matching sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty
       }

tc_sub1 sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty
  = do { traceTc (text "tc_sub1 - case 6")
       ; defer_to_boxy_matching sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty
       }

-----------------------------------
defer_to_boxy_matching sub_ctxt act_sty actual_ty exp_ib exp_sty expected_ty
  = do	{ coi <- addSubCtxt sub_ctxt act_sty exp_sty $
	  u_tys outer False act_sty actual_ty exp_ib exp_sty expected_ty
	; return $ case coi of
			IdCo   -> idHsWrapper 
			ACo co -> WpCo co
	}
  where
    outer = case sub_ctxt of		-- Ugh
		SubDone -> False
		other	-> True

-----------------------------------
tc_sub_funs act_arg act_res exp_ib exp_arg exp_res
  = do	{ arg_coi   <- uTys False act_arg exp_ib exp_arg
	; co_fn_res <- tc_sub SubDone act_res act_res exp_ib exp_res exp_res
	; wrapper1  <- wrapFunResCoercion [exp_arg] co_fn_res 
        ; let wrapper2 = case arg_coi of 
				IdCo   -> idHsWrapper
				ACo co -> WpCo $ FunTy co act_res
	; return (wrapper1 <.> wrapper2)
        }

-----------------------------------
wrapFunResCoercion 
	:: [TcType]	-- Type of args
	-> HsWrapper 	-- HsExpr a -> HsExpr b
	-> TcM HsWrapper	-- HsExpr (arg_tys -> a) -> HsExpr (arg_tys -> b)
wrapFunResCoercion arg_tys co_fn_res
  | isIdHsWrapper co_fn_res 
  = return idHsWrapper
  | null arg_tys	   
  = return co_fn_res
  | otherwise 	       
  = do	{ arg_ids <- newSysLocalIds FSLIT("sub") arg_tys
	; return (mkWpLams arg_ids <.> co_fn_res <.> mkWpApps arg_ids) }
\end{code}



%************************************************************************
%*									*
\subsection{Generalisation}
%*									*
%************************************************************************

\begin{code}
tcGen :: BoxySigmaType				-- expected_ty
      -> TcTyVarSet				-- Extra tyvars that the universally
						--	quantified tyvars of expected_ty
						-- 	must not be unified
      -> ([TcTyVar] -> BoxyRhoType -> TcM result)
      -> TcM (HsWrapper, result)
	-- The expression has type: spec_ty -> expected_ty

tcGen expected_ty extra_tvs thing_inside	-- We expect expected_ty to be a forall-type
						-- If not, the call is a no-op
  = do	{ traceTc (text "tcGen")	
		-- We want the GenSkol info in the skolemised type variables to 
		-- mention the *instantiated* tyvar names, so that we get a
		-- good error message "Rigid variable 'a' is bound by (forall a. a->a)"
		-- Hence the tiresome but innocuous fixM
	; ((tvs', theta', rho'), skol_info) <- fixM (\ ~(_, skol_info) ->
		do { (forall_tvs, theta, rho_ty) <- tcInstSkolType skol_info expected_ty
			-- Get loation from monad, not from expected_ty
		   ; let skol_info = GenSkol forall_tvs (mkPhiTy theta rho_ty)
		   ; return ((forall_tvs, theta, rho_ty), skol_info) })

#ifdef DEBUG
	; traceTc (text "tcGen" <+> vcat [text "extra_tvs" <+> ppr extra_tvs,
				    text "expected_ty" <+> ppr expected_ty,
				    text "inst ty" <+> ppr tvs' <+> ppr theta' <+> ppr rho',
				    text "free_tvs" <+> ppr free_tvs])
#endif

	-- Type-check the arg and unify with poly type
	; (result, lie) <- getLIE (thing_inside tvs' rho')

	-- Check that the "forall_tvs" havn't been constrained
	-- The interesting bit here is that we must include the free variables
	-- of the expected_ty.  Here's an example:
	--	 runST (newVar True)
	-- Here, if we don't make a check, we'll get a type (ST s (MutVar s Bool))
	-- for (newVar True), with s fresh.  Then we unify with the runST's arg type
	-- forall s'. ST s' a. That unifies s' with s, and a with MutVar s Bool.
	-- So now s' isn't unconstrained because it's linked to a.
	-- Conclusion: include the free vars of the expected_ty in the
	-- list of "free vars" for the signature check.

	; loc <- getInstLoc (SigOrigin skol_info)
	; dicts <- newDictBndrs loc theta'
	; inst_binds <- tcSimplifyCheck loc tvs' dicts lie

	; checkSigTyVarsWrt free_tvs tvs'
	; traceTc (text "tcGen:done")

	; let
	    -- The WpLet binds any Insts which came out of the simplification.
	    dict_vars = map instToVar dicts
	    co_fn = mkWpTyLams tvs' <.> mkWpLams dict_vars <.> WpLet inst_binds
	; returnM (co_fn, result) }
  where
    free_tvs = tyVarsOfType expected_ty `unionVarSet` extra_tvs
\end{code}

    

%************************************************************************
%*									*
		Boxy unification
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

\begin{code}
boxyUnify :: BoxyType -> BoxyType -> TcM CoercionI
-- Acutal and expected, respectively
boxyUnify ty1 ty2 
  = addErrCtxtM (unifyCtxt ty1 ty2) $
    uTysOuter False ty1 False ty2

---------------
boxyUnifyList :: [BoxyType] -> [BoxyType] -> TcM [CoercionI] 
-- Arguments should have equal length
-- Acutal and expected types
boxyUnifyList tys1 tys2 = uList boxyUnify tys1 tys2

---------------
unifyType :: TcTauType -> TcTauType -> TcM CoercionI
-- No boxes expected inside these types
-- Acutal and expected types
unifyType ty1 ty2 	-- ty1 expected, ty2 inferred
  = ASSERT2( not (isBoxyTy ty1), ppr ty1 )
    ASSERT2( not (isBoxyTy ty2), ppr ty2 )
    addErrCtxtM (unifyCtxt ty1 ty2) $
    uTysOuter True ty1 True ty2

---------------
unifyPred :: PredType -> PredType -> TcM CoercionI
-- Acutal and expected types
unifyPred p1 p2 = addErrCtxtM (unifyCtxt (mkPredTy p1) (mkPredTy p2)) $
		  	uPred True True p1 True p2

unifyTheta :: TcThetaType -> TcThetaType -> TcM [CoercionI]
-- Acutal and expected types
unifyTheta theta1 theta2
  = do	{ checkTc (equalLength theta1 theta2)
		  (vcat [ptext SLIT("Contexts differ in length"),
			 nest 2 $ parens $ ptext SLIT("Use -fglasgow-exts to allow this")])
	; uList unifyPred theta1 theta2 
        }

---------------
uList :: (a -> a -> TcM b)
       -> [a] -> [a] -> TcM [b]
-- Unify corresponding elements of two lists of types, which
-- should be of equal length.  We charge down the list explicitly so that
-- we can complain if their lengths differ.
uList unify []         []	  = return []
uList unify (ty1:tys1) (ty2:tys2) = do { x  <- unify ty1 ty2; 
                                       ; xs <- uList unify tys1 tys2 
				       ; return (x:xs)
				       }
uList unify ty1s ty2s = panic "Unify.uList: mismatched type lists!"
\end{code}

@unifyTypeList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTypeList :: [TcTauType] -> TcM ()
unifyTypeList []		 = returnM ()
unifyTypeList [ty]		 = returnM ()
unifyTypeList (ty1:tys@(ty2:_)) = do { unifyType ty1 ty2
				      ; unifyTypeList tys }
\end{code}

%************************************************************************
%*									*
\subsection[Unify-uTys]{@uTys@: getting down to business}
%*									*
%************************************************************************

@uTys@ is the heart of the unifier.  Each arg occurs twice, because
we want to report errors in terms of synomyms if possible.  The first of
the pair is used in error messages only; it is always the same as the
second, except that if the first is a synonym then the second may be a
de-synonym'd version.  This way we get better error messages.

We call the first one \tr{ps_ty1}, \tr{ps_ty2} for ``possible synomym''.

\begin{code}
type SwapFlag = Bool
	-- False <=> the two args are (actual, expected) respectively
	-- True  <=> the two args are (expected, actual) respectively

type InBox = Bool	-- True  <=> we are inside a box
			-- False <=> we are outside a box
	-- The importance of this is that if we get "filled-box meets 
	-- filled-box", we'll look into the boxes and unify... but
	-- we must not allow polytypes.  But if we are in a box on
	-- just one side, then we can allow polytypes

type Outer = Bool	-- True <=> this is the outer level of a unification
			--	    so that the types being unified are the
			--	    very ones we began with, not some sub
			--	    component or synonym expansion
-- The idea is that if Outer is true then unifyMisMatch should
-- pop the context to remove the "Expected/Acutal" context

uTysOuter, uTys
     :: InBox -> TcType	-- ty1 is the *actual*   type
     -> InBox -> TcType	-- ty2 is the *expected* type
     -> TcM CoercionI
uTysOuter nb1 ty1 nb2 ty2 
	= do { traceTc (text "uTysOuter" <+> ppr ty1 <+> ppr ty2)
	     ; u_tys True nb1 ty1 ty1 nb2 ty2 ty2 }
uTys nb1 ty1 nb2 ty2 
	= do { traceTc (text "uTys" <+> ppr ty1 <+> ppr ty2)
	     ; u_tys False nb1 ty1 ty1 nb2 ty2 ty2 }


--------------
uTys_s :: InBox -> [TcType]	-- tys1 are the *actual*   types
       -> InBox -> [TcType]	-- tys2 are the *expected* types
       -> TcM [CoercionI] 
uTys_s nb1 []	      nb2 []	     = returnM []
uTys_s nb1 (ty1:tys1) nb2 (ty2:tys2) = do { coi <- uTys nb1 ty1 nb2 ty2
					  ; cois <- uTys_s nb1 tys1 nb2 tys2 
					  ; return (coi:cois)
					  }
uTys_s nb1 ty1s nb2 ty2s = panic "Unify.uTys_s: mismatched type lists!"

--------------
u_tys :: Outer
      -> InBox -> TcType -> TcType	-- ty1 is the *actual* type
      -> InBox -> TcType -> TcType	-- ty2 is the *expected* type
      -> TcM CoercionI

u_tys outer nb1 orig_ty1 ty1 nb2 orig_ty2 ty2
  = do { traceTc (text "u_tys " <+> ppr ty1 <+> text " " <+> ppr ty2)
       ; coi <- go outer ty1 ty2
       ; traceTc (case coi of
		        ACo co -> text "u_tys yields coercion: " <+> ppr co	
			IdCo   -> text "u_tys yields no coercion")
       ; return coi
       }
  where 

    go :: Outer -> TcType -> TcType -> TcM CoercionI
    go outer ty1 ty2 =
	do { traceTc (text "go " <+> ppr orig_ty1 <+> text "/" <+> ppr ty1
			 <+> ppr orig_ty2 <+> text "/" <+>  ppr ty2)
	   ; go1 outer ty1 ty2
	   }
           
    go1 :: Outer -> TcType -> TcType -> TcM CoercionI
	-- Always expand synonyms: see Note [Unification and synonyms]
        -- (this also throws away FTVs)
    go1 outer ty1 ty2 
      | Just ty1' <- tcView ty1 = go False ty1' ty2
      | Just ty2' <- tcView ty2 = go False ty1 ty2'

	-- Variables; go for uVar
    go1 outer (TyVarTy tyvar1) ty2 = uVar outer False tyvar1 nb2 orig_ty2 ty2
    go1 outer ty1 (TyVarTy tyvar2) = uVar outer True  tyvar2 nb1 orig_ty1 ty1
				-- "True" means args swapped

	-- The case for sigma-types must *follow* the variable cases
	-- because a boxy variable can be filed with a polytype;
	-- but must precede FunTy, because ((?x::Int) => ty) look
	-- like a FunTy; there isn't necy a forall at the top
    go1 _ ty1 ty2
      | isSigmaTy ty1 || isSigmaTy ty2
      = do   { traceTc (text "We have sigma types: equalLength" <+> ppr tvs1 <+> ppr tvs2)
	     ; checkM (equalLength tvs1 tvs2)
		      (unifyMisMatch outer False orig_ty1 orig_ty2)
	     ; traceTc (text "We're past the first length test")
	     ; tvs <- tcInstSkolTyVars UnkSkol tvs1	-- Not a helpful SkolemInfo
			-- Get location from monad, not from tvs1
	     ; let tys      = mkTyVarTys tvs
	           in_scope = mkInScopeSet (mkVarSet tvs)
 	           phi1   = substTy (mkTvSubst in_scope (zipTyEnv tvs1 tys)) body1
 	           phi2   = substTy (mkTvSubst in_scope (zipTyEnv tvs2 tys)) body2
		   (theta1,tau1) = tcSplitPhiTy phi1
		   (theta2,tau2) = tcSplitPhiTy phi2

	     ; addErrCtxtM (unifyForAllCtxt tvs phi1 phi2) $ do
	     { checkM (equalLength theta1 theta2)
		      (unifyMisMatch outer False orig_ty1 orig_ty2)
	     
	     ; cois <- uPreds False nb1 theta1 nb2 theta2 -- TOMDO: do something with these pred_cois
	     ; traceTc (text "TOMDO!")
	     ; coi <- uTys nb1 tau1 nb2 tau2

		-- Check for escape; e.g. (forall a. a->b) ~ (forall a. a->a)
	     ; free_tvs <- zonkTcTyVarsAndFV (varSetElems (tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2))
	     ; ifM (any (`elemVarSet` free_tvs) tvs)
		   (bleatEscapedTvs free_tvs tvs tvs)

		-- If both sides are inside a box, we are in a "box-meets-box"
		-- situation, and we should not have a polytype at all.  
		-- If we get here we have two boxes, already filled with
		-- the same polytype... but it should be a monotype.
		-- This check comes last, because the error message is 
		-- extremely unhelpful.  
	     ; ifM (nb1 && nb2) (notMonoType ty1)
	     ; return coi
	     }}
      where
	(tvs1, body1) = tcSplitForAllTys ty1
	(tvs2, body2) = tcSplitForAllTys ty2

	-- Predicates
    go1 outer (PredTy p1) (PredTy p2) 
	= uPred False nb1 p1 nb2 p2

	-- Type constructors must match
    go1 _ (TyConApp con1 tys1) (TyConApp con2 tys2)
      | con1 == con2 && not (isOpenSynTyCon con1)
      = do { cois <- uTys_s nb1 tys1 nb2 tys2
           ; return $ mkTyConAppCoI con1 tys1 cois
	   }
	-- See Note [TyCon app]
      | con1 == con2 && identicalOpenSynTyConApp
      = do { cois <- uTys_s nb1 tys1' nb2 tys2'
           ; return $ mkTyConAppCoI con1 tys1 (replicate n IdCo ++ cois)
           }
      where
        n                        = tyConArity con1
        (idxTys1, tys1')         = splitAt n tys1
        (idxTys2, tys2')         = splitAt n tys2
        identicalOpenSynTyConApp = idxTys1 `tcEqTypes` idxTys2
	-- See Note [OpenSynTyCon app]

	-- Functions; just check the two parts
    go1 _ (FunTy fun1 arg1) (FunTy fun2 arg2)
      = do { coi_l <- uTys nb1 fun1 nb2 fun2
	   ; coi_r <- uTys nb1 arg1 nb2 arg2 
	   ; return $ mkFunTyCoI fun1 coi_l arg1 coi_r
	   }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
    go1 outer (AppTy s1 t1) ty2
      | Just (s2,t2) <- tcSplitAppTy_maybe ty2
      = do { coi_s <- uTys nb1 s1 nb2 s2; coi_t <- uTys nb1 t1 nb2 t2
	   ; return $ mkAppTyCoI s1 coi_s t1 coi_t }

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
    go1 outer ty1 (AppTy s2 t2)
      | Just (s1,t1) <- tcSplitAppTy_maybe ty1
      = do { coi_s <- uTys nb1 s1 nb2 s2; coi_t <- uTys nb1 t1 nb2 t2 
	   ; return $ mkAppTyCoI s1 coi_s t1 coi_t }

        -- One or both outermost constructors are type family applications.
        -- If we can normalise them away, proceed as usual; otherwise, we
        -- need to defer unification by generating a wanted equality constraint.
    go1 outer ty1 ty2
      | ty1_is_fun || ty2_is_fun
      = do { (coi1, ty1') <- if ty1_is_fun then tcNormaliseFamInst ty1 
                                           else return (IdCo, ty1)
	   ; (coi2, ty2') <- if ty2_is_fun then tcNormaliseFamInst ty2 
                                           else return (IdCo, ty2)
	   ; coi <- if isOpenSynTyConApp ty1' || isOpenSynTyConApp ty2'
		    then do { -- One type family app can't be reduced yet
			      -- => defer
			    ; ty1'' <- zonkTcType ty1'
			    ; ty2'' <- zonkTcType ty2'
			    ; if tcEqType ty1'' ty2'' 
			      then return IdCo
			      else -- see [Deferred Unification]
			        defer_unification outer False orig_ty1 orig_ty2
			    }
		     else -- unification can proceed
			  go outer ty1' ty2'
	   ; return $ coi1 `mkTransCoI` coi `mkTransCoI` (mkSymCoI coi2)
	   }
	where
	  ty1_is_fun = isOpenSynTyConApp ty1
	  ty2_is_fun = isOpenSynTyConApp ty2

	-- Anything else fails	
    go1 outer _ _ = unifyMisMatch outer False orig_ty1 orig_ty2


----------
uPred outer nb1 (IParam n1 t1) nb2 (IParam n2 t2)
  | n1 == n2 = 
	do { coi <- uTys nb1 t1 nb2 t2
	   ; return $ mkIParamPredCoI n1 coi
	   }
uPred outer nb1 (ClassP c1 tys1) nb2 (ClassP c2 tys2)
  | c1 == c2 = 
	do { cois <- uTys_s nb1 tys1 nb2 tys2		-- Guaranteed equal lengths because the kinds check
	   ; return $ mkClassPPredCoI c1 tys1 cois
	   }
uPred outer _ p1 _ p2 = unifyMisMatch outer False (mkPredTy p1) (mkPredTy p2)

uPreds outer nb1 []       nb2 []       = return []
uPreds outer nb1 (p1:ps1) nb2 (p2:ps2) = 
	do { coi  <- uPred outer nb1 p1 nb2 p2
           ; cois <- uPreds outer nb1 ps1 nb2 ps2
	   ; return (coi:cois)
	   }
uPreds outer nb1 ps1      nb2 ps2      = panic "uPreds"
\end{code}

Note [TyCon app]
~~~~~~~~~~~~~~~~
When we find two TyConApps, the argument lists are guaranteed equal
length.  Reason: intially the kinds of the two types to be unified is
the same. The only way it can become not the same is when unifying two
AppTys (f1 a1):=:(f2 a2).  In that case there can't be a TyConApp in
the f1,f2 (because it'd absorb the app).  If we unify f1:=:f2 first,
which we do, that ensures that f1,f2 have the same kind; and that
means a1,a2 have the same kind.  And now the argument repeats.

Note [OpenSynTyCon app]
~~~~~~~~~~~~~~~~~~~~~~~
Given

  type family T a :: * -> *

the two types (T () a) and (T () Int) must unify, even if there are
no type instances for T at all.  Should we just turn them into an
equality (T () a ~ T () Int)?  I don't think so.  We currently try to 
eagerly unify everything we can before generating equalities; otherwise,
we could turn the unification of [Int] with [a] into an equality, too.

Note [Unification and synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you are tempted to make a short cut on synonyms, as in this
pseudocode...

\begin{verbatim}
-- NO	uTys (SynTy con1 args1 ty1) (SynTy con2 args2 ty2)
-- NO     = if (con1 == con2) then
-- NO	-- Good news!  Same synonym constructors, so we can shortcut
-- NO	-- by unifying their arguments and ignoring their expansions.
-- NO	unifyTypepeLists args1 args2
-- NO    else
-- NO	-- Never mind.  Just expand them and try again
-- NO	uTys ty1 ty2
\end{verbatim}

then THINK AGAIN.  Here is the whole story, as detected and reported
by Chris Okasaki \tr{<Chris_Okasaki@loch.mess.cs.cmu.edu>}:
\begin{quotation}
Here's a test program that should detect the problem:

\begin{verbatim}
	type Bogus a = Int
	x = (1 :: Bogus Char) :: Bogus Bool
\end{verbatim}

The problem with [the attempted shortcut code] is that
\begin{verbatim}
	con1 == con2
\end{verbatim}
is not a sufficient condition to be able to use the shortcut!
You also need to know that the type synonym actually USES all
its arguments.  For example, consider the following type synonym
which does not use all its arguments.
\begin{verbatim}
	type Bogus a = Int
\end{verbatim}

If you ever tried unifying, say, \tr{Bogus Char} with \tr{Bogus Bool},
the unifier would blithely try to unify \tr{Char} with \tr{Bool} and
would fail, even though the expanded forms (both \tr{Int}) should
match.

Similarly, unifying \tr{Bogus Char} with \tr{Bogus t} would
unnecessarily bind \tr{t} to \tr{Char}.

... You could explicitly test for the problem synonyms and mark them
somehow as needing expansion, perhaps also issuing a warning to the
user.
\end{quotation}


%************************************************************************
%*									*
\subsection[Unify-uVar]{@uVar@: unifying with a type variable}
%*									*
%************************************************************************

@uVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.

\begin{code}
uVar :: Outer
     -> SwapFlag	-- False => tyvar is the "actual" (ty is "expected")
			-- True  => ty is the "actual" (tyvar is "expected")
     -> TcTyVar
     -> InBox		-- True <=> definitely no boxes in t2
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM CoercionI

uVar outer swapped tv1 nb2 ps_ty2 ty2
  = do 	{ let expansion | showSDoc (ppr ty2) == showSDoc (ppr ps_ty2) = empty
			| otherwise = brackets (equals <+> ppr ty2)
	; traceTc (text "uVar" <+> ppr swapped <+> 
			sep [ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1 ),
				nest 2 (ptext SLIT(" <-> ")),
			     ppr ps_ty2 <+> dcolon <+> ppr (typeKind ty2) <+> expansion])
	; details <- lookupTcTyVar tv1
	; case details of
	    IndirectTv ty1 
		| swapped   -> u_tys outer nb2  ps_ty2 ty2 True ty1    ty1	-- Swap back
		| otherwise -> u_tys outer True ty1    ty1 nb2  ps_ty2 ty2	-- Same order
			-- The 'True' here says that ty1 is now inside a box
	    DoneTv details1 -> uUnfilledVar outer swapped tv1 details1 ps_ty2 ty2
	}

----------------
uUnfilledVar :: Outer
	     -> SwapFlag
     	     -> TcTyVar -> TcTyVarDetails	-- Tyvar 1
     	     -> TcTauType -> TcTauType		-- Type 2
     	     -> TcM CoercionI
-- Invariant: tyvar 1 is not unified with anything

uUnfilledVar outer swapped tv1 details1 ps_ty2 ty2
  | Just ty2' <- tcView ty2
  = 	-- Expand synonyms; ignore FTVs
    uUnfilledVar False swapped tv1 details1 ps_ty2 ty2'

uUnfilledVar outer swapped tv1 details1 ps_ty2 (TyVarTy tv2)
  | tv1 == tv2	-- Same type variable => no-op (but watch out for the boxy case)
  = case details1 of
	MetaTv BoxTv ref1  -- A boxy type variable meets itself;
			   -- this is box-meets-box, so fill in with a tau-type
	      -> do { tau_tv <- tcInstTyVar tv1
		    ; updateMeta tv1 ref1 (mkTyVarTy tau_tv) 
	 	    ; return IdCo
                    }
	other -> returnM IdCo	-- No-op

  | otherwise  -- Distinct type variables
  = do	{ lookup2 <- lookupTcTyVar tv2
	; case lookup2 of
	    IndirectTv ty2' -> uUnfilledVar outer swapped tv1 details1 ty2' ty2'
	    DoneTv details2 -> uUnfilledVars outer swapped tv1 details1 tv2 details2
	}

uUnfilledVar outer swapped tv1 details1 ps_ty2 non_var_ty2
  =     -- ty2 is not a type variable
    case details1 of	
      MetaTv (SigTv _) _ -> rigid_variable
      MetaTv info ref1   -> 
        uMetaVar outer swapped tv1 info ref1 ps_ty2 non_var_ty2 
      SkolemTv _         -> rigid_variable
  where
    rigid_variable 
      | isOpenSynTyConApp non_var_ty2
      =           -- 'non_var_ty2's outermost constructor is a type family,
                  -- which we may may be able to normalise
        do { (coi2, ty2') <- tcNormaliseFamInst non_var_ty2
           ; case coi2 of
	       IdCo   ->   -- no progress, but maybe after other instantiations
		         defer_unification outer swapped (TyVarTy tv1) ps_ty2
               ACo co ->   -- progress: so lets try again
        	 do { traceTc $
                        ppr co <+> text "::"<+> ppr non_var_ty2 <+> text "~" <+>
                        ppr ty2'
		    ; coi <- uUnfilledVar outer swapped tv1 details1 ps_ty2 ty2'
		    ; let coi2' = (if swapped then id else mkSymCoI) coi2
       		    ; return $ coi2' `mkTransCoI` coi
		    }
           }
      | SkolemTv RuntimeUnkSkol <- details1
                   -- runtime unknown will never match
      = unifyMisMatch outer swapped (TyVarTy tv1) ps_ty2
      | otherwise  -- defer as a given equality may still resolve this
      = defer_unification outer swapped (TyVarTy tv1) ps_ty2
\end{code}

Note [Deferred Unification]
~~~~~~~~~~~~~~~~~~~~
We may encounter a unification ty1 = ty2 that cannot be performed syntactically,
and yet its consistency is undetermined. Previously, there was no way to still
make it consistent. So a mismatch error was issued. 

Now these unfications are deferred until constraint simplification, where type
family instances and given equations may (or may not) establish the consistency.
Deferred unifications are of the form 
		F ... ~ ... 
or 		x ~ ... 
where F is a type function and x is a type variable.   
E.g. 
	id :: x ~ y => x -> y
	id e = e

involves the unfication x = y. It is deferred until we bring into account the
context x ~ y to establish that it holds.

If available, we defer original types (rather than those where closed type
synonyms have already been expanded via tcCoreView).  This is, as usual, to
improve error messages.

We need to both 'unBox' and zonk deferred types.  We need to unBox as
functions, such as TcExpr.tcMonoExpr promise to fill boxes in the expected
type.  We need to zonk as the types go into the kind of the coercion variable
`cotv' and those are not zonked in Inst.zonkInst.  (Maybe it would be better
to zonk in zonInst instead.  Would that be sufficient?)

\begin{code}
defer_unification :: Bool               -- pop innermost context?
                  -> SwapFlag
	          -> TcType
	          -> TcType
	          -> TcM CoercionI
defer_unification outer True ty1 ty2
  = defer_unification outer False ty2 ty1
defer_unification outer False ty1 ty2
  = do	{ ty1' <- unBox ty1 >>= zonkTcType      -- unbox *and* zonk..
 	; ty2' <- unBox ty2 >>= zonkTcType      -- ..see preceding note
        ; traceTc $ text "deferring:" <+> ppr ty1 <+> text "~" <+> ppr ty2
	; cotv <- newMetaCoVar ty1' ty2'
		-- put ty1 ~ ty2 in LIE
		-- Left means "wanted"
	; inst <- (if outer then popErrCtxt else id) $
                  mkEqInst (EqPred ty1' ty2') (Left cotv)
	; extendLIE inst 
	; return $ ACo $ TyVarTy cotv  }

----------------
uMetaVar :: Bool               -- pop innermost context?
         -> SwapFlag
	 -> TcTyVar -> BoxInfo -> IORef MetaDetails
	 -> TcType -> TcType
	 -> TcM CoercionI
-- tv1 is an un-filled-in meta type variable (maybe boxy, maybe tau)
-- ty2 is not a type variable

uMetaVar outer swapped tv1 BoxTv ref1 ps_ty2 non_var_ty2
  = 	-- tv1 is a BoxTv.  So we must unbox ty2, to ensure
	-- that any boxes in ty2 are filled with monotypes
	-- 
	-- It should not be the case that tv1 occurs in ty2
	-- (i.e. no occurs check should be needed), but if perchance
	-- it does, the unbox operation will fill it, and the DEBUG
	-- checks for that.
    do 	{ final_ty <- unBox ps_ty2
#ifdef DEBUG
	; meta_details <- readMutVar ref1
	; case meta_details of
	    Indirect ty -> WARN( True, ppr tv1 <+> ppr ty )
			   return ()	-- This really should *not* happen
	    Flexi -> return ()
#endif
	; checkUpdateMeta swapped tv1 ref1 final_ty
        ; return IdCo
        }

uMetaVar outer swapped tv1 info1 ref1 ps_ty2 non_var_ty2
  = do	{ -- Occurs check + monotype check
        ; mb_final_ty <- checkTauTvUpdate tv1 ps_ty2
        ; case mb_final_ty of
            Nothing       ->    -- tv1 occured in type family parameter
              defer_unification outer swapped (mkTyVarTy tv1) ps_ty2
            Just final_ty -> 
              do { checkUpdateMeta swapped tv1 ref1 final_ty 
                 ; return IdCo
                 }
        }

----------------
uUnfilledVars :: Outer
	      -> SwapFlag
      	      -> TcTyVar -> TcTyVarDetails	-- Tyvar 1
      	      -> TcTyVar -> TcTyVarDetails	-- Tyvar 2
      	      -> TcM CoercionI
-- Invarant: The type variables are distinct, 
-- 	     Neither is filled in yet
--    	     They might be boxy or not

uUnfilledVars outer swapped tv1 (SkolemTv _) tv2 (SkolemTv _)
  = -- see [Deferred Unification]
    defer_unification outer swapped (mkTyVarTy tv1) (mkTyVarTy tv2)

uUnfilledVars outer swapped tv1 (MetaTv info1 ref1) tv2 (SkolemTv _)
  = checkUpdateMeta swapped tv1 ref1 (mkTyVarTy tv2) >> return IdCo
uUnfilledVars outer swapped tv1 (SkolemTv _) tv2 (MetaTv info2 ref2)
  = checkUpdateMeta (not swapped) tv2 ref2 (mkTyVarTy tv1) >> return IdCo

-- ToDo: this function seems too long for what it acutally does!
uUnfilledVars outer swapped tv1 (MetaTv info1 ref1) tv2 (MetaTv info2 ref2)
  = case (info1, info2) of
	(BoxTv,   BoxTv)   -> box_meets_box >> return IdCo

	-- If a box meets a TauTv, but the fomer has the smaller kind
	-- then we must create a fresh TauTv with the smaller kind
	(_,       BoxTv)   | k1_sub_k2 -> update_tv2 >> return IdCo
			   | otherwise -> box_meets_box >> return IdCo
	(BoxTv,   _    )   | k2_sub_k1 -> update_tv1 >> return IdCo
			   | otherwise -> box_meets_box >> return IdCo

	-- Avoid SigTvs if poss
	(SigTv _, _      ) | k1_sub_k2 -> update_tv2 >> return IdCo
	(_,       SigTv _) | k2_sub_k1 -> update_tv1 >> return IdCo

	(_,   _) | k1_sub_k2 -> if k2_sub_k1 && nicer_to_update_tv1
				then update_tv1 >> return IdCo 	-- Same kinds
				else update_tv2 >> return IdCo
		 | k2_sub_k1 -> update_tv1 >> return IdCo
		 | otherwise -> kind_err >> return IdCo

	-- Update the variable with least kind info
	-- See notes on type inference in Kind.lhs
	-- The "nicer to" part only applies if the two kinds are the same,
	-- so we can choose which to do.
  where
	-- Kinds should be guaranteed ok at this point
    update_tv1 = updateMeta tv1 ref1 (mkTyVarTy tv2)
    update_tv2 = updateMeta tv2 ref2 (mkTyVarTy tv1)

    box_meets_box | k1_sub_k2 = if k2_sub_k1 && nicer_to_update_tv1
				then fill_from tv2
				else fill_from tv1
		  | k2_sub_k1 = fill_from tv2
		  | otherwise = kind_err

	-- Update *both* tyvars with a TauTv whose name and kind
	-- are gotten from tv (avoid losing nice names is poss)
    fill_from tv = do { tv' <- tcInstTyVar tv
		      ; let tau_ty = mkTyVarTy tv'
		      ; updateMeta tv1 ref1 tau_ty
		      ; updateMeta tv2 ref2 tau_ty }

    kind_err = addErrCtxtM (unifyKindCtxt swapped tv1 (mkTyVarTy tv2))	$
	       unifyKindMisMatch k1 k2

    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    k1_sub_k2 = k1 `isSubKind` k2
    k2_sub_k1 = k2 `isSubKind` k1

    nicer_to_update_tv1 = isSystemName (Var.varName tv1)
	-- Try to update sys-y type variables in preference to ones
	-- gotten (say) by instantiating a polymorphic function with
	-- a user-written type sig
\end{code}

\begin{code}
refineBox :: TcType -> TcM TcType
-- Unbox the outer box of a boxy type (if any)
refineBox ty@(TyVarTy box_tv) 
  | isMetaTyVar box_tv
  = do	{ cts <- readMetaTyVar box_tv
	; case cts of
		Flexi -> return ty
		Indirect ty -> return ty } 
refineBox other_ty = return other_ty

refineBoxToTau :: TcType -> TcM TcType
-- Unbox the outer box of a boxy type, filling with a monotype if it is empty
-- Like refineBox except for the "fill with monotype" part.
refineBoxToTau ty@(TyVarTy box_tv) 
  | isMetaTyVar box_tv
  , MetaTv BoxTv ref <- tcTyVarDetails box_tv
  = do	{ cts <- readMutVar ref
	; case cts of
		Flexi -> fillBoxWithTau box_tv ref
		Indirect ty -> return ty } 
refineBoxToTau other_ty = return other_ty

zapToMonotype :: BoxySigmaType -> TcM TcTauType
-- Subtle... we must zap the boxy res_ty
-- to kind * before using it to instantiate a LitInst
-- Calling unBox instead doesn't do the job, because the box
-- often has an openTypeKind, and we don't want to instantiate
-- with that type.
zapToMonotype res_ty
  = do 	{ res_tau <- newFlexiTyVarTy liftedTypeKind
	; boxyUnify res_tau res_ty
	; return res_tau }

unBox :: BoxyType -> TcM TcType
-- unBox implements the judgement 
--	|- s' ~ box(s)
-- with input s', and result s
-- 
-- It removes all boxes from the input type, returning a non-boxy type.
-- A filled box in the type can only contain a monotype; unBox fails if not
-- The type can have empty boxes, which unBox fills with a monotype
--
-- Compare this wth checkTauTvUpdate
--
-- For once, it's safe to treat synonyms as opaque!

unBox (NoteTy n ty) 	= do { ty' <- unBox ty; return (NoteTy n ty') }
unBox (TyConApp tc tys) = do { tys' <- mapM unBox tys; return (TyConApp tc tys') }
unBox (AppTy f a)       = do { f' <- unBox f; a' <- unBox a; return (mkAppTy f' a') }
unBox (FunTy f a)       = do { f' <- unBox f; a' <- unBox a; return (FunTy f' a') }
unBox (PredTy p)	= do { p' <- unBoxPred p; return (PredTy p') }
unBox (ForAllTy tv ty)  = ASSERT( isImmutableTyVar tv )
			  do { ty' <- unBox ty; return (ForAllTy tv ty') }
unBox (TyVarTy tv)
  | isTcTyVar tv				-- It's a boxy type variable
  , MetaTv BoxTv ref <- tcTyVarDetails tv	-- NB: non-TcTyVars are possible
  = do	{ cts <- readMutVar ref			--     under nested quantifiers
	; case cts of
	    Flexi -> fillBoxWithTau tv ref
	    Indirect ty -> do { non_boxy_ty <- unBox ty
			      ; if isTauTy non_boxy_ty 
				then return non_boxy_ty
				else notMonoType non_boxy_ty }
	}
  | otherwise 	-- Skolems, and meta-tau-variables
  = return (TyVarTy tv)

unBoxPred (ClassP cls tys) = do { tys' <- mapM unBox tys; return (ClassP cls tys') }
unBoxPred (IParam ip ty)   = do { ty' <- unBox ty; return (IParam ip ty') }
unBoxPred (EqPred ty1 ty2) = do { ty1' <- unBox ty1; ty2' <- unBox ty2; return (EqPred ty1' ty2') }
\end{code}



%************************************************************************
%*									*
\subsection[Unify-context]{Errors and contexts}
%*									*
%************************************************************************

Errors
~~~~~~

\begin{code}
unifyCtxt act_ty exp_ty tidy_env
  = do	{ act_ty' <- zonkTcType act_ty
	; exp_ty' <- zonkTcType exp_ty
	; let (env1, exp_ty'') = tidyOpenType tidy_env exp_ty'
	      (env2, act_ty'') = tidyOpenType env1     act_ty'
	; return (env2, mkExpectedActualMsg act_ty'' exp_ty'') }

----------------
mkExpectedActualMsg act_ty exp_ty
  = nest 2 (vcat [ text "Expected type" <> colon <+> ppr exp_ty,
		   text "Inferred type" <> colon <+> ppr act_ty ])

----------------
-- If an error happens we try to figure out whether the function
-- function has been given too many or too few arguments, and say so.
addSubCtxt SubDone actual_res_ty expected_res_ty thing_inside
  = thing_inside
addSubCtxt sub_ctxt actual_res_ty expected_res_ty thing_inside
  = addErrCtxtM mk_err thing_inside
  where
    mk_err tidy_env
      = do { exp_ty' <- zonkTcType expected_res_ty
	   ; act_ty' <- zonkTcType actual_res_ty
	   ; let (env1, exp_ty'') = tidyOpenType tidy_env exp_ty'
	         (env2, act_ty'') = tidyOpenType env1     act_ty'
	         (exp_args, _)    = tcSplitFunTys exp_ty''
	         (act_args, _)    = tcSplitFunTys act_ty''
	
	         len_act_args     = length act_args
	         len_exp_args     = length exp_args

	         message = case sub_ctxt of
	   		  SubFun fun | len_exp_args < len_act_args -> wrongArgsCtxt "too few"  fun
	   		             | len_exp_args > len_act_args -> wrongArgsCtxt "too many" fun
	   		  other -> mkExpectedActualMsg act_ty'' exp_ty''
	   ; return (env2, message) }

    wrongArgsCtxt too_many_or_few fun
      = ptext SLIT("Probable cause:") <+> quotes (ppr fun)
	<+> ptext SLIT("is applied to") <+> text too_many_or_few 
	<+> ptext SLIT("arguments")

------------------
unifyForAllCtxt tvs phi1 phi2 env
  = returnM (env2, msg)
  where
    (env', tvs') = tidyOpenTyVars env tvs	-- NB: not tidyTyVarBndrs
    (env1, phi1') = tidyOpenType env' phi1
    (env2, phi2') = tidyOpenType env1 phi2
    msg = vcat [ptext SLIT("When matching") <+> quotes (ppr (mkForAllTys tvs' phi1')),
	        ptext SLIT("          and") <+> quotes (ppr (mkForAllTys tvs' phi2'))]

-----------------------
unifyMisMatch outer swapped ty1 ty2
  = do	{ (env, msg) <- if swapped then misMatchMsg ty2 ty1
				   else misMatchMsg ty1 ty2

	-- This is the whole point of the 'outer' stuff
	; if outer then popErrCtxt (failWithTcM (env, msg))
	  	   else failWithTcM (env, msg)
	} 
\end{code}


%************************************************************************
%*									*
		Kind unification
%*									*
%************************************************************************

Unifying kinds is much, much simpler than unifying types.

\begin{code}
unifyKind :: TcKind		    -- Expected
	  -> TcKind		    -- Actual
	  -> TcM ()
unifyKind (TyConApp kc1 []) (TyConApp kc2 []) 
  | isSubKindCon kc2 kc1 = returnM ()

unifyKind (FunTy a1 r1) (FunTy a2 r2)
  = do { unifyKind a2 a1; unifyKind r1 r2 }
		-- Notice the flip in the argument,
		-- so that the sub-kinding works right
unifyKind (TyVarTy kv1) k2 = uKVar False kv1 k2
unifyKind k1 (TyVarTy kv2) = uKVar True kv2 k1
unifyKind k1 k2 = unifyKindMisMatch k1 k2

unifyKinds :: [TcKind] -> [TcKind] -> TcM ()
unifyKinds []       []       = returnM ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenM_`
			       unifyKinds ks1 ks2
unifyKinds _ _ 		     = panic "unifyKinds: length mis-match"

----------------
uKVar :: Bool -> KindVar -> TcKind -> TcM ()
uKVar swapped kv1 k2
  = do 	{ mb_k1 <- readKindVar kv1
	; case mb_k1 of
	    Flexi -> uUnboundKVar swapped kv1 k2
	    Indirect k1 | swapped   -> unifyKind k2 k1
		        | otherwise -> unifyKind k1 k2 }

----------------
uUnboundKVar :: Bool -> KindVar -> TcKind -> TcM ()
uUnboundKVar swapped kv1 k2@(TyVarTy kv2)
  | kv1 == kv2 = returnM ()
  | otherwise	-- Distinct kind variables
  = do	{ mb_k2 <- readKindVar kv2
	; case mb_k2 of
	    Indirect k2 -> uUnboundKVar swapped kv1 k2
	    Flexi -> writeKindVar kv1 k2 }

uUnboundKVar swapped kv1 non_var_k2
  = do	{ k2' <- zonkTcKind non_var_k2
	; kindOccurCheck kv1 k2'
	; k2'' <- kindSimpleKind swapped k2'
		-- KindVars must be bound only to simple kinds
		-- Polarities: (kindSimpleKind True ?) succeeds 
		-- returning *, corresponding to unifying
		--	expected: ?
		--	actual:   kind-ver
	; writeKindVar kv1 k2'' }

----------------
kindOccurCheck kv1 k2	-- k2 is zonked
  = checkTc (not_in k2) (kindOccurCheckErr kv1 k2)
  where
    not_in (TyVarTy kv2)   = kv1 /= kv2
    not_in (FunTy a2 r2) = not_in a2 && not_in r2
    not_in other         = True

kindSimpleKind :: Bool -> Kind -> TcM SimpleKind
-- (kindSimpleKind True k) returns a simple kind sk such that sk <: k
-- If the flag is False, it requires k <: sk
-- E.g. 	kindSimpleKind False ?? = *
-- What about (kv -> *) :=: ?? -> *
kindSimpleKind orig_swapped orig_kind
  = go orig_swapped orig_kind
  where
    go sw (FunTy k1 k2) = do { k1' <- go (not sw) k1
	                     ; k2' <- go sw k2
	                     ; return (mkArrowKind k1' k2') }
    go True k
     | isOpenTypeKind k = return liftedTypeKind
     | isArgTypeKind k  = return liftedTypeKind
    go sw k
     | isLiftedTypeKind k   = return liftedTypeKind
     | isUnliftedTypeKind k = return unliftedTypeKind
    go sw k@(TyVarTy _)	  = return k	-- KindVars are always simple
    go swapped kind = failWithTc (ptext SLIT("Unexpected kind unification failure:")
				  <+> ppr orig_swapped <+> ppr orig_kind)
	-- I think this can't actually happen

-- T v = MkT v		 v must be a type 
-- T v w = MkT (v -> w)	 v must not be an umboxed tuple

----------------
kindOccurCheckErr tyvar ty
  = hang (ptext SLIT("Occurs check: cannot construct the infinite kind:"))
       2 (sep [ppr tyvar, char '=', ppr ty])
\end{code}

\begin{code}
unifyFunKind :: TcKind -> TcM (Maybe (TcKind, TcKind))
-- Like unifyFunTy, but does not fail; instead just returns Nothing

unifyFunKind (TyVarTy kvar)
  = readKindVar kvar `thenM` \ maybe_kind ->
    case maybe_kind of
      Indirect fun_kind -> unifyFunKind fun_kind
      Flexi -> 
          do { arg_kind <- newKindVar
             ; res_kind <- newKindVar
             ; writeKindVar kvar (mkArrowKind arg_kind res_kind)
             ; returnM (Just (arg_kind,res_kind)) }
    
unifyFunKind (FunTy arg_kind res_kind) = returnM (Just (arg_kind,res_kind))
unifyFunKind other		       = returnM Nothing
\end{code}

%************************************************************************
%*									*
	Checking kinds
%*									*
%************************************************************************

---------------------------
-- We would like to get a decent error message from
--   (a) Under-applied type constructors
--		f :: (Maybe, Maybe)
--   (b) Over-applied type constructors
--		f :: Int x -> Int x
--

\begin{code}
checkExpectedKind :: Outputable a => a -> TcKind -> TcKind -> TcM ()
-- A fancy wrapper for 'unifyKind', which tries 
-- to give decent error messages.
-- 	(checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible 
-- 	with the expected kind exp_kind
-- The first argument, ty, is used only in the error message generation
checkExpectedKind ty act_kind exp_kind
  | act_kind `isSubKind` exp_kind -- Short cut for a very common case
  = returnM ()
  | otherwise
  = tryTc (unifyKind exp_kind act_kind)	`thenM` \ (_errs, mb_r) ->
    case mb_r of {
	Just r  -> returnM () ;	-- Unification succeeded
	Nothing ->

	-- So there's definitely an error
	-- Now to find out what sort
    zonkTcKind exp_kind		`thenM` \ exp_kind ->
    zonkTcKind act_kind		`thenM` \ act_kind ->

    tcInitTidyEnv		`thenM` \ env0 -> 
    let (exp_as, _) = splitKindFunTys exp_kind
        (act_as, _) = splitKindFunTys act_kind
	n_exp_as = length exp_as
	n_act_as = length act_as
	
	(env1, tidy_exp_kind) = tidyKind env0 exp_kind
	(env2, tidy_act_kind) = tidyKind env1 act_kind

	err | n_exp_as < n_act_as	-- E.g. [Maybe]
	    = quotes (ppr ty) <+> ptext SLIT("is not applied to enough type arguments")

		-- Now n_exp_as >= n_act_as. In the next two cases, 
		-- n_exp_as == 0, and hence so is n_act_as
	    | isLiftedTypeKind exp_kind && isUnliftedTypeKind act_kind
	    = ptext SLIT("Expecting a lifted type, but") <+> quotes (ppr ty)
		<+> ptext SLIT("is unlifted")

	    | isUnliftedTypeKind exp_kind && isLiftedTypeKind act_kind
	    = ptext SLIT("Expecting an unlifted type, but") <+> quotes (ppr ty)
		<+> ptext SLIT("is lifted")

	    | otherwise 		-- E.g. Monad [Int]
	    = ptext SLIT("Kind mis-match")

	more_info = sep [ ptext SLIT("Expected kind") <+> 
				quotes (pprKind tidy_exp_kind) <> comma,
		   	  ptext SLIT("but") <+> quotes (ppr ty) <+> 
			  	ptext SLIT("has kind") <+> quotes (pprKind tidy_act_kind)]
   in
   failWithTcM (env2, err $$ more_info)
   }
\end{code}

%************************************************************************
%*									*
\subsection{Checking signature type variables}
%*									*
%************************************************************************

@checkSigTyVars@ checks that a set of universally quantified type varaibles
are not mentioned in the environment.  In particular:

	(a) Not mentioned in the type of a variable in the envt
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

	(d) Not (unified with another type variable that is) in scope.
		eg f x :: (r->r) = (\y->y) :: forall a. a->r
	    when checking the expression type signature, we find that
	    even though there is nothing in scope whose type mentions r,
	    nevertheless the type signature for the expression isn't right.

	    Another example is in a class or instance declaration:
		class C a where
		   op :: forall b. a -> b
		   op x = x
	    Here, b gets unified with a

Before doing this, the substitution is applied to the signature type variable.

\begin{code}
checkSigTyVars :: [TcTyVar] -> TcM ()
checkSigTyVars sig_tvs = check_sig_tyvars emptyVarSet sig_tvs

checkSigTyVarsWrt :: TcTyVarSet -> [TcTyVar] -> TcM ()
-- The extra_tvs can include boxy type variables; 
-- 	e.g. TcMatches.tcCheckExistentialPat
checkSigTyVarsWrt extra_tvs sig_tvs
  = do	{ extra_tvs' <- zonkTcTyVarsAndFV (varSetElems extra_tvs)
	; check_sig_tyvars extra_tvs' sig_tvs }

check_sig_tyvars
	:: TcTyVarSet	-- Global type variables. The universally quantified
			-- 	tyvars should not mention any of these
			--	Guaranteed already zonked.
	-> [TcTyVar]	-- Universally-quantified type variables in the signature
			--	Guaranteed to be skolems
	-> TcM ()
check_sig_tyvars extra_tvs []
  = returnM ()
check_sig_tyvars extra_tvs sig_tvs 
  = ASSERT( all isSkolemTyVar sig_tvs )
    do	{ gbl_tvs <- tcGetGlobalTyVars
	; traceTc (text "check_sig_tyvars" <+> (vcat [text "sig_tys" <+> ppr sig_tvs,
				      text "gbl_tvs" <+> ppr gbl_tvs,
				      text "extra_tvs" <+> ppr extra_tvs]))

	; let env_tvs = gbl_tvs `unionVarSet` extra_tvs
	; ifM (any (`elemVarSet` env_tvs) sig_tvs)
	      (bleatEscapedTvs env_tvs sig_tvs sig_tvs)
	}

bleatEscapedTvs :: TcTyVarSet	-- The global tvs
	        -> [TcTyVar]	-- The possibly-escaping type variables
		-> [TcTyVar]	-- The zonked versions thereof
		-> TcM ()
-- Complain about escaping type variables
-- We pass a list of type variables, at least one of which
-- escapes.  The first list contains the original signature type variable,
-- while the second  contains the type variable it is unified to (usually itself)
bleatEscapedTvs globals sig_tvs zonked_tvs
  = do	{ env0 <- tcInitTidyEnv
	; let (env1, tidy_tvs)        = tidyOpenTyVars env0 sig_tvs
	      (env2, tidy_zonked_tvs) = tidyOpenTyVars env1 zonked_tvs

	; (env3, msgs) <- foldlM check (env2, []) (tidy_tvs `zip` tidy_zonked_tvs)
	; failWithTcM (env3, main_msg $$ nest 2 (vcat msgs)) }
  where
    main_msg = ptext SLIT("Inferred type is less polymorphic than expected")

    check (tidy_env, msgs) (sig_tv, zonked_tv)
      | not (zonked_tv `elemVarSet` globals) = return (tidy_env, msgs)
      | otherwise
      = do { (tidy_env1, globs) <- findGlobals (unitVarSet zonked_tv) tidy_env
	   ; returnM (tidy_env1, escape_msg sig_tv zonked_tv globs : msgs) }

-----------------------
escape_msg sig_tv zonked_tv globs
  | notNull globs 
  = vcat [sep [msg, ptext SLIT("is mentioned in the environment:")], 
	  nest 2 (vcat globs)]
  | otherwise
  = msg <+> ptext SLIT("escapes")
	-- Sigh.  It's really hard to give a good error message
	-- all the time.   One bad case is an existential pattern match.
	-- We rely on the "When..." context to help.
  where
    msg = ptext SLIT("Quantified type variable") <+> quotes (ppr sig_tv) <+> is_bound_to
    is_bound_to 
	| sig_tv == zonked_tv = empty
	| otherwise = ptext SLIT("is unified with") <+> quotes (ppr zonked_tv) <+> ptext SLIT("which")
\end{code}

These two context are used with checkSigTyVars
    
\begin{code}
sigCtxt :: Id -> [TcTyVar] -> TcThetaType -> TcTauType
	-> TidyEnv -> TcM (TidyEnv, Message)
sigCtxt id sig_tvs sig_theta sig_tau tidy_env
  = zonkTcType sig_tau		`thenM` \ actual_tau ->
    let
	(env1, tidy_sig_tvs)    = tidyOpenTyVars tidy_env sig_tvs
	(env2, tidy_sig_rho)	= tidyOpenType env1 (mkPhiTy sig_theta sig_tau)
	(env3, tidy_actual_tau) = tidyOpenType env2 actual_tau
	sub_msg = vcat [ptext SLIT("Signature type:    ") <+> pprType (mkForAllTys tidy_sig_tvs tidy_sig_rho),
		        ptext SLIT("Type to generalise:") <+> pprType tidy_actual_tau
		   ]
 	msg = vcat [ptext SLIT("When trying to generalise the type inferred for") <+> quotes (ppr id),
		    nest 2 sub_msg]
    in
    returnM (env3, msg)
\end{code}
