%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Type subsumption and unification}

\begin{code}
module TcUnify (
	-- Full-blown subsumption
  tcSubExp, tcFunResTy, tcGen, 
  checkSigTyVars, checkSigTyVarsWrt, bleatEscapedTvs, sigCtxt, 

	-- Various unifications
  unifyType, unifyTypeList, unifyTheta,
  unifyKind, unifyKinds, unifyFunKind, 
  checkExpectedKind, 
  boxySubMatchType, boxyMatchTypes,

  --------------------------------
  -- Holes
  tcInfer, subFunTys, unBox, stripBoxyType, withBox, 
  boxyUnify, boxyUnifyList, zapToMonotype,
  boxySplitListTy, boxySplitTyConApp, boxySplitAppTy,
  wrapFunResCoercion
  ) where

#include "HsVersions.h"

import HsSyn		( ExprCoFn(..), idCoercion, isIdCoercion, (<.>) )
import TypeRep		( Type(..), PredType(..) )

import TcMType		( lookupTcTyVar, LookupTyVarResult(..),
                          tcInstSkolType, newKindVar, newMetaTyVar,
			  tcInstBoxy, newBoxyTyVar, newBoxyTyVarTys, readFilledBox, 
			  readMetaTyVar, writeMetaTyVar, newFlexiTyVarTy,
			  tcInstSkolTyVars, tcInstTyVar,
			  zonkTcKind, zonkType, zonkTcType,  zonkTcTyVarsAndFV, 
			  readKindVar, writeKindVar )
import TcSimplify	( tcSimplifyCheck )
import TcEnv		( tcGetGlobalTyVars, findGlobals )
import TcIface		( checkWiredInTyCon )
import TcRnMonad         -- TcType, amongst others
import TcType		( TcKind, TcType, TcTyVar, BoxyTyVar, TcTauType,
			  BoxySigmaType, BoxyRhoType, BoxyType, 
			  TcTyVarSet, TcThetaType, TcTyVarDetails(..), BoxInfo(..), 
			  SkolemInfo( GenSkol, UnkSkol ), MetaDetails(..), isImmutableTyVar,
			  pprSkolTvBinding, isTauTy, isTauTyCon, isSigmaTy, 
			  mkFunTy, mkFunTys, mkTyConApp, isMetaTyVar,
			  tcSplitForAllTys, tcSplitAppTy_maybe, tcSplitFunTys, mkTyVarTys,
			  tyVarsOfType, mkPhiTy, mkTyVarTy, mkPredTy, 
			  typeKind, mkForAllTys, mkAppTy, isBoxyTyVar,
			  tidyOpenType, tidyOpenTyVar, tidyOpenTyVars,
			  pprType, tidyKind, tidySkolemTyVar, isSkolemTyVar, tcView, 
			  TvSubst, mkTvSubst, zipTyEnv, substTy, emptyTvSubst, 
			  lookupTyVar, extendTvSubst )
import Kind		( Kind(..), SimpleKind, KindVar, isArgTypeKind,
			  openTypeKind, liftedTypeKind, mkArrowKind, defaultKind,
			  isOpenTypeKind, argTypeKind, isLiftedTypeKind, isUnliftedTypeKind,
			  isSubKind, pprKind, splitKindFunTys )
import TysPrim		( alphaTy, betaTy )
import Inst		( newDicts, instToId )
import TyCon		( TyCon, tyConArity, tyConTyVars, isSynTyCon )
import TysWiredIn	( listTyCon )
import Id		( Id, mkSysLocal )
import Var		( Var, varName, tyVarKind, isTcTyVar, tcTyVarDetails )
import VarSet		( emptyVarSet, mkVarSet, unitVarSet, unionVarSet, elemVarSet, varSetElems,
			  extendVarSet, intersectsVarSet )
import VarEnv
import Name		( Name, isSystemName )
import ErrUtils		( Message )
import Maybes		( expectJust, isNothing )
import BasicTypes	( Arity )
import UniqSupply	( uniqsFromSupply )
import Util		( notNull, equalLength )
import Outputable

-- Assertion imports
#ifdef DEBUG
import TcType		( isBoxyTy, isFlexi )
#endif
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
	; res_ty <- readFilledBox box	-- Guaranteed filled-in by now
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
	  -> TcM (ExprCoFn, a)
-- Attempt to decompse res_ty to have enough top-level arrows to
-- match the number of patterns in the match group
-- 
-- If (subFunTys n_args res_ty thing_inside) = (co_fn, res)
-- and the inner call to thing_inside passes args: [a1,...,an], b
-- then co_fn :: (a1 -> ... -> an -> b) -> res_ty
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
    loop n args_so_far res_ty
	| Just res_ty' <- tcView res_ty  = loop n args_so_far res_ty'

    loop n args_so_far res_ty
	| isSigmaTy res_ty 	-- Do this before checking n==0, because we 
				-- guarantee to return a BoxyRhoType, not a BoxySigmaType
	= do { (gen_fn, (co_fn, res)) <- tcGen res_ty emptyVarSet $ \ res_ty' ->
					 loop n args_so_far res_ty'
	     ; return (gen_fn <.> co_fn, res) }

    loop 0 args_so_far res_ty 
	= do { res <- thing_inside (reverse args_so_far) res_ty
	     ; return (idCoercion, res) }

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
	     ; (_, mb_unit) <- tryTcErrs $ boxyUnify res_ty (FunTy arg_ty' res_ty')
	     ; if isNothing mb_unit then bale_out args_so_far res_ty
	       else loop n args_so_far (FunTy arg_ty' res_ty') }

    loop n args_so_far (TyVarTy tv)
        | not (isImmutableTyVar tv)
	= do { cts <- readMetaTyVar tv 
	     ; case cts of
		 Indirect ty -> loop n args_so_far ty
		 Flexi -> do { (res_ty:arg_tys) <- withMetaTvs tv kinds mk_res_ty
			     ; res <- thing_inside (reverse args_so_far ++ arg_tys) res_ty
			     ; return (idCoercion, res) } }
	where
	  mk_res_ty (res_ty' : arg_tys') = mkFunTys arg_tys' res_ty'
	  kinds = openTypeKind : take n (repeat argTypeKind)
		-- Note argTypeKind: the args can have an unboxed type,
		-- but not an unboxed tuple.

    loop n args_so_far res_ty = bale_out args_so_far res_ty

    bale_out args_so_far res_ty
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
		  -> TcM [BoxySigmaType]	-- Element types, a b c
  -- It's used for wired-in tycons, so we call checkWiredInTyCOn
  -- Precondition: never called with FunTyCon
  -- Precondition: input type :: *

boxySplitTyConApp tc orig_ty
  = do	{ checkWiredInTyCon tc 
	; loop (tyConArity tc) [] orig_ty }
  where
    loop n_req args_so_far ty 
      | Just ty' <- tcView ty = loop n_req args_so_far ty'

    loop n_req args_so_far (TyConApp tycon args)
      | tc == tycon
      = ASSERT( n_req == length args) 	-- ty::*
	return (args ++ args_so_far)

    loop n_req args_so_far (AppTy fun arg)
      = loop (n_req - 1) (arg:args_so_far) fun

    loop n_req args_so_far (TyVarTy tv)
      | not (isImmutableTyVar tv)
      = do { cts <- readMetaTyVar tv
	   ; case cts of
	       Indirect ty -> loop n_req args_so_far ty
	       Flexi 	   -> do { arg_tys <- withMetaTvs tv arg_kinds mk_res_ty
				 ; return (arg_tys ++ args_so_far) }
	}
      where
	mk_res_ty arg_tys' = mkTyConApp tc arg_tys'
	arg_kinds = map tyVarKind (take n_req (tyConTyVars tc))

    loop _ _ _ = boxySplitFailure (mkTyConApp tc (mkTyVarTys (tyConTyVars tc))) orig_ty

----------------------
boxySplitListTy :: BoxyRhoType -> TcM BoxySigmaType	-- Special case for lists
boxySplitListTy exp_ty = do { [elt_ty] <- boxySplitTyConApp listTyCon exp_ty
			    ; return elt_ty }


----------------------
boxySplitAppTy :: BoxyRhoType				-- Type to split: m a
	       -> TcM (BoxySigmaType, BoxySigmaType)	-- Returns m, a
-- Assumes (m: * -> k), where k is the kind of the incoming type
-- If the incoming type is boxy, then so are the result types; and vice versa

boxySplitAppTy orig_ty
  = loop orig_ty
  where
    loop ty 
      | Just ty' <- tcView ty = loop ty'

    loop ty 
      | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
      = return (fun_ty, arg_ty)

    loop (TyVarTy tv)
      | not (isImmutableTyVar tv)
      = do { cts <- readMetaTyVar tv
	   ; case cts of
	       Indirect ty -> loop ty
	       Flexi       -> do { [fun_ty,arg_ty] <- withMetaTvs tv kinds mk_res_ty
				 ; return (fun_ty, arg_ty) } }
      where
        mk_res_ty [fun_ty', arg_ty'] = mkAppTy fun_ty' arg_ty'
	tv_kind = tyVarKind tv
	kinds = [mkArrowKind liftedTypeKind (defaultKind tv_kind),
					 	-- m :: * -> k
		 liftedTypeKind]		-- arg type :: *
	-- The defaultKind is a bit smelly.  If you remove it,
	-- try compiling	f x = do { x }
	-- and you'll get a kind mis-match.  It smells, but
	-- not enough to lose sleep over.
	
    loop _ = boxySplitFailure (mkAppTy alphaTy betaTy) orig_ty

------------------
boxySplitFailure actual_ty expected_ty
  = unifyMisMatch False False actual_ty expected_ty
	-- "outer" is False, so we don't pop the context
	-- which is what we want since we have not pushed one!
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
	; ty  <- readFilledBox box_tv
  	; return (res, ty) }
\end{code}


%************************************************************************
%*									*
		Approximate boxy matching
%*									*
%************************************************************************

\begin{code}
boxySubMatchType 
	:: TcTyVarSet -> TcType	-- The "template"; the tyvars are skolems
	-> BoxyRhoType		-- Type to match (note a *Rho* type)
	-> TvSubst 		-- Substitution of the [TcTyVar] to BoxySigmaTypes

boxyMatchTypes 
	:: TcTyVarSet -> [TcType] -- The "template"; the tyvars are skolems
	-> [BoxySigmaType]	  -- Type to match
	-> TvSubst 		  -- Substitution of the [TcTyVar] to BoxySigmaTypes

-- Find a *boxy* substitution that makes the template look as much 
-- 	like the BoxySigmaType as possible.  
-- It's always ok to return an empty substitution; 
--	anything more is jam on the pudding
-- 
-- NB1: This is a pure, non-monadic function.  
--	It does no unification, and cannot fail
--
-- Note [Matching kinds]
-- 	The target type might legitimately not be a sub-kind of template.  
--	For example, suppose the target is simply a box with an OpenTypeKind, 
--	and the template is a type variable with LiftedTypeKind.  
--	Then it's ok (because the target type will later be refined).
--	We simply don't bind the template type variable.
--
--	It might also be that the kind mis-match is an error. For example,
--	suppose we match the template (a -> Int) against (Int# -> Int),
--	where the template type variable 'a' has LiftedTypeKind.  This
--	matching function does not fail; it simply doesn't bind the template.
--	Later stuff will fail.
-- 
-- Precondition: the arg lengths are equal
-- Precondition: none of the template type variables appear in the [BoxySigmaType]
-- Precondition: any nested quantifiers in either type differ from 
-- 		 the template type variables passed as arguments
--
-- Note [Sub-match]
-- ~~~~~~~~~~~~~~~~
-- Consider this
-- 	head :: [a] -> a
-- 	|- head xs : <rhobox>
-- We will do a boxySubMatchType between 	a ~ <rhobox>
-- But we *don't* want to match [a |-> <rhobox>] because 
--     (a)	The box should be filled in with a rho-type, but
-- 	but the returned substitution maps TyVars to boxy *sigma*
-- 	types
--     (b) In any case, the right final answer might be *either*
-- 	instantiate 'a' with a rho-type or a sigma type
-- 	   head xs : Int   vs   head xs : forall b. b->b
-- So the matcher MUST NOT make a choice here.   In general, we only
-- bind a template type variable in boxyMatchType, not in boxySubMatchType.
	
boxySubMatchType tmpl_tvs tmpl_ty boxy_ty
  = go tmpl_ty boxy_ty
  where
    go t_ty b_ty 
	| Just t_ty' <- tcView t_ty = go t_ty' b_ty
	| Just b_ty' <- tcView b_ty = go t_ty b_ty'

    go (FunTy arg1 res1) (FunTy arg2 res2)
	= do_match arg1 arg2 (go res1 res2)
		-- Match the args, and sub-match the results

    go (TyVarTy _) b_ty = emptyTvSubst	-- Do not bind!  See Note [Sub-match]

    go t_ty b_ty = do_match t_ty b_ty emptyTvSubst	-- Otherwise we are safe to bind

    do_match t_ty b_ty subst = boxy_match tmpl_tvs t_ty emptyVarSet b_ty subst

------------
boxyMatchTypes tmpl_tvs tmpl_tys boxy_tys
  = ASSERT( length tmpl_tys == length boxy_tys )
    boxy_match_s tmpl_tvs tmpl_tys emptyVarSet boxy_tys emptyTvSubst
	-- ToDo: add error context?

boxy_match_s tmpl_tvs [] boxy_tvs [] subst
  = subst
boxy_match_s tmpl_tvs (t_ty:t_tys) boxy_tvs (b_ty:b_tys) subst
  = boxy_match_s tmpl_tvs t_tys boxy_tvs b_tys $
    boxy_match tmpl_tvs t_ty boxy_tvs b_ty subst

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

    go (ForAllTy _ ty1) (ForAllTy tv2 ty2)
	= boxy_match tmpl_tvs ty1 (boxy_tvs `extendVarSet` tv2) ty2 subst

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
	| tc1 == tc2 = go_s tys1 tys2

    go (FunTy arg1 res1) (FunTy arg2 res2)
	= go_s [arg1,res1] [arg2,res2]

    go t_ty b_ty
	| Just (s1,t1) <- tcSplitAppTy_maybe t_ty,
	  Just (s2,t2) <- tcSplitAppTy_maybe b_ty,
	  typeKind t2 `isSubKind` typeKind t1	-- Maintain invariant
	= go_s [s1,t1] [s2,t2]

    go (TyVarTy tv) b_ty
 	| tv `elemVarSet` tmpl_tvs	-- Template type variable in the template
	, not (intersectsVarSet boxy_tvs (tyVarsOfType orig_boxy_ty))
	, typeKind b_ty `isSubKind` tyVarKind tv
	= extendTvSubst subst tv boxy_ty'
	where
	  boxy_ty' = case lookupTyVar subst tv of
			Nothing -> orig_boxy_ty
			Just ty -> ty `boxyLub` orig_boxy_ty

    go _ _ = subst	-- Always safe

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
      | isTcTyVar tv1, isMetaTyVar tv1 	-- choose ty2 if ty2 is a box
      = ty2	

	-- Look inside type synonyms, but only if the naive version fails
    go ty1 ty2 | Just ty1' <- tcView ty1 = go ty1' ty2
	       | Just ty2' <- tcView ty1 = go ty1 ty2'

    -- For now, we don't look inside ForAlls, PredTys
    go ty1 ty2 = orig_ty1	-- Default
\end{code}


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
	co_fn :: offered_ty -> expected_ty
which takes an HsExpr of type offered_ty into one of type
expected_ty.

\begin{code}
-----------------
tcSubExp :: BoxySigmaType -> BoxySigmaType -> TcM ExprCoFn	-- Locally used only
	-- (tcSub act exp) checks that 
	--	act <= exp
tcSubExp actual_ty expected_ty
  = addErrCtxtM (unifyCtxt actual_ty expected_ty)
		(tc_sub True actual_ty actual_ty expected_ty expected_ty)

tcFunResTy :: Name -> BoxySigmaType -> BoxySigmaType -> TcM ExprCoFn	-- Locally used only
tcFunResTy fun actual_ty expected_ty
  = addErrCtxtM (checkFunResCtxt fun actual_ty expected_ty) $
		(tc_sub True actual_ty actual_ty expected_ty expected_ty)
		   
-----------------
tc_sub :: Outer			-- See comments with uTys
       -> BoxySigmaType		-- actual_ty, before expanding synonyms
       -> BoxySigmaType		-- 		..and after
       -> BoxySigmaType		-- expected_ty, before
       -> BoxySigmaType		-- 		..and after
       -> TcM ExprCoFn

tc_sub outer act_sty act_ty exp_sty exp_ty
  | Just exp_ty' <- tcView exp_ty = tc_sub False act_sty act_ty exp_sty exp_ty'
tc_sub outer act_sty act_ty exp_sty exp_ty
  | Just act_ty' <- tcView act_ty = tc_sub False act_sty act_ty' exp_sty exp_ty

-----------------------------------
-- Rule SBOXY, plus other cases when act_ty is a type variable
-- Just defer to boxy matching
-- This rule takes precedence over SKOL!
tc_sub outer act_sty (TyVarTy tv) exp_sty exp_ty
  = do	{ uVar outer False tv False exp_sty exp_ty
	; return idCoercion }

-----------------------------------
-- Skolemisation case (rule SKOL)
-- 	actual_ty:   d:Eq b => b->b
--	expected_ty: forall a. Ord a => a->a
--	co_fn e      /\a. \d2:Ord a. let d = eqFromOrd d2 in e

-- It is essential to do this *before* the specialisation case
-- Example:  f :: (Eq a => a->a) -> ...
--	     g :: Ord b => b->b
-- Consider  f g !

tc_sub outer act_sty act_ty exp_sty exp_ty
  | isSigmaTy exp_ty
  = do	{ (gen_fn, co_fn) <- tcGen exp_ty act_tvs $ \ body_exp_ty ->
			     tc_sub False act_sty act_ty body_exp_ty body_exp_ty
	; return (gen_fn <.> co_fn) }
  where
    act_tvs = tyVarsOfType act_ty
		-- It's really important to check for escape wrt the free vars of
		-- both expected_ty *and* actual_ty

-----------------------------------
-- Specialisation case (rule ASPEC):
--	actual_ty:   forall a. Ord a => a->a
--	expected_ty: Int -> Int
--	co_fn e =    e Int dOrdInt

tc_sub outer act_sty actual_ty exp_sty expected_ty
  | isSigmaTy actual_ty
  = do	{ (tyvars, theta, tau) <- tcInstBoxy actual_ty
	; dicts <- newDicts InstSigOrigin theta
	; extendLIEs dicts
	; let inst_fn = CoApps (CoTyApps CoHole (mkTyVarTys tyvars)) 
			       (map instToId dicts)
	; co_fn <- tc_sub False tau tau exp_sty expected_ty
	; return (co_fn <.> inst_fn) }

-----------------------------------
-- Function case (rule F1)
tc_sub _ _ (FunTy act_arg act_res) _ (FunTy exp_arg exp_res)
  = tc_sub_funs act_arg act_res exp_arg exp_res

-- Function case (rule F2)
tc_sub outer act_sty act_ty@(FunTy act_arg act_res) exp_sty (TyVarTy exp_tv)
  | isBoxyTyVar exp_tv
  = do	{ cts <- readMetaTyVar exp_tv
	; case cts of
	    Indirect ty -> do { u_tys outer False act_sty act_ty True exp_sty ty
			      ; return idCoercion }
	    Flexi       -> do { [arg_ty,res_ty] <- withMetaTvs exp_tv fun_kinds mk_res_ty
 			      ; tc_sub_funs act_arg act_res arg_ty res_ty } }
 where
    mk_res_ty [arg_ty', res_ty'] = mkFunTy arg_ty' res_ty'
    fun_kinds = [argTypeKind, openTypeKind]

-- Everything else: defer to boxy matching
tc_sub outer act_sty actual_ty exp_sty expected_ty
  = do	{ u_tys outer False act_sty actual_ty False exp_sty expected_ty
	; return idCoercion }


-----------------------------------
tc_sub_funs act_arg act_res exp_arg exp_res
  = do	{ uTys False act_arg False exp_arg
	; co_fn_res <- tc_sub False act_res act_res exp_res exp_res
	; wrapFunResCoercion [exp_arg] co_fn_res }

-----------------------------------
wrapFunResCoercion 
	:: [TcType]	-- Type of args
	-> ExprCoFn 	-- HsExpr a -> HsExpr b
	-> TcM ExprCoFn	-- HsExpr (arg_tys -> a) -> HsExpr (arg_tys -> b)
wrapFunResCoercion arg_tys co_fn_res
  | isIdCoercion co_fn_res = return idCoercion
  | null arg_tys	   = return co_fn_res
  | otherwise 	       
  = do	{ us <- newUniqueSupply
	; let arg_ids = zipWith (mkSysLocal FSLIT("sub")) (uniqsFromSupply us) arg_tys
	; return (CoLams arg_ids (co_fn_res <.> (CoApps CoHole arg_ids))) }
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
      -> (BoxyRhoType -> TcM result)		-- spec_ty
      -> TcM (ExprCoFn, result)
	-- The expression has type: spec_ty -> expected_ty

tcGen expected_ty extra_tvs thing_inside	-- We expect expected_ty to be a forall-type
						-- If not, the call is a no-op
  = do	{	-- We want the GenSkol info in the skolemised type variables to 
		-- mention the *instantiated* tyvar names, so that we get a
		-- good error message "Rigid variable 'a' is bound by (forall a. a->a)"
		-- Hence the tiresome but innocuous fixM
	  ((forall_tvs, theta, rho_ty), skol_info) <- fixM (\ ~(_, skol_info) ->
		do { (forall_tvs, theta, rho_ty) <- tcInstSkolType skol_info expected_ty
		   ; span <- getSrcSpanM
		   ; let skol_info = GenSkol forall_tvs (mkPhiTy theta rho_ty) span
		   ; return ((forall_tvs, theta, rho_ty), skol_info) })

#ifdef DEBUG
	; traceTc (text "tcGen" <+> vcat [text "extra_tvs" <+> ppr extra_tvs,
				    text "expected_ty" <+> ppr expected_ty,
				    text "inst ty" <+> ppr forall_tvs <+> ppr theta <+> ppr rho_ty,
				    text "free_tvs" <+> ppr free_tvs,
				    text "forall_tvs" <+> ppr forall_tvs])
#endif

	-- Type-check the arg and unify with poly type
	; (result, lie) <- getLIE (thing_inside rho_ty)

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

	; dicts <- newDicts (SigOrigin skol_info) theta
	; inst_binds <- tcSimplifyCheck sig_msg forall_tvs dicts lie

	; checkSigTyVarsWrt free_tvs forall_tvs
	; traceTc (text "tcGen:done")

	; let
	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
		dict_ids   = map instToId dicts
		co_fn = CoTyLams forall_tvs $ CoLams dict_ids $ CoLet inst_binds CoHole 
	; returnM (co_fn, result) }
  where
    free_tvs = tyVarsOfType expected_ty `unionVarSet` extra_tvs
    sig_msg  = ptext SLIT("expected type of an expression")
\end{code}    

    

%************************************************************************
%*									*
		Boxy unification
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

\begin{code}
boxyUnify :: BoxyType -> BoxyType -> TcM ()
-- Acutal and expected, respectively
boxyUnify ty1 ty2 
  = addErrCtxtM (unifyCtxt ty1 ty2) $
    uTysOuter False ty1 False ty2

---------------
boxyUnifyList :: [BoxyType] -> [BoxyType] -> TcM ()
-- Arguments should have equal length
-- Acutal and expected types
boxyUnifyList tys1 tys2 = uList boxyUnify tys1 tys2

---------------
unifyType :: TcTauType -> TcTauType -> TcM ()
-- No boxes expected inside these types
-- Acutal and expected types
unifyType ty1 ty2 	-- ty1 expected, ty2 inferred
  = ASSERT2( not (isBoxyTy ty1), ppr ty1 )
    ASSERT2( not (isBoxyTy ty2), ppr ty2 )
    addErrCtxtM (unifyCtxt ty1 ty2) $
    uTysOuter True ty1 True ty2

---------------
unifyPred :: PredType -> PredType -> TcM ()
-- Acutal and expected types
unifyPred p1 p2 = addErrCtxtM (unifyCtxt (mkPredTy p1) (mkPredTy p2)) $
		  uPred True True p1 True p2

unifyTheta :: TcThetaType -> TcThetaType -> TcM ()
-- Acutal and expected types
unifyTheta theta1 theta2
  = do	{ checkTc (equalLength theta1 theta2)
		  (ptext SLIT("Contexts differ in length"))
	; uList unifyPred theta1 theta2 }

---------------
uList :: (a -> a -> TcM ())
       -> [a] -> [a] -> TcM ()
-- Unify corresponding elements of two lists of types, which
-- should be f equal length.  We charge down the list explicitly so that
-- we can complain if their lengths differ.
uList unify []         []	  = return ()
uList unify (ty1:tys1) (ty2:tys2) = do { unify ty1 ty2; uList unify tys1 tys2 }
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

@uTys@ is the heart of the unifier.  Each arg happens twice, because
we want to report errors in terms of synomyms if poss.  The first of
the pair is used in error messages only; it is always the same as the
second, except that if the first is a synonym then the second may be a
de-synonym'd version.  This way we get better error messages.

We call the first one \tr{ps_ty1}, \tr{ps_ty2} for ``possible synomym''.

\begin{code}
type NoBoxes = Bool	-- True  <=> definitely no boxes in this type
			-- False <=> there might be boxes (always safe)

type Outer = Bool	-- True <=> this is the outer level of a unification
			--	    so that the types being unified are the
			--	    very ones we began with, not some sub
			--	    component or synonym expansion
-- The idea is that if Outer is true then unifyMisMatch should
-- pop the context to remove the "Expected/Acutal" context

uTysOuter, uTys
     :: NoBoxes -> TcType	-- ty1 is the *expected* type
     -> NoBoxes -> TcType	-- ty2 is the *actual* type
     -> TcM ()
uTysOuter nb1 ty1 nb2 ty2 = u_tys True nb1 ty1 ty1 nb2 ty2 ty2
uTys      nb1 ty1 nb2 ty2 = u_tys False nb1 ty1 ty1 nb2 ty2 ty2


--------------
uTys_s :: NoBoxes -> [TcType]	-- ty1 is the *actual* types
       -> NoBoxes -> [TcType]	-- ty2 is the *expected* types
       -> TcM ()
uTys_s nb1 []	 	nb2 []	       = returnM ()
uTys_s nb1 (ty1:tys1) nb2 (ty2:tys2) = do { uTys nb1 ty1 nb2 ty2
					  ; uTys_s nb1 tys1 nb2 tys2 }
uTys_s nb1 ty1s nb2 ty2s = panic "Unify.uTys_s: mismatched type lists!"

--------------
u_tys :: Outer
      -> NoBoxes -> TcType -> TcType	-- ty1 is the *actual* type
      -> NoBoxes -> TcType -> TcType	-- ty2 is the *expected* type
      -> TcM ()

u_tys outer nb1 orig_ty1 ty1 nb2 orig_ty2 ty2
  = go outer ty1 ty2
  where 

	-- Always expand synonyms (see notes at end)
        -- (this also throws away FTVs)
    go outer ty1 ty2 
      | Just ty1' <- tcView ty1 = go False ty1' ty2
      | Just ty2' <- tcView ty2 = go False ty1 ty2'

	-- Variables; go for uVar
    go outer (TyVarTy tyvar1) ty2 = uVar outer False tyvar1 nb2 orig_ty2 ty2
    go outer ty1 (TyVarTy tyvar2) = uVar outer True  tyvar2 nb1 orig_ty1 ty1
				-- "True" means args swapped
	-- Predicates
    go outer (PredTy p1) (PredTy p2) = uPred outer nb1 p1 nb2 p2

	-- Type constructors must match
    go _ (TyConApp con1 tys1) (TyConApp con2 tys2)
      | con1 == con2 = uTys_s nb1 tys1 nb2 tys2
	-- See Note [TyCon app]

	-- Functions; just check the two parts
    go _ (FunTy fun1 arg1) (FunTy fun2 arg2)
      = do { uTys nb1 fun1 nb2 fun2
	   ; uTys nb1 arg1 nb2 arg2 }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
    go outer (AppTy s1 t1) ty2
      | Just (s2,t2) <- tcSplitAppTy_maybe ty2
      = do { uTys nb1 s1 nb2 s2; uTys nb1 t1 nb2 t2 }

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
    go outer ty1 (AppTy s2 t2)
      | Just (s1,t1) <- tcSplitAppTy_maybe ty1
      = do { uTys nb1 s1 nb2 s2; uTys nb1 t1 nb2 t2 }

    go _ ty1@(ForAllTy _ _) ty2@(ForAllTy _ _)
      | length tvs1 == length tvs2
      = do   { tvs <- tcInstSkolTyVars UnkSkol tvs1	-- Not a helpful SkolemInfo
	     ; let tys      = mkTyVarTys tvs
	           in_scope = mkInScopeSet (mkVarSet tvs)
 	           subst1   = mkTvSubst in_scope (zipTyEnv tvs1 tys)
	           subst2   = mkTvSubst in_scope (zipTyEnv tvs2 tys)
	     ; uTys nb1 (substTy subst1 body1) nb2 (substTy subst2 body2)

		-- If both sides are inside a box, we should not have
		-- a polytype at all.  This check comes last, because
		-- the error message is extremely unhelpful.
	     ; ifM (nb1 && nb2) (notMonoType ty1)
	     }
      where
	(tvs1, body1) = tcSplitForAllTys ty1
	(tvs2, body2) = tcSplitForAllTys ty2

	-- Anything else fails
    go outer _ _ = unifyMisMatch outer False orig_ty1 orig_ty2

----------
uPred outer nb1 (IParam n1 t1) nb2 (IParam n2 t2)
  | n1 == n2 = uTys nb1 t1 nb2 t2
uPred outer nb1 (ClassP c1 tys1) nb2 (ClassP c2 tys2)
  | c1 == c2 = uTys_s nb1 tys1 nb2 tys2		-- Guaranteed equal lengths because the kinds check
uPred outer _ p1 _ p2 = unifyMisMatch outer False (mkPredTy p1) (mkPredTy p2)
\end{code}

Note [Tycon app]
~~~~~~~~~~~~~~~~
When we find two TyConApps, the argument lists are guaranteed equal
length.  Reason: intially the kinds of the two types to be unified is
the same. The only way it can become not the same is when unifying two
AppTys (f1 a1):=:(f2 a2).  In that case there can't be a TyConApp in
the f1,f2 (because it'd absorb the app).  If we unify f1:=:f2 first,
which we do, that ensures that f1,f2 have the same kind; and that
means a1,a2 have the same kind.  And now the argument repeats.


Notes on synonyms
~~~~~~~~~~~~~~~~~
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
     -> Bool		-- False => tyvar is the "expected"
			-- True  => ty    is the "expected" thing
     -> TcTyVar
     -> NoBoxes		-- True <=> definitely no boxes in t2
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM ()

uVar outer swapped tv1 nb2 ps_ty2 ty2
  = do 	{ let expansion | showSDoc (ppr ty2) == showSDoc (ppr ps_ty2) = empty
			| otherwise = brackets (equals <+> ppr ty2)
	; traceTc (text "uVar" <+> ppr swapped <+> 
			sep [ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1 ),
				nest 2 (ptext SLIT(" :=: ")),
			     ppr ps_ty2 <+> dcolon <+> ppr (typeKind ty2) <+> expansion])
	; details <- lookupTcTyVar tv1
	; case details of
	    IndirectTv ty1 
		| swapped   -> u_tys outer nb2  ps_ty2 ty2 True ty1    ty1	-- Swap back
		| otherwise -> u_tys outer True ty1    ty1 nb2  ps_ty2 ty2	-- Same order
			-- The 'True' here says that ty1 
			-- is definitely box-free
	    DoneTv details1 -> uUnfilledVar outer swapped tv1 details1 nb2 ps_ty2 ty2
	}

----------------
uUnfilledVar :: Outer
	     -> Bool 				-- Args are swapped
     	     -> TcTyVar -> TcTyVarDetails		-- Tyvar 1
     	     -> NoBoxes -> TcTauType -> TcTauType	-- Type 2
     	     -> TcM ()
-- Invariant: tyvar 1 is not unified with anything

uUnfilledVar outer swapped tv1 details1 nb2 ps_ty2 ty2
  | Just ty2' <- tcView ty2
  = 	-- Expand synonyms; ignore FTVs
    uUnfilledVar False swapped tv1 details1 nb2 ps_ty2 ty2'

uUnfilledVar outer swapped tv1 details1 nb2 ps_ty2 ty2@(TyVarTy tv2)
	-- Same type variable => no-op
  | tv1 == tv2
  = returnM ()

	-- Distinct type variables
  | otherwise
  = do	{ lookup2 <- lookupTcTyVar tv2
	; case lookup2 of
	    IndirectTv ty2' -> uUnfilledVar  outer swapped tv1 details1 True ty2' ty2'
	    DoneTv details2 -> uUnfilledVars outer swapped tv1 details1 tv2 details2
	}

uUnfilledVar outer swapped tv1 details1 nb2 ps_ty2 non_var_ty2	-- ty2 is not a type variable
  = case details1 of
	MetaTv (SigTv _) ref1 -> mis_match	-- Can't update a skolem with a non-type-variable
	MetaTv info ref1      -> uMetaVar swapped tv1 info ref1 nb2 ps_ty2 non_var_ty2
	skolem_details        -> mis_match
  where
    mis_match = unifyMisMatch outer swapped (TyVarTy tv1) ps_ty2

----------------
uMetaVar :: Bool
	 -> TcTyVar -> BoxInfo -> IORef MetaDetails
	 -> NoBoxes -> TcType -> TcType
	 -> TcM ()
-- tv1 is an un-filled-in meta type variable (maybe boxy, maybe tau)
-- ty2 is not a type variable

uMetaVar swapped tv1 info1 ref1 nb2 ps_ty2 non_var_ty2
  = do	{ final_ty <- case info1 of
			BoxTv -> unBox ps_ty2			-- No occurs check
			other -> checkTauTvUpdate tv1 ps_ty2	-- Occurs check + monotype check
	; checkUpdateMeta swapped tv1 ref1 final_ty }

----------------
uUnfilledVars :: Outer
	      -> Bool 			-- Args are swapped
      	      -> TcTyVar -> TcTyVarDetails	-- Tyvar 1
      	      -> TcTyVar -> TcTyVarDetails	-- Tyvar 2
      	      -> TcM ()
-- Invarant: The type variables are distinct, 
-- 	     Neither is filled in yet
--    	     They might be boxy or not

uUnfilledVars outer swapped tv1 (SkolemTv _) tv2 (SkolemTv _)
  = unifyMisMatch outer swapped (mkTyVarTy tv1) (mkTyVarTy tv2)

uUnfilledVars outer swapped tv1 (MetaTv info1 ref1) tv2 (SkolemTv _)
  = checkUpdateMeta swapped tv1 ref1 (mkTyVarTy tv2)
uUnfilledVars outer swapped tv1 (SkolemTv _) tv2 (MetaTv info2 ref2)
  = checkUpdateMeta (not swapped) tv2 ref2 (mkTyVarTy tv1)

-- ToDo: this function seems too long for what it acutally does!
uUnfilledVars outer swapped tv1 (MetaTv info1 ref1) tv2 (MetaTv info2 ref2)
  = case (info1, info2) of
	(BoxTv,   BoxTv)   -> box_meets_box

	-- If a box meets a TauTv, but the fomer has the smaller kind
	-- then we must create a fresh TauTv with the smaller kind
	(_,       BoxTv)   | k1_sub_k2 -> update_tv2
			   | otherwise -> box_meets_box
	(BoxTv,   _    )   | k2_sub_k1 -> update_tv1
			   | otherwise -> box_meets_box

	-- Avoid SigTvs if poss
	(SigTv _, _      ) | k1_sub_k2 -> update_tv2
	(_,       SigTv _) | k2_sub_k1 -> update_tv1

	(_,   _) | k1_sub_k2 -> if k2_sub_k1 && nicer_to_update_tv1
				then update_tv1 	-- Same kinds
				else update_tv2
		 | k2_sub_k1 -> update_tv1
		 | otherwise -> kind_err 

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

    nicer_to_update_tv1 = isSystemName (varName tv1)
	-- Try to update sys-y type variables in preference to ones
	-- gotten (say) by instantiating a polymorphic function with
	-- a user-written type sig
	
----------------
checkUpdateMeta :: Bool -> TcTyVar -> IORef MetaDetails -> TcType -> TcM ()
-- Update tv1, which is flexi; occurs check is alrady done
-- The 'check' version does a kind check too
-- We do a sub-kind check here: we might unify (a b) with (c d) 
--	where b::*->* and d::*; this should fail

checkUpdateMeta swapped tv1 ref1 ty2
  = do	{ checkKinds swapped tv1 ty2
	; updateMeta tv1 ref1 ty2 }

updateMeta :: TcTyVar -> IORef MetaDetails -> TcType -> TcM ()
updateMeta tv1 ref1 ty2
  = ASSERT( isMetaTyVar tv1 )
    ASSERT( isBoxyTyVar tv1 || isTauTy ty2 )
    do	{ ASSERTM2( do { details <- readMetaTyVar tv1; return (isFlexi details) }, ppr tv1 )
	; traceTc (text "updateMeta" <+> ppr tv1 <+> text ":=" <+> ppr ty2)
	; writeMutVar ref1 (Indirect ty2) }

----------------
checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- ty2 has been zonked at this stage, which ensures that
-- its kind has as much boxity information visible as possible.
  | tk2 `isSubKind` tk1 = returnM ()

  | otherwise
	-- Either the kinds aren't compatible
	--	(can happen if we unify (a b) with (c d))
	-- or we are unifying a lifted type variable with an
	-- 	unlifted type: e.g.  (id 3#) is illegal
  = addErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$
    unifyKindMisMatch k1 k2
  where
    (k1,k2) | swapped   = (tk2,tk1)
	    | otherwise = (tk1,tk2)
    tk1 = tyVarKind tv1
    tk2 = typeKind ty2

----------------
checkTauTvUpdate :: TcTyVar -> TcType -> TcM TcType
--    (checkTauTvUpdate tv ty)
-- We are about to update the TauTv tv with ty.
-- Check (a) that tv doesn't occur in ty (occurs check)
--	 (b) that ty is a monotype
-- Furthermore, in the interest of (b), if you find an
-- empty box (BoxTv that is Flexi), fill it in with a TauTv
-- 
-- Returns the (non-boxy) type to update the type variable with, or fails

checkTauTvUpdate orig_tv orig_ty
  = go orig_ty
  where
    go (TyConApp tc tys)
	| isSynTyCon tc  = go_syn tc tys
	| otherwise	 = do { tys' <- mappM go tys; return (TyConApp tc tys') }
    go (NoteTy _ ty2) 	 = go ty2	-- Discard free-tyvar annotations
    go (PredTy p)	 = do { p' <- go_pred p; return (PredTy p') }
    go (FunTy arg res)   = do { arg' <- go arg; res' <- go res; return (FunTy arg' res') }
    go (AppTy fun arg)	 = do { fun' <- go fun; arg' <- go arg; return (mkAppTy fun' arg') }
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.
    go (ForAllTy tv ty) = notMonoType orig_ty		-- (b)

    go (TyVarTy tv)
	| orig_tv == tv = occurCheck tv orig_ty		-- (a)
	| isTcTyVar tv  = go_tyvar tv (tcTyVarDetails tv)
	| otherwise     = return (TyVarTy tv)
		 -- Ordinary (non Tc) tyvars
		 -- occur inside quantified types

    go_pred (ClassP c tys) = do { tys' <- mapM go tys; return (ClassP c tys') }
    go_pred (IParam n ty)  = do { ty' <- go ty;        return (IParam n ty') }

    go_tyvar tv (SkolemTv _) = return (TyVarTy tv)
    go_tyvar tv (MetaTv box ref)
	= do { cts <- readMutVar ref
	     ; case cts of
		  Indirect ty -> go ty 
		  Flexi -> case box of
				BoxTv -> fillBoxWithTau tv ref
				other -> return (TyVarTy tv)
	     }

	-- go_syn is called for synonyms only
	-- See Note [Type synonyms and the occur check]
    go_syn tc tys
	| not (isTauTyCon tc)
	= notMonoType orig_ty	-- (b) again
	| otherwise
	= do { (msgs, mb_tys') <- tryTc (mapM go tys)
	     ; case mb_tys' of
		Just tys' -> return (TyConApp tc tys')
				-- Retain the synonym (the common case)
		Nothing   -> go (expectJust "checkTauTvUpdate" 
					(tcView (TyConApp tc tys)))
				-- Try again, expanding the synonym
	     }

fillBoxWithTau :: BoxyTyVar -> IORef MetaDetails -> TcM TcType
-- (fillBoxWithTau tv ref) fills ref with a freshly allocated 
--  tau-type meta-variable, whose print-name is the same as tv
-- Choosing the same name is good: when we instantiate a function
-- we allocate boxy tyvars with the same print-name as the quantified
-- tyvar; and then we often fill the box with a tau-tyvar, and again
-- we want to choose the same name.
fillBoxWithTau tv ref 
  = do	{ tv' <- tcInstTyVar tv		-- Do not gratuitously forget
	; let tau = mkTyVarTy tv'	-- name of the type variable
	; writeMutVar ref (Indirect tau)
	; return tau }
\end{code}

Note [Type synonyms and the occur check]
~~~~~~~~~~~~~~~~~~~~
Basically we want to update     tv1 := ps_ty2
because ps_ty2 has type-synonym info, which improves later error messages

But consider 
	type A a = ()

	f :: (A a -> a -> ()) -> ()
	f = \ _ -> ()

	x :: ()
	x = f (\ x p -> p x)

In the application (p x), we try to match "t" with "A t".  If we go
ahead and bind t to A t (= ps_ty2), we'll lead the type checker into 
an infinite loop later.
But we should not reject the program, because A t = ().
Rather, we should bind t to () (= non_var_ty2).

\begin{code}
stripBoxyType :: BoxyType -> TcM TcType
-- Strip all boxes from the input type, returning a non-boxy type.
-- It's fine for there to be a polytype inside a box (c.f. unBox)
-- All of the boxes should have been filled in by now; 
-- hence we return a TcType
stripBoxyType ty = zonkType strip_tv ty
  where
    strip_tv tv = ASSERT( not (isBoxyTyVar tv) ) return (TyVarTy tv)
	-- strip_tv will be called for *Flexi* meta-tyvars
	-- There should not be any Boxy ones; hence the ASSERT

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
-- It remove all boxes from the input type, returning a non-boxy type.
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
	    Flexi	-> fillBoxWithTau tv ref
	    Indirect ty -> do { non_boxy_ty <- unBox ty
			      ; if isTauTy non_boxy_ty 
				then return non_boxy_ty
				else notMonoType non_boxy_ty }
	}
  | otherwise 	-- Skolems, and meta-tau-variables
  = return (TyVarTy tv)

unBoxPred (ClassP cls tys) = do { tys' <- mapM unBox tys; return (ClassP cls tys') }
unBoxPred (IParam ip ty)   = do { ty' <- unBox ty; return (IParam ip ty') }
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
checkFunResCtxt fun actual_res_ty expected_res_ty tidy_env
  = do	{ exp_ty' <- zonkTcType expected_res_ty
	; act_ty' <- zonkTcType actual_res_ty
	; let
	      (env1, exp_ty'') = tidyOpenType tidy_env exp_ty'
	      (env2, act_ty'') = tidyOpenType env1     act_ty'
	      (exp_args, _)    = tcSplitFunTys exp_ty''
	      (act_args, _)    = tcSplitFunTys act_ty''
	
	      len_act_args     = length act_args
	      len_exp_args     = length exp_args

	      message | len_exp_args < len_act_args = wrongArgsCtxt "too few"  fun
	              | len_exp_args > len_act_args = wrongArgsCtxt "too many" fun
		      | otherwise		    = mkExpectedActualMsg act_ty'' exp_ty''
	; return (env2, message) }

  where
    wrongArgsCtxt too_many_or_few fun
      = ptext SLIT("Probable cause:") <+> quotes (ppr fun)
	<+> ptext SLIT("is applied to") <+> text too_many_or_few 
	<+> ptext SLIT("arguments")

------------------
unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
	-- tv1 and ty2 are zonked already
  = returnM msg
  where
    msg = (env2, ptext SLIT("When matching the kinds of") <+> 
		 sep [quotes pp_expected <+> ptext SLIT("and"), quotes pp_actual])

    (pp_expected, pp_actual) | swapped   = (pp2, pp1)
		             | otherwise = (pp1, pp2)
    (env1, tv1') = tidyOpenTyVar tidy_env tv1
    (env2, ty2') = tidyOpenType  env1 ty2
    pp1 = ppr tv1' <+> dcolon <+> ppr (tyVarKind tv1)
    pp2 = ppr ty2' <+> dcolon <+> ppr (typeKind ty2)

unifyMisMatch outer swapped ty1 ty2
  = do	{ (env, msg) <- if swapped then misMatchMsg ty1 ty2
				   else misMatchMsg ty2 ty1

	-- This is the whole point of the 'outer' stuff
	; if outer then popErrCtxt (failWithTcM (env, msg))
	  	   else failWithTcM (env, msg)
	} 

misMatchMsg ty1 ty2
  = do	{ env0 <- tcInitTidyEnv
	; (env1, pp1, extra1) <- ppr_ty env0 ty1
	; (env2, pp2, extra2) <- ppr_ty env1 ty2
	; return (env2, sep [sep [ptext SLIT("Couldn't match expected type") <+> pp1, 
				  nest 7 (ptext SLIT("against inferred type") <+> pp2)],
			     nest 2 extra1, nest 2 extra2]) }

ppr_ty :: TidyEnv -> TcType -> TcM (TidyEnv, SDoc, SDoc)
ppr_ty env ty
  = do { ty' <- zonkTcType ty
       ; let (env1,tidy_ty) = tidyOpenType env ty'
	     simple_result  = (env1, quotes (ppr tidy_ty), empty)
       ; case tidy_ty of
	   TyVarTy tv 
		| isSkolemTyVar tv -> return (env2, pp_rigid tv',
					      pprSkolTvBinding tv')
		| otherwise -> return simple_result
		where
		  (env2, tv') = tidySkolemTyVar env1 tv
	   other -> return simple_result }
  where
    pp_rigid tv = quotes (ppr tv) <+> parens (ptext SLIT("a rigid variable"))


notMonoType ty
  = do	{ ty' <- zonkTcType ty
	; env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty'
	      msg = ptext SLIT("Cannot match a monotype with") <+> ppr tidy_ty
	; failWithTcM (env1, msg) }

occurCheck tyvar ty
  = do	{ env0 <- tcInitTidyEnv
	; ty'  <- zonkTcType ty
	; let (env1, tidy_tyvar) = tidyOpenTyVar env0 tyvar
	      (env2, tidy_ty)    = tidyOpenType  env1 ty'
	      extra = sep [ppr tidy_tyvar, char '=', ppr tidy_ty]
	; failWithTcM (env2, hang msg 2 extra) }
  where
    msg = ptext SLIT("Occurs check: cannot construct the infinite type:")
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
unifyKind LiftedTypeKind   LiftedTypeKind   = returnM ()
unifyKind UnliftedTypeKind UnliftedTypeKind = returnM ()

unifyKind OpenTypeKind k2 | isOpenTypeKind k2 = returnM ()
unifyKind ArgTypeKind  k2 | isArgTypeKind k2    = returnM ()
  -- Respect sub-kinding

unifyKind (FunKind a1 r1) (FunKind a2 r2)
 = do { unifyKind a2 a1; unifyKind r1 r2 }
		-- Notice the flip in the argument,
		-- so that the sub-kinding works right

unifyKind (KindVar kv1) k2 = uKVar False kv1 k2
unifyKind k1 (KindVar kv2) = uKVar True kv2 k1
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
	    Nothing -> uUnboundKVar swapped kv1 k2
	    Just k1 | swapped   -> unifyKind k2 k1
		    | otherwise -> unifyKind k1 k2 }

----------------
uUnboundKVar :: Bool -> KindVar -> TcKind -> TcM ()
uUnboundKVar swapped kv1 k2@(KindVar kv2)
  | kv1 == kv2 = returnM ()
  | otherwise	-- Distinct kind variables
  = do	{ mb_k2 <- readKindVar kv2
	; case mb_k2 of
	    Just k2 -> uUnboundKVar swapped kv1 k2
	    Nothing -> writeKindVar kv1 k2 }

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
    not_in (KindVar kv2)   = kv1 /= kv2
    not_in (FunKind a2 r2) = not_in a2 && not_in r2
    not_in other	   = True

kindSimpleKind :: Bool -> Kind -> TcM SimpleKind
-- (kindSimpleKind True k) returns a simple kind sk such that sk <: k
-- If the flag is False, it requires k <: sk
-- E.g. 	kindSimpleKind False ?? = *
-- What about (kv -> *) :=: ?? -> *
kindSimpleKind orig_swapped orig_kind
  = go orig_swapped orig_kind
  where
    go sw (FunKind k1 k2) = do { k1' <- go (not sw) k1
			       ; k2' <- go sw k2
			       ; return (FunKind k1' k2') }
    go True OpenTypeKind = return liftedTypeKind
    go True ArgTypeKind  = return liftedTypeKind
    go sw LiftedTypeKind  = return liftedTypeKind
    go sw k@(KindVar _)	  = return k	-- KindVars are always simple
    go swapped kind = failWithTc (ptext SLIT("Unexpected kind unification failure:")
				  <+> ppr orig_swapped <+> ppr orig_kind)
	-- I think this can't actually happen

-- T v = MkT v		 v must be a type 
-- T v w = MkT (v -> w)	 v must not be an umboxed tuple

----------------
kindOccurCheckErr tyvar ty
  = hang (ptext SLIT("Occurs check: cannot construct the infinite kind:"))
       2 (sep [ppr tyvar, char '=', ppr ty])

unifyKindMisMatch ty1 ty2
  = zonkTcKind ty1	`thenM` \ ty1' ->
    zonkTcKind ty2	`thenM` \ ty2' ->
    let
	msg = hang (ptext SLIT("Couldn't match kind"))
		   2 (sep [quotes (ppr ty1'), 
			   ptext SLIT("against"), 
			   quotes (ppr ty2')])
    in
    failWithTc msg
\end{code}

\begin{code}
unifyFunKind :: TcKind -> TcM (Maybe (TcKind, TcKind))
-- Like unifyFunTy, but does not fail; instead just returns Nothing

unifyFunKind (KindVar kvar)
  = readKindVar kvar	`thenM` \ maybe_kind ->
    case maybe_kind of
	Just fun_kind -> unifyFunKind fun_kind
	Nothing       -> do { arg_kind <- newKindVar
			    ; res_kind <- newKindVar
			    ; writeKindVar kvar (mkArrowKind arg_kind res_kind)
			    ; returnM (Just (arg_kind,res_kind)) }
    
unifyFunKind (FunKind arg_kind res_kind) = returnM (Just (arg_kind,res_kind))
unifyFunKind other	   	         = returnM Nothing
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
