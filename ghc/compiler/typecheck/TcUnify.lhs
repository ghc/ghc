%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Type subsumption and unification}

\begin{code}
module TcUnify (
	-- Full-blown subsumption
  tcSub, tcGen, subFunTy,
  checkSigTyVars, sigCtxt, sigPatCtxt,

	-- Various unifications
  unifyTauTy, unifyTauTyList, unifyTauTyLists, 
  unifyFunTy, unifyListTy, unifyPArrTy, unifyTupleTy,
  unifyKind, unifyKinds, unifyOpenTypeKind,

	-- Coercions
  Coercion, ExprCoFn, PatCoFn, 
  (<$>), (<.>), mkCoercion, 
  idCoercion, isIdCoercion

  ) where

#include "HsVersions.h"


import HsSyn		( HsExpr(..) )
import TcHsSyn		( TypecheckedHsExpr, TcPat, 
			  mkHsDictApp, mkHsTyApp, mkHsLet )
import TypeRep		( Type(..), SourceType(..), TyNote(..),
			  openKindCon, typeCon )

import TcMonad          -- TcType, amongst others
import TcType		( TcKind, TcType, TcSigmaType, TcPhiType, TcTyVar, TcTauType,
			  TcTyVarSet, TcThetaType,
			  isTauTy, isSigmaTy, 
			  tcSplitAppTy_maybe, tcSplitTyConApp_maybe, 
			  tcGetTyVar_maybe, tcGetTyVar, 
			  mkTyConApp, mkTyVarTys, mkFunTy, tyVarsOfType, mkRhoTy,
			  typeKind, tcSplitFunTy_maybe, mkForAllTys,
			  isHoleTyVar, isSkolemTyVar, isUserTyVar, allDistinctTyVars, 
			  tidyOpenType, tidyOpenTypes, tidyOpenTyVar, tidyOpenTyVars,
			  eqKind, openTypeKind, liftedTypeKind, isTypeKind,
			  hasMoreBoxityInfo, tyVarBindingInfo
			)
import qualified Type	( getTyVar_maybe )
import Inst		( LIE, emptyLIE, plusLIE, mkLIE, 
			  newDicts, instToId
			)
import TcMType		( getTcTyVar, putTcTyVar, tcInstType, 
			  newTyVarTy, newTyVarTys, newBoxityVar, newHoleTyVarTy,
			  zonkTcType, zonkTcTyVars, zonkTcTyVar )
import TcSimplify	( tcSimplifyCheck )
import TysWiredIn	( listTyCon, parrTyCon, mkListTy, mkPArrTy, mkTupleTy )
import TcEnv		( TcTyThing(..), tcExtendGlobalTyVars, tcGetGlobalTyVars, tcLEnvElts )
import TyCon		( tyConArity, isTupleTyCon, tupleTyConBoxity )
import PprType		( pprType )
import CoreFVs		( idFreeTyVars )
import Id		( mkSysLocal, idType )
import Var		( Var, varName, tyVarKind )
import VarSet		( elemVarSet, varSetElems )
import VarEnv
import Name		( isSystemName, getSrcLoc )
import ErrUtils		( Message )
import BasicTypes	( Boxity, Arity, isBoxed )
import Util		( isSingleton, equalLength )
import Maybe		( isNothing )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Subsumption}
%*									*
%************************************************************************

\begin{code}
tcSub :: TcSigmaType		-- expected_ty; can be a type scheme;
				--		can be a "hole" type variable
      -> TcSigmaType		-- actual_ty; can be a type scheme
      -> TcM (ExprCoFn, LIE)
\end{code}

(tcSub expected_ty actual_ty) checks that 
	actual_ty <= expected_ty
That is, that a value of type actual_ty is acceptable in
a place expecting a value of type expected_ty.

It returns a coercion function 
	co_fn :: actual_ty -> expected_ty
which takes an HsExpr of type actual_ty into one of type
expected_ty.

\begin{code}
tcSub expected_ty actual_ty
  = traceTc (text "tcSub" <+> details)		`thenNF_Tc_`
    tcAddErrCtxtM (unifyCtxt "type" expected_ty actual_ty)
		  (tc_sub expected_ty expected_ty actual_ty actual_ty)
  where
    details = vcat [text "Expected:" <+> ppr expected_ty,
		    text "Actual:  " <+> ppr actual_ty]
\end{code}

tc_sub carries the types before and after expanding type synonyms

\begin{code}
tc_sub :: TcSigmaType		-- expected_ty, before expanding synonyms
       -> TcSigmaType		-- 		..and after
       -> TcSigmaType		-- actual_ty, before
       -> TcSigmaType		-- 		..and after
       -> TcM (ExprCoFn, LIE)

-----------------------------------
-- Expand synonyms
tc_sub exp_sty (NoteTy _ exp_ty) act_sty act_ty = tc_sub exp_sty exp_ty act_sty act_ty
tc_sub exp_sty exp_ty act_sty (NoteTy _ act_ty) = tc_sub exp_sty exp_ty act_sty act_ty

-----------------------------------
-- "Hole type variable" case
-- Do this case before unwrapping for-alls in the actual_ty

tc_sub _ (TyVarTy tv) act_sty act_ty
  | isHoleTyVar tv
  = 	-- It's a "hole" type variable
    getTcTyVar tv	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of

	Just ty ->    	-- Already been assigned
		    tc_sub ty ty act_sty act_ty ;

	Nothing ->	-- Assign it
		    putTcTyVar tv act_sty		`thenNF_Tc_`
		    returnTc (idCoercion, emptyLIE)


-----------------------------------
-- Generalisation case
-- 	actual_ty:   d:Eq b => b->b
--	expected_ty: forall a. Ord a => a->a
--	co_fn e      /\a. \d2:Ord a. let d = eqFromOrd d2 in e

-- It is essential to do this *before* the specialisation case
-- Example:  f :: (Eq a => a->a) -> ...
--	     g :: Ord b => b->b
-- Consider  f g !

tc_sub exp_sty expected_ty act_sty actual_ty
  | isSigmaTy expected_ty
  = tcGen expected_ty (
	\ body_exp_ty -> tc_sub body_exp_ty body_exp_ty act_sty actual_ty
    )				`thenTc` \ (gen_fn, co_fn, lie) ->
    returnTc (gen_fn <.> co_fn, lie)

-----------------------------------
-- Specialisation case:
--	actual_ty:   forall a. Ord a => a->a
--	expected_ty: Int -> Int
--	co_fn e =    e Int dOrdInt

tc_sub exp_sty expected_ty act_sty actual_ty
  | isSigmaTy actual_ty
  = tcInstType actual_ty	`thenNF_Tc` \ (tvs, theta, body_ty) ->
    newDicts orig theta		`thenNF_Tc` \ dicts ->
    let
	inst_fn e = mkHsDictApp (mkHsTyApp e (mkTyVarTys tvs))
				(map instToId dicts)
    in
    tc_sub exp_sty expected_ty body_ty body_ty	`thenTc` \ (co_fn, lie) ->
    returnTc (co_fn <.> mkCoercion inst_fn, lie `plusLIE` mkLIE dicts)
  where
    orig = Rank2Origin

-----------------------------------
-- Function case

tc_sub _ (FunTy exp_arg exp_res) _ (FunTy act_arg act_res)
  = tcSub_fun exp_arg exp_res act_arg act_res

-----------------------------------
-- Type variable meets function: imitate
--
-- NB 1: we can't just unify the type variable with the type
--	 because the type might not be a tau-type, and we aren't
--	 allowed to instantiate an ordinary type variable with
--	 a sigma-type
--
-- NB 2: can we short-cut to an error case?
--	 when the arg/res is not a tau-type?
-- NO!  e.g.   f :: ((forall a. a->a) -> Int) -> Int
--	then   x = (f,f)
--	is perfectly fine!

tc_sub exp_sty exp_ty@(FunTy exp_arg exp_res) _ (TyVarTy tv)
  = getTcTyVar tv	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> tc_sub exp_sty exp_ty ty ty
	Nothing -> imitateFun tv exp_sty	`thenNF_Tc` \ (act_arg, act_res) ->
		   tcSub_fun exp_arg exp_res act_arg act_res

tc_sub _ (TyVarTy tv) act_sty act_ty@(FunTy act_arg act_res)
  = getTcTyVar tv	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> tc_sub ty ty act_sty act_ty
	Nothing -> imitateFun tv act_sty	`thenNF_Tc` \ (exp_arg, exp_res) ->
		   tcSub_fun exp_arg exp_res act_arg act_res

-----------------------------------
-- Unification case
-- If none of the above match, we revert to the plain unifier
tc_sub exp_sty expected_ty act_sty actual_ty
  = uTys exp_sty expected_ty act_sty actual_ty	`thenTc_`
    returnTc (idCoercion, emptyLIE)
\end{code}    
    
%************************************************************************
%*									*
\subsection{Functions}
%*									*
%************************************************************************

\begin{code}
tcSub_fun exp_arg exp_res act_arg act_res
  = tcSub act_arg exp_arg	`thenTc` \ (co_fn_arg, lie1) ->
    tcSub exp_res act_res	`thenTc` \ (co_fn_res, lie2) ->
    tcGetUnique			`thenNF_Tc` \ uniq ->
    let
	-- co_fn_arg :: HsExpr exp_arg -> HsExpr act_arg
	-- co_fn_res :: HsExpr act_res -> HsExpr exp_res
	-- co_fn     :: HsExpr (act_arg -> act_res) -> HsExpr (exp_arg -> exp_res)
	arg_id = mkSysLocal SLIT("sub") uniq exp_arg
   	coercion | isIdCoercion co_fn_arg,
		   isIdCoercion co_fn_res = idCoercion
	         | otherwise	          = mkCoercion co_fn

	co_fn e = DictLam [arg_id] 
		     (co_fn_res <$> (HsApp e (co_fn_arg <$> (HsVar arg_id))))
		-- Slight hack; using a "DictLam" to get an ordinary simple lambda
		-- 	HsVar arg_id :: HsExpr exp_arg
		--	co_fn_arg $it :: HsExpr act_arg
		--	HsApp e $it   :: HsExpr act_res
		-- 	co_fn_res $it :: HsExpr exp_res
    in
    returnTc (coercion, lie1 `plusLIE` lie2)

imitateFun :: TcTyVar -> TcType -> NF_TcM (TcType, TcType)
imitateFun tv ty
  = ASSERT( not (isHoleTyVar tv) )
	-- NB: tv is an *ordinary* tyvar and so are the new ones

   	-- Check that tv isn't a type-signature type variable
	-- (This would be found later in checkSigTyVars, but
	--  we get a better error message if we do it here.)
    checkTcM (not (isSkolemTyVar tv))
	     (failWithTcM (unifyWithSigErr tv ty))	`thenTc_`

    newTyVarTy openTypeKind		`thenNF_Tc` \ arg ->
    newTyVarTy openTypeKind		`thenNF_Tc` \ res ->
    putTcTyVar tv (mkFunTy arg res)	`thenNF_Tc_`
    returnNF_Tc (arg,res)
\end{code}


%************************************************************************
%*									*
\subsection{Generalisation}
%*									*
%************************************************************************

\begin{code}
tcGen :: TcSigmaType				-- expected_ty
      -> (TcPhiType -> TcM (result, LIE))	-- spec_ty
      -> TcM (ExprCoFn, result, LIE)
	-- The expression has type: spec_ty -> expected_ty

tcGen expected_ty thing_inside	-- We expect expected_ty to be a forall-type
				-- If not, the call is a no-op
  = tcInstType expected_ty 		`thenNF_Tc` \ (forall_tvs, theta, phi_ty) ->

	-- Type-check the arg and unify with poly type
    thing_inside phi_ty		`thenTc` \ (result, lie) ->

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

    tcExtendGlobalTyVars free_tvs				$
    tcAddErrCtxtM (sigCtxt forall_tvs theta phi_ty)	$

    newDicts SignatureOrigin theta			`thenNF_Tc` \ dicts ->
    tcSimplifyCheck sig_msg forall_tvs dicts lie	`thenTc` \ (free_lie, inst_binds) ->
    checkSigTyVars forall_tvs free_tvs			`thenTc` \ zonked_tvs ->

    let
	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
	dict_ids = map instToId dicts
	co_fn e  = TyLam zonked_tvs (DictLam dict_ids (mkHsLet inst_binds e))
    in
    returnTc (mkCoercion co_fn, result, free_lie)
  where
    free_tvs = tyVarsOfType expected_ty
    sig_msg  = ptext SLIT("When generalising the type of an expression")
\end{code}    

    

%************************************************************************
%*									*
\subsection{Coercion functions}
%*									*
%************************************************************************

\begin{code}
type Coercion a = Maybe (a -> a)
	-- Nothing => identity fn

type ExprCoFn = Coercion TypecheckedHsExpr
type PatCoFn  = Coercion TcPat

(<.>) :: Coercion a -> Coercion a -> Coercion a	-- Composition
Nothing <.> Nothing = Nothing
Nothing <.> Just f  = Just f
Just f  <.> Nothing = Just f
Just f1 <.> Just f2 = Just (f1 . f2)

(<$>) :: Coercion a -> a -> a
Just f  <$> e = f e
Nothing <$> e = e

mkCoercion :: (a -> a) -> Coercion a
mkCoercion f = Just f

idCoercion :: Coercion a
idCoercion = Nothing

isIdCoercion :: Coercion a -> Bool
isIdCoercion = isNothing
\end{code}

%************************************************************************
%*									*
\subsection[Unify-exported]{Exported unification functions}
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

Unify two @TauType@s.  Dead straightforward.

\begin{code}
unifyTauTy :: TcTauType -> TcTauType -> TcM ()
unifyTauTy ty1 ty2 	-- ty1 expected, ty2 inferred
  = 	-- The unifier should only ever see tau-types 
	-- (no quantification whatsoever)
    ASSERT2( isTauTy ty1, ppr ty1 )
    ASSERT2( isTauTy ty2, ppr ty2 )
    tcAddErrCtxtM (unifyCtxt "type" ty1 ty2) $
    uTys ty1 ty1 ty2 ty2
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: [TcTauType] -> [TcTauType] ->  TcM ()
unifyTauTyLists [] 	     []	        = returnTc ()
unifyTauTyLists (ty1:tys1) (ty2:tys2) = uTys ty1 ty1 ty2 ty2   `thenTc_`
					unifyTauTyLists tys1 tys2
unifyTauTyLists ty1s ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TcTauType] -> TcM ()
unifyTauTyList []		 = returnTc ()
unifyTauTyList [ty]		 = returnTc ()
unifyTauTyList (ty1:tys@(ty2:_)) = unifyTauTy ty1 ty2	`thenTc_`
				   unifyTauTyList tys
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
uTys :: TcTauType -> TcTauType	-- Error reporting ty1 and real ty1
				-- ty1 is the *expected* type

     -> TcTauType -> TcTauType	-- Error reporting ty2 and real ty2
				-- ty2 is the *actual* type
     -> TcM ()

	-- Always expand synonyms (see notes at end)
        -- (this also throws away FTVs)
uTys ps_ty1 (NoteTy n1 ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (NoteTy n2 ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Variables; go for uVar
uTys ps_ty1 (TyVarTy tyvar1) ps_ty2 ty2 = uVar False tyvar1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (TyVarTy tyvar2) = uVar True  tyvar2 ps_ty1 ty1
					-- "True" means args swapped

	-- Predicates
uTys _ (SourceTy (IParam n1 t1)) _ (SourceTy (IParam n2 t2))
  | n1 == n2 = uTys t1 t1 t2 t2
uTys _ (SourceTy (ClassP c1 tys1)) _ (SourceTy (ClassP c2 tys2))
  | c1 == c2 = unifyTauTyLists tys1 tys2
uTys _ (SourceTy (NType tc1 tys1)) _ (SourceTy (NType tc2 tys2))
  | tc1 == tc2 = unifyTauTyLists tys1 tys2

	-- Functions; just check the two parts
uTys _ (FunTy fun1 arg1) _ (FunTy fun2 arg2)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

	-- Type constructors must match
uTys ps_ty1 (TyConApp con1 tys1) ps_ty2 (TyConApp con2 tys2)
  | con1 == con2 && equalLength tys1 tys2
  = unifyTauTyLists tys1 tys2

  | con1 == openKindCon
	-- When we are doing kind checking, we might match a kind '?' 
	-- against a kind '*' or '#'.  Notably, CCallable :: ? -> *, and
	-- (CCallable Int) and (CCallable Int#) are both OK
  = unifyOpenTypeKind ps_ty2

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTys ps_ty1 (AppTy s1 t1) ps_ty2 ty2
  = case tcSplitAppTy_maybe ty2 of
	Just (s2,t2) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
uTys ps_ty1 ty1 ps_ty2 (AppTy s2 t2)
  = case tcSplitAppTy_maybe ty1 of
	Just (s1,t1) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Not expecting for-alls in unification
	-- ... but the error message from the unifyMisMatch more informative
	-- than a panic message!

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = unifyMisMatch ps_ty1 ps_ty2
\end{code}


Notes on synonyms
~~~~~~~~~~~~~~~~~
If you are tempted to make a short cut on synonyms, as in this
pseudocode...

\begin{verbatim}
-- NO	uTys (SynTy con1 args1 ty1) (SynTy con2 args2 ty2)
-- NO     = if (con1 == con2) then
-- NO	-- Good news!  Same synonym constructors, so we can shortcut
-- NO	-- by unifying their arguments and ignoring their expansions.
-- NO	unifyTauTypeLists args1 args2
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
uVar :: Bool		-- False => tyvar is the "expected"
			-- True  => ty    is the "expected" thing
     -> TcTyVar
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM ()

uVar swapped tv1 ps_ty2 ty2
  = traceTc (text "uVar" <+> ppr swapped <+> ppr tv1 <+> (ppr ps_ty2 $$ ppr ty2))	`thenNF_Tc_`
    getTcTyVar tv1	`thenNF_Tc` \ maybe_ty1 ->
    case maybe_ty1 of
	Just ty1 | swapped   -> uTys ps_ty2 ty2 ty1 ty1	-- Swap back
		 | otherwise -> uTys ty1 ty1 ps_ty2 ty2	-- Same order
	other       -> uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2

	-- Expand synonyms; ignore FTVs
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 (NoteTy n2 ty2)
  = uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2


	-- The both-type-variable case
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2@(TyVarTy tv2)

	-- Same type variable => no-op
  | tv1 == tv2
  = returnTc ()

	-- Distinct type variables
	-- ASSERT maybe_ty1 /= Just
  | otherwise
  = getTcTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
    case maybe_ty2 of
	Just ty2' -> uUnboundVar swapped tv1 maybe_ty1 ty2' ty2'

	Nothing | update_tv2

		-> WARN( not (k1 `hasMoreBoxityInfo` k2), (ppr tv1 <+> ppr k1) $$ (ppr tv2 <+> ppr k2) )
		   putTcTyVar tv2 (TyVarTy tv1)		`thenNF_Tc_`
		   returnTc ()
		|  otherwise

		-> WARN( not (k2 `hasMoreBoxityInfo` k1), (ppr tv2 <+> ppr k2) $$ (ppr tv1 <+> ppr k1) )
                   putTcTyVar tv1 ps_ty2		`thenNF_Tc_`
	  	   returnTc ()
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    update_tv2 = (k2 `eqKind` openTypeKind) || (not (k1 `eqKind` openTypeKind) && nicer_to_update_tv2)
			-- Try to get rid of open type variables as soon as poss

    nicer_to_update_tv2 =  isUserTyVar tv1
				-- Don't unify a signature type variable if poss
			|| isSystemName (varName tv2)
				-- Try to update sys-y type variables in preference to sig-y ones

	-- Second one isn't a type variable
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 non_var_ty2
  = 	-- Check that tv1 isn't a type-signature type variable
    checkTcM (not (isSkolemTyVar tv1))
	     (failWithTcM (unifyWithSigErr tv1 ps_ty2))	`thenTc_`

	-- Do the occurs check, and check that we are not
	-- unifying a type variable with a polytype
	-- Returns a zonked type ready for the update
    checkValue tv1 ps_ty2 non_var_ty2	`thenTc` \ ty2 ->

   	-- Check that the kinds match
    checkKinds swapped tv1 ty2		`thenTc_`

	-- Perform the update
    putTcTyVar tv1 ty2			`thenNF_Tc_`
    returnTc ()
\end{code}

\begin{code}
checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- ty2 has been zonked at this stage, which ensures that
-- its kind has as much boxity information visible as possible.
  | tk2 `hasMoreBoxityInfo` tk1 = returnTc ()

  | otherwise
	-- Either the kinds aren't compatible
	--	(can happen if we unify (a b) with (c d))
	-- or we are unifying a lifted type variable with an
	-- 	unlifted type: e.g.  (id 3#) is illegal
  = tcAddErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$
    unifyMisMatch k1 k2

  where
    (k1,k2) | swapped   = (tk2,tk1)
	    | otherwise = (tk1,tk2)
    tk1 = tyVarKind tv1
    tk2 = typeKind ty2
\end{code}

\begin{code}
checkValue tv1 ps_ty2 non_var_ty2
-- Do the occurs check, and check that we are not
-- unifying a type variable with a polytype
-- Return the type to update the type variable with, or fail

-- Basically we want to update     tv1 := ps_ty2
-- because ps_ty2 has type-synonym info, which improves later error messages
-- 
-- But consider 
--	type A a = ()
--
--	f :: (A a -> a -> ()) -> ()
--	f = \ _ -> ()
--
-- 	x :: ()
-- 	x = f (\ x p -> p x)
--
-- In the application (p x), we try to match "t" with "A t".  If we go
-- ahead and bind t to A t (= ps_ty2), we'll lead the type checker into 
-- an infinite loop later.
-- But we should not reject the program, because A t = ().
-- Rather, we should bind t to () (= non_var_ty2).
-- 
-- That's why we have this two-state occurs-check
  = zonkTcType ps_ty2			`thenNF_Tc` \ ps_ty2' ->
    case okToUnifyWith tv1 ps_ty2' of {
	Nothing -> returnTc ps_ty2' ;	-- Success
	other ->

    zonkTcType non_var_ty2		`thenNF_Tc` \ non_var_ty2' ->
    case okToUnifyWith tv1 non_var_ty2' of
	Nothing -> 	-- This branch rarely succeeds, except in strange cases
			-- like that in the example above
		    returnTc non_var_ty2'

	Just problem -> failWithTcM (unifyCheck problem tv1 ps_ty2')
    }

data Problem = OccurCheck | NotMonoType

okToUnifyWith :: TcTyVar -> TcType -> Maybe Problem
-- (okToUnifyWith tv ty) checks whether it's ok to unify
-- 	tv :=: ty
-- Nothing => ok
-- Just p  => not ok, problem p

okToUnifyWith tv ty
  = ok ty
  where
    ok (TyVarTy tv') | tv == tv' = Just OccurCheck
		     | otherwise = Nothing
    ok (AppTy t1 t2)   		= ok t1 `and` ok t2
    ok (FunTy t1 t2)   		= ok t1 `and` ok t2
    ok (TyConApp _ ts) 		= oks ts
    ok (ForAllTy _ _)  		= Just NotMonoType
    ok (SourceTy st)   		= ok_st st
    ok (NoteTy (FTVNote _) t)   = ok t
    ok (NoteTy (SynNote t1) t2) = ok t1 `and` ok t2
		-- Type variables may be free in t1 but not t2
		-- A forall may be in t2 but not t1

    oks ts = foldr (and . ok) Nothing ts

    ok_st (ClassP _ ts) = oks ts
    ok_st (IParam _ t)  = ok t
    ok_st (NType _ ts)  = oks ts

    Nothing `and` m = m
    Just p  `and` m = Just p
\end{code}

%************************************************************************
%*									*
\subsection[Unify-fun]{@unifyFunTy@}
%*									*
%************************************************************************

@subFunTy@ and @unifyFunTy@ is used to avoid the fruitless 
creation of type variables.

* subFunTy is used when we might be faced with a "hole" type variable,
  in which case we should create two new holes. 

* unifyFunTy is used when we expect to encounter only "ordinary" 
  type variables, so we should create new ordinary type variables

\begin{code}
subFunTy :: TcSigmaType	 		-- Fail if ty isn't a function type
	 -> TcM (TcType, TcType)	-- otherwise return arg and result types
subFunTy ty@(TyVarTy tyvar)
  
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> subFunTy ty
	Nothing | isHoleTyVar tyvar
		-> newHoleTyVarTy 	`thenNF_Tc` \ arg ->
		   newHoleTyVarTy 	`thenNF_Tc` \ res ->
		   putTcTyVar tyvar (mkFunTy arg res)	`thenNF_Tc_` 
	   	   returnTc (arg,res)
		| otherwise 
		-> unify_fun_ty_help ty

subFunTy ty
  = case tcSplitFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

		 
unifyFunTy :: TcPhiType	 		-- Fail if ty isn't a function type
	   -> TcM (TcType, TcType)	-- otherwise return arg and result types

unifyFunTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyFunTy ty'
	Nothing  -> unify_fun_ty_help ty

unifyFunTy ty
  = case tcSplitFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy openTypeKind	`thenNF_Tc` \ arg ->
    newTyVarTy openTypeKind	`thenNF_Tc` \ res ->
    unifyTauTy ty (mkFunTy arg res)	`thenTc_`
    returnTc (arg,res)
\end{code}

\begin{code}
unifyListTy :: TcType              -- expected list type
	    -> TcM TcType      -- list element type

unifyListTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyListTy ty'
	other	 -> unify_list_ty_help ty

unifyListTy ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, [arg_ty]) | tycon == listTyCon -> returnTc arg_ty
	other					    -> unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy liftedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy ty (mkListTy elt_ty)	`thenTc_`
    returnTc elt_ty

-- variant for parallel arrays
--
unifyPArrTy :: TcType              -- expected list type
	    -> TcM TcType	   -- list element type

unifyPArrTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
      Just ty' -> unifyPArrTy ty'
      _        -> unify_parr_ty_help ty
unifyPArrTy ty
  = case tcSplitTyConApp_maybe ty of
      Just (tycon, [arg_ty]) | tycon == parrTyCon -> returnTc arg_ty
      _  					  -> unify_parr_ty_help ty

unify_parr_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy liftedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy ty (mkPArrTy elt_ty)	`thenTc_`
    returnTc elt_ty
\end{code}

\begin{code}
unifyTupleTy :: Boxity -> Arity -> TcType -> TcM [TcType]
unifyTupleTy boxity arity ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyTupleTy boxity arity ty'
	other	 -> unify_tuple_ty_help boxity arity ty

unifyTupleTy boxity arity ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, arg_tys)
		|  isTupleTyCon tycon 
		&& tyConArity tycon == arity
		&& tupleTyConBoxity tycon == boxity
		-> returnTc arg_tys
	other -> unify_tuple_ty_help boxity arity ty

unify_tuple_ty_help boxity arity ty
  = newTyVarTys arity kind				`thenNF_Tc` \ arg_tys ->
    unifyTauTy ty (mkTupleTy boxity arity arg_tys)	`thenTc_`
    returnTc arg_tys
  where
    kind | isBoxed boxity = liftedTypeKind
	 | otherwise      = openTypeKind
\end{code}


%************************************************************************
%*									*
\subsection{Kind unification}
%*									*
%************************************************************************

\begin{code}
unifyKind :: TcKind		    -- Expected
	  -> TcKind		    -- Actual
	  -> TcM ()
unifyKind k1 k2 
  = tcAddErrCtxtM (unifyCtxt "kind" k1 k2) $
    uTys k1 k1 k2 k2

unifyKinds :: [TcKind] -> [TcKind] -> TcM ()
unifyKinds []       []       = returnTc ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenTc_`
			       unifyKinds ks1 ks2
unifyKinds _ _ = panic "unifyKinds: length mis-match"
\end{code}

\begin{code}
unifyOpenTypeKind :: TcKind -> TcM ()	
-- Ensures that the argument kind is of the form (Type bx)
-- for some boxity bx

unifyOpenTypeKind ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyOpenTypeKind ty'
	other	 -> unify_open_kind_help ty

unifyOpenTypeKind ty
  | isTypeKind ty = returnTc ()
  | otherwise     = unify_open_kind_help ty

unify_open_kind_help ty	-- Revert to ordinary unification
  = newBoxityVar 	`thenNF_Tc` \ boxity ->
    unifyKind ty (mkTyConApp typeCon [boxity])
\end{code}


%************************************************************************
%*									*
\subsection[Unify-context]{Errors and contexts}
%*									*
%************************************************************************

Errors
~~~~~~

\begin{code}
unifyCtxt s ty1 ty2 tidy_env	-- ty1 expected, ty2 inferred
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty1' ty2')
  where
    err ty1 ty2 = (env1, 
		   nest 4 
			(vcat [
			   text "Expected" <+> text s <> colon <+> ppr tidy_ty1,
			   text "Inferred" <+> text s <> colon <+> ppr tidy_ty2
		        ]))
		  where
		    (env1, [tidy_ty1,tidy_ty2]) = tidyOpenTypes tidy_env [ty1,ty2]

unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
	-- tv1 is zonked already
  = zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty2')
  where
    err ty2 = (env2, ptext SLIT("When matching types") <+> 
		     sep [quotes pp_expected, ptext SLIT("and"), quotes pp_actual])
	    where
	      (pp_expected, pp_actual) | swapped   = (pp2, pp1)
				       | otherwise = (pp1, pp2)
	      (env1, tv1') = tidyOpenTyVar tidy_env tv1
	      (env2, ty2') = tidyOpenType  env1 ty2
	      pp1 = ppr tv1'
	      pp2 = ppr ty2'

unifyMisMatch ty1 ty2
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    let
    	(env, [tidy_ty1, tidy_ty2]) = tidyOpenTypes emptyTidyEnv [ty1',ty2']
	msg = hang (ptext SLIT("Couldn't match"))
		   4 (sep [quotes (ppr tidy_ty1), 
			   ptext SLIT("against"), 
			   quotes (ppr tidy_ty2)])
    in
    failWithTcM (env, msg)

unifyWithSigErr tyvar ty
  = (env2, hang (ptext SLIT("Cannot unify the type-signature variable") <+> quotes (ppr tidy_tyvar))
	      4 (ptext SLIT("with the type") <+> quotes (ppr tidy_ty)))
  where
    (env1, tidy_tyvar) = tidyOpenTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1         ty

unifyCheck problem tyvar ty
  = (env2, hang msg
	      4 (sep [ppr tidy_tyvar, char '=', ppr tidy_ty]))
  where
    (env1, tidy_tyvar) = tidyOpenTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1         ty

    msg = case problem of
	    OccurCheck  -> ptext SLIT("Occurs check: cannot construct the infinite type:")
	    NotMonoType -> ptext SLIT("Cannot unify a type variable with a type scheme:")
\end{code}



%************************************************************************
%*									*
\subsection{Checking signature type variables}
%*									*
%************************************************************************

@checkSigTyVars@ is used after the type in a type signature has been unified with
the actual type found.  It then checks that the type variables of the type signature
are
	(a) Still all type variables
		eg matching signature [a] against inferred type [(p,q)]
		[then a will be unified to a non-type variable]

	(b) Still all distinct
		eg matching signature [(a,b)] against inferred type [(p,p)]
		[then a and b will be unified together]

	(c) Not mentioned in the environment
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

We used to have the notion of a "DontBind" type variable, which would
only be bound to itself or nothing.  Then points (a) and (b) were 
self-checking.  But it gave rise to bogus consequential error messages.
For example:

   f = (*)	-- Monomorphic

   g :: Num a => a -> a
   g x = f x x

Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num x) context arising from f's definition;
we try to unify x with Int (to default it), but find that x has already
been unified with the DontBind variable "a" from g's signature.
This is really a problem with side-effecting unification; we'd like to
undo g's effects when its type signature fails, but unification is done
by side effect, so we can't (easily).

So we revert to ordinary type variables for signatures, and try to
give a helpful message in checkSigTyVars.

\begin{code}
checkSigTyVars :: [TcTyVar]		-- Universally-quantified type variables in the signature
	       -> TcTyVarSet		-- Tyvars that are free in the type signature
					--	Not necessarily zonked
					-- 	These should *already* be in the free-in-env set, 
					-- 	and are used here only to improve the error message
	       -> TcM [TcTyVar]		-- Zonked signature type variables

checkSigTyVars [] free = returnTc []
checkSigTyVars sig_tyvars free_tyvars
  = zonkTcTyVars sig_tyvars		`thenNF_Tc` \ sig_tys ->
    tcGetGlobalTyVars			`thenNF_Tc` \ globals ->

    checkTcM (allDistinctTyVars sig_tys globals)
	     (complain sig_tys globals)	`thenTc_`

    returnTc (map (tcGetTyVar "checkSigTyVars") sig_tys)

  where
    complain sig_tys globals
      = -- "check" checks each sig tyvar in turn
        foldlNF_Tc check
		   (env2, emptyVarEnv, [])
		   (tidy_tvs `zip` tidy_tys)	`thenNF_Tc` \ (env3, _, msgs) ->

        failWithTcM (env3, main_msg $$ vcat msgs)
      where
	(env1, tidy_tvs) = tidyOpenTyVars emptyTidyEnv sig_tyvars
	(env2, tidy_tys) = tidyOpenTypes  env1	       sig_tys

	main_msg = ptext SLIT("Inferred type is less polymorphic than expected")

	check (tidy_env, acc, msgs) (sig_tyvar,ty)
		-- sig_tyvar is from the signature;
		-- ty is what you get if you zonk sig_tyvar and then tidy it
		--
		-- acc maps a zonked type variable back to a signature type variable
	  = case tcGetTyVar_maybe ty of {
	      Nothing ->			-- Error (a)!
			returnNF_Tc (tidy_env, acc, unify_msg sig_tyvar (quotes (ppr ty)) : msgs) ;

	      Just tv ->

	    case lookupVarEnv acc tv of {
		Just sig_tyvar' -> 	-- Error (b)!
			returnNF_Tc (tidy_env, acc, unify_msg sig_tyvar thing : msgs)
		    where
			thing = ptext SLIT("another quantified type variable") <+> quotes (ppr sig_tyvar')

	      ; Nothing ->

	    if tv `elemVarSet` globals	-- Error (c) or (d)! Type variable escapes
					-- The least comprehensible, so put it last
			-- Game plan: 
			--    a) get the local TcIds and TyVars from the environment,
			-- 	 and pass them to find_globals (they might have tv free)
			--    b) similarly, find any free_tyvars that mention tv
	    then   tcGetEnv 							`thenNF_Tc` \ ve ->
        	   find_globals tv tidy_env  (tcLEnvElts ve)			`thenNF_Tc` \ (tidy_env1, globs) ->
        	   find_frees   tv tidy_env1 [] (varSetElems free_tyvars)	`thenNF_Tc` \ (tidy_env2, frees) ->
		   returnNF_Tc (tidy_env2, acc, escape_msg sig_tyvar tv globs frees : msgs)

	    else 	-- All OK
	    returnNF_Tc (tidy_env, extendVarEnv acc tv sig_tyvar, msgs)
	    }}

-----------------------
-- find_globals looks at the value environment and finds values
-- whose types mention the offending type variable.  It has to be 
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.

find_globals :: Var 
             -> TidyEnv 
             -> [TcTyThing] 
             -> NF_TcM (TidyEnv, [SDoc])

find_globals tv tidy_env things
  = go tidy_env [] things
  where
    go tidy_env acc [] = returnNF_Tc (tidy_env, acc)
    go tidy_env acc (thing : things)
      = find_thing ignore_it tidy_env thing 	`thenNF_Tc` \ (tidy_env1, maybe_doc) ->
	case maybe_doc of
	  Just d  -> go tidy_env1 (d:acc) things
	  Nothing -> go tidy_env1 acc     things

    ignore_it ty = not (tv `elemVarSet` tyVarsOfType ty)

-----------------------
find_thing ignore_it tidy_env (ATcId id)
  = zonkTcType  (idType id)	`thenNF_Tc` \ id_ty ->
    if ignore_it id_ty then
	returnNF_Tc (tidy_env, Nothing)
    else let
	(tidy_env', tidy_ty) = tidyOpenType tidy_env id_ty
	msg = sep [ppr id <+> dcolon <+> ppr tidy_ty, 
		   nest 2 (parens (ptext SLIT("bound at") <+>
			 	   ppr (getSrcLoc id)))]
    in
    returnNF_Tc (tidy_env', Just msg)

find_thing ignore_it tidy_env (ATyVar tv)
  = zonkTcTyVar tv		`thenNF_Tc` \ tv_ty ->
    if ignore_it tv_ty then
	returnNF_Tc (tidy_env, Nothing)
    else let
	(tidy_env1, tv1)     = tidyOpenTyVar tidy_env  tv
	(tidy_env2, tidy_ty) = tidyOpenType  tidy_env1 tv_ty
	msg = sep [ptext SLIT("Type variable") <+> quotes (ppr tv1) <+> eq_stuff, nest 2 bound_at]

	eq_stuff | Just tv' <- Type.getTyVar_maybe tv_ty, tv == tv' = empty
		 | otherwise	  				    = equals <+> ppr tv_ty
		-- It's ok to use Type.getTyVar_maybe because ty is zonked by now
	
	bound_at = tyVarBindingInfo tv
    in
    returnNF_Tc (tidy_env2, Just msg)

-----------------------
find_frees tv tidy_env acc []
  = returnNF_Tc (tidy_env, acc)
find_frees tv tidy_env acc (ftv:ftvs)
  = zonkTcTyVar ftv	`thenNF_Tc` \ ty ->
    if tv `elemVarSet` tyVarsOfType ty then
	let
	    (tidy_env', ftv') = tidyOpenTyVar tidy_env ftv
	in
	find_frees tv tidy_env' (ftv':acc) ftvs
    else
	find_frees tv tidy_env  acc        ftvs


escape_msg sig_tv tv globs frees
  = mk_msg sig_tv <+> ptext SLIT("escapes") $$
    if not (null globs) then
	vcat [pp_it <+> ptext SLIT("is mentioned in the environment:"), 
	      nest 2 (vcat globs)]
     else if not (null frees) then
	vcat [ptext SLIT("It is reachable from the type variable(s)") <+> pprQuotedList frees,
	      nest 2 (ptext SLIT("which") <+> is_are <+> ptext SLIT("free in the signature"))
	]
     else
	empty	-- Sigh.  It's really hard to give a good error message
		-- all the time.   One bad case is an existential pattern match
  where
    is_are | isSingleton frees = ptext SLIT("is")
	   | otherwise         = ptext SLIT("are")
    pp_it | sig_tv /= tv = ptext SLIT("It unifies with") <+> quotes (ppr tv) <> comma <+> ptext SLIT("which")
	  | otherwise    = ptext SLIT("It")

    vcat_first :: Int -> [SDoc] -> SDoc
    vcat_first n []     = empty
    vcat_first 0 (x:xs) = text "...others omitted..."
    vcat_first n (x:xs) = x $$ vcat_first (n-1) xs


unify_msg tv thing = mk_msg tv <+> ptext SLIT("is unified with") <+> thing
mk_msg tv          = ptext SLIT("Quantified type variable") <+> quotes (ppr tv)
\end{code}

These two context are used with checkSigTyVars
    
\begin{code}
sigCtxt :: [TcTyVar] -> TcThetaType -> TcTauType
	-> TidyEnv -> NF_TcM (TidyEnv, Message)
sigCtxt sig_tyvars sig_theta sig_tau tidy_env
  = zonkTcType sig_tau		`thenNF_Tc` \ actual_tau ->
    let
	(env1, tidy_sig_tyvars)  = tidyOpenTyVars tidy_env sig_tyvars
	(env2, tidy_sig_rho)	 = tidyOpenType env1 (mkRhoTy sig_theta sig_tau)
	(env3, tidy_actual_tau)  = tidyOpenType env2 actual_tau
	msg = vcat [ptext SLIT("Signature type:    ") <+> pprType (mkForAllTys tidy_sig_tyvars tidy_sig_rho),
		    ptext SLIT("Type to generalise:") <+> pprType tidy_actual_tau
		   ]
    in
    returnNF_Tc (env3, msg)

sigPatCtxt bound_tvs bound_ids tidy_env
  = returnNF_Tc (env1,
		 sep [ptext SLIT("When checking a pattern that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys))])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    (env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
    ppr_id id ty     = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}


