%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Type subsumption and unification}

\begin{code}
module TcUnify (
	-- Full-blown subsumption
  tcSubOff, tcSubExp, tcGen, 
  checkSigTyVars, checkSigTyVarsWrt, sigCtxt, findGlobals,

	-- Various unifications
  unifyTauTy, unifyTauTyList, unifyTauTyLists, 
  unifyKind, unifyKinds, unifyOpenTypeKind, unifyFunKind,

  --------------------------------
  -- Holes
  Expected(..), newHole, readExpectedType, 
  zapExpectedType, zapExpectedTo, zapExpectedBranches,
  subFunTys,   	unifyFunTy, 
  zapToListTy, 	unifyListTy, 
  zapToPArrTy, 	unifyPArrTy, 
  zapToTupleTy, unifyTupleTy

  ) where

#include "HsVersions.h"


import HsSyn		( HsExpr(..) )
import TcHsSyn		( mkHsLet,
			  ExprCoFn, idCoercion, isIdCoercion, mkCoercion, (<.>), (<$>) )
import TypeRep		( Type(..), SourceType(..), TyNote(..), openKindCon )

import TcRnMonad         -- TcType, amongst others
import TcType		( TcKind, TcType, TcSigmaType, TcRhoType, TcTyVar, TcTauType,
			  TcTyVarSet, TcThetaType, TyVarDetails(SigTv),
			  isTauTy, isSigmaTy, mkFunTys,
			  tcSplitAppTy_maybe, tcSplitTyConApp_maybe, 
			  tcGetTyVar_maybe, tcGetTyVar, 
			  mkFunTy, tyVarsOfType, mkPhiTy,
			  typeKind, tcSplitFunTy_maybe, mkForAllTys,
			  isSkolemTyVar, isUserTyVar, 
			  tidyOpenType, tidyOpenTypes, tidyOpenTyVar, tidyOpenTyVars,
			  eqKind, openTypeKind, liftedTypeKind, isTypeKind, mkArrowKind,
			  hasMoreBoxityInfo, allDistinctTyVars
			)
import Inst		( newDicts, instToId, tcInstCall )
import TcMType		( getTcTyVar, putTcTyVar, tcInstType, newKindVar,
			  newTyVarTy, newTyVarTys, newOpenTypeKind, 
			  zonkTcType, zonkTcTyVars, zonkTcTyVarsAndFV )
import TcSimplify	( tcSimplifyCheck )
import TysWiredIn	( listTyCon, parrTyCon, mkListTy, mkPArrTy, mkTupleTy )
import TcEnv		( tcGetGlobalTyVars, findGlobals )
import TyCon		( tyConArity, isTupleTyCon, tupleTyConBoxity )
import PprType		( pprType )
import Id		( Id, mkSysLocal )
import Var		( Var, varName, tyVarKind )
import VarSet		( emptyVarSet, unitVarSet, unionVarSet, elemVarSet, varSetElems )
import VarEnv
import Name		( isSystemName )
import ErrUtils		( Message )
import BasicTypes	( Boxity, Arity, isBoxed )
import Util		( equalLength, lengthExceeds, notNull )
import Outputable
\end{code}

Notes on holes
~~~~~~~~~~~~~~
* A hole is always filled in with an ordinary type, not another hole.

%************************************************************************
%*									*
\subsection{'hole' type variables}
%*									*
%************************************************************************

\begin{code}
data Expected ty = Infer (TcRef ty)	-- The hole to fill in for type inference
		 | Check ty		-- The type to check during type checking

newHole :: TcM (TcRef ty)
newHole = newMutVar (error "Empty hole in typechecker")

readExpectedType :: Expected ty -> TcM ty
readExpectedType (Infer hole) = readMutVar hole
readExpectedType (Check ty)   = returnM ty

zapExpectedType :: Expected TcType -> TcM TcTauType
-- In the inference case, ensure we have a monotype
zapExpectedType (Infer hole)
  = do { ty <- newTyVarTy openTypeKind ;
	 writeMutVar hole ty ;
	 return ty }

zapExpectedType (Check ty) = return ty

zapExpectedTo :: Expected TcType -> TcTauType -> TcM ()
zapExpectedTo (Infer hole) ty2 = writeMutVar hole ty2
zapExpectedTo (Check ty1)  ty2 = unifyTauTy ty1 ty2

zapExpectedBranches :: [a] -> Expected TcType -> TcM (Expected TcType)
-- Zap the expected type to a monotype if there is more than one branch
zapExpectedBranches branches exp_ty
  | lengthExceeds branches 1 = zapExpectedType exp_ty 	`thenM` \ exp_ty' -> 
			       return (Check exp_ty')
  | otherwise		     = returnM exp_ty		

instance Outputable ty => Outputable (Expected ty) where
  ppr (Check ty)   = ptext SLIT("Expected type") <+> ppr ty
  ppr (Infer hole) = ptext SLIT("Inferring type")
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
subFunTys :: [pat]
	 -> Expected TcRhoType	-- Fail if ty isn't a function type
	 -> ([(pat, Expected TcRhoType)] -> Expected TcRhoType -> TcM a)
	 -> TcM a

subFunTys pats (Infer hole) thing_inside
  = 	-- This is the interesting case
    mapM new_pat_hole pats	`thenM` \ pats_w_holes ->
    newHole			`thenM` \ res_hole ->

	-- Do the business
    thing_inside pats_w_holes (Infer res_hole)	`thenM` \ answer ->

	-- Extract the answers
    mapM read_pat_hole pats_w_holes	`thenM` \ arg_tys ->
    readMutVar res_hole			`thenM` \ res_ty ->

	-- Write the answer into the incoming hole
    writeMutVar hole (mkFunTys arg_tys res_ty)	`thenM_` 

	-- And return the answer
    returnM answer
  where
    new_pat_hole pat = newHole `thenM` \ hole -> return (pat, Infer hole)
    read_pat_hole (pat, Infer hole) = readMutVar hole

subFunTys pats (Check ty) thing_inside
  = go pats ty		`thenM` \ (pats_w_tys, res_ty) ->
    thing_inside pats_w_tys res_ty
  where
    go []         ty = return ([], Check ty)
    go (pat:pats) ty = unifyFunTy ty 	`thenM` \ (arg,res) ->
		       go pats res	`thenM` \ (pats_w_tys, final_res) ->
		       return ((pat, Check arg) : pats_w_tys, final_res)
		 
unifyFunTy :: TcRhoType	 		-- Fail if ty isn't a function type
	   -> TcM (TcType, TcType)	-- otherwise return arg and result types

unifyFunTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyFunTy ty'
	Nothing  -> unify_fun_ty_help ty

unifyFunTy ty
  = case tcSplitFunTy_maybe ty of
	Just arg_and_res -> returnM arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy openTypeKind	`thenM` \ arg ->
    newTyVarTy openTypeKind	`thenM` \ res ->
    unifyTauTy ty (mkFunTy arg res)	`thenM_`
    returnM (arg,res)
\end{code}

\begin{code}
zapToListTy :: Expected TcType -- expected list type
	    -> TcM TcType      -- list element type

zapToListTy (Check ty)   = unifyListTy ty
zapToListTy (Infer hole) = do { elt_ty <- newTyVarTy liftedTypeKind ;
				writeMutVar hole (mkListTy elt_ty) ;
				return elt_ty }

unifyListTy :: TcType -> TcM TcType
unifyListTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyListTy ty'
	other	 -> unify_list_ty_help ty

unifyListTy ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, [arg_ty]) | tycon == listTyCon -> returnM arg_ty
	other					    -> unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy liftedTypeKind		`thenM` \ elt_ty ->
    unifyTauTy ty (mkListTy elt_ty)	`thenM_`
    returnM elt_ty

-- variant for parallel arrays
--
zapToPArrTy :: Expected TcType     -- Expected list type
	    -> TcM TcType	   -- List element type

zapToPArrTy (Check ty)   = unifyPArrTy ty
zapToPArrTy (Infer hole) = do { elt_ty <- newTyVarTy liftedTypeKind ;
				writeMutVar hole (mkPArrTy elt_ty) ;
				return elt_ty }

unifyPArrTy :: TcType -> TcM TcType

unifyPArrTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
      Just ty' -> unifyPArrTy ty'
      _        -> unify_parr_ty_help ty
unifyPArrTy ty
  = case tcSplitTyConApp_maybe ty of
      Just (tycon, [arg_ty]) | tycon == parrTyCon -> returnM arg_ty
      _  					  -> unify_parr_ty_help ty

unify_parr_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy liftedTypeKind		`thenM` \ elt_ty ->
    unifyTauTy ty (mkPArrTy elt_ty)	`thenM_`
    returnM elt_ty
\end{code}

\begin{code}
zapToTupleTy :: Boxity -> Arity -> Expected TcType -> TcM [TcType]
zapToTupleTy boxity arity (Check ty)   = unifyTupleTy boxity arity ty
zapToTupleTy boxity arity (Infer hole) = do { (tup_ty, arg_tys) <- new_tuple_ty boxity arity ;
					      writeMutVar hole tup_ty ;
				 	      return arg_tys }

unifyTupleTy boxity arity ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyTupleTy boxity arity ty'
	other	 -> unify_tuple_ty_help boxity arity ty

unifyTupleTy boxity arity ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, arg_tys)
		|  isTupleTyCon tycon 
		&& tyConArity tycon == arity
		&& tupleTyConBoxity tycon == boxity
		-> returnM arg_tys
	other -> unify_tuple_ty_help boxity arity ty

unify_tuple_ty_help boxity arity ty
  = new_tuple_ty boxity arity	`thenM` \ (tup_ty, arg_tys) ->
    unifyTauTy ty tup_ty	`thenM_`
    returnM arg_tys

new_tuple_ty boxity arity
  = newTyVarTys arity kind	`thenM` \ arg_tys ->
    return (mkTupleTy boxity arity arg_tys, arg_tys)
  where
    kind | isBoxed boxity = liftedTypeKind
	 | otherwise      = openTypeKind
\end{code}


%************************************************************************
%*									*
\subsection{Subsumption}
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
tcSubExp :: Expected TcRhoType -> TcRhoType  -> TcM ExprCoFn
tcSubOff :: TcSigmaType  -> Expected TcSigmaType -> TcM ExprCoFn
\end{code}

These two check for holes

\begin{code}
tcSubExp expected_ty offered_ty
  = traceTc (text "tcSubExp" <+> (ppr expected_ty $$ ppr offered_ty)) 	`thenM_`
    checkHole expected_ty offered_ty tcSub

tcSubOff expected_ty offered_ty
  = checkHole offered_ty expected_ty (\ off exp -> tcSub exp off)

-- checkHole looks for a hole in its first arg; 
-- If so, and it is uninstantiated, it fills in the hole 
--	  with its second arg
-- Otherwise it calls thing_inside, passing the two args, looking
-- through any instantiated hole

checkHole (Infer hole) other_ty thing_inside
  = do { writeMutVar hole other_ty; return idCoercion }

checkHole (Check ty) other_ty thing_inside 
  = thing_inside ty other_ty
\end{code}

No holes expected now.  Add some error-check context info.

\begin{code}
tcSub :: TcSigmaType -> TcSigmaType -> TcM ExprCoFn	-- Locally used only
tcSub expected_ty actual_ty
  = traceTc (text "tcSub" <+> details)		`thenM_`
    addErrCtxtM (unifyCtxt "type" expected_ty actual_ty)
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
       -> TcM ExprCoFn

-----------------------------------
-- Expand synonyms
tc_sub exp_sty (NoteTy _ exp_ty) act_sty act_ty = tc_sub exp_sty exp_ty act_sty act_ty
tc_sub exp_sty exp_ty act_sty (NoteTy _ act_ty) = tc_sub exp_sty exp_ty act_sty act_ty

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
  = tcGen expected_ty (tyVarsOfType actual_ty) (
	-- It's really important to check for escape wrt the free vars of
	-- both expected_ty *and* actual_ty
	\ body_exp_ty -> tc_sub body_exp_ty body_exp_ty act_sty actual_ty
    )				`thenM` \ (gen_fn, co_fn) ->
    returnM (gen_fn <.> co_fn)

-----------------------------------
-- Specialisation case:
--	actual_ty:   forall a. Ord a => a->a
--	expected_ty: Int -> Int
--	co_fn e =    e Int dOrdInt

tc_sub exp_sty expected_ty act_sty actual_ty
  | isSigmaTy actual_ty
  = tcInstCall Rank2Origin actual_ty		`thenM` \ (inst_fn, body_ty) ->
    tc_sub exp_sty expected_ty body_ty body_ty	`thenM` \ co_fn ->
    returnM (co_fn <.> inst_fn)

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
--	is perfectly fine, because we can instantiat f's type to a monotype
--
-- However, we get can get jolly unhelpful error messages.  
--	e.g.	foo = id runST
--
--    Inferred type is less polymorphic than expected
--	Quantified type variable `s' escapes
--	Expected type: ST s a -> t
--	Inferred type: (forall s1. ST s1 a) -> a
--    In the first argument of `id', namely `runST'
--    In a right-hand side of function `foo': id runST
--
-- I'm not quite sure what to do about this!

tc_sub exp_sty exp_ty@(FunTy exp_arg exp_res) _ (TyVarTy tv)
  = getTcTyVar tv	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty -> tc_sub exp_sty exp_ty ty ty
	Nothing -> imitateFun tv exp_sty	`thenM` \ (act_arg, act_res) ->
		   tcSub_fun exp_arg exp_res act_arg act_res

tc_sub _ (TyVarTy tv) act_sty act_ty@(FunTy act_arg act_res)
  = getTcTyVar tv	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty -> tc_sub ty ty act_sty act_ty
	Nothing -> imitateFun tv act_sty	`thenM` \ (exp_arg, exp_res) ->
		   tcSub_fun exp_arg exp_res act_arg act_res

-----------------------------------
-- Unification case
-- If none of the above match, we revert to the plain unifier
tc_sub exp_sty expected_ty act_sty actual_ty
  = uTys exp_sty expected_ty act_sty actual_ty	`thenM_`
    returnM idCoercion
\end{code}    
    
%************************************************************************
%*									*
\subsection{Functions}
%*									*
%************************************************************************

\begin{code}
tcSub_fun exp_arg exp_res act_arg act_res
  = tc_sub act_arg act_arg exp_arg exp_arg	`thenM` \ co_fn_arg ->
    tc_sub exp_res exp_res act_res act_res	`thenM` \ co_fn_res ->
    newUnique					`thenM` \ uniq ->
    let
	-- co_fn_arg :: HsExpr exp_arg -> HsExpr act_arg
	-- co_fn_res :: HsExpr act_res -> HsExpr exp_res
	-- co_fn     :: HsExpr (act_arg -> act_res) -> HsExpr (exp_arg -> exp_res)
	arg_id = mkSysLocal FSLIT("sub") uniq exp_arg
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
    returnM coercion

imitateFun :: TcTyVar -> TcType -> TcM (TcType, TcType)
imitateFun tv ty
  = 	-- NB: tv is an *ordinary* tyvar and so are the new ones

   	-- Check that tv isn't a type-signature type variable
	-- (This would be found later in checkSigTyVars, but
	--  we get a better error message if we do it here.)
    checkM (not (isSkolemTyVar tv))
	   (failWithTcM (unifyWithSigErr tv ty))	`thenM_`

    newTyVarTy openTypeKind		`thenM` \ arg ->
    newTyVarTy openTypeKind		`thenM` \ res ->
    putTcTyVar tv (mkFunTy arg res)	`thenM_`
    returnM (arg,res)
\end{code}


%************************************************************************
%*									*
\subsection{Generalisation}
%*									*
%************************************************************************

\begin{code}
tcGen :: TcSigmaType				-- expected_ty
      -> TcTyVarSet				-- Extra tyvars that the universally
						--	quantified tyvars of expected_ty
						-- 	must not be unified
      -> (TcRhoType -> TcM result)		-- spec_ty
      -> TcM (ExprCoFn, result)
	-- The expression has type: spec_ty -> expected_ty

tcGen expected_ty extra_tvs thing_inside	-- We expect expected_ty to be a forall-type
						-- If not, the call is a no-op
  = tcInstType SigTv expected_ty 	`thenM` \ (forall_tvs, theta, phi_ty) ->

	-- Type-check the arg and unify with poly type
    getLIE (thing_inside phi_ty)	`thenM` \ (result, lie) ->

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

    newDicts SignatureOrigin theta			`thenM` \ dicts ->
    tcSimplifyCheck sig_msg forall_tvs dicts lie	`thenM` \ inst_binds ->

#ifdef DEBUG
    zonkTcTyVars forall_tvs `thenM` \ forall_tys ->
    traceTc (text "tcGen" <+> vcat [text "extra_tvs" <+> ppr extra_tvs,
				    text "expected_ty" <+> ppr expected_ty,
				    text "inst ty" <+> ppr forall_tvs <+> ppr theta <+> ppr phi_ty,
				    text "free_tvs" <+> ppr free_tvs,
				    text "forall_tys" <+> ppr forall_tys])	`thenM_`
#endif

    checkSigTyVarsWrt free_tvs forall_tvs		`thenM` \ zonked_tvs ->

    traceTc (text "tcGen:done") `thenM_`

    let
	    -- This HsLet binds any Insts which came out of the simplification.
	    -- It's a bit out of place here, but using AbsBind involves inventing
	    -- a couple of new names which seems worse.
	dict_ids = map instToId dicts
	co_fn e  = TyLam zonked_tvs (DictLam dict_ids (mkHsLet inst_binds e))
    in
    returnM (mkCoercion co_fn, result)
  where
    free_tvs = tyVarsOfType expected_ty `unionVarSet` extra_tvs
    sig_msg  = ptext SLIT("expected type of an expression")
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
    addErrCtxtM (unifyCtxt "type" ty1 ty2) $
    uTys ty1 ty1 ty2 ty2
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: [TcTauType] -> [TcTauType] ->  TcM ()
unifyTauTyLists [] 	     []	        = returnM ()
unifyTauTyLists (ty1:tys1) (ty2:tys2) = uTys ty1 ty1 ty2 ty2   `thenM_`
					unifyTauTyLists tys1 tys2
unifyTauTyLists ty1s ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TcTauType] -> TcM ()
unifyTauTyList []		 = returnM ()
unifyTauTyList [ty]		 = returnM ()
unifyTauTyList (ty1:tys@(ty2:_)) = unifyTauTy ty1 ty2	`thenM_`
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
  = uTys fun1 fun1 fun2 fun2	`thenM_`    uTys arg1 arg1 arg2 arg2

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
	Just (s2,t2) -> uTys s1 s1 s2 s2	`thenM_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
uTys ps_ty1 ty1 ps_ty2 (AppTy s2 t2)
  = case tcSplitAppTy_maybe ty1 of
	Just (s1,t1) -> uTys s1 s1 s2 s2	`thenM_`    uTys t1 t1 t2 t2
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
  = traceTc (text "uVar" <+> ppr swapped <+> ppr tv1 <+> (ppr ps_ty2 $$ ppr ty2))	`thenM_`
    getTcTyVar tv1	`thenM` \ maybe_ty1 ->
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
  = returnM ()

	-- Distinct type variables
	-- ASSERT maybe_ty1 /= Just
  | otherwise
  = getTcTyVar tv2	`thenM` \ maybe_ty2 ->
    case maybe_ty2 of
	Just ty2' -> uUnboundVar swapped tv1 maybe_ty1 ty2' ty2'

	Nothing | update_tv2

		-> WARN( not (k1 `hasMoreBoxityInfo` k2), (ppr tv1 <+> ppr k1) $$ (ppr tv2 <+> ppr k2) )
		   putTcTyVar tv2 (TyVarTy tv1)		`thenM_`
		   returnM ()
		|  otherwise

		-> WARN( not (k2 `hasMoreBoxityInfo` k1), (ppr tv2 <+> ppr k2) $$ (ppr tv1 <+> ppr k1) )
                   putTcTyVar tv1 ps_ty2		`thenM_`
	  	   returnM ()
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
    checkM (not (isSkolemTyVar tv1))
	   (failWithTcM (unifyWithSigErr tv1 ps_ty2))	`thenM_`

	-- Do the occurs check, and check that we are not
	-- unifying a type variable with a polytype
	-- Returns a zonked type ready for the update
    checkValue tv1 ps_ty2 non_var_ty2	`thenM` \ ty2 ->

   	-- Check that the kinds match
    checkKinds swapped tv1 ty2		`thenM_`

	-- Perform the update
    putTcTyVar tv1 ty2			`thenM_`
    returnM ()
\end{code}

\begin{code}
checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- ty2 has been zonked at this stage, which ensures that
-- its kind has as much boxity information visible as possible.
  | tk2 `hasMoreBoxityInfo` tk1 = returnM ()

  | otherwise
	-- Either the kinds aren't compatible
	--	(can happen if we unify (a b) with (c d))
	-- or we are unifying a lifted type variable with an
	-- 	unlifted type: e.g.  (id 3#) is illegal
  = addErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$
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
  = zonkTcType ps_ty2			`thenM` \ ps_ty2' ->
    case okToUnifyWith tv1 ps_ty2' of {
	Nothing -> returnM ps_ty2' ;	-- Success
	other ->

    zonkTcType non_var_ty2		`thenM` \ non_var_ty2' ->
    case okToUnifyWith tv1 non_var_ty2' of
	Nothing -> 	-- This branch rarely succeeds, except in strange cases
			-- like that in the example above
		    returnM non_var_ty2'

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
\subsection{Kind unification}
%*									*
%************************************************************************

\begin{code}
unifyKind :: TcKind		    -- Expected
	  -> TcKind		    -- Actual
	  -> TcM ()
unifyKind k1 k2 = uTys k1 k1 k2 k2

unifyKinds :: [TcKind] -> [TcKind] -> TcM ()
unifyKinds []       []       = returnM ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenM_`
			       unifyKinds ks1 ks2
unifyKinds _ _ = panic "unifyKinds: length mis-match"
\end{code}

\begin{code}
unifyOpenTypeKind :: TcKind -> TcM ()	
-- Ensures that the argument kind is of the form (Type bx)
-- for some boxity bx

unifyOpenTypeKind ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyOpenTypeKind ty'
	other	 -> unify_open_kind_help ty

unifyOpenTypeKind ty
  | isTypeKind ty = returnM ()
  | otherwise     = unify_open_kind_help ty

unify_open_kind_help ty	-- Revert to ordinary unification
  = newOpenTypeKind 	`thenM` \ open_kind ->
    unifyKind ty open_kind
\end{code}

\begin{code}
unifyFunKind :: TcKind -> TcM (Maybe (TcKind, TcKind))
-- Like unifyFunTy, but does not fail; instead just returns Nothing

unifyFunKind (TyVarTy tyvar)
  = getTcTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just fun_kind -> unifyFunKind fun_kind
	Nothing       -> newKindVar	`thenM` \ arg_kind ->
			 newKindVar	`thenM` \ res_kind ->
			 putTcTyVar tyvar (mkArrowKind arg_kind res_kind)	`thenM_`
			 returnM (Just (arg_kind,res_kind))
    
unifyFunKind (FunTy arg_kind res_kind) = returnM (Just (arg_kind,res_kind))
unifyFunKind (NoteTy _ ty)	       = unifyFunKind ty
unifyFunKind other	   	       = returnM Nothing
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
  = zonkTcType ty1	`thenM` \ ty1' ->
    zonkTcType ty2	`thenM` \ ty2' ->
    returnM (err ty1' ty2')
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
  = zonkTcType ty2	`thenM` \ ty2' ->
    returnM (err ty2')
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
  = zonkTcType ty1	`thenM` \ ty1' ->
    zonkTcType ty2	`thenM` \ ty2' ->
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
checkSigTyVars :: [TcTyVar] -> TcM [TcTyVar]
checkSigTyVars sig_tvs = check_sig_tyvars emptyVarSet sig_tvs

checkSigTyVarsWrt :: TcTyVarSet -> [TcTyVar] -> TcM [TcTyVar]
checkSigTyVarsWrt extra_tvs sig_tvs
  = zonkTcTyVarsAndFV (varSetElems extra_tvs)	`thenM` \ extra_tvs' ->
    check_sig_tyvars extra_tvs' sig_tvs

check_sig_tyvars
	:: TcTyVarSet		-- Global type variables. The universally quantified
				-- 	tyvars should not mention any of these
				--	Guaranteed already zonked.
	-> [TcTyVar]		-- Universally-quantified type variables in the signature
				--	Not guaranteed zonked.
	-> TcM [TcTyVar]	-- Zonked signature type variables

check_sig_tyvars extra_tvs []
  = returnM []
check_sig_tyvars extra_tvs sig_tvs 
  = zonkTcTyVars sig_tvs	`thenM` \ sig_tys ->
    tcGetGlobalTyVars		`thenM` \ gbl_tvs ->
    let
	env_tvs = gbl_tvs `unionVarSet` extra_tvs
    in
    traceTc (text "check_sig_tyvars" <+> (vcat [text "sig_tys" <+> ppr sig_tys,
				      text "gbl_tvs" <+> ppr gbl_tvs,
				      text "extra_tvs" <+> ppr extra_tvs]))	`thenM_`

    checkM (allDistinctTyVars sig_tys env_tvs)
	   (complain sig_tys env_tvs)		`thenM_`

    returnM (map (tcGetTyVar "checkSigTyVars") sig_tys)

  where
    complain sig_tys globals
      = -- "check" checks each sig tyvar in turn
        foldlM check
	       (env2, emptyVarEnv, [])
	       (tidy_tvs `zip` tidy_tys)	`thenM` \ (env3, _, msgs) ->

        failWithTcM (env3, main_msg $$ nest 4 (vcat msgs))
      where
	(env1, tidy_tvs) = tidyOpenTyVars emptyTidyEnv sig_tvs
	(env2, tidy_tys) = tidyOpenTypes  env1	       sig_tys

	main_msg = ptext SLIT("Inferred type is less polymorphic than expected")

	check (tidy_env, acc, msgs) (sig_tyvar,ty)
		-- sig_tyvar is from the signature;
		-- ty is what you get if you zonk sig_tyvar and then tidy it
		--
		-- acc maps a zonked type variable back to a signature type variable
	  = case tcGetTyVar_maybe ty of {
	      Nothing ->			-- Error (a)!
			returnM (tidy_env, acc, unify_msg sig_tyvar (quotes (ppr ty)) : msgs) ;

	      Just tv ->

	    case lookupVarEnv acc tv of {
		Just sig_tyvar' -> 	-- Error (b)!
			returnM (tidy_env, acc, unify_msg sig_tyvar thing : msgs)
		    where
			thing = ptext SLIT("another quantified type variable") <+> quotes (ppr sig_tyvar')

	      ; Nothing ->

	    if tv `elemVarSet` globals	-- Error (c) or (d)! Type variable escapes
					-- The least comprehensible, so put it last
			-- Game plan: 
			--       get the local TcIds and TyVars from the environment,
			-- 	 and pass them to find_globals (they might have tv free)
	    then   findGlobals (unitVarSet tv) tidy_env 	`thenM` \ (tidy_env1, globs) ->
		   returnM (tidy_env1, acc, escape_msg sig_tyvar tv globs : msgs)

	    else 	-- All OK
	    returnM (tidy_env, extendVarEnv acc tv sig_tyvar, msgs)
	    }}
\end{code}


\begin{code}
-----------------------
escape_msg sig_tv tv globs
  = mk_msg sig_tv <+> ptext SLIT("escapes") $$
    if notNull globs then
	vcat [pp_it <+> ptext SLIT("is mentioned in the environment:"), 
	      nest 2 (vcat globs)]
     else
	empty	-- Sigh.  It's really hard to give a good error message
		-- all the time.   One bad case is an existential pattern match.
		-- We rely on the "When..." context to help.
  where
    pp_it | sig_tv /= tv = ptext SLIT("It unifies with") <+> quotes (ppr tv) <> comma <+> ptext SLIT("which")
	  | otherwise    = ptext SLIT("It")


unify_msg tv thing = mk_msg tv <+> ptext SLIT("is unified with") <+> thing
mk_msg tv          = ptext SLIT("Quantified type variable") <+> quotes (ppr tv)
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
		    nest 4 sub_msg]
    in
    returnM (env3, msg)
\end{code}
