%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Type subsumption and unification}

\begin{code}
module TcUnify (
	-- Full-blown subsumption
  tcSubPat, tcSubExp, tcSub, tcGen, 
  checkSigTyVars, checkSigTyVarsWrt, bleatEscapedTvs, sigCtxt, 

	-- Various unifications
  unifyTauTy, unifyTauTyList, unifyTheta,
  unifyKind, unifyKinds, unifyFunKind, 
  checkExpectedKind,

  --------------------------------
  -- Holes
  Expected(..), tcInfer, readExpectedType, 
  zapExpectedType, zapExpectedTo, zapExpectedBranches,
  subFunTys,   	 unifyFunTys, 
  zapToListTy, 	 unifyListTy, 
  zapToTyConApp, unifyTyConApp,
  unifyAppTy
  ) where

#include "HsVersions.h"

import HsSyn		( HsExpr(..) , MatchGroup(..), hsLMatchPats )
import TcHsSyn		( mkHsLet, mkHsDictLam,
			  ExprCoFn, idCoercion, isIdCoercion, mkCoercion, (<.>), (<$>) )
import TypeRep		( Type(..), PredType(..), TyNote(..) )

import TcRnMonad         -- TcType, amongst others
import TcType		( TcKind, TcType, TcSigmaType, TcRhoType, TcTyVar, TcTauType,
			  TcTyVarSet, TcThetaType, Expected(..), TcTyVarDetails(..),
			  SkolemInfo( GenSkol ), MetaDetails(..), 
			  pprTcTyVar, isTauTy, isSigmaTy, mkFunTys, mkTyConApp,
			  tcSplitAppTy_maybe, tcSplitTyConApp_maybe, 
			  tyVarsOfType, mkPhiTy, mkTyVarTy, mkPredTy,
			  typeKind, tcSplitFunTy_maybe, mkForAllTys, mkAppTy,
			  tidyOpenType, tidyOpenTypes, tidyOpenTyVar, tidyOpenTyVars,
			  pprType, tidySkolemTyVar, isSkolemTyVar )
import Kind		( Kind(..), SimpleKind, KindVar, isArgTypeKind,
			  openTypeKind, liftedTypeKind, mkArrowKind, 
			  isOpenTypeKind, argTypeKind, isLiftedTypeKind, isUnliftedTypeKind,
			  isSubKind, pprKind, splitKindFunTys )
import Inst		( newDicts, instToId, tcInstCall )
import TcMType		( condLookupTcTyVar, LookupTyVarResult(..),
                          tcSkolType, newKindVar, tcInstTyVars, newMetaTyVar,
			  newTyFlexiVarTy, zonkTcKind, zonkType, zonkTcType,  zonkTcTyVarsAndFV, 
			  readKindVar, writeKindVar )
import TcSimplify	( tcSimplifyCheck )
import TcIface		( checkWiredInTyCon )
import TcEnv		( tcGetGlobalTyVars, findGlobals )
import TyCon		( TyCon, tyConArity, tyConTyVars )
import TysWiredIn	( listTyCon )
import Id		( Id, mkSysLocal )
import Var		( Var, varName, tyVarKind )
import VarSet		( emptyVarSet, unitVarSet, unionVarSet, elemVarSet, varSetElems )
import VarEnv
import Name		( isSystemName, mkSysTvName )
import ErrUtils		( Message )
import SrcLoc		( noLoc )
import BasicTypes	( Arity )
import Util		( notNull, equalLength )
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
newHole = newMutVar (error "Empty hole in typechecker")

tcInfer :: (Expected ty -> TcM a) -> TcM (a,ty)
tcInfer tc_infer
  = do	{ hole <- newHole
	; res <- tc_infer (Infer hole)
	; res_ty <- readMutVar hole
	; return (res, res_ty) }

readExpectedType :: Expected ty -> TcM ty
readExpectedType (Infer hole) = readMutVar hole
readExpectedType (Check ty)   = returnM ty

zapExpectedType :: Expected TcType -> Kind -> TcM TcTauType
-- In the inference case, ensure we have a monotype
-- (including an unboxed tuple)
zapExpectedType (Infer hole) kind
  = do { ty <- newTyFlexiVarTy kind ;
	 writeMutVar hole ty ;
	 return ty }

zapExpectedType (Check ty) kind 
  | typeKind ty `isSubKind` kind = return ty
  | otherwise			 = do { ty1 <- newTyFlexiVarTy kind
				      ; unifyTauTy ty1 ty
				      ; return ty }
	-- The unify is to ensure that 'ty' has the desired kind
	-- For example, in (case e of r -> b) we push an OpenTypeKind
	-- type variable 

zapExpectedBranches :: MatchGroup id -> Expected TcRhoType -> TcM (Expected TcRhoType)
-- If there is more than one branch in a case expression, 
-- and exp_ty is a 'hole', all branches must be types, not type schemes, 
-- otherwise the order in which we check them would affect the result.
zapExpectedBranches (MatchGroup [match] _) exp_ty
   = return exp_ty	-- One branch
zapExpectedBranches matches (Check ty)
  = return (Check ty)
zapExpectedBranches matches (Infer hole)
  = do	{ 	-- Many branches, and inference mode, 
		-- so switch to checking mode with a monotype
	  ty <- newTyFlexiVarTy openTypeKind
	; writeMutVar hole ty
	; return (Check ty) }

zapExpectedTo :: Expected TcType -> TcTauType -> TcM ()
zapExpectedTo (Check ty1)  ty2 = unifyTauTy ty1 ty2
zapExpectedTo (Infer hole) ty2 = do { ty2' <- zonkTcType ty2; writeMutVar hole ty2' }
	-- See Note [Zonk return type]

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
subFunTys :: MatchGroup name
	  -> Expected TcRhoType		-- Fail if ty isn't a function type
	  -> ([Expected TcRhoType] -> Expected TcRhoType -> TcM a)
	  -> TcM a

subFunTys (MatchGroup (match:null_matches) _) (Infer hole) thing_inside
  = 	-- This is the interesting case
    ASSERT( null null_matches )
    do	{ pat_holes <- mapM (\ _ -> newHole) (hsLMatchPats match)
	; res_hole  <- newHole

		-- Do the business
	; res <- thing_inside (map Infer pat_holes) (Infer res_hole)

		-- Extract the answers
	; arg_tys <- mapM readMutVar pat_holes
	; res_ty  <- readMutVar res_hole

		-- Write the answer into the incoming hole
	; writeMutVar hole (mkFunTys arg_tys res_ty)

		-- And return the answer
	; return res }

subFunTys (MatchGroup (match:matches) _) (Check ty) thing_inside
  = ASSERT( all ((== length (hsLMatchPats match)) . length . hsLMatchPats) matches )
	-- Assertion just checks that all the matches have the same number of pats
    do	{ (pat_tys, res_ty) <- unifyFunTys (length (hsLMatchPats match)) ty
	; thing_inside (map Check pat_tys) (Check res_ty) }

unifyFunTys :: Arity -> TcRhoType -> TcM ([TcSigmaType], TcRhoType)	 		
-- Fail if ty isn't a function type, otherwise return arg and result types
-- The result types are guaranteed wobbly if the argument is wobbly
--
-- Does not allocate unnecessary meta variables: if the input already is 
-- a function, we just take it apart.  Not only is this efficient, it's important
-- for 	(a) higher rank: the argument might be of form
--		(forall a. ty) -> other
--	    If allocated (fresh-meta-var1 -> fresh-meta-var2) and unified, we'd
--	    blow up with the meta var meets the forall
--
--	(b) GADTs: if the argument is not wobbly we do not want the result to be

unifyFunTys arity ty = unify_fun_ty True arity ty

unify_fun_ty use_refinement arity ty
  | arity == 0 
  = do	{ res_ty <- wobblify use_refinement ty
	; return ([], ty) }

unify_fun_ty use_refinement arity (NoteTy _ ty)
  = unify_fun_ty use_refinement arity ty

unify_fun_ty use_refinement arity ty@(TyVarTy tv)
  = do	{ details <- condLookupTcTyVar use_refinement tv
	; case details of
	    IndirectTv use' ty' -> unify_fun_ty use' arity ty'
	    other 		-> unify_fun_help arity ty
	}

unify_fun_ty use_refinement arity ty
  = case tcSplitFunTy_maybe ty of
	Just (arg,res) -> do { arg'	     <- wobblify use_refinement arg
			     ; (args', res') <- unify_fun_ty use_refinement (arity-1) res
			     ; return (arg':args', res') }

	Nothing -> unify_fun_help arity ty
	-- Usually an error, but ty could be (a Int Bool), which can match

unify_fun_help :: Arity -> TcRhoType -> TcM ([TcSigmaType], TcRhoType)	 		
unify_fun_help arity ty
  = do { args <- mappM newTyFlexiVarTy (replicate arity argTypeKind)
       ; res <- newTyFlexiVarTy openTypeKind
       ; unifyTauTy ty (mkFunTys args res)
       ; return (args, res) }
\end{code}

\begin{code}
----------------------
zapToTyConApp :: TyCon			-- T :: k1 -> ... -> kn -> *
	      -> Expected TcSigmaType 	-- Expected type (T a b c)
	      -> TcM [TcType]      	-- Element types, a b c
  -- Insists that the Expected type is not a forall-type
  -- It's used for wired-in tycons, so we call checkWiredInTyCOn
zapToTyConApp tc (Check ty)
   = do { checkWiredInTyCon tc ; unifyTyConApp tc ty }	 -- NB: fails for a forall-type

zapToTyConApp tc (Infer hole) 
  = do	{ (tc_app, elt_tys) <- newTyConApp tc
	; writeMutVar hole tc_app
	; traceTc (text "zap" <+> ppr tc)
	; checkWiredInTyCon tc
	; return elt_tys }

zapToListTy :: Expected TcType -> TcM TcType	-- Special case for lists
zapToListTy exp_ty = do	{ [elt_ty] <- zapToTyConApp listTyCon exp_ty
			; return elt_ty }

----------------------
unifyTyConApp :: TyCon -> TcType -> TcM [TcType]
unifyTyConApp tc ty = unify_tc_app True tc ty
	-- Add a boolean flag to remember whether to use 
	-- the type refinement or not

unifyListTy :: TcType -> TcM TcType	-- Special case for lists
unifyListTy exp_ty = do	{ [elt_ty] <- unifyTyConApp listTyCon exp_ty
			; return elt_ty }

----------
unify_tc_app use_refinement tc (NoteTy _ ty)
  = unify_tc_app use_refinement tc ty

unify_tc_app use_refinement tc ty@(TyVarTy tyvar)
  = do	{ details <- condLookupTcTyVar use_refinement tyvar
	; case details of
	    IndirectTv use' ty' -> unify_tc_app use' tc ty'
	    other	 	-> unify_tc_app_help tc ty
	}

unify_tc_app use_refinement tc ty
  | Just (tycon, arg_tys) <- tcSplitTyConApp_maybe ty,
    tycon == tc
  = ASSERT( tyConArity tycon == length arg_tys )	-- ty::*
    mapM (wobblify use_refinement) arg_tys		

unify_tc_app use_refinement tc ty = unify_tc_app_help tc ty

unify_tc_app_help tc ty		-- Revert to ordinary unification
  = do	{ (tc_app, arg_tys) <- newTyConApp tc
	; if not (isTauTy ty) then	-- Can happen if we call zapToTyConApp tc (forall a. ty)
	     unifyMisMatch ty tc_app
	  else do
	{ unifyTauTy ty tc_app
	; returnM arg_tys } }


----------------------
unifyAppTy :: TcType			-- Type to split: m a
	   -> TcM (TcType, TcType)	-- (m,a)
-- Assumes (m:*->*)

unifyAppTy ty = unify_app_ty True ty

unify_app_ty use (NoteTy _ ty) = unify_app_ty use ty

unify_app_ty use ty@(TyVarTy tyvar)
  = do	{ details <- condLookupTcTyVar use tyvar
	; case details of
	    IndirectTv use' ty' -> unify_app_ty use' ty'
	    other	 	-> unify_app_ty_help ty
	}

unify_app_ty use ty
  | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
  = do	{ fun' <- wobblify use fun_ty
	; arg' <- wobblify use arg_ty
	; return (fun', arg') }

  | otherwise = unify_app_ty_help ty

unify_app_ty_help ty		-- Revert to ordinary unification
  = do	{ fun_ty <- newTyFlexiVarTy (mkArrowKind liftedTypeKind liftedTypeKind)
	; arg_ty <- newTyFlexiVarTy liftedTypeKind
	; unifyTauTy (mkAppTy fun_ty arg_ty) ty
	; return (fun_ty, arg_ty) }


----------------------
wobblify :: Bool	-- True <=> don't wobblify
	 -> TcTauType
	 -> TcM TcTauType	
-- Return a wobbly type.  At the moment we do that by 
-- allocating a fresh meta type variable.
wobblify True  ty = return ty
wobblify False ty = do	{ uniq <- newUnique
    			; tv <- newMetaTyVar (mkSysTvName uniq FSLIT("w")) 
					     (typeKind ty) 
					     (Indirect ty)
			; return (mkTyVarTy tv) }

----------------------
newTyConApp :: TyCon -> TcM (TcTauType, [TcTauType])
newTyConApp tc = do { (tvs, args, _) <- tcInstTyVars (tyConTyVars tc)
		    ; return (mkTyConApp tc args, args) }
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
-----------------------
-- tcSubExp is used for expressions
tcSubExp :: Expected TcRhoType -> TcRhoType  -> TcM ExprCoFn

tcSubExp (Infer hole) offered_ty
  = do { offered' <- zonkTcType offered_ty
	-- Note [Zonk return type]
	-- zonk to take advantage of the current GADT type refinement.
	-- If we don't we get spurious "existential type variable escapes":
	-- 	case (x::Maybe a) of
	--	  Just b (y::b) -> y
	-- We need the refinement [b->a] to be applied to the result type
	; writeMutVar hole offered'
	; return idCoercion }

tcSubExp (Check expected_ty) offered_ty
  = tcSub expected_ty offered_ty

-----------------------
-- tcSubPat is used for patterns
tcSubPat :: TcSigmaType 		-- Pattern type signature
	 -> Expected TcSigmaType	-- Type from context
	 -> TcM ()
-- In patterns we insist on an exact match; hence no CoFn returned
-- 	See Note [Pattern coercions] in TcPat
-- However, we can't call unify directly, because both types might be
-- polymorphic; hence the call to tcSub, followed by a check for
-- the identity coercion

tcSubPat sig_ty (Infer hole) 
  = do { sig_ty' <- zonkTcType sig_ty
	; writeMutVar hole sig_ty'	-- See notes with tcSubExp above
	; return () }

tcSubPat sig_ty (Check exp_ty) 
  = do	{ co_fn <- tcSub sig_ty exp_ty

	; if isIdCoercion co_fn then
		return ()
	  else
		unifyMisMatch sig_ty exp_ty }
\end{code}



%************************************************************************
%*									*
	tcSub: main subsumption-check code
%*									*
%************************************************************************

No holes expected now.  Add some error-check context info.

\begin{code}
-----------------
tcSub :: TcSigmaType -> TcSigmaType -> TcM ExprCoFn	-- Locally used only
	-- tcSub exp act checks that 
	--	act <= exp
tcSub expected_ty actual_ty
  = traceTc (text "tcSub" <+> details)		`thenM_`
    addErrCtxtM (unifyCtxt "type" expected_ty actual_ty)
		(tc_sub expected_ty expected_ty actual_ty actual_ty)
  where
    details = vcat [text "Expected:" <+> ppr expected_ty,
		    text "Actual:  " <+> ppr actual_ty]

-----------------
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
  = tcInstCall InstSigOrigin actual_ty		`thenM` \ (inst_fn, _, body_ty) ->
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
--	is perfectly fine, because we can instantiate f's type to a monotype
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

tc_sub exp_sty exp_ty@(FunTy exp_arg exp_res) _ act_ty
  = do	{ ([act_arg], act_res) <- unifyFunTys 1 act_ty
	; tcSub_fun exp_arg exp_res act_arg act_res }

tc_sub _ exp_ty act_sty act_ty@(FunTy act_arg act_res)
  = do	{ ([exp_arg], exp_res) <- unifyFunTys 1 exp_ty
 	; tcSub_fun exp_arg exp_res act_arg act_res }

-----------------------------------
-- Unification case
-- If none of the above match, we revert to the plain unifier
tc_sub exp_sty expected_ty act_sty actual_ty
  = uTys True exp_sty expected_ty True act_sty actual_ty	`thenM_`
    returnM idCoercion
\end{code}    
    
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
		     (noLoc (co_fn_res <$> (HsApp (noLoc e) (noLoc (co_fn_arg <$> HsVar arg_id)))))
		-- Slight hack; using a "DictLam" to get an ordinary simple lambda
		-- 	HsVar arg_id :: HsExpr exp_arg
		--	co_fn_arg $it :: HsExpr act_arg
		--	HsApp e $it   :: HsExpr act_res
		-- 	co_fn_res $it :: HsExpr exp_res
    in
    returnM coercion
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
  = do	{	-- We want the GenSkol info in the skolemised type variables to 
		-- mention the *instantiated* tyvar names, so that we get a
		-- good error message "Rigid variable 'a' is bound by (forall a. a->a)"
		-- Hence the tiresome but innocuous fixM
	  ((forall_tvs, theta, rho_ty), skol_info) <- fixM (\ ~(_, skol_info) ->
		do { (forall_tvs, theta, rho_ty) <- tcSkolType skol_info expected_ty
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
		dict_ids = map instToId dicts
		co_fn e  = TyLam forall_tvs (mkHsDictLam dict_ids (mkHsLet inst_binds (noLoc e)))
	; returnM (mkCoercion co_fn, result) }
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
    uTys True ty1 ty1 True ty2 ty2

unifyTheta :: TcThetaType -> TcThetaType -> TcM ()
unifyTheta theta1 theta2
  = do { checkTc (equalLength theta1 theta2)
		 (ptext SLIT("Contexts differ in length"))
       ; unifyTauTyLists True (map mkPredTy theta1) True (map mkPredTy theta2) }
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: Bool ->  -- Allow refinements on tys1
                   [TcTauType] ->
                   Bool ->  -- Allow refinements on tys2
                   [TcTauType] ->  TcM ()
-- Precondition: lists must be same length
-- Having the caller check gives better error messages
-- Actually the caller neve does  need to check; see Note [Tycon app]
unifyTauTyLists r1 [] 	      r2 []	        = returnM ()
unifyTauTyLists r1 (ty1:tys1) r2 (ty2:tys2)     = uTys r1 ty1 ty1 r2 ty2 ty2   `thenM_`
					unifyTauTyLists r1 tys1 r2 tys2
unifyTauTyLists r1 ty1s r2 ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
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
uTys :: Bool                    -- Allow refinements to ty1
     -> TcTauType -> TcTauType	-- Error reporting ty1 and real ty1
				-- ty1 is the *expected* type
     -> Bool                    -- Allow refinements to ty2 
     -> TcTauType -> TcTauType	-- Error reporting ty2 and real ty2
				-- ty2 is the *actual* type
     -> TcM ()

	-- Always expand synonyms (see notes at end)
        -- (this also throws away FTVs)
uTys r1 ps_ty1 (NoteTy n1 ty1) r2 ps_ty2 ty2 = uTys r1 ps_ty1 ty1 r2 ps_ty2 ty2
uTys r1 ps_ty1 ty1 r2 ps_ty2 (NoteTy n2 ty2) = uTys r1 ps_ty1 ty1 r2 ps_ty2 ty2

	-- Variables; go for uVar
uTys r1 ps_ty1 (TyVarTy tyvar1) r2 ps_ty2 ty2 = uVar False r1 tyvar1 r2 ps_ty2 ty2
uTys r1 ps_ty1 ty1 r2 ps_ty2 (TyVarTy tyvar2) = uVar True  r2 tyvar2 r1 ps_ty1 ty1
					-- "True" means args swapped

	-- Predicates
uTys r1 _ (PredTy (IParam n1 t1)) r2 _ (PredTy (IParam n2 t2))
  | n1 == n2 = uTys r1 t1 t1 r2 t2 t2
uTys r1 _ (PredTy (ClassP c1 tys1)) r2 _ (PredTy (ClassP c2 tys2))
  | c1 == c2 = unifyTauTyLists r1 tys1 r2 tys2
	-- Guaranteed equal lengths because the kinds check

	-- Functions; just check the two parts
uTys r1 _ (FunTy fun1 arg1) r2 _ (FunTy fun2 arg2)
  = uTys r1 fun1 fun1 r2 fun2 fun2	`thenM_`    uTys r1 arg1 arg1 r2 arg2 arg2

	-- Type constructors must match
uTys r1 ps_ty1 (TyConApp con1 tys1) r2 ps_ty2 (TyConApp con2 tys2)
  | con1 == con2 = unifyTauTyLists r1 tys1 r2 tys2
	-- See Note [TyCon app]

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTys r1 ps_ty1 (AppTy s1 t1) r2 ps_ty2 ty2
  = case tcSplitAppTy_maybe ty2 of
	Just (s2,t2) -> uTys r1 s1 s1 r2 s2 s2	`thenM_`    uTys r1 t1 t1 r2 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
uTys r1 ps_ty1 ty1 r2 ps_ty2 (AppTy s2 t2)
  = case tcSplitAppTy_maybe ty1 of
	Just (s1,t1) -> uTys r1 s1 s1 r2 s2 s2	`thenM_`    uTys r1 t1 t1 r2 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Not expecting for-alls in unification
	-- ... but the error message from the unifyMisMatch more informative
	-- than a panic message!

	-- Anything else fails
uTys r1 ps_ty1 ty1 r2 ps_ty2 ty2  = unifyMisMatch ps_ty1 ps_ty2
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
     -> Bool            -- True, allow refinements to tv1, False don't
     -> TcTyVar
     -> Bool            -- Allow refinements to ty2? 
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM ()

uVar swapped r1 tv1 r2 ps_ty2 ty2
  = traceTc (text "uVar" <+> ppr swapped <+> ppr tv1 <+> (ppr ps_ty2 $$ ppr ty2))	`thenM_`
    condLookupTcTyVar r1 tv1	`thenM` \ details ->
    case details of
	IndirectTv r1' ty1 | swapped   -> uTys r2   ps_ty2 ty2 r1' ty1    ty1	-- Swap back
		           | otherwise -> uTys r1' ty1     ty1 r2  ps_ty2 ty2	-- Same order
	DoneTv details1 -> uDoneVar swapped tv1 details1 r2 ps_ty2 ty2

----------------
uDoneVar :: Bool 			-- Args are swapped
     	 -> TcTyVar -> TcTyVarDetails	-- Tyvar 1
     	 -> Bool	 		-- Allow refinements to ty2
     	 -> TcTauType -> TcTauType	-- Type 2
     	 -> TcM ()
-- Invariant: tyvar 1 is not unified with anything

uDoneVar swapped tv1 details1 r2 ps_ty2 (NoteTy n2 ty2)
  = 	-- Expand synonyms; ignore FTVs
    uDoneVar swapped tv1 details1 r2 ps_ty2 ty2

uDoneVar swapped tv1 details1 r2 ps_ty2 ty2@(TyVarTy tv2)
	-- Same type variable => no-op
  | tv1 == tv2
  = returnM ()

	-- Distinct type variables
  | otherwise
  = do	{ lookup2 <- condLookupTcTyVar r2 tv2
	; case lookup2 of
		IndirectTv b ty2' -> uDoneVar  swapped tv1 details1 b ty2' ty2'
		DoneTv details2	  -> uDoneVars swapped tv1 details1 tv2 details2
	}

uDoneVar swapped tv1 details1 r2 ps_ty2 non_var_ty2	-- ty2 is not a type variable
  = case details1 of
	MetaTv ref1 -> do {	-- Do the occurs check, and check that we are not
				-- unifying a type variable with a polytype
				-- Returns a zonked type ready for the update
			    ty2 <- checkValue tv1 r2 ps_ty2 non_var_ty2
			  ; updateMeta swapped tv1 ref1 ty2 }

	skolem_details -> unifyMisMatch (TyVarTy tv1) ps_ty2


----------------
uDoneVars :: Bool 			-- Args are swapped
      	  -> TcTyVar -> TcTyVarDetails	-- Tyvar 1
      	  -> TcTyVar -> TcTyVarDetails	-- Tyvar 2
      	  -> TcM ()
-- Invarant: the type variables are distinct, 
-- and are not already unified with anything

uDoneVars swapped tv1 (MetaTv ref1) tv2 details2
  = case details2 of
	MetaTv ref2 | update_tv2 -> updateMeta (not swapped) tv2 ref2 (mkTyVarTy tv1)
	other 	    		 -> updateMeta swapped 	     tv1 ref1 (mkTyVarTy tv2)
	-- Note that updateMeta does a sub-kind check
	-- We might unify (a b) with (c d) where b::*->* and d::*; this should fail
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    update_tv2 = k1 `isSubKind` k2 && (k1 /= k2 || nicer_to_update_tv2)
	-- Update the variable with least kind info
	-- See notes on type inference in Kind.lhs
	-- The "nicer to" part only applies if the two kinds are the same,
	-- so we can choose which to do.

    nicer_to_update_tv2 = isSystemName (varName tv2)
	-- Try to update sys-y type variables in preference to ones
	-- gotten (say) by instantiating a polymorphic function with
	-- a user-written type sig
	
uDoneVars swapped tv1 (SkolemTv _) tv2 details2
  = case details2 of
	MetaTv ref2 -> updateMeta (not swapped) tv2 ref2 (mkTyVarTy tv1)
	other 	    -> unifyMisMatch (mkTyVarTy tv1) (mkTyVarTy tv2)

uDoneVars swapped tv1 (SigSkolTv _ ref1) tv2 details2
  = case details2 of
	MetaTv ref2   -> updateMeta (not swapped) tv2 ref2 (mkTyVarTy tv1)
	SigSkolTv _ _ -> updateMeta swapped tv1 ref1 (mkTyVarTy tv2)
	other	      -> unifyMisMatch (mkTyVarTy tv1) (mkTyVarTy tv2)

----------------
updateMeta :: Bool -> TcTyVar -> IORef MetaDetails -> TcType -> TcM ()
-- Update tv1, which is flexi; occurs check is alrady done
updateMeta swapped tv1 ref1 ty2
  = do	{ checkKinds swapped tv1 ty2
	; writeMutVar ref1 (Indirect ty2) }
\end{code}

\begin{code}
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
\end{code}

\begin{code}
checkValue tv1 r2 ps_ty2 non_var_ty2
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
  = zonk_tc_type r2 ps_ty2			`thenM` \ ps_ty2' ->
    case okToUnifyWith tv1 ps_ty2' of {
	Nothing -> returnM ps_ty2' ;	-- Success
	other ->

    zonk_tc_type r2 non_var_ty2		`thenM` \ non_var_ty2' ->
    case okToUnifyWith tv1 non_var_ty2' of
	Nothing -> 	-- This branch rarely succeeds, except in strange cases
			-- like that in the example above
		    returnM non_var_ty2'

	Just problem -> failWithTcM (unifyCheck problem tv1 ps_ty2')
    }
  where
    zonk_tc_type refine ty
      = zonkType (\tv -> return (TyVarTy tv)) refine ty
	-- We may already be inside a wobbly type t2, and
	-- should take that into account here

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
    ok (PredTy st)   		= ok_st st
    ok (NoteTy (FTVNote _) t)   = ok t
    ok (NoteTy (SynNote t1) t2) = ok t1 `and` ok t2
		-- Type variables may be free in t1 but not t2
		-- A forall may be in t2 but not t1

    oks ts = foldr (and . ok) Nothing ts

    ok_st (ClassP _ ts) = oks ts
    ok_st (IParam _ t)  = ok t

    Nothing `and` m = m
    Just p  `and` m = Just p
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
		   nest 2 
			(vcat [
			   text "Expected" <+> text s <> colon <+> ppr tidy_ty1,
			   text "Inferred" <+> text s <> colon <+> ppr tidy_ty2
		        ]))
		  where
		    (env1, [tidy_ty1,tidy_ty2]) = tidyOpenTypes tidy_env [ty1,ty2]

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

unifyMisMatch ty1 ty2
  = do	{ (env1, pp1, extra1) <- ppr_ty emptyTidyEnv ty1
	; (env2, pp2, extra2) <- ppr_ty env1 ty2
	; let msg = sep [sep [ptext SLIT("Couldn't match") <+> pp1, nest 7 (ptext SLIT("against") <+> pp2)],
			 nest 2 extra1, nest 2 extra2]
    in
    failWithTcM (env2, msg) }

ppr_ty :: TidyEnv -> TcType -> TcM (TidyEnv, SDoc, SDoc)
ppr_ty env ty
  = do { ty' <- zonkTcType ty
       ; let (env1,tidy_ty) = tidyOpenType env ty'
	     simple_result  = (env1, quotes (ppr tidy_ty), empty)
       ; case tidy_ty of
	   TyVarTy tv 
		| isSkolemTyVar tv -> return (env2, pp_rigid tv',
					      pprTcTyVar tv')
		| otherwise -> return simple_result
		where
		  (env2, tv') = tidySkolemTyVar env1 tv
	   other -> return simple_result }
  where
    pp_rigid tv = ptext SLIT("the rigid variable") <+> quotes (ppr tv)

unifyCheck problem tyvar ty
  = (env2, hang msg
	      2 (sep [ppr tidy_tyvar, char '=', ppr tidy_ty]))
  where
    (env1, tidy_tyvar) = tidyOpenTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1         ty

    msg = case problem of
	    OccurCheck  -> ptext SLIT("Occurs check: cannot construct the infinite type:")
	    NotMonoType -> ptext SLIT("Cannot unify a type variable with a type scheme:")
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
  = tryTc (unifyKind exp_kind act_kind)	`thenM` \ (errs, mb_r) ->
    case mb_r of {
	Just _  -> returnM () ;	-- Unification succeeded
	Nothing ->

	-- So there's definitely an error
	-- Now to find out what sort
    zonkTcKind exp_kind		`thenM` \ exp_kind ->
    zonkTcKind act_kind		`thenM` \ act_kind ->

    let (exp_as, _) = splitKindFunTys exp_kind
        (act_as, _) = splitKindFunTys act_kind
	n_exp_as = length exp_as
	n_act_as = length act_as

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
				quotes (pprKind exp_kind) <> comma,
		   	  ptext SLIT("but") <+> quotes (ppr ty) <+> 
			  	ptext SLIT("has kind") <+> quotes (pprKind act_kind)]
   in
   failWithTc (err $$ more_info)
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
checkSigTyVarsWrt extra_tvs sig_tvs
  = zonkTcTyVarsAndFV (varSetElems extra_tvs)	`thenM` \ extra_tvs' ->
    check_sig_tyvars extra_tvs' sig_tvs

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
  = do	{ (env3, msgs) <- foldlM check (env2, []) (tidy_tvs `zip` tidy_zonked_tvs)
	; failWithTcM (env3, main_msg $$ nest 2 (vcat msgs)) }
  where
    (env1, tidy_tvs)         = tidyOpenTyVars emptyTidyEnv sig_tvs
    (env2, tidy_zonked_tvs) = tidyOpenTyVars env1 	  zonked_tvs

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
