%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CoreUtils]{Utility functions}

These functions are re-exported by the various parameterisations of
@CoreSyn@.

\begin{code}
#include "HsVersions.h"

module CoreFuns (
	typeOfCoreExpr, typeOfCoreAlts,

	instCoreExpr,   substCoreExpr,   -- UNUSED: cloneCoreExpr,
	substCoreExprUS, -- UNUSED: instCoreExprUS, cloneCoreExprUS,

	instCoreBindings,

	bindersOf,

	mkCoLetAny, mkCoLetNoUnboxed, mkCoLetUnboxedToCase,
	mkCoLetsAny, mkCoLetsNoUnboxed, mkCoLetsUnboxedToCase,
	mkCoLetrecAny, mkCoLetrecNoUnboxed,
	mkCoLam, mkCoreIfThenElse,
--	mkCoApp, mkCoCon, mkCoPrim, -- no need to export
	mkCoApps,
	mkCoTyLam, mkCoTyApps,
	mkErrorCoApp, escErrorMsg,
	pairsFromCoreBinds,
	mkFunction, atomToExpr,
	digForLambdas,
	exprSmallEnoughToDup,
	manifestlyWHNF, manifestlyBottom, --UNUSED: manifestWHNFArgs,
	coreExprArity,
	isWrapperFor,
	maybeErrorApp,
--UNUSED: boilsDownToConApp,
	nonErrorRHSs,
	squashableDictishCcExpr,

	unTagBinders, unTagBindersAlts,

#ifdef DPH
	mkNonRecBinds,
	isParCoreCaseAlternative,
#endif {- Data Parallel Haskell -}

	-- to make the interface self-sufficient...
	CoreAtom, CoreExpr, Id, UniType, UniqueSupply, UniqSM(..),
	IdEnv(..), UniqFM, Unique, TyVarEnv(..), Maybe
    ) where

--IMPORT_Trace		-- ToDo: debugging only
import Pretty

import AbsPrel		( mkFunTy, trueDataCon, falseDataCon,
			  eRROR_ID, pAT_ERROR_ID, aBSENT_ERROR_ID,
			  buildId, augmentId,
			  boolTyCon, fragilePrimOp,
			  PrimOp(..), typeOfPrimOp,
			  PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
#ifdef DPH
			  , mkPodTy, mkPodizedPodNTy
#endif {- Data Parallel Haskell -}
			)
import AbsUniType
import BasicLit		( isNoRepLit, typeOfBasicLit, BasicLit(..)
			  IF_ATTACK_PRAGMAS(COMMA isLitLitLit)
			)
import CostCentre	( isDictCC, CostCentre )
import Id
import IdEnv
import IdInfo
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
import Outputable
import CoreSyn
import PlainCore	-- the main stuff we're defining functions for
import SrcLoc		( SrcLoc, mkUnknownSrcLoc )
#ifdef DPH
import TyCon 		( getPodizedPodDimension )
#endif {- Data Parallel Haskell -}
import TyVarEnv
import SplitUniq
import Unique		-- UniqueSupply monadery used here
import Util
\end{code}

%************************************************************************
%*									*
\subsection[bindersOf]{Small but useful}
%*									*
%************************************************************************


\begin{code}
bindersOf :: CoreBinding bder bdee -> [bder]
bindersOf (CoNonRec binder _) = [binder]
bindersOf (CoRec pairs)       = [binder | (binder,_) <- pairs]
\end{code}


%************************************************************************
%*									*
\subsection[typeOfCore]{Find the type of a Core atom/expression}
%*									*
%************************************************************************

\begin{code}
typeOfCoreExpr :: PlainCoreExpr -> UniType
typeOfCoreExpr (CoVar var)		= getIdUniType var
typeOfCoreExpr (CoLit lit)		= typeOfBasicLit lit
typeOfCoreExpr (CoLet binds body)	= typeOfCoreExpr body
typeOfCoreExpr (CoSCC label expr)	= typeOfCoreExpr expr

-- a CoCon is a fully-saturated application of a data constructor
typeOfCoreExpr (CoCon con tys _)
  = applyTyCon (getDataConTyCon con) tys

-- and, analogously, ...
typeOfCoreExpr expr@(CoPrim op tys args)
  -- Note: CoPrims may be polymorphic, so we do de-forall'ing.
  = let
	op_ty	  = typeOfPrimOp op
	op_tau_ty = foldl applyTy op_ty tys
    in
    funResultTy op_tau_ty (length args)

typeOfCoreExpr (CoCase _ alts)	= typeOfCoreAlts alts
  -- Q: What if the one you happen to grab is an "error"?
  -- A: NO problem.  The type application of error to its type will give you
  -- 	the answer.

typeOfCoreExpr (CoLam binders expr)
  = foldr (mkFunTy . getIdUniType) (typeOfCoreExpr expr) binders

typeOfCoreExpr (CoTyLam tyvar expr)
  = case (quantifyTy [tyvar] (typeOfCoreExpr expr)) of
      (_, ty) -> ty	-- not worried about the TyVarTemplates that come back

typeOfCoreExpr expr@(CoApp _ _)   = typeOfCoreApp expr
typeOfCoreExpr expr@(CoTyApp _ _) = typeOfCoreApp expr

#ifdef DPH
typeOfCoreExpr (CoParCon con ctxt tys args)
  = mkPodizedPodNTy ctxt (applyTyCon (getDataConTyCon con) tys)

typeOfCoreExpr (CoZfExpr expr quals)
  = mkPodTy (typeOfCoreExpr expr)

typeOfCoreExpr (CoParComm _ expr _)
  = typeOfCoreExpr expr
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
typeOfCoreApp application
  = case (collectArgs application) of { (fun, args) ->
    apply_args (typeOfCoreExpr fun) args
    }
  where
    apply_args expr_ty [] = expr_ty

    apply_args fun_ty (TypeArg ty_arg : args)
      = apply_args (applyTy fun_ty ty_arg) args

    apply_args fun_ty (ValArg val_arg : args)
      = case (maybeUnpackFunTy fun_ty) of
	  Just (_, result_ty) -> apply_args result_ty args

	  Nothing -> pprPanic "typeOfCoreApp:\n" 
		(ppAboves
			[ppr PprDebug val_arg,
			 ppr PprDebug fun_ty,
			 ppr PprShowAll application])
\end{code}

\begin{code}
typeOfCoreAlts :: PlainCoreCaseAlternatives -> UniType
typeOfCoreAlts (CoAlgAlts [] deflt)         = typeOfDefault deflt
typeOfCoreAlts (CoAlgAlts ((_,_,rhs1):_) _) = typeOfCoreExpr rhs1

typeOfCoreAlts (CoPrimAlts [] deflt)       = typeOfDefault deflt
typeOfCoreAlts (CoPrimAlts ((_,rhs1):_) _) = typeOfCoreExpr rhs1
#ifdef DPH
typeOfCoreAlts (CoParAlgAlts _ _ _ [] deflt)       = typeOfDefault deflt
typeOfCoreAlts (CoParAlgAlts _ _ _ ((_,rhs1):_) _) = typeOfCoreExpr rhs1

typeOfCoreAlts (CoParPrimAlts _ _ [] deflt)       = typeOfDefault deflt
typeOfCoreAlts (CoParPrimAlts _ _ ((_,rhs1):_) _) = typeOfCoreExpr rhs1
#endif {- Data Parallel Haskell -}

typeOfDefault CoNoDefault           = panic "typeOfCoreExpr:CoCase:typeOfDefault"
typeOfDefault (CoBindDefault _ rhs) = typeOfCoreExpr rhs
\end{code}

%************************************************************************
%*									*
\subsection[CoreFuns-instantiate]{Instantiating core expressions: interfaces}
%*									*
%************************************************************************

These subst/inst functions {\em must not} use splittable
UniqueSupplies! (yet)

All of the desired functions are done by one piece of code, which
carries around a little (monadised) state (a @UniqueSupply@).
Meanwhile, here is what the outside world sees (NB: @UniqueSupply@
passed in and out):
\begin{code}
{- UNUSED:
cloneCoreExpr	:: UniqueSupply
		-> PlainCoreExpr -- template
		-> (UniqueSupply, PlainCoreExpr)

cloneCoreExpr us expr = instCoreExpr us expr
-}

--------------------

instCoreExpr	:: UniqueSupply
		-> PlainCoreExpr
		-> (UniqueSupply, PlainCoreExpr)

instCoreExpr us expr
  = initUs us (do_CoreExpr nullIdEnv nullTyVarEnv expr)

instCoreBindings :: UniqueSupply
		 -> [PlainCoreBinding]
		 -> (UniqueSupply, [PlainCoreBinding])

instCoreBindings us binds
  = initUs us (do_CoreBindings nullIdEnv nullTyVarEnv binds)

--------------------

substCoreExpr	:: UniqueSupply
		-> ValEnv
		-> TypeEnv  -- TyVar=>UniType
		-> PlainCoreExpr
		-> (UniqueSupply, PlainCoreExpr)

substCoreExpr us venv tenv expr
  = initUs us (substCoreExprUS venv tenv expr)

-- we are often already in a UniqSM world, so here are the interfaces
-- for that:
{- UNUSED:
cloneCoreExprUS	:: PlainCoreExpr{-template-} -> UniqSM PlainCoreExpr

cloneCoreExprUS = instCoreExprUS

instCoreExprUS	:: PlainCoreExpr -> UniqSM PlainCoreExpr

instCoreExprUS expr = do_CoreExpr nullIdEnv nullTyVarEnv expr
-}

--------------------

substCoreExprUS	:: ValEnv
		-> TypeEnv -- TyVar=>UniType
		-> PlainCoreExpr
		-> UniqSM PlainCoreExpr

substCoreExprUS venv tenv expr
  -- if the envs are empty, then avoid doing anything
  = if (isNullIdEnv venv && isNullTyVarEnv tenv) then
       returnUs expr
    else
       do_CoreExpr venv tenv expr
\end{code}

%************************************************************************
%*									*
\subsection[CoreFuns-inst-exprs]{Actual expression-instantiating code}
%*									*
%************************************************************************

The equiv code for @UniTypes@ is in @UniTyFuns@.

Because binders aren't necessarily unique: we don't do @plusEnvs@
(which check for duplicates); rather, we use the shadowing version,
@growIdEnv@ (and shorthand @addOneToIdEnv@).

\begin{code}
type ValEnv  = IdEnv PlainCoreExpr

do_CoreBinding :: ValEnv
	       -> TypeEnv
	       -> PlainCoreBinding
	       -> UniqSM (PlainCoreBinding, ValEnv)

do_CoreBinding venv tenv (CoNonRec binder rhs)
  = do_CoreExpr venv tenv rhs	`thenUs` \ new_rhs ->

    dup_binder tenv binder	`thenUs` \ (new_binder, (old, new)) ->
    -- now plug new bindings into envs
    let  new_venv = addOneToIdEnv venv old new  in

    returnUs (CoNonRec new_binder new_rhs, new_venv)

do_CoreBinding venv tenv (CoRec binds)
  = -- for letrec, we plug in new bindings BEFORE cloning rhss
    mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_maps) ->
    let  new_venv = growIdEnvList venv new_maps in

    mapUs (do_CoreExpr new_venv tenv) rhss `thenUs` \ new_rhss ->
    returnUs (CoRec (new_binders `zip` new_rhss), new_venv)
  where
    binders	= map fst binds
    rhss	= map snd binds
\end{code}

@do_CoreBindings@ takes into account the semantics of a list of
@CoreBindings@---things defined early in the list are visible later in
the list, but not vice versa.

\begin{code}
do_CoreBindings :: ValEnv
	        -> TypeEnv
	        -> [PlainCoreBinding]
	        -> UniqSM [PlainCoreBinding]

do_CoreBindings venv tenv [] = returnUs []
do_CoreBindings venv tenv (b:bs)
  = do_CoreBinding  venv     tenv b	`thenUs` \ (new_b,  new_venv) ->
    do_CoreBindings new_venv tenv bs	`thenUs` \  new_bs ->
    returnUs (new_b : new_bs)
\end{code}

\begin{code}
do_CoreAtom :: ValEnv
	    -> TypeEnv
	    -> PlainCoreAtom
	    -> UniqSM PlainCoreExpr

do_CoreAtom venv tenv a@(CoLitAtom lit)   = returnUs (CoLit lit)

do_CoreAtom venv tenv orig_a@(CoVarAtom v)
  = returnUs (
      case (lookupIdEnv venv v) of
        Nothing   -> --false:ASSERT(toplevelishId v)
		     CoVar v
        Just expr -> expr
    )
\end{code}

\begin{code}
do_CoreExpr :: ValEnv
	    -> TypeEnv
	    -> PlainCoreExpr
	    -> UniqSM PlainCoreExpr

do_CoreExpr venv tenv orig_expr@(CoVar var)
  = returnUs (
      case (lookupIdEnv venv var) of
	Nothing	    -> --false:ASSERT(toplevelishId var) (SIGH)
		       orig_expr
	Just expr   -> expr
    )

do_CoreExpr venv tenv e@(CoLit _) = returnUs e

do_CoreExpr venv tenv (CoCon  con ts as)
  = let
	new_ts = map (applyTypeEnvToTy tenv) ts
    in
    mapUs  (do_CoreAtom venv tenv) as `thenUs`  \ new_as ->
    mkCoCon con new_ts new_as

do_CoreExpr venv tenv (CoPrim op tys as)
  = let
	new_tys = map (applyTypeEnvToTy tenv) tys
    in
    mapUs  (do_CoreAtom venv tenv) as 	`thenUs`  \ new_as ->
    do_PrimOp op			`thenUs`  \ new_op ->
    mkCoPrim new_op new_tys new_as
  where
    do_PrimOp (CCallOp label is_asm may_gc arg_tys result_ty)
      = let
	    new_arg_tys   = map (applyTypeEnvToTy tenv) arg_tys
	    new_result_ty = applyTypeEnvToTy tenv result_ty
	in
	returnUs (CCallOp label is_asm may_gc new_arg_tys new_result_ty)

    do_PrimOp other_op = returnUs other_op

do_CoreExpr venv tenv (CoLam binders expr)
  = mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_maps) ->
    let  new_venv = growIdEnvList venv new_maps  in
    do_CoreExpr new_venv tenv expr  `thenUs` \ new_expr ->
    returnUs (CoLam new_binders new_expr)

do_CoreExpr venv tenv (CoTyLam tyvar expr)
  = dup_tyvar tyvar		    `thenUs` \ (new_tyvar, (old, new)) ->
    let
	new_tenv = addOneToTyVarEnv tenv old new
    in
    do_CoreExpr venv new_tenv expr  `thenUs` \ new_expr ->
    returnUs (CoTyLam new_tyvar new_expr)

do_CoreExpr venv tenv (CoApp expr atom)
  = do_CoreExpr venv tenv expr	`thenUs` \ new_expr ->
    do_CoreAtom venv tenv atom  `thenUs` \ new_atom ->
    mkCoApp new_expr new_atom

do_CoreExpr venv tenv (CoTyApp expr ty)
  = do_CoreExpr venv tenv expr	    `thenUs`  \ new_expr ->
    let
	new_ty = applyTypeEnvToTy tenv ty
    in
    returnUs (CoTyApp new_expr new_ty)

do_CoreExpr venv tenv (CoCase expr alts)
  = do_CoreExpr venv tenv expr	    `thenUs` \ new_expr ->
    do_alts venv tenv alts	    `thenUs` \ new_alts ->
    returnUs (CoCase new_expr new_alts)
  where
    do_alts venv tenv (CoAlgAlts alts deflt)
      = mapUs (do_boxed_alt venv tenv) alts `thenUs` \ new_alts ->
    	do_default venv tenv deflt	    `thenUs` \ new_deflt ->
	returnUs (CoAlgAlts new_alts new_deflt)
      where
	do_boxed_alt venv tenv (con, binders, expr)
	  = mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_vmaps) ->
	    let  new_venv = growIdEnvList venv new_vmaps  in
	    do_CoreExpr new_venv tenv expr  `thenUs` \ new_expr ->
	    returnUs (con, new_binders, new_expr)


    do_alts venv tenv (CoPrimAlts alts deflt)
      = mapUs (do_unboxed_alt venv tenv) alts `thenUs` \ new_alts ->
    	do_default venv tenv deflt	      `thenUs` \ new_deflt ->
	returnUs (CoPrimAlts new_alts new_deflt)
      where
	do_unboxed_alt venv tenv (lit, expr)
	  = do_CoreExpr venv tenv expr	`thenUs` \ new_expr ->
	    returnUs (lit, new_expr)
#ifdef DPH
    do_alts venv tenv (CoParAlgAlts tycon dim params alts deflt)
      = mapAndUnzipUs (dup_binder tenv) params `thenUs` \ (new_params,new_vmaps) ->
	let  new_venv = growIdEnvList venv new_vmaps  in
	mapUs (do_boxed_alt new_venv tenv) alts
					 `thenUs` \ new_alts ->
    	do_default venv tenv deflt	 `thenUs` \ new_deflt ->
	returnUs (CoParAlgAlts tycon dim new_params new_alts new_deflt)
      where
	do_boxed_alt venv tenv (con, expr)
	  = do_CoreExpr venv tenv expr  `thenUs` \ new_expr ->
	    returnUs (con,  new_expr)

    do_alts venv tenv (CoParPrimAlts tycon dim alts deflt)
      = mapUs (do_unboxed_alt venv tenv) alts `thenUs` \ new_alts ->
    	do_default venv tenv deflt	      `thenUs` \ new_deflt ->
	returnUs (CoParPrimAlts tycon dim new_alts new_deflt)
      where
	do_unboxed_alt venv tenv (lit, expr)
	  = do_CoreExpr venv tenv expr	`thenUs` \ new_expr ->
	    returnUs (lit, new_expr)
#endif {- Data Parallel Haskell -}

    do_default venv tenv CoNoDefault = returnUs CoNoDefault

    do_default venv tenv (CoBindDefault binder expr)
      =	dup_binder tenv binder		`thenUs` \ (new_binder, (old, new)) ->
	let  new_venv = addOneToIdEnv venv old new  in
        do_CoreExpr new_venv tenv expr	`thenUs` \ new_expr ->
	returnUs (CoBindDefault new_binder new_expr)

do_CoreExpr venv tenv (CoLet core_bind expr)
  = do_CoreBinding venv tenv core_bind	`thenUs` \ (new_bind, new_venv) ->
    -- and do the body of the let
    do_CoreExpr new_venv tenv expr  	`thenUs` \ new_expr ->
    returnUs (CoLet new_bind new_expr)

do_CoreExpr venv tenv (CoSCC label expr)
  = do_CoreExpr venv tenv expr	    	`thenUs` \ new_expr ->
    returnUs (CoSCC label new_expr)

#ifdef DPH
do_CoreExpr venv tenv (CoParCon  con ctxt ts es)
  = let
	new_ts = map (applyTypeEnvToTy tenv) ts
    in
    mapUs  (do_CoreExpr venv tenv) es) `thenUs`  \ new_es ->
    returnUs (CoParCon con ctxt new_ts new_es)

do_CoreExpr venv tenv (CoZfExpr expr quals)
  = do_CoreParQuals  venv  tenv quals	`thenUs` \ (quals',venv') ->
    do_CoreExpr      venv' tenv expr	`thenUs` \ expr'  ->
    returnUs (CoZfExpr expr' quals')

do_CoreExpr venv tenv (CoParComm dim expr comm)
  = do_CoreExpr venv tenv expr		`thenUs` \ expr' ->
    do_ParComm  comm			`thenUs` \ comm' ->
    returnUs (CoParComm dim expr' comm')
  where
     do_ParComm (CoParSend exprs)
       = mapUs (do_CoreExpr venv tenv) exprs `thenUs` \ exprs' ->
         returnUs (CoParSend exprs')
     do_ParComm (CoParFetch exprs)
       = mapUs (do_CoreExpr venv tenv) exprs `thenUs` \ exprs' ->
         returnUs (CoParFetch exprs')
     do_ParComm (CoToPodized)
       = returnUs (CoToPodized)
     do_ParComm (CoFromPodized)
       = returnUs (CoFromPodized)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
do_CoreParQuals :: ValEnv
	    -> TypeEnv
	    -> PlainCoreParQuals
	    -> UniqSM (PlainCoreParQuals, ValEnv)

do_CoreParQuals venv tenv (CoAndQuals l r) 
   = do_CoreParQuals venv       tenv r	`thenUs` \ (r',right_venv) ->
     do_CoreParQuals right_venv tenv l	`thenUs` \ (l',left_env) ->
     returnUs (CoAndQuals l' r',left_env)

do_CoreParQuals venv tenv (CoParFilter expr)
   = do_CoreExpr venv tenv expr		`thenUs` \ expr' ->
     returnUs (CoParFilter expr',venv))

do_CoreParQuals venv tenv (CoDrawnGen binders binder expr) 
   = mapAndUnzipUs (dup_binder tenv) binders `thenUs` 	\ (newBs,newMs) ->
     let  new_venv = growIdEnvList venv newMs  in
     dup_binder tenv binder		`thenUs` 	\ (newB,(old,new)) ->
     let  new_venv' = addOneToIdEnv new_venv old new in
     do_CoreExpr new_venv' tenv expr	`thenUs`	\ new_expr ->
     returnUs (CoDrawnGen newBs newB new_expr,new_venv')

do_CoreParQuals venv tenv (CoIndexGen exprs binder expr) 
   = mapUs (do_CoreExpr venv tenv) exprs `thenUs`	\ new_exprs ->
     dup_binder tenv binder		 `thenUs` 	\ (newB,(old,new)) ->
     let  new_venv = addOneToIdEnv venv old new  in
     do_CoreExpr new_venv tenv expr	`thenUs`	\ new_expr ->
     returnUs (CoIndexGen new_exprs newB new_expr,new_venv)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
dup_tyvar :: TyVar -> UniqSM (TyVar, (TyVar, UniType))
dup_tyvar tyvar
  = getUnique			`thenUs` \ uniq ->
    let  new_tyvar = cloneTyVar tyvar uniq  in
    returnUs (new_tyvar, (tyvar, mkTyVarTy new_tyvar))

-- same thing all over again --------------------

dup_binder :: TypeEnv -> Id -> UniqSM (Id, (Id, PlainCoreExpr))
dup_binder tenv b
  = if (toplevelishId b) then
	-- binder is "top-level-ish"; -- it should *NOT* be renamed
	-- ToDo: it's unsavoury that we return something to heave in env
	returnUs (b, (b, CoVar b))

    else -- otherwise, the full business
	getUnique			    `thenUs`  \ uniq ->
	let
	    new_b1 = mkIdWithNewUniq b uniq
	    new_b2 = applyTypeEnvToId tenv new_b1
	in
	returnUs (new_b2, (b, CoVar new_b2))
\end{code}

%************************************************************************
%*									*
\subsection[mk_CoreExpr_bits]{Routines to manufacture bits of @CoreExpr@}
%*									*
%************************************************************************

When making @CoLets@, we may want to take evasive action if the thing
being bound has unboxed type. We have different variants ...

@mkCoLet(s|rec)Any@ 		let-binds any binding, regardless of type
@mkCoLet(s|rec)NoUnboxed@ 	prohibits unboxed bindings
@mkCoLet(s)UnboxedToCase@ 	converts an unboxed binding to a case
				(unboxed bindings in a letrec are still prohibited)

\begin{code}
mkCoLetAny :: PlainCoreBinding -> PlainCoreExpr -> PlainCoreExpr

mkCoLetAny bind@(CoRec binds) body
  = mkCoLetrecAny binds body
mkCoLetAny bind@(CoNonRec binder rhs) body
  = case body of
      CoVar binder2 | binder `eqId` binder2
	 -> rhs   -- hey, I have the rhs
      other
	 -> CoLet bind body

mkCoLetsAny []    expr = expr
mkCoLetsAny binds expr = foldr mkCoLetAny expr binds

mkCoLetrecAny :: [(Id, PlainCoreExpr)]	-- bindings
  	      -> PlainCoreExpr		-- body
	      -> PlainCoreExpr 		-- result

mkCoLetrecAny []    body = body
mkCoLetrecAny binds body
  = CoLet (CoRec binds) body
\end{code}

\begin{code}
mkCoLetNoUnboxed :: PlainCoreBinding -> PlainCoreExpr -> PlainCoreExpr

mkCoLetNoUnboxed bind@(CoRec binds) body
  = mkCoLetrecNoUnboxed binds body
mkCoLetNoUnboxed bind@(CoNonRec binder rhs) body
  = ASSERT (not (isUnboxedDataType (getIdUniType binder)))
    case body of
      CoVar binder2 | binder `eqId` binder2
	 -> rhs   -- hey, I have the rhs
      other
	 -> CoLet bind body

mkCoLetsNoUnboxed []    expr = expr
mkCoLetsNoUnboxed binds expr = foldr mkCoLetNoUnboxed expr binds

mkCoLetrecNoUnboxed :: [(Id, PlainCoreExpr)]	-- bindings
  	            -> PlainCoreExpr		-- body
	            -> PlainCoreExpr 		-- result

mkCoLetrecNoUnboxed []    body = body
mkCoLetrecNoUnboxed binds body
  = ASSERT (all is_boxed_bind binds)
    CoLet (CoRec binds) body
  where
    is_boxed_bind (binder, rhs)
      = (not . isUnboxedDataType . getIdUniType) binder
\end{code}

\begin{code}
mkCoLetUnboxedToCase :: PlainCoreBinding -> PlainCoreExpr -> PlainCoreExpr

mkCoLetUnboxedToCase bind@(CoRec binds) body
  = mkCoLetrecNoUnboxed binds body
mkCoLetUnboxedToCase bind@(CoNonRec binder rhs) body
  = case body of
      CoVar binder2 | binder `eqId` binder2
	 -> rhs   -- hey, I have the rhs
      other
	 -> if (not (isUnboxedDataType (getIdUniType binder))) then
		CoLet bind body		 -- boxed...
	    else
#ifdef DPH
		let  (tycon,_,_) = getUniDataTyCon (getIdUniType binder) in
		if isPodizedPodTyCon tycon
		then CoCase rhs
		       (CoParPrimAlts tycon (getPodizedPodDimension tycon) []
		          (CoBindDefault binder body))
	        else
#endif {- DPH -}
		CoCase rhs		  -- unboxed...
	    	  (CoPrimAlts []
		    (CoBindDefault binder body))

mkCoLetsUnboxedToCase []    expr = expr
mkCoLetsUnboxedToCase binds expr = foldr mkCoLetUnboxedToCase expr binds
\end{code}

Clump CoLams together if possible; friendlier to the code generator.

\begin{code}
mkCoLam :: [binder] -> CoreExpr binder bindee -> CoreExpr binder bindee
mkCoLam []      body = body
mkCoLam binders body
  = case (digForLambdas body) of { (tyvars, body_binders, body_expr) ->
    if not (null tyvars) then
	pprTrace "Inner /\\'s:" (ppr PprDebug tyvars)
	  (CoLam binders (mkCoTyLam tyvars (mkCoLam body_binders body_expr)))
    else
	CoLam (binders ++ body_binders) body_expr
    }

mkCoTyLam :: [TyVar] -> CoreExpr binder bindee -> CoreExpr binder bindee
mkCoTyLam tvs body = foldr CoTyLam body tvs

mkCoTyApps :: CoreExpr binder bindee -> [UniType] -> CoreExpr binder bindee
mkCoTyApps expr tys = foldl mkCoTyApp expr tys
\end{code}

\begin{code}
mkCoreIfThenElse (CoVar bool) then_expr else_expr
    | bool `eqId` trueDataCon	= then_expr
    | bool `eqId` falseDataCon	= else_expr

mkCoreIfThenElse guard then_expr else_expr
  = CoCase guard
      (CoAlgAlts [ (trueDataCon,  [], then_expr),
		   (falseDataCon, [], else_expr) ]
		 CoNoDefault )
\end{code}

\begin{code}
mkErrorCoApp :: UniType -> Id -> String -> PlainCoreExpr

mkErrorCoApp ty str_var error_msg
--OLD:  | not (isPrimType ty)
  = CoLet (CoNonRec str_var (CoLit (NoRepStr (_PK_ error_msg)))) (
    CoApp (CoTyApp (CoVar pAT_ERROR_ID) ty) (CoVarAtom str_var))
{- TOO PARANOID: removed 95/02 WDP
  | otherwise
    -- for now, force the user to write their own suitably-typed error msg
  = error (ppShow 80 (ppAboves [
	ppStr "ERROR: can't generate a pattern-matching error message",
	ppStr " when a primitive type is involved.",
	ppCat [ppStr "Type:", ppr PprDebug ty],
	ppCat [ppStr "Var :", ppr PprDebug str_var],
	ppCat [ppStr "Msg :", ppStr error_msg]
    ]))
-}

escErrorMsg [] = []
escErrorMsg ('%':xs) = '%' : '%' : escErrorMsg xs
escErrorMsg (x:xs)   = x : escErrorMsg xs
\end{code}

For making @CoApps@ and @CoLets@, we must take appropriate evasive
action if the thing being bound has unboxed type.  @mkCoApp@ requires
a name supply to do its work.  Other-monad code will call @mkCoApp@
through its own interface function (e.g., the desugarer uses
@mkCoAppDs@).

@mkCoApp@, @mkCoCon@ and @mkCoPrim@ also handle the
arguments-must-be-atoms constraint.

\begin{code}
mkCoApp :: PlainCoreExpr -> PlainCoreExpr -> UniqSM PlainCoreExpr

mkCoApp e1 (CoVar v) = returnUs (CoApp e1 (CoVarAtom v))
mkCoApp e1 (CoLit l) = returnUs (CoApp e1 (CoLitAtom l))
mkCoApp e1 e2
  = let
	e2_ty = typeOfCoreExpr e2
    in
    getUnique	`thenUs` \ uniq ->
    let
	new_var = mkSysLocal SLIT("a") uniq e2_ty mkUnknownSrcLoc
    in
    returnUs (
    	mkCoLetUnboxedToCase (CoNonRec new_var e2)
		             (CoApp e1 (CoVarAtom new_var))
    )
\end{code}

\begin{code}
mkCoCon  :: Id     -> [UniType] -> [PlainCoreExpr] -> UniqSM PlainCoreExpr
mkCoPrim :: PrimOp -> [UniType] -> [PlainCoreExpr] -> UniqSM PlainCoreExpr

mkCoCon con tys args = mkCoThing (CoCon con) tys args
mkCoPrim op tys args = mkCoThing (CoPrim op) tys args

mkCoThing thing tys args
  = mapAndUnzipUs expr_to_atom args `thenUs` \ (atoms, maybe_binds) ->
    returnUs (mkCoLetsUnboxedToCase (catMaybes maybe_binds) (thing tys atoms))
  where
    expr_to_atom :: PlainCoreExpr
	       -> UniqSM (PlainCoreAtom, Maybe PlainCoreBinding)

    expr_to_atom (CoVar v) = returnUs (CoVarAtom v, Nothing)
    expr_to_atom (CoLit l) = returnUs (CoLitAtom l, Nothing)
    expr_to_atom other_expr
      = let
	    e_ty = typeOfCoreExpr other_expr
	in
	getUnique	`thenUs` \ uniq ->
	let
	    new_var  = mkSysLocal SLIT("a") uniq e_ty mkUnknownSrcLoc
	    new_atom = CoVarAtom new_var
	in
	returnUs (new_atom, Just (CoNonRec new_var other_expr))
\end{code}

\begin{code}
atomToExpr :: CoreAtom bindee -> CoreExpr binder bindee

atomToExpr (CoVarAtom v)   = CoVar v
atomToExpr (CoLitAtom lit) = CoLit lit
\end{code}

\begin{code}
pairsFromCoreBinds :: [CoreBinding a b] -> [(a, CoreExpr a b)]

pairsFromCoreBinds []			 = []
pairsFromCoreBinds ((CoNonRec b e) : bs) = (b,e) :  (pairsFromCoreBinds bs)
pairsFromCoreBinds ((CoRec  pairs) : bs) = pairs ++ (pairsFromCoreBinds bs)
\end{code}

\begin{code}
#ifdef DPH
mkNonRecBinds :: [(a, CoreExpr a b)] -> [CoreBinding a b]
mkNonRecBinds xs = [ CoNonRec b e | (b,e) <- xs ]

isParCoreCaseAlternative :: CoreCaseAlternatives a b -> Bool
{-
isParCoreCaseAlternative (CoParAlgAlts _ _ _ _ _) = True
isParCoreCaseAlternative (CoParPrimAlts _ _ _ _)  = True
-}
isParCoreCaseAlternative  _			  = False
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
mkFunction tys args e
  = foldr CoTyLam (mkCoLam args e) tys

mkCoApps :: PlainCoreExpr -> [PlainCoreExpr] -> UniqSM PlainCoreExpr

mkCoApps fun []  = returnUs fun
mkCoApps fun (arg:args)
  = mkCoApp fun arg `thenUs` \ new_fun ->
    mkCoApps new_fun args
\end{code}

We often want to strip off leading \tr{/\}-bound @TyVars@ and
\tr{\}-bound binders, before we get down to business.  @digForLambdas@
is your friend.

\begin{code}
digForLambdas :: CoreExpr bndr bdee -> ([TyVar], [bndr], CoreExpr bndr bdee)

digForLambdas (CoTyLam tyvar body)
  = let
	(tyvars, args, final_body) = digForLambdas body
    in
    (tyvar:tyvars, args, final_body)

digForLambdas other
  = let
	(args, body) = dig_in_lambdas other
    in
    ([], args, body)
  where
    dig_in_lambdas (CoLam args_here body)
      = let
	    (args, final_body) = dig_in_lambdas body
	in
	(args_here ++ args, final_body)

#ifdef DEBUG
    dig_in_lambdas body@(CoTyLam ty expr) 
      =	trace "Inner /\\'s when digging" ([],body)
#endif

    dig_in_lambdas body
      = ([], body)
\end{code}

\begin{code}
exprSmallEnoughToDup :: CoreExpr binder Id -> Bool

exprSmallEnoughToDup (CoCon _ _ _)   = True	-- Could check # of args
exprSmallEnoughToDup (CoPrim op _ _) = not (fragilePrimOp op)	-- Could check # of args
exprSmallEnoughToDup (CoLit lit) = not (isNoRepLit lit)

exprSmallEnoughToDup expr  -- for now, just: <var> applied to <args>
  = case (collectArgs expr) of { (fun, args) ->
    case fun of
      CoVar v -> v /= buildId 
		 && v /= augmentId
		 && length args <= 6 -- or 10 or 1 or 4 or anything smallish.
      _       -> False
    }
\end{code}
Question (ADR): What is the above used for?  Is a _ccall_ really small
enough?

@manifestlyWHNF@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form.  It isn't a disaster if it
errs on the conservative side (returning \tr{False})---I've probably
left something out... [WDP]

\begin{code}
manifestlyWHNF :: CoreExpr bndr Id -> Bool

manifestlyWHNF (CoVar _)     = True
manifestlyWHNF (CoLit _)     = True
manifestlyWHNF (CoCon _ _ _) = True  -- ToDo: anything for CoPrim?
manifestlyWHNF (CoLam _ _)   = True
manifestlyWHNF (CoTyLam _ e) = manifestlyWHNF e
manifestlyWHNF (CoSCC _ e)   = manifestlyWHNF e
manifestlyWHNF (CoLet _ e)   = False
manifestlyWHNF (CoCase _ _)  = False

manifestlyWHNF other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, args) ->
    case fun of
      CoVar f -> let
		    num_val_args = length [ a | (ValArg a) <- args ]
		 in 
		 num_val_args == 0 ||		-- Just a type application of
						-- a variable (f t1 t2 t3)
						-- counts as WHNF
		 case (arityMaybe (getIdArity f)) of
		   Nothing     -> False
		   Just arity  -> num_val_args < arity

      _ -> False
    }
\end{code}

@manifestlyBottom@ looks at a Core expression and returns \tr{True} if
it is obviously bottom, that is, it will certainly return bottom at
some point.  It isn't a disaster if it errs on the conservative side
(returning \tr{False}).

\begin{code}
manifestlyBottom :: CoreExpr bndr Id -> Bool

manifestlyBottom (CoVar v)     = isBottomingId v
manifestlyBottom (CoLit _)     = False
manifestlyBottom (CoCon _ _ _) = False
manifestlyBottom (CoPrim _ _ _)= False
manifestlyBottom (CoLam _ _)   = False  -- we do not assume \x.bottom == bottom. should we? ToDo
manifestlyBottom (CoTyLam _ e) = manifestlyBottom e
manifestlyBottom (CoSCC _ e)   = manifestlyBottom e
manifestlyBottom (CoLet _ e)   = manifestlyBottom e

manifestlyBottom (CoCase e a)
  = manifestlyBottom e
  || (case a of
	CoAlgAlts  alts def -> all mbalg  alts && mbdef def
	CoPrimAlts alts def -> all mbprim alts && mbdef def
     )
  where
    mbalg  (_,_,e') = manifestlyBottom e'

    mbprim (_,e')   = manifestlyBottom e'

    mbdef CoNoDefault          = True
    mbdef (CoBindDefault _ e') = manifestlyBottom e'

manifestlyBottom other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, args) ->
    case fun of
      CoVar f | isBottomingId f -> True		-- Application of a function which
						-- always gives bottom; we treat this as
						-- a WHNF, because it certainly doesn't
						-- need to be shared!
      _ -> False
    }
\end{code}

UNUSED: @manifestWHNFArgs@ guarantees that an expression can absorb n args
before it ceases to be a manifest WHNF.  E.g.,
\begin{verbatim}
  (\x->x)   	 gives 1
  (\x -> +Int x) gives 2
\end{verbatim} 

The function guarantees to err on the side of conservatism: the
conservative result is (Just 0).

An applications of @error@ are special, because it can absorb as many
arguments as you care to give it.  For this special case we return Nothing.

\begin{code}
{- UNUSED:
manifestWHNFArgs :: CoreExpr bndr Id 
	         -> Maybe Int		-- Nothing indicates applicn of "error"

manifestWHNFArgs expr 
  = my_trace (man expr)
  where
    man (CoLit _)	= Just 0
    man (CoCon _ _ _)	= Just 0
    man (CoLam bs e)	= man e `plus_args`  length bs
    man (CoApp e _)	= man e `minus_args` 1
    man (CoTyLam _ e)	= man e
    man (CoSCC _ e)	= man e
    man (CoLet _ e)	= man e

    man (CoVar f)
      | isBottomingId f = Nothing
      | otherwise       = case (arityMaybe (getIdArity f)) of
			    Nothing    -> Just 0
			    Just arity -> Just arity

    man other 		= Just 0 -- Give up on case

    plus_args, minus_args :: Maybe Int -> Int -> Maybe Int

    plus_args Nothing m = Nothing
    plus_args (Just n) m = Just (n+m)

    minus_args Nothing m = Nothing 
    minus_args (Just n) m = Just (n-m)

    my_trace n = n 
    -- if n == 0 then n 
    -- else pprTrace "manifest:" (ppCat [ppr PprDebug fun, 
    --		           	  ppr PprDebug args, ppStr "=>", ppInt n]) 
    --			     	  n
-}
\end{code}

\begin{code}
coreExprArity 
	:: (Id -> Maybe (CoreExpr bndr Id))
	-> CoreExpr bndr Id 
	-> Int
coreExprArity f (CoLam bnds expr) = coreExprArity f expr + length (bnds)
coreExprArity f (CoTyLam _ expr) = coreExprArity f expr
coreExprArity f (CoApp expr arg) = max (coreExprArity f expr - 1) 0
coreExprArity f (CoTyApp expr _) = coreExprArity f expr
coreExprArity f (CoVar v) = max further info
   where
	further 
	     = case f v of
		Nothing -> 0
		Just expr -> coreExprArity f expr
	info = case (arityMaybe (getIdArity v)) of
		Nothing    -> 0
		Just arity -> arity	
coreExprArity f _ = 0
\end{code}

@isWrapperFor@: we want to see exactly:
\begin{verbatim}
/\ ... \ args -> case <arg> of ... -> case <arg> of ... -> wrkr <stuff>
\end{verbatim}

Probably a little too HACKY [WDP].

\begin{code}
isWrapperFor :: PlainCoreExpr -> Id -> Bool

expr `isWrapperFor` var
  = case (digForLambdas  expr) of { (_, args, body) -> -- lambdas off the front
    unravel_casing args body
    --NO, THANKS: && not (null args)
    }
  where
    var's_worker = getWorkerId (getIdStrictness var)

    is_elem = isIn "isWrapperFor"

    --------------
    unravel_casing case_ables (CoCase scrut alts)
      = case (collectArgs scrut) of { (fun, args) ->
	case fun of
	  CoVar scrut_var -> let
				answer =
				     scrut_var /= var && all (doesn't_mention var) args
				  && scrut_var `is_elem` case_ables
				  && unravel_alts case_ables alts
			     in
			     answer

	  _ -> False
	}

    unravel_casing case_ables other_expr
      = case (collectArgs other_expr) of { (fun, args) ->
	case fun of
	  CoVar wrkr -> let
			    answer =
				-- DOESN'T WORK: wrkr == var's_worker
				wrkr /= var
			     && isWorkerId wrkr
			     && all (doesn't_mention var)  args
			     && all (only_from case_ables) args
			in
			answer

	  _ -> False
	}

    --------------
    unravel_alts case_ables (CoAlgAlts [(_,params,rhs)] CoNoDefault)
      = unravel_casing (params ++ case_ables) rhs
    unravel_alts case_ables other = False

    -------------------------
    doesn't_mention var (ValArg (CoVarAtom v)) = v /= var
    doesn't_mention var other = True

    -------------------------
    only_from case_ables (ValArg (CoVarAtom v)) = v `is_elem` case_ables
    only_from case_ables other = True
\end{code}

All the following functions operate on binders, perform a uniform
transformation on them; ie. the function @(\ x -> (x,False))@
annotates all binders with False.

\begin{code}
unTagBinders :: CoreExpr (Id,tag) bdee -> CoreExpr Id bdee
unTagBinders e 	      = bop_expr fst e

unTagBindersAlts :: CoreCaseAlternatives (Id,tag) bdee -> CoreCaseAlternatives Id bdee
unTagBindersAlts alts = bop_alts fst alts
\end{code}

\begin{code}
bop_expr  :: (a -> b) -> (CoreExpr a c) -> CoreExpr b c

bop_expr f (CoVar b)		= CoVar b
bop_expr f (CoLit lit)		= CoLit lit
bop_expr f (CoCon id u atoms)	= CoCon id u atoms
bop_expr f (CoPrim op tys atoms)= CoPrim op tys atoms
bop_expr f (CoLam binders expr)	= CoLam [ f x | x <- binders ] (bop_expr f expr)
bop_expr f (CoTyLam ty expr)	= CoTyLam ty (bop_expr f expr)
bop_expr f (CoApp expr atom)	= CoApp (bop_expr f expr) atom
bop_expr f (CoTyApp expr ty)	= CoTyApp (bop_expr f expr) ty
bop_expr f (CoSCC label expr)	= CoSCC label (bop_expr f expr)
bop_expr f (CoLet bind expr)	= CoLet (bop_bind f bind) (bop_expr f expr)
bop_expr f (CoCase expr alts)
  = CoCase (bop_expr f expr) (bop_alts f alts)

bop_bind f (CoNonRec b e)	= CoNonRec (f b) (bop_expr f e)
bop_bind f (CoRec pairs)	= CoRec [(f b, bop_expr f e) | (b, e) <- pairs]

bop_alts f (CoAlgAlts alts deflt)
  = CoAlgAlts [ (con, [f b | b <- binders], bop_expr f e)
    	  | (con, binders, e) <- alts ]
    	  (bop_deflt f deflt)

bop_alts f (CoPrimAlts alts deflt)
  = CoPrimAlts [ (lit, bop_expr f e) | (lit, e) <- alts ]
    	   (bop_deflt f deflt)

bop_deflt f (CoNoDefault)		= CoNoDefault
bop_deflt f (CoBindDefault b expr)	= CoBindDefault (f b) (bop_expr f expr)

#ifdef DPH
bop_expr f (CoZfExpr expr quals)
  = CoZfExpr (bop_expr f expr) (bop_quals quals)
  where
    bop_quals (CoAndQuals l r)    = CoAndQuals (bop_quals l) (bop_quals r)
    bop_quals (CoParFilter e)     = CoParFilter (bop_expr f e)
    bop_quals (CoDrawnGen bs b e) = CoDrawnGen (map f bs) (f b) (bop_expr f e)
    bop_quals (CoIndexGen es b e) = CoIndexGen (map (bop_expr f) es) (f b)
					       (bop_expr f e)

bop_expr f (CoParCon con ctxt tys args)
  = CoParCon con ctxt tys (map (bop_expr f) args)

bop_expr f (CoParComm ctxt e comm)
  = CoParComm ctxt (bop_expr f e) (bop_comm comm)
  where
    bop_comm (CoParSend es)  = CoParSend  (map (bop_expr f) es)
    bop_comm (CoParFetch es) = CoParFetch (map (bop_expr f) es)
    bop_comm (CoToPodized)   = CoToPodized
    bop_comm (CoFromPodized) = CoFromPodized
#endif {- DPH -}
\end{code}

OLD (but left here because of the nice example): @singleAlt@ checks
whether a bunch of case alternatives is actually just one alternative.
It specifically {\em ignores} alternatives which consist of just a
call to @error@, because they won't result in any code duplication.

Example: 
\begin{verbatim}
	case (case <something> of
		True  -> <rhs>
		False -> error "Foo") of
	<alts>

===> 

	case <something> of
	   True ->  case <rhs> of
		    <alts>
	   False -> case error "Foo" of
		    <alts>

===>

	case <something> of
	   True ->  case <rhs> of
		    <alts>
	   False -> error "Foo"
\end{verbatim}
Notice that the \tr{<alts>} don't get duplicated.

\begin{code}
{- UNUSED:
boilsDownToConApp :: CoreExpr bndr bdee -> Bool	-- Looks through lets
  -- ToDo: could add something for NoRep literals...

boilsDownToConApp (CoCon _ _ _) = True
boilsDownToConApp (CoTyLam _ e) = boilsDownToConApp e
boilsDownToConApp (CoTyApp e _) = boilsDownToConApp e
boilsDownToConApp (CoLet _ e)	= boilsDownToConApp e
boilsDownToConApp other         = False
-}
\end{code}

\begin{code}
nonErrorRHSs :: CoreCaseAlternatives binder Id -> [CoreExpr binder Id]

nonErrorRHSs alts = filter not_error_app (find_rhss alts)
  where
    find_rhss (CoAlgAlts  alts deflt) = [rhs | (_,_,rhs) <- alts] ++ deflt_rhs deflt
    find_rhss (CoPrimAlts alts deflt) = [rhs | (_,rhs)   <- alts] ++ deflt_rhs deflt

    deflt_rhs CoNoDefault           = []
    deflt_rhs (CoBindDefault _ rhs) = [rhs]

    not_error_app rhs = case maybeErrorApp rhs Nothing of
			 Just _  -> False
			 Nothing -> True
\end{code}

maybeErrorApp checkes whether an expression is of the form

	error ty args

If so, it returns 

	Just (error ty' args)

where ty' is supplied as an argument to maybeErrorApp.

Here's where it is useful:

		case (error ty "Foo" e1 e2) of <alts>
 ===>
		error ty' "Foo"

where ty' is the type of any of the alternatives.
You might think this never occurs, but see the comments on
the definition of @singleAlt@.

Note: we *avoid* the case where ty' might end up as a
primitive type: this is very uncool (totally wrong).

NOTICE: in the example above we threw away e1 and e2, but
not the string "Foo".  How did we know to do that?

Answer: for now anyway, we only handle the case of a function
whose type is of form

	bottomingFn :: forall a. t1 -> ... -> tn -> a
	    	    	      ^---------------------^ NB!

Furthermore, we only count a bottomingApp if the function is
applied to more than n args.  If so, we transform:

	bottomingFn ty e1 ... en en+1 ... em
to
	bottomingFn ty' e1 ... en

That is, we discard en+1 .. em

\begin{code}
maybeErrorApp :: CoreExpr bndr Id   -- Expr to look at
	      -> Maybe UniType	    -- Just ty => a result type *already cloned*; 
				    -- Nothing => don't know result ty; we
				    -- *pretend* that the result ty won't be
				    -- primitive -- somebody later must
				    -- ensure this.
	       -> Maybe (CoreExpr bndr Id)

maybeErrorApp expr result_ty_maybe
  = case collectArgs expr of
      (CoVar fun, (TypeArg ty : other_args))
	| isBottomingId fun
	&& maybeToBool result_ty_maybe -- we *know* the result type
				       -- (otherwise: live a fairy-tale existence...)
	&& not (isPrimType result_ty) ->
	case splitType (getIdUniType fun) of
	  ([tyvar_tmpl], [], tau_ty) -> 
	      case (splitTyArgs tau_ty) of { (arg_tys, res_ty) ->
	      let			
		  n_args_to_keep = length arg_tys
		  args_to_keep   = take n_args_to_keep other_args
	      in
	      if  res_ty == mkTyVarTemplateTy tyvar_tmpl &&
		  n_args_to_keep <= length other_args
	      then
		    -- Phew!  We're in business
		  Just (applyToArgs (CoVar fun) 
				    (TypeArg result_ty : args_to_keep))
	      else
		  Nothing
	      }

	  other -> 	-- Function type wrong shape
		    Nothing
      other -> Nothing
  where
    Just result_ty = result_ty_maybe
\end{code}

\begin{code}
squashableDictishCcExpr :: CostCentre -> CoreExpr a b -> Bool

squashableDictishCcExpr cc expr
  = if not (isDictCC cc) then
	False -- that was easy...
    else
	squashable expr -- note: quite like the "atomic_rhs" stuff in simplifier
  where
    squashable (CoVar _)      = True
    squashable (CoTyApp f _)  = squashable f
    squashable (CoCon _ _ _)  = True -- I think so... WDP 94/09
    squashable (CoPrim _ _ _) = True -- ditto
    squashable other	      = False
\end{code}

