
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplBind ) where

#include "HsVersions.h"

import CmdLineOpts	( switchIsOn, opt_SccProfilingOn, opt_PprStyle_Debug,
			  opt_NoPreInlining, opt_DictsStrict, opt_D_dump_inlinings,
			  SimplifierSwitch(..)
			)
import SimplMonad
import SimplUtils	( mkCase, etaCoreExpr, etaExpandCount, findAlt, mkRhsTyLam,
			  simplBinder, simplBinders, simplIds, findDefault
			)
import Var		( TyVar, mkSysTyVar, tyVarKind )
import VarEnv
import VarSet
import Id		( Id, idType, 
			  getIdUnfolding, setIdUnfolding, 
			  getIdSpecialisation, setIdSpecialisation,
			  getIdDemandInfo, setIdDemandInfo,
			  getIdArity, setIdArity, 
			  getIdStrictness,
			  setInlinePragma, getInlinePragma, idMustBeINLINEd,
			  idWantsToBeINLINEd
			)
import IdInfo		( InlinePragInfo(..), OccInfo(..), StrictnessInfo(..), 
		 	  ArityInfo, atLeastArity, arityLowerBound, unknownArity
			)
import Demand		( Demand, isStrict, wwLazy )
import Const		( isWHNFCon, conOkForAlt )
import ConFold		( tryPrimOp )
import PrimOp		( PrimOp, primOpStrictness )
import DataCon		( DataCon, dataConNumInstArgs, dataConStrictMarks, dataConSig, dataConArgTys )
import Const		( Con(..) )
import MagicUFs		( applyMagicUnfoldingFun )
import Name		( isExported, isLocallyDefined )
import CoreSyn
import CoreUnfold	( Unfolding(..), UnfoldingGuidance(..),
			  mkUnfolding, smallEnoughToInline, 
			  isEvaldUnfolding
			)
import CoreUtils	( IdSubst, SubstCoreExpr(..),
			  cheapEqExpr, exprIsDupable, exprIsWHNF, exprIsTrivial,
			  coreExprType, coreAltsType, exprIsCheap, substExpr,
			  FormSummary(..), mkFormSummary, whnfOrBottom
			)
import SpecEnv		( lookupSpecEnv, isEmptySpecEnv, substSpecEnv )
import CostCentre	( isSubsumedCCS, currentCCS, isEmptyCC )
import Type		( Type, mkTyVarTy, mkTyVarTys, isUnLiftedType, fullSubstTy, 
			  mkFunTy, splitFunTys, splitTyConApp_maybe, splitFunTy_maybe,
			  applyTy, applyTys, funResultTy, isDictTy, isDataType
			)
import TyCon		( isDataTyCon, tyConDataCons, tyConClass_maybe, tyConArity, isDataTyCon )
import TysPrim		( realWorldStatePrimTy )
import PrelVals		( realWorldPrimId )
import BasicTypes	( StrictnessMark(..) )
import Maybes		( maybeToBool )
import Util		( zipWithEqual, stretchZipEqual )
import PprCore
import Outputable
\end{code}


The guts of the simplifier is in this module, but the driver
loop for the simplifier is in SimplPgm.lhs.


%************************************************************************
%*									*
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*									*
%************************************************************************

\begin{code}
addBind :: CoreBind -> OutStuff a -> OutStuff a
addBind bind    (binds,  res) = (bind:binds,     res)

addBinds :: [CoreBind] -> OutStuff a -> OutStuff a
addBinds []     stuff	      = stuff
addBinds binds1 (binds2, res) = (binds1++binds2, res)
\end{code}

The reason for this OutExprStuff stuff is that we want to float *after*
simplifying a RHS, not before.  If we do so naively we get quadratic
behaviour as things float out.

To see why it's important to do it after, consider this (real) example:

	let t = f x
	in fst t
==>
	let t = let a = e1
		    b = e2
		in (a,b)
	in fst t
==>
	let a = e1
	    b = e2
	    t = (a,b)
	in
	a	-- Can't inline a this round, cos it appears twice
==>
	e1

Each of the ==> steps is a round of simplification.  We'd save a
whole round if we float first.  This can cascade.  Consider

	let f = g d
	in \x -> ...f...
==>
	let f = let d1 = ..d.. in \y -> e
	in \x -> ...f...
==>
	let d1 = ..d..
	in \x -> ...(\y ->e)...

Only in this second round can the \y be applied, and it 
might do the same again.


\begin{code}
simplExpr :: CoreExpr -> SimplCont -> SimplM CoreExpr
simplExpr expr cont = simplExprB expr cont	`thenSmpl` \ (binds, (_, body)) ->
  		      returnSmpl (mkLetBinds binds body)

simplExprB :: InExpr -> SimplCont -> SimplM OutExprStuff

simplExprB (Note InlineCall (Var v)) cont
  = simplVar True v cont

simplExprB (Var v) cont
  = simplVar False v cont

simplExprB expr@(Con (PrimOp op) args) cont
  = simplType (coreExprType expr)	`thenSmpl` \ expr_ty ->
    getInScope				`thenSmpl` \ in_scope ->
    getSubstEnv				`thenSmpl` \ se ->
    let
	(val_arg_demands, _) = primOpStrictness op

	-- Main game plan: loop through the arguments, simplifying
	-- each of them with an ArgOf continuation.  Getting the right
	-- cont_ty in the ArgOf continuation is a bit of a nuisance.
        go []         ds     args' = rebuild_primop (reverse args')
        go (arg:args) ds     args' 
	   | isTypeArg arg  	   = setSubstEnv se (simplArg arg)	`thenSmpl` \ arg' ->
				     go args ds (arg':args')
        go (arg:args) (d:ds) args' 
	   | not (isStrict d)      = setSubstEnv se (simplArg arg)	`thenSmpl` \ arg' ->
				     go args ds (arg':args')
	   | otherwise		   = setSubstEnv se (simplExprB arg (mk_cont args ds args'))

	cont_ty = contResultType in_scope expr_ty cont
	mk_cont args ds args' = ArgOf NoDup (\ arg' -> go args ds (arg':args')) cont_ty
    in
    go args val_arg_demands []
  where

    rebuild_primop args'
      =	-- 	Try the prim op simplification
	-- It's really worth trying simplExpr again if it succeeds,
	-- because you can find
	--	case (eqChar# x 'a') of ...
	-- ==>  
	-- 	case (case x of 'a' -> True; other -> False) of ...
	case tryPrimOp op args' of
	  Just e' -> zapSubstEnv (simplExprB e' cont)
	  Nothing -> rebuild (Con (PrimOp op) args') cont

simplExprB (Con con@(DataCon _) args) cont
  = simplConArgs args		$ \ args' ->
    rebuild (Con con args') cont

simplExprB expr@(Con con@(Literal _) args) cont
  = ASSERT( null args )
    rebuild expr cont

simplExprB (App fun arg) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprB fun (ApplyTo NoDup arg se cont)

simplExprB (Case scrut bndr alts) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprB scrut (Select NoDup bndr alts se cont)

simplExprB (Note (Coerce to from) e) cont
  | to == from = simplExprB e cont
  | otherwise  = getSubstEnv		`thenSmpl` \ se ->
    		 simplExprB e (CoerceIt NoDup to se cont)

-- hack: we only distinguish subsumed cost centre stacks for the purposes of
-- inlining.  All other CCCSs are mapped to currentCCS.
simplExprB (Note (SCC cc) e) cont
  = setEnclosingCC currentCCS $
    simplExpr e Stop	`thenSmpl` \ e ->
    rebuild (mkNote (SCC cc) e) cont

simplExprB (Note note e) cont
  = simplExpr e Stop	`thenSmpl` \ e' ->
    rebuild (mkNote note e') cont

-- A non-recursive let is dealt with by simplBeta
simplExprB (Let (NonRec bndr rhs) body) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplBeta bndr rhs se body cont

simplExprB (Let (Rec pairs) body) cont
  = simplRecBind pairs (simplExprB body cont)

-- Type-beta reduction
simplExprB expr@(Lam bndr body) cont@(ApplyTo _ (Type ty_arg) arg_se body_cont)
  = ASSERT( isTyVar bndr )
    tick BetaReduction				`thenSmpl_`
    setSubstEnv arg_se (simplType ty_arg)	`thenSmpl` \ ty' ->
    extendTySubst bndr ty'			$
    simplExprB body body_cont

-- Ordinary beta reduction
simplExprB expr@(Lam bndr body) cont@(ApplyTo _ arg arg_se body_cont)
  = tick BetaReduction		`thenSmpl_`
    simplBeta bndr' arg arg_se body body_cont
  where
    bndr' = zapLambdaBndr bndr body body_cont

simplExprB (Lam bndr body) cont  
  = simplBinder bndr			$ \ bndr' ->
    simplExpr body Stop			`thenSmpl` \ body' ->
    rebuild (Lam bndr' body') cont

simplExprB (Type ty) cont
  = ASSERT( case cont of { Stop -> True; ArgOf _ _ _ -> True; other -> False } )
    simplType ty	`thenSmpl` \ ty' ->
    rebuild (Type ty') cont
\end{code}


---------------------------------
\begin{code}
simplArg :: InArg -> SimplM OutArg
simplArg arg = simplExpr arg Stop
\end{code}

---------------------------------
simplConArgs makes sure that the arguments all end up being atomic.
That means it may generate some Lets, hence the 

\begin{code}
simplConArgs :: [InArg] -> ([OutArg] -> SimplM OutExprStuff) -> SimplM OutExprStuff
simplConArgs [] thing_inside
  = thing_inside []

simplConArgs (arg:args) thing_inside
  = switchOffInlining (simplArg arg)	`thenSmpl` \ arg' ->
	-- Simplify the RHS with inlining switched off, so that
	-- only absolutely essential things will happen.

    simplConArgs args				$ \ args' ->

	-- If the argument ain't trivial, then let-bind it
    if exprIsTrivial arg' then
	thing_inside (arg' : args')
    else
	newId (coreExprType arg')		$ \ arg_id ->
	thing_inside (Var arg_id : args')	`thenSmpl` \ res ->
	returnSmpl (addBind (NonRec arg_id arg') res)
\end{code}


---------------------------------
\begin{code}
simplType :: InType -> SimplM OutType
simplType ty
  = getTyEnv		`thenSmpl` \ (ty_subst, in_scope) ->
    returnSmpl (fullSubstTy ty_subst in_scope ty)
\end{code}


\begin{code}
-- Find out whether the lambda is saturated, 
-- if not zap the over-optimistic info in the binder

zapLambdaBndr bndr body body_cont
  | isTyVar bndr || safe_info || definitely_saturated 20 body body_cont
	-- The "20" is to catch pathalogical cases with bazillions of arguments
	-- because we are using an n**2 algorithm here
  = bndr		-- No need to zap
  | otherwise
  = setInlinePragma (setIdDemandInfo bndr wwLazy)
		    safe_inline_prag

  where
    inline_prag      	= getInlinePragma bndr
    demand		= getIdDemandInfo bndr

    safe_info        	= is_safe_inline_prag && not (isStrict demand)

    is_safe_inline_prag = case inline_prag of
			 	ICanSafelyBeINLINEd StrictOcc nalts -> False
			 	ICanSafelyBeINLINEd LazyOcc   nalts -> False
				other				    -> True

    safe_inline_prag    = case inline_prag of
			 	ICanSafelyBeINLINEd _ nalts
				      -> ICanSafelyBeINLINEd InsideLam nalts
				other -> inline_prag

    definitely_saturated 0 _	        _		     = False	-- Too expensive to find out
    definitely_saturated n (Lam _ body) (ApplyTo _ _ _ cont) = definitely_saturated (n-1) body cont
    definitely_saturated n (Lam _ _)    other_cont	     = False
    definitely_saturated n _            _		     = True
\end{code}

%************************************************************************
%*									*
\subsection{Variables}
%*									*
%************************************************************************

Coercions
~~~~~~~~~
\begin{code}
simplVar inline_call var cont
  = getValEnv		`thenSmpl` \ (id_subst, in_scope) ->
    case lookupVarEnv id_subst var of
	Just (Done e)
		-> zapSubstEnv (simplExprB e cont)

	Just (SubstMe e ty_subst id_subst)
		-> setSubstEnv (ty_subst, id_subst) (simplExprB e cont)

	Nothing -> let
			var' = case lookupVarSet in_scope var of
				 Just v' -> v'
				 Nothing -> 
#ifdef DEBUG
					    if isLocallyDefined var && not (idMustBeINLINEd var) then
						-- Not in scope
						pprTrace "simplVar:" (ppr var) var
					    else
#endif
					    var
		   in
		   getSwitchChecker	`thenSmpl` \ sw_chkr ->
		   completeVar sw_chkr in_scope inline_call var' cont

completeVar sw_chkr in_scope inline_call var cont

{-	MAGIC UNFOLDINGS NOT USED NOW
  | maybeToBool maybe_magic_result
  = tick MagicUnfold	`thenSmpl_`
    magic_result
-}
	-- Look for existing specialisations before trying inlining
  | maybeToBool maybe_specialisation
  = tick SpecialisationDone			`thenSmpl_`
    setSubstEnv (spec_bindings, emptyVarEnv)	(
	-- See note below about zapping the substitution here

    simplExprB spec_template remaining_cont
    )

	-- Don't actually inline the scrutinee when we see
	--	case x of y { .... }
	-- and x has unfolding (C a b).  Why not?  Because
	-- we get a silly binding y = C a b.  If we don't
	-- inline knownCon can directly substitute x for y instead.
  | has_unfolding && var_is_case_scrutinee && unfolding_is_constr
  = knownCon (Var var) con con_args cont

	-- Look for an unfolding. There's a binding for the
	-- thing, but perhaps we want to inline it anyway
  | has_unfolding && (inline_call || ok_to_inline)
  = getEnclosingCC	`thenSmpl` \ encl_cc ->
    if must_be_unfolded || costCentreOk encl_cc (coreExprCc unf_template)
    then	-- OK to unfold

	tickUnfold var		`thenSmpl_` (

	zapSubstEnv 		$
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!
#ifdef DEBUG
	if opt_D_dump_inlinings then
		pprTrace "Inlining:" (ppr var <+> ppr unf_template) $
		simplExprB unf_template cont
	else
#endif
	simplExprB unf_template cont
	)
    else
#ifdef DEBUG
	pprTrace "Inlining disallowed due to CC:\n" (ppr encl_cc <+> ppr unf_template <+> ppr (coreExprCc unf_template)) $
#endif
	-- Can't unfold because of bad cost centre
	rebuild (Var var) cont

  | inline_call		-- There was an InlineCall note, but we didn't inline!
  = rebuild (Note InlineCall (Var var)) cont

  | otherwise
  = rebuild (Var var) cont

  where
    unfolding = getIdUnfolding var

{-	MAGIC UNFOLDINGS NOT USED CURRENTLY
	---------- Magic unfolding stuff
    maybe_magic_result	= case unfolding of
				MagicUnfolding _ magic_fn -> applyMagicUnfoldingFun magic_fn 
										    cont
			        other 			  -> Nothing
    Just magic_result = maybe_magic_result
-}

	---------- Unfolding stuff
    has_unfolding = case unfolding of
			CoreUnfolding _ _ _ -> True
			other		    -> False
    CoreUnfolding form guidance unf_template = unfolding

	-- overrides cost-centre business
    must_be_unfolded = case getInlinePragma var of
			  IMustBeINLINEd -> True
			  _		 -> False

    ok_to_inline	= okToInline sw_chkr in_scope var form guidance cont
    unfolding_is_constr = case unf_template of
				  Con con _ -> conOkForAlt con
				  other	    -> False
    Con con con_args    = unf_template

    	---------- Specialisation stuff
    ty_args		      = initial_ty_args cont
    remaining_cont	      = drop_ty_args cont
    maybe_specialisation      = lookupSpecEnv (ppr var) (getIdSpecialisation var) ty_args
    Just (spec_bindings, spec_template) = maybe_specialisation

    initial_ty_args (ApplyTo _ (Type ty) (ty_subst,_) cont) 
	= fullSubstTy ty_subst in_scope ty : initial_ty_args cont
	-- Having to do the substitution here is a bit of a bore
    initial_ty_args other_cont = []

    drop_ty_args (ApplyTo _ (Type _) _ cont) = drop_ty_args cont
    drop_ty_args other_cont		     = other_cont

	---------- Switches

    var_is_case_scrutinee = case cont of
				  Select _ _ _ _ _ -> True
				  other		   -> False

----------- costCentreOk
-- costCentreOk checks that it's ok to inline this thing
-- The time it *isn't* is this:
--
--	f x = let y = E in
--	      scc "foo" (...y...)
--
-- Here y has a "current cost centre", and we can't inline it inside "foo",
-- regardless of whether E is a WHNF or not.
    
costCentreOk ccs_encl cc_rhs
  =  not opt_SccProfilingOn
  || isSubsumedCCS ccs_encl	  -- can unfold anything into a subsumed scope
  || not (isEmptyCC cc_rhs)	  -- otherwise need a cc on the unfolding
\end{code}		   


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
simplBind :: InBind -> SimplM (OutStuff a) -> SimplM (OutStuff a)

simplBind (NonRec bndr rhs) thing_inside
  = simplTopRhs bndr rhs	`thenSmpl` \ (binds, in_scope,  rhs', arity) ->
    setInScope in_scope							$
    completeBindNonRec (bndr `setIdArity` arity) rhs' thing_inside	`thenSmpl` \ stuff ->
    returnSmpl (addBinds binds stuff)

simplBind (Rec pairs) thing_inside
  = simplRecBind pairs thing_inside
	-- The assymetry between the two cases is a bit unclean

simplRecBind :: [(InId, InExpr)] -> SimplM (OutStuff a) -> SimplM (OutStuff a)
simplRecBind pairs thing_inside
  = simplIds (map fst pairs) 		$ \ bndrs' -> 
	-- NB: bndrs' don't have unfoldings or spec-envs
	-- We add them as we go down, using simplPrags

    go (pairs `zip` bndrs')		`thenSmpl` \ (pairs', stuff) ->
    returnSmpl (addBind (Rec pairs') stuff)
  where
    go [] = thing_inside 	`thenSmpl` \ stuff ->
	    returnSmpl ([], stuff)

    go (((bndr, rhs), bndr') : pairs) 
	= simplTopRhs bndr rhs 				`thenSmpl` \ (rhs_binds, in_scope, rhs', arity) ->
	  setInScope in_scope				$
	  completeBindRec bndr (bndr' `setIdArity` arity) 
			  rhs' (go pairs)		`thenSmpl` \ (pairs', stuff) ->
	  returnSmpl (flatten rhs_binds pairs', stuff)

    flatten (NonRec b r : binds) prs  = (b,r) : flatten binds prs
    flatten (Rec prs1   : binds) prs2 = prs1 ++ flatten binds prs2
    flatten []			 prs  = prs


completeBindRec bndr bndr' rhs' thing_inside
  |  postInlineUnconditionally bndr etad_rhs
	-- NB: a loop breaker never has postInlineUnconditionally True
	-- and non-loop-breakers only have *forward* references
	-- Hence, it's safe to discard the binding
  =  tick PostInlineUnconditionally		`thenSmpl_`
     extendIdSubst bndr (Done etad_rhs) thing_inside

  |  otherwise
  =  	-- Here's the only difference from completeBindNonRec: we 
	-- don't do simplBinder first, because we've already
	-- done simplBinder on the recursive binders
     simplPrags bndr bndr' etad_rhs		`thenSmpl` \ bndr'' ->
     modifyInScope bndr''			$
     thing_inside				`thenSmpl` \ (pairs, res) ->
     returnSmpl ((bndr'', etad_rhs) : pairs, res)
  where
     etad_rhs = etaCoreExpr rhs'
\end{code}


%************************************************************************
%*									*
\subsection{Right hand sides}
%*									*
%************************************************************************

simplRhs basically just simplifies the RHS of a let(rec).
It does two important optimisations though:

	* It floats let(rec)s out of the RHS, even if they
	  are hidden by big lambdas

	* It does eta expansion

\begin{code}
simplTopRhs :: InId -> InExpr
  -> SimplM ([OutBind], InScopeEnv, OutExpr, ArityInfo)
simplTopRhs bndr rhs 
  = getSubstEnv  		`thenSmpl` \ bndr_se ->
    simplRhs bndr bndr_se rhs

simplRhs bndr bndr_se rhs
  | idWantsToBeINLINEd bndr	-- Don't inline in the RHS of something that has an
				-- inline pragma.  But be careful that the InScopeEnv that
				-- we return does still have inlinings on!
  = switchOffInlining (simplExpr rhs Stop)	`thenSmpl` \ rhs' ->
    getInScope					`thenSmpl` \ in_scope ->
    returnSmpl ([], in_scope, rhs', unknownArity)

  | otherwise
  = 	-- Swizzle the inner lets past the big lambda (if any)
    mkRhsTyLam rhs			`thenSmpl` \ swizzled_rhs ->

	-- Simplify the swizzled RHS
    simplRhs2 bndr bndr_se swizzled_rhs	`thenSmpl` \ (floats, (in_scope, rhs', arity)) ->

    if not (null floats) && exprIsWHNF rhs' then	-- Do the float
	tick LetFloatFromLet	`thenSmpl_`
	returnSmpl (floats, in_scope, rhs', arity)
    else			-- Don't do it
	getInScope		`thenSmpl` \ in_scope ->
	returnSmpl ([], in_scope, mkLetBinds floats rhs', arity)
\end{code}

---------------------------------------------------------
	Try eta expansion for RHSs

We need to pass in the substitution environment for the RHS, because
it might be different to the current one (see simplBeta, as called
from simplExpr for an applied lambda).  The binder needs to 

\begin{code}
simplRhs2 bndr bndr_se (Let bind body)
  = simplBind bind (simplRhs2 bndr bndr_se body)

simplRhs2 bndr bndr_se rhs 
  | null ids	-- Prevent eta expansion for both thunks 
		-- (would lose sharing) and variables (nothing gained).
		-- To see why we ignore it for thunks, consider
		--	let f = lookup env key in (f 1, f 2)
		-- We'd better not eta expand f just because it is 
		-- always applied!
		--
		-- Also if there isn't a lambda at the top we use
		-- simplExprB so that we can do (more) let-floating
  = simplExprB rhs Stop		`thenSmpl` \ (binds, (in_scope, rhs')) ->
    returnSmpl (binds, (in_scope, rhs', unknownArity))

  | otherwise	-- Consider eta expansion
  = getSwitchChecker		`thenSmpl` \ sw_chkr ->
    getInScope			`thenSmpl` \ in_scope ->
    simplBinders tyvars		$ \ tyvars' ->
    simplBinders ids		$ \ ids' ->

    if switchIsOn sw_chkr SimplDoLambdaEtaExpansion
    && not (null extra_arg_tys)
    then
	tick EtaExpansion			`thenSmpl_`
	setSubstEnv bndr_se (mapSmpl simplType extra_arg_tys)
						`thenSmpl` \ extra_arg_tys' ->
	newIds extra_arg_tys'			$ \ extra_bndrs' ->
	simplExpr body (mk_cont extra_bndrs') 	`thenSmpl` \ body' ->
	let
	    expanded_rhs = mkLams tyvars'
			 $ mkLams ids' 
 			 $ mkLams extra_bndrs' body'
	    expanded_arity = atLeastArity (no_of_ids + no_of_extras)	
	in
	returnSmpl ([], (in_scope, expanded_rhs, expanded_arity))

    else
	simplExpr body Stop 			`thenSmpl` \ body' ->
	let
	    unexpanded_rhs = mkLams tyvars'
			   $ mkLams ids' body'
	    unexpanded_arity = atLeastArity no_of_ids
	in
	returnSmpl ([], (in_scope, unexpanded_rhs, unexpanded_arity))

  where
    (tyvars, ids, body) = collectTyAndValBinders rhs
    no_of_ids		= length ids

    potential_extra_arg_tys :: [InType]	-- NB: InType
    potential_extra_arg_tys  = case splitFunTys (applyTys (idType bndr) (mkTyVarTys tyvars)) of
				  (arg_tys, _) -> drop no_of_ids arg_tys

    extra_arg_tys :: [InType]
    extra_arg_tys  = take no_extras_wanted potential_extra_arg_tys
    no_of_extras   = length extra_arg_tys

    no_extras_wanted =  -- Use information about how many args the fn is applied to
			(arity - no_of_ids) 	`max`

			-- See if the body could obviously do with more args
			etaExpandCount body	`max`

			-- Finally, see if it's a state transformer, in which
			-- case we eta-expand on principle! This can waste work,
			-- but usually doesn't
			case potential_extra_arg_tys of
				[ty] | ty == realWorldStatePrimTy -> 1
				other				  -> 0

    arity = arityLowerBound (getIdArity bndr)

    mk_cont []     = Stop
    mk_cont (b:bs) = ApplyTo OkToDup (Var b) emptySubstEnv (mk_cont bs)
\end{code}


%************************************************************************
%*									*
\subsection{Binding}
%*									*
%************************************************************************

\begin{code}
simplBeta :: InId 			-- Binder
	  -> InExpr -> SubstEnv		-- Arg, with its subst-env
	  -> InExpr -> SimplCont 	-- Lambda body
	  -> SimplM OutExprStuff
#ifdef DEBUG
simplBeta bndr rhs rhs_se body cont
  | isTyVar bndr
  = pprPanic "simplBeta" ((ppr bndr <+> ppr rhs) $$ ppr cont)
#endif

simplBeta bndr rhs rhs_se body cont
  |  isUnLiftedType bndr_ty
  || (isStrict (getIdDemandInfo bndr) || is_dict bndr) && not (exprIsWHNF rhs)
  = tick Let2Case	`thenSmpl_`
    getSubstEnv 	`thenSmpl` \ body_se ->
    setSubstEnv rhs_se	$
    simplExprB rhs (Select NoDup bndr [(DEFAULT, [], body)] body_se cont)

  | preInlineUnconditionally bndr && not opt_NoPreInlining
  = tick PreInlineUnconditionally			`thenSmpl_`
    case rhs_se of 					{ (ty_subst, id_subst) ->
    extendIdSubst bndr (SubstMe rhs ty_subst id_subst)	$
    simplExprB body cont }

  | otherwise
  = getSubstEnv 		`thenSmpl` \ bndr_se ->
    setSubstEnv rhs_se (simplRhs bndr bndr_se rhs)
				`thenSmpl` \ (floats, in_scope, rhs', arity) ->
    setInScope in_scope				$
    completeBindNonRec (bndr `setIdArity` arity) rhs' (
	    simplExprB body cont		
    )						`thenSmpl` \ stuff ->
    returnSmpl (addBinds floats stuff)
  where
	-- Return true only for dictionary types where the dictionary
	-- has more than one component (else we risk poking on the component
	-- of a newtype dictionary)
    is_dict bndr = opt_DictsStrict && isDictTy bndr_ty && isDataType bndr_ty
    bndr_ty      = idType bndr
\end{code}


completeBindNonRec
	- deals only with Ids, not TyVars
	- take an already-simplified RHS
	- always produce let bindings

It does *not* attempt to do let-to-case.  Why?  Because they are used for

	- top-level bindings
		(when let-to-case is impossible) 

	- many situations where the "rhs" is known to be a WHNF
		(so let-to-case is inappropriate).

\begin{code}
completeBindNonRec :: InId 		-- Binder
	        -> OutExpr		-- Simplified RHS
	   	-> SimplM (OutStuff a)	-- Thing inside
	   	-> SimplM (OutStuff a)
completeBindNonRec bndr rhs thing_inside
  |  isDeadBinder bndr		-- This happens; for example, the case_bndr during case of
				-- known constructor:  case (a,b) of x { (p,q) -> ... }
				-- Here x isn't mentioned in the RHS, so we don't want to
				-- create the (dead) let-binding  let x = (a,b) in ...
  =  thing_inside

  |  postInlineUnconditionally bndr etad_rhs
  =  tick PostInlineUnconditionally	`thenSmpl_`
     extendIdSubst bndr (Done etad_rhs)	
     thing_inside

  |  otherwise			-- Note that we use etad_rhs here
				-- This gives maximum chance for a remaining binding
				-- to be zapped by the indirection zapper in OccurAnal
  =  simplBinder bndr				$ \ bndr' ->
     simplPrags bndr bndr' etad_rhs		`thenSmpl` \ bndr'' ->
     modifyInScope bndr''			$ 
     thing_inside				`thenSmpl` \ stuff ->
     returnSmpl (addBind (NonRec bndr' etad_rhs) stuff)
  where
     etad_rhs = etaCoreExpr rhs

-- (simplPrags old_bndr new_bndr new_rhs) does two things
--	(a) it attaches the new unfolding to new_bndr
--	(b) it grabs the SpecEnv from old_bndr, applies the current
--	    substitution to it, and attaches it to new_bndr
--  The assumption is that new_bndr, which is produced by simplBinder
--  has no unfolding or specenv.

simplPrags old_bndr new_bndr new_rhs
  | isEmptySpecEnv spec_env
  = returnSmpl (bndr_w_unfolding)

  | otherwise
  = getSimplBinderStuff `thenSmpl` \ (ty_subst, id_subst, in_scope, us) ->
    let
	spec_env' = substSpecEnv ty_subst in_scope (subst_val id_subst) spec_env
    in
    returnSmpl (bndr_w_unfolding `setIdSpecialisation` spec_env')
  where
    bndr_w_unfolding = new_bndr `setIdUnfolding` mkUnfolding new_rhs

    spec_env = getIdSpecialisation old_bndr
    subst_val id_subst ty_subst in_scope expr
	= substExpr ty_subst id_subst in_scope expr
\end{code}    

\begin{code}
preInlineUnconditionally :: InId -> Bool
	-- Examines a bndr to see if it is used just once in a 
	-- completely safe way, so that it is safe to discard the binding
	-- inline its RHS at the (unique) usage site, REGARDLESS of how
	-- big the RHS might be.  If this is the case we don't simplify
	-- the RHS first, but just inline it un-simplified.
	--
	-- This is much better than first simplifying a perhaps-huge RHS
	-- and then inlining and re-simplifying it.
	--
	-- NB: we don't even look at the RHS to see if it's trivial
	-- We might have
	--			x = y
	-- where x is used many times, but this is the unique occurrence
	-- of y.  We should NOT inline x at all its uses, because then
	-- we'd do the same for y -- aargh!  So we must base this
	-- pre-rhs-simplification decision solely on x's occurrences, not
	-- on its rhs.
preInlineUnconditionally bndr
  = case getInlinePragma bndr of
	ICanSafelyBeINLINEd InsideLam  _    -> False
	ICanSafelyBeINLINEd not_in_lam True -> True	-- Not inside a lambda,
							-- one occurrence ==> safe!
	other -> False


postInlineUnconditionally :: InId -> OutExpr -> Bool
	-- Examines a (bndr = rhs) binding, AFTER the rhs has been simplified
	-- It returns True if it's ok to discard the binding and inline the
	-- RHS at every use site.

	-- NOTE: This isn't our last opportunity to inline.
	-- We're at the binding site right now, and
	-- we'll get another opportunity when we get to the ocurrence(s)

postInlineUnconditionally bndr rhs
  | isExported bndr 
  = False
  | otherwise
  = case getInlinePragma bndr of
	IAmALoopBreaker			    	  -> False   
	IMustNotBeINLINEd 		    	  -> False
	IAmASpecPragmaId		    	  -> False	-- Don't discard SpecPrag Ids

	ICanSafelyBeINLINEd InsideLam one_branch  -> exprIsTrivial rhs
			-- Don't inline even WHNFs inside lambdas; this
			-- isn't the last chance; see NOTE above.

	ICanSafelyBeINLINEd not_in_lam one_branch -> one_branch || exprIsDupable rhs

	other				    	  -> exprIsTrivial rhs	-- Duplicating is *free*
		-- NB: Even IWantToBeINLINEd and IMustBeINLINEd are ignored here
		-- Why?  Because we don't even want to inline them into the
		-- RHS of constructor arguments. See NOTE above

inlineCase bndr scrut
  = case getInlinePragma bndr of
	-- Not expecting IAmALoopBreaker etc; this is a case binder!

	ICanSafelyBeINLINEd StrictOcc one_branch
		-> one_branch || exprIsDupable scrut
		-- This case is the entire reason we distinguish StrictOcc from LazyOcc
		-- We want eliminate the "case" only if we aren't going to
		-- build a thunk instead, and that's what StrictOcc finds
		-- For example:
		-- 	case (f x) of y { DEFAULT -> g y }
		-- Here we DO NOT WANT:
		--	g (f x)
		-- *even* if g is strict.  We want to avoid constructing the
		-- thunk for (f x)!  So y gets a LazyOcc.

	other	-> exprIsTrivial scrut			-- Duplication is free
		&& (  isUnLiftedType (idType bndr) 
		   || scrut_is_evald_var		-- So dropping the case won't change termination
		   || isStrict (getIdDemandInfo bndr))	-- It's going to get evaluated later, so again
							-- termination doesn't change
  where
	-- Check whether or not scrut is known to be evaluted
	-- It's not going to be a visible value (else the previous
	-- blob would apply) so we just check the variable case
    scrut_is_evald_var = case scrut of
				Var v -> isEvaldUnfolding (getIdUnfolding v)
				other -> False
\end{code}

okToInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.

\begin{code}
okToInline :: SwitchChecker
	   -> InScopeEnv
	   -> Id		-- The Id
	   -> FormSummary	-- The thing is WHNF or bottom; 
	   -> UnfoldingGuidance
	   -> SimplCont
	   -> Bool		-- True <=> inline it

-- A non-WHNF can be inlined if it doesn't occur inside a lambda,
-- and occurs exactly once or 
--     occurs once in each branch of a case and is small
--
-- If the thing is in WHNF, there's no danger of duplicating work, 
-- so we can inline if it occurs once, or is small

okToInline sw_chkr in_scope id form guidance cont
  | switchIsOn sw_chkr EssentialUnfoldingsOnly
  =
#ifdef DEBUG
    if opt_D_dump_inlinings then
	pprTrace "Considering inlining"
		 (ppr id <+> vcat [text "essential inlinings only",
				   text "inline prag:" <+> ppr inline_prag,
				   text "ANSWER =" <+> if result then text "YES" else text "NO"])
		 result
    else
#endif
    result
  where
    inline_prag  = getInlinePragma id
    result = idMustBeINLINEd id
		-- If "essential_unfoldings_only" is true we do no inlinings at all,
		-- EXCEPT for things that absolutely have to be done
		-- (see comments with idMustBeINLINEd)


okToInline sw_chkr in_scope id form guidance cont
	-- Essential unfoldings only not on
  =
#ifdef DEBUG
    if opt_D_dump_inlinings then
	pprTrace "Considering inlining"
		 (ppr id <+> vcat [text "inline prag:" <+> ppr inline_prag,
				   text "whnf" <+> ppr whnf,
				   text "small enough" <+> ppr small_enough,
				   text "some benefit" <+> ppr some_benefit,
			  	   text "arg evals" <+> ppr arg_evals,
				   text "result scrut" <+> ppr result_scrut,
				   text "ANSWER =" <+> if result then text "YES" else text "NO"])
		  result
     else
#endif
    result
  where
    result = case inline_prag of
		IAmDead		  -> pprTrace "okToInline: dead" (ppr id) False

		IAmASpecPragmaId  -> False
		IMustNotBeINLINEd -> False
		IAmALoopBreaker   -> False
		IMustBeINLINEd    -> True
		IWantToBeINLINEd  -> True
	
		ICanSafelyBeINLINEd inside_lam one_branch
			-> (small_enough || one_branch) && some_benefit &&
			   (whnf || not_inside_lam)
		    
			where
			   not_inside_lam = case inside_lam of {InsideLam -> False; other -> True}

		other   -> whnf && small_enough && some_benefit
			-- We could consider using exprIsCheap here,
			-- as in postInlineUnconditionally, but unlike the latter we wouldn't
			-- necessarily eliminate a thunk; and the "form" doesn't tell
			-- us that.

    inline_prag  = getInlinePragma id
    whnf         = whnfOrBottom form
    small_enough = smallEnoughToInline id arg_evals result_scrut guidance
    (arg_evals, result_scrut) = get_evals cont

	-- some_benefit checks that *something* interesting happens to
	-- the variable after it's inlined.
    some_benefit = contIsInteresting cont

	-- Finding out whether the args are evaluated.  This isn't completely easy
	-- because the args are not yet simplified, so we have to peek into them.
    get_evals (ApplyTo _ arg (te,ve) cont) 
      | isValArg arg = case get_evals cont of 
			  (args, res) -> (get_arg_eval arg ve : args, res)
      | otherwise    = get_evals cont

    get_evals (Select _ _ _ _ _) = ([], True)
    get_evals other		 = ([], False)

    get_arg_eval (Con con _) ve = isWHNFCon con
    get_arg_eval (Var v)     ve = case lookupVarEnv ve v of
				    Just (SubstMe e' _ ve') -> get_arg_eval e' ve'
				    Just (Done (Con con _)) -> isWHNFCon con
				    Just (Done (Var v'))    -> get_var_eval v'
				    Just (Done other)	    -> False
				    Nothing		    -> get_var_eval v
    get_arg_eval other	     ve = False

    get_var_eval v = case lookupVarSet in_scope v of
			Just v' -> isEvaldUnfolding (getIdUnfolding v')
			Nothing -> isEvaldUnfolding (getIdUnfolding v)


contIsInteresting :: SimplCont -> Bool
contIsInteresting Stop			      = False
contIsInteresting (ArgOf _ _ _)		      = False
contIsInteresting (ApplyTo _ (Type _) _ cont) = contIsInteresting cont
contIsInteresting (CoerceIt _ _ _ cont)	      = contIsInteresting cont

-- Even a case with only a default case is a bit interesting;
-- 	we may be able to eliminate it after inlining.
-- contIsInteresting (Select _ _ [(DEFAULT,_,_)] _ _) = False

contIsInteresting _ 			      = True
\end{code}

Comment about some_benefit above
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.  

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

	let x = _coerce_ (T Int) Int (I# 3) in
	case _coerce_ Int (T Int) x of
		I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

	case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
-------------------------------------------------------------------
rebuild :: OutExpr -> SimplCont -> SimplM OutExprStuff

rebuild expr cont
  = tick LeavesExamined					`thenSmpl_`
    do_rebuild expr cont

rebuild_done expr
  = getInScope			`thenSmpl` \ in_scope ->		
    returnSmpl ([], (in_scope, expr))

---------------------------------------------------------
--	Stop continuation

do_rebuild expr Stop = rebuild_done expr


---------------------------------------------------------
--	ArgOf continuation

do_rebuild expr (ArgOf _ cont_fn _) = cont_fn expr

---------------------------------------------------------
--	ApplyTo continuation

do_rebuild expr cont@(ApplyTo _ arg se cont')
  = case expr of
	Var v -> case getIdStrictness v of
		    NoStrictnessInfo		        -> non_strict_case
		    StrictnessInfo demands result_bot _ -> ASSERT( not (null demands) || result_bot )
								-- If this happened we'd get an infinite loop
							   rebuild_strict demands result_bot expr (idType v) cont
	other -> non_strict_case
  where
    non_strict_case = setSubstEnv se (simplArg arg)	`thenSmpl` \ arg' ->
		      do_rebuild (App expr arg') cont'


---------------------------------------------------------
--	Coerce continuation

do_rebuild expr (CoerceIt _ to_ty se cont)
  = setSubstEnv se	$
    simplType to_ty	`thenSmpl` \ to_ty' ->
    do_rebuild (mk_coerce to_ty' expr) cont
  where
    mk_coerce to_ty' (Note (Coerce _ from_ty) expr) = Note (Coerce to_ty' from_ty) expr
    mk_coerce to_ty' expr			    = Note (Coerce to_ty' (coreExprType expr)) expr


---------------------------------------------------------
-- 	Case of known constructor or literal

do_rebuild expr@(Con con args) cont@(Select _ _ _ _ _)
  | conOkForAlt con	-- Knocks out PrimOps and NoRepLits
  = knownCon expr con args cont


---------------------------------------------------------

--	Case of other value (e.g. a partial application or lambda)
--	Turn it back into a let

do_rebuild expr (Select _ bndr ((DEFAULT, bs, rhs):alts) se cont)
  | case mkFormSummary expr of { ValueForm -> True; other -> False }
  = ASSERT( null bs && null alts )
    tick Case2Let		`thenSmpl_`
    setSubstEnv se 		(
    completeBindNonRec bndr expr 	$
    simplExprB rhs cont
    )


---------------------------------------------------------
-- 	The other Select cases

do_rebuild scrut (Select _ bndr alts se cont)
  = getSwitchChecker					`thenSmpl` \ chkr ->

    if all (cheapEqExpr rhs1) other_rhss
       && inlineCase bndr scrut
       && all binders_unused alts
       && switchIsOn chkr SimplDoCaseElim
    then
  	-- Get rid of the case altogether
	-- See the extensive notes on case-elimination below
	-- Remember to bind the binder though!
	    tick  CaseElim		`thenSmpl_`
	    setSubstEnv se			(
	    extendIdSubst bndr (Done scrut)	$
	    simplExprB rhs1 cont
	    )

    else
	rebuild_case chkr scrut bndr alts se cont
  where
    (rhs1:other_rhss)		 = [rhs | (_,_,rhs) <- alts]
    binders_unused (_, bndrs, _) = all isDeadBinder bndrs
\end{code}

Case elimination [see the code above]
~~~~~~~~~~~~~~~~
Start with a simple situation:

	case x# of	===>   e[x#/y#]
	  y# -> e

(when x#, y# are of primitive type, of course).  We can't (in general)
do this for algebraic cases, because we might turn bottom into
non-bottom!

Actually, we generalise this idea to look for a case where we're
scrutinising a variable, and we know that only the default case can
match.  For example:
\begin{verbatim}
	case x of
	  0#    -> ...
	  other -> ...(case x of
			 0#    -> ...
			 other -> ...) ...
\end{code}
Here the inner case can be eliminated.  This really only shows up in
eliminating error-checking code.

We also make sure that we deal with this very common case:

 	case e of 
	  x -> ...x...

Here we are using the case as a strict let; if x is used only once
then we want to inline it.  We have to be careful that this doesn't 
make the program terminate when it would have diverged before, so we
check that 
	- x is used strictly, or
	- e is already evaluated (it may so if e is a variable)

Lastly, we generalise the transformation to handle this:

	case e of	===> r
	   True  -> r
	   False -> r

We only do this for very cheaply compared r's (constructors, literals
and variables).  If pedantic bottoms is on, we only do it when the
scrutinee is a PrimOp which can't fail.

We do it *here*, looking at un-simplified alternatives, because we
have to check that r doesn't mention the variables bound by the
pattern in each alternative, so the binder-info is rather useful.

So the case-elimination algorithm is:

	1. Eliminate alternatives which can't match

	2. Check whether all the remaining alternatives
		(a) do not mention in their rhs any of the variables bound in their pattern
	   and  (b) have equal rhss

	3. Check we can safely ditch the case:
		   * PedanticBottoms is off,
		or * the scrutinee is an already-evaluated variable
		or * the scrutinee is a primop which is ok for speculation
			-- ie we want to preserve divide-by-zero errors, and
			-- calls to error itself!

		or * [Prim cases] the scrutinee is a primitive variable

		or * [Alg cases] the scrutinee is a variable and
		     either * the rhs is the same variable
			(eg case x of C a b -> x  ===>   x)
		     or     * there is only one alternative, the default alternative,
				and the binder is used strictly in its scope.
				[NB this is helped by the "use default binder where
				 possible" transformation; see below.]


If so, then we can replace the case with one of the rhss.


\begin{code}
---------------------------------------------------------
--	Rebuiling a function with strictness info

rebuild_strict :: [Demand] -> Bool 	-- Stricness info
	       -> OutExpr -> OutType	-- Function and type
	       -> SimplCont		-- Continuation
	       -> SimplM OutExprStuff

rebuild_strict [] True  fun fun_ty cont = rebuild_bot fun fun_ty cont
rebuild_strict [] False fun fun_ty cont = do_rebuild fun cont

rebuild_strict ds result_bot fun fun_ty (ApplyTo _ (Type ty_arg) se cont)
				-- Type arg; don't consume a demand
	= setSubstEnv se (simplType ty_arg)	`thenSmpl` \ ty_arg' ->
	  rebuild_strict ds result_bot (App fun (Type ty_arg')) 
			 (applyTy fun_ty ty_arg') cont

rebuild_strict (d:ds) result_bot fun fun_ty (ApplyTo _ val_arg se cont)
	| isStrict d || isUnLiftedType arg_ty	-- Strict value argument
	= getInScope 				`thenSmpl` \ in_scope ->
	  let
		cont_ty = contResultType in_scope res_ty cont
	  in
 	  setSubstEnv se (simplExprB val_arg (ArgOf NoDup cont_fn cont_ty))

	| otherwise  				-- Lazy value argument
	= setSubstEnv se (simplArg val_arg)	`thenSmpl` \ val_arg' ->
	  cont_fn val_arg'

	where
	  Just (arg_ty, res_ty) = splitFunTy_maybe fun_ty
	  cont_fn arg'          = rebuild_strict ds result_bot 
						 (App fun arg') res_ty
						 cont

rebuild_strict ds result_bot fun fun_ty cont = do_rebuild fun cont

---------------------------------------------------------
-- 	Dealing with
--	* case (error "hello") of { ... }
--	* (error "Hello") arg
--	etc

rebuild_bot expr expr_ty Stop				-- No coerce needed
  = rebuild_done expr

rebuild_bot expr expr_ty (CoerceIt _ to_ty se Stop)	-- Don't "tick" on this,
							-- else simplifier never stops
  = setSubstEnv se	$
    simplType to_ty	`thenSmpl` \ to_ty' ->
    rebuild_done (mkNote (Coerce to_ty' expr_ty) expr)

rebuild_bot expr expr_ty cont
  = tick CaseOfError		`thenSmpl_`
    getInScope			`thenSmpl` \ in_scope ->
    let
	result_ty = contResultType in_scope expr_ty cont
    in
    rebuild_done (mkNote (Coerce result_ty expr_ty) expr)
\end{code}

Blob of helper functions for the "case-of-something-else" situation.

\begin{code}
---------------------------------------------------------
-- 	Case of something else

rebuild_case sw_chkr scrut case_bndr alts se cont
  = 	-- Prepare case alternatives
    prepareCaseAlts (splitTyConApp_maybe (idType case_bndr))
		    scrut_cons alts		`thenSmpl` \ better_alts ->
    
	-- Set the new subst-env in place (before dealing with the case binder)
    setSubstEnv se				$

	-- Deal with the case binder, and prepare the continuation;
	-- The new subst_env is in place
    simplBinder case_bndr 			$ \ case_bndr' ->
    prepareCaseCont better_alts cont		$ \ cont' ->
	

	-- Deal with variable scrutinee
    substForVarScrut scrut case_bndr'		$ \ zap_occ_info ->
    let
	case_bndr'' = zap_occ_info case_bndr'
    in

	-- Deal with the case alternaatives
    simplAlts zap_occ_info scrut_cons 
	      case_bndr'' better_alts cont'	`thenSmpl` \ alts' ->

    mkCase sw_chkr scrut case_bndr'' alts'	`thenSmpl` \ case_expr ->
    rebuild_done case_expr	
  where
	-- scrut_cons tells what constructors the scrutinee can't possibly match
    scrut_cons = case scrut of
		   Var v -> case getIdUnfolding v of
				OtherCon cons -> cons
				other	      -> []
		   other -> []


knownCon expr con args (Select _ bndr alts se cont)
  = tick KnownBranch		`thenSmpl_`
    setSubstEnv se 		(
    case findAlt con alts of
	(DEFAULT, bs, rhs)     -> ASSERT( null bs )
				  completeBindNonRec bndr expr $
			          simplExprB rhs cont

	(Literal lit, bs, rhs) -> ASSERT( null bs )
				  extendIdSubst bndr (Done expr)	$
					-- Unconditionally substitute, because expr must
					-- be a variable or a literal.  It can't be a
					-- NoRep literal because they don't occur in
					-- case patterns.
				  simplExprB rhs cont

	(DataCon dc, bs, rhs)  -> completeBindNonRec bndr expr 		$
				  extend bs real_args			$
			          simplExprB rhs cont
			       where
				  real_args = drop (dataConNumInstArgs dc) args
    )
  where
    extend []     []	     thing_inside = thing_inside
    extend (b:bs) (arg:args) thing_inside = extendIdSubst b (Done arg)	$
					    extend bs args thing_inside
\end{code}

\begin{code}
prepareCaseCont :: [InAlt] -> SimplCont
		-> (SimplCont -> SimplM (OutStuff a))
		-> SimplM (OutStuff a)
	-- Polymorphic recursion here!

prepareCaseCont [alt] cont thing_inside = thing_inside cont
prepareCaseCont alts  cont thing_inside = mkDupableCont (coreAltsType alts) cont thing_inside
\end{code}

substForVarScrut checks whether the scrutinee is a variable, v.
If so, try to eliminate uses of v in the RHSs in favour of case_bndr; 
that way, there's a chance that v will now only be used once, and hence inlined.

If we do this, then we have to nuke any occurrence info (eg IAmDead)
in the case binder, because the case-binder now effectively occurs
whenever v does.  AND we have to do the same for the pattern-bound
variables!  Example:

	(case x of { (a,b) -> a }) (case x of { (p,q) -> q })

Here, b and p are dead.  But when we move the argment inside the first
case RHS, and eliminate the second case, we get

	case x or { (a,b) -> a b

Urk! b is alive!  Reason: the scrutinee was a variable, and case elimination
happened.  Hence the zap_occ_info function returned by substForVarScrut

\begin{code}
substForVarScrut (Var v) case_bndr' thing_inside
  | isLocallyDefined v		-- No point for imported things
  = modifyInScope (v `setIdUnfolding` mkUnfolding (Var case_bndr')
		     `setInlinePragma` IMustBeINLINEd)			$
	-- We could extend the substitution instead, but it would be
	-- a hack because then the substitution wouldn't be idempotent
	-- any more.
    thing_inside (\ bndr ->  bndr `setInlinePragma` NoInlinePragInfo)
	    
substForVarScrut other_scrut case_bndr' thing_inside
  = thing_inside (\ bndr -> bndr)	-- NoOp on bndr
\end{code}

prepareCaseAlts does two things:

1.  Remove impossible alternatives

2.  If the DEFAULT alternative can match only one possible constructor,
    then make that constructor explicit.
    e.g.
	case e of x { DEFAULT -> rhs }
     ===>
	case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.

\begin{code}
prepareCaseAlts (Just (tycon, inst_tys)) scrut_cons alts
  | isDataTyCon tycon
  = case (findDefault filtered_alts, missing_cons) of

	((alts_no_deflt, Just rhs), [data_con]) 	-- Just one missing constructor!
		-> tick FillInCaseDefault	`thenSmpl_`
		   let
			(_,_,ex_tyvars,_,_,_) = dataConSig data_con
		   in
		   getUniquesSmpl (length ex_tyvars)				`thenSmpl` \ tv_uniqs ->
		   let
			ex_tyvars' = zipWithEqual "simpl_alt" mk tv_uniqs ex_tyvars
			mk uniq tv = mkSysTyVar uniq (tyVarKind tv)
		   in
    		   newIds (dataConArgTys
				data_con
				(inst_tys ++ mkTyVarTys ex_tyvars'))		$ \ bndrs ->
		   returnSmpl ((DataCon data_con, ex_tyvars' ++ bndrs, rhs) : alts_no_deflt)

	other -> returnSmpl filtered_alts
  where
	-- Filter out alternatives that can't possibly match
    filtered_alts = case scrut_cons of
			[]    -> alts
			other -> [alt | alt@(con,_,_) <- alts, not (con `elem` scrut_cons)]

    missing_cons = [data_con | data_con <- tyConDataCons tycon, 
			       not (data_con `elem` handled_data_cons)]
    handled_data_cons = [data_con | DataCon data_con         <- scrut_cons] ++
			[data_con | (DataCon data_con, _, _) <- filtered_alts]

-- The default case
prepareCaseAlts _ scrut_cons alts
  = returnSmpl alts			-- Functions


----------------------
simplAlts zap_occ_info scrut_cons case_bndr'' alts cont'
  = mapSmpl simpl_alt alts
  where
    inst_tys' = case splitTyConApp_maybe (idType case_bndr'') of
			Just (tycon, inst_tys) -> inst_tys

	-- handled_cons is all the constructors that are dealt
	-- with, either by being impossible, or by there being an alternative
    handled_cons = scrut_cons ++ [con | (con,_,_) <- alts, con /= DEFAULT]

    simpl_alt (DEFAULT, _, rhs)
	= modifyInScope (case_bndr'' `setIdUnfolding` OtherCon handled_cons)	$
	  simplExpr rhs cont'							`thenSmpl` \ rhs' ->
	  returnSmpl (DEFAULT, [], rhs')

    simpl_alt (con, vs, rhs)
	= 	-- Deal with the case-bound variables
		-- Mark the ones that are in ! positions in the data constructor
		-- as certainly-evaluated
	  simplBinders (add_evals con vs)	$ \ vs' ->

		-- Bind the case-binder to (Con args)
		-- In the default case we record the constructors it *can't* be.
		-- We take advantage of any OtherCon info in the case scrutinee
	  let
		con_app = Con con (map Type inst_tys' ++ map varToCoreExpr vs')
	  in
	  modifyInScope (case_bndr'' `setIdUnfolding` mkUnfolding con_app)	$
	  simplExpr rhs cont'		`thenSmpl` \ rhs' ->
	  returnSmpl (con, vs', rhs')


	-- add_evals records the evaluated-ness of the bound variables of
	-- a case pattern.  This is *important*.  Consider
	--	data T = T !Int !Int
	--
	--	case x of { T a b -> T (a+1) b }
	--
	-- We really must record that b is already evaluated so that we don't
	-- go and re-evaluated it when constructing the result.

    add_evals (DataCon dc) vs = stretchZipEqual add_eval vs (dataConStrictMarks dc)
    add_evals other_con    vs = vs

    add_eval v m | isTyVar v = Nothing
		 | otherwise = case m of
				  MarkedStrict    -> Just (zap_occ_info v `setIdUnfolding` OtherCon [])
				  NotMarkedStrict -> Just (zap_occ_info v)
\end{code}




%************************************************************************
%*									*
\subsection{Duplicating continuations}
%*									*
%************************************************************************

\begin{code}
mkDupableCont :: InType		-- Type of the thing to be given to the continuation
	      -> SimplCont 
	      -> (SimplCont -> SimplM (OutStuff a))
	      -> SimplM (OutStuff a)
mkDupableCont ty cont thing_inside 
  | contIsDupable cont
  = thing_inside cont

mkDupableCont _ (CoerceIt _ ty se cont) thing_inside
  = mkDupableCont ty cont		$ \ cont' ->
    thing_inside (CoerceIt OkToDup ty se cont')

mkDupableCont join_arg_ty (ArgOf _ cont_fn res_ty) thing_inside
  = 	-- Build the RHS of the join point
    simplType join_arg_ty				`thenSmpl` \ join_arg_ty' ->
    newId join_arg_ty'					( \ arg_id ->
    	getSwitchChecker				`thenSmpl` \ chkr ->
	cont_fn (Var arg_id)				`thenSmpl` \ (binds, (_, rhs)) ->
	returnSmpl (Lam arg_id (mkLetBinds binds rhs))
    )							`thenSmpl` \ join_rhs ->
   
	-- Build the join Id and continuation
    newId (coreExprType join_rhs)		$ \ join_id ->
    let
	new_cont = ArgOf OkToDup
			 (\arg' -> rebuild_done (App (Var join_id) arg'))
			 res_ty
    in
	
	-- Do the thing inside
    thing_inside new_cont		`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec join_id join_rhs) res)

mkDupableCont ty (ApplyTo _ arg se cont) thing_inside
  = mkDupableCont (funResultTy ty) cont 		$ \ cont' ->
    setSubstEnv se (simplArg arg)			`thenSmpl` \ arg' ->
    if exprIsDupable arg' then
	thing_inside (ApplyTo OkToDup arg' emptySubstEnv cont')
    else
    newId (coreExprType arg')						$ \ bndr ->
    thing_inside (ApplyTo OkToDup (Var bndr) emptySubstEnv cont')	`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec bndr arg') res)

mkDupableCont ty (Select _ case_bndr alts se cont) thing_inside
  = tick CaseOfCase						`thenSmpl_` (
    setSubstEnv se	(
	simplBinder case_bndr					$ \ case_bndr' ->
	prepareCaseCont alts cont				$ \ cont' ->
	mapAndUnzipSmpl (mkDupableAlt case_bndr' cont') alts	`thenSmpl` \ (alt_binds_s, alts') ->
	returnSmpl (concat alt_binds_s, (case_bndr', alts'))
    )					`thenSmpl` \ (alt_binds, (case_bndr', alts')) ->

    extendInScopes [b | NonRec b _ <- alt_binds]			$
    thing_inside (Select OkToDup case_bndr' alts' emptySubstEnv Stop)	`thenSmpl` \ res ->
    returnSmpl (addBinds alt_binds res)
    )

mkDupableAlt :: OutId -> SimplCont -> InAlt -> SimplM (OutStuff CoreAlt)
mkDupableAlt case_bndr' cont alt@(con, bndrs, rhs)
  = simplBinders bndrs					$ \ bndrs' ->
    simplExpr rhs cont					`thenSmpl` \ rhs' ->
    if exprIsDupable rhs' then
	-- It's small, so don't bother to let-bind it
	returnSmpl ([], (con, bndrs', rhs'))
    else
	-- It's big, so let-bind it
    let
	rhs_ty' = coreExprType rhs'
        used_bndrs' = filter (not . isDeadBinder) (case_bndr' : bndrs')
    in
    ( if null used_bndrs' && isUnLiftedType rhs_ty'
	then newId realWorldStatePrimTy  $ \ rw_id ->
	     returnSmpl ([rw_id], [varToCoreExpr realWorldPrimId])
	else 
	     returnSmpl (used_bndrs', map varToCoreExpr used_bndrs')
    )
	`thenSmpl` \ (final_bndrs', final_args) ->

	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::State# RealWorld#.  The value passed to this function
	-- is realworld#, which generates (almost) no code.

	-- There's a slight infelicity here: we pass the overall 
	-- case_bndr to all the join points if it's used in *any* RHS,
	-- because we don't know its usage in each RHS separately

    newId (foldr (mkFunTy . idType) rhs_ty' final_bndrs')	$ \ join_bndr ->
    returnSmpl ([NonRec join_bndr (mkLams final_bndrs' rhs')],
		(con, bndrs', mkApps (Var join_bndr) final_args))
\end{code}
