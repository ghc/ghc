
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( intSwitchSet,
			  opt_SccProfilingOn, opt_PprStyle_Debug, opt_SimplDoEtaReduction,
			  opt_SimplNoPreInlining, opt_DictsStrict, opt_SimplPedanticBottoms,
			  SimplifierSwitch(..)
			)
import SimplMonad
import SimplUtils	( mkCase, transformRhs, findAlt,
			  simplBinder, simplBinders, simplIds, findDefault, mkCoerce
			)
import Var		( TyVar, mkSysTyVar, tyVarKind, maybeModifyIdInfo )
import VarEnv
import VarSet
import Id		( Id, idType, idInfo, idUnique,
			  getIdUnfolding, setIdUnfolding, isExportedId, 
			  getIdSpecialisation, setIdSpecialisation,
			  getIdDemandInfo, setIdDemandInfo,
			  getIdArity, setIdArity, 
			  getIdStrictness, 
			  setInlinePragma, getInlinePragma, idMustBeINLINEd,
			  setOneShotLambda
			)
import IdInfo		( InlinePragInfo(..), OccInfo(..), StrictnessInfo(..), 
		 	  ArityInfo(..), atLeastArity, arityLowerBound, unknownArity,
			  specInfo, inlinePragInfo, zapLamIdInfo
			)
import Demand		( Demand, isStrict, wwLazy )
import Const		( isWHNFCon, conOkForAlt )
import ConFold		( tryPrimOp )
import PrimOp		( PrimOp, primOpStrictness, primOpType )
import DataCon		( DataCon, dataConNumInstArgs, dataConRepStrictness, dataConSig, dataConArgTys )
import Const		( Con(..) )
import Name		( isLocallyDefined )
import CoreSyn
import CoreFVs		( exprFreeVars )
import CoreUnfold	( Unfolding, mkOtherCon, mkUnfolding, otherCons,
			  callSiteInline, blackListed
			)
import CoreUtils	( cheapEqExpr, exprIsDupable, exprIsCheap, exprIsTrivial,
			  coreExprType, coreAltsType, exprArity, exprIsValue,
			  exprOkForSpeculation
			)
import Rules		( lookupRule )
import CostCentre	( isSubsumedCCS, currentCCS, isEmptyCC )
import Type		( Type, mkTyVarTy, mkTyVarTys, isUnLiftedType, 
			  mkFunTy, splitFunTys, splitTyConApp_maybe, splitFunTy_maybe,
			  funResultTy, isDictTy, isDataType, applyTy, applyTys, mkFunTys
			)
import Subst		( Subst, mkSubst, emptySubst, substExpr, substTy, 
			  substEnv, lookupInScope, lookupSubst, substRules
			)
import TyCon		( isDataTyCon, tyConDataCons, tyConClass_maybe, tyConArity, isDataTyCon )
import TysPrim		( realWorldStatePrimTy )
import PrelInfo		( realWorldPrimId )
import BasicTypes	( TopLevelFlag(..), isTopLevel )
import Maybes		( maybeToBool )
import Util		( zipWithEqual, stretchZipEqual, lengthExceeds )
import PprCore
import Outputable
\end{code}


The guts of the simplifier is in this module, but the driver
loop for the simplifier is in SimplCore.lhs.


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
simplTopBinds :: [InBind] -> SimplM [OutBind]

simplTopBinds binds
  = 	-- Put all the top-level binders into scope at the start
	-- so that if a transformation rule has unexpectedly brought
	-- anything into scope, then we don't get a complaint about that.
	-- It's rather as if the top-level binders were imported.
    extendInScopes top_binders	$
    simpl_binds binds		`thenSmpl` \ (binds', _) ->
    freeTick SimplifierDone	`thenSmpl_`
    returnSmpl binds'
  where
    top_binders	= bindersOfBinds binds

    simpl_binds []			  = returnSmpl ([], panic "simplTopBinds corner")
    simpl_binds (NonRec bndr rhs : binds) = simplLazyBind TopLevel bndr  bndr rhs	 (simpl_binds binds)
    simpl_binds (Rec pairs       : binds) = simplRecBind  TopLevel pairs (map fst pairs) (simpl_binds binds)


simplRecBind :: TopLevelFlag -> [(InId, InExpr)] -> [OutId]
	     -> SimplM (OutStuff a) -> SimplM (OutStuff a)
simplRecBind top_lvl pairs bndrs' thing_inside
  = go pairs bndrs'		`thenSmpl` \ (binds', stuff) ->
    returnSmpl (addBind (Rec (flattenBinds binds')) stuff)
  where
    go [] _ = thing_inside 	`thenSmpl` \ stuff ->
	      returnSmpl ([], stuff)
	
    go ((bndr, rhs) : pairs) (bndr' : bndrs')
	= simplLazyBind top_lvl bndr bndr' rhs (go pairs bndrs')
		-- Don't float unboxed bindings out,
		-- because we can't "rec" them
\end{code}


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
simplExpr :: CoreExpr -> SimplM CoreExpr
simplExpr expr = getSubst	`thenSmpl` \ subst ->
		 simplExprC expr (Stop (substTy subst (coreExprType expr)))
	-- The type in the Stop continuation is usually not used
	-- It's only needed when discarding continuations after finding
	-- a function that returns bottom

simplExprC :: CoreExpr -> SimplCont -> SimplM CoreExpr
	-- Simplify an expression, given a continuation

simplExprC expr cont = simplExprF expr cont	`thenSmpl` \ (floats, (_, body)) ->
  		       returnSmpl (mkLets floats body)

simplExprF :: InExpr -> SimplCont -> SimplM OutExprStuff
	-- Simplify an expression, returning floated binds

simplExprF (Var v) cont
  = simplVar v cont

simplExprF expr@(Con (PrimOp op) args) cont
  = getSubstEnv				`thenSmpl` \ se ->
    prepareArgs (ppr op)
		(primOpType op)
		(primOpStrictness op)
		(pushArgs se args cont)	$ \ args1 cont1 ->

    let
	-- Boring... we may have too many arguments now, so we push them back
	n_args = length args
	args2 = ASSERT( length args1 >= n_args )
		 take n_args args1
	cont2 = pushArgs emptySubstEnv (drop n_args args1) cont1
    in				
     	-- 	Try the prim op simplification
	-- It's really worth trying simplExpr again if it succeeds,
	-- because you can find
	--	case (eqChar# x 'a') of ...
	-- ==>  
	-- 	case (case x of 'a' -> True; other -> False) of ...
     case tryPrimOp op args2 of
	  Just e' -> zapSubstEnv (simplExprF e' cont2)
	  Nothing -> rebuild (Con (PrimOp op) args2) cont2

simplExprF (Con con@(DataCon _) args) cont
  = freeTick LeafVisit			`thenSmpl_`
    simplConArgs args		( \ args' ->
    rebuild (Con con args') cont)

simplExprF expr@(Con con@(Literal _) args) cont
  = ASSERT( null args )
    freeTick LeafVisit			`thenSmpl_`
    rebuild expr cont

simplExprF (App fun arg) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprF fun (ApplyTo NoDup arg se cont)

simplExprF (Case scrut bndr alts) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprF scrut (Select NoDup bndr alts se cont)


simplExprF (Let (Rec pairs) body) cont
  = simplIds (map fst pairs) 		$ \ bndrs' -> 
	-- NB: bndrs' don't have unfoldings or spec-envs
	-- We add them as we go down, using simplPrags

    simplRecBind NotTopLevel pairs bndrs' (simplExprF body cont)

simplExprF expr@(Lam _ _) cont = simplLam expr cont

simplExprF (Type ty) cont
  = ASSERT( case cont of { Stop _ -> True; ArgOf _ _ _ -> True; other -> False } )
    simplType ty	`thenSmpl` \ ty' ->
    rebuild (Type ty') cont

simplExprF (Note (Coerce to from) e) cont
  | to == from = simplExprF e cont
  | otherwise  = getSubst		`thenSmpl` \ subst ->
    		 simplExprF e (CoerceIt (substTy subst to) cont)

-- hack: we only distinguish subsumed cost centre stacks for the purposes of
-- inlining.  All other CCCSs are mapped to currentCCS.
simplExprF (Note (SCC cc) e) cont
  = setEnclosingCC currentCCS $
    simplExpr e 	`thenSmpl` \ e ->
    rebuild (mkNote (SCC cc) e) cont

simplExprF (Note InlineCall e) cont
  = simplExprF e (InlinePlease cont)

-- Comments about the InlineMe case 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Don't inline in the RHS of something that has an
-- inline pragma.  But be careful that the InScopeEnv that
-- we return does still have inlinings on!
-- 
-- It really is important to switch off inlinings.  This function
-- may be inlinined in other modules, so we don't want to remove
-- (by inlining) calls to functions that have specialisations, or
-- that may have transformation rules in an importing scope.
-- E.g. 	{-# INLINE f #-}
-- 		f x = ...g...
-- and suppose that g is strict *and* has specialisations.
-- If we inline g's wrapper, we deny f the chance of getting
-- the specialised version of g when f is inlined at some call site
-- (perhaps in some other module).

simplExprF (Note InlineMe e) cont
  = case cont of
	Stop _ -> 	-- Totally boring continuation
			-- Don't inline inside an INLINE expression
		  switchOffInlining (simplExpr e)	`thenSmpl` \ e' ->
		  rebuild (mkNote InlineMe e') cont

	other  -> 	-- Dissolve the InlineMe note if there's
			-- an interesting context of any kind to combine with
			-- (even a type application -- anything except Stop)
		  simplExprF e cont	

-- A non-recursive let is dealt with by simplBeta
simplExprF (Let (NonRec bndr rhs) body) cont
  = getSubstEnv			`thenSmpl` \ se ->
    simplBeta bndr rhs se (contResultType cont)	$
    simplExprF body cont
\end{code}


---------------------------------

\begin{code}
simplLam fun cont
  = go fun cont
  where
    zap_it = mkLamBndrZapper fun (countArgs cont)
    cont_ty = contResultType cont

      	-- Type-beta reduction
    go (Lam bndr body) (ApplyTo _ (Type ty_arg) arg_se body_cont)
      =	ASSERT( isTyVar bndr )
	tick (BetaReduction bndr)		`thenSmpl_`
	getInScope				`thenSmpl` \ in_scope ->
	let
		ty' = substTy (mkSubst in_scope arg_se) ty_arg
	in
	extendSubst bndr (DoneTy ty')
	(go body body_cont)

	-- Ordinary beta reduction
    go (Lam bndr body) cont@(ApplyTo _ arg arg_se body_cont)
      = tick (BetaReduction bndr)			`thenSmpl_`
	simplBeta zapped_bndr arg arg_se cont_ty
	(go body body_cont)
      where
	zapped_bndr = zap_it bndr

	-- Not enough args
    go lam@(Lam _ _) cont = completeLam [] lam cont

	-- Exactly enough args
    go expr cont = simplExprF expr cont


-- completeLam deals with the case where a lambda doesn't have an ApplyTo
-- continuation.  Try for eta reduction, but *only* if we get all
-- the way to an exprIsTrivial expression.  
-- 'acc' holds the simplified binders, in reverse order

completeLam acc (Lam bndr body) cont
  = simplBinder bndr			$ \ bndr' ->
    completeLam (bndr':acc) body cont

completeLam acc body cont
  = simplExpr body 			`thenSmpl` \ body' ->

    case (opt_SimplDoEtaReduction, check_eta acc body') of
	(True, Just body'') 	-- Eta reduce!
		-> tick (EtaReduction (head acc))	`thenSmpl_`
		   rebuild body'' cont

	other	-> 	-- No eta reduction
		   rebuild (foldl (flip Lam) body' acc) cont
			-- Remember, acc is the reversed binders
  where
	-- NB: the binders are reversed
    check_eta (b : bs) (App fun arg)
	|  (varToCoreExpr b `cheapEqExpr` arg)
	= check_eta bs fun

    check_eta [] body
	| exprIsTrivial body && 		-- ONLY if the body is trivial
	  not (any (`elemVarSet` body_fvs) acc)
	= Just body		-- Success!
	where
	  body_fvs = exprFreeVars body

    check_eta _ _ = Nothing	-- Bale out

mkLamBndrZapper :: CoreExpr 	-- Function
		-> Int		-- Number of args
		-> Id -> Id	-- Use this to zap the binders
mkLamBndrZapper fun n_args
  | n_args >= n_params fun = \b -> b		-- Enough args
  | otherwise		   = \b -> maybeModifyIdInfo zapLamIdInfo b
  where
    n_params (Lam b e) | isId b    = 1 + n_params e
		       | otherwise = n_params e
    n_params other		   = 0::Int
\end{code}


---------------------------------
simplConArgs makes sure that the arguments all end up being atomic.
That means it may generate some Lets, hence the strange type

\begin{code}
simplConArgs :: [InArg] -> ([OutArg] -> SimplM OutExprStuff) -> SimplM OutExprStuff
simplConArgs [] thing_inside
  = thing_inside []

simplConArgs (arg:args) thing_inside
  = switchOffInlining (simplExpr arg)	`thenSmpl` \ arg' ->
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
  = getSubst	`thenSmpl` \ subst ->
    returnSmpl (substTy subst ty)
\end{code}


%************************************************************************
%*									*
\subsection{Binding}
%*									*
%************************************************************************

@simplBeta@ is used for non-recursive lets in expressions, 
as well as true beta reduction.

Very similar to @simplLazyBind@, but not quite the same.

\begin{code}
simplBeta :: InId 			-- Binder
	  -> InExpr -> SubstEnv		-- Arg, with its subst-env
	  -> OutType			-- Type of thing computed by the context
	  -> SimplM OutExprStuff	-- The body
	  -> SimplM OutExprStuff
#ifdef DEBUG
simplBeta bndr rhs rhs_se cont_ty thing_inside
  | isTyVar bndr
  = pprPanic "simplBeta" (ppr bndr <+> ppr rhs)
#endif

simplBeta bndr rhs rhs_se cont_ty thing_inside
  | preInlineUnconditionally bndr && not opt_SimplNoPreInlining
  = tick (PreInlineUnconditionally bndr)		`thenSmpl_`
    extendSubst bndr (ContEx rhs_se rhs) thing_inside

  | otherwise
  = 	-- Simplify the RHS
    simplBinder bndr					$ \ bndr' ->
    simplArg (idType bndr') (getIdDemandInfo bndr)
	     rhs rhs_se cont_ty				$ \ rhs' ->

	-- Now complete the binding and simplify the body
    completeBeta bndr bndr' rhs' thing_inside

completeBeta bndr bndr' rhs' thing_inside
  | isUnLiftedType (idType bndr') && not (exprOkForSpeculation rhs')
	-- Make a case expression instead of a let
	-- These can arise either from the desugarer,
	-- or from beta reductions: (\x.e) (x +# y)
  = getInScope 			`thenSmpl` \ in_scope ->
    thing_inside		`thenSmpl` \ (floats, (_, body)) ->
    returnSmpl ([], (in_scope, Case rhs' bndr' [(DEFAULT, [], mkLets floats body)]))

  | otherwise
  = completeBinding bndr bndr' rhs' thing_inside
\end{code}


\begin{code}
simplArg :: OutType -> Demand
	 -> InExpr -> SubstEnv
	 -> OutType		-- Type of thing computed by the context
	 -> (OutExpr -> SimplM OutExprStuff)
	 -> SimplM OutExprStuff
simplArg arg_ty demand arg arg_se cont_ty thing_inside
  | isStrict demand || 
    isUnLiftedType arg_ty || 
    (opt_DictsStrict && isDictTy arg_ty && isDataType arg_ty)
	-- Return true only for dictionary types where the dictionary
	-- has more than one component (else we risk poking on the component
	-- of a newtype dictionary)
  = getSubstEnv					`thenSmpl` \ body_se ->
    transformRhs arg				`thenSmpl` \ t_arg ->
    setSubstEnv arg_se (simplExprF t_arg (ArgOf NoDup cont_ty $ \ arg' ->
    setSubstEnv body_se (thing_inside arg')
    ))	-- NB: we must restore body_se before carrying on with thing_inside!!

  | otherwise
  = simplRhs NotTopLevel True arg_ty arg arg_se thing_inside
\end{code}


completeBinding
	- deals only with Ids, not TyVars
	- take an already-simplified RHS

It does *not* attempt to do let-to-case.  Why?  Because they are used for

	- top-level bindings
		(when let-to-case is impossible) 

	- many situations where the "rhs" is known to be a WHNF
		(so let-to-case is inappropriate).

\begin{code}
completeBinding :: InId 		-- Binder
		-> OutId		-- New binder
	        -> OutExpr		-- Simplified RHS
		-> SimplM (OutStuff a)	-- Thing inside
	   	-> SimplM (OutStuff a)

completeBinding old_bndr new_bndr new_rhs thing_inside
  |  isDeadBinder old_bndr	-- This happens; for example, the case_bndr during case of
				-- known constructor:  case (a,b) of x { (p,q) -> ... }
				-- Here x isn't mentioned in the RHS, so we don't want to
				-- create the (dead) let-binding  let x = (a,b) in ...
  =  thing_inside

  |  postInlineUnconditionally old_bndr new_rhs
	-- Maybe we don't need a let-binding!  Maybe we can just
	-- inline it right away.  Unlike the preInlineUnconditionally case
	-- we are allowed to look at the RHS.
	--
	-- NB: a loop breaker never has postInlineUnconditionally True
	-- and non-loop-breakers only have *forward* references
	-- Hence, it's safe to discard the binding
  =  tick (PostInlineUnconditionally old_bndr)	`thenSmpl_`
     extendSubst old_bndr (DoneEx new_rhs)	
     thing_inside

  |  otherwise
  =  getSubst			`thenSmpl` \ subst ->
     let
	bndr_info = idInfo old_bndr
	old_rules = specInfo bndr_info
	new_rules = substRules subst old_rules

	-- The new binding site Id needs its specialisations re-attached
	bndr_w_arity = new_bndr `setIdArity` ArityAtLeast (exprArity new_rhs)

	binding_site_id
	  | isEmptyCoreRules old_rules = bndr_w_arity 
	  | otherwise		       = bndr_w_arity `setIdSpecialisation` new_rules

	-- At the occurrence sites we want to know the unfolding,
	-- and the occurrence info of the original
	-- (simplBinder cleaned up the inline prag of the original
	--  to eliminate un-stable info, in case this expression is
	--  simplified a second time; hence the need to reattach it)
	occ_site_id = binding_site_id
		      `setIdUnfolding` mkUnfolding new_rhs
		      `setInlinePragma` inlinePragInfo bndr_info
     in
     modifyInScope occ_site_id thing_inside	`thenSmpl` \ stuff ->
     returnSmpl (addBind (NonRec binding_site_id new_rhs) stuff)
\end{code}    


%************************************************************************
%*									*
\subsection{simplLazyBind}
%*									*
%************************************************************************

simplLazyBind basically just simplifies the RHS of a let(rec).
It does two important optimisations though:

	* It floats let(rec)s out of the RHS, even if they
	  are hidden by big lambdas

	* It does eta expansion

\begin{code}
simplLazyBind :: TopLevelFlag
	      -> InId -> OutId
	      -> InExpr 		-- The RHS
	      -> SimplM (OutStuff a)	-- The body of the binding
	      -> SimplM (OutStuff a)
-- When called, the subst env is correct for the entire let-binding
-- and hence right for the RHS.
-- Also the binder has already been simplified, and hence is in scope

simplLazyBind top_lvl bndr bndr' rhs thing_inside
  | preInlineUnconditionally bndr && not opt_SimplNoPreInlining
  = tick (PreInlineUnconditionally bndr)		`thenSmpl_`
    getSubstEnv 					`thenSmpl` \ rhs_se ->
    (extendSubst bndr (ContEx rhs_se rhs) thing_inside)

  | otherwise
  = 	-- Simplify the RHS
    getSubstEnv 					`thenSmpl` \ rhs_se ->

    simplRhs top_lvl False {- Not ok to float unboxed -}
	     (idType bndr')
	     rhs rhs_se  				$ \ rhs' ->

	-- Now compete the binding and simplify the body
    completeBinding bndr bndr' rhs' thing_inside
\end{code}



\begin{code}
simplRhs :: TopLevelFlag
	 -> Bool		-- True <=> OK to float unboxed (speculative) bindings
	 -> OutType -> InExpr -> SubstEnv
	 -> (OutExpr -> SimplM (OutStuff a))
	 -> SimplM (OutStuff a)
simplRhs top_lvl float_ubx rhs_ty rhs rhs_se thing_inside
  =    	-- Swizzle the inner lets past the big lambda (if any)
	-- and try eta expansion
    transformRhs rhs					`thenSmpl` \ t_rhs ->

	-- Simplify it
    setSubstEnv rhs_se (simplExprF t_rhs (Stop rhs_ty))	`thenSmpl` \ (floats, (in_scope', rhs')) ->

	-- Float lets out of RHS
    let
	(floats_out, rhs'') | float_ubx = (floats, rhs')
			    | otherwise	= splitFloats floats rhs' 
    in
    if (isTopLevel top_lvl || exprIsCheap rhs') && 	-- Float lets if (a) we're at the top level
        not (null floats_out)				-- or 		 (b) it exposes a cheap (i.e. duplicatable) expression
    then
	tickLetFloat floats_out				`thenSmpl_`
		-- Do the float
		-- 
		-- There's a subtlety here.  There may be a binding (x* = e) in the
		-- floats, where the '*' means 'will be demanded'.  So is it safe
		-- to float it out?  Answer no, but it won't matter because
		-- we only float if arg' is a WHNF,
		-- and so there can't be any 'will be demanded' bindings in the floats.
		-- Hence the assert
	WARN( any demanded_float floats_out, ppr floats_out )
	setInScope in_scope' (thing_inside rhs'') 	`thenSmpl` \ stuff ->
		-- in_scope' may be excessive, but that's OK;
		-- it's a superset of what's in scope
	returnSmpl (addBinds floats_out stuff)
    else	
		-- Don't do the float
	thing_inside (mkLets floats rhs')

-- In a let-from-let float, we just tick once, arbitrarily
-- choosing the first floated binder to identify it
tickLetFloat (NonRec b r      : fs) = tick (LetFloatFromLet b)
tickLetFloat (Rec ((b,r):prs) : fs) = tick (LetFloatFromLet b)
	
demanded_float (NonRec b r) = isStrict (getIdDemandInfo b) && not (isUnLiftedType (idType b))
		-- Unlifted-type (cheap-eagerness) lets may well have a demanded flag on them
demanded_float (Rec _)	    = False

-- Don't float any unlifted bindings out, because the context
-- is either a Rec group, or the top level, neither of which
-- can tolerate them.
splitFloats floats rhs
  = go floats
  where
    go []		    = ([], rhs)
    go (f:fs) | must_stay f = ([], mkLets (f:fs) rhs)
	      | otherwise   = case go fs of
				   (out, rhs') -> (f:out, rhs')

    must_stay (Rec prs)    = False	-- No unlifted bindings in here
    must_stay (NonRec b r) = isUnLiftedType (idType b)
\end{code}



%************************************************************************
%*									*
\subsection{Variables}
%*									*
%************************************************************************

\begin{code}
simplVar var cont
  = freeTick LeafVisit	`thenSmpl_`
    getSubst		`thenSmpl` \ subst ->
    case lookupSubst subst var of
	Just (DoneEx (Var v)) -> zapSubstEnv (simplVar v cont)
	Just (DoneEx e)	      -> zapSubstEnv (simplExprF e cont)
	Just (ContEx env' e)  -> setSubstEnv env' (simplExprF e cont)

	Nothing -> let
			var' = case lookupInScope subst var of
				 Just v' -> v'
				 Nothing -> 
#ifdef DEBUG
					    if isLocallyDefined var && not (idMustBeINLINEd var)
						-- The idMustBeINLINEd test accouunts for the fact
						-- that class dictionary constructors don't have top level
						-- bindings and hence aren't in scope.
					    then
						-- Not in scope
						pprTrace "simplVar:" (ppr var) var
					    else
#endif
					    var
		   in
		   getBlackList		`thenSmpl` \ black_list ->
		   getInScope		`thenSmpl` \ in_scope ->
		   completeCall black_list in_scope var' cont

---------------------------------------------------------
--	Dealing with a call

completeCall black_list_fn in_scope var cont
	-- Look for rules or specialisations that match
	-- Do this *before* trying inlining because some functions
	-- have specialisations *and* are strict; we don't want to
	-- inline the wrapper of the non-specialised thing... better
	-- to call the specialised thing instead.
  | maybeToBool maybe_rule_match
  = tick (RuleFired rule_name)			`thenSmpl_`
    zapSubstEnv (simplExprF rule_rhs (pushArgs emptySubstEnv rule_args result_cont))
	-- See note below about zapping the substitution here

	-- Look for an unfolding. There's a binding for the
	-- thing, but perhaps we want to inline it anyway
  | maybeToBool maybe_inline
  = tick (UnfoldingDone var)		`thenSmpl_`
    zapSubstEnv (completeInlining var unf_template discard_inline_cont)
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!
    
  | otherwise		-- Neither rule nor inlining
			-- Use prepareArgs to use function strictness
  = prepareArgs (ppr var) (idType var) (get_str var) cont	$ \ args' cont' ->
    rebuild (mkApps (Var var) args') cont'

  where
    get_str var = case getIdStrictness var of
			NoStrictnessInfo		  -> (repeat wwLazy, False)
			StrictnessInfo demands result_bot -> (demands, result_bot)

  
    (args', result_cont) = contArgs in_scope cont
    inline_call	         = contIsInline result_cont
    interesting_cont     = contIsInteresting result_cont
    discard_inline_cont  | inline_call = discardInline cont
		         | otherwise   = cont

	---------- Unfolding stuff
    maybe_inline  = callSiteInline black_listed inline_call 
				   var args' interesting_cont
    Just unf_template = maybe_inline
    black_listed      = black_list_fn var

    	---------- Specialisation stuff
    maybe_rule_match           = lookupRule in_scope var args'
    Just (rule_name, rule_rhs, rule_args) = maybe_rule_match


-- First a special case
-- Don't actually inline the scrutinee when we see
--	case x of y { .... }
-- and x has unfolding (C a b).  Why not?  Because
-- we get a silly binding y = C a b.  If we don't
-- inline knownCon can directly substitute x for y instead.
completeInlining var (Con con con_args) (Select _ bndr alts se cont)
  | conOkForAlt con 
  = knownCon (Var var) con con_args bndr alts se cont

-- Now the normal case
completeInlining var unfolding cont
  = simplExprF unfolding cont

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


\begin{code}
---------------------------------------------------------
--	Preparing arguments for a call

prepareArgs :: SDoc 	-- Error message info
	    -> OutType -> ([Demand],Bool) -> SimplCont
	    -> ([OutExpr] -> SimplCont -> SimplM OutExprStuff)
	    -> SimplM OutExprStuff

prepareArgs pp_fun orig_fun_ty (fun_demands, result_bot) orig_cont thing_inside
  = go [] demands orig_fun_ty orig_cont
  where
    not_enough_args = fun_demands `lengthExceeds` countValArgs orig_cont
	-- "No strictness info" is signalled by an infinite list of wwLazy
 
    demands | not_enough_args = repeat wwLazy			-- Not enough args, or no strictness
	    | result_bot      = fun_demands			-- Enough args, and function returns bottom
	    | otherwise	      = fun_demands ++ repeat wwLazy	-- Enough args and function does not return bottom
	-- NB: demands is finite iff enough args and result_bot is True

	-- Main game plan: loop through the arguments, simplifying
	-- each of them in turn.  We carry with us a list of demands,
	-- and the type of the function-applied-to-earlier-args

	-- Type argument
    go acc ds fun_ty (ApplyTo _ arg@(Type ty_arg) se cont)
	= getInScope		`thenSmpl` \ in_scope ->
	  let
		ty_arg' = substTy (mkSubst in_scope se) ty_arg
		res_ty  = applyTy fun_ty ty_arg'
	  in
	  go (Type ty_arg' : acc) ds res_ty cont

	-- Value argument
    go acc (d:ds) fun_ty (ApplyTo _ val_arg se cont)
	= case splitFunTy_maybe fun_ty of {
		Nothing -> pprTrace "prepareArgs" (pp_fun $$ ppr orig_fun_ty $$ ppr orig_cont) 
			   (thing_inside (reverse acc) cont) ;
		Just (arg_ty, res_ty) ->
	  simplArg arg_ty d val_arg se (contResultType cont) 	$ \ arg' ->
	  go (arg':acc) ds res_ty cont }

	-- We've run out of demands, which only happens for functions
	-- we *know* now return bottom
	-- This deals with
	--	* case (error "hello") of { ... }
	--	* (error "Hello") arg
	--	* f (error "Hello") where f is strict
	--	etc
    go acc [] fun_ty cont = tick_case_of_error cont		`thenSmpl_`
    			    thing_inside (reverse acc) (discardCont cont)

	-- We're run out of arguments
    go acc ds fun_ty cont = thing_inside (reverse acc) cont

-- Boring: we must only record a tick if there was an interesting
--	   continuation to discard.  If not, we tick forever.
tick_case_of_error (Stop _)		 = returnSmpl ()
tick_case_of_error (CoerceIt _ (Stop _)) = returnSmpl ()
tick_case_of_error other		 = tick BottomFound
\end{code}

%************************************************************************
%*									*
\subsection{Decisions about inlining}
%*									*
%************************************************************************

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
	-- 
	-- Evne RHSs labelled InlineMe aren't caught here, because
	-- there might be no benefit from inlining at the call site.
	-- But things labelled 'IMustBeINLINEd' *are* caught.  We use this
	-- for the trivial bindings introduced by SimplUtils.mkRhsTyLam
preInlineUnconditionally bndr
  = case getInlinePragma bndr of
	IMustBeINLINEd			      -> True
	ICanSafelyBeINLINEd NotInsideLam True -> True	-- Not inside a lambda,
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
  | isExportedId bndr 
  = False
  | otherwise
  = case getInlinePragma bndr of
	IAmALoopBreaker			    	  -> False   

	ICanSafelyBeINLINEd InsideLam one_branch  -> exprIsTrivial rhs
		-- Don't inline even WHNFs inside lambdas; doing so may
		-- simply increase allocation when the function is called
		-- This isn't the last chance; see NOTE above.

	ICanSafelyBeINLINEd not_in_lam one_branch -> one_branch || exprIsTrivial rhs
		-- Was 'exprIsDupable' instead of 'exprIsTrivial' but the
		-- decision about duplicating code is best left to callSiteInline

	other				    	  -> exprIsTrivial rhs	-- Duplicating is *free*
		-- NB: Even InlineMe and IMustBeINLINEd are ignored here
		-- Why?  Because we don't even want to inline them into the
		-- RHS of constructor arguments. See NOTE above
		-- NB: Even IMustBeINLINEd is ignored here: if the rhs is trivial
		-- it's best to inline it anyway.  We often get a=E; b=a
		-- from desugaring, with both a and b marked NOINLINE.
\end{code}



%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
-------------------------------------------------------------------
-- Finish rebuilding
rebuild_done expr
  = getInScope			`thenSmpl` \ in_scope ->
    returnSmpl ([], (in_scope, expr))

---------------------------------------------------------
rebuild :: OutExpr -> SimplCont -> SimplM OutExprStuff

--	Stop continuation
rebuild expr (Stop _) = rebuild_done expr

--	ArgOf continuation
rebuild expr (ArgOf _ _ cont_fn) = cont_fn expr

--	ApplyTo continuation
rebuild expr cont@(ApplyTo _ arg se cont')
  = setSubstEnv se (simplExpr arg)	`thenSmpl` \ arg' ->
    rebuild (App expr arg') cont'

--	Coerce continuation
rebuild expr (CoerceIt to_ty cont)
  = rebuild (mkCoerce to_ty expr) cont

--	Inline continuation
rebuild expr (InlinePlease cont)
  = rebuild (Note InlineCall expr) cont

-- 	Case of known constructor or literal
rebuild expr@(Con con args) (Select _ bndr alts se cont)
  | conOkForAlt con	-- Knocks out PrimOps and NoRepLits
  = knownCon expr con args bndr alts se cont


---------------------------------------------------------
-- 	The other Select cases

rebuild scrut (Select _ bndr alts se cont)
  | 	-- Check that the RHSs are all the same, and
	-- don't use the binders in the alternatives
	-- This test succeeds rapidly in the common case of
	-- a single DEFAULT alternative
    all (cheapEqExpr rhs1) other_rhss && all binders_unused alts

	-- Check that the scrutinee can be let-bound instead of case-bound
    && (   (isUnLiftedType (idType bndr) && 	-- It's unlifted and floatable
	    exprOkForSpeculation scrut)		-- NB: scrut = an unboxed variable satisfies 
	|| exprIsValue scrut			-- It's already evaluated
	|| var_demanded_later scrut		-- It'll be demanded later

--      || not opt_SimplPedanticBottoms)	-- Or we don't care!
--	We used to allow improving termination by discarding cases, unless -fpedantic-bottoms was on,
-- 	but that breaks badly for the dataToTag# primop, which relies on a case to evaluate
-- 	its argument:  case x of { y -> dataToTag# y }
--	Here we must *not* discard the case, because dataToTag# just fetches the tag from
--	the info pointer.  So we'll be pedantic all the time, and see if that gives any
-- 	other problems
       )

--    && opt_SimplDoCaseElim
--	[June 99; don't test this flag.  The code generator dies if it sees
--		case (\x.e) of f -> ...  
--	so better to always do it

=   	-- Get rid of the case altogether
	-- See the extensive notes on case-elimination below
	-- Remember to bind the binder though!
    tick (CaseElim bndr)		`thenSmpl_` (
    setSubstEnv se			$			
    simplBinder bndr			$ \ bndr' ->
    completeBinding bndr bndr' scrut 	$
    simplExprF rhs1 cont)

  | otherwise
  = rebuild_case scrut bndr alts se cont
  where
    (rhs1:other_rhss)		 = [rhs | (_,_,rhs) <- alts]
    binders_unused (_, bndrs, _) = all isDeadBinder bndrs

    var_demanded_later (Var v) = isStrict (getIdDemandInfo bndr)	-- It's going to be evaluated later
    var_demanded_later other   = False
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


Blob of helper functions for the "case-of-something-else" situation.

\begin{code}
---------------------------------------------------------
-- 	Case of something else

rebuild_case scrut case_bndr alts se cont
  = 	-- Prepare case alternatives
    prepareCaseAlts case_bndr (splitTyConApp_maybe (idType case_bndr))
		    scrut_cons alts		`thenSmpl` \ better_alts ->
    
	-- Set the new subst-env in place (before dealing with the case binder)
    setSubstEnv se				$

	-- Deal with the case binder, and prepare the continuation;
	-- The new subst_env is in place
    prepareCaseCont better_alts cont		$ \ cont' ->
	

	-- Deal with variable scrutinee
    (	simplBinder case_bndr 			$ \ case_bndr' ->
	substForVarScrut scrut case_bndr'		$ \ zap_occ_info ->
	let
	   case_bndr'' = zap_occ_info case_bndr'
	in

	-- Deal with the case alternaatives
	simplAlts zap_occ_info scrut_cons 
	          case_bndr'' better_alts cont'	`thenSmpl` \ alts' ->

	mkCase scrut case_bndr'' alts'
    )						`thenSmpl` \ case_expr ->

	-- Notice that the simplBinder, prepareCaseCont, etc, do *not* scope
	-- over the rebuild_done; rebuild_done returns the in-scope set, and
	-- that should not include these chaps!
    rebuild_done case_expr	
  where
	-- scrut_cons tells what constructors the scrutinee can't possibly match
    scrut_cons = case scrut of
		   Var v -> otherCons (getIdUnfolding v)
		   other -> []


knownCon expr con args bndr alts se cont
  = tick (KnownBranch bndr)	`thenSmpl_`
    setSubstEnv se		(
    simplBinder bndr		$ \ bndr' ->
    case findAlt con alts of
	(DEFAULT, bs, rhs)     -> ASSERT( null bs )
				  completeBinding bndr bndr' expr $
					-- Don't use completeBeta here.  The expr might be
					-- an unboxed literal, like 3, or a variable
					-- whose unfolding is an unboxed literal... and
					-- completeBeta will just construct another case
					-- expression!
			          simplExprF rhs cont

	(Literal lit, bs, rhs) -> ASSERT( null bs )
				  extendSubst bndr (DoneEx expr)	$
					-- Unconditionally substitute, because expr must
					-- be a variable or a literal.  It can't be a
					-- NoRep literal because they don't occur in
					-- case patterns.
				  simplExprF rhs cont

	(DataCon dc, bs, rhs)  -> ASSERT( length bs == length real_args )
				  completeBinding bndr bndr' expr	$
					-- See note above
				  extendSubstList bs (map mk real_args) $
			          simplExprF rhs cont
			       where
				  real_args    = drop (dataConNumInstArgs dc) args
				  mk (Type ty) = DoneTy ty
				  mk other     = DoneEx other
    )
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

	case x or { (a,b) -> a b }

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
prepareCaseAlts bndr (Just (tycon, inst_tys)) scrut_cons alts
  | isDataTyCon tycon
  = case (findDefault filtered_alts, missing_cons) of

	((alts_no_deflt, Just rhs), [data_con]) 	-- Just one missing constructor!
		-> tick (FillInCaseDefault bndr)	`thenSmpl_`
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
prepareCaseAlts _ _ scrut_cons alts
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
	=	-- In the default case we record the constructors that the
		-- case-binder *can't* be.
		-- We take advantage of any OtherCon info in the case scrutinee
	  modifyInScope (case_bndr'' `setIdUnfolding` mkOtherCon handled_cons)	$ 
	  simplExprC rhs cont'							`thenSmpl` \ rhs' ->
	  returnSmpl (DEFAULT, [], rhs')

    simpl_alt (con, vs, rhs)
	= 	-- Deal with the pattern-bound variables
		-- Mark the ones that are in ! positions in the data constructor
		-- as certainly-evaluated
	  simplBinders (add_evals con vs)	$ \ vs' ->

		-- Bind the case-binder to (Con args)
	  let
		con_app = Con con (map Type inst_tys' ++ map varToCoreExpr vs')
	  in
	  modifyInScope (case_bndr'' `setIdUnfolding` mkUnfolding con_app)	$
	  simplExprC rhs cont'		`thenSmpl` \ rhs' ->
	  returnSmpl (con, vs', rhs')


	-- add_evals records the evaluated-ness of the bound variables of
	-- a case pattern.  This is *important*.  Consider
	--	data T = T !Int !Int
	--
	--	case x of { T a b -> T (a+1) b }
	--
	-- We really must record that b is already evaluated so that we don't
	-- go and re-evaluate it when constructing the result.

    add_evals (DataCon dc) vs = cat_evals vs (dataConRepStrictness dc)
    add_evals other_con    vs = vs

    cat_evals [] [] = []
    cat_evals (v:vs) (str:strs)
	| isTyVar v    = v				     : cat_evals vs (str:strs)
	| isStrict str = (v' `setIdUnfolding` mkOtherCon []) : cat_evals vs strs
	| otherwise    = v'				     : cat_evals vs strs
	where
	  v' = zap_occ_info v
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

mkDupableCont _ (CoerceIt ty cont) thing_inside
  = mkDupableCont ty cont		$ \ cont' ->
    thing_inside (CoerceIt ty cont')

mkDupableCont ty (InlinePlease cont) thing_inside
  = mkDupableCont ty cont		$ \ cont' ->
    thing_inside (InlinePlease cont')

mkDupableCont join_arg_ty (ArgOf _ cont_ty cont_fn) thing_inside
  = 	-- Build the RHS of the join point
    simplType join_arg_ty				`thenSmpl` \ join_arg_ty' ->
    newId join_arg_ty'					( \ arg_id ->
    	getSwitchChecker				`thenSmpl` \ chkr ->
	cont_fn (Var arg_id)				`thenSmpl` \ (binds, (_, rhs)) ->
	returnSmpl (Lam arg_id (mkLets binds rhs))
    )							`thenSmpl` \ join_rhs ->
   
	-- Build the join Id and continuation
    newId (coreExprType join_rhs)		$ \ join_id ->
    let
	new_cont = ArgOf OkToDup cont_ty
			 (\arg' -> rebuild_done (App (Var join_id) arg'))
    in
	
	-- Do the thing inside
    thing_inside new_cont		`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec join_id join_rhs) res)

mkDupableCont ty (ApplyTo _ arg se cont) thing_inside
  = mkDupableCont (funResultTy ty) cont 		$ \ cont' ->
    setSubstEnv se (simplExpr arg)			`thenSmpl` \ arg' ->
    if exprIsDupable arg' then
	thing_inside (ApplyTo OkToDup arg' emptySubstEnv cont')
    else
    newId (coreExprType arg')						$ \ bndr ->
    thing_inside (ApplyTo OkToDup (Var bndr) emptySubstEnv cont')	`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec bndr arg') res)

mkDupableCont ty (Select _ case_bndr alts se cont) thing_inside
  = tick (CaseOfCase case_bndr)						`thenSmpl_`
    setSubstEnv se (
	simplBinder case_bndr						$ \ case_bndr' ->
	prepareCaseCont alts cont					$ \ cont' ->
	mapAndUnzipSmpl (mkDupableAlt case_bndr case_bndr' cont') alts	`thenSmpl` \ (alt_binds_s, alts') ->
	returnSmpl (concat alt_binds_s, alts')
    )					`thenSmpl` \ (alt_binds, alts') ->

    extendInScopes [b | NonRec b _ <- alt_binds]		$

	-- NB that the new alternatives, alts', are still InAlts, using the original
	-- binders.  That means we can keep the case_bndr intact. This is important
	-- because another case-of-case might strike, and so we want to keep the
	-- info that the case_bndr is dead (if it is, which is often the case).
	-- This is VITAL when the type of case_bndr is an unboxed pair (often the
	-- case in I/O rich code.  We aren't allowed a lambda bound
	-- arg of unboxed tuple type, and indeed such a case_bndr is always dead
    thing_inside (Select OkToDup case_bndr alts' se (Stop (contResultType cont)))	`thenSmpl` \ res ->

    returnSmpl (addBinds alt_binds res)


mkDupableAlt :: InId -> OutId -> SimplCont -> InAlt -> SimplM (OutStuff InAlt)
mkDupableAlt case_bndr case_bndr' cont alt@(con, bndrs, rhs)
  =	-- Not worth checking whether the rhs is small; the
	-- inliner will inline it if so.
    simplBinders bndrs					$ \ bndrs' ->
    simplExprC rhs cont					`thenSmpl` \ rhs' ->
    let
	rhs_ty' = coreExprType rhs'
        (used_bndrs, used_bndrs')
	   = unzip [pr | pr@(bndr,bndr') <- zip (case_bndr  : bndrs)
						(case_bndr' : bndrs'),
			 not (isDeadBinder bndr)]
		-- The new binders have lost their occurrence info,
		-- so we have to extract it from the old ones
    in
    ( if null used_bndrs' 
	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::State# RealWorld#.  The value passed to this function
	-- is realworld#, which generates (almost) no code.

	-- There's a slight infelicity here: we pass the overall 
	-- case_bndr to all the join points if it's used in *any* RHS,
	-- because we don't know its usage in each RHS separately

	-- We used to say "&& isUnLiftedType rhs_ty'" here, but now
	-- we make the join point into a function whenever used_bndrs'
	-- is empty.  This makes the join-point more CPR friendly. 
	-- Consider:	let j = if .. then I# 3 else I# 4
	--		in case .. of { A -> j; B -> j; C -> ... }
	--
	-- Now CPR should not w/w j because it's a thunk, so
	-- that means that the enclosing function can't w/w either,
	-- which is a lose.  Here's the example that happened in practice:
	--	kgmod :: Int -> Int -> Int
	--	kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
	--	            then 78
	--		    else 5

	then newId realWorldStatePrimTy  $ \ rw_id ->
	     returnSmpl ([rw_id], [Var realWorldPrimId])
	else 
	     returnSmpl (used_bndrs', map varToCoreExpr used_bndrs)
    )
	`thenSmpl` \ (final_bndrs', final_args) ->

    newId (foldr (mkFunTy . idType) rhs_ty' final_bndrs')	$ \ join_bndr ->

	-- Notice that we make the lambdas into one-shot-lambdas.  The
	-- join point is sure to be applied at most once, and doing so
	-- prevents the body of the join point being floated out by
	-- the full laziness pass
    returnSmpl ([NonRec join_bndr (mkLams (map setOneShotLambda final_bndrs') rhs')],
		(con, bndrs, mkApps (Var join_bndr) final_args))
\end{code}
