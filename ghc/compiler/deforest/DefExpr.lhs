%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DefExpr]{Transformation Algorithm for Expressions}

>#include "HsVersions.h"

> module DefExpr (
> 	tran
>	) where
>
> import DefSyn
> import CoreSyn
> import DefUtils
> import Core2Def	( c2d ) 		-- for unfoldings
> import TreelessForm
> import Cyclic

> import Type		( applyTypeEnvToTy, isPrimType,
>			  SigmaType(..), Type
>			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
>			)
> import CmdLineOpts	( SwitchResult, switchIsOn )
> import CoreUnfold	( UnfoldingDetails(..) )
> import CoreUtils	( mkValLam, unTagBinders, coreExprType )
> import Id		( applyTypeEnvToId, getIdUnfolding, isTopLevId, Id,
> 			  isInstId_maybe
>			)
> import Inst		-- Inst(..)
> import IdInfo
> import Maybes		( Maybe(..) )
> import Outputable
> import UniqSupply
> import Util

> -- tmp
> import Pretty
> import Def2Core

-----------------------------------------------------------------------------
Top level transformation

A type environment mapping type variables to types is carried around.
This is extended by one rule only: reduction of a type application.

> tran
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr		-- Environment
>	-> TypeEnv			-- Type environment
>	-> DefExpr			-- input expression
>	-> [DefCoreArg]			-- args
>	-> UniqSM DefExpr

> tran sw p t e@(Var (DefArgVar id)) as =
> 	tranVar sw p id
>    		(
>		 mapArgs (\e -> tran sw p t e []) as  `thenUs` \as ->
>		 returnUs (mkGenApp (Var (DefArgVar new_id)) as)
>		)
>		(
>		 \e ->
>		   tran sw p t e as	`thenUs` \e ->
>		   returnUs (mkLabel (mkGenApp (Var (DefArgVar new_id))
>		   			(map (substTyArg t) as))
>				      e)
>		)
>	where new_id = applyTypeEnvToId t id

> tran sw p t e@(Lit l) [] =
> 	returnUs e
>
> tran sw p t (Con c ts es) [] =
> 	mapUs (tranAtom sw p t) es 		`thenUs` \es ->
>	returnUs (Con c (map (applyTypeEnvToTy t) ts) es)
>
> tran sw p t (Prim op ts es) [] =	-- XXX constant folding?
> 	mapUs (tranAtom sw p t) es	`thenUs` \es ->
>	returnUs (Prim op (map (applyTypeEnvToTy t) ts) es)
>
> tran sw p t (Lam vs e) [] =
> 	tran sw p t e []			`thenUs` \e ->
>	returnUs (mkValLam (map (applyTypeEnvToId t) vs) e)
>
> tran sw p t (Lam vs e) as =
> 	subst s e				`thenUs` \e ->
> 	tran sw p t (mkValLam rvs e) ras
>   where
>   	(rvs,ras,s) = mkSubst vs as []

> tran sw p t (CoTyLam alpha e) [] =
> 	tran sw p t e []			`thenUs` \e ->
>	returnUs (CoTyLam alpha e)
>

	ToDo: use the environment rather than doing explicit substitution
	(didn't work last time I tried :)

> tran sw p t (CoTyLam alpha e) (TypeArg ty : as) =
> 	tran sw p t (applyTypeEnvToExpr (mkTyVarEnv [(alpha,ty)]) e) as

> tran sw p t (App e v) as =
> 	maybeJumbleApp e v			`thenUs` \j ->
>	case j of
>		Nothing -> tran sw p t e (ValArg v : as)
>		Just e' -> tran sw p t e' as
>
> tran sw p t (CoTyApp e ty) as =
>	tran sw p t e (TypeArg (applyTypeEnvToTy t ty) : as)
>
> tran sw p t (Let (NonRec v e) e') as =
> 	tran sw p t e []			`thenUs` \e  ->
>	if isConstant e then
>		trace "yippee!!" $
>		subst [(v,removeLabels e)] e'		`thenUs` \e' ->
>		tran sw p t e' as
>	else
>		tran sw p t e' as		`thenUs` \e' ->
>		returnUs (Let (NonRec (applyTypeEnvToId t v) e) e')
>
> tran sw p t (Let (Rec bs) e) as =
> 	tranRecBinds sw p t bs e		`thenUs` \(p',resid,e) ->
>	tran sw p' t e as			`thenUs` \e ->
>	returnUs (mkDefLetrec resid e)
>
> tran sw p t (SCC l e) as =
> 	tran sw p t e []			`thenUs` \e ->
>	mapArgs (\e -> tran sw p t e []) as	`thenUs` \as ->
>	returnUs (mkGenApp (SCC l e) as)
>
> tran sw p t (Case e ps) as =
> 	tranCase sw p t e [] ps as
>
> tran _ _ _ e as =
> 	defPanic "DefExpr" "tran" (mkGenApp e as)

-----------------------------------------------------------------------------
Transformation for case expressions of the form (case e1..en of {..})

> tranCase
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr
>	-> TypeEnv
>	-> DefExpr
>	-> [DefCoreArg]
>	-> DefCaseAlternatives
>	-> [DefCoreArg]
>	-> UniqSM DefExpr

> tranCase sw p t e bs ps as = case e of
>
>	Var (DefArgVar id) ->
>	   	tranVar sw p id
>		   (
>		     tranAlts sw p t ps as	`thenUs` \ps ->
>		     mapArgs (\e -> tran sw p t e []) bs  `thenUs` \bs ->
>		     returnUs
>			  (Case
>		           (mkGenApp (Var (DefArgVar
>			   			  (applyTypeEnvToId t id)))
>			   	  bs)
>			   ps)
>		   )
>		   (
>		     \e ->
>		     tranCase sw p t e bs ps as	`thenUs` \e ->
>		     returnUs
>		       (mkLabel
>		           (mkGenApp
>			      (Case (mkGenApp (Var (DefArgVar id))
>			      		(map (substTyArg t) bs))
>				      ps)
>			      (map (substTyArg t) as))
>			   e)
>		   )
>
>	Lit l ->
>		case bs of
>		  [] -> tranAlts sw p t ps as		`thenUs` \ps ->
>		  	returnUs (Case e ps)
>		  _ -> die_horribly
>
>	Prim op ts es ->
>		case bs of
>		  [] -> tranAlts sw p t ps as 		`thenUs` \ps ->
>			mapUs (tranAtom sw p t) es	`thenUs` \es ->
>			returnUs (Case (Prim op
>					(map (applyTypeEnvToTy t) ts) es) ps)
>		  _ -> die_horribly
>
>	Con c ts es ->
>		case bs of
>		  [] -> case ps of
>			  AlgAlts alts def ->
>				reduceCase sw p c ts es alts def as
>			  PrimAlts alts def -> die_horribly
>		  _ -> die_horribly
>
>	Lam vs e ->
>		case bs of
>			[] -> die_horribly
>			(TypeArg _ : _) -> die_horribly
>			_ -> subst s e		`thenUs` \e ->
>			     tranCase sw p t e rbs ps as
>	   where
>	   	(rvs,rbs,s) = mkSubst vs bs []
>
>	CoTyLam alpha e ->
>		case bs of
>		  TypeArg ty : bs' -> tranCase sw p t e' bs' ps as
>		     where e' = applyTypeEnvToExpr (mkTyVarEnv [(alpha,ty)]) e
>		  _ -> die_horribly
>
>	App e v ->
>		maybeJumbleApp e v	 		`thenUs` \j ->
>		case j of
>			Nothing -> tranCase sw p t e (ValArg v : bs) ps as
>			Just e' -> tranCase sw p t e' bs ps as
>
>	CoTyApp e ty ->
>		tranCase sw p t e (TypeArg (applyTypeEnvToTy t ty) : bs)
>			ps as
>
>	Let (NonRec v e) e' ->
> 		tran sw p t e []			`thenUs` \e  ->
>		if isConstant e then
>			trace "yippee2!!" $
>			subst [(v,removeLabels e)] e'	`thenUs` \e' ->
>			tranCase sw p t e' bs ps as
>		else
>			tranCase sw p t e' bs ps as	`thenUs` \e' ->
>			returnUs (Let (NonRec
>						(applyTypeEnvToId t v) e) e')
>
>	Let (Rec binds) e ->
> 		tranRecBinds sw p t binds e	`thenUs` \(p',resid,e) ->
>		tranCase sw p' t e bs ps as		`thenUs` \e ->
>		returnUs (mkDefLetrec resid e)
>
>	-- ToDo: sort out cost centres.  Currently they act as a barrier
>	-- to optimisation.
>	SCC l e ->
>	   	tran sw p t e []			`thenUs` \e ->
>		mapArgs (\e -> tran sw p t e []) bs
>							`thenUs` \bs ->
>		tranAlts sw p t ps as			`thenUs` \ps ->
>		returnUs (Case (mkGenApp (SCC l e) bs)
>				  ps)
>
>	Case e ps' ->
>		tranCase sw p t e []
>		     (mapAlts (\e -> mkGenApp (Case e ps) bs) ps') as
>
>	_ -> die_horribly
>
>    where die_horribly = defPanic "DefExpr" "tranCase"
>    			(mkGenApp (Case (mkGenApp e bs) ps) as)

-----------------------------------------------------------------------------
Deciding whether or not to replace a function variable with it's
definition.  The tranVar function is passed four arguments: the
environment, the Id itself, the expression to return if no
unfolding takes place, and a function to apply to the unfolded expression
should an unfolding be required.

> tranVar
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr
>	-> Id
>	-> UniqSM DefExpr
>	-> (DefExpr -> UniqSM DefExpr)
>	-> UniqSM DefExpr
>
> tranVar sw p id no_unfold unfold_with =
>
>   case lookupIdEnv p id of
>	Just e' ->
>		rebindExpr e' 	`thenUs` \e' ->
>		if deforestable id
>	   	   then unfold_with e'
>		   else panic "DefExpr(tran): not deforestable id in env"

	No mapping in the environment, but it could be an
	imported function that was annotated with DEFOREST,
	in which case it will have an unfolding inside the Id
	itself.

>	Nothing ->
>	  if (not . deforestable) id
>	  	then  no_unfold
>
>		else case (getIdUnfolding id) of
>			GenForm _ _ expr guidance ->
>			  panic "DefExpr:GenForm has changed a little; needs mod here"
>			  -- SLPJ March 95
>
>--???			  -- ToDo: too much overhead here.
>--???		          let e' = c2d nullIdEnv expr in
>--???			  convertToTreelessForm sw e'	`thenUs` \e'' ->
>--???			  unfold_with e''
>		      	_ -> no_unfold

			   If the unfolding isn't present, this is
 			   a sign that the function is from this module and
 			   is not in the environemnt yet (maybe because
 			   we are transforming the body of the definition
 			   itself).

>			{- panic
>		       		("DefExpr(tran): Deforestable id `"
>		     		++ ppShow 80 (ppr PprDebug id)
>				++ "' doesn't have an unfolding.") -}

-----------------------------------------------------------------------------
Transform a set of case alternatives.

> tranAlts
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr
>	-> TypeEnv
>	-> DefCaseAlternatives
>	-> [DefCoreArg]
>	-> UniqSM DefCaseAlternatives

> tranAlts sw p t (AlgAlts alts def) as =
> 	mapUs (tranAlgAlt sw p t as) alts	`thenUs` \alts ->
>	tranDefault sw p t def as		`thenUs` \def ->
>	returnUs (AlgAlts alts def)
> tranAlts sw p t (PrimAlts alts def) as =
> 	mapUs (tranPrimAlt sw p t as) alts	`thenUs` \alts ->
>	tranDefault sw p t def as		`thenUs` \def ->
>	returnUs (PrimAlts alts def)

> tranAlgAlt sw p t as (c, vs, e) =
> 	tran sw p t e as			`thenUs` \e ->
>	returnUs (c, map (applyTypeEnvToId t) vs, e)
> tranPrimAlt sw p t as (l, e) =
> 	tran sw p t e as			`thenUs` \e ->
>	returnUs (l, e)
>
> tranDefault sw p t NoDefault as = returnUs NoDefault
> tranDefault sw p t (BindDefault v e) as =
> 	tran sw p t e as			`thenUs` \e ->
>	returnUs (BindDefault (applyTypeEnvToId t v) e)

-----------------------------------------------------------------------------
Transform an atom.

> tranAtom
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr
>	-> TypeEnv
>	-> DefAtom
>	-> UniqSM DefAtom

> tranAtom sw p t (VarArg v) =
> 	tranArg sw p t v			`thenUs` \v ->
>	returnUs (VarArg v)
> tranAtom sw p t e@(LitArg l) =	-- XXX
> 	returnUs e

> tranArg sw p t (DefArgExpr e) =
> 	tran sw p t e []			`thenUs` \e ->
>	returnUs (DefArgExpr e)
> tranArg sw p t e@(Label _ _) =
> 	defPanic "DefExpr" "tranArg" (Var e)
> tranArg sw p t (DefArgVar v) =
> 	tran sw p t (Var (DefArgVar v)) []	`thenUs` \e ->
>	returnUs (DefArgExpr e) 	-- XXX remove this case

-----------------------------------------------------------------------------
Translating recursive definition groups.

We first transform each binding, and then seperate the results into
deforestable and non-deforestable sets of bindings.  The deforestable
bindings are processed by the knot-tyer, and added to the current
environment.   The rest of the bindings are returned as residual.

ToDo: conversion to treeless form should be unnecessary here, becuase
the transformer/knot-tyer should leave things in treeless form.

> tranRecBinds sw p t bs e =

Transform all the deforestable definitions, yielding
	(extracted,rhss)
list of extracted functions = concat extracted ok, so let's get the
total set of free variables of the whole function set, call this set
fvs.  Expand the argument list of each function by
    (fvs - freeVars rhs)
and substitute the new function calls throughout the function set.


>	let
>	    (unfold,resid) = partition (deforestable . fst) bs
>	in

> 	mapUs (tranRecBind sw p t) unfold	`thenUs` \unfold ->
> 	mapUs (tranRecBind sw p t) resid	`thenUs` \resid ->

	Tie knots in the deforestable right-hand sides, and convert the
	results to treeless form. Then extract any nested deforestable
	recursive functions, and place everything we've got in the new
	environment.

> 	let (vs,es) = unzip unfold in
>	mapUs mkLoops es			`thenUs` \res ->
>	let
>		(extracted,new_rhss) = unzip res
>		new_binds = zip vs new_rhss ++ concat extracted
>	in

	Convert everything to treeless form (these functions aren't
	necessarily already in treeless form because the functions
	bound in this letrec are about to change status from not
	unfolded to unfolded).

>	mapUs (\(v,e) ->
>		convertToTreelessForm sw e 	`thenUs` \e ->
>		returnUs (v,e)) new_binds	`thenUs` \fs ->

	Now find the total set of free variables of this function set.

>	let
>		fvs = filter (\id -> isArgId id{- && (not . isLitId) id-})
>			(foldr union [] (map freeVars (map snd fs)))
>	in

	Now expand the argument lists to include the total set of free vars.

>	let
>	    stuff          = [ fixupFreeVars fvs id e | (id,e) <- fs ]
>	    fs'	   	   = map fst stuff
>	    s 		   = concat (map snd stuff)
>	    subIt (id,e)   = subst s e `thenUs` \e -> returnUs (id,e)
>	in
>	subst s e				`thenUs` \e  ->
>	mapUs subIt resid			`thenUs` \resid ->
>	mapUs subIt fs'			`thenUs` \fs ->

>	let res = returnUs (growIdEnvList p fs, resid, e) in
>	case unzip fs of
>		(evs,ees) -> mapUs d2c ees `thenUs` \ees ->
>			   let (vs',es') = unzip bs in
>			   mapUs d2c es' `thenUs` \es' ->
>		      trace ("extraction "
>		      		++ showIds (map fst bs)
>		      		++ showIds evs
>				++ "\n{ input:\n" ++ (concat (map showBind (zip vs' es'))) ++ "}\n"
>				++ "{ result:\n" ++ (concat  (map showBind (zip evs ees))) ++ "}\n") res
>		   where showBind (v,e) = ppShow 80 (ppr PprDebug v) ++ "=\n" ++ ppShow 80 (ppr PprDebug e) ++ "\n"

> tranRecBind sw p t (id,e) =
>	tran sw p t e []			`thenUs` \e ->
>	returnUs (applyTypeEnvToId t id,e)

> showIds :: [Id] -> String
> showIds ids = "(" ++ concat (map ((' ' :) . ppShow 80 . ppr PprDebug) ids)
> 	++ " )"

-----------------------------------------------------------------------------

> reduceCase sw p c ts es alts def as =
> 	case [ a | a@(c',vs,e) <- alts, c' == c ] of
>		[(c,vs,e)] ->
>			subst (zip vs (map atom2expr es)) e `thenUs` \e ->
>			tran sw p nullTyVarEnv e as
>		[] -> case def of
>			NoDefault ->
>				panic "DefExpr(reduceCase): no match"
>			BindDefault v e ->
>				subst [(v,Con c ts es)] e `thenUs` \e ->
>				tran sw p nullTyVarEnv e as
>		_ -> panic "DefExpr(reduceCase): multiple matches"

-----------------------------------------------------------------------------
Type Substitutions.

> applyTypeEnvToExpr
> 	:: TypeEnv
> 	-> DefExpr
>	-> DefExpr

> applyTypeEnvToExpr p e = substTy e
>   where
>     substTy e' = case e' of
> 	Var (DefArgExpr e) -> panic "DefExpr(substTy): Var (DefArgExpr _)"
>	Var (Label l e)    -> panic "DefExpr(substTy): Var (Label _ _)"
>       Var (DefArgVar id) -> Var (DefArgVar (applyTypeEnvToId p id))
>       Lit l              -> e'
>       Con c ts es        ->
>		Con c (map (applyTypeEnvToTy p) ts) (map substTyAtom es)
>       Prim op ts es      ->
>		Prim op (map (applyTypeEnvToTy p) ts) (map substTyAtom es)
>       Lam vs e           -> Lam (map (applyTypeEnvToId p) vs) (substTy e)
>       CoTyLam alpha e      -> CoTyLam alpha (substTy e)
>       App e v            -> App (substTy e) (substTyAtom v)
>       CoTyApp e t          -> CoTyApp (substTy e) (applyTypeEnvToTy p t)
>       Case e ps          -> Case (substTy e) (substTyCaseAlts ps)
>       Let (NonRec id e) e' ->
>		Let (NonRec (applyTypeEnvToId p id) (substTy e))
>			(substTy e')
>       Let (Rec bs) e   ->
>		Let (Rec (map substTyRecBind bs)) (substTy e)
>		where substTyRecBind (v,e) = (applyTypeEnvToId p v, substTy e)
>       SCC l e            -> SCC l (substTy e)

>     substTyAtom :: DefAtom -> DefAtom
>     substTyAtom (VarArg v) = VarArg (substTyArg v)
>     substTyAtom (LitArg l) = LitArg l -- XXX

>     substTyArg :: DefBindee -> DefBindee
>     substTyArg (DefArgExpr e) = DefArgExpr (substTy e)
>     substTyArg e@(Label _ _)  = panic "DefExpr(substArg): Label _ _"
>     substTyArg e@(DefArgVar id)  =	-- XXX
>		DefArgVar (applyTypeEnvToId p id)

>     substTyCaseAlts (AlgAlts as def)
> 	= AlgAlts (map substTyAlgAlt as) (substTyDefault def)
>     substTyCaseAlts (PrimAlts as def)
> 	= PrimAlts (map substTyPrimAlt as) (substTyDefault def)

>     substTyAlgAlt  (c, vs, e) = (c, map (applyTypeEnvToId p) vs, substTy e)
>     substTyPrimAlt (l, e) = (l, substTy e)

>     substTyDefault NoDefault = NoDefault
>     substTyDefault (BindDefault id e) =
>     		BindDefault (applyTypeEnvToId p id) (substTy e)

> substTyArg t (ValArg e)   =
> 	ValArg (VarArg (DefArgExpr (applyTypeEnvToExpr t (atom2expr e))))
> substTyArg t (TypeArg ty) = TypeArg ty

-----------------------------------------------------------------------------

> mapAlts f ps = case ps of
>	AlgAlts alts def ->
>	   AlgAlts (map (\(c,vs,e) -> (c,vs,f e)) alts) (mapDef f def)
>	PrimAlts alts def ->
>	   PrimAlts (map (\(l,e) -> (l, f e)) alts) (mapDef f def)
>
> mapDef f NoDefault 		= NoDefault
> mapDef f (BindDefault v e)  = BindDefault v (f e)

-----------------------------------------------------------------------------
Apply a function to all the ValArgs in an Args list.

> mapArgs
> 	:: (DefExpr -> UniqSM DefExpr)
>	-> [DefCoreArg]
>	-> UniqSM [DefCoreArg]
>
> mapArgs f [] =
> 	returnUs []
> mapArgs f (a@(TypeArg ty) : as) =
> 	mapArgs f as			`thenUs` \as ->
>	returnUs (a:as)
> mapArgs f (ValArg v : as) =
> 	f (atom2expr v)			`thenUs` \e ->
>	mapArgs f as			`thenUs` \as ->
>	returnUs (ValArg (VarArg (DefArgExpr e)) : as)
>

> mkSubst [] as s = ([],as,s)
> mkSubst vs [] s = (vs,[],s)
> mkSubst (v:vs) (ValArg e:as) s = mkSubst vs as ((v,atom2expr e):s)

-----------------------------------------------------------------------------

The next function does a bit of extraction for applicative terms
before they are transformed.  We look for boring expressions - those
that won't be any use in removing intermediate data structures.  These
include applicative terms where we cannot unfold the head,
non-reducible case expressions, primitive applications and some let
bindings.

Extracting these expressions helps the knot-tyer to find loops
earlier, and avoids the need to do matching instead of renaming.

We also pull out lets from function arguments, and primitive case
expressions (which can't fail anyway).

Think:

	(t (case u of x -> v))
	====>
	let x = u in t v

Maybe shouldn't do this if -fpedantic-bottoms?  Also can't do it if u
has an unboxed type.

ToDo: sort this mess out - could be more efficient.

> maybeJumbleApp :: DefExpr -> DefAtom -> UniqSM (Maybe DefExpr)
> maybeJumbleApp e (LitArg _) = returnUs Nothing -- ToDo remove
> maybeJumbleApp e (VarArg (DefArgExpr (Var (DefArgVar _))))
> 	= returnUs Nothing
> maybeJumbleApp e (VarArg (DefArgExpr t))
>	= let t' = pull_out t [] in
>	  case t' of
>		Let _ _ -> returnUs (Just t')
>		Case (Prim _ _ _) (PrimAlts [] _) -> returnUs (Just t')
>		_ -> if isBoringExpr t then
>			rebind_with_let t
>		     else
>		     	returnUs Nothing

>	where isBoringExpr (Var (DefArgVar z)) = (not . deforestable) z
>	      isBoringExpr (Prim op ts es) = True
>	      isBoringExpr (Case e ps) = isBoringExpr e
>	      			&& boringCaseAlternatives ps
>	      isBoringExpr (App l r) = isBoringExpr l
>	      isBoringExpr (CoTyApp l t) = isBoringExpr l
>	      isBoringExpr _ = False
>
>	      boringCaseAlternatives (AlgAlts as d) =
>	      	all boringAlgAlt as && boringDefault d
>	      boringCaseAlternatives (PrimAlts as d) =
>	      	all boringPrimAlt as && boringDefault d
>
>	      boringAlgAlt  (c,xs,e) = isBoringExpr e
>	      boringPrimAlt (l,e)    = isBoringExpr e
>
>	      boringDefault NoDefault = True
>	      boringDefault (BindDefault x e) = isBoringExpr e

>	      pull_out (Let b t) as = Let b (pull_out t as)
>	      pull_out (App l r) as = pull_out l (r:as)
>	      pull_out (Case prim@(Prim _ _ _)
>	    		(PrimAlts [] (BindDefault x u))) as
>		= Case prim (PrimAlts [] (BindDefault x
>			(pull_out u as)))
>	      pull_out t as
>	      	= App e (VarArg (DefArgExpr (foldl App t as)))
>
>	      rebind_with_let t =
>			d2c t	`thenUs`  \core_t ->
>			newDefId (coreExprType core_t) `thenUs` \x ->
>			trace "boring epxr found!" $
>			returnUs (Just (Let (NonRec x t)
>				     (App e (VarArg (
>					DefArgExpr (Var (
>					   DefArgVar x)))))))

-----------------------------------------------------------------------------

> isLitId id = case isInstId_maybe id of
>		Just (LitInst _ _ _ _) -> True
>		_ -> False

> isConstant (Con c [] []) = True
> isConstant (Lit l)       = True
> isConstant (Var (Label l e)) = isConstant e
> isConstant _               = False

> removeLabels (Var (Label l e)) = removeLabels e
> removeLabels e = e
