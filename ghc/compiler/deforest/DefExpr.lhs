%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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

> import AbsUniType	( applyTypeEnvToTy, isPrimType,
>			  SigmaType(..), UniType
>			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
>			)
> import CmdLineOpts	( SwitchResult, switchIsOn )
> import CoreFuns	( mkCoLam, unTagBinders, typeOfCoreExpr )
> import Id		( applyTypeEnvToId, getIdUnfolding, isTopLevId, Id,
> 			  isInstId_maybe
>			)
> import Inst		-- Inst(..)
> import IdEnv
> import IdInfo
> import Maybes		( Maybe(..) )
> import Outputable
> import SimplEnv	( SwitchChecker(..), UnfoldingDetails(..) )
> import SplitUniq
> import TyVarEnv
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
>	-> SUniqSM DefExpr

> tran sw p t e@(CoVar (DefArgVar id)) as =
> 	tranVar sw p id
>    		(
>		 mapArgs (\e -> tran sw p t e []) as  `thenSUs` \as ->
>		 returnSUs (applyToArgs (CoVar (DefArgVar new_id)) as)
>		)
>		(
>		 \e -> 
>		   tran sw p t e as	`thenSUs` \e ->
>		   returnSUs (mkLabel (applyToArgs (CoVar (DefArgVar new_id)) 
>		   			(map (substTyArg t) as)) 
>				      e)
>		)
>	where new_id = applyTypeEnvToId t id

> tran sw p t e@(CoLit l) [] =
> 	returnSUs e
>	
> tran sw p t (CoCon c ts es) [] =
> 	mapSUs (tranAtom sw p t) es 		`thenSUs` \es ->
>	returnSUs (CoCon c (map (applyTypeEnvToTy t) ts) es)
>	
> tran sw p t (CoPrim op ts es) [] =	-- XXX constant folding?
> 	mapSUs (tranAtom sw p t) es	`thenSUs` \es ->
>	returnSUs (CoPrim op (map (applyTypeEnvToTy t) ts) es)
>
> tran sw p t (CoLam vs e) [] =
> 	tran sw p t e []			`thenSUs` \e ->
>	returnSUs (mkCoLam (map (applyTypeEnvToId t) vs) e)
>
> tran sw p t (CoLam vs e) as =
> 	subst s e				`thenSUs` \e ->
> 	tran sw p t (mkCoLam rvs e) ras
>   where
>   	(rvs,ras,s) = mkSubst vs as []

> tran sw p t (CoTyLam alpha e) [] =
> 	tran sw p t e []			`thenSUs` \e ->
>	returnSUs (CoTyLam alpha e)
>

	ToDo: use the environment rather than doing explicit substitution
	(didn't work last time I tried :)

> tran sw p t (CoTyLam alpha e) (TypeArg ty : as) =
> 	tran sw p t (applyTypeEnvToExpr (mkTyVarEnv [(alpha,ty)]) e) as

> tran sw p t (CoApp e v) as =
> 	maybeJumbleApp e v			`thenSUs` \j ->
>	case j of
>		Nothing -> tran sw p t e (ValArg v : as)
>		Just e' -> tran sw p t e' as
>
> tran sw p t (CoTyApp e ty) as =
>	tran sw p t e (TypeArg (applyTypeEnvToTy t ty) : as)
>
> tran sw p t (CoLet (CoNonRec v e) e') as =
> 	tran sw p t e []			`thenSUs` \e  ->
>	if isConstant e then
>		trace "yippee!!" $
>		subst [(v,removeLabels e)] e'		`thenSUs` \e' ->
>		tran sw p t e' as
>	else
>		tran sw p t e' as		`thenSUs` \e' ->
>		returnSUs (CoLet (CoNonRec (applyTypeEnvToId t v) e) e')
>
> tran sw p t (CoLet (CoRec bs) e) as =
> 	tranRecBinds sw p t bs e		`thenSUs` \(p',resid,e) ->
>	tran sw p' t e as			`thenSUs` \e ->
>	returnSUs (mkDefLetrec resid e)
>	
> tran sw p t (CoSCC l e) as =
> 	tran sw p t e []			`thenSUs` \e ->
>	mapArgs (\e -> tran sw p t e []) as	`thenSUs` \as ->
>	returnSUs (applyToArgs (CoSCC l e) as)
>	
> tran sw p t (CoCase e ps) as =
> 	tranCase sw p t e [] ps as
>	
> tran _ _ _ e as = 
> 	defPanic "DefExpr" "tran" (applyToArgs e as)

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
>	-> SUniqSM DefExpr

> tranCase sw p t e bs ps as = case e of
>	
>	CoVar (DefArgVar id) ->
>	   	tranVar sw p id
>		   (
>		     tranAlts sw p t ps as	`thenSUs` \ps ->
>		     mapArgs (\e -> tran sw p t e []) bs  `thenSUs` \bs ->
>		     returnSUs 
>			  (CoCase 
>		           (applyToArgs (CoVar (DefArgVar 
>			   			  (applyTypeEnvToId t id))) 
>			   	  bs)
>			   ps)
>		   )
>		   (
>		     \e ->
>		     tranCase sw p t e bs ps as	`thenSUs` \e ->
>		     returnSUs 
>		       (mkLabel 
>		           (applyToArgs 
>			      (CoCase (applyToArgs (CoVar (DefArgVar id)) 
>			      		(map (substTyArg t) bs))
>				      ps)
>			      (map (substTyArg t) as))
>			   e)
>		   )
>
>	CoLit l ->
>		case bs of
>		  [] -> tranAlts sw p t ps as		`thenSUs` \ps ->
>		  	returnSUs (CoCase e ps)
>		  _ -> die_horribly
>		
>	CoPrim op ts es -> 
>		case bs of
>		  [] -> tranAlts sw p t ps as 		`thenSUs` \ps ->
>			mapSUs (tranAtom sw p t) es	`thenSUs` \es ->
>			returnSUs (CoCase (CoPrim op 
>					(map (applyTypeEnvToTy t) ts) es) ps)
>		  _ -> die_horribly
>		  
>	CoCon c ts es ->
>		case bs of
>		  [] -> case ps of
>			  CoAlgAlts alts def -> 
>				reduceCase sw p c ts es alts def as
>			  CoPrimAlts alts def -> die_horribly
>		  _ -> die_horribly
>	
>	CoLam vs e ->
>		case bs of
>			[] -> die_horribly
>			(TypeArg _ : _) -> die_horribly
>			_ -> subst s e		`thenSUs` \e ->
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
>	CoApp e v ->
>		maybeJumbleApp e v	 		`thenSUs` \j ->
>		case j of
>			Nothing -> tranCase sw p t e (ValArg v : bs) ps as
>			Just e' -> tranCase sw p t e' bs ps as
>		
>	CoTyApp e ty ->
>		tranCase sw p t e (TypeArg (applyTypeEnvToTy t ty) : bs)
>			ps as
>	
>	CoLet (CoNonRec v e) e' ->
> 		tran sw p t e []			`thenSUs` \e  ->
>		if isConstant e then
>			trace "yippee2!!" $
>			subst [(v,removeLabels e)] e'	`thenSUs` \e' ->
>			tranCase sw p t e' bs ps as
>		else
>			tranCase sw p t e' bs ps as	`thenSUs` \e' ->
>			returnSUs (CoLet (CoNonRec 
>						(applyTypeEnvToId t v) e) e')
>
>	CoLet (CoRec binds) e ->
> 		tranRecBinds sw p t binds e	`thenSUs` \(p',resid,e) ->
>		tranCase sw p' t e bs ps as		`thenSUs` \e ->
>		returnSUs (mkDefLetrec resid e)
>		
>	-- ToDo: sort out cost centres.  Currently they act as a barrier
>	-- to optimisation.
>	CoSCC l e ->
>	   	tran sw p t e []			`thenSUs` \e ->
>		mapArgs (\e -> tran sw p t e []) bs
>							`thenSUs` \bs ->
>		tranAlts sw p t ps as			`thenSUs` \ps ->
>		returnSUs (CoCase (applyToArgs (CoSCC l e) bs)
>				  ps)
>		
>	CoCase e ps' ->
>		tranCase sw p t e []
>		     (mapAlts (\e -> applyToArgs (CoCase e ps) bs) ps') as
>		
>	_ -> die_horribly
>	
>    where die_horribly = defPanic "DefExpr" "tranCase" 
>    			(applyToArgs (CoCase (applyToArgs e bs) ps) as)

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
>	-> SUniqSM DefExpr
>	-> (DefExpr -> SUniqSM DefExpr)
>	-> SUniqSM DefExpr
>	
> tranVar sw p id no_unfold unfold_with =
> 	
>   case lookupIdEnv p id of
>	Just e' ->
>		rebindExpr e' 	`thenSUs` \e' ->
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
>			GeneralForm _ _ expr guidance ->
>			  panic "DefExpr:GeneralForm has changed a little; needs mod here"
>			  -- SLPJ March 95
>
>--???			  -- ToDo: too much overhead here.
>--???		          let e' = c2d nullIdEnv expr in
>--???			  convertToTreelessForm sw e'	`thenSUs` \e'' ->
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
>	-> SUniqSM DefCaseAlternatives

> tranAlts sw p t (CoAlgAlts alts def) as =
> 	mapSUs (tranAlgAlt sw p t as) alts	`thenSUs` \alts ->
>	tranDefault sw p t def as		`thenSUs` \def ->
>	returnSUs (CoAlgAlts alts def)
> tranAlts sw p t (CoPrimAlts alts def) as =
> 	mapSUs (tranPrimAlt sw p t as) alts	`thenSUs` \alts ->
>	tranDefault sw p t def as		`thenSUs` \def ->
>	returnSUs (CoPrimAlts alts def)

> tranAlgAlt sw p t as (c, vs, e) =
> 	tran sw p t e as			`thenSUs` \e ->
>	returnSUs (c, map (applyTypeEnvToId t) vs, e)
> tranPrimAlt sw p t as (l, e) =
> 	tran sw p t e as			`thenSUs` \e ->
>	returnSUs (l, e)
>	
> tranDefault sw p t CoNoDefault as = returnSUs CoNoDefault
> tranDefault sw p t (CoBindDefault v e) as =
> 	tran sw p t e as			`thenSUs` \e ->
>	returnSUs (CoBindDefault (applyTypeEnvToId t v) e)

-----------------------------------------------------------------------------
Transform an atom.

> tranAtom 
> 	:: SwitchChecker who_knows
>	-> IdEnv DefExpr 
>	-> TypeEnv 
>	-> DefAtom 
>	-> SUniqSM DefAtom

> tranAtom sw p t (CoVarAtom v) =
> 	tranArg sw p t v			`thenSUs` \v ->
>	returnSUs (CoVarAtom v)
> tranAtom sw p t e@(CoLitAtom l) =	-- XXX
> 	returnSUs e

> tranArg sw p t (DefArgExpr e) =
> 	tran sw p t e []			`thenSUs` \e ->
>	returnSUs (DefArgExpr e)
> tranArg sw p t e@(Label _ _) =
> 	defPanic "DefExpr" "tranArg" (CoVar e)
> tranArg sw p t (DefArgVar v) =
> 	tran sw p t (CoVar (DefArgVar v)) []	`thenSUs` \e -> 
>	returnSUs (DefArgExpr e) 	-- XXX remove this case

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

> 	mapSUs (tranRecBind sw p t) unfold	`thenSUs` \unfold ->
> 	mapSUs (tranRecBind sw p t) resid	`thenSUs` \resid ->

	Tie knots in the deforestable right-hand sides, and convert the 
	results to treeless form. Then extract any nested deforestable 
	recursive functions, and place everything we've got in the new 
	environment.

> 	let (vs,es) = unzip unfold in
>	mapSUs mkLoops es			`thenSUs` \res ->
>	let 
>		(extracted,new_rhss) = unzip res
>		new_binds = zip vs new_rhss ++ concat extracted
>	in

	Convert everything to treeless form (these functions aren't
	necessarily already in treeless form because the functions
	bound in this letrec are about to change status from not
	unfolded to unfolded).

>	mapSUs (\(v,e) -> 
>		convertToTreelessForm sw e 	`thenSUs` \e ->
>		returnSUs (v,e)) new_binds	`thenSUs` \fs ->

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
>	    subIt (id,e)   = subst s e `thenSUs` \e -> returnSUs (id,e)
>	in
>	subst s e				`thenSUs` \e  ->
>	mapSUs subIt resid			`thenSUs` \resid ->
>	mapSUs subIt fs'			`thenSUs` \fs ->

>	let res = returnSUs (growIdEnvList p fs, resid, e) in
>	case unzip fs of
>		(evs,ees) -> mapSUs d2c ees `thenSUs` \ees ->
>			   let (vs',es') = unzip bs in
>			   mapSUs d2c es' `thenSUs` \es' ->
>		      trace ("extraction " 
>		      		++ showIds (map fst bs) 
>		      		++ showIds evs
>				++ "\n{ input:\n" ++ (concat (map showBind (zip vs' es'))) ++ "}\n"
>				++ "{ result:\n" ++ (concat  (map showBind (zip evs ees))) ++ "}\n") res
>		   where showBind (v,e) = ppShow 80 (ppr PprDebug v) ++ "=\n" ++ ppShow 80 (ppr PprDebug e) ++ "\n"

> tranRecBind sw p t (id,e) =
>	tran sw p t e []			`thenSUs` \e ->
>	returnSUs (applyTypeEnvToId t id,e)

> showIds :: [Id] -> String
> showIds ids = "(" ++ concat (map ((' ' :) . ppShow 80 . ppr PprDebug) ids) 
> 	++ " )"

-----------------------------------------------------------------------------

> reduceCase sw p c ts es alts def as = 
> 	case [ a | a@(c',vs,e) <- alts, c' == c ] of
>		[(c,vs,e)] ->
>			subst (zip vs (map atom2expr es)) e `thenSUs` \e ->
>			tran sw p nullTyVarEnv e as
>		[] -> case def of
>			CoNoDefault -> 
>				panic "DefExpr(reduceCase): no match"
>			CoBindDefault v e ->
>				subst [(v,CoCon c ts es)] e `thenSUs` \e ->
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
> 	CoVar (DefArgExpr e) -> panic "DefExpr(substTy): CoVar (DefArgExpr _)"
>	CoVar (Label l e)    -> panic "DefExpr(substTy): CoVar (Label _ _)"
>       CoVar (DefArgVar id) -> CoVar (DefArgVar (applyTypeEnvToId p id))
>       CoLit l              -> e'
>       CoCon c ts es        -> 
>		CoCon c (map (applyTypeEnvToTy p) ts) (map substTyAtom es)
>       CoPrim op ts es      -> 
>		CoPrim op (map (applyTypeEnvToTy p) ts) (map substTyAtom es)
>       CoLam vs e           -> CoLam (map (applyTypeEnvToId p) vs) (substTy e)
>       CoTyLam alpha e      -> CoTyLam alpha (substTy e)
>       CoApp e v            -> CoApp (substTy e) (substTyAtom v)
>       CoTyApp e t          -> mkCoTyApp (substTy e) (applyTypeEnvToTy p t)
>       CoCase e ps          -> CoCase (substTy e) (substTyCaseAlts ps)
>       CoLet (CoNonRec id e) e' -> 
>		CoLet (CoNonRec (applyTypeEnvToId p id) (substTy e)) 
>			(substTy e')
>       CoLet (CoRec bs) e   -> 
>		CoLet (CoRec (map substTyRecBind bs)) (substTy e)
>		where substTyRecBind (v,e) = (applyTypeEnvToId p v, substTy e)
>       CoSCC l e            -> CoSCC l (substTy e)

>     substTyAtom :: DefAtom -> DefAtom
>     substTyAtom (CoVarAtom v) = CoVarAtom (substTyArg v)
>     substTyAtom (CoLitAtom l) = CoLitAtom l -- XXX

>     substTyArg :: DefBindee -> DefBindee
>     substTyArg (DefArgExpr e) = DefArgExpr (substTy e)
>     substTyArg e@(Label _ _)  = panic "DefExpr(substArg): Label _ _"
>     substTyArg e@(DefArgVar id)  =	-- XXX
>		DefArgVar (applyTypeEnvToId p id)

>     substTyCaseAlts (CoAlgAlts as def) 
> 	= CoAlgAlts (map substTyAlgAlt as) (substTyDefault def)
>     substTyCaseAlts (CoPrimAlts as def) 
> 	= CoPrimAlts (map substTyPrimAlt as) (substTyDefault def)

>     substTyAlgAlt  (c, vs, e) = (c, map (applyTypeEnvToId p) vs, substTy e)
>     substTyPrimAlt (l, e) = (l, substTy e)

>     substTyDefault CoNoDefault = CoNoDefault
>     substTyDefault (CoBindDefault id e) = 
>     		CoBindDefault (applyTypeEnvToId p id) (substTy e)

> substTyArg t (ValArg e)   = 
> 	ValArg (CoVarAtom (DefArgExpr (applyTypeEnvToExpr t (atom2expr e))))
> substTyArg t (TypeArg ty) = TypeArg ty

-----------------------------------------------------------------------------

> mapAlts f ps = case ps of
>	CoAlgAlts alts def -> 
>	   CoAlgAlts (map (\(c,vs,e) -> (c,vs,f e)) alts) (mapDef f def)
>	CoPrimAlts alts def ->
>	   CoPrimAlts (map (\(l,e) -> (l, f e)) alts) (mapDef f def)
>				
> mapDef f CoNoDefault 		= CoNoDefault
> mapDef f (CoBindDefault v e)  = CoBindDefault v (f e)

-----------------------------------------------------------------------------
Apply a function to all the ValArgs in an Args list.

> mapArgs 
> 	:: (DefExpr -> SUniqSM DefExpr) 
>	-> [DefCoreArg] 
>	-> SUniqSM [DefCoreArg]
>	
> mapArgs f [] = 
> 	returnSUs []
> mapArgs f (a@(TypeArg ty) : as) = 
> 	mapArgs f as			`thenSUs` \as ->
>	returnSUs (a:as)
> mapArgs f (ValArg v : as) =
> 	f (atom2expr v)			`thenSUs` \e ->
>	mapArgs f as			`thenSUs` \as ->
>	returnSUs (ValArg (CoVarAtom (DefArgExpr e)) : as)
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

> maybeJumbleApp :: DefExpr -> DefAtom -> SUniqSM (Maybe DefExpr)
> maybeJumbleApp e (CoLitAtom _) = returnSUs Nothing -- ToDo remove
> maybeJumbleApp e (CoVarAtom (DefArgExpr (CoVar (DefArgVar _))))
> 	= returnSUs Nothing
> maybeJumbleApp e (CoVarAtom (DefArgExpr t))
>	= let t' = pull_out t [] in
>	  case t' of
>		CoLet _ _ -> returnSUs (Just t')
>		CoCase (CoPrim _ _ _) (CoPrimAlts [] _) -> returnSUs (Just t')
>		_ -> if isBoringExpr t then
>			rebind_with_let t
>		     else
>		     	returnSUs Nothing

>	where isBoringExpr (CoVar (DefArgVar z)) = (not . deforestable) z
>	      isBoringExpr (CoPrim op ts es) = True
>	      isBoringExpr (CoCase e ps) = isBoringExpr e 
>	      			&& boringCaseAlternatives ps
>	      isBoringExpr (CoApp l r) = isBoringExpr l
>	      isBoringExpr (CoTyApp l t) = isBoringExpr l
>	      isBoringExpr _ = False
>
>	      boringCaseAlternatives (CoAlgAlts as d) =
>	      	all boringAlgAlt as && boringDefault d
>	      boringCaseAlternatives (CoPrimAlts as d) =
>	      	all boringPrimAlt as && boringDefault d
>		
>	      boringAlgAlt  (c,xs,e) = isBoringExpr e
>	      boringPrimAlt (l,e)    = isBoringExpr e
>	      
>	      boringDefault CoNoDefault = True
>	      boringDefault (CoBindDefault x e) = isBoringExpr e

>	      pull_out (CoLet b t) as = CoLet b (pull_out t as)
>	      pull_out (CoApp l r) as = pull_out l (r:as)
>	      pull_out (CoCase prim@(CoPrim _ _ _) 
>	    		(CoPrimAlts [] (CoBindDefault x u))) as
>		= CoCase prim (CoPrimAlts [] (CoBindDefault x 
>			(pull_out u as)))
>	      pull_out t as 
>	      	= CoApp e (CoVarAtom (DefArgExpr (foldl CoApp t as)))
>	      
>	      rebind_with_let t = 
>			d2c t	`thenSUs`  \core_t ->
>			newDefId (typeOfCoreExpr core_t) `thenSUs` \x ->
>			trace "boring epxr found!" $
>			returnSUs (Just (CoLet (CoNonRec x t)
>				     (CoApp e (CoVarAtom (
>					DefArgExpr (CoVar (
>					   DefArgVar x)))))))

-----------------------------------------------------------------------------

> isLitId id = case isInstId_maybe id of
>		Just (LitInst _ _ _ _) -> True
>		_ -> False

> isConstant (CoCon c [] []) = True
> isConstant (CoLit l)       = True
> isConstant (CoVar (Label l e)) = isConstant e
> isConstant _               = False

> removeLabels (CoVar (Label l e)) = removeLabels e
> removeLabels e = e
