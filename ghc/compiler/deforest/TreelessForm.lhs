%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TreelessForm]{Convert Arbitrary expressions into treeless form}

>#include "HsVersions.h"
>
> module TreelessForm (
> 	convertToTreelessForm
>	) where
> 
> import DefSyn
> import PlainCore
> import DefUtils

> import CoreFuns	( typeOfCoreExpr )
> import IdEnv
> import CmdLineOpts	( SwitchResult, switchIsOn )
> import SplitUniq
> import SimplEnv	( SwitchChecker(..) )
> import Maybes		( Maybe(..) )
> import Id		( replaceIdInfo, getIdInfo )
> import IdInfo
> import Util
> import Outputable


> -- tmp
> import Pretty
> import Def2Core

Very simplistic approach to begin with:

case e of {...}  ====>  let x = e in case x of {...}
x e1 ... en      ====>  let x1 = e1 in ... let xn = en in (x x1 ... xn)

ToDo: make this better.

> convertToTreelessForm
> 	:: SwitchChecker sw
>	-> DefExpr
>	-> SUniqSM DefExpr
>	
> convertToTreelessForm sw e
> 	= convExpr e
>
> convExpr
> 	:: DefExpr
>	-> SUniqSM DefExpr

> convExpr e = case e of
>
> 	CoVar (DefArgExpr e) -> 
>		panic "TreelessForm(substTy): CoVar (DefArgExpr _)"
>		
>	CoVar (Label l e) -> 
>		panic "TreelessForm(substTy): CoVar (Label _ _)"
>		
>       CoVar (DefArgVar id) -> returnSUs e
>	
>       CoLit l -> returnSUs e
>	
>       CoCon c ts es -> 
>		mapSUs convAtom es		`thenSUs` \es ->
>		returnSUs (CoCon c ts es)
>	
>       CoPrim op ts es -> 
>		mapSUs convAtom es		`thenSUs` \es ->
>		returnSUs (CoPrim op ts es)
>		
>       CoLam vs e -> 
>		convExpr e			`thenSUs` \e ->
>		returnSUs (CoLam vs e)
>
>       CoTyLam alpha e -> 
>		convExpr e			`thenSUs` \e ->
>		returnSUs (CoTyLam alpha e)
>
>       CoApp e v -> 
>		convExpr e			`thenSUs` \e ->
>		case v of
>		  CoLitAtom l -> returnSUs (CoApp e v)
>		  CoVarAtom v' ->
>		    case v' of
>		    	DefArgVar _ -> panic "TreelessForm(convExpr): DefArgVar"
>			DefArgExpr (CoVar (DefArgVar id)) 
>				| (not.deforestable) id -> 
>					returnSUs (CoApp e v)
>			DefArgExpr e' -> 
>			   newLet e' (\id -> CoApp e (CoVarAtom 
>			   				(DefArgExpr id)))
>						
>       CoTyApp e ty -> 
>		convExpr e			`thenSUs` \e ->
>		returnSUs (CoTyApp e ty)
>		
>       CoCase e ps -> 
>		convCaseAlts ps			`thenSUs` \ps ->
>		case e of 
>			CoVar (DefArgVar id)  | (not.deforestable) id ->
>				returnSUs (CoCase e ps)
>			CoPrim op ts es -> returnSUs (CoCase e ps) 
>			_ -> d2c e		`thenSUs` \e' ->
>			     newLet e (\v -> CoCase v ps)
>
>       CoLet (CoNonRec id e) e' -> 
>		convExpr e			`thenSUs` \e  ->
>		convExpr e'			`thenSUs` \e' ->
>		returnSUs (CoLet (CoNonRec id e) e')
>		
>       CoLet (CoRec bs) e -> 
>--		convRecBinds bs e		`thenSUs` \(bs,e) ->
>--		returnSUs (CoLet (CoRec bs) e)
>		convExpr e			`thenSUs` \e ->
>		mapSUs convRecBind bs		`thenSUs` \bs ->
>		returnSUs (CoLet (CoRec bs) e)
>	   where
>	   	convRecBind (v,e) = 
>			convExpr e		`thenSUs` \e ->
>			returnSUs (v,e)
>			
>       CoSCC l e ->
>		convExpr e			`thenSUs` \e ->
>		returnSUs (CoSCC l e)

Mark all the recursive functions as deforestable.  Might as well,
since they will be in treeless form anyway.  This helps to cope with
overloaded functions, where the compiler earlier lifts out the
dictionary deconstruction.

> convRecBinds bs e =
> 	convExpr e				`thenSUs` \e'   ->
>	mapSUs convExpr es			`thenSUs` \es'  ->
> 	mapSUs (subst s) es'			`thenSUs` \es'' ->
>	subst s	e'				`thenSUs` \e''  ->
> 	returnSUs (zip vs' es', e')
>    where
>	(vs,es) = unzip bs
>	vs'  = map mkDeforestable vs
>	s = zip vs (map (CoVar . DefArgVar) vs')
>	mkDeforestable v = replaceIdInfo v (addInfo (getIdInfo v) DoDeforest)

> convAtom :: DefAtom -> SUniqSM DefAtom
> 
> convAtom (CoVarAtom v) = 
> 	convArg v				`thenSUs` \v ->
> 	returnSUs (CoVarAtom v)
> convAtom (CoLitAtom l) =
> 	returnSUs (CoLitAtom l)		-- XXX

> convArg :: DefBindee -> SUniqSM DefBindee
> 
> convArg (DefArgExpr e) =
> 	convExpr e				`thenSUs` \e ->
>	returnSUs (DefArgExpr e)
> convArg e@(Label _ _)  = 
> 	panic "TreelessForm(convArg): Label _ _"
> convArg e@(DefArgVar id)  =
> 	panic "TreelessForm(convArg): DefArgVar _ _"

> convCaseAlts :: DefCaseAlternatives -> SUniqSM DefCaseAlternatives
> 
> convCaseAlts (CoAlgAlts as def) =
> 	mapSUs convAlgAlt as			`thenSUs` \as ->
>	convDefault def				`thenSUs` \def ->
>	returnSUs (CoAlgAlts as def)
> convCaseAlts (CoPrimAlts as def) =
> 	mapSUs convPrimAlt as			`thenSUs` \as ->
>	convDefault def				`thenSUs` \def ->
>	returnSUs (CoPrimAlts as def)

> convAlgAlt  (c, vs, e) = 
> 	convExpr e				`thenSUs` \e ->
>	returnSUs (c, vs, e)
> convPrimAlt (l, e) = 
> 	convExpr e				`thenSUs` \e ->
>	returnSUs (l, e)

> convDefault CoNoDefault = 
> 	returnSUs CoNoDefault
> convDefault (CoBindDefault id e) = 
> 	convExpr e				`thenSUs` \e ->
>     	returnSUs (CoBindDefault id e)

> newLet :: DefExpr -> (DefExpr -> DefExpr) -> SUniqSM DefExpr
> newLet e body = 
> 	d2c e					`thenSUs` \core_expr ->
>	newDefId (typeOfCoreExpr core_expr)	`thenSUs` \new_id ->
>	returnSUs (CoLet (CoNonRec new_id e) (body (CoVar (DefArgVar new_id))))
