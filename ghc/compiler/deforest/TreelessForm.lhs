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
> import DefUtils

> import CmdLineOpts	( SwitchResult, switchIsOn )
> import CoreUtils	( coreExprType )
> import Id		( replaceIdInfo, getIdInfo )
> import IdInfo
> import Outputable
> import SimplEnv	( SYN_IE(SwitchChecker) )
> import UniqSupply
> import Util

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
>	-> UniqSM DefExpr
>
> convertToTreelessForm sw e
> 	= convExpr e
>
> convExpr
> 	:: DefExpr
>	-> UniqSM DefExpr

> convExpr e = case e of
>
> 	Var (DefArgExpr e) ->
>		panic "TreelessForm(substTy): Var (DefArgExpr _)"
>
>	Var (Label l e) ->
>		panic "TreelessForm(substTy): Var (Label _ _)"
>
>       Var (DefArgVar id) -> returnUs e
>
>       Lit l -> returnUs e
>
>       Con c ts es ->
>		mapUs convAtom es		`thenUs` \es ->
>		returnUs (Con c ts es)
>
>       Prim op ts es ->
>		mapUs convAtom es		`thenUs` \es ->
>		returnUs (Prim op ts es)
>
>       Lam vs e ->
>		convExpr e			`thenUs` \e ->
>		returnUs (Lam vs e)
>
>       CoTyLam alpha e ->
>		convExpr e			`thenUs` \e ->
>		returnUs (CoTyLam alpha e)
>
>       App e v ->
>		convExpr e			`thenUs` \e ->
>		case v of
>		  LitArg l -> returnUs (App e v)
>		  VarArg v' ->
>		    case v' of
>		    	DefArgVar _ -> panic "TreelessForm(convExpr): DefArgVar"
>			DefArgExpr (Var (DefArgVar id))
>				| (not.deforestable) id ->
>					returnUs (App e v)
>			DefArgExpr e' ->
>			   newLet e' (\id -> App e (VarArg
>			   				(DefArgExpr id)))
>
>       CoTyApp e ty ->
>		convExpr e			`thenUs` \e ->
>		returnUs (CoTyApp e ty)
>
>       Case e ps ->
>		convCaseAlts ps			`thenUs` \ps ->
>		case e of
>			Var (DefArgVar id)  | (not.deforestable) id ->
>				returnUs (Case e ps)
>			Prim op ts es -> returnUs (Case e ps)
>			_ -> d2c e		`thenUs` \e' ->
>			     newLet e (\v -> Case v ps)
>
>       Let (NonRec id e) e' ->
>		convExpr e			`thenUs` \e  ->
>		convExpr e'			`thenUs` \e' ->
>		returnUs (Let (NonRec id e) e')
>
>       Let (Rec bs) e ->
>--		convRecBinds bs e		`thenUs` \(bs,e) ->
>--		returnUs (Let (Rec bs) e)
>		convExpr e			`thenUs` \e ->
>		mapUs convRecBind bs		`thenUs` \bs ->
>		returnUs (Let (Rec bs) e)
>	   where
>	   	convRecBind (v,e) =
>			convExpr e		`thenUs` \e ->
>			returnUs (v,e)
>
>       SCC l e ->
>		convExpr e			`thenUs` \e ->
>		returnUs (SCC l e)
>
>	Coerce _ _ _ -> panic "TreelessForm:convExpr:Coerce"

Mark all the recursive functions as deforestable.  Might as well,
since they will be in treeless form anyway.  This helps to cope with
overloaded functions, where the compiler earlier lifts out the
dictionary deconstruction.

> convRecBinds bs e =
> 	convExpr e				`thenUs` \e'   ->
>	mapUs convExpr es			`thenUs` \es'  ->
> 	mapUs (subst s) es'			`thenUs` \es'' ->
>	subst s	e'				`thenUs` \e''  ->
> 	returnUs (zip vs' es', e')
>    where
>	(vs,es) = unzip bs
>	vs'  = map mkDeforestable vs
>	s = zip vs (map (Var . DefArgVar) vs')
>	mkDeforestable v = replaceIdInfo v (addInfo (getIdInfo v) DoDeforest)

> convAtom :: DefAtom -> UniqSM DefAtom
>
> convAtom (VarArg v) =
> 	convArg v				`thenUs` \v ->
> 	returnUs (VarArg v)
> convAtom (LitArg l) =
> 	returnUs (LitArg l)		-- XXX

> convArg :: DefBindee -> UniqSM DefBindee
>
> convArg (DefArgExpr e) =
> 	convExpr e				`thenUs` \e ->
>	returnUs (DefArgExpr e)
> convArg e@(Label _ _)  =
> 	panic "TreelessForm(convArg): Label _ _"
> convArg e@(DefArgVar id)  =
> 	panic "TreelessForm(convArg): DefArgVar _ _"

> convCaseAlts :: DefCaseAlternatives -> UniqSM DefCaseAlternatives
>
> convCaseAlts (AlgAlts as def) =
> 	mapUs convAlgAlt as			`thenUs` \as ->
>	convDefault def				`thenUs` \def ->
>	returnUs (AlgAlts as def)
> convCaseAlts (PrimAlts as def) =
> 	mapUs convPrimAlt as			`thenUs` \as ->
>	convDefault def				`thenUs` \def ->
>	returnUs (PrimAlts as def)

> convAlgAlt  (c, vs, e) =
> 	convExpr e				`thenUs` \e ->
>	returnUs (c, vs, e)
> convPrimAlt (l, e) =
> 	convExpr e				`thenUs` \e ->
>	returnUs (l, e)

> convDefault NoDefault =
> 	returnUs NoDefault
> convDefault (BindDefault id e) =
> 	convExpr e				`thenUs` \e ->
>     	returnUs (BindDefault id e)

> newLet :: DefExpr -> (DefExpr -> DefExpr) -> UniqSM DefExpr
> newLet e body =
> 	d2c e					`thenUs` \core_expr ->
>	newDefId (coreExprType core_expr)	`thenUs` \new_id ->
>	returnUs (Let (NonRec new_id e) (body (Var (DefArgVar new_id))))
