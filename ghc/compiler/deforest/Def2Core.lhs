%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Def2Core]{Translate a DefProgram back into a CoreProgram}

>#include "HsVersions.h"
>
> module Def2Core (
> 	def2core, d2c,
>
>	-- and to make the interface self-sufficient, all this stuff:
>	DefBinding(..), SYN_IE(UniqSM),
>	GenCoreBinding, Id, DefBindee,
>	defPanic
>	) where

> import DefSyn
> import DefUtils
>
> import Outputable
> import Pretty
> import UniqSupply
> import Util


> def2core :: DefProgram -> UniqSM [CoreBinding]
> def2core prog = mapUs defBinding2core prog

> defBinding2core :: DefBinding -> UniqSM CoreBinding
> defBinding2core (NonRec v e) =
> 	d2c e `thenUs` \e' ->
>	returnUs (NonRec v e')
> defBinding2core (Rec bs) =
> 	mapUs recBind2core bs `thenUs` \bs' ->
>	returnUs (Rec bs')
>		where recBind2core (v,e)
>			= d2c e `thenUs` \e' ->
>			  returnUs (v, e')


> defAtom2core :: DefAtom -> UniqSM (CoreArg, Maybe CoreExpr)
> defAtom2core atom = case atom of
> 	LitArg l -> returnUs (LitArg l, Nothing)
> 	VarArg (DefArgVar id) -> returnUs (VarArg id, Nothing)
>	VarArg (DefArgExpr (Var (DefArgVar id))) ->
>		returnUs (VarArg id, Nothing)
>	VarArg (DefArgExpr (Lit l)) ->
>		returnUs (LitArg l, Nothing)
> 	VarArg (DefArgExpr e) ->
>		d2c e		`thenUs` \e' ->
>		newTmpId (coreExprType e')	`thenUs` \new_id ->
>		returnUs (VarArg new_id, Just e')
>	VarArg (Label _ _) ->
>		panic "Def2Core(defAtom2core): VarArg (Label _ _)"

> d2c :: DefExpr -> UniqSM CoreExpr
> d2c e = case e of
>
>	Var (DefArgExpr e) ->
>		panic "Def2Core(d2c): Var (DefArgExpr _)"
>
>	Var (Label _ _) ->
>		panic "Def2Core(d2c): Var (Label _ _)"
>
>	Var (DefArgVar v) ->
>		returnUs (Var v)
>
>       Lit l ->
>		returnUs (Lit l)
>
>       Con c ts as ->
>		mapUs defAtom2core as	`thenUs` \atom_expr_pairs ->
>		returnUs (
>			foldr (\(a,b) -> mkLet a b)
>				(Con c ts (map fst atom_expr_pairs))
>				atom_expr_pairs)
>
>       Prim op ts as ->
>		mapUs defAtom2core as	`thenUs` \atom_expr_pairs ->
>		returnUs (
>			foldr (\(a,b) -> mkLet a b)
>				(Prim op ts (map fst atom_expr_pairs))
>				atom_expr_pairs)
>
>       Lam vs e ->
>		d2c e			`thenUs` \e' ->
>		returnUs (Lam vs e')
>
>       CoTyLam alpha e ->
>		d2c e			`thenUs` \e' ->
>		returnUs (CoTyLam alpha e')
>
>       App e v       ->
>		d2c e			`thenUs` \e' ->
>		defAtom2core v		`thenUs` \(v',e'') ->
>		returnUs (mkLet v' e'' (App e' v'))
>
>       CoTyApp e t     ->
>		d2c e			`thenUs` \e' ->
>		returnUs (CoTyApp e' t)
>
>       Case e ps ->
>		d2c e			`thenUs` \e' ->
>		defCaseAlts2Core ps	`thenUs` \ps' ->
>		returnUs (Case e' ps')
>
>	Let b e ->
>		d2c e			`thenUs` \e' ->
>		defBinding2core b	`thenUs` \b' ->
>		returnUs (Let b' e')
>
>       SCC l e ->
>		d2c e			`thenUs` \e' ->
>		returnUs (SCC l e')
>	Coerce _ _ _ ->
>		panic "Def2Core:Coerce"

> defCaseAlts2Core :: DefCaseAlternatives
> 	-> UniqSM CoreCaseAlts
>
> defCaseAlts2Core alts = case alts of
> 	AlgAlts alts dflt ->
>		mapUs algAlt2Core alts	`thenUs` \alts' ->
>		defAlt2Core dflt	`thenUs` \dflt' ->
>		returnUs (AlgAlts alts' dflt')
>
>	PrimAlts alts dflt ->
>		mapUs primAlt2Core alts `thenUs` \alts' ->
>		defAlt2Core dflt	 `thenUs` \dflt' ->
>		returnUs (PrimAlts alts' dflt')
>
>  where
>
>	algAlt2Core (c, vs, e)	= d2c e `thenUs` \e' -> returnUs (c, vs, e')
>	primAlt2Core (l, e)	= d2c e `thenUs` \e' -> returnUs (l, e')
>
>	defAlt2Core NoDefault = returnUs NoDefault
>	defAlt2Core (BindDefault v e) =
>		d2c e `thenUs` \e' ->
>		returnUs (BindDefault v e')

> mkLet :: CoreArg
> 	-> Maybe CoreExpr
>	-> CoreExpr
>	-> CoreExpr
>
> mkLet (VarArg v) (Just e) e' = Let (NonRec v e) e'
> mkLet v Nothing  e' = e'

-----------------------------------------------------------------------------
XXX - in here becuase if it goes in DefUtils we've got mutual recursion.

> defPanic :: String -> String -> DefExpr -> UniqSM a
> defPanic modl fun expr =
> 	d2c expr	`thenUs` \expr ->
> 	panic (modl ++ "(" ++ fun ++ "): " ++ ppShow 80 (ppr PprDebug expr))
