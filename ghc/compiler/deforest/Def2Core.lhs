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
>	DefBinding(..), SUniqSM(..), PlainCoreProgram(..),
>	CoreBinding, Id, DefBindee,
>	defPanic
>	) where

> import DefSyn
> import DefUtils
> 
> import Maybes		( Maybe(..) )
> import Outputable
> import PlainCore
> import Pretty
> import SplitUniq
> import Util


> def2core :: DefProgram -> SUniqSM PlainCoreProgram
> def2core prog = mapSUs defBinding2core prog

> defBinding2core :: DefBinding -> SUniqSM PlainCoreBinding
> defBinding2core (CoNonRec v e) = 
> 	d2c e `thenSUs` \e' -> 
>	returnSUs (CoNonRec v e')
> defBinding2core (CoRec bs) = 
> 	mapSUs recBind2core bs `thenSUs` \bs' ->
>	returnSUs (CoRec bs')
>		where recBind2core (v,e) 
>			= d2c e `thenSUs` \e' -> 
>			  returnSUs (v, e')


> defAtom2core :: DefAtom -> SUniqSM (PlainCoreAtom, Maybe PlainCoreExpr)
> defAtom2core atom = case atom of
> 	CoLitAtom l -> returnSUs (CoLitAtom l, Nothing)
> 	CoVarAtom (DefArgVar id) -> returnSUs (CoVarAtom id, Nothing)
>	CoVarAtom (DefArgExpr (CoVar (DefArgVar id))) ->
>		returnSUs (CoVarAtom id, Nothing)
>	CoVarAtom (DefArgExpr (CoLit l)) ->
>		returnSUs (CoLitAtom l, Nothing)
> 	CoVarAtom (DefArgExpr e) -> 
>		d2c e		`thenSUs` \e' ->
>		newTmpId (typeOfCoreExpr e')	`thenSUs` \new_id ->
>		returnSUs (CoVarAtom new_id, Just e')
>	CoVarAtom (Label _ _) -> 
>		panic "Def2Core(defAtom2core): CoVarAtom (Label _ _)"

> d2c :: DefExpr -> SUniqSM PlainCoreExpr
> d2c e = case e of
> 
>	CoVar (DefArgExpr e) ->
>		panic "Def2Core(d2c): CoVar (DefArgExpr _)"
>		
>	CoVar (Label _ _) ->
>		panic "Def2Core(d2c): CoVar (Label _ _)"
>		
>	CoVar (DefArgVar v) ->
>		returnSUs (CoVar v)
>	
>       CoLit l -> 
>		returnSUs (CoLit l)
>	
>       CoCon c ts as -> 
>		mapSUs defAtom2core as	`thenSUs` \atom_expr_pairs ->
>		returnSUs (
>			foldr (\(a,b) -> mkLet a b) 
>				(CoCon c ts (map fst atom_expr_pairs))
>				atom_expr_pairs)
>			   
>       CoPrim op ts as -> 
>		mapSUs defAtom2core as	`thenSUs` \atom_expr_pairs ->
>		returnSUs (
>			foldr (\(a,b) -> mkLet a b)
>				(CoPrim op ts (map fst atom_expr_pairs))
>				atom_expr_pairs)
>			   
>       CoLam vs e -> 
>		d2c e			`thenSUs` \e' ->
>		returnSUs (CoLam vs e')
>		
>       CoTyLam alpha e -> 
>		d2c e			`thenSUs` \e' ->
>		returnSUs (CoTyLam alpha e')
>		
>       CoApp e v       -> 
>		d2c e			`thenSUs` \e' ->
>		defAtom2core v		`thenSUs` \(v',e'') ->
>		returnSUs (mkLet v' e'' (CoApp e' v'))
>		
>       CoTyApp e t     -> 
>		d2c e			`thenSUs` \e' ->
>		returnSUs (CoTyApp e' t)	
>
>       CoCase e ps ->
>		d2c e			`thenSUs` \e' ->
>		defCaseAlts2Core ps	`thenSUs` \ps' ->
>		returnSUs (CoCase e' ps')
>		
>	CoLet b e ->
>		d2c e			`thenSUs` \e' ->
>		defBinding2core b	`thenSUs` \b' ->
>		returnSUs (CoLet b' e')
> 
>       CoSCC l e ->
>		d2c e			`thenSUs` \e' ->
>		returnSUs (CoSCC l e')

> defCaseAlts2Core :: DefCaseAlternatives 
> 	-> SUniqSM PlainCoreCaseAlternatives
>	
> defCaseAlts2Core alts = case alts of
> 	CoAlgAlts alts dflt -> 
>		mapSUs algAlt2Core alts	`thenSUs` \alts' ->
>		defAlt2Core dflt	`thenSUs` \dflt' ->
>		returnSUs (CoAlgAlts alts' dflt')
>		
>	CoPrimAlts alts dflt ->
>		mapSUs primAlt2Core alts `thenSUs` \alts' ->
>		defAlt2Core dflt	 `thenSUs` \dflt' ->
>		returnSUs (CoPrimAlts alts' dflt')
> 
>  where
>  	
>	algAlt2Core (c, vs, e)	= d2c e `thenSUs` \e' -> returnSUs (c, vs, e')
>	primAlt2Core (l, e)	= d2c e `thenSUs` \e' -> returnSUs (l, e')
>	
>	defAlt2Core CoNoDefault = returnSUs CoNoDefault
>	defAlt2Core (CoBindDefault v e) = 
>		d2c e `thenSUs` \e' ->
>		returnSUs (CoBindDefault v e')

> mkLet :: PlainCoreAtom
> 	-> Maybe PlainCoreExpr 
>	-> PlainCoreExpr 
>	-> PlainCoreExpr
>	
> mkLet (CoVarAtom v) (Just e) e' = CoLet (CoNonRec v e) e'
> mkLet v Nothing  e' = e'

-----------------------------------------------------------------------------
XXX - in here becuase if it goes in DefUtils we've got mutual recursion.

> defPanic :: String -> String -> DefExpr -> SUniqSM a
> defPanic modl fun expr =
> 	d2c expr	`thenSUs` \expr ->
> 	panic (modl ++ "(" ++ fun ++ "): " ++ ppShow 80 (ppr PprDebug expr))
