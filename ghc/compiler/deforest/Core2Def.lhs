%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Core2Def]{Translate the CoreProgram into a DefProgram}

>#include "HsVersions.h"
>
> module Core2Def (
> 	core2def, c2d,
>
>	DefProgram(..),
>	GenCoreBinding, Id, DefBindee ) where
>
> import DefSyn

> import CoreSyn
> import BinderInfo	-- ( BinderInfo(..), isFun, isDupDanger )
> import CmdLineOpts	( switchIsOn, SwitchResult, SimplifierSwitch )
> import OccurAnal	( occurAnalyseBinds )
> import SimplEnv	( SwitchChecker(..) )
> import Util
> import Pretty
> import Outputable

This module translates the CoreProgram into a DefCoreProgram,
which includes non-atomic right-hand sides.  The decisions about which
expressions to inline are left to the substitution analyser, which we
run beforehand.

Current thinking:

1.  Inline all non-recursive non-top-level lets that occur only
    once (including inside lambdas, hoping full laziness
    will sort things out later).

2.  We don't inline top-level lets that occur only once, because these
    might not be pulled out again by the let-floater, due to non-
    garbage collection of CAFs.

2.1.  Also, what about these lit things that occur at the top level,
    and are usually marked as macros?

3.  No recusrive functions are unfolded.

ToDo:
4.  Lambdas and case alternatives that bind a variable that occurs
    multiple times are transformed:
    \x -> ..x..x..  ===>  \x -> let x' = x in ..x'..x'..


> core2def :: (GlobalSwitch -> SwitchResult) -> [CoreBinding] -> DefProgram
> core2def sw prog =
> 	map coreBinding2def tagged_program
>   where
>   	tagged_program = occurAnalyseBinds prog switch_is_on (const False)
>	switch_is_on   = switchIsOn sw


> coreBinding2def :: SimplifiableCoreBinding -> DefBinding
> coreBinding2def (NonRec (v,_) e) = NonRec v (c2d nullIdEnv e)
> coreBinding2def (Rec bs) = Rec (map recBind2def bs)
> 	where recBind2def ((v,_),e) = (v, c2d nullIdEnv e)


> coreAtom2def :: IdEnv DefExpr -> CoreArg -> DefAtom
> coreAtom2def p (VarArg v) = VarArg (DefArgExpr (lookup p v))
> coreAtom2def p (LitArg l) = VarArg (DefArgExpr (Lit l))

> isTrivial (Con c [] []) = True
> isTrivial (Var v)       = True
> isTrivial (Lit l)       = True
> isTrivial _               = False

> c2d :: IdEnv DefExpr -> SimplifiableCoreExpr -> DefExpr
> c2d p e = case e of
>
>       Var v         -> lookup p v
>
>       Lit l         -> Lit l
>
>       Con c ts es   -> Con c ts (map (coreAtom2def p) es)
>
>       Prim op ts es -> Prim op ts (map (coreAtom2def p) es)
>
>       Lam vs e      -> Lam (map fst vs) (c2d p e)
>
>       CoTyLam alpha e -> CoTyLam alpha (c2d p e)
>
>       App e v       -> App (c2d p e) (coreAtom2def p v)
>
>       CoTyApp e t     -> CoTyApp (c2d p e) t
>
>       Case e ps     -> Case (c2d p e) (coreCaseAlts2def p ps)
>
>       Let (NonRec (v,ManyOcc _) e) e'
>		| isTrivial e -> c2d (addOneToIdEnv p v (c2d p e)) e'
>		| otherwise ->
>		pprTrace "Not inlining ManyOcc " (ppr PprDebug v) $
>		Let (NonRec v (c2d p e)) (c2d p e')
>
>	Let (NonRec (v,DeadCode) e) e' ->
>		panic "Core2Def(c2d): oops, unexpected DeadCode"
>
>	Let (NonRec (v,OneOcc fun_or_arg dup_danger _ _ _) e) e'
>	   | isTrivial e -> inline_it
>	   | isDupDanger dup_danger ->
>		pprTrace "Not inlining DupDanger " (ppr PprDebug v) $
>		Let (NonRec v (c2d p e)) (c2d p e')
>	   | isFun fun_or_arg ->
>		panic "Core2Def(c2d): oops, unexpected Macro"
>	   | otherwise -> inline_it
>	 where inline_it = c2d (addOneToIdEnv p v (c2d p e)) e'
>
>       Let (Rec bs) e -> Let (Rec (map recBind2def bs)) (c2d p e)
>               where recBind2def ((v,_),e) = (v, c2d p e)
>
>       SCC l e       -> SCC l (c2d p e)


> coreCaseAlts2def
> 	:: IdEnv DefExpr
> 	-> SimplifiableCoreCaseAlts
>	-> DefCaseAlternatives
>
> coreCaseAlts2def p alts = case alts of
> 	AlgAlts as def  -> AlgAlts (map algAlt2def as) (defAlt2def def)
>	PrimAlts as def -> PrimAlts (map primAlt2def as) (defAlt2def def)
>
>   where
>
>	algAlt2def  (c, vs, e) = (c, (map fst vs), c2d p e)
>	primAlt2def (l, e)     = (l, c2d p e)

>	defAlt2def NoDefault = NoDefault
>	defAlt2def (BindDefault (v,_) e) = BindDefault v (c2d p e)


> lookup :: IdEnv DefExpr -> Id -> DefExpr
> lookup p v = case lookupIdEnv p v of
> 			Nothing -> Var (DefArgVar v)
>			Just e  -> e
