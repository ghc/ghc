%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Core2Def]{Translate the CoreProgram into a DefProgram}

>#include "HsVersions.h"
>
> module Core2Def ( 
> 	core2def, c2d,
>	
>	PlainCoreProgram(..), DefProgram(..),
>	CoreBinding, Id, DefBindee ) where
> 
> import DefSyn
>#ifdef __HBC__
> import Trace
>#endif

> import CoreSyn
> import IdEnv
> import PlainCore
> import TaggedCore
> import BinderInfo	-- ( BinderInfo(..), isFun, isDupDanger )
> import CmdLineOpts	( switchIsOn, SwitchResult, SimplifierSwitch )
> import OccurAnal	( occurAnalyseBinds )
> import SimplEnv	( SwitchChecker(..) )
> import Util
> import Pretty
> import Outputable

This module translates the PlainCoreProgram into a DefCoreProgram,
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


> core2def :: (GlobalSwitch -> SwitchResult) -> PlainCoreProgram -> DefProgram
> core2def sw prog = 
> 	map coreBinding2def tagged_program
>   where  
>   	tagged_program = occurAnalyseBinds prog switch_is_on (const False)
>	switch_is_on   = switchIsOn sw


> coreBinding2def :: SimplifiableCoreBinding -> DefBinding
> coreBinding2def (CoNonRec (v,_) e) = CoNonRec v (c2d nullIdEnv e)
> coreBinding2def (CoRec bs) = CoRec (map recBind2def bs)
> 	where recBind2def ((v,_),e) = (v, c2d nullIdEnv e)


> coreAtom2def :: IdEnv DefExpr -> PlainCoreAtom -> DefAtom
> coreAtom2def p (CoVarAtom v) = CoVarAtom (DefArgExpr (lookup p v))
> coreAtom2def p (CoLitAtom l) = CoVarAtom (DefArgExpr (CoLit l))

> isTrivial (CoCon c [] []) = True
> isTrivial (CoVar v)       = True
> isTrivial (CoLit l)       = True
> isTrivial _               = False

> c2d :: IdEnv DefExpr -> SimplifiableCoreExpr -> DefExpr
> c2d p e = case e of
> 
>       CoVar v         -> lookup p v 
>	
>       CoLit l         -> CoLit l
>	
>       CoCon c ts es   -> CoCon c ts (map (coreAtom2def p) es)
>	
>       CoPrim op ts es -> CoPrim op ts (map (coreAtom2def p) es)
>	
>       CoLam vs e      -> CoLam (map fst vs) (c2d p e)
>	
>       CoTyLam alpha e -> CoTyLam alpha (c2d p e)
>	
>       CoApp e v       -> CoApp (c2d p e) (coreAtom2def p v)
>	
>       CoTyApp e t     -> CoTyApp (c2d p e) t
>	
>       CoCase e ps     -> CoCase (c2d p e) (coreCaseAlts2def p ps)
>	
>       CoLet (CoNonRec (v,ManyOcc _) e) e' 
>		| isTrivial e -> c2d (addOneToIdEnv p v (c2d p e)) e'
>		| otherwise ->
>		trace ("Not inlining ManyOcc " ++ ppShow 80 (ppr PprDebug v)) (
>		CoLet (CoNonRec v (c2d p e)) (c2d p e'))
>		
>	CoLet (CoNonRec (v,DeadCode) e) e' ->
>		panic "Core2Def(c2d): oops, unexpected DeadCode"
>		
>	CoLet (CoNonRec (v,OneOcc fun_or_arg dup_danger _ _ _) e) e'
>	   | isTrivial e -> inline_it
>	   | isDupDanger dup_danger ->
>		trace ("Not inlining DupDanger " ++ ppShow 80 (ppr PprDebug v))(
>		CoLet (CoNonRec v (c2d p e)) (c2d p e'))
>	   | isFun fun_or_arg ->
>		panic "Core2Def(c2d): oops, unexpected Macro"
>	   | otherwise -> inline_it
>	 where inline_it = c2d (addOneToIdEnv p v (c2d p e)) e'
>	 
>       CoLet (CoRec bs) e -> CoLet (CoRec (map recBind2def bs)) (c2d p e)
>               where recBind2def ((v,_),e) = (v, c2d p e)
>		
>       CoSCC l e       -> CoSCC l (c2d p e)


> coreCaseAlts2def 
> 	:: IdEnv DefExpr 
> 	-> SimplifiableCoreCaseAlternatives
>	-> DefCaseAlternatives
>	
> coreCaseAlts2def p alts = case alts of
> 	CoAlgAlts as def  -> CoAlgAlts (map algAlt2def as) (defAlt2def def)
>	CoPrimAlts as def -> CoPrimAlts (map primAlt2def as) (defAlt2def def)
>	
>   where 
>	
>	algAlt2def  (c, vs, e) = (c, (map fst vs), c2d p e)
>	primAlt2def (l, e)     = (l, c2d p e)

>	defAlt2def CoNoDefault = CoNoDefault
>	defAlt2def (CoBindDefault (v,_) e) = CoBindDefault v (c2d p e)


> lookup :: IdEnv DefExpr -> Id -> DefExpr
> lookup p v = case lookupIdEnv p v of
> 			Nothing -> CoVar (DefArgVar v)
>			Just e  -> e
