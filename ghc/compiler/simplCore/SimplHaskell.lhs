%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[SimplHaskell]{Printing Core that looks like Haskell}

\begin{code}
#include "HsVersions.h"

module SimplHaskell ( coreToHaskell ) where

IMPORT_Trace
import Outputable
import Pretty

import BasicLit		( BasicLit )
import PlainCore
import IdEnv
import IdInfo
import Maybes
import Util
import AbsPrel		( PrimOp, nilDataCon, consDataCon )
\end{code}

\begin{code}
coreToHaskell :: PlainCoreProgram -> String {- 0 -}
coreToHaskell binds = ("[Haskell:\n\n" ++ ppShow 80 (pprHaskFuns (transformCoreProg binds)) ++ "\n\n]\n")
\end{code}

\begin{code}
data HaskFun = HaskFun Id [([HaskExp],HaskExp)]

data HaskExp 
	= HaskVar Bool Id		-- true of used many times
	| HaskLit BasicLit
	| HaskWild
	| HaskCon Id [HaskExp]
	| HaskPrim PrimOp [HaskExp]
	| HaskLam [HaskExp] HaskExp
	| HaskApp HaskExp HaskExp
	| HaskCase HaskExp [(HaskExp,HaskExp)] 
	| HaskIf HaskExp HaskExp HaskExp
	| HaskLet [HaskFun] HaskExp
\end{code}

Here is where the fun begins, you transform Core into Haskell!

\begin{code}
type InEnv = IdEnv HaskExp
type OutEnv = IdEnv (Int,Bool)	-- number of times used, and if save to inline


mkHaskPatVar :: OutEnv -> Id -> HaskExp
mkHaskPatVar env id = case lookupIdEnv env id of
			Nothing -> HaskWild
			Just (n,_) -> HaskVar (n > 1) id

transformCoreProg :: PlainCoreProgram -> [HaskFun]
transformCoreProg prog = mergeCasesBindings funs
  where
   (_,_,funs) = transformCoreBindings nullIdEnv nullIdEnv prog

transformCoreBindings :: InEnv -> OutEnv -> [PlainCoreBinding] -> (InEnv,OutEnv,[HaskFun])
transformCoreBindings in_env out_env [bnd]      = transformCoreBinding in_env out_env bnd
transformCoreBindings in_env out_env (bnd:bnds) = (in_env'',out_env',hask_bnd ++ hask_bnds)
  where
    (in_env',out_env',hask_bnd)    = transformCoreBinding in_env out_env'' bnd
    (in_env'',out_env'',hask_bnds) = transformCoreBindings in_env' out_env bnds

transformCoreBinding :: InEnv -> OutEnv -> PlainCoreBinding -> (InEnv,OutEnv,[HaskFun])
transformCoreBinding in_env out_env (CoNonRec v expr) = (in_env',out_env'',[HaskFun v rhs])
  where
    out_env''      = merge out_env out_env'          
    (out_env',rhs) = transformCoreRhs in_env expr
    in_env'        = in_env `growIdEnvList` [ (v,exp) | [([],exp)] <- [rhs], False ]

transformCoreBinding in_env out_env (CoRec bnds) = (in_env,out_env'',hask_bnds)
  where
    out_env''  = foldl merge out_env out_envs 
    (out_envs,hask_bnds) = unzip
		[ (out_env',HaskFun v rhs) |
			(v,exp) <- bnds,
			(out_env',rhs) <- [transformCoreRhs in_env exp]]


transformCoreRhs :: InEnv -> PlainCoreExpr -> (OutEnv,[([HaskExp],HaskExp)])
transformCoreRhs in_env exp = (out_env,[(vars',hask_exp)])
    where
	vars'		   = [ mkHaskPatVar out_env v | v <- vars ] 
	(vars,exp')        = getLambdaVars exp
	(out_env,hask_exp) = transformCoreExp in_env exp'
	getLambdaVars (CoTyLam _ e) = getLambdaVars e
	getLambdaVars (CoLam xs e) = (xs ++ xs',e')
  	   where (xs',e') = getLambdaVars e
	getLambdaVars e = ([],e)

transformCoreExp :: InEnv -> PlainCoreExpr -> (OutEnv,HaskExp)
transformCoreExp _      (CoVar v) = (unitIdEnv v (1,True),HaskVar False v)	-- lookup Env ?
transformCoreExp _      (CoLit i) = (nullIdEnv,HaskLit i)
transformCoreExp in_env (CoCon i _ atoms) = (out_env,HaskCon i hask_exps)
  where
    (out_env,hask_exps) = transformCoreExps in_env (map atomToExpr atoms)
transformCoreExp in_env (CoPrim i _ atoms) = (out_env,HaskPrim i hask_exps)
  where
    (out_env,hask_exps) = transformCoreExps in_env (map atomToExpr atoms)
-- CoLam
-- CoTyLam
transformCoreExp in_env (CoLam args exp) = (out_env,HaskLam args' h_exp)
   where -- modify the env !!!!!
	args' = [ mkHaskPatVar out_env v | v <- args ]
	(out_env,h_exp) = transformCoreExp in_env exp
transformCoreExp in_env (CoTyLam _ exp) = transformCoreExp in_env exp
transformCoreExp in_env (CoApp fun atom) = (merge o1 o2,HaskApp h_fun h_arg)
   where
	(o1,h_fun) = transformCoreExp in_env fun
	(o2,h_arg) = transformCoreExp in_env (atomToExpr atom)
transformCoreExp in_env (CoTyApp fun _) = transformCoreExp in_env fun
transformCoreExp in_env (CoCase e alts) = (foldl merge o1 o2,HaskCase h_e h_alts)
   where
	(o1,h_e)    = transformCoreExp in_env e
	(o2,h_alts) = unzip [ (out_env,(pat,h_e)) | (out_env,pat,h_e) <- transformCoreAlts in_env alts ]

transformCoreExp in_env exp@(CoLet _ _) = (o1,HaskLet h_binds h_exp)
  where
	(binds,exp') = getLets exp
	(in_env',o1,h_binds) = transformCoreBindings in_env o2 binds
	(o2,h_exp) = transformCoreExp in_env' exp'
	getLets (CoLet bind exp) = (bind:binds,exp')
	    where (binds,exp') = getLets exp
	getLets exp = ([],exp)

transformCoreExp _ _         = (nullIdEnv,HaskWild)

transformCoreExps :: InEnv -> [PlainCoreExpr] -> (OutEnv,[HaskExp])
transformCoreExps _ []          = (nullIdEnv,[])
transformCoreExps in_env (e:es) = (merge o1 o2,h_e:hs_e)
  where
   (o1,h_e)  = transformCoreExp  in_env e
   (o2,hs_e) = transformCoreExps in_env es

transformCoreAlts :: InEnv -> PlainCoreCaseAlternatives -> [(OutEnv,HaskExp,HaskExp)]
transformCoreAlts in_env (CoAlgAlts alts def) = map trans alts ++ mkdef def
   where
	trans (id,ids,e) = (o1,HaskCon id (map (mkHaskPatVar o1) ids),h_e)
	   where
		(o1,h_e) = transformCoreExp in_env e
	mkdef (CoBindDefault bnd e) = [(o1,mkHaskPatVar o1 bnd,h_e)]
	  where
	    (o1,h_e) = transformCoreExp in_env e
	mkdef _ = []
transformCoreAlts in_env (CoPrimAlts alts def) = map trans alts ++ mkdef def
   where
	trans (lit,e) = (o1,HaskLit lit,h_e)
	   where
		(o1,h_e) = transformCoreExp in_env e
	mkdef (CoBindDefault bnd e) = [(o1,mkHaskPatVar o1 bnd,h_e)]
	  where
	    (o1,h_e) = transformCoreExp in_env e
	mkdef _ = []
\end{code}

\begin{code}
merge :: OutEnv -> OutEnv -> OutEnv
merge e1 e2 = combineIdEnvs fn e1 e2
  where
	fn (n,_) (m,_) = (n+m,False)
\end{code}


\begin{code}
mergeCasesBindings = map mergeCasesFun 

mergeCasesFun (HaskFun id rhss) = HaskFun id (concat (map mergeCasesRhs rhss))

mergeCasesRhs (pats,exp) = [(pats,exp)]

{-
case v of 
   A x -> e1	, v		==> Branch v  [ (A x,e1), (B y,e2) ]
   B y -> e2			OR
				    NoBranches (case v of 
						  A x -> ...
						  B y -> ...)

-}
--mergeCases :: HaskExp -> Set Id -> [(Id,HaskExp,HaskExp)]
--mergeCases _ _ = []
\end{code}



Maybe ???

type SM a = OutEnv Z
returnSH a s = (a,s)
thenSH m k s = case m s of
		(r,s') -> k r s
thenSH_ m k s = case m s of
		(_,s') -> k s

\begin{code}
pprHaskFuns xs = ppAboves (map pprHaskFun xs)

pprHaskFun (HaskFun id stuff) = 
	ppAboves [
		ppSep [ ppCat ([ppr PprForUser id] ++ map (pprHaskExp True) pats),
		        ppNest 2 (ppCat [ppStr "=",pprHaskExp False rhs])]
		| (pats,rhs) <- stuff]

pprHaskExp :: Bool -> HaskExp -> Pretty
pprHaskExp _ (HaskVar _ id) = ppr PprForUser id
pprHaskExp _ (HaskLit i)  = ppr PprForUser i
pprHaskExp _ (HaskWild)   = ppStr "_"
pprHaskExp True exp       = ppBesides [ppLparen,pprHaskExp False exp,ppRparen]
pprHaskExp _ (HaskCon con []) | con == nilDataCon = ppStr "[]"
pprHaskExp _ (HaskCon con [e1,e2]) | con == consDataCon =
		ppCat [pprHaskExp True e1,ppStr ":",pprHaskExp True e2]
pprHaskExp _ (HaskCon con exps) = 
		ppCat (ppr PprForUser con:map (pprHaskExp True) exps)
pprHaskExp _ (HaskPrim prim exps) = 
		ppCat (ppr PprForUser prim:map (pprHaskExp True) exps)
pprHaskExp _ app@(HaskLam xs e) = -- \ xs -> e
	ppSep [ ppCat ([ppStr "\\"] ++ map (pprHaskExp True) xs),
		ppNest 2 (ppCat [ppStr "->",pprHaskExp False e])]
pprHaskExp _ app@(HaskApp _ _) = pprHaskApp app
pprHaskExp _ (HaskCase e opts)
  = ppAboves [ppCat [ppStr "case", pprHaskExp False e,ppStr "of"],
	ppNest 2 (
	   ppAboves [
		(ppSep [ppCat [pprHaskExp False pat,ppStr "->"],
				ppNest 2 (pprHaskExp False exp)])
			| (pat,exp) <- opts])]
pprHaskExp _ (HaskIf i t e) = ppAboves
		[ppCat [ppStr "if",pprHaskExp False i],
		 ppCat [ppStr "then",pprHaskExp False t],
		 ppCat [ppStr "else",pprHaskExp False e]]
pprHaskExp _ (HaskLet binds e)
  = ppAboves [ppStr "let",
	   ppNest 2 (pprHaskFuns binds),
	   ppCat [ppStr "in",ppNest 1 (pprHaskExp False e)]]
pprHaskExp _ _ = panic "pprHaskExp failed"


pprHaskApp (HaskApp fun arg) = ppCat [pprHaskApp fun,pprHaskExp True arg]
pprHaskApp e                 = pprHaskExp True e
\end{code}



pprHaskExp n exp = ppr
