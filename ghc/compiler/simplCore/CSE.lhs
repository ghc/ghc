%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
module CSE (
	cseProgram
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_cse, opt_D_verbose_core2core )
import Id		( Id, idType )
import CoreUtils	( hashExpr, cheapEqExpr, exprIsBig )
import Const		( isBoxedDataCon )
import Type		( splitTyConApp_maybe )
import CoreSyn
import VarEnv	
import CoreLint		( beginPass, endPass )
import Outputable
import Util		( mapAccumL )
import UniqFM
\end{code}


			Simple common sub-expression

When we see
	x1 = C a b
	x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
				 C x1 b -> x2
and apply that to the rest of the program.

When we then see
	y1 = C a b
	y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y  to the mapping.  Rather, we want the reverse, y -> x1
so that a subsequent binding
	z = C y b
will get transformed to C x1 b, and then to x2.  

So we carry an extra var->var mapping which we apply before looking up in the
reverse mapping.


IMPORTANT NOTE
~~~~~~~~~~~~~~
This pass relies on the no-shadowing invariant, so it must run
immediately after the simplifier.

For example, consider
	f = \x -> let y = x+x in
	              h = \x -> x+x
	          in ...

Here we must *not* do CSE on the x+x!


%************************************************************************
%*									*
\section{Common subexpression}
%*									*
%************************************************************************

\begin{code}
cseProgram :: [CoreBind] -> IO [CoreBind]

cseProgram binds
  = do {
	beginPass "Common sub-expression";
	let { binds' = cseBinds emptyCSEnv binds };
	endPass "Common sub-expression" 
	 	(opt_D_dump_cse || opt_D_verbose_core2core)
		binds'	
    }

cseBinds :: CSEnv -> [CoreBind] -> [CoreBind]
cseBinds env []     = []
cseBinds env (b:bs) = (b':bs')
		    where
		      (env1, b') = cseBind  env  b
		      bs'        = cseBinds env1 bs

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e) = let (env', (_,e')) = do_one env (b, e)
			   in (env', NonRec b e')
cseBind env (Rec pairs)  = let (env', pairs') = mapAccumL do_one env pairs
			   in (env', Rec pairs')
			 

do_one env (id, rhs) = case lookupCSEnv env rhs' of
		    	  Just other_id -> (extendSubst env id other_id, (id, Var other_id))
			  Nothing       -> (addCSEnvItem env id rhs',    (id, rhs'))
		     where
	 	        rhs' = cseExpr env rhs


tryForCSE :: CSEnv -> CoreExpr -> CoreExpr
tryForCSE env (Type t) = Type t
tryForCSE env expr     = case lookupCSEnv env expr' of
			    Just id  -> Var id
 			    Nothing  -> expr'
		       where
	 	         expr' = cseExpr env expr


cseExpr :: CSEnv -> CoreExpr -> CoreExpr
cseExpr env (Var v)		   = Var (lookupSubst env v)
cseExpr env (App f (Type t)) 	   = App (cseExpr env f) (Type t)
cseExpr env (App f a)        	   = App (cseExpr env f) (tryForCSE env a)
cseExpr env expr@(Con con args)    = case lookupCSEnv env expr of
				       Just id  -> Var id
				       Nothing  -> Con con [tryForCSE env arg | arg <- args]
cseExpr env (Note n e)       	   = Note n (cseExpr env e)
cseExpr env (Lam b e)	     	   = Lam b (cseExpr env e)
cseExpr env (Let bind e)    	   = let (env1, bind') = cseBind env bind
				     in Let bind' (cseExpr env1 e)
cseExpr env (Type t)		   = Type t
cseExpr env (Case scrut bndr alts) = Case (tryForCSE env scrut) bndr (cseAlts env bndr alts) 


cseAlts env bndr alts
  = map cse_alt alts
  where
    arg_tys = case splitTyConApp_maybe (idType bndr) of
		Just (_, arg_tys) -> map Type arg_tys
		other		  -> pprPanic "cseAlts" (ppr bndr)

    cse_alt (con, args, rhs)
	| null args || not (isBoxedDataCon con) = (con, args, cseExpr env rhs)
		-- Don't try CSE if there are no args; it just increases the number
		-- of live vars.  E.g.
		--	case x of { True -> ....True.... }
		-- Don't replace True by x!  
		-- Hence the 'null args', which also deal with literals and DEFAULT
		-- And we can't CSE on unboxed tuples
	| otherwise
	= (con, args, cseExpr (extendCSEnv env bndr (Con con (arg_tys ++ (map varToCoreExpr args)))) rhs)
\end{code}


%************************************************************************
%*									*
\section{The CSE envt}
%*									*
%************************************************************************

\begin{code}
data CSEnv = CS (UniqFM [(Id, CoreExpr)])	-- The expr in the range has already been CSE'd
	        (IdEnv Id)			-- Simple substitution

emptyCSEnv = CS emptyUFM emptyVarEnv

lookupCSEnv :: CSEnv -> CoreExpr -> Maybe Id
lookupCSEnv (CS cs _) expr
  = case lookupUFM cs (hashExpr expr) of
	Nothing -> Nothing
	Just pairs -> lookup_list pairs expr

lookup_list :: [(Id,CoreExpr)] -> CoreExpr -> Maybe Id
lookup_list [] expr = Nothing
lookup_list ((x,e):es) expr | cheapEqExpr e expr = Just x
			    | otherwise	   	 = lookup_list es expr

addCSEnvItem env id expr | exprIsBig expr = env
			 | otherwise      = extendCSEnv env id expr

extendCSEnv (CS cs sub) id expr
  = CS (addToUFM_C combine cs hash [(id, expr)]) sub
  where
    hash   = hashExpr expr
    combine old new = WARN( length result > 4, text "extendCSEnv: long list:" <+> ppr result )
		      result
		    where
		      result = new ++ old

lookupSubst (CS _ sub) x = case lookupVarEnv sub x of
			     Just y  -> y
			     Nothing -> x

extendSubst (CS cs sub) x y = CS cs (extendVarEnv sub x y)
\end{code}
